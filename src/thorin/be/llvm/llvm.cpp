#include "thorin/be/llvm/llvm.h"

#include <algorithm>
#include <iostream>
#include <stdexcept>

#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/IRReader/IRReader.h>
#include "llvm/Support/raw_ostream.h"
#include <llvm/Support/SourceMgr.h>

#ifdef WFV2_SUPPORT
#include <wfvInterface.h>
#endif

#include "thorin/def.h"
#include "thorin/lambda.h"
#include "thorin/literal.h"
#include "thorin/memop.h"
#include "thorin/primop.h"
#include "thorin/type.h"
#include "thorin/world.h"
#include "thorin/util/array.h"
#include "thorin/analyses/schedule.h"
#include "thorin/analyses/scope.h"
#include "thorin/be/llvm/cpu.h"
#include "thorin/be/llvm/nvvm.h"
#include "thorin/be/llvm/spir.h"
#include "thorin/transform/import.h"

namespace thorin {

CodeGen::CodeGen(World& world, llvm::CallingConv::ID calling_convention)
    : world_(world)
    , context_()
    , module_(new llvm::Module(world.name(), context_))
    , builder_(context_)
    , calling_convention_(calling_convention)
{
    nvvm_device_ptr_ty_ = llvm::IntegerType::getInt64Ty(context_);
    spir_device_ptr_ty_ = llvm::IntegerType::getInt64Ty(context_);

    llvm::SMDiagnostic diag;
    nvvm_module_ = llvm::ParseIRFile("nvvm.s", diag, context_);
    spir_module_ = llvm::ParseIRFile("spir.s", diag, context_);
}

static llvm::Function* get(llvm::Module* from, llvm::Module* to, const char* name) { 
    return llvm::cast<llvm::Function>(to->getOrInsertFunction(name, from->getFunction(name)->getFunctionType()));
}

llvm::Function* CodeGen::nvvm(const char* name) { return get(nvvm_module_, module_, name); }
llvm::Function* CodeGen::spir(const char* name) { return get(spir_module_, module_, name); }

Lambda* CodeGen::emit_builtin(llvm::Function* current, Lambda* lambda) {
    Lambda* to = lambda->to()->as_lambda();
    if (to->attribute().is(Lambda::NVVM))
        return emit_nvvm(lambda);
    if (to->attribute().is(Lambda::SPIR))
        return emit_spir(lambda);
    assert(to->attribute().is(Lambda::Vectorize));
#ifdef WFV2_SUPPORT
    return emit_vectorized(current, lambda);
#else
    assert(false && "vectorization not supported: missing WFV2");
    return nullptr;
#endif
}

llvm::Function* CodeGen::emit_function_decl(std::string& name, Lambda* lambda) {
    auto ft = llvm::cast<llvm::FunctionType>(map(lambda->type()));
    return llvm::Function::Create(ft, llvm::Function::ExternalLinkage, name, module_);
}

void CodeGen::emit() {
    // map all root-level lambdas to llvm function stubs
    auto top_level = top_level_lambdas(world_);
    for (auto lambda : top_level) {
        if (lambda->is_builtin())
            continue;
        llvm::Function* f = nullptr;
        std::string name = lambda->name;
        if (lambda->attribute().is(Lambda::Intrinsic)) {
            std::transform(name.begin(), name.end(), name.begin(), [] (char c) { return c == '_' ? '.' : c; });
            name = "llvm." + name;
            f = emit_intrinsic_decl(name, lambda);
        } else
            f = emit_function_decl(name, lambda);

        assert(f != nullptr && "invalid function declaration");
        fcts_.emplace(lambda, f);
    }

    // emit all globals
    for (auto primop : world_.primops()) {
        if (auto global = primop->isa<Global>()) {
            llvm::Value* val;
            if (auto lambda = global->init()->isa_lambda())
                val = fcts_[lambda];
            else {
                auto var = llvm::cast<llvm::GlobalVariable>(module_->getOrInsertGlobal(global->name, map(global->referenced_type())));
                var->setInitializer(llvm::cast<llvm::Constant>(emit(global->init())));
                val = var;
            }
            primops_[global] = val;
        }
    }

    // emit connected functions first
    std::sort(top_level.begin(), top_level.end(), [](Lambda* first, Lambda* second) {
        return first->is_connected_to_builtin();
    });

    // for all top-level functions
    for (auto lambda : top_level) {
        if (lambda->is_builtin() || lambda->empty())
            continue;

        assert(lambda->is_returning());
        llvm::Function* fct = fcts_[lambda];

        // map params
        const Param* ret_param = 0;
        auto arg = fct->arg_begin();
        for (auto param : lambda->params()) {
            if (param->type()->isa<Mem>())
                continue;
            if (param->order() == 0) {
                arg->setName(param->name);
                params_[param] = arg++;
            }
            else {
                assert(!ret_param);
                ret_param = param;
            }
        }
        assert(ret_param);

        Scope scope(lambda);
        BBMap bbs;

        for (auto lambda : scope.rpo()) {
            // map all bb-like lambdas to llvm bb stubs
            auto bb = bbs[lambda] = llvm::BasicBlock::Create(context_, lambda->name, fct);

            // create phi node stubs (for all non-cascading lambdas different from entry)
            if (!lambda->is_cascading() && !scope.is_entry(lambda)) {
                for (auto param : lambda->params())
                    if (!param->type()->isa<Mem>())
                        phis_[param] = llvm::PHINode::Create(map(param->type()), (unsigned) param->peek().size(), param->name, bb);
            }

        }

        // never use early schedule here - this may break memory operations
        Schedule schedule = schedule_smart(scope);

        // emit body for each bb
        for (auto lambda : scope.rpo()) {
            if (lambda->empty())
                continue;
            assert(scope.is_entry(lambda) || lambda->is_basicblock());
            builder_.SetInsertPoint(bbs[lambda]);

            for (auto primop :  schedule[lambda]) {
                // skip higher-order primops, stuff dealing with frames and all memory related stuff except stores
                if (!primop->type()->isa<Pi>() && !primop->type()->isa<Frame>()
                        && (!primop->type()->isa<Mem>() || primop->isa<Store>()))
                    primops_[primop] = emit(primop);
            }

            // terminate bb
            if (lambda->to() == ret_param) { // return
                size_t num_args = lambda->num_args();
                switch (num_args) {
                    case 0: builder_.CreateRetVoid(); break;
                    case 1:
                            if (lambda->arg(0)->type()->isa<Mem>())
                                builder_.CreateRetVoid();
                            else
                                builder_.CreateRet(lookup(lambda->arg(0)));
                            break;
                    case 2: {
                                if (lambda->arg(0)->type()->isa<Mem>()) {
                                    builder_.CreateRet(lookup(lambda->arg(1)));
                                    break;
                                } else if (lambda->arg(1)->type()->isa<Mem>()) {
                                    builder_.CreateRet(lookup(lambda->arg(0)));
                                    break;
                                }
                                // FALLTHROUGH
                            }
                    default: {
                                 Array<llvm::Value*> values(num_args);
                                 Array<llvm::Type*> elems(num_args);

                                 size_t n = 0;
                                 for (size_t a = 0; a < num_args; ++a) {
                                     if (!lambda->arg(n)->type()->isa<Mem>()) {
                                         llvm::Value* val = lookup(lambda->arg(a));
                                         values[n] = val;
                                         elems[n++] = val->getType();
                                     }
                                 }

                                 assert(n == num_args || n+1 == num_args);
                                 values.shrink(n);
                                 elems.shrink(n);
                                 llvm::Value* agg = llvm::UndefValue::get(llvm::StructType::get(context_, llvm_ref(elems)));

                                 for (size_t i = 0; i != n; ++i)
                                     agg = builder_.CreateInsertValue(agg, values[i], { unsigned(i) });

                                 builder_.CreateRet(agg);
                                 break;
                             }
                }
            } else if (auto select = lambda->to()->isa<Select>()) { // conditional branch
                llvm::Value* cond = lookup(select->cond());
                llvm::BasicBlock* tbb = bbs[select->tval()->as_lambda()];
                llvm::BasicBlock* fbb = bbs[select->fval()->as_lambda()];
                builder_.CreateCondBr(cond, tbb, fbb);
            } else {
                Lambda* to_lambda = lambda->to()->as_lambda();
                if (to_lambda->is_basicblock())      // ordinary jump
                    builder_.CreateBr(bbs[to_lambda]);
                else {
                    if (to_lambda->is_builtin()) {
                        Lambda* ret_lambda = emit_builtin(fct, lambda);
                        builder_.CreateBr(bbs[ret_lambda]);
                    } else {
                        // put all first-order args into an array
                        Array<llvm::Value*> args(lambda->args().size() - 1);
                        size_t i = 0;
                        Def ret_arg = 0;
                        for (auto arg : lambda->args()) {
                            if (arg->order() == 0) {
                                if (!arg->type()->isa<Mem>())
                                    args[i++] = lookup(arg);
                            }
                            else {
                                assert(!ret_arg);
                                ret_arg = arg;
                            }
                        }
                        args.shrink(i);
                        llvm::CallInst* call = builder_.CreateCall(fcts_[to_lambda], llvm_ref(args));
                        call->setCallingConv(calling_convention_); // set proper calling convention

                        if (ret_arg == ret_param)       // call + return
                            builder_.CreateRet(call);
                        else {                          // call + continuation
                            Lambda* succ = ret_arg->as_lambda();
                            const Param* param = succ->param(0)->type()->isa<Mem>() ? nullptr : succ->param(0);
                            if (param == nullptr && succ->num_params() == 2)
                                param = succ->param(1);

                            builder_.CreateBr(bbs[succ]);
                            if (param) {
                                auto i = phis_.find(param);
                                if (i != phis_.end())
                                    i->second->addIncoming(call, builder_.GetInsertBlock());
                                else
                                    params_[param] = call;
                            }
                        }
                    }
                }
            }
        }

        // add missing arguments to phis_
        for (auto p : phis_) {
            const Param* param = p.first;
            llvm::PHINode* phi = p.second;

            for (auto peek : param->peek())
                phi->addIncoming(lookup(peek.def()), bbs[peek.from()]);
        }

        params_.clear();
        phis_.clear();
        primops_.clear();
    }

    // remove marked functions
    for (llvm::Function* rem : fcts_to_remove_) {
        rem->removeFromParent();
        rem->deleteBody();
    }

    {
        std::string error;
        auto bc_name = world_.name() + ".bc";
        llvm::raw_fd_ostream out(bc_name.c_str(), error, llvm::raw_fd_ostream::F_Binary);
        if (!error.empty())
            throw std::runtime_error("cannot write '" + bc_name + "': " + error);

        llvm::WriteBitcodeToFile(module_, out);
    }

    {
        std::string error;
        auto ll_name = world_.name() + ".ll";
        llvm::raw_fd_ostream out(ll_name.c_str(), error);
        if (!error.empty())
            throw std::runtime_error("cannot write '" + ll_name + "': " + error);

        module_->print(out, nullptr);
    }

#ifndef NDEBUG
    llvm::verifyModule(*this->module_);
#endif
}

llvm::Value* CodeGen::lookup(Def def) {
    if (def->is_const())
        return emit(def);

    if (auto primop = def->isa<PrimOp>())
        return primops_[primop];

    const Param* param = def->as<Param>();
    auto i = params_.find(param);
    if (i != params_.end())
        return i->second;

    assert(phis_.find(param) != phis_.end());
    return phis_[param];
}

llvm::AllocaInst* CodeGen::emit_alloca(llvm::Type* type, const std::string& name) {
    assert(type->isArrayTy());
    auto entry = &builder_.GetInsertBlock()->getParent()->getEntryBlock();
    llvm::AllocaInst* alloca;
    if (entry->empty())
        alloca = new llvm::AllocaInst(type, nullptr, name, entry);
    else
        alloca = new llvm::AllocaInst(type, nullptr, name, entry->getFirstNonPHIOrDbg());
    return alloca;
}

llvm::Value* CodeGen::emit(Def def) {
    if (auto bin = def->isa<BinOp>()) {
        llvm::Value* lhs = lookup(bin->lhs());
        llvm::Value* rhs = lookup(bin->rhs());
        std::string& name = bin->name;

        if (auto cmp = bin->isa<Cmp>()) {
            if (cmp->lhs()->type()->is_type_s()) {
                switch (cmp->cmp_kind()) {
                    case Cmp_eq:  return builder_.CreateICmpEQ (lhs, rhs, name);
                    case Cmp_ne:  return builder_.CreateICmpNE (lhs, rhs, name);
                    case Cmp_gt:  return builder_.CreateICmpSGT(lhs, rhs, name);
                    case Cmp_ge:  return builder_.CreateICmpSGE(lhs, rhs, name);
                    case Cmp_lt:  return builder_.CreateICmpSLT(lhs, rhs, name);
                    case Cmp_le:  return builder_.CreateICmpSLE(lhs, rhs, name);
                }
            } else if (cmp->lhs()->type()->is_type_u()) {
                switch (cmp->cmp_kind()) {
                    case Cmp_eq:  return builder_.CreateICmpEQ (lhs, rhs, name);
                    case Cmp_ne:  return builder_.CreateICmpNE (lhs, rhs, name);
                    case Cmp_gt:  return builder_.CreateICmpUGT(lhs, rhs, name);
                    case Cmp_ge:  return builder_.CreateICmpUGE(lhs, rhs, name);
                    case Cmp_lt:  return builder_.CreateICmpULT(lhs, rhs, name);
                    case Cmp_le:  return builder_.CreateICmpULE(lhs, rhs, name);
                }
            } else if (cmp->lhs()->type()->is_type_pf()) {
                switch (cmp->cmp_kind()) {
                    case Cmp_eq: return builder_.CreateFCmpOEQ (lhs, rhs, name);
                    case Cmp_ne: return builder_.CreateFCmpONE (lhs, rhs, name);
                    case Cmp_gt: return builder_.CreateFCmpOGT (lhs, rhs, name);
                    case Cmp_ge: return builder_.CreateFCmpOGE (lhs, rhs, name);
                    case Cmp_lt: return builder_.CreateFCmpOLT (lhs, rhs, name);
                    case Cmp_le: return builder_.CreateFCmpOLE (lhs, rhs, name);
                }
            } else if (cmp->lhs()->type()->is_type_qf()) {
                switch (cmp->cmp_kind()) {
                    case Cmp_eq: return builder_.CreateFCmpUEQ(lhs, rhs, name);
                    case Cmp_ne: return builder_.CreateFCmpUNE(lhs, rhs, name);
                    case Cmp_gt: return builder_.CreateFCmpUGT(lhs, rhs, name);
                    case Cmp_ge: return builder_.CreateFCmpUGE(lhs, rhs, name);
                    case Cmp_lt: return builder_.CreateFCmpULT(lhs, rhs, name);
                    case Cmp_le: return builder_.CreateFCmpULE(lhs, rhs, name);
                }
            }
        }

        if (auto arithop = bin->isa<ArithOp>()) {
            if (arithop->lhs()->type()->is_type_f()) {
                switch (arithop->arithop_kind()) {
                    case ArithOp_add: return builder_.CreateFAdd(lhs, rhs, name);
                    case ArithOp_sub: return builder_.CreateFSub(lhs, rhs, name);
                    case ArithOp_mul: return builder_.CreateFMul(lhs, rhs, name);
                    case ArithOp_div: return builder_.CreateFDiv(lhs, rhs, name);
                    case ArithOp_rem: return builder_.CreateFRem(lhs, rhs, name);
                    case ArithOp_and:
                    case ArithOp_or:
                    case ArithOp_xor:
                    case ArithOp_shl:
                    case ArithOp_shr: THORIN_UNREACHABLE;
                }
            }

            bool nw = arithop->type()->is_type_q(); // quick? -> nsw/nuw/fast float

            if (arithop->type()->is_type_s()) {
                switch (arithop->arithop_kind()) {
                    case ArithOp_add: return builder_.CreateAdd (lhs, rhs, name, false, nw);
                    case ArithOp_sub: return builder_.CreateSub (lhs, rhs, name, false, nw);
                    case ArithOp_mul: return builder_.CreateMul (lhs, rhs, name, false, nw);
                    case ArithOp_div: return builder_.CreateSDiv(lhs, rhs, name);
                    case ArithOp_rem: return builder_.CreateSRem(lhs, rhs, name);
                    case ArithOp_and: return builder_.CreateAnd (lhs, rhs, name);
                    case ArithOp_or:  return builder_.CreateOr  (lhs, rhs, name);
                    case ArithOp_xor: return builder_.CreateXor (lhs, rhs, name);
                    case ArithOp_shl: return builder_.CreateShl (lhs, rhs, name, false, nw);
                    case ArithOp_shr: return builder_.CreateAShr(lhs, rhs, name);
                }
            }
            if (arithop->type()->is_type_u()) {
                switch (arithop->arithop_kind()) {
                    case ArithOp_add: return builder_.CreateAdd (lhs, rhs, name, nw, false);
                    case ArithOp_sub: return builder_.CreateSub (lhs, rhs, name, nw, false);
                    case ArithOp_mul: return builder_.CreateMul (lhs, rhs, name, nw, false);
                    case ArithOp_div: return builder_.CreateUDiv(lhs, rhs, name);
                    case ArithOp_rem: return builder_.CreateURem(lhs, rhs, name);
                    case ArithOp_and: return builder_.CreateAnd (lhs, rhs, name);
                    case ArithOp_or:  return builder_.CreateOr  (lhs, rhs, name);
                    case ArithOp_xor: return builder_.CreateXor (lhs, rhs, name);
                    case ArithOp_shl: return builder_.CreateShl (lhs, rhs, name, nw, false);
                    case ArithOp_shr: return builder_.CreateLShr(lhs, rhs, name);
                }
            }
        }
    }

    if (auto conv = def->isa<ConvOp>()) {
        auto from = lookup(conv->from());
        auto src = conv->from()->type()->as<PrimType>();
        auto dst = conv->type()->as<PrimType>();
        auto to = map(dst);

        if (conv->isa<Cast>()) {
            if (src->isa<Ptr>()) {
                assert(dst->is_type_i());
                return builder_.CreatePtrToInt(from, to);
            }
            if (dst->isa<Ptr>()) {
                assert(src->is_type_i());
                return builder_.CreateIntToPtr(from, to);
            }
            if (src->is_type_f() && dst->is_type_f()) {
                assert(num_bits(src->primtype_kind()) != num_bits(dst->primtype_kind()));
                return builder_.CreateFPCast(from, to);
            } 
            if (src->is_type_f()) {
                if (dst->is_type_s())
                    return builder_.CreateFPToSI(from, to);
                return builder_.CreateFPToUI(from, to);
            }
            if (dst->is_type_f()) {
                if (src->is_type_s())
                    return builder_.CreateSIToFP(from, to);
                return builder_.CreateSIToFP(from, to);
            }
            if (src->is_type_i() && dst->is_type_i() && (num_bits(src->primtype_kind()) > num_bits(dst->primtype_kind())))
                return builder_.CreateTrunc(from, to);
            if (src->is_type_s() && dst->is_type_s() && (num_bits(src->primtype_kind()) < num_bits(dst->primtype_kind())))
                return builder_.CreateSExt(from, to);
            if (src->is_type_u() && dst->is_type_u() && (num_bits(src->primtype_kind()) < num_bits(dst->primtype_kind())))
                return builder_.CreateZExt(from, to);

            assert(from->getType() == to);
            return from;
        }

        if (conv->isa<Bitcast>())
            return builder_.CreateBitCast(from, to);
    }

    if (auto select = def->isa<Select>()) {
        llvm::Value* cond = lookup(select->cond());
        llvm::Value* tval = lookup(select->tval());
        llvm::Value* fval = lookup(select->fval());
        return builder_.CreateSelect(cond, tval, fval);
    }

    if (auto array = def->isa<ArrayAgg>()) {
        auto type = llvm::cast<llvm::ArrayType>(map(array->type()));
        if (array->is_const()) {
            size_t size = array->size();
            Array<llvm::Constant*> vals(size);
            for (size_t i = 0; i != size; ++i)
                vals[i] = llvm::cast<llvm::Constant>(emit(array->op(i)));
            return llvm::ConstantArray::get(type, llvm_ref(vals));
        }
        std::cout << "warning: slow" << std::endl;
        auto alloca = emit_alloca(type, array->name);
        llvm::Instruction* cur = alloca;

        u64 i = 0;
        llvm::Value* args[2] = { builder_.getInt64(0), nullptr };
        for (auto op : array->ops()) {
            args[1] = builder_.getInt64(i++);
            auto gep = llvm::GetElementPtrInst::CreateInBounds(alloca, args, op->name);
            gep->insertAfter(cur);
            auto store = new llvm::StoreInst(lookup(op), gep);
            store->insertAfter(gep);
            cur = store;
        }

        return builder_.CreateLoad(alloca);
    }

    if (auto tuple = def->isa<Tuple>()) {
        llvm::Value* agg = llvm::UndefValue::get(map(tuple->type()));
        for (size_t i = 0, e = tuple->ops().size(); i != e; ++i)
            agg = builder_.CreateInsertValue(agg, lookup(tuple->op(i)), { unsigned(i) });
        return agg;
    }

    if (auto aggop = def->isa<AggOp>()) {
        auto agg = lookup(aggop->agg());
        auto idx = lookup(aggop->index());

        if (aggop->agg_type()->isa<Sigma>()) {
            unsigned i = aggop->index()->primlit_value<unsigned>();

            if (aggop->isa<Extract>())
                return builder_.CreateExtractValue(agg, { i });

            auto insert = def->as<Insert>();
            auto value = lookup(insert->value());

            return builder_.CreateInsertValue(agg, value, { i });
        } else if (aggop->agg_type()->isa<ArrayType>()) {
            // TODO use llvm::ConstantArray if applicable
            std::cout << "warning: slow" << std::endl;
            auto alloca = emit_alloca(agg->getType(), aggop->name);
            builder_.CreateStore(agg, alloca);

            llvm::Value* args[2] = { builder_.getInt64(0), idx };
            auto gep = builder_.CreateInBoundsGEP(alloca, args);

            if (aggop->isa<Extract>())
                return builder_.CreateLoad(gep);

            builder_.CreateStore(lookup(aggop->as<Insert>()->value()), gep);
            return builder_.CreateLoad(alloca);
        } else {
            if (aggop->isa<Extract>())
                return builder_.CreateExtractElement(agg, idx);
            return builder_.CreateInsertElement(agg, lookup(aggop->as<Insert>()->value()), idx);
        }
    }

    if (auto primlit = def->isa<PrimLit>()) {
        llvm::Type* type = map(primlit->type());
        Box box = primlit->value();

        switch (primlit->primtype_kind()) {
            case PrimType_bool:                     return builder_. getInt1(box.get_bool());
            case PrimType_ps8:  case PrimType_qs8:  return builder_. getInt8(box. get_s8());
            case PrimType_pu8:  case PrimType_qu8:  return builder_. getInt8(box. get_u8());
            case PrimType_ps16: case PrimType_qs16: return builder_.getInt16(box.get_s16());
            case PrimType_pu16: case PrimType_qu16: return builder_.getInt16(box.get_u16());
            case PrimType_ps32: case PrimType_qs32: return builder_.getInt32(box.get_s32());
            case PrimType_pu32: case PrimType_qu32: return builder_.getInt32(box.get_u32());
            case PrimType_ps64: case PrimType_qs64: return builder_.getInt64(box.get_s64());
            case PrimType_pu64: case PrimType_qu64: return builder_.getInt64(box.get_u64());
            case PrimType_pf32: case PrimType_qf32: return llvm::ConstantFP::get(type, box.get_f32());
            case PrimType_pf64: case PrimType_qf64: return llvm::ConstantFP::get(type, box.get_f64());
        }
    }

    if (auto undef = def->isa<Undef>()) // bottom and any
        return llvm::UndefValue::get(map(undef->type()));

    if (auto load = def->isa<Load>())
        return builder_.CreateLoad(lookup(load->ptr()));

    if (auto store = def->isa<Store>())
        return builder_.CreateStore(lookup(store->val()), lookup(store->ptr()));

    if (auto slot = def->isa<Slot>())
        return builder_.CreateAlloca(map(slot->type()->as<Ptr>()->referenced_type()), 0, slot->unique_name());

    if (def->isa<Enter>() || def->isa<Leave>())
        return nullptr;

    if (auto vector = def->isa<Vector>()) {
        llvm::Value* vec = llvm::UndefValue::get(map(vector->type()));
        for (size_t i = 0, e = vector->size(); i != e; ++i)
            vec = builder_.CreateInsertElement(vec, lookup(vector->op(i)), lookup(world_.literal_pu32(i)));

        return vec;
    }

    if (auto lea = def->isa<LEA>()) {
        if (lea->referenced_type()->isa<Sigma>())
            return builder_.CreateConstInBoundsGEP2_64(lookup(lea->ptr()), 0ull, lea->index()->primlit_value<u64>());

        assert(lea->referenced_type()->isa<ArrayType>());
        llvm::Value* args[2] = { builder_.getInt64(0), lookup(lea->index()) };
        return builder_.CreateInBoundsGEP(lookup(lea->ptr()), args);
    }

    THORIN_UNREACHABLE;
}

llvm::Type* CodeGen::map(const Type* type) {
    assert(!type->isa<Mem>());
    llvm::Type* llvm_type;
    switch (type->kind()) {
        case PrimType_bool:                                                             llvm_type = builder_. getInt1Ty(); break;
        case PrimType_ps8:  case PrimType_qs8:  case PrimType_pu8:  case PrimType_qu8:  llvm_type = builder_. getInt8Ty(); break;
        case PrimType_ps16: case PrimType_qs16: case PrimType_pu16: case PrimType_qu16: llvm_type = builder_.getInt16Ty(); break;
        case PrimType_ps32: case PrimType_qs32: case PrimType_pu32: case PrimType_qu32: llvm_type = builder_.getInt32Ty(); break;
        case PrimType_ps64: case PrimType_qs64: case PrimType_pu64: case PrimType_qu64: llvm_type = builder_.getInt64Ty(); break;
        case PrimType_pf32: case PrimType_qf32:                                         llvm_type = builder_.getFloatTy(); break;
        case PrimType_pf64: case PrimType_qf64:                                         llvm_type = builder_.getDoubleTy();break;
        case Node_Ptr: 
            llvm_type = llvm::PointerType::getUnqual(map(type->as<Ptr>()->referenced_type())); break;
        case Node_IndefArray: 
            return llvm::ArrayType::get(map(type->as<ArrayType>()->elem_type()), 0);
        case Node_DefArray: {
            auto array = type->as<DefArray>();
            return llvm::ArrayType::get(map(array->elem_type()), array->dim());
        }
        case Node_Pi: {
            // extract "return" type, collect all other types
            const Pi* pi = type->as<Pi>();
            llvm::Type* ret = 0;
            size_t i = 0;
            Array<llvm::Type*> elems(pi->size() - 1);
            for (auto elem : pi->elems()) {
                if (elem->isa<Mem>())
                    continue;
                if (auto pi = elem->isa<Pi>()) {
                    assert(!ret && "only one 'return' supported");
                    if (pi->empty())
                        ret = llvm::Type::getVoidTy(context_);
                    else if (pi->size() == 1)
                        ret = pi->elem(0)->isa<Mem>() ? llvm::Type::getVoidTy(context_) : map(pi->elem(0));
                    else if (pi->size() == 2) {
                        if (pi->elem(0)->isa<Mem>())
                            ret = map(pi->elem(1));
                        else if (pi->elem(1)->isa<Mem>())
                            ret = map(pi->elem(0));
                        else
                            goto multiple;
                    } else {
multiple:
                        Array<llvm::Type*> elems(pi->size());
                        size_t num = 0;
                        for (size_t j = 0, e = elems.size(); j != e; ++j) {
                            if (pi->elem(j)->isa<Mem>())
                                continue;
                            ++num;
                            elems[j] = map(pi->elem(j));
                        }
                        elems.shrink(num);
                        ret = llvm::StructType::get(context_, llvm_ref(elems));
                    }
                } else
                    elems[i++] = map(elem);
            }
            elems.shrink(i);
            assert(ret);

            return llvm::FunctionType::get(ret, llvm_ref(elems), false);
        }

        case Node_Sigma: {
            // TODO watch out for cycles!
            const Sigma* sigma = type->as<Sigma>();
            Array<llvm::Type*> elems(sigma->size());
            size_t num = 0;
            for (auto elem : sigma->elems())
                elems[num++] = map(elem);
            elems.shrink(num);
            return llvm::StructType::get(context_, llvm_ref(elems));
        }

        default: 
            THORIN_UNREACHABLE;
    }

    if (type->length() == 1)
        return llvm_type;
    return llvm::VectorType::get(llvm_type, type->length());
}

//------------------------------------------------------------------------------

void emit_llvm(World& world) {
    World nvvm(world.name() + "_nvvm");
    World spir(world.name() + "_spir");

    // determine different parts of the world which need to be compiled differently
    for (auto lambda : top_level_lambdas(world)) {
        if (lambda->is_connected_to_builtin(Lambda::NVVM))
            import(nvvm, lambda)->name = lambda->unique_name();
        else if (lambda->is_connected_to_builtin(Lambda::SPIR))
            import(spir, lambda)->name = lambda->unique_name();
        else
            continue;

        lambda->name = lambda->unique_name();
        lambda->destroy_body();
        lambda->attribute().set(Lambda::Extern);
    }

    if (!nvvm.lambdas().empty() || !spir.lambdas().empty())
        world.cleanup();

    CPUCodeGen(world).emit();
    if (!nvvm.lambdas().empty())
        NVVMCodeGen(nvvm).emit();
    if (!spir.lambdas().empty())
        SPIRCodeGen(spir).emit();
}

//------------------------------------------------------------------------------

} // namespace thorin