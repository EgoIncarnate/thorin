#include "thorin/primop.h"

#include "thorin/type.h"
#include "thorin/world.h"
#include "thorin/util/array.h"

namespace thorin {

//------------------------------------------------------------------------------

/*
 * constructors
 */

PrimLit::PrimLit(World& world, PrimTypeTag tag, Box box, const Location& loc, const std::string& name)
    : Literal((NodeTag) tag, world.type(tag), loc, name)
    , box_(box)
{}

Cmp::Cmp(CmpTag tag, const Def* lhs, const Def* rhs, const Location& loc, const std::string& name)
    : BinOp((NodeTag) tag, lhs->world().type_bool(vector_length(lhs->type())), lhs, rhs, loc, name)
{}

DefiniteArray::DefiniteArray(World& world, const Def* elem, Defs args, const Location& loc, const std::string& name)
    : Aggregate(Node_DefiniteArray, args, loc, name)
{
    set_type(world.definite_array_type(elem, args.size()));
#ifndef NDEBUG
    for (size_t i = 0, e = num_ops(); i != e; ++i)
        assert(args[i]->type() == type()->elem_type());
#endif
}

IndefiniteArray::IndefiniteArray(World& world, const Def* elem, const Def* dim, const Location& loc, const std::string& name)
    : Aggregate(Node_IndefiniteArray, {dim}, loc, name)
{
    set_type(world.indefinite_array_type(elem));
}

Vector::Vector(World& world, Defs args, const Location& loc, const std::string& name)
    : Aggregate(Node_Vector, args, loc, name)
{
    if (auto primtype = args.front()->type()->isa<PrimType>()) {
        assert(primtype->length() == 1);
        set_type(world.type(primtype->primtype_tag(), args.size()));
    } else {
        auto ptr = args.front()->type()->as<PtrType>();
        assert(ptr->length() == 1);
        set_type(world.ptr_type(ptr->referenced_type(), args.size()));
    }
}

LEA::LEA(const Def* ptr, const Def* index, const Location& loc, const std::string& name)
    : PrimOp(Node_LEA, nullptr, {ptr, index}, loc, name)
{
    auto& world = index->world();
    auto type = ptr_type();
    if (auto tuple = ptr_referenced_type()->isa<Tuple>()) {
        set_type(world.ptr_type(get(tuple->ops(), index), type->length(), type->device(), type->addr_space()));
    } else if (auto array = ptr_referenced_type()->isa<ArrayType>()) {
        set_type(world.ptr_type(array->elem_type(), type->length(), type->device(), type->addr_space()));
    } else if (auto struct_type = ptr_referenced_type()->isa<StructType>()) {
        set_type(world.ptr_type(get(struct_type->ops(), index)));
    } else {
        THORIN_UNREACHABLE;
    }
}

SizeOf::SizeOf(const Def* of, const Location& loc, const std::string& name)
    : PrimOp(Node_SizeOf, of->world().type_qs32(), {}, loc, name)
    , of_(of)
{}

Slot::Slot(const Def* type, const Def* frame, const Location& loc, const std::string& name)
    : PrimOp(Node_Slot, type->world().ptr_type(type), {frame}, loc, name)
{
    assert(frame->type()->isa<FrameType>());
}

Global::Global(const Def* init, bool is_mutable, const Location& loc, const std::string& name)
    : PrimOp(Node_Global, init->type()->world().ptr_type(init->type()), {init}, loc, name)
    , is_mutable_(is_mutable)
{
    assert(is_const(init));
}

Alloc::Alloc(const Def* type, const Def* mem, const Def* extra, const Location& loc, const std::string& name)
    : MemOp(Node_Alloc, nullptr, {mem, extra}, loc, name)
{
    World& w = mem->world();
    set_type(w.tuple_type({w.mem_type(), w.ptr_type(type)}));
}

Load::Load(const Def* mem, const Def* ptr, const Location& loc, const std::string& name)
    : Access(Node_Load, nullptr, {mem, ptr}, loc, name)
{
    World& w = mem->world();
    set_type(w.tuple_type({w.mem_type(), ptr->type()->as<PtrType>()->referenced_type()}));
}

Enter::Enter(const Def* mem, const Location& loc, const std::string& name)
    : MemOp(Node_Enter, nullptr, {mem}, loc, name)
{
    World& w = mem->world();
    set_type(w.tuple_type({w.mem_type(), w.frame_type()}));
}

//------------------------------------------------------------------------------

/*
 * hash
 */

uint64_t PrimLit::vhash() const { return hash_combine(Literal::vhash(), bcast<uint64_t, Box>(value())); }
uint64_t SizeOf::vhash() const { return hash_combine(PrimOp::vhash(), of()); }
uint64_t Slot::vhash() const { return hash_combine((int) tag(), gid()); }

//------------------------------------------------------------------------------

/*
 * equal
 */

bool PrimLit::equal(const Def* other) const {
    return Literal::equal(other) ? this->value() == other->as<PrimLit>()->value() : false;
}

bool SizeOf::equal(const PrimOp* other) const {
    return PrimOp::equal(other) ? this->of() == other->as<SizeOf>()->of() : false;
}

bool Slot::equal(const PrimOp* other) const { return this == other; }

//------------------------------------------------------------------------------

/*
 * vrebuild
 */

// do not use any of PrimOp's type getters - during import we need to derive types from 't' in the new world 'to'

const Def* ArithOp::vrebuild(World& to, Defs ops, const Def*  ) const { return to.arithop(arithop_tag(), ops[0], ops[1], loc(), name); }
const Def* Bitcast::vrebuild(World& to, Defs ops, const Def* t) const { return to.bitcast(t, ops[0], loc(), name); }
const Def* Bottom ::vrebuild(World& to, Defs,     const Def* t) const { return to.bottom(t, loc()); }
const Def* Cast   ::vrebuild(World& to, Defs ops, const Def* t) const { return to.cast(t, ops[0], loc(), name); }
const Def* Cmp    ::vrebuild(World& to, Defs ops, const Def*  ) const { return to.cmp(cmp_tag(), ops[0], ops[1], loc(), name); }
const Def* Enter  ::vrebuild(World& to, Defs ops, const Def*  ) const { return to.enter(ops[0], loc(), name); }
const Def* Extract::vrebuild(World& to, Defs ops, const Def*  ) const { return to.extract(ops[0], ops[1], loc(), name); }
const Def* Global ::vrebuild(World& to, Defs ops, const Def*  ) const { return to.global(ops[0], loc(), is_mutable(), name); }
const Def* Hlt    ::vrebuild(World& to, Defs ops, const Def*  ) const { return to.hlt(ops[0], ops[1], loc(), name); }
const Def* Insert ::vrebuild(World& to, Defs ops, const Def*  ) const { return to.insert(ops[0], ops[1], ops[2], loc(), name); }
const Def* LEA    ::vrebuild(World& to, Defs ops, const Def*  ) const { return to.lea(ops[0], ops[1], loc(), name); }
const Def* Load   ::vrebuild(World& to, Defs ops, const Def*  ) const { return to.load(ops[0], ops[1], loc(), name); }
const Def* PrimLit::vrebuild(World& to, Defs,     const Def*  ) const { return to.literal(primtype_tag(), value(), loc()); }
const Def* Run    ::vrebuild(World& to, Defs ops, const Def*  ) const { return to.run(ops[0], ops[1], loc(), name); }
const Def* Select ::vrebuild(World& to, Defs ops, const Def*  ) const { return to.select(ops[0], ops[1], ops[2], loc(), name); }
const Def* SizeOf ::vrebuild(World& to, Defs,     const Def*  ) const { return to.size_of(of(), loc(), name); }
const Def* Slot   ::vrebuild(World& to, Defs ops, const Def* t) const { return to.slot(t->as<PtrType>()->referenced_type(), ops[0], loc(), name); }
const Def* Store  ::vrebuild(World& to, Defs ops, const Def*  ) const { return to.store(ops[0], ops[1], ops[2], loc(), name); }
const Def* Tuple  ::vrebuild(World& to, Defs ops, const Def*  ) const { return to.tuple(ops, loc(), name); }
const Def* Vector ::vrebuild(World& to, Defs ops, const Def*  ) const { return to.vector(ops, loc(), name); }

const Def* Alloc::vrebuild(World& to, Defs ops, const Def* t) const {
    return to.alloc(t->as<TupleType>()->op(1)->as<PtrType>()->referenced_type(), ops[0], ops[1], loc(), name);
}


const Def* DefiniteArray::vrebuild(World& to, Defs ops, const Def* t) const {
    return to.definite_array(t->as<DefiniteArrayType>()->elem_type(), ops, loc(), name());
}

const Def* StructAgg::vrebuild(World& to, Defs ops, const Def* t) const {
    return to.struct_agg(t->as<StructType>(), ops, loc(), name());
}

const Def* IndefiniteArray::vrebuild(World& to, Defs ops, const Def* t) const {
    return to.indefinite_array(t->as<IndefiniteArrayType>()->elem_type(), ops[0], loc(), name());
}

//------------------------------------------------------------------------------

/*
 * op_name
 */

const char* PrimOp::op_name() const {
    switch (tag()) {
#define THORIN_NODE(op, abbr) case Node_##op: return #abbr;
#include "thorin/tables/nodetable.h"
        default: THORIN_UNREACHABLE;
    }
}

const char* ArithOp::op_name() const {
    switch (tag()) {
#define THORIN_ARITHOP(op) case ArithOp_##op: return #op;
#include "thorin/tables/arithoptable.h"
        default: THORIN_UNREACHABLE;
    }
}

const char* Cmp::op_name() const {
    switch (tag()) {
#define THORIN_CMP(op) case Cmp_##op: return #op;
#include "thorin/tables/cmptable.h"
        default: THORIN_UNREACHABLE;
    }
}

const char* Global::op_name() const { return is_mutable() ? "global_mutable" : "global_immutable"; }

//------------------------------------------------------------------------------

/*
 * stream
 */

std::ostream& PrimOp::stream(std::ostream& os) const {
    if (is_const(this)) {
        if (empty())
            return streamf(os, "% %", op_name(), type());
        else
            return streamf(os, "(% % %)", type(), op_name(), stream_list(ops(), [&](const Def* def) { os << def; }));
    } else
        return os << unique_name();
}

std::ostream& PrimLit::stream(std::ostream& os) const {
    os << type() << ' ';
    auto tag = primtype_tag();

    // print i8 as ints
    switch (tag) {
        case PrimType_qs8: return os << (int) qs8_value();
        case PrimType_ps8: return os << (int) ps8_value();
        case PrimType_qu8: return os << (unsigned) qu8_value();
        case PrimType_pu8: return os << (unsigned) pu8_value();
        default:
            switch (tag) {
#define THORIN_ALL_TYPE(T, M) case PrimType_##T: return os << value().get_##M();
#include "thorin/tables/primtypetable.h"
                default: THORIN_UNREACHABLE;
            }
    }
}

std::ostream& Global::stream(std::ostream& os) const { return os << unique_name(); }

std::ostream& PrimOp::stream_assignment(std::ostream& os) const {
    return streamf(os, "% % = % %", type(), unique_name(), op_name(), stream_list(ops(), [&] (const Def* def) { os << def; })) << endl;
}

//------------------------------------------------------------------------------

/*
 * misc
 */

const Def* PrimOp::out(size_t i) const {
    assert(i < type()->as<Tuple>()->num_ops());
    return world().extract(this, i, loc());
}

const Def* PrimOp::rebuild(Def2Def& old2new) const {
    auto i = old2new.find(this);
    if (i == old2new.end()) {
        if (is_outdated()) {
            Array<const Def*> ops(num_ops());
            for (size_t i = 0, e = num_ops(); i != e; ++i)
                ops[i] = op(i)->rebuild(old2new);

            auto def = rebuild(ops);
            if (this == def)
                is_outdated_ = false;
            return old2new[this] = def;
        } else
            return old2new[this] = this;
    } else
        return i->second;
}

const Def* Extract::extracted_type(const Def* agg, const Def* index) {
    if (auto tuple = agg->type()->isa<Tuple>())
        return get(tuple->ops(), index);
    else if (auto array = agg->type()->isa<ArrayType>())
        return array->elem_type();
    else if (auto vector = agg->type()->isa<VectorType>())
        return vector->scalarize();
    else if (auto struct_type = agg->type()->isa<StructType>())
        return get(struct_type->ops(), index);

    THORIN_UNREACHABLE;
}

//------------------------------------------------------------------------------

}
