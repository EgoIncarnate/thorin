#include <sstream>
#include "thorin/lambda.h"
#include "thorin/primop.h"
#include "thorin/type.h"
#include "thorin/world.h"
#include "thorin/analyses/schedule.h"
#include "thorin/analyses/scope.h"
#include "thorin/util/printer.h"
#include "thorin/be/thorin.h"

namespace thorin {

typedef std::function<void ()> Emitter;

class YCompGen : public Printer {
private:
    DefSet emitted_defs;
    bool scheduled_;

    static void EMIT_NOOP() { }

    std::ostream& emit_operands(Def def);
    std::ostream& emit_lambda_graph_begin(const Lambda*);
    std::ostream& emit_lambda_graph_params(const Lambda*);
    std::ostream& emit_lambda_graph_continuation(const Lambda*);
    std::ostream& emit_lambda_graph_end();

    std::ostream& emit_def(Def);
    std::ostream& emit_primop(const PrimOp*);
    std::ostream& emit_param(const Param*);
    std::ostream& emit_lambda(const Lambda*);
    std::ostream& emit_lambda_graph(const Lambda*);
    std::ostream& emit_block(const Schedule::Block&);

    template<typename T, typename U>
    std::ostream& write_edge(T source, U target, bool control_flow,
        Emitter label = EMIT_NOOP);

    template<typename T>
    std::ostream& write_node(T gid, Emitter label,
        Emitter info1 = EMIT_NOOP,
        Emitter info2 = EMIT_NOOP,
        Emitter info3 = EMIT_NOOP);
public:
    YCompGen(bool scheduled)
        : Printer(std::cout),
        scheduled_(scheduled) {
    }

    void emit_scope(const Scope& scope);
    void emit_world(const World& world);
};

//------------------------------------------------------------------------------


std::ostream& YCompGen::emit_operands(Def def) {
    int i = 0;
    Emitter emit_label = EMIT_NOOP;
    if (def->size() > 1) {
        emit_label = [&] { stream() << i++; };
    }
    dump_list([&](Def operand) {
            write_edge(def->gid(), operand->gid(), false, emit_label);
        }, def->ops(), "", "", "");
    return stream();
}

std::ostream& YCompGen::emit_def(Def def) {
    if (emitted_defs.contains(def)) {
        return stream();
    }
    if (auto primop = def->isa<PrimOp>()) {
        emit_primop(primop);
    } else if (auto lambda = def->isa<Lambda>()) {
        emit_lambda_graph(lambda);
    } else if (auto param = def->isa<Param>()) {
        emit_param(param);
    } else {
        // default, but should not happen...
        write_node(def->gid(),
            [&] { emit_name(def); },
            [&] { emit_type(def->type()); });
        emit_operands(def);
    }
    emitted_defs.insert(def);
    return stream();
}

std::ostream& YCompGen::emit_primop(const PrimOp* primop) {
    if (emitted_defs.contains(primop)) {
        return stream();
    }
    emitted_defs.insert(primop);
    auto emit_label = [&] {
        if (primop->is_proxy()) {
            stream() << "<proxy>";
        } else if (auto primlit = primop->isa<PrimLit>()) {
            auto kind = primlit->primtype_kind();

            // print i8 as ints
            if (kind == PrimType_qs8)
                stream() << (int) primlit->qs8_value();
            else if (kind == PrimType_ps8)
                stream() << (int) primlit->ps8_value();
            else if (kind == PrimType_qu8)
                stream() << (unsigned) primlit->qu8_value();
            else if (kind == PrimType_pu8)
                stream() << (unsigned) primlit->pu8_value();
            else {
                switch (kind) {
#define THORIN_ALL_TYPE(T, M) case PrimType_##T: stream() << primlit->T##_value(); break;
#include "thorin/tables/primtypetable.h"
                    default: THORIN_UNREACHABLE;
                }
            }
        } else if (primop->isa<Global>()) {
            stream() << primop->op_name() << " ";
            emit_name(primop);
        } else if (primop->is_const()) {
            if (primop->empty()) {
                stream() << primop->op_name() << ' ';
                emit_type(primop->type());
            } else {
                stream() << '(';
                if (primop->isa<PrimLit>()) {
                    emit_type(primop->type());
                    stream() << ' ';
                }
                stream() << primop->op_name() << ')';
            }
        } else {
            stream() << primop->op_name() << " ";
            emit_name(primop);
        }
        if (auto vectorop = primop->isa<VectorOp>()) {
            if (!vectorop->cond()->is_allset()) {
                stream() << "@ ";
                emit_name(vectorop->cond());
                stream() << " ";
            }
        }
    };
    write_node(primop->gid(), emit_label,
        [&] { emit_type(primop->type()); });
    emit_operands(primop);
    return stream();
}

template<typename T, typename U>
std::ostream& YCompGen::write_edge(T source, U target, bool control_flow,
        Emitter emit_label) {
    newline() << "edge: { sourcename: \"" << source << "\" targetname: \"" << target << "\"" << " class: ";
    if (control_flow) {
        stream() << 13 << " color: blue";
    } else {
        stream() << 16;
    }
    stream() << " label: \"";
    emit_label();
    return stream() << "\"}";
}

template<typename T>
std::ostream& YCompGen::write_node(T id, Emitter emit_label,
        Emitter emit_info1, Emitter emit_info2, Emitter emit_info3) {
    newline() << "node: { title: \"" << id << "\" label: \"";
    emit_label();
    stream() << "\" info1: \"";
    emit_info1();
    stream() << "\" info2: \"";
    emit_info2();
    stream() << "\" info3: \"";
    emit_info3();
    return stream() << "\"}";
}

std::ostream& YCompGen::emit_param(const Param* param) {
    if (emitted_defs.contains(param)) {
        return stream();
    }
    emitted_defs.insert(param);
    write_node(param->gid(), [&] { stream() << "Param "; emit_name(param); },
        [&] { emit_type(param->type()); });
    emit_operands(param);
    return stream();
}

std::ostream& YCompGen::emit_lambda(const Lambda* lambda) {
    if (emitted_defs.contains(lambda)) {
        return stream();
    }
    emitted_defs.insert(lambda);
    write_node(lambda->gid(),
        [&] { stream() << "λ "; emit_name(lambda); },
        [&] { emit_type(lambda->type()); });
    if (!lambda->empty()) {
        write_edge(lambda->gid(), lambda->to()->gid(), true);
        int i = 0;
        for (auto def : lambda->args()) {
            write_edge(lambda->gid(), def->gid(), false, [&] { stream() << i++;});
        }
    }
    return stream();
}

std::ostream& YCompGen::emit_lambda_graph_begin(const Lambda* lambda) {
    newline() << "graph: {";
    up() << "title: \"" << lambda->gid() << "\"";
    newline() << "label: \"λ ";
    emit_name(lambda);
    stream() << "\"";
    newline() << "info1: \"";
    emit_type(lambda->type());

    if (lambda->is_external())
        stream() << " extern";
    if (lambda->cc() == CC::Device)
        stream() << " device";
    return stream() << "\"";
}

std::ostream& YCompGen::emit_lambda_graph_params(const Lambda* lambda) {
    for (auto param : lambda->params())
        emit_param(param);
    return stream();
}

std::ostream& YCompGen::emit_lambda_graph_continuation(const Lambda* lambda) {
    if (!lambda->empty()) {
        write_node("cont"+std::to_string(lambda->gid()),
            [&] { stream() << "continue"; });
        write_edge("cont"+std::to_string(lambda->gid()), lambda->to()->gid(), true);
        int i = 0;
        for (auto def : lambda->args()) {
            write_edge("cont"+std::to_string(lambda->gid()), def->gid(), false,
                [&] { stream() << i++; });
        }
        // write_edge(lambda->gid(), lambda->to()->gid(), true, [&] {;});
        // int i = 0;
        // for (auto def : lambda->args()) {
        //     write_edge(lambda->gid(), def->gid(), false, [&] { stream() << i; i++; });
        // }
    }
    return stream();
}

std::ostream& YCompGen::emit_lambda_graph_end() { return down() << "}"; }

std::ostream& YCompGen::emit_block(const Schedule::Block& block) {
    auto lambda = block.lambda();
    emitted_defs.insert(lambda);
    emit_lambda_graph_begin(lambda);
    emit_lambda_graph_params(lambda);
    emit_lambda_graph_continuation(lambda);
    for (auto primop : block)
        emit_primop(primop);

    return emit_lambda_graph_end();
}
std::ostream& YCompGen::emit_lambda_graph(const Lambda* lambda) {
    emitted_defs.insert(lambda);
    emit_lambda_graph_begin(lambda);
    emit_lambda_graph_params(lambda);
    emit_lambda_graph_continuation(lambda);
    return emit_lambda_graph_end();
}

void YCompGen::emit_scope(const Scope& scope) {
    newline() << "graph: {";
    up() << "title: \"scope" << scope.id() << "\"";
    newline() << "label: \"scope " << scope.id() << "\"";
    if (scheduled_) {
        auto schedule = schedule_smart(scope);
        for (auto& block : schedule)
            emit_block(block);
    } else {
        for (auto lambda : scope) {
            emit_lambda_graph(lambda);
        }
        for (auto def : scope.in_scope()) {
           emit_def(def);
        }
    }
    down() << "}";
    newline();
}

void YCompGen::emit_world(const World& world) {
    stream() << "graph: {";
    up() << "layoutalgorithm: mindepth //$ \"Compilergraph\"";
    newline() << "orientation: bottom_to_top";
    newline() << "graph: {";
    up() << "label: \"module " << world.name() << "\"";

    Scope::for_each<false>(world, [&] (const Scope& scope) { emit_scope(scope); });

    for (auto primop : world.primops()) {
        emit_def(primop);
        if (!scheduled_) {
            if (auto global = primop->isa<Global>()) {
                emit_primop(global);
            }
        }
    }

    down() << "}";
    down() << "}";
}

//------------------------------------------------------------------------------

void emit_ycomp(const Scope& scope, bool scheduled) {
    YCompGen cg(scheduled);
    cg.emit_scope(scope);
}

void emit_ycomp(const World& world, bool scheduled) {
    YCompGen cg(scheduled);
    cg.emit_world(world);
}

//------------------------------------------------------------------------------


}