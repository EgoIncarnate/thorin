#include "thorin/world.h"

namespace thorin {

const Def* import(World& to, Def2Def& old2new, const Def* old_def) {
    if (auto new_def = find(old2new, old_def)) {
        assert(&new_def->world() == &to);
        return new_def;
    }

    auto get_new_ops = [&] () {
        Array<const Def*> result(old_def->num_ops());
        for (size_t i = 0; i != result.size(); ++i) {
            result[i] = import(to, old2new, old_def->op(i));
            assert(&result[i]->world() == &to);
        }
        return result;
    };

    auto new_type = old_def->type() ? import(to, old2new, old_def->type()) : nullptr;

    if (old_def->is_structural())
        return old2new[old_def] = old_def->rebuild(to, /*new_type, */get_new_ops());

    if (auto old_param = old_def->isa<Param>()) {
        import(to, old2new, old_param->continuation());
        auto new_param = find(old2new, old_param);
        assert(new_param && &new_param->world() == &to);
        return new_param;
    }

    Continuation* new_continuation = nullptr;
    if (auto old_continuation = old_def->isa_continuation()) { // create stub in new world
        // TODO maybe we want to deal with intrinsics in a more streamlined way
        if (old_continuation == old_continuation->world().branch())
            return old2new[old_continuation] = to.branch();
        if (old_continuation == old_continuation->world().end_scope())
            return old2new[old_continuation] = to.end_scope();
        new_continuation = to.continuation(new_type->as<FnType>(), old_continuation->loc(), old_continuation->cc(), old_continuation->intrinsic(), old_continuation->name());
        for (size_t i = 0, e = old_continuation->num_params(); i != e; ++i) {
            new_continuation->param(i)->name_ = old_continuation->param(i)->name();
            old2new[old_continuation->param(i)] = new_continuation->param(i);
        }

        old2new[old_continuation] = new_continuation;

        if (old_continuation->is_external())
            new_continuation->make_external();

        if (old_continuation->num_ops() > 0 && old_continuation->callee() == old_continuation->world().branch()) {
            auto cond = import(to, old2new, old_continuation->arg(0));
            if (auto lit = cond->isa<PrimLit>()) {
                auto callee = import(to, old2new, lit->value().get_bool() ? old_continuation->arg(1) : old_continuation->arg(2));
                new_continuation->jump(callee, {}, old_continuation->jump_loc());
                return new_continuation;
            }
        }
    }

    auto old_continuation = old_def->as_continuation();
    assert(new_continuation && &new_continuation->world() == &to);
    if (!old_continuation->empty()) {
        auto new_ops = get_new_ops();
        new_continuation->jump(new_ops.front(), new_ops.skip_front(), old_continuation->jump_loc());
    }
    return new_continuation;
}

const Def* import(World& to, const Def* old_def) {
    Def2Def old2new;
    return import(to, old2new, old_def);
}

}
