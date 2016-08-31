#include "thorin/world.h"

namespace thorin {

#if 0

const Def* import(Def2Def& old2new, World& to, const Def* odef) {
    if (auto ntype = find(old2new, odef)) {
        assert(&ntype->world() == &to);
        return ntype;
    }

    auto ntype = odef->type() ? import(old2new, to, odef->type()) : nullptr;
    size_t size = odef->num_ops();

    if (odef->is_nominal()) {
        if (auto oparam = odef->isa<Param>()) {
            import(old2new, to, oparam->continuation())->as_continuation();
            auto nparam = find(old2new, oparam);
            assert(nparam && &nparam->world() == &to);
            return nparam;
        }

        Continuation* ncontinuation = nullptr;
        if (auto ocontinuation = odef->isa_continuation()) { // create stub in new world
            // TODO maybe we want to deal with intrinsics in a more streamlined way
            if (ocontinuation == ocontinuation->world().branch())
                return old2new[ocontinuation] = to.branch();
            if (ocontinuation == ocontinuation->world().end_scope())
                return old2new[ocontinuation] = to.end_scope();
            auto npi = import(old2new, to, ocontinuation->type())->as<FnType>();
            ncontinuation = to.continuation(npi, ocontinuation->loc(), ocontinuation->cc(), ocontinuation->intrinsic(), ocontinuation->name());
            for (size_t i = 0, e = ocontinuation->num_params(); i != e; ++i) {
                ncontinuation->param(i)->name_ = ocontinuation->param(i)->name();
                old2new[ocontinuation->param(i)] = ncontinuation->param(i);
            }

            old2new[ocontinuation] = ncontinuation;
        }

        auto ocontinuation = odef->as_continuation();
        assert(ncontinuation && &ncontinuation->world() == &to);
        if (size > 0)
            ncontinuation->jump(nops.front(), nops.skip_front(), ocontinuation->jump_loc());
        return ncontinuation;
    } else {
        Array<const Def*> nops(size);
        for (size_t i = 0; i != size; ++i) {
            nops[i] = import(old2new, to, odef->op(i));
            assert(&nops[i]->world() == &to);
        }

        if (auto oprimop = odef->isa<PrimOp>())
            return old2new[oprimop] = oprimop->rebuild(to, nops, ntype);

        if (auto otype = odef->isa<Type>())
            return old2new[otype] = otype->rebuild(to, nops);

        return ntype;
    }
}

const Def* import(World& to, const Def* odef) {
    Def2Def old2new;
    return import(old2new, to, odef);
}
#endif

}
