#include "thorin/def.h"

#include <algorithm>
#include <iostream>
#include <sstream>
#include <stack>

#include "thorin/continuation.h"
#include "thorin/primop.h"
#include "thorin/type.h"
#include "thorin/world.h"
#include "thorin/util/queue.h"
#include "thorin/util/log.h"

namespace thorin {

#define HENK_STRUCT_EXTRA_NAME name
#define HENK_STRUCT_EXTRA_TYPE const char*
#define HENK_TABLE_NAME world
#define HENK_TABLE_TYPE World
#include "thorin/henk.cpp.h"

#if 0

//------------------------------------------------------------------------------

bool is_const(const Def* def) {
    if (def->isa<Param>()) return false;
    if (def->isa<PrimOp>()) {
        for (auto op : def->ops()) { // TODO slow because ops form a DAG not a tree
            if (!is_const(op))
                return false;
        }
    }

    return true; // continuations are always const
}

size_t vector_length(const Def* def) { return def->type()->as<VectorType>()->length(); }

bool is_primlit(const Def* def, int val) {
    if (auto lit = def->isa<PrimLit>()) {
        switch (lit->primtype_tag()) {
#define THORIN_I_TYPE(T, M) case PrimType_##T: return lit->value().get_##T() == T(val);
#include "thorin/tables/primtypetable.h"
            default: ; // FALLTHROUGH
        }
    }

    if (auto vector = def->isa<Vector>()) {
        for (auto op : vector->ops()) {
            if (!is_primlit(op, val))
                return false;
        }
        return true;
    }
    return false;
}

bool is_minus_zero(const Def* def) {
    if (auto lit = def->isa<PrimLit>()) {
        Box box = lit->value();
        switch (lit->primtype_tag()) {
#define THORIN_I_TYPE(T, M) case PrimType_##T: return box.get_##M() == M(0);
#define THORIN_F_TYPE(T, M) case PrimType_##T: return box.get_##M() == M(-0.0);
#include "thorin/tables/primtypetable.h"
            default: THORIN_UNREACHABLE;
        }
    }
    return false;
}

void Def::replace(const Def* with) const {
    DLOG("replace: % -> %", this, with);
    assert(type() == with->type());
    if (this != with) {
        for (auto& use : uses()) {
            auto def = const_cast<Def*>(use.def());
            auto index = use.index();
            def->unset_op(index);
            def->set_op(index, with);
        }

        auto& this_trackers = world().trackers(this);
        auto& with_trackers = world().trackers(with);
        for (auto tracker : this_trackers) {
            tracker->def_ = with;
            with_trackers.emplace(tracker);
        }

        uses_.clear();
        this_trackers.clear();
    }
}

void Def::dump() const {
    auto primop = this->isa<PrimOp>();
    if (primop && !is_const(primop))
        primop->stream_assignment(std::cout);
    else {
        std::cout << this;
        std::cout << std::endl;
    }
}

World& Def::world() const { return type()->world(); }
Continuation* Def::as_continuation() const { return const_cast<Continuation*>(scast<Continuation>(this)); }
Continuation* Def::isa_continuation() const { return const_cast<Continuation*>(dcast<Continuation>(this)); }
std::ostream& Def::stream(std::ostream& out) const { return out << unique_name(); }

HashSet<Tracker*>& Tracker::trackers(const Def* def) { return def->world().trackers_[def]; }

#endif
}
