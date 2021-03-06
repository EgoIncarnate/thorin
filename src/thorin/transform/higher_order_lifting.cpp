#include <algorithm>

#include "thorin/analyses/cfg.h"
#include "thorin/analyses/free_defs.h"
#include "thorin/analyses/scope.h"
#include "thorin/transform/mangle.h"
#include "thorin/util/log.h"

namespace thorin {

void higher_order_lifting(World& world) {
    ContinuationSet top;
    Scope::for_each<false>(world, [&](const Scope& scope) { top.emplace(scope.entry()); });

    Scope::for_each(world, [&](Scope& scope) {
        bool dirty = false;

        for (auto n : scope.f_cfg().post_order()) {
            auto continuation = n->continuation();
            if (continuation != scope.entry() && continuation->order() > 1) {
                Scope cur(continuation);
                std::vector<const Def*> defs;

                for (auto def : free_defs(cur)) {
                    if (!def->isa<Continuation>() || !top.contains(def->as_continuation()))
                        defs.push_back(def);
                }

                Scope lifted(lift(cur, defs));

                for (auto use : continuation->copy_uses()) {
                    if (auto caller = use->isa_continuation()) {
                        if (use.index() == 0) {
                            caller->jump(lifted.entry(), concat(caller->args(), defs), caller->jump_debug());
                            dirty = true;
                        }
                    }
                }

                top.insert(lifted.entry());
            }
        }

        if (dirty)
            scope.update();
    });
}

}
