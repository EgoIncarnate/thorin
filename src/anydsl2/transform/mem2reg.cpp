#include <boost/unordered_map.hpp>

#include "anydsl2/memop.h"
#include "anydsl2/world.h"
#include "anydsl2/analyses/scope.h"
#include "anydsl2/analyses/schedule.h"
#include "anydsl2/analyses/verify.h"
#include "anydsl2/transform/inliner.h"
#include "anydsl2/transform/merge_lambdas.h"

namespace anydsl2 {

// currently, this transformation only works when in CFF

void mem2reg(World& world) {
    Array<Lambda*> top = top_level_lambdas(world);

    for_all (lambda, world.lambdas()) {
        lambda->set_parent(lambda);
        lambda->unseal();
    }

    for_all (lambda, top) {
        lambda->set_parent(0);
        lambda->seal();
    }

    AutoVector<Tracker*> enters;
    AutoVector<Tracker*> leaves;

    for_all (root, top) {
        Scope scope(root);
        std::vector<const Access*> accesses;
        Schedule schedule = schedule_late(scope);
        const size_t pass = world.new_pass();
        size_t cur_handle = 0;
        boost::unordered_map<const Load*, const Tracker*> load2tracker;

        for (size_t i = 0, e = scope.size(); i != e; ++i) {
            Lambda* lambda = scope[i];

            // Search for slots/loads/stores from top down and use set_value/get_value to install parameters.
            // Then, we now what must be replaced but do not yet replace anything:
            // Defs in the schedule might get invalid!
            for_all (primop, schedule[i]) {
                if (const Slot* slot = primop->isa<Slot>()) {
                    // are all users loads and store?
                    for_all (use, slot->uses()) {
                        if (!use->isa<Load>() && !use->isa<Store>()) {
                            slot->counter = size_t(-1);     // mark as "address taken"
                            goto next_primop;
                        }
                    }
                    slot->counter = cur_handle++;
                } else if (const Store* store = primop->isa<Store>()) {
                    if (const Slot* slot = store->ptr()->isa<Slot>()) {
                        if (slot->counter != size_t(-1)) {  // if not "address taken"
                            lambda->set_value(slot->counter, store->val());
                            accesses.push_back(store);
                        }
                    }
                } else if (const Load* load = primop->isa<Load>()) {
                    if (const Slot* slot = load->ptr()->isa<Slot>()) {
                        if (slot->counter != size_t(-1)) {  // if not "address taken"
                            const Type* type = slot->type()->as<Ptr>()->referenced_type();
                            load2tracker[load] = new Tracker(lambda->get_value(slot->counter, type, slot->name.c_str()));
                            accesses.push_back(load);
                        }
                    }
                } else if (const Enter* enter = primop->isa<Enter>()) {
                    enters.push_back(new Tracker(enter));   // keep track of Enters - they might get superfluous 
                } else if (const Leave* leave = primop->isa<Leave>()) {
                    leaves.push_back(new Tracker(leave));   // keep track of Leaves - they might get superfluous 
                }
            }

            // seal successors of last lambda if applicable
            for_all (succ, lambda->succs()) {
                if (succ->parent() != 0) {
                    if (!succ->visit(pass))
                        succ->counter = succ->preds().size();
                    if (--succ->counter == 0)
                        succ->seal();
                }
            }
        }

        // now replace everything from bottom up
        for (size_t i = accesses.size(); i-- != 0;) {
            if (const Load* load = accesses[i]->isa<Load>()) {
                load->extract_val()->replace(load2tracker[load]->def());
                load->extract_mem()->replace(load->mem());
            } else {
                const Store* store = accesses[i]->as<Store>();
                store->replace(store->mem());
            }
        }

        for_all (p, load2tracker)
            delete p.second;

next_primop:;
    }

    for_all (lambda, world.lambdas())
        lambda->clear();

    // this will wipe out dead Slots
    world.cleanup();

    // are there any Leave/Enter pairs superfluous?
    for (size_t i = leaves.size(); i-- != 0;) {
        const Leave* leave = leaves[i]->def()->as<Leave>();
        const Enter* enter = leave->frame()->as<TupleExtract>()->tuple()->as<Enter>();

        for_all (use, enter->uses()) {
            if (use->isa<Slot>())
                goto next_leave;
        }

        enter->extract_mem()->replace(enter->mem());
        leave->replace(leave->mem());

next_leave:;
    }

    // are there superfluous poor, lonely Enters? no mercy - eliminate them
    for (size_t i = enters.size(); i-- != 0;) {
        if (const Enter* enter = enters[i]->def()->isa<Enter>()) {
            if (enter->extract_frame()->num_uses() == 0)
                enter->extract_mem()->replace(enter->mem());
        }
    }

    debug_verify(world);
}

}
