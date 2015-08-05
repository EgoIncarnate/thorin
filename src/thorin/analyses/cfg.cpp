#include "thorin/analyses/cfg.h"

#include <iostream>
#include <fstream>

#include "thorin/primop.h"
#include "thorin/analyses/scope.h"
#include "thorin/analyses/domtree.h"
#include "thorin/analyses/looptree.h"
#include "thorin/be/ycomp.h"
#include "thorin/be/thorin.h"
#include "thorin/util/queue.h"

namespace thorin {


std::ostream& operator<<(std::ostream& out, const CFNode& node) {
    return out << (node.isa<InNode>() ? "" : node.in_node()->def()->unique_name() + "-")
        << node.def()->unique_name();
}

std::ostream& operator<<(std::ostream& out, const CFNode* node) {
    return out << *node;
}
//------------------------------------------------------------------------------

uint64_t CFNodeHash::operator() (const CFNode* n) const {
    if (auto in = n->isa<InNode>())
        return hash_value(in->lambda()->gid());
    auto out = n->as<OutNode>();
    return hash_combine(hash_value(out->def()->gid()), out->context()->lambda()->gid());
}

//------------------------------------------------------------------------------

static void leaves(Def def, std::function<void(Def)> f) {
    DefSet done;
    std::queue<Def> queue;

    auto enqueue = [&] (Def def) {
        if (!done.contains(def)) {
            queue.push(def);
            done.insert(def);
        }
    };

    enqueue(def);
    while (!queue.empty()) {
        auto def = pop(queue);
        if (def->isa<Param>() || def->isa<Lambda>())
            f(def);
        else {
            for (auto op : def->as<PrimOp>()->ops())
                enqueue(op);
        }
    }
}

//------------------------------------------------------------------------------

InNode::~InNode() {
    for (auto p : out_nodes_)
        delete p.second;
}

//------------------------------------------------------------------------------

class CFABuilder {
public:
    CFABuilder(CFA& cfa)
        : cfa_(cfa)
        , log_(cfa.scope().entry()->unique_name() + "-cfa.log")
        , log_indent(0)
    {
        in_node(scope().entry());
        in_node(scope().exit());
        run_cfa();
        build_cfg();
    }

    ~CFABuilder() {
        log_.close();
    }

    void run_cfa();
    void unreachable_node_elimination();
    void build_cfg();

    const CFA& cfa() const { return cfa_; }
    const Scope& scope() const { return cfa_.scope(); }


    CFNodeSet cf_nodes(const InNode*, const size_t&);
    CFNodeSet cf_nodes_def_compute(const InNode*, const Def&);
    CFNodeSet cf_nodes_param_compute(const InNode*, const Param*);

    const InNode* in_node(Lambda* lambda) {
        assert(scope().outer_contains(lambda));
        if (auto in = find(cfa().in_nodes(), lambda))
            return in;
        ++cfa_.num_in_nodes_;
        auto in = cfa_.in_nodes_[lambda] = new InNode(lambda);
        return in;
    }

    const OutNode* out_node(const Param* param) {
        return out_node(in_node(param->lambda()), param);
    }

    const OutNode* out_node(const InNode* in, Def def) {
        if (auto out = find(in->out_nodes_, def))
            return out;
        ++cfa_.num_out_nodes_;
        return in->out_nodes_[def] = new OutNode(in, def);
    }

    const CFNode* cf_node(const InNode* node, Lambda* lambda) {
        if (!scope().inner_contains(lambda)) {
            return { out_node(node, lambda) };
        } else {
            return { in_node(lambda) };
        }
    }

    const CFNode* context_cf_node(const InNode* context, const CFNode* node) {
        if (auto out = node->isa<OutNode>()) {
            auto context_out = out_node(context, out->def());
            // add back link
            link(context_out, out);
            return context_out;
        } else {
            return node;
        }
    }

    bool is_out_def(const Def& def) {
        if (auto lambda = def->isa_lambda()) {
            return scope().inner_contains(lambda);
        } else if (auto param = def->isa<Param>) {
            return scope().inner_contains(param->lambda());
        }
        return false;
    }


    void link(const CFNode* src, const CFNode* tgt) {
        if(src->link(tgt))
            log_nl() << "> link " << src << " -> " << tgt;
    }

    std::ofstream& log_nl() {
        log_ << std::endl;
        for(size_t i = log_indent; i > 0; i--)
            log_ << "  ";
        return log_;
    }

    std::ofstream& log() {
        return log_;
    }

private:
    CFA& cfa_;
    DefMap<CFNodeSet> def2nodes_;
    DefSet defs_stable_;
    std::queue<std::pair<const InNode*, size_t>> worklist;
    std::ofstream log_;
    size_t log_indent;
};

CFNodeSet CFABuilder::cf_nodes_param_compute(const InNode* context, const Param* param) {
    if (param->lambda() == scope().entry()) {
        return { out_node(context, param) };
    } else {
        // join all args from all currently known predecessor lambdas
        CFNodeSet nodes;
        for(auto predecessor : in_node(param->lambda())->preds()) {
            if (predecessor->isa<OutNode>()) {
                // any fn from predecessor OutNode might leak into param.
                // we don't need to propagate a OutNode in to-position to the params
                // since the OutNode is always a predecessor of the params lambda
                nodes.insert(predecessor);
            } else {
                auto pred_in = predecessor->as<InNode>();
                auto arg_set = cf_nodes(pred_in, param->index() + 1);
                nodes.insert(arg_set.begin(), arg_set.end());
            }
        }
        return nodes;
    }
}


CFNodeSet CFABuilder::cf_nodes_def_compute(const InNode* node, const Def& def) {
    if (auto param = def->isa<Param>()) {
        return cf_nodes_param_compute(node, param);
    } else if (def->isa<Lambda>()) {
        auto lambda = def->as_lambda();
        return { cf_node(node, lambda) };
    } else {
        // currently all other Defs should be ops of some Lambda
        // XXX leaves assumes that there are never primops that change the set of lambdas/params
        CFNodeSet nodes;
        leaves(def, [&] (Def leaf) {
            if (auto leaf_param = leaf->isa<Param>()) {
                auto set = cf_nodes(in_node(leaf_param->lambda()), leaf_param->index() + 1);
                nodes.insert(set.begin(), set.end());
            } else {
                nodes.insert(cf_node(node, leaf->as_lambda()));
            }
        });
        return nodes;
    }
}

std::ostream& operator << (std::ostream& o, Def def) {
    emit_type(def->type(), o);
    return o << " " << def->unique_name();
}

// computes all necessary dependencies and set def to stable and reachable
CFNodeSet CFABuilder::cf_nodes(const InNode* node, const size_t& op_index) {
    auto def = node->lambda()->op(op_index);
    log_indent++;
    log_nl() << "cf_nodes(" << node->lambda()->unique_name() << ", " << op_index << ") op is: " << def;
    log_indent++;
    assert(def->type().isa<FnType>() && "Should never call this with a non-function type Def");
    if (defs_stable_.contains(def)) {
        log_nl() << "is stable: " << def2nodes_[def];
        log_indent--;
        return def2nodes_[def];
    }

    if (def2nodes_.find(def) == def2nodes_.end()) {
        def2nodes_[def] = CFNodeSet();
    }
    defs_stable_.insert(def);

    auto old_set = def2nodes_[def];
    auto new_set = cf_nodes_def_compute(node, def);
    log_nl() << "old set: " << old_set;
    log_nl() << "new set: " << new_set;

    // fast check whether something changed, since analysis should be monotonic
    if (old_set.size() < new_set.size()) {
        auto difference = new_set - old_set;

        assert(new_set > old_set && "old_set must be a subset of new_set or monotonicity has broken");
        def2nodes_[def] = new_set; // TODO emplace or something?

        log_indent++;
        for (auto new_node : difference) {
            log_nl() << "newly found node: " << new_node;
            if (auto new_in_node = new_node->isa<InNode>()) {
                if (op_index == 0) {
                    link(node, new_in_node);
                    // new_in_node->lambda() becomes reachable
                    // TODO only put in worklist if currently stable or never computed!
                    defs_stable_.erase(new_in_node->lambda()->op(0));
                    worklist.push(std::pair<const InNode*, size_t>(new_in_node, 0));
                } else {
                    // TODO do we need to put dependent uses into the worklist?
                }
            } else {
                // TODO extract to helper fn
                auto new_out_node = new_node->as<OutNode>();
                if (op_index == 0) {
                    // make sure it's a 1-context-sensitive out_node
                    auto local_out_node = out_node(node, new_out_node->def());
                    link(node, local_out_node);
                    if (new_out_node->in_node() != node)
                        link(local_out_node, new_out_node);
                    // all cf_nodes of args may be succs of local_out_node
                    auto lambda = node->lambda();
                    log_indent++;
                    for (size_t arg_i = 1; arg_i < lambda->size() ; arg_i++) {
                        auto arg = lambda->op(arg_i);
                        // compute their nodes now, need to link
                        if (!arg->type().isa<FnType>())
                            continue;
                        log_nl() << "arg " << arg->unique_name() << " (" << lambda->unique_name() << ", " << arg_i << "): ";
                        emit_type(arg->type(), log());
                        log_indent++;
                        for (auto arg_node : cf_nodes(node, arg_i)) {
                            // make sure it's a 1-context-sensitive out_node if necessary
                            link(local_out_node, arg_node);
                            if (auto arg_in = arg_node->isa<InNode>()) {
                                // found a new predecessor of local_arg_node->lambda
                                // need to recompute all its FnType ops
                                log_nl() << "recompute ops of " << arg_in << ":";
                                log_indent++;
                                for (size_t index = 0; index < arg_in->lambda()->size(); ++index) {
                                    auto op = arg_in->lambda()->op(index);
                                    if (!op->type().isa<FnType>())
                                        continue;
                                    log_nl() << "+WL: op " << index << " = " << op->unique_name() << ": ";
                                    emit_type(op->type(), log());
                                    // TODO only put in worklist if currently stable or never computed!
                                    defs_stable_.erase(op);
                                    worklist.push(std::pair<const InNode*, size_t>(arg_in, index));
                                }
                                log_indent--;
                            }
                        }
                        log_indent--;
                    }
                    log_indent--;
                } else {
                    // TODO do we need to put dependent uses into the worklist?
                }
            }
        }
        log_indent--;
    }
    log_indent -= 2;
    return new_set;
}

void CFABuilder::run_cfa() {
    worklist = std::queue<std::pair<const InNode*, size_t>>();

    worklist.push(std::pair<const InNode*, size_t>(in_node(scope().entry()), 0));

    log_nl() << "run_cfa()";
    while (!worklist.empty()) {
        auto pair = pop(worklist);
        log() << std::endl;
        log_nl() << "run_cfa() pop: " << pair.first->lambda()->unique_name() << ": ";
        emit_type(pair.first->lambda()->type(), log());
        log() << " - " << pair.second << " " << pair.first->lambda()->op(pair.second);
        cf_nodes(pair.first, pair.second);
    }
    log_nl() << "run_cfa: worklist empty, finished";
    log_nl() << "Final links:";
    log_indent++;
    for (auto node : cfa().in_nodes()) {
        log_nl() << node;
        log_indent++;
        for (auto succ : node->succs()) {
            log_nl() << succ;
            if (succ->isa<OutNode>()) {
                log_indent++;
                for (auto o_succ : succ->succs()) {
                    log_nl() << o_succ;
                }
                log_indent--;
            }
        }
        log_indent--;
    }
    log_indent--;

}

void CFABuilder::unreachable_node_elimination() {
    CFNodeSet reachable;
    std::queue<const CFNode*> queue;

    auto enqueue = [&] (const CFNode* n) {
        if (reachable.insert(n).second)
            queue.push(n);
    };

    enqueue(cfa().entry());
    while (!queue.empty()) {
        for (auto succ : pop(queue)->succs())
            enqueue(succ);
    }

    enqueue(cfa().exit());

    auto unlink = [&] (const CFNode* n) {
        for (auto pred : n->preds())
            pred->succs_.erase(n);
        for (auto succ : n->succs())
            succ->preds_.erase(n);
    };

    for (size_t i = 0, e = cfa().in_nodes().array().size(); i != e; ++i) {
        if (auto& in = cfa_.in_nodes_.array().data()[i]) {
#ifndef NDEBUG
            for (auto p : in->out_nodes()) {
                auto out = p.second;
                assert(!(reachable.contains(in) ^ reachable.contains(out)) && "TODO");
            }
#endif
            if (!reachable.contains(in)) {
                for (auto p : in->out_nodes()) {
                    auto out = p.second;
                    unlink(out);
                    --cfa_.num_out_nodes_;
                }
                unlink(in);
                delete in;
                in = nullptr;
                --cfa_.num_in_nodes_;
            }
        }
    }
}

void CFABuilder::build_cfg() {
    for (auto in : cfa().in_nodes()) {
        for (auto pair : in->out_nodes()) {
            auto out = pair.second;

            // TODO only those that don't have a succ? don't care about this control flow path?
            if (out->def()->isa<Param>())
                out->link(cfa().exit());

            // for (const auto& arg : info.skip_front()) {
                // for (auto n_arg : arg)
                    // out->link(n_arg);
            // }
        }
    }

    // TODO link CFNodes not reachable from exit
    // HACK
    if (scope().entry()->empty())
        cfa().entry()->link(cfa().exit());

    // unreachable_node_elimination();

#ifndef NDEBUG
    bool error = false;
    for (auto in : cfa().in_nodes()) {
        if (in != cfa().entry() && in->preds_.size() == 0) {
            std::cout << "missing predecessors: " << in->lambda()->unique_name() << std::endl;
            error = true;
        }
    }
    if (error) {
        std::ofstream out("out.vcg");
        emit_ycomp(scope(), false, out);
        out.close();
        abort();
    }
#endif
}

//------------------------------------------------------------------------------

CFA::CFA(const Scope& scope)
    : scope_(scope)
    , in_nodes_(scope)
{
    CFABuilder cfa(*this);
}

CFA::~CFA() {
    for (auto n : in_nodes_.array()) delete n;
}

const F_CFG& CFA::f_cfg() const { return lazy_init(this, f_cfg_); }
const B_CFG& CFA::b_cfg() const { return lazy_init(this, b_cfg_); }

//------------------------------------------------------------------------------

template<bool forward>
CFG<forward>::CFG(const CFA& cfa)
    : cfa_(cfa)
    , rpo_(*this)
{
    size_t result = post_order_visit(entry(), cfa.num_cf_nodes());
    assert(result == 0);
}

template<bool forward>
size_t CFG<forward>::post_order_visit(const CFNode* n, size_t i) {
    auto& n_index = forward ? n->f_index_ : n->b_index_;
    assert(n_index == size_t(-1));
    n_index = size_t(-2);

    for (auto succ : succs(n)) {
        if (index(succ) == size_t(-1))
            i = post_order_visit(succ, i);
    }

    n_index = i-1;
    rpo_[n] = n;
    return n_index;
}

template<bool forward>
void CFG<forward>::dump() const {
    for (auto n : rpo()) {
        for (auto succ : n->succs())
            std::cout << n->def()->unique_name() << " -> " << succ->def()->unique_name() << std::endl;
    }
}

template<bool forward> const DomTreeBase<forward>& CFG<forward>::domtree() const { return lazy_init(this, domtree_); }
template<bool forward> const LoopTree<forward>& CFG<forward>::looptree() const { return lazy_init(this, looptree_); }

template class CFG<true>;
template class CFG<false>;

//------------------------------------------------------------------------------

}
        // for (auto use : def->uses()) {
        //     if (!defs_reachable_.contains(use))
        //         continue;
        //
        //     if (auto use_lambda = use->isa_lambda()) {
        //         assert(find(cfa().in_nodes(), use_lambda) && "InNode must exist for reachable lambdas");
        //         auto use_node = in_node(use_lambda);
        //         // so def is an op of use_lambda
        //         for (auto new_node : difference) {
        //             if (auto new_in_node = new_node->isa<InNode>()) {
        //                 if (use.index() == 0) {
        //                     use_node->link(new_in_node);
        //                     // new_in_node->lambda() becomes reachable
        //                     worklist.push(std::pair<Def, Lambda*>(new_in_node->lambda()->to(),
        //                         new_in_node->lambda()));
        //                 } else {
        //                     // TODO
        //                 }
        //             } else {
        //                 // TODO extract to helper fn
        //                 auto new_out_node = new_node->as<OutNode>();
        //                 if (use.index() == 0) {
        //                     // construct a 1-context-sensitive out_node
        //                     auto local_out_node = out_node(use_node, new_out_node->def());
        //                     use_node->link(local_out_node);
        //                     // all cf_nodes of args of use may be succs of local_out_node
        //                     for (auto arg : use_lambda->args()) {
        //                         // compute their nodes now, need to link
        //                         for (auto arg_node : cf_nodes(use_node, arg)) {
        //                             local_out_node->link(arg_node);
        //                             // found a new predecessor of arg_node->lambda
        //                             // need to recompute all its FnType ops
        //                             if (auto arg_in = arg_node->isa<InNode>()) {
        //                                 for (auto op_arg : arg_in->lambda()->ops()) {
        //                                     defs_stable_.erase(op_arg);
        //                                     worklist.push(std::pair<Def, Lambda*>(op_arg, arg_in->lambda()));
        //                                 }
        //                             }
        //                         }
        //                     }
        //                 } else {
        //                     // TODO
        //                 }
        //             }
        //         }
        //     }
        // }
