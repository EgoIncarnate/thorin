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

typedef std::pair<const InNode*, size_t> CFPair;

struct CFPairHash {
    uint64_t operator() (const CFPair& op_c) const {
        return hash_combine(CFNodeHash()(op_c.first), op_c.second);
    }
};

struct CFPairEqual {
    bool operator() (const CFPair& op_c_1, const CFPair& op_c_2) const {
        return op_c_1.first->lambda() == op_c_2.first->lambda()  && op_c_1.second == op_c_2.second;
    }
};

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
#ifdef LOG
        , log_(cfa.scope().entry()->unique_name() + "-cfa.log")
        , log_indent(0)
#endif
    {
        in_node(scope().entry());
        in_node(scope().exit());
        if (!scope().entry()->is_intrinsic()) {
            run_cfa();
        }
        build_cfg();
    }

#ifdef LOG
    ~CFABuilder() {
        log_.close();
    }
#endif

    void run_cfa();
    void build_cfg();

    const CFA& cfa() const { return cfa_; }
    const Scope& scope() const { return cfa_.scope(); }


    CFNodeSet cf_nodes(const CFPair&);
    CFNodeSet cf_nodes_def_compute(const InNode*, const Def&);
    CFNodeSet cf_nodes_param_compute(const InNode*, const Param*);

    const InNode* in_node(Lambda* lambda) {
        assert(scope().outer_contains(lambda));
        if (auto in = find(cfa().in_nodes(), lambda))
            return in;
        ++cfa_.num_in_nodes_;
#ifdef LOG
        log_nl() << "in_node: new InNode(" << lambda->unique_name() << ")";
#endif
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
#ifdef LOG
        log_nl() << "out_node: new OutNode(" << in << ", " << def->unique_name() << ")";
#endif
        return in->out_nodes_[def] = new OutNode(in, def);
    }

    const CFNode* cf_node(const InNode* node, Lambda* lambda) {
        if (!scope().outer_contains(lambda)) {
            return { out_node(node, lambda) };
        } else {
            return { in_node(lambda) };
        }
    }

    const CFNode* context_cf_node(const InNode* context, const CFNode* node) {
        if (auto out = node->isa<OutNode>()) {
            auto context_out = out_node(context, out->def());
            link(context, context_out);
            if (context != out->context()) {
                // add back link
                link(context_out, out);
            }
            return context_out;
        } else {
            return node;
        }
    }

    bool is_out(const Def& def) {
        if (auto lambda = def->isa_lambda()) {
            return !scope().outer_contains(lambda);
        } else if (auto param = def->isa<Param>()) {
            return param->lambda() == scope().entry();
        }
        return false;
    }

    void link(const CFNode* src, const CFNode* dst) {
#ifdef LOG
        if (src->link(dst))
            log_nl() << "> link " << src << " -> " << dst;
#else
        src->link(dst);
#endif
    }

    void add_to_worklist(const CFPair& op_c) {
        auto iter = stable_.find(op_c);
        bool is_first_compute = iter == stable_.end();
        if (is_first_compute || iter->second) {
            if (!is_first_compute) { // keep info that op_c was not computed yet
                stable_[op_c] = false;
            }
            worklist.push(op_c);
#ifdef LOG
            auto op = op_c.first->lambda()->op(op_c.second);
            log_nl() << "+WL: " << op_c.first << ", op " << op_c.second << " = " << op->unique_name() << ": ";
            emit_type(op->type(), log());
#endif
        } else {
#ifdef LOG
            log_nl() << "=WL: Not adding to WL, unstable: " << op_c.first << ", op " << op_c.second;
#endif
        }
    }

    void set_stable(const CFPair& op_c) {
#ifdef LOG
        log_nl() << "marking stable: " << op_c.first << ", op " << op_c.second;
#endif
        stable_[op_c] = true;
    }

#ifdef LOG
    std::ofstream& log_nl() {
        log_ << std::endl;
        for(size_t i = log_indent; i > 0; i--)
            log_ << "  ";
        return log_;
    }

    std::ofstream& log() {
        return log_;
    }
#endif

private:
    CFA& cfa_;
    HashMap<CFPair, CFNodeSet, CFPairHash, CFPairEqual> op2nodes_;
    HashMap<CFPair, bool, CFPairHash, CFPairEqual> stable_;
    std::queue<CFPair> worklist;
#ifdef LOG
    std::ofstream log_;
    size_t log_indent;
#endif
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
                auto arg_set = cf_nodes(CFPair(pred_in, param->index() + 1));
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
                auto set = cf_nodes(CFPair(in_node(leaf_param->lambda()), leaf_param->index() + 1));
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
CFNodeSet CFABuilder::cf_nodes(const CFPair& op_c) {
    auto node = op_c.first;
    auto op_index = op_c.second;
    auto def = node->lambda()->op(op_index);
#ifdef LOG
    log_indent++;
    log_nl() << "cf_nodes(" << node->lambda()->unique_name() << ", " << op_index << ") op is: " << def;
    log_indent++;
#endif
    assert(def->type().isa<FnType>() && "Should never call this with a non-function type Def");
    auto iter = stable_.find(op_c);
    bool is_first_compute = (iter == stable_.end());
    if (!is_first_compute && iter->second) {
#ifdef LOG
        log_nl() << "is stable: " << op2nodes_[op_c];
        log_indent -= 2;
#endif
        return op2nodes_[op_c];
    }

    // initialize if necessary for cycles
    if (op2nodes_.find(op_c) == op2nodes_.end()) {
        op2nodes_[op_c] = CFNodeSet();
    }
    set_stable(op_c);

    auto old_set = op2nodes_[op_c];
    auto new_set = cf_nodes_def_compute(node, def);

#ifdef LOG
    log_nl() << "old set: " << old_set;
    log_nl() << "new set: " << new_set;
#endif

    if (is_first_compute) {
#ifdef LOG
        log_nl() << "ignoring old_set, first time computing this";
#endif
        old_set = {};
    }

    // fast check whether something changed, works since analysis is monotonic
    if (old_set.size() < new_set.size()) {
        auto difference = new_set - old_set;
        assert(new_set > old_set && "old_set must be a subset of new_set or monotonicity was broken");

        op2nodes_[op_c] = new_set;

#ifdef LOG
        log_indent++;
#endif
        for (auto new_node : difference) {
#ifdef LOG
            log_nl() << "newly found node: " << new_node;
#endif
            if (auto new_in_node = new_node->isa<InNode>()) {
                if (op_index == 0) {
                    link(node, new_in_node);
                    add_to_worklist(CFPair(new_in_node, 0));
                }
            } else {
                auto new_out_node = new_node->as<OutNode>();
                if (op_index == 0) {
                    // make sure it's a 1-context-sensitive out_node
                    auto local_out_node = context_cf_node(node, new_out_node);
                    // all cf_nodes of args may be succs of local_out_node
                    auto lambda = node->lambda();
#ifdef LOG
                    log_indent++;
#endif
                    for (size_t arg_i = 1; arg_i < lambda->size() ; arg_i++) {
                        auto arg = lambda->op(arg_i);
                        // compute their nodes now, need to link
                        if (!arg->type().isa<FnType>())
                            continue;
#ifdef LOG
                        log_nl() << "arg " << arg->unique_name() << " (" << lambda->unique_name() << ", " << arg_i << "): ";
                        emit_type(arg->type(), log());
                        log_indent++;
#endif
                        for (auto arg_node : cf_nodes(CFPair(node, arg_i))) {
                            link(local_out_node, arg_node);
                            if (auto arg_in = arg_node->isa<InNode>()) {
                                // found a new predecessor of local_arg_node->lambda
#ifdef LOG
                                log_nl() << "recompute continuation of " << arg_in << ":";
#endif
                                add_to_worklist(CFPair(arg_in, 0));
                            }
                        }
#ifdef LOG
                        log_indent--;
#endif
                    }
#ifdef LOG
                    log_indent--;
#endif
                }
            }
        }
#ifdef LOG
        log_indent--;
#endif
    }
#ifdef LOG
    log_indent -= 2;
#endif
    return new_set;
}

void CFABuilder::run_cfa() {
#ifdef LOG
    log_nl() << "run_cfa()";
#endif
    if (!cfa().entry()->lambda()->empty())
        add_to_worklist(CFPair(cfa().entry(), 0));

    while (!worklist.empty()) {
        auto pair = pop(worklist);
#ifdef LOG
        log() << std::endl;
        log_nl() << "run_cfa() pop: " << pair.first->lambda()->unique_name() << ": ";
        emit_type(pair.first->lambda()->type(), log());
        log() << " - " << pair.second << " " << pair.first->lambda()->op(pair.second);
#endif
        cf_nodes(CFPair(pair.first, pair.second));
    }

#ifdef LOG
    log_nl() << "run_cfa: worklist empty, finished";
    log_nl() << "Final links after CFA:";
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
#endif
}

void CFABuilder::build_cfg() {
#ifdef LOG
    log_nl() << "build_cfg: adding necessary exit links";
#endif
    for (auto in : cfa().in_nodes()) {
        for (auto pair : in->out_nodes()) {
            auto out = pair.second;

            // only link those that don't have a successor
            // we don't care about this other control flow path as with other OutNodes
            // this is just to unify the end node as the canonical unique postdom
            if (out->succs().size() == 0) {
                link(out, cfa().exit());
            }
        }
    }

    // TODO old comment: link CFNodes not reachable from exit
    // TODO Klaas: for Outs see loop above, if there's an InNode w/o path to exit: endless loop?
    // TODO Roland: yes, endless loops.
    // HACK
#ifdef LOG
    log_nl() << "scope entry: " << cfa().entry();
    log_nl() << "cfa entry: " << cfa().exit();
#endif
    if (scope().entry()->empty()) {
        link(cfa().entry(), cfa().exit());
    }

#ifdef LOG
    bool error = false;
    for (auto in : cfa().in_nodes()) {
        if (in != cfa().entry() && in->preds_.size() == 0) {
            std::cerr << "missing predecessors: " << in->lambda()->unique_name() << std::endl;
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
