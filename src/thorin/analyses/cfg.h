#ifndef THORIN_ANALYSES_CFG_H
#define THORIN_ANALYSES_CFG_H

#include <vector>

#include "thorin/lambda.h"
#include "thorin/analyses/scope.h"
#include "thorin/util/array.h"
#include "thorin/util/autoptr.h"
#include "thorin/util/indexmap.h"

namespace thorin {

class LoopTree;

template<bool> class DomTreeBase;
typedef DomTreeBase<true>  DomTree;
typedef DomTreeBase<false> PostDomTree;
template<bool> class CFG;
typedef CFG<true>  F_CFG;
typedef CFG<false> B_CFG;

class CFNode {
public:
    CFNode(Lambda* lambda)
        : lambda_(lambda)
    {}

    Lambda* lambda() const { return lambda_; }

private:
    ArrayRef<const CFNode*> preds() const { return preds_; }
    ArrayRef<const CFNode*> succs() const { return succs_; }
    void link(const CFNode* other) const {
        assert(this->lambda()->intrinsic() != Intrinsic::EndScope);
        this->succs_.push_back(other);
        other->preds_.push_back(this);
    }

    Lambda* lambda_;
    mutable std::vector<const CFNode*> preds_;
    mutable std::vector<const CFNode*> succs_;

    friend class CFABuilder;
    friend class CFA;
};

class CFA {
public:
    CFA(const CFA&) = delete;
    CFA& operator= (CFA) = delete;

    explicit CFA(const Scope& scope);

    const Scope& scope() const { return scope_; }
    size_t size() const { return nodes_.size(); }
    const Scope::Map<const CFNode*>& nodes() const { return nodes_; }
    ArrayRef<const CFNode*> preds(Lambda* lambda) const { return nodes_[lambda]->preds(); }
    ArrayRef<const CFNode*> succs(Lambda* lambda) const { return nodes_[lambda]->succs(); }
    size_t num_preds(Lambda* lambda) const { return preds(lambda).size(); }
    size_t num_succs(Lambda* lambda) const { return succs(lambda).size(); }
    const CFNode* entry() const { return nodes_.entry(); }
    const CFNode* exit() const { return nodes_.exit(); }
    const F_CFG* f_cfg() const;
    const B_CFG* b_cfg() const;
    const DomTree* domtree() const;
    const PostDomTree* postdomtree() const;
    const LoopTree* looptree() const;
    const CFNode* lookup(Lambda* lambda) const { return find(nodes_, lambda); }

private:
    const CFNode* _lookup(Lambda* lambda) const { return nodes_[lambda]; }
    const Scope& scope_;
    Scope::Map<const CFNode*> nodes_;
    mutable AutoPtr<const F_CFG> f_cfg_;
    mutable AutoPtr<const B_CFG> b_cfg_;
    mutable AutoPtr<const LoopTree> looptree_;

    friend class CFABuilder;
};

template<bool forward>
class CFG {
public:
    template<class Value>
    using Map = IndexMap<CFG<forward>, const CFNode*, Value>;
    using Set = IndexSet<CFG<forward>, const CFNode*>;

    CFG(const CFG&) = delete;
    CFG& operator= (CFG) = delete;

    explicit CFG(const CFA&);

    const CFA& cfa() const { return cfa_; }
    size_t size() const { return indices_.size(); }
    ArrayRef<const CFNode*> preds(const CFNode* n) const { return forward ? cfa().preds(n->lambda()) : cfa().succs(n->lambda()); }
    ArrayRef<const CFNode*> succs(const CFNode* n) const { return forward ? cfa().succs(n->lambda()) : cfa().preds(n->lambda()); }
    size_t num_preds(const CFNode* n) const { return preds(n).size(); }
    size_t num_succs(const CFNode* n) const { return succs(n).size(); }
    const CFNode* entry() const { return forward ? cfa().entry() : cfa().exit();  }
    const CFNode* exit()  const { return forward ? cfa().exit()  : cfa().entry(); }
    size_t index(const CFNode* n) const { return indices_[n->lambda()]; }
    /// All lambdas within this scope in reverse post-order.
    ArrayRef<const CFNode*> rpo() const { return rpo_.array(); }
    const CFNode* rpo(size_t i) const { return rpo_.array()[i]; }
    /// Like \p rpo() but without \p entry()
    ArrayRef<const CFNode*> body() const { return rpo().slice_from_begin(1); }
    const CFNode* lookup(Lambda* lambda) const { return cfa().lookup(lambda); }
    const DomTreeBase<forward>* domtree() const;

    typedef ArrayRef<const CFNode*>::const_iterator const_iterator;
    const_iterator begin() const { return rpo().begin(); }
    const_iterator end() const { return rpo().end(); }

private:
    size_t& _index(const CFNode* n) { return indices_[n->lambda()]; }
    size_t post_order_number(const CFNode*, size_t);

    const CFA& cfa_;
    Scope::Map<size_t> indices_;
    Map<const CFNode*> rpo_;
    mutable AutoPtr<const DomTreeBase<forward>> domtree_;
};

}

#endif
