#ifndef THORIN_DEF_H
#define THORIN_DEF_H

#include "thorin/util/location.h"

#if 0
#include <string>
#include <vector>

#include "thorin/enums.h"
#include "thorin/type.h"
#include "thorin/util/autoptr.h"
#include "thorin/util/location.h"

namespace thorin {

//------------------------------------------------------------------------------

class Def;
class Tracker;
class Continuation;
class PrimOp;
class Use;
class World;

typedef ArrayRef<const Def*> Defs;

//------------------------------------------------------------------------------

/**
 * References a user.
 * A \p Def \c u which uses \p Def \c d as \c i^th operand is a \p Use with \p index_ \c i of \p Def \c d.
 */
class Use {
public:
    Use() {}
    Use(size_t index, const Def* def)
        : index_(index)
        , def_(def)
    {}

    size_t index() const { return index_; }
    const Def* def() const { return def_; }
    operator const Def*() const { return def_; }
    const Def* operator->() const { return def_; }
    bool operator==(Use other) const { return this->def() == other.def() && this->index() == other.index(); }

private:
    size_t index_;
    const Def* def_;
};

//------------------------------------------------------------------------------

struct UseHash {
    inline uint64_t operator()(Use use) const;
};

typedef HashSet<Use, UseHash> Uses;

template<class To>
using DefMap  = GIDMap<Def, To>;
using DefSet  = GIDSet<Def>;
using Def2Def = DefMap<const Def*>;

std::ostream& operator<<(std::ostream&, const Def*);
std::ostream& operator<<(std::ostream&, Use);

//------------------------------------------------------------------------------

/**
 * The base class for all three kinds of Definitions in AnyDSL.
 * These are:
 * - \p PrimOp%s
 * - \p Param%s and
 * - \p Continuation%s.
 */
class Def : public HasLocation, public MagicCast<Def>, public Streamable {
private:
    Def& operator=(const Def&); ///< Do not copy-assign a \p Def instance.
    Def(const Def&);              ///< Do not copy-construct a \p Def.

protected:
    Def(NodeKind kind, const Type* type, size_t size, const Location& loc, const std::string& name);
    virtual ~Def() {}

    void clear_type() { type_ = nullptr; }
    void set_type(const Type* type) { assert(type->is_closed()); type_ = type; }
    void unregister_use(size_t i) const;
    void unregister_uses() const;
    void resize(size_t n) { ops_.resize(n, nullptr); }

public:
    NodeKind kind() const { return kind_; }
    size_t num_ops() const { return ops_.size(); }
    bool empty() const { return ops_.empty(); }
    void set_op(size_t i, const Def* def);
    void unset_op(size_t i);
    void unset_ops();
    Continuation* as_continuation() const;
    Continuation* isa_continuation() const;
    void dump() const;
    const Uses& uses() const { return uses_; }
    size_t num_uses() const { return uses().size(); }
    size_t gid() const { return gid_; }
    std::string unique_name() const;
    const Type* type() const { return type_; }
    int order() const { return type()->order(); }
    World& world() const;
    Defs ops() const { return ops_; }
    const Def* op(size_t i) const { assert(i < ops().size() && "index out of bounds"); return ops_[i]; }
    void replace(const Def*) const;

    virtual bool is_outdated() const { return false; }
    virtual const Def* rebuild(Def2Def&) const { return this; }
    virtual std::ostream& stream(std::ostream&) const;
    static size_t gid_counter() { return gid_counter_; }

private:
    const NodeKind kind_;
    std::vector<const Def*> ops_;
    const Type* type_;
    mutable Uses uses_;
    const size_t gid_;
    mutable uint32_t candidate_ = 0;
    static size_t gid_counter_;

public:
    mutable std::string name; ///< Just do what ever you want with this field.

    friend class Defx;
    friend class PrimOp;
    friend class Scope;
    friend class World;
    friend class Cleaner;
    friend class Tracker;
};

/// Returns the vector length. Raises an assertion if type of this is not a \p VectorType.
size_t vector_length(const Def*);
bool is_const(const Def*);
bool is_primlit(const Def*, int);
bool is_minus_zero(const Def*);
inline bool is_mem        (const Def* def) { return def->type()->isa<MemType>(); }
inline bool is_zero       (const Def* def) { return is_primlit(def, 0); }
inline bool is_one        (const Def* def) { return is_primlit(def, 1); }
inline bool is_allset     (const Def* def) { return is_primlit(def, -1); }
inline bool is_bitop      (const Def* def) { return thorin::is_bitop(def->kind()); }
inline bool is_shift      (const Def* def) { return thorin::is_shift(def->kind()); }
inline bool is_not        (const Def* def) { return def->kind() == Node_xor && is_allset(def->op(0)); }
inline bool is_minus      (const Def* def) { return def->kind() == Node_sub && is_minus_zero(def->op(0)); }
inline bool is_div_or_rem (const Def* def) { return thorin::is_div_or_rem(def->kind()); }
inline bool is_commutative(const Def* def) { return thorin::is_commutative(def->kind()); }
inline bool is_associative(const Def* def) { return thorin::is_associative(def->kind()); }

namespace detail {
    inline std::ostream& stream(std::ostream& os, const Def* def) { return def->stream(os); }
    inline std::ostream& stream(std::ostream& os, const Type* type) { return type->stream(os); }
}

inline std::ostream& operator<<(std::ostream& os, const Def* def) { return def->stream(os); }
inline std::ostream& operator<<(std::ostream& os, const Type* type) { return type->stream(os); }
inline std::ostream& operator<<(std::ostream& os, Use use) { return use->stream(os); }

//------------------------------------------------------------------------------

class Tracker {
public:
    Tracker()
        : def_(nullptr)
    {}
    Tracker(const Def* def)
        : def_(def)
    {
        if (def) {
            put(*this);
            verify();
        }
    }
    Tracker(const Tracker& other)
        : def_(other)
    {
        if (other) {
            put(*this);
            verify();
        }
    }
    Tracker(Tracker&& other)
        : def_(*other)
    {
        if (other) {
            other.unregister();
            other.def_ = nullptr;
            put(*this);
            verify();
        }
    }
    ~Tracker() { if (*this) unregister(); }

    const Def* operator*() const { return def_; }
    bool operator==(const Tracker& other) const { return this->def_ == other.def_; }
    bool operator!=(const Tracker& other) const { return this->def_ != other.def_; }
    bool operator==(const Def* def) const { return this->def_ == def; }
    bool operator!=(const Def* def) const { return this->def_ != def; }
    const Def* operator->() const { return **this; }
    operator const Def*() const { return **this; }
    explicit operator bool() { return def_; }
    Tracker& operator=(Tracker other) { swap(*this, other); return *this; }

    friend void swap(Tracker& t1, Tracker& t2) {
        using std::swap;

        if (t1 != t2) {
            if (t1) {
                if (t2) {
                    t1.update(t2);
                    t2.update(t1);
                } else {
                    t1.update(t2);
                }
            } else {
                assert(!t1 && t2);
                t2.update(t1);
            }

            std::swap(t1.def_, t2.def_);
        } else {
            t1.verify();
            t2.verify();
        }
    }

private:
    HashSet<Tracker*>& trackers(const Def* def);
    void verify() { assert(!def_ || trackers(def_).contains(this)); }
    void put(Tracker& other) {
        auto p = trackers(def_).insert(&other);
        assert_unused(p.second && "couldn't insert tracker");
    }

    void unregister() {
        assert(trackers(def_).contains(this) && "tracker not found");
        trackers(def_).erase(this);
    }

    void update(Tracker& other) {
        unregister();
        put(other);
    }

    mutable const Def* def_;
    friend void Def::replace(const Def*) const;
};

//------------------------------------------------------------------------------

uint64_t UseHash::operator()(Use use) const {
    return hash_combine(hash_begin(use->gid()), use.index());
}

template<>
struct Hash<Defs> {
    uint64_t operator()(Defs defs) const {
        uint64_t seed = hash_begin(defs.size());
        for (auto def : defs)
            seed = hash_combine(seed, def ? def->gid() : uint64_t(-1));
        return seed;
    }
};

template<>
struct Hash<Array<const Def*>> {
    uint64_t operator()(const Array<const Def*> defs) const { return Hash<Defs>()(defs); }
};

//------------------------------------------------------------------------------

}

#endif
#endif
