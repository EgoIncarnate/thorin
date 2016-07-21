#ifndef HENK_TABLE_NAME
#error "please define the type table name HENK_TABLE_NAME"
#endif

#ifndef HENK_TABLE_TYPE
#error "please define the type table type HENK_TABLE_TYPE"
#endif

#ifndef HENK_STRUCT_EXTRA_TYPE
#error "please define the type to unify StructTypes HENK_STRUCT_EXTRA_TYPE"
#endif

#ifndef HENK_STRUCT_EXTRA_NAME
#error "please define the name for HENK_STRUCT_EXTRA_TYPE: HENK_STRUCT_EXTRA_NAME"
#endif

#define HENK_UNDERSCORE(N) N##_
#define HENK_TABLE_NAME_          HENK_UNDERSCORE(HENK_TABLE_NAME)
#define HENK_STRUCT_EXTRA_NAME_ HENK_UNDERSCORE(HENK_STRUCT_EXTRA_NAME)

//------------------------------------------------------------------------------

class Continuation;
class Def;

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

class Var;
class HENK_TABLE_TYPE;

template<class T>
struct GIDHash {
    uint64_t operator()(T n) const { return n->gid(); }
};

template<class Key, class Value>
using GIDMap    = thorin::HashMap<const Key*, Value, GIDHash<const Key*>>;
template<class Key>
using GIDSet    = thorin::HashSet<const Key*, GIDHash<const Key*>>;

template<class To>
using DefMap      = GIDMap<Def, To>;
using DefSet      = GIDSet<Def>;
using Def2Def    = DefMap<const Def*>;

typedef thorin::ArrayRef<const Def*> Defs;

//------------------------------------------------------------------------------

enum class Sort {
    Term, Type, Kind
};

inline Sort prev_sort(Sort sort) {
    assert(sort != Sort::Term);
    return Sort(int(sort)-1);
}

inline Sort next_sort(Sort sort) {
    assert(sort != Sort::Kind);
    return Sort(int(sort)+1);
}

/// Base class for all \p Def%s.
class Def : public thorin::HasLocation, public thorin::Streamable, public thorin::MagicCast<Def> {
protected:
    Def(const Def&) = delete;
    Def& operator=(const Def&) = delete;

    /// Use for nominal @p Def%s.
    Def(HENK_TABLE_TYPE& table, int tag, Sort sort, const Def* type, size_t num_ops, const Location& loc, const std::string& name)
        : HENK_TABLE_NAME_(table)
        , tag_(tag)
        , sort_(sort)
        , ops_(num_ops)
        , gid_(gid_counter_++)
        , nominal_(true)
    {}

    /// Use for structural @p Def%s.
    Def(HENK_TABLE_TYPE& table, int tag, Sort sort, const Def* type, Defs ops, const Location& loc, const std::string& name)
        : HENK_TABLE_NAME_(table)
        , tag_(tag)
        , sort_(sort)
        , ops_(ops.size())
        , gid_(gid_counter_++)
        , nominal_(false)
    {
        for (size_t i = 0, e = num_ops(); i != e; ++i) {
            if (auto op = ops[i])
                set(i, op);
        }
    }

    void set(size_t i, const Def* def) {
        ops_[i] = def;
        order_  = std::max(order_, def->order());
        monomorphic_ &= def->is_monomorphic();
        known_       &= def->is_known();
    }

public:
    int tag() const { return tag_; }
    Sort sort() const { return sort_; }
    HENK_TABLE_TYPE& HENK_TABLE_NAME() const { return HENK_TABLE_NAME_; }

    Defs ops() const { return ops_; }
    const Def* op(size_t i) const;
    size_t num_ops() const { return ops_.size(); }
    bool empty() const { return ops_.empty(); }
    const Uses& uses() const { return uses_; }
    size_t num_uses() const { return uses().size(); }
    const Def* type() const { return type_; }
    const std::string& name() const { return name_; }

    void set_op(size_t i, const Def* def);
    void unset_op(size_t i);
    void unset_ops();
    Continuation* as_continuation() const;
    Continuation* isa_continuation() const;
    void replace(const Def*) const;

    bool is_nominal() const { return nominal_; }              ///< A nominal @p Def is always different from each other @p Def.
    bool is_hashed()  const { return hashed_; }               ///< This @p Def is already recorded inside of @p HENK_TABLE_TYPE.
    bool is_known()   const { return known_; }                ///< Deos this @p Def depend on any @p Unknown%s?
    bool is_monomorphic() const { return monomorphic_; }      ///< Does this @p Def not depend on any @p Var%s?.
    bool is_polymorphic() const { return !is_monomorphic(); } ///< Does this @p Def depend on any @p Var%s?.
    int order() const { return order_; }
    size_t gid() const { return gid_; }
    uint64_t hash() const { return is_hashed() ? hash_ : hash_ = vhash(); }
    virtual bool equal(const Def*) const;
    virtual bool is_outdated() const { return false; }
    virtual const Def* rebuild(Def2Def&) const { return this; }

    const Def* reduce(int, const Def*, Def2Def&) const;
    const Def* rebuild(HENK_TABLE_TYPE& to, Defs ops) const;
    const Def* rebuild(Defs ops) const { return rebuild(HENK_TABLE_NAME(), ops); }

    static size_t gid_counter() { return gid_counter_; }

protected:
    void set_type(const Def* type) { assert(type->is_nominal()); type_ = type; }

    virtual uint64_t vhash() const;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const = 0;
    thorin::Array<const Def*> reduce_ops(int, const Def*, Def2Def&) const;

    mutable uint64_t hash_ = 0;
    int order_ = 0;
    mutable bool hashed_      = false;
    mutable bool known_       = true;
    mutable bool monomorphic_ = true;

private:
    virtual const Def* vrebuild(HENK_TABLE_TYPE& to, Defs ops) const = 0;

    HENK_TABLE_TYPE& HENK_TABLE_NAME_;
    int tag_;
    Sort sort_;
    const Def* type_;
    thorin::Array<const Def*> ops_;
    mutable size_t gid_;
    static size_t gid_counter_;
    mutable bool nominal_;
    mutable uint32_t candidate_ = 0; // HACK for scope analysis
    mutable Uses uses_;

public:
    mutable std::string name_;

    template<class> friend class TableBase;
    friend class Scope;
    friend class Tracker;
};

class Lambda : public Def {
private:
    Lambda(HENK_TABLE_TYPE& table, Sort sort, const Def* var_type, const Def* body, const Location& loc, const std::string& name)
        : Def(table, Node_Lambda, sort, infer_type(table, sort, var_type, body, loc, name), {body}, loc, name)
    {}

    static const Def* infer_type(HENK_TABLE_TYPE& table, Sort sort, const Def* var_type, const Def* body, const Location& loc, const std::string& name);

public:
    const Def* body() const { return op(0); }
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Def* vrebuild(HENK_TABLE_TYPE& to, Defs ops) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    template<class> friend class TableBase;
};

class Star : public Def {
private:
    Star(HENK_TABLE_TYPE& table)
        : Def(table, Node_Star, Sort::Kind, nullptr, {}, Location(), "type")
    {}

public:
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Def* vrebuild(HENK_TABLE_TYPE& to, Defs ops) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    template<class> friend class TableBase;
};

class Var : public Def {
private:
    Var(HENK_TABLE_TYPE& table, int depth, const Def* type, const Location& loc, const std::string& name)
        : Def(table, Node_Var, type->sort(), type, {}, loc, name)
        , depth_(depth)
    {
        monomorphic_ = false;
    }

public:
    int depth() const { return depth_; }
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual uint64_t vhash() const override;
    virtual bool equal(const Def*) const override;
    virtual const Def* vrebuild(HENK_TABLE_TYPE& to, Defs ops) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    int depth_;

    template<class> friend class TableBase;
};

class App : public Def {
private:
    App(HENK_TABLE_TYPE& table, const Def* callee, const Def* arg, const Location& loc, const std::string& name)
        : Def(table, Node_App, infer_sort(callee), infer_type(table, callee, arg, loc, name), {callee, arg}, loc, name)
    {}
    App(HENK_TABLE_TYPE& table, const Def* callee, Defs args, const Location& loc, const std::string& name)
        : Def(table, Node_App, infer_sort(callee), infer_type(table, callee, args, loc, name), concat(callee, args), loc, name)
    {}

    static Sort infer_sort(const Def* callee);
    static const Def* infer_type(HENK_TABLE_TYPE& table, const Def* callee, const Def* arg, const Location& loc, const std::string& name);
    static const Def* infer_type(HENK_TABLE_TYPE& table, const Def* callee, Defs arg, const Location& loc, const std::string& name);

public:
    const Def* callee() const { return Def::op(0); }
    const Def* arg() const { return Def::op(1); }
    virtual std::ostream& stream(std::ostream&) const override;
    virtual const Def* vrebuild(HENK_TABLE_TYPE& to, Defs ops) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

private:
    mutable const Def* cache_ = nullptr;
    template<class> friend class TableBase;
};

class Tuple : public Def {
private:
    Tuple(HENK_TABLE_TYPE& table, Sort sort, Defs ops, const Location& loc, const std::string& name)
        : Def(table, Node_Tuple, sort, infer_type(table, ops, loc, name), ops, loc, name)
    {}

    static const Def* infer_type(HENK_TABLE_TYPE& table, Defs ops, const Location& loc, const std::string& name);

    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;
    virtual const Def* vrebuild(HENK_TABLE_TYPE& to, Defs ops) const override;

public:
    virtual std::ostream& stream(std::ostream&) const override;

    template<class> friend class TableBase;
};

class Error : public Def {
private:
    Error(HENK_TABLE_TYPE& table, const Def* type)
        : Def(table, Node_Error, prev_sort(type->sort()), type, {}, Location(), "<error>")
    {}

public:
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Def* vrebuild(HENK_TABLE_TYPE& to, Defs ops) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    template<class> friend class TableBase;
};

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

template<class HENK_TABLE_TYPE>
class TableBase {
private:
    HENK_TABLE_TYPE& HENK_TABLE_NAME() { return *static_cast<HENK_TABLE_TYPE*>(this); }

public:
    struct DefHash { uint64_t operator()(const Def* def) const { return def->hash(); } };
    struct DefEqual { bool operator()(const Def* d1, const Def* d2) const { return d2->equal(d1); } };
    typedef thorin::HashSet<const Def*, DefHash, DefEqual> DefSet;

    TableBase& operator=(const TableBase&);
    TableBase(const TableBase&);

    TableBase()
        : unit_(unify(new Tuple(HENK_TABLE_NAME(), Defs())))
        , type_error_(unify(new Error(HENK_TABLE_NAME())))
    {}
    virtual ~TableBase() { for (auto def : defs_) delete def; }

    const Star* star();
    const Var* var(int depth) { return unify(new Var(HENK_TABLE_NAME(), depth)); }
    const Lambda* lambda(Sort sort, const Def* var_type, const Def* body, const std::string& name) { return unify(new Lambda(HENK_TABLE_NAME(), sort, var_type, body, name)); }
    const Lambda* lambda(const Def* var_type, const Def* body, const std::string& name) { return unify(new Lambda(HENK_TABLE_NAME(), Sort::Term, var_type, body, name)); }
    const Lambda* pi(const Def* var_type, const Def* body, const std::string& name) { return unify(new Lambda(HENK_TABLE_NAME(), Sort::Type, var_type, body, name)); }
    const Def* app(const Def* callee, const Def* arg);
    const Def* app(const Def* callee, Defs args);
    const Tuple* tuple(Defs ops) { return unify(new Tuple(HENK_TABLE_NAME(), ops)); }
    const Tuple* unit() { return unit_; } ///< Returns unit, i.e., an empty @p Tuple.
    const Error* type_error() { return type_error_; }

    const DefSet& defs() const { return defs_; }

protected:
    const Def* unify_base(const Def* type);
    template<class T> const T* unify(const T* type) { return unify_base(type)->template as<T>(); }
    const Def* insert(const Def*);
    void destroy(const Def*);
    void destroy(const Def*, thorin::HashSet<const Def*>& done);

    Defs defs_;
    const Tuple* unit_; ///< tuple().
    const Error* type_error_;

    friend class Lambda;
};

//------------------------------------------------------------------------------

#undef HENK_STRUCT_EXTRA_NAME
#undef HENK_STRUCT_EXTRA_TYPE
#undef HENK_TABLE_NAME
#undef HENK_TABLE_TYPE
