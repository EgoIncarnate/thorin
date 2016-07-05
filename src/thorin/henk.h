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

class Def;
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

/// Base class for all \p Def%s.
class Def : public thorin::HasLocation, public thorin::Streamable, public thorin::MagicCast<Def> {
protected:
    Def(const Def&) = delete;
    Def& operator=(const Def&) = delete;

    Def(HENK_TABLE_TYPE& table, const Def* type, int kind, size_t num_ops, const Location& loc, const char* name)
        : HENK_TABLE_NAME_(table)
        , kind_(kind)
        , ops_(num_ops)
        , gid_(gid_counter_++)
        , nominal_(true)
    {
    }

    Def(HENK_TABLE_TYPE& table, const Def* type, int kind, Defs ops, const Location& loc, const char* name)
        : HENK_TABLE_NAME_(table)
        , kind_(kind)
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
    int kind() const { return kind_; }
    HENK_TABLE_TYPE& HENK_TABLE_NAME() const { return HENK_TABLE_NAME_; }

    Defs ops() const { return ops_; }
    const Def* op(size_t i) const;
    size_t num_ops() const { return ops_.size(); }
    bool empty() const { return ops_.empty(); }
    const Def* type() const { return type_; }
    const char* name() const { return name_; }

    bool is_nominal() const { return nominal_; }              ///< A nominal @p Def is always different from each other @p Def.
    bool is_hashed()  const { return hashed_; }               ///< This @p Def is already recorded inside of @p HENK_TABLE_TYPE.
    bool is_known()   const { return known_; }                ///< Deos this @p Def depend on any @p Unknown%s?
    bool is_monomorphic() const { return monomorphic_; }      ///< Does this @p Def not depend on any @p Var%s?.
    bool is_polymorphic() const { return !is_monomorphic(); } ///< Does this @p Def depend on any @p Var%s?.
    int order() const { return order_; }
    size_t gid() const { return gid_; }
    uint64_t hash() const { return is_hashed() ? hash_ : hash_ = vhash(); }
    virtual bool equal(const Def*) const;

    const Def* reduce(int, const Def*, Def2Def&) const;
    const Def* rebuild(HENK_TABLE_TYPE& to, Defs ops) const;
    const Def* rebuild(Defs ops) const { return rebuild(HENK_TABLE_NAME(), ops); }

    static size_t gid_counter() { return gid_counter_; }

protected:
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
    int kind_;
    const Def* type_;
    thorin::Array<const Def*> ops_;
    const char* name_;
    mutable size_t gid_;
    static size_t gid_counter_;
    mutable bool nominal_;

    template<class> friend class TableBase;
};

class Lambda : public Def {
private:
    Lambda(HENK_TABLE_TYPE& table, const Def* var_type, const Def* body, const Location& loc, const char* name)
        : Def(table, Node_Lambda, {body}, loc, name)
    {}

public:
    const Def* body() const { return op(0); }
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Def* vrebuild(HENK_TABLE_TYPE& to, Defs ops) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    const char* name_;

    template<class> friend class TableBase;
};

class Var : public Def {
private:
    Var(HENK_TABLE_TYPE& table, int depth)
        : Def(table, Node_Var, {})
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
    App(HENK_TABLE_TYPE& table, const Def* callee, const Def* arg)
        : Def(table, Node_App, {callee, arg})
    {}

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
    Tuple(HENK_TABLE_TYPE& table, Defs ops)
        : Def(table, Node_Tuple, ops)
    {}

    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;
    virtual const Def* vrebuild(HENK_TABLE_TYPE& to, Defs ops) const override;

public:
    virtual std::ostream& stream(std::ostream&) const override;

    template<class> friend class TableBase;
};

class StructType : public Def {
private:
    StructType(HENK_TABLE_TYPE& table, HENK_STRUCT_EXTRA_TYPE HENK_STRUCT_EXTRA_NAME, size_t size)
        : Def(table, Node_StructType, thorin::Array<const Def*>(size))
        , HENK_STRUCT_EXTRA_NAME_(HENK_STRUCT_EXTRA_NAME)
    {
        nominal_ = true;
    }

public:
    HENK_STRUCT_EXTRA_TYPE HENK_STRUCT_EXTRA_NAME() const { return HENK_STRUCT_EXTRA_NAME_; }
    void set(size_t i, const Def* type) const { return const_cast<StructType*>(this)->Def::set(i, type); }

private:
    virtual const Def* vrebuild(HENK_TABLE_TYPE& to, Defs ops) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;
    virtual std::ostream& stream(std::ostream&) const override;

    HENK_STRUCT_EXTRA_TYPE HENK_STRUCT_EXTRA_NAME_;

    template<class> friend class TableBase;
};

class TypeError : public Def {
private:
    TypeError(HENK_TABLE_TYPE& table)
        : Def(table, Node_TypeError, {})
    {}

public:
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Def* vrebuild(HENK_TABLE_TYPE& to, Defs ops) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    template<class> friend class TableBase;
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
        , type_error_(unify(new TypeError(HENK_TABLE_NAME())))
    {}
    virtual ~TableBase() { for (auto def : defs_) delete def; }

    const Var* var(int depth) { return unify(new Var(HENK_TABLE_NAME(), depth)); }
    const Lambda* lambda(const char* name) { return new Lambda(HENK_TABLE_NAME(), name); }
    const Lambda* lambda(const Def* body, const char* name) { return unify(new Lambda(HENK_TABLE_NAME(), body, name)); }
    const Def* app(const Def* callee, const Def* arg);
    const Tuple* tuple(Defs ops) { return unify(new Tuple(HENK_TABLE_NAME(), ops)); }
    const Tuple* unit() { return unit_; } ///< Returns unit, i.e., an empty @p Tuple.
    const StructType* struct_type(HENK_STRUCT_EXTRA_TYPE HENK_STRUCT_EXTRA_NAME, size_t size);
    const TypeError* type_error() { return type_error_; }

    const DefSet& defs() const { return defs_; }

protected:
    const Def* unify_base(const Def* type);
    template<class T> const T* unify(const T* type) { return unify_base(type)->template as<T>(); }
    const Def* insert(const Def*);
    void destroy(const Def*);
    void destroy(const Def*, thorin::HashSet<const Def*>& done);

    Defs defs_;
    const Tuple* unit_; ///< tuple().
    const TypeError* type_error_;

    friend class Lambda;
};

//------------------------------------------------------------------------------

#undef HENK_STRUCT_EXTRA_NAME
#undef HENK_STRUCT_EXTRA_TYPE
#undef HENK_TABLE_NAME
#undef HENK_TABLE_TYPE
