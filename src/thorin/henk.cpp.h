#ifndef HENK_TABLE_NAME
#error "please define the type table name HENK_TABLE_NAME"
#endif

#ifndef HENK_TABLE_TYPE
#error "please define the type table type HENK_TABLE_TYPE"
#endif

#ifndef HENK_STRUCT_EXTRA_NAME
#error "please define the name for HENK_STRUCT_EXTRA_TYPE: HENK_STRUCT_EXTRA_NAME"
#endif

#ifndef HENK_STRUCT_EXTRA_TYPE
#error "please define the type to unify StructTypes HENK_STRUCT_EXTRA_TYPE"
#endif

size_t Def::gid_counter_ = 1;

//------------------------------------------------------------------------------

const Def* Def::op(size_t i) const { return i < num_ops() ? ops()[i] : HENK_TABLE_NAME().type_error(); }

//------------------------------------------------------------------------------

/*
 * hash
 */

uint64_t Def::vhash() const {
    if (is_nominal())
        return gid();

    uint64_t seed = thorin::hash_combine(thorin::hash_begin(int(kind())), num_ops(), type() ? type()->gid() : 0);
    for (auto op : ops_)
        seed = thorin::hash_combine(seed, op->hash());
    return seed;
}

uint64_t Var::vhash() const {
    return thorin::hash_combine(thorin::hash_begin(int(kind())), depth());
}

//------------------------------------------------------------------------------

/*
 * equal
 */

bool Def::equal(const Def* other) const {
    if (is_nominal())
        return this == other;

    bool result = this->kind() == other->kind() && this->num_ops() == other->num_ops()
        && this->is_monomorphic() == other->is_monomorphic();

    if (result) {
        for (size_t i = 0, e = num_ops(); result && i != e; ++i) {
            assert(this->op(i)->is_hashed() && other->op(i)->is_hashed());
            result &= this->op(i) == other->op(i);
        }
    }

    return result;
}

bool Var::equal(const Def* other) const {
    return other->isa<Var>() ? this->as<Var>()->depth() == other->as<Var>()->depth() : false;
}

//------------------------------------------------------------------------------

/*
 * rebuild
 */

const Def* Def::rebuild(HENK_TABLE_TYPE& to, Defs ops) const {
    assert(num_ops() == ops.size());
    if (ops.empty() && &HENK_TABLE_NAME() == &to)
        return this;
    return vrebuild(to, ops);
}

const Def* StructType::vrebuild(HENK_TABLE_TYPE& to, Defs ops) const {
    auto ntype = to.struct_type(HENK_STRUCT_EXTRA_NAME(), ops.size());
    for (size_t i = 0, e = ops.size(); i != e; ++i)
        const_cast<StructType*>(ntype)->set(i, ops[i]);
    return ntype;
}

const Def* App      ::vrebuild(HENK_TABLE_TYPE& to, Defs ops) const { return to.app(ops[0], ops[1]); }
const Def* Tuple    ::vrebuild(HENK_TABLE_TYPE& to, Defs ops) const { return to.tuple(ops); }
const Def* Lambda   ::vrebuild(HENK_TABLE_TYPE& to, Defs ops) const { return to.lambda(ops[0], name()); }
const Def* Var      ::vrebuild(HENK_TABLE_TYPE& to, Defs    ) const { return to.var(depth()); }
const Def* TypeError::vrebuild(HENK_TABLE_TYPE&,    Defs    ) const { return this; }

//------------------------------------------------------------------------------

/*
 * reduce
 */

const Def* Type::reduce(int depth, const Def* type, Def2Def& map) const {
    if (auto result = find(map, this))
        return result;
    if (is_monomorphic())
        return this;
    return map[this] = vreduce(depth, type, map);
}

Array<const Def*> Type::reduce_ops(int depth, const Def* type, Def2Def& map) const {
    Array<const Def*> result(num_ops());
    for (size_t i = 0, e = num_ops(); i != e; ++i)
        result[i] = op(i)->reduce(depth, type, map);
    return result;
}

const Def* Lambda::vreduce(int depth, const Def* type, Def2Def& map) const {
    return HENK_TABLE_NAME().lambda(body()->reduce(depth+1, type, map), name());
}

const Def* Var::vreduce(int depth, const Def* type, Def2Def&) const {
    if (this->depth() == depth)
        return type;
    else if (this->depth() > depth)
        return HENK_TABLE_NAME().var(this->depth()-1);  // this is a free variable - shift by one
    else
        return this;                                    // this variable is not free - don't adjust
}

const Def* StructType::vreduce(int depth, const Def* type, Def2Def& map) const {
    auto struct_type = HENK_TABLE_NAME().struct_type(HENK_STRUCT_EXTRA_NAME(), num_ops());
    map[this] = struct_type;
    auto ops = reduce_ops(depth, type, map);

    for (size_t i = 0, e = num_ops(); i != e; ++i)
        struct_type->set(i, ops[i]);

    return struct_type;
}

const Def* App::vreduce(int depth, const Def* type, Def2Def& map) const {
    auto ops = reduce_ops(depth, type, map);
    return HENK_TABLE_NAME().app(ops[0], ops[1]);
}

const Def* Tuple::vreduce(int depth, const Def* type, Def2Def& map) const {
    return HENK_TABLE_NAME().tuple(reduce_ops(depth, type, map));
}

const Def* TypeError::vreduce(int, const Def*, Def2Def&) const { return this; }

//------------------------------------------------------------------------------

template<class T>
const StructType* TableBase<T>::struct_type(HENK_STRUCT_EXTRA_TYPE HENK_STRUCT_EXTRA_NAME, size_t size) {
    auto type = new StructType(HENK_TABLE_NAME(), HENK_STRUCT_EXTRA_NAME, size);
    const auto& p = types_.insert(type);
    assert_unused(p.second && "hash/equal broken");
    assert(!type->is_hashed());
    type->hashed_ = true;
    return type;
}

template<class T>
const Def* TableBase<T>::app(const Def* callee, const Def* op) {
    auto app = unify(new App(HENK_TABLE_NAME(), callee, op));

    if (app->is_hashed()) {
        if (auto cache = app->cache_)
            return cache;
        if (auto lambda = app->callee()->template isa<Lambda>()) {
            Def2Def map;
            return app->cache_ = lambda->body()->reduce(1, op, map);
        } else {
            return app->cache_ = app;
        }
    }

    return app;
}

template<class T>
const Def* TableBase<T>::unify_base(const Def* type) {
    if (type->is_hashed() || !type->is_closed())
        return type;

    auto i = types_.find(type);
    if (i != types_.end()) {
        destroy(type);
        type = *i;
        assert(type->is_hashed());
        return type;
    }

    return insert(type);
}

template<class T>
const Def* TableBase<T>::insert(const Def* type) {
    for (auto op : type->ops()) {
        if (!op->is_hashed())
            insert(op);
    }

    const auto& p = types_.insert(type);
    assert_unused(p.second && "hash/equal broken");
    assert(!type->is_hashed());
    type->hashed_ = true;
    return type;
}

template<class T>
void TableBase<T>::destroy(const Def* type) {
    thorin::HashSet<const Def*> done;
    destroy(type, done);
}

template<class T>
void TableBase<T>::destroy(const Def* type, thorin::HashSet<const Def*>& done) {
    if (!done.contains(type) && !type->is_hashed()) {
        done.insert(type);
        for (auto op : type->ops())
            destroy(op, done);
        delete type;
    }
}

template class TableBase<HENK_TABLE_TYPE>;

//------------------------------------------------------------------------------

#undef HENK_STRUCT_EXTRA_NAME
#undef HENK_STRUCT_EXTRA_TYPE
#undef HENK_TABLE_NAME
#undef HENK_TABLE_TYPE
