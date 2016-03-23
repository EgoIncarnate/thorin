#ifndef HENK_TABLE_NAME
#error "please define the type table name HENK_TABLE_NAME"
#endif

#ifndef HENK_TABLE_TYPE
#error "please define the type table type HENK_TABLE_TYPE"
#endif

#ifndef HENK_STRUCT_UNIFIER_NAME
#error "please define the name for HENK_STRUCT_UNIFIER_TYPE: HENK_STRUCT_UNIFIER_NAME"
#endif

size_t Type::gid_counter_ = 1;

//------------------------------------------------------------------------------

const Lambda* close(const Lambda*& lambda, const Type* body) {
    assert(lambda->body() == nullptr);
    const_cast<Lambda*&>(lambda)->set(0, body);

    std::stack<const Type*> stack;
    TypeSet done;
    int depth = 0;

    auto push = [&](const Type* type) {
        if (!type->is_closed() && !done.contains(type)) {
            if (auto var = type->isa<Var>()) {
                if (var->lambda() == lambda) {
                    var->closed_ = true;
                    var->depth_  = depth;
                }
                done.insert(var);
            } else {
                if (type->isa<Lambda>())
                    ++depth;
                done.insert(type);
                stack.push(type);
                return true;
            }
        }
        return false;
    };

    push(lambda);

    // TODO this is potentially quadratic when closing n types
    while (!stack.empty()) {
        auto type = stack.top();

        bool todo = false;
        for (size_t i = 0, e = type->size(); i != e; ++i)
            todo |= push(type->arg(i));

        if (!todo) {
            if (type->isa<Lambda>())
                --depth;
            stack.pop();
            type->closed_ = true;
            for (size_t i = 0, e = type->size(); i != e && type->closed_; ++i)
                type->closed_ &= type->arg(i)->is_closed();
        }
    }

    assert(depth == 0);
    return lambda->HENK_TABLE_NAME().unify(lambda);
}

//------------------------------------------------------------------------------

/*
 * hash
 */

uint64_t Type::vhash() const {
    uint64_t seed = thorin::hash_combine(thorin::hash_begin(int(kind())), size());
    for (auto arg : args_)
        seed = thorin::hash_combine(seed, arg->hash());
    return seed;
}

uint64_t Var::vhash() const {
    return thorin::hash_combine(thorin::hash_begin(int(kind())), depth());
}

uint64_t StructType::vhash() const {
    return thorin::hash_combine(thorin::hash_begin(int(kind())), int(kind()), int(size()), HENK_STRUCT_UNIFIER_NAME());
}

//------------------------------------------------------------------------------

/*
 * equal
 */

bool Type::equal(const Type* other) const {
    bool result = this->kind() == other->kind() && this->size() == other->size()
        && this->is_monomorphic() == other->is_monomorphic();

    if (result) {
        for (size_t i = 0, e = size(); result && i != e; ++i) {
            //result &= this->arg(i)->is_hashed()
                //? this->arg(i) == other->arg(i)
                //: this->arg(i)->equal(other->arg(i));
            bool asdf;
            if (this->arg(i)->is_hashed())
                asdf = this->arg(i) == other->arg(i);
            else {
                asdf = this->arg(i)->equal(other->arg(i));
                WLOG("EQUAL");
            }
            result &= asdf;
        }
    }

    return result;
}

bool Var::equal(const Type* other) const {
    return other->isa<Var>() ? this->as<Var>()->depth() == other->as<Var>()->depth() : false;
}

bool StructType::equal(const Type* other) const {
    if (auto other_struct_type = other->isa<StructType>())
        return this->HENK_STRUCT_UNIFIER_NAME() == other_struct_type->HENK_STRUCT_UNIFIER_NAME();
    return false;
}

//------------------------------------------------------------------------------

/*
 * rebuild
 */

const Type* Type::rebuild(HENK_TABLE_TYPE& to, Types args) const {
    assert(size() == args.size());
    if (args.empty() && &HENK_TABLE_NAME() == &to)
        return this;
    return vrebuild(to, args);
}

const Type* StructType::vrebuild(HENK_TABLE_TYPE& to, Types args) const {
    auto ntype = to.struct_type(HENK_STRUCT_UNIFIER_NAME(), args.size());
    for (size_t i = 0, e = args.size(); i != e; ++i)
        const_cast<StructType*>(ntype)->set(i, args[i]);
    return ntype;
}

const Type* Application::vrebuild(HENK_TABLE_TYPE& to, Types args) const { return to.application(args[0], args[1]); }
const Type* TupleType  ::vrebuild(HENK_TABLE_TYPE& to, Types args) const { return to.tuple_type(args); }
const Type* Var        ::vrebuild(HENK_TABLE_TYPE&,    Types     ) const { THORIN_UNREACHABLE; }
const Type* Lambda     ::vrebuild(HENK_TABLE_TYPE&,    Types     ) const { THORIN_UNREACHABLE; }

//------------------------------------------------------------------------------

/*
 * reduce
 */

const Type* Type::reduce(int depth, const Type* type, Type2Type& map) const {
    if (auto result = find(map, this))
        return result;
    if (is_monomorphic())
        return this;
    return map[this] = vreduce(depth, type, map);
}

Array<const Type*> Type::reduce_args(int depth, const Type* type, Type2Type& map) const {
    Array<const Type*> result(size());
    for (size_t i = 0, e = size(); i != e; ++i)
        result[i] = arg(i)->reduce(depth, type, map);
    return result;
}

const Type* Lambda::vreduce(int depth, const Type* type, Type2Type& map) const {
    return HENK_TABLE_NAME().lambda(body()->reduce(depth+1, type, map), name());
}

const Type* Var::vreduce(int depth, const Type* type, Type2Type&) const {
    if (this->depth() == depth)
        return type;
    else if (this->depth() < depth)
        return HENK_TABLE_NAME().var(depth-1);  // shift by one
    else
        return this;                            // this is a free variable - don't adjust
}

const Type* StructType::vreduce(int, const Type*, Type2Type&) const {
    assert(false && "TODO");
}

const Type* Application::vreduce(int depth, const Type* type, Type2Type& map) const {
    auto args = reduce_args(depth, type, map);
    return HENK_TABLE_NAME().application(args[0], args[1]);
}

const Type* TupleType::vreduce(int depth, const Type* type, Type2Type& map) const {
    return HENK_TABLE_NAME().tuple_type(reduce_args(depth, type, map));
}

//------------------------------------------------------------------------------

template<class T>
const Type* TypeTableBase<T>::application(const Type* callee, const Type* arg) {
    auto application = unify(new Application(HENK_TABLE_NAME(), callee, arg));

    if (application->is_hashed()) {
        if (!application->cache_) {
            if (auto lambda = application->callee()->template isa<Lambda>()) {
                Type2Type map;
                application->cache_ = lambda->body()->reduce(1, arg, map);
            } else
                application->cache_ = application;
        }
        return application->cache_;
    }

    return application;
}

template<class T>
const Type* TypeTableBase<T>::unify_base(const Type* type) {
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
const Type* TypeTableBase<T>::insert(const Type* type) {
    for (auto arg : type->args()) {
        if (!arg->is_hashed())
            insert(arg);
    }

    const auto& p = types_.insert(type);
    assert_unused(p.second && "hash/equal broken");
    assert(!type->is_hashed());
    type->hashed_ = true;
    return type;
}

template<class T>
void TypeTableBase<T>::destroy(const Type* type) {
    thorin::HashSet<const Type*> done;
    destroy(type, done);
}

template<class T>
void TypeTableBase<T>::destroy(const Type* type, thorin::HashSet<const Type*>& done) {
    if (!done.contains(type) && !type->is_hashed()) {
        done.insert(type);
        for (auto arg : type->args())
            destroy(arg, done);
        delete type;
    }
}

template class TypeTableBase<HENK_TABLE_TYPE>;

//------------------------------------------------------------------------------

#undef HENK_STRUCT_UNIFIER_NAME
#undef HENK_TABLE_NAME
#undef HENK_TABLE_TYPE
