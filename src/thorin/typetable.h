#ifndef THORIN_TYPETABLE_H
#define THORIN_TYPETABLE_H

#include "thorin/type.h"

namespace thorin {

class TypeTable {
public:
    struct TypeHash { uint64_t operator () (const Type* t) const { return t->hash(); } };
    struct TypeEqual { bool operator () (const Type* t1, const Type* t2) const { return t1->equal(t2); } };
    typedef HashSet<const Type*, TypeHash, TypeEqual> TypeSet;

    TypeTable& operator = (const TypeTable&);
    TypeTable(const TypeTable&);

    TypeTable();
    virtual ~TypeTable();

#define THORIN_ALL_TYPE(T, M) const PrimType* type_##T(size_t length = 1) { \
    return length == 1 ? T##_ : new PrimType(*this, PrimType_##T, length); \
}
#include "thorin/tables/primtypetable.h"

    /// Get PrimType.
    const PrimType*    type(PrimTypeKind kind, size_t length = 1) {
        size_t i = kind - Begin_PrimType;
        assert(i < (size_t) Num_PrimTypes);
        return length == 1 ? primtypes_[i] : unify(new PrimType(*this, kind, length));
    }
    const MemType*     mem_type() const { return mem_; }
    const FrameType*   frame_type() const { return frame_; }
    const PtrType*     ptr_type(const Type* referenced_type, size_t length = 1, int32_t device = -1, AddrSpace addr_space = AddrSpace::Generic) {
        return unify(new PtrType(*this, referenced_type, length, device, addr_space));
    }
    const TupleType*           tuple_type() { return tuple0_; } ///< Returns unit, i.e., an empty @p TupleType.
    const TupleType*           tuple_type(Types args) { return unify(new TupleType(*this, args)); }
    const StructAbsType*       struct_abs_type(size_t size, size_t num_type_params = 0, const std::string& name = "");
    const StructAppType*       struct_app_type(const StructAbsType* struct_abs_type, Types args) {
        return unify(new StructAppType(struct_abs_type, args));
    }
    const FnType*              fn_type() { return fn0_; }       ///< Returns an empty @p FnType.
    const FnType*              fn_type(Types args, size_t num_type_params = 0) {
        return unify(new FnType(*this, args, num_type_params));
    }
    const TypeParam*           type_param(const std::string& name) { return unify(new TypeParam(*this, name)); }
    const DefiniteArrayType*   definite_array_type(const Type* elem, u64 dim) { return unify(new DefiniteArrayType(*this, elem, dim)); }
    const IndefiniteArrayType* indefinite_array_type(const Type* elem) { return unify(new IndefiniteArrayType(*this, elem)); }

    const TypeSet& types() const { return types_; }

protected:
    const Type* unify_base(const Type*);
    template<class T> const T* unify(const T* type) { return unify_base(type)->template as<T>(); }

    TypeSet types_;

    union {
        struct {
            const TupleType* tuple0_;///< tuple().
            const FnType*    fn0_;   ///< fn().
            const MemType*   mem_;
            const FrameType* frame_;

            union {
                struct {
#define THORIN_ALL_TYPE(T, M) const PrimType* T##_;
#include "thorin/tables/primtypetable.h"
                };

                const PrimType* primtypes_[Num_PrimTypes];
            };
        };
    };

    friend const Type* close_base(const Type*&, ArrayRef<const TypeParam*>);
};

}

#endif
