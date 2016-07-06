#ifndef THORIN_TYPE_H
#define THORIN_TYPE_H

#include "thorin/def.h"

namespace thorin {

//------------------------------------------------------------------------------

class StructType : public Def {
private:
    StructType(World& world, size_t num_ops, const Location& loc, const std::string& name);

public:
    void set(size_t i, const Def* type) const { return const_cast<StructType*>(this)->Def::set(i, type); }

private:
    virtual const Def* vrebuild(World& to, Defs ops) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;
    virtual std::ostream& stream(std::ostream&) const override;

    friend class World;
};

class Type : public Def {
protected:
    Type(World& world, int kind, Defs ops);
};

/// The type of the memory monad.
class MemType : public Type {
public:
    virtual std::ostream& stream(std::ostream&) const override;

private:
    MemType(World& world)
        : Type(world, Node_MemType, {})
    {}

    virtual const Def* vrebuild(World& to, Defs ops) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    friend class World;
};

inline bool is_mem(const Def* def) { return def->type()->isa<MemType>(); }

/// The type of a stack frame.
class FrameType : public Type {
public:
    virtual std::ostream& stream(std::ostream&) const override;

private:
    FrameType(World& world)
        : Type(world, Node_FrameType, {})
    {}

    virtual const Def* vrebuild(World& to, Defs ops) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    friend class World;
};

/// Base class for all SIMD types.
class VectorType : public Type {
protected:
    VectorType(World& world, int kind, Defs ops, size_t length)
        : Type(world, kind, ops)
        , length_(length)
    {}

    virtual uint64_t vhash() const override { return hash_combine(Def::vhash(), length()); }
    virtual bool equal(const Def* other) const override {
        return Def::equal(other) && this->length() == other->as<VectorType>()->length();
    }

public:
    /// The number of vector arguments - the vector length.
    size_t length() const { return length_; }
    bool is_vector() const { return length_ != 1; }
    /// Rebuilds the type with vector length 1.
    const VectorType* scalarize() const;

private:
    size_t length_;
};

/// Returns the vector length. Raises an assertion if this type is not a @p VectorType.
inline size_t vector_length(const Def* type) { return type->as<VectorType>()->length(); }

/// Primitive type.
class PrimType : public VectorType {
private:
    PrimType(World& world, PrimTypeKind kind, size_t length)
        : VectorType(world, (int) kind, {}, length)
    {}

public:
    PrimTypeKind primtype_kind() const { return (PrimTypeKind) kind(); }

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Def* vrebuild(World& to, Defs ops) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    friend class World;
};

inline bool is_primtype (const Def* d) { return thorin::is_primtype(d->kind()); }
inline bool is_type_ps  (const Def* d) { return thorin::is_type_ps (d->kind()); }
inline bool is_type_pu  (const Def* d) { return thorin::is_type_pu (d->kind()); }
inline bool is_type_qs  (const Def* d) { return thorin::is_type_qs (d->kind()); }
inline bool is_type_qu  (const Def* d) { return thorin::is_type_qu (d->kind()); }
inline bool is_type_pf  (const Def* d) { return thorin::is_type_pf (d->kind()); }
inline bool is_type_qf  (const Def* d) { return thorin::is_type_qf (d->kind()); }
inline bool is_type_p   (const Def* d) { return thorin::is_type_p  (d->kind()); }
inline bool is_type_q   (const Def* d) { return thorin::is_type_q  (d->kind()); }
inline bool is_type_s   (const Def* d) { return thorin::is_type_s  (d->kind()); }
inline bool is_type_u   (const Def* d) { return thorin::is_type_u  (d->kind()); }
inline bool is_type_i   (const Def* d) { return thorin::is_type_i  (d->kind()); }
inline bool is_type_f   (const Def* d) { return thorin::is_type_f  (d->kind()); }
inline bool is_type_bool(const Def* d) { return d->kind() == Node_PrimType_bool; }

enum class AddrSpace : uint32_t {
    Generic  = 0,
    Global   = 1,
    Texture  = 2,
    Shared   = 3,
    Constant = 4,
};

/// Pointer type.
class PtrType : public VectorType {
private:
    PtrType(World& world, const Def* referenced_type, size_t length, int32_t device, AddrSpace addr_space)
        : VectorType(world, Node_PtrType, {referenced_type}, length)
        , addr_space_(addr_space)
        , device_(device)
    {}

public:
    const Def* referenced_type() const { return op(0); }
    AddrSpace addr_space() const { return addr_space_; }
    int32_t device() const { return device_; }
    bool is_host_device() const { return device_ == -1; }

    virtual uint64_t vhash() const override;
    virtual bool equal(const Def* other) const override;

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Def* vrebuild(World& to, Defs ops) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    AddrSpace addr_space_;
    int32_t device_;

    friend class World;
};

class FnType : public Type {
private:
    FnType(World& world, Defs ops)
        : Type(world, Node_FnType, ops)
    {
        ++order_;
    }

public:
    bool is_basicblock() const { return order() == 1; }
    bool is_returning() const;

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Def* vrebuild(World& to, Defs ops) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    friend class World;
};

//------------------------------------------------------------------------------

class ArrayType : public Type {
protected:
    ArrayType(World& world, int kind, const Def* elem_type)
        : Type(world, kind, {elem_type})
    {}

public:
    const Def* elem_type() const { return op(0); }
};

class IndefiniteArrayType : public ArrayType {
public:
    IndefiniteArrayType(World& world, const Def* elem_type)
        : ArrayType(world, Node_IndefiniteArrayType, elem_type)
    {}

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Def* vrebuild(World& to, Defs ops) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    friend class World;
};

class DefiniteArrayType : public ArrayType {
public:
    DefiniteArrayType(World& world, const Def* elem_type, u64 dim)
        : ArrayType(world, Node_DefiniteArrayType, elem_type)
        , dim_(dim)
    {}

    u64 dim() const { return dim_; }
    virtual uint64_t vhash() const override { return hash_combine(Type::vhash(), dim()); }
    virtual bool equal(const Def* other) const override {
        return Type::equal(other) && this->dim() == other->as<DefiniteArrayType>()->dim();
    }

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Def* vrebuild(World& to, Defs ops) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    u64 dim_;

    friend class World;
};

const IndefiniteArrayType* is_indefinite(const Def*);
bool use_lea(const Def*);

//------------------------------------------------------------------------------

}

#endif
