#ifndef THORIN_ENUMS_H
#define THORIN_ENUMS_H

#include "thorin/util/types.h"

namespace thorin {

//------------------------------------------------------------------------------


enum NodeKind {
#define THORIN_GLUE(pre, next)
#define THORIN_NODE(node, abbr) Node_##node,
#define THORIN_PRIMTYPE(T) Node_PrimType_##T,
#define THORIN_ARITHOP(op) Node_##op,
#define THORIN_CMP(op) Node_##op,
#include "thorin/tables/allnodes.h"
};

enum Markers {
#define THORIN_GLUE(pre, next) \
    End_##pre, \
    Begin_##next = End_##pre, \
    zzz##Begin_##next = Begin_##next - 1,
#define THORIN_NODE(node, abbr) zzzMarker_##node,
#define THORIN_PRIMTYPE(T) zzzMarker_PrimType_##T,
#define THORIN_ARITHOP(op) zzzMarker_##op,
#define THORIN_CMP(op) zzzMarker_##op,
#include "thorin/tables/allnodes.h"
    End_Cmp,
    Begin_Node = 0,
    End_AllNodes    = End_Cmp,

    Begin_AllNodes  = Begin_Node,

    Begin_PrimType  = Begin_PrimType_bool,
    End_PrimType    = End_PrimType_qf,

    Num_AllNodes    = End_AllNodes   - Begin_AllNodes,
    Num_Nodes       = End_Node       - Begin_Node,

    Num_ArithOps    = End_ArithOp    - Begin_ArithOp,
    Num_Cmps        = End_Cmp        - Begin_Cmp,

    Num_PrimTypes   = End_PrimType_qf - Begin_PrimType_bool,
};

enum PrimTypeKind {
#define THORIN_ALL_TYPE(T, M) PrimType_##T = Node_PrimType_##T,
#include "thorin/tables/primtypetable.h"
};

enum ArithOpKind {
#define THORIN_ARITHOP(op) ArithOp_##op = Node_##op,
#include "thorin/tables/arithoptable.h"
};

enum CmpKind {
#define THORIN_CMP(op) Cmp_##op = Node_##op,
#include "thorin/tables/cmptable.h"
};

inline bool is_type_ps(int kind) { return (int) Begin_PrimType_ps <= kind && kind < (int) End_PrimType_ps; }
inline bool is_type_pu(int kind) { return (int) Begin_PrimType_pu <= kind && kind < (int) End_PrimType_pu; }
inline bool is_type_qs(int kind) { return (int) Begin_PrimType_qs <= kind && kind < (int) End_PrimType_qs; }
inline bool is_type_qu(int kind) { return (int) Begin_PrimType_qu <= kind && kind < (int) End_PrimType_qu; }
inline bool is_type_pf(int kind) { return (int) Begin_PrimType_pf <= kind && kind < (int) End_PrimType_pf; }
inline bool is_type_qf(int kind) { return (int) Begin_PrimType_qf <= kind && kind < (int) End_PrimType_qf; }

inline bool is_type_q(int kind) { return is_type_qs(kind) || is_type_qu(kind) || is_type_qf(kind); }
inline bool is_type_p(int kind) { return is_type_ps(kind) || is_type_pu(kind) || is_type_pf(kind); }
inline bool is_type_s(int kind) { return is_type_ps(kind) || is_type_qs(kind); }
inline bool is_type_u(int kind) { return is_type_pu(kind) || is_type_qu(kind); }
inline bool is_type_i(int kind) { return is_type_s (kind) || is_type_u (kind); }
inline bool is_type_f(int kind) { return is_type_pf(kind) || is_type_qf(kind); }

inline bool is_primtype(int kind){ return (int) Begin_PrimType <= kind && kind < (int) End_PrimType; }
inline bool is_arithop(int kind) { return (int) Begin_ArithOp <= kind && kind < (int) End_ArithOp; }
inline bool is_cmp(int kind)     { return (int) Begin_Cmp   <= kind && kind < (int) End_Cmp; }

inline bool is_bitop(int kind) { return  kind == ArithOp_and || kind == ArithOp_or || kind == ArithOp_xor; }
inline bool is_shift(int kind) { return  kind == ArithOp_shl || kind == ArithOp_shr; }
inline bool is_div_or_rem(int kind) { return kind == ArithOp_div || kind == ArithOp_rem; }
inline bool is_commutative(int kind) { return kind == ArithOp_add  || kind == ArithOp_mul
                                           || kind == ArithOp_and  || kind == ArithOp_or || kind == ArithOp_xor; }
inline bool is_associative(int kind) { return kind == ArithOp_add || kind == ArithOp_mul
                                           || kind == ArithOp_and || kind == ArithOp_or || kind == ArithOp_xor; }

template<PrimTypeKind kind> struct kind2type {};
#define THORIN_ALL_TYPE(T, M) template<> struct kind2type<PrimType_##T> { typedef T type; };
#include "thorin/tables/primtypetable.h"

template<class T> struct type2kind {};
#define THORIN_ALL_TYPE(T, M) template<> struct type2kind<T> { static const PrimTypeKind kind = PrimType_##T; };
#include "thorin/tables/primtypetable.h"

const char* kind2str(NodeKind kind);
int num_bits(PrimTypeKind);

CmpKind negate(CmpKind kind);

} // namespace thorin

#endif
