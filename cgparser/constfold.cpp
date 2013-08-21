/****************************************************************************\
 Copyright (c) 2002, NVIDIA Corporation.
 
 NVIDIA Corporation("NVIDIA") supplies this software to you in
 consideration of your agreement to the following terms, and your use,
 installation, modification or redistribution of this NVIDIA software
 constitutes acceptance of these terms.  If you do not agree with these
 terms, please do not use, install, modify or redistribute this NVIDIA
 software.
 
 In consideration of your agreement to abide by the following terms, and
 subject to these terms, NVIDIA grants you a personal, non-exclusive
 license, under NVIDIA's copyrights in this original NVIDIA software (the
 "NVIDIA Software"), to use, reproduce, modify and redistribute the
 NVIDIA Software, with or without modifications, in source and/or Binary
 forms; provided that if you redistribute the NVIDIA Software, you must
 retain the copyright notice of NVIDIA, this notice and the following
 text and disclaimers in all such redistributions of the NVIDIA Software.
 Neither the name, trademarks, service marks nor logos of NVIDIA
 Corporation may be used to endorse or promote products derived from the
 NVIDIA Software without specific prior written permission from NVIDIA.
 Except as expressly stated in this notice, no other rights or licenses
 express or implied, are granted by NVIDIA herein, including but not
 limited to any patent rights that may be infringed by your derivative
 works or by other works in which the NVIDIA Software may be
 incorporated. No hardware is licensed hereunder.
 
 THE NVIDIA SOFTWARE IS BEING PROVIDED ON AN "AS IS" BASIS, WITHOUT
 WARRANTIES OR CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED,
 INCLUDING WITHOUT LIMITATION, WARRANTIES OR CONDITIONS OF TITLE,
 NON-INFRINGEMENT, MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR
 ITS USE AND OPERATION EITHER ALONE OR IN COMBINATION WITH OTHER
 PRODUCTS.
 
 IN NO EVENT SHalL NVIDIA BE LIABLE FOR ANY SPECIAL, INDIRECT,
 INCIDENTAL, EXEMPLARY, CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 TO, LOST PROFITS; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) OR ARISING IN ANY WAY
 OUT OF THE USE, REPRODUCTION, MODIFICATION AND/OR DISTRIBUTION OF THE
 NVIDIA SOFTWARE, HOWEVER CAUSED AND WHETHER UNDER THEORY OF CONTRACT,
 TORT (INCLUDING NEGLIGENCE), STRICT LIABILITY OR OTHERWISE, EVEN IF
 NVIDIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 \****************************************************************************/

//
// constfold.c
//

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
//#include <string.h>

#include "slglobals.h"

extern Operations *runtime_ops[TB_LastUser+1];

#if 0
#define DB(X)   X
#else
#define DB(X)
#endif

// IsConstant(Expr *)
// return true iff the argument is a Constant
static int IsConstant(Expr *fexpr)
{
  return fexpr && fexpr->kind == CONST_N;
} // IsConstant

// IsConstList(Expr *)
// return true iff the argument is a Constant or a list of constants
static int IsConstList(Expr *fexpr)
{
  while (fexpr &&
         fexpr->kind == BINARY_N &&
         static_cast< Binary * >( fexpr )->op == EXPR_LIST_OP
         ) {
    if (!IsConstant(static_cast< Binary * >( fexpr )->left)) return 0;
    if (!(fexpr = static_cast< Binary * >( fexpr )->right)) return 1;
  }
  return fexpr && fexpr->kind == CONST_N;
} // IsConstList

// NewConst
// create a new blank Constant node we can fill in
// This is a bit of a hack, as the opcode field is really redundant with
// the type info in the node, so we just create an opcode that's as close
// as possible to what we want -- which sometimes may not exist (vectors
// other than float
static Expr *NewConst( CgContext *cg, TypeBase base, int len)
{
  float       tmp[4] = { 0, 0, 0, 0 };
  opcode      op = FCONST_OP;
  
  // This is a bit of a hack -- we use NewFConstNodeV to allocate a node
  // even for non-FCONST_V nodes.  It turns out that it works out ok.
	if (runtime_ops[base]) {
		op = runtime_ops[base]->const_opcode;
	}
	if (len) {
		op = (opcode) ( int( op ) + 1 );
	}
  return (Expr *)NewFConstNodeV( cg, op, tmp, len, base);
}

// GetConstVal
// get the actual value from a Constant node
static ScalarConstant *GetConstVal(Expr *constexpr) {
  if (!constexpr || constexpr->kind != CONST_N) return 0;
  return static_cast< Constant * >( constexpr )->val;
} // GetConstVal

typedef struct constlist_iter {
  Expr        *curr, *rest;
  int         i, lim;
} constlist_iter;

static void InitConstList_iter(constlist_iter *iter, Expr *fexpr)
{
  iter->curr = 0;
  iter->rest = fexpr;
  iter->i = iter->lim = 0;
} // InitConstList_iter

static ScalarConstant *ConstListNext(constlist_iter *iter) {
  if (iter->i >= iter->lim) {
    do {
      if (!iter->rest) return 0;
      if (iter->rest->kind == BINARY_N &&
          static_cast< Binary * >( iter->rest )->op == EXPR_LIST_OP
          ) {
        iter->curr = static_cast< Binary * >( iter->rest )->left;
        iter->rest = static_cast< Binary * >( iter->rest )->right;
      } else if (iter->rest->kind == CONST_N) {
        iter->curr = iter->rest;
        iter->rest = 0;
      } else {
        iter->rest = 0;
        return 0;
      }
    } while (!iter->curr || iter->curr->kind != CONST_N);
    iter->i = 0;
    iter->lim = SUBOP_GET_S(static_cast< Constant * >( iter->curr )->subop);
  }
  return &static_cast< Constant * >( iter->curr )->val[iter->i++];
} // ConstListNext

DB(
   static void pconst(ScalarConstant *v, int type) {
     switch(type) {
       case TB_Boolean:
         cg->Printf("%c", v->i ? 'T' : 'F');
         break;
       case TB_Int:
       case TB_Cint:
         cg->Printf("%d", v->i);
         break;
       default:
         cg->Printf("%g", v->f);
         break;
     }
   }
   
   static void DumpConstList(Expr *fexpr, int type) {
     constlist_iter      iter;
     int                 first = 1;
     ScalarConstant     *v;
     
     InitConstList_iter(&iter, fexpr);
     cg->Printf("(");
     while ((v = ConstListNext(&iter))) {
       if (first) first = 0;
       else cg->Printf(", ");
       pconst(v, type);
     }
     cg->Printf(")");
   }
   )

/*
 * FoldConstants() - Fold this expression if possible.
 *
 */

Expr *FoldConstants( CgContext *cg, Expr *fexpr)
{
  return PostApplyToNodes( cg, ConstantFoldNode, fexpr, 0, 0);
}


// ConstantFoldNode
// all the real work is done here
Expr *ConstantFoldNode( CgContext *cg, Expr *fexpr, void *_arg1, int arg2)
{
  Expr *rv = fexpr;
  TypeBase base;
	TypeBase target;
  int len;
  int i;
  ScalarConstant *a1, *a2;
  void (*unfn)(ScalarConstant *, const ScalarConstant *) = 0;
  void (*binfn)(ScalarConstant *, const ScalarConstant *, const ScalarConstant *) = 0;
  void (*shfn)(ScalarConstant *, const ScalarConstant *, int) = 0;
  int (*cmpfn)(const ScalarConstant *, const ScalarConstant *) = 0;
  int op_offset, a1_mask, a2_mask;
  if (!fexpr) return fexpr;
  switch(fexpr->kind) {
    case UNARY_N:
      base = (TypeBase)SUBOP_GET_T(static_cast< Unary * >( fexpr )->subop);
      len = SUBOP_GET_S(static_cast< Unary * >( fexpr )->subop);
      if (static_cast< Unary * >( fexpr )->op == VECTOR_V_OP && IsConstList(static_cast< Unary * >( fexpr )->arg)) {
        constlist_iter      iter;
        DB (cg->Printf("fold VECTOR_V_OP[%d:%d]", len, base);
            DumpConstList(static_cast< Unary * >( fexpr )->arg, base);
            cg->Printf(" -> ");)
        InitConstList_iter(&iter, static_cast< Unary * >( fexpr )->arg);
        rv = NewConst( cg, base, len);
        for (i=0; i<len; i++) {
          static_cast< Constant * >( rv )->val[i] = *ConstListNext(&iter);
          DB (if (i) cg->Printf(", ");
              pconst(&static_cast< Constant * >( rv )->val[i], base);)
        }
        DB(cg->Printf("\n");)
        return rv;
      }
      if (!IsConstant(static_cast< Unary * >( fexpr )->arg)) break;
      if (!runtime_ops[base]) break;
      a1 = GetConstVal(static_cast< Unary * >( fexpr )->arg);
      switch (static_cast< Unary * >( fexpr )->op) {
        case SWIZZLE_Z_OP: {
          int mask = SUBOP_GET_MASK(static_cast< Unary * >( fexpr )->subop);
          len = SUBOP_GET_S2(static_cast< Unary * >( fexpr )->subop);
          DB (cg->Printf("fold SWIZ[%d:%d].", len, base);
              for (i=0; i<len; i++)
              putchar("xyzw"[(mask>>(i*2))&3]);
              DumpConstList(static_cast< Unary * >( fexpr )->arg, base);
              cg->Printf(" -> ");)
          rv = NewConst( cg, base, len);
          for (i=0; i==0 || i<len; i++) {
            static_cast< Constant * >( rv )->val[i] = a1[mask&3];
            mask >>= 2;
            DB (if (i) cg->Printf(", ");
                pconst(&static_cast< Constant * >( rv )->val[i], base);)
          }
          DB(cg->Printf("\n");)
          break; }
        case SWIZMAT_Z_OP:
          break;
        case CAST_CS_OP:
        case CAST_CV_OP:
          target = (TypeBase)SUBOP_GET_T2(static_cast< Unary * >( fexpr )->subop);
          DB (cg->Printf("fold CAST[%d:%d->%d]", len, base, target);
              DumpConstList(static_cast< Unary * >( fexpr )->arg, base);
              cg->Printf(" -> ");)
          unfn = runtime_ops[base]->cvtTo[target];
          if (!unfn && runtime_ops[target])
            unfn = runtime_ops[target]->cvtFrom[base];
          base = target;
          goto normal_unop;
        case CAST_CM_OP:
          break;
        case NEG_OP:
        case NEG_V_OP:
          DB (cg->Printf("fold NEG[%d:%d]", len, base);
              DumpConstList(static_cast< Unary * >( fexpr )->arg, base);
              cg->Printf(" -> ");)
          unfn = runtime_ops[base]->op_neg;
        normal_unop:
          if (!unfn) {
            DB(cg->Printf("no function, abort\n");)
            break;
          }
          rv = NewConst( cg, base, len);
          for (i=0; i==0 || i<len; i++) {
            unfn(&static_cast< Constant * >( rv )->val[i], &a1[i]);
            DB (if (i) cg->Printf(", ");
                pconst(&static_cast< Constant * >( rv )->val[i], base);)
          }
          DB(cg->Printf("\n");)
          break;
        case POS_OP:
        case POS_V_OP:
          rv = static_cast< Unary * >( fexpr )->arg;
          break;
        case NOT_OP:
        case NOT_V_OP:
          DB (cg->Printf("fold NOT[%d:%d]", len, base);
              DumpConstList(static_cast< Unary * >( fexpr )->arg, base);
              cg->Printf(" -> ");)
          unfn = runtime_ops[base]->op_not;
          goto normal_unop;
        case BNOT_OP:
        case BNOT_V_OP:
          DB (cg->Printf("fold BNOT[%d:%d]", len, base);
              DumpConstList(static_cast< Unary * >( fexpr )->arg, base);
              cg->Printf(" -> ");)
          unfn = runtime_ops[base]->op_bnot;
          goto normal_unop;
          break;
        default:
          break;
      }
      break;
    case BINARY_N:
      if (!IsConstant(static_cast< Binary * >( fexpr )->left) ||
          !IsConstant(static_cast< Binary * >( fexpr )->right)
          )
        break;
      base = (TypeBase)SUBOP_GET_T(static_cast< Binary * >( fexpr )->subop);
      if (!runtime_ops[base]) break;
      len = SUBOP_GET_S(static_cast< Binary * >( fexpr )->subop);
      a1 = GetConstVal(static_cast< Binary * >( fexpr )->left);
      a2 = GetConstVal(static_cast< Binary * >( fexpr )->right);
      switch(static_cast< Binary * >( fexpr )->op) {
        case MEMBER_SELECTOR_OP:
        case ARRAY_INDEX_OP:
        case FUN_CALL_OP:
        case FUN_BUILTIN_OP:
        case FUN_ARG_OP:
        case EXPR_LIST_OP:
          break;
        case MUL_OP:
        case MUL_V_OP:
        case MUL_SV_OP:
        case MUL_VS_OP:
          DB (cg->Printf("fold MUL");)
          binfn = runtime_ops[base]->op_mul;
          op_offset = static_cast< Binary * >( fexpr )->op - MUL_OP;
        normal_binop:
          DB (cg->Printf("[%d:%d]", len, base);
              DumpConstList(static_cast< Binary * >( fexpr )->left, base);
              DumpConstList(static_cast< Binary * >( fexpr )->right, base);
              cg->Printf(" -> ");)
          if (!binfn) {
            DB(cg->Printf("no function, abort\n");)
            break;
          }
          rv = NewConst( cg, base, len);
          // set a1_mask to all 0s or all 1s, depending on whether a1
          // (left arg) is scalar (all 0s) or vector (all 1s).  a2_mask
          // is set according to a2.  This is dependent on the ordering
          // of the OFFSET_ tags in supprt.h
          a1_mask = 0 - (op_offset&1);
          a2_mask = 0 - (((op_offset>>1)^op_offset)&1);
          for (i=0; i==0 || i<len; i++) {
            binfn(&static_cast< Constant * >( rv )->val[i], &a1[i & a1_mask], &a2[i & a2_mask]);
            DB (if (i) cg->Printf(", ");
                pconst(&static_cast< Constant * >( rv )->val[i], base);)
          }
          DB(cg->Printf("\n");)
          break;
        case DIV_OP:
        case DIV_V_OP:
        case DIV_SV_OP:
        case DIV_VS_OP:
          DB (cg->Printf("fold DIV");)
          binfn = runtime_ops[base]->op_div;
          op_offset = static_cast< Binary * >( fexpr )->op - DIV_OP;
          goto normal_binop;
        case MOD_OP:
        case MOD_V_OP:
        case MOD_SV_OP:
        case MOD_VS_OP:
          DB (cg->Printf("fold MOD");)
          binfn = runtime_ops[base]->op_mod;
          op_offset = static_cast< Binary * >( fexpr )->op - MOD_OP;
          goto normal_binop;
        case ADD_OP:
        case ADD_V_OP:
        case ADD_SV_OP:
        case ADD_VS_OP:
          DB (cg->Printf("fold ADD");)
          binfn = runtime_ops[base]->op_add;
          op_offset = static_cast< Binary * >( fexpr )->op - ADD_OP;
          goto normal_binop;
        case SUB_OP:
        case SUB_V_OP:
        case SUB_SV_OP:
        case SUB_VS_OP:
          DB (cg->Printf("fold SUB");)
          binfn = runtime_ops[base]->op_sub;
          op_offset = static_cast< Binary * >( fexpr )->op - SUB_OP;
          goto normal_binop;
        case SHL_OP:
        case SHL_V_OP:
          DB (cg->Printf("fold SHL");)
          shfn = runtime_ops[base]->op_shl;
        normal_shiftop:
          DB (cg->Printf("[%d:%d]", len, base);
              DumpConstList(static_cast< Binary * >( fexpr )->left, base);
              DumpConstList(static_cast< Binary * >( fexpr )->right, TB_Cint);
              cg->Printf(" -> ");)
          if (!shfn) {
            DB(cg->Printf("no function, abort\n");)
            break;
          }
          rv = NewConst( cg, base, len);
          for (i=0; i==0 || i<len; i++) {
            shfn(&static_cast< Constant * >( rv )->val[i], &a1[i], a2->i);
            DB (if (i) cg->Printf(", ");
                pconst(&static_cast< Constant * >( rv )->val[i], base);)
          }
          DB(cg->Printf("\n");)
          break;
        case SHR_OP:
        case SHR_V_OP:
          DB (cg->Printf("fold SHR");)
          shfn = runtime_ops[base]->op_shr;
          goto normal_shiftop;
        case LT_OP:
        case LT_V_OP:
        case LT_SV_OP:
        case LT_VS_OP:
          DB (cg->Printf("fold LT");)
          cmpfn = runtime_ops[base]->op_lt;
          op_offset = static_cast< Binary * >( fexpr )->op - LT_OP;
        normal_cmpop:
          DB (cg->Printf("[%d:%d]", len, base);
              DumpConstList(static_cast< Binary * >( fexpr )->left, base);
              DumpConstList(static_cast< Binary * >( fexpr )->right, TB_Cint);
              cg->Printf(" -> ");)
          if (!cmpfn) {
            DB(cg->Printf("no function, abort\n");)
            break;
          }
          rv = NewConst( cg, TB_Boolean, len);
          // set a1_mask to all 0s or all 1s, depending on whether a1
          // (left arg) is scalar (all 0s) or vector (all 1s).  a2_mask
          // is set according to a2.  This is dependent on the ordering
          // of the OFFSET_ tags in supprt.h
          a1_mask = 0 - (op_offset&1);
          a2_mask = 0 - (((op_offset>>1)^op_offset)&1);
          for (i=0; i==0 || i<len; i++) {
            static_cast< Constant * >( rv )->val[i].i = cmpfn(&a1[i & a1_mask], &a2[i & a2_mask]);
            DB (if (i) cg->Printf(", ");
                pconst(&static_cast< Constant * >( rv )->val[i], TB_Boolean);)
          }
          DB(cg->Printf("\n");)
          break;
        case GT_OP:
        case GT_V_OP:
        case GT_SV_OP:
        case GT_VS_OP:
          cmpfn = runtime_ops[base]->op_gt;
          op_offset = static_cast< Binary * >( fexpr )->op - GT_OP;
          goto normal_cmpop;
        case LE_OP:
        case LE_V_OP:
        case LE_SV_OP:
        case LE_VS_OP:
          cmpfn = runtime_ops[base]->op_le;
          op_offset = static_cast< Binary * >( fexpr )->op - LE_OP;
          goto normal_cmpop;
        case GE_OP:
        case GE_V_OP:
        case GE_SV_OP:
        case GE_VS_OP:
          cmpfn = runtime_ops[base]->op_ge;
          op_offset = static_cast< Binary * >( fexpr )->op - GE_OP;
          goto normal_cmpop;
        case EQ_OP:
        case EQ_V_OP:
        case EQ_SV_OP:
        case EQ_VS_OP:
          cmpfn = runtime_ops[base]->op_eq;
          op_offset = static_cast< Binary * >( fexpr )->op - EQ_OP;
          goto normal_cmpop;
        case NE_OP:
        case NE_V_OP:
        case NE_SV_OP:
        case NE_VS_OP:
          cmpfn = runtime_ops[base]->op_ne;
          op_offset = static_cast< Binary * >( fexpr )->op - NE_OP;
          goto normal_cmpop;
        case AND_OP:
        case AND_V_OP:
        case AND_SV_OP:
        case AND_VS_OP:
          DB (cg->Printf("fold AND");)
          binfn = runtime_ops[base]->op_and;
          op_offset = static_cast< Binary * >( fexpr )->op - AND_OP;
          goto normal_binop;
        case XOR_OP:
        case XOR_V_OP:
        case XOR_SV_OP:
        case XOR_VS_OP:
          DB (cg->Printf("fold XOR");)
          binfn = runtime_ops[base]->op_xor;
          op_offset = static_cast< Binary * >( fexpr )->op - XOR_OP;
          goto normal_binop;
        case OR_OP:
        case OR_V_OP:
        case OR_SV_OP:
        case OR_VS_OP:
          DB (cg->Printf("fold OR");)
          binfn = runtime_ops[base]->op_or;
          op_offset = static_cast< Binary * >( fexpr )->op - OR_OP;
          goto normal_binop;
        case BAND_OP:
        case BAND_V_OP:
        case BAND_SV_OP:
        case BAND_VS_OP:
          DB (cg->Printf("fold BAND");)
          binfn = runtime_ops[base]->op_band;
          op_offset = static_cast< Binary * >( fexpr )->op - BAND_OP;
          goto normal_binop;
        case BOR_OP:
        case BOR_V_OP:
        case BOR_SV_OP:
        case BOR_VS_OP:
          DB (cg->Printf("fold BOR");)
          binfn = runtime_ops[base]->op_bor;
          op_offset = static_cast< Binary * >( fexpr )->op - BOR_OP;
          goto normal_binop;
        case ASSIGN_OP:
        case ASSIGN_V_OP:
        case ASSIGN_GEN_OP:
        case ASSIGN_MASKED_KV_OP:
        default:
          break;
      }
      break;
    case TRINARY_N:
      switch(static_cast< Trinary * >( fexpr )->op) {
        case COND_OP:
        case COND_V_OP:
        case COND_SV_OP:
        case COND_GEN_OP:
        case ASSIGN_COND_OP:
        case ASSIGN_COND_V_OP:
        case ASSIGN_COND_SV_OP:
        case ASSIGN_COND_GEN_OP:
        default:
          break;
      }
      break;
    default:
      break;
  }
  return rv;
} // ConstantFoldNode

static void int_neg(ScalarConstant *r, const ScalarConstant *a)
{
  r->i = - a->i;
}
static void int_not(ScalarConstant *r, const ScalarConstant *a)
{
  r->i = ~ a->i;
}
static void int_add(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->i = a->i + b->i;
}
static void int_sub(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->i = a->i - b->i;
}
static void int_mul(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->i = a->i * b->i;
}
static void int_div(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->i = a->i / b->i;
}
static void int_mod(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->i = a->i % b->i;
}
static void int_and(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->i = a->i & b->i;
}
static void int_or(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->i = a->i | b->i;
}
static void int_xor(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->i = a->i ^ b->i;
}
static void int_shr(ScalarConstant *r, const ScalarConstant *a, int b)
{
  r->i = a->i >> b;
}
static void int_shl(ScalarConstant *r, const ScalarConstant *a, int b)
{
  r->i = a->i << b;
}
static int int_lt(const ScalarConstant *a, const ScalarConstant *b)
{
  return a->i < b->i;
}
static int int_gt(const ScalarConstant *a, const ScalarConstant *b)
{
  return a->i > b->i;
}
static int int_le(const ScalarConstant *a, const ScalarConstant *b)
{
  return a->i <= b->i;
}
static int int_ge(const ScalarConstant *a, const ScalarConstant *b)
{
  return a->i >= b->i;
}
static int int_eq(const ScalarConstant *a, const ScalarConstant *b)
{
  return a->i == b->i;
}
static int int_ne(const ScalarConstant *a, const ScalarConstant *b)
{
  return a->i != b->i;
}

static void float_neg(ScalarConstant *r, const ScalarConstant *a)
{
  r->f = - a->f;
}
static void float_add(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->f = a->f + b->f;
}
static void float_sub(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->f = a->f - b->f;
}
static void float_mul(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->f = a->f * b->f;
}
static void float_div(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->f = a->f / b->f;
}
static int float_lt(const ScalarConstant *a, const ScalarConstant *b)
{
  return a->f < b->f;
}
static int float_gt(const ScalarConstant *a, const ScalarConstant *b)
{
  return a->f > b->f;
}
static int float_le(const ScalarConstant *a, const ScalarConstant *b)
{
  return a->f <= b->f;
}
static int float_ge(const ScalarConstant *a, const ScalarConstant *b)
{
  return a->f >= b->f;
}
static int float_eq(const ScalarConstant *a, const ScalarConstant *b)
{
  return a->f == b->f;
}
static int float_ne(const ScalarConstant *a, const ScalarConstant *b)
{
  return a->f != b->f;
}

static void bool_not(ScalarConstant *r, const ScalarConstant *a)
{
  r->i = !a->i;
}
static void bool_and(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->i = a->i && b->i;
}
static void bool_or(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->i = a->i || b->i;
}

static void int2float(ScalarConstant *r, const ScalarConstant *a)
{
  r->f = (float)a->i;
}
static void float2int(ScalarConstant *r, const ScalarConstant *a)
{
  r->i = (int)a->f;
}
static void copy(ScalarConstant *r, const ScalarConstant *a)
{
  *r = *a;
}
static void int2bool(ScalarConstant *r, const ScalarConstant *a)
{
  r->i = (a->i != 0);
}
static void bool2int(ScalarConstant *r, const ScalarConstant *a)
{
  r->i = a->i ? 1 : 0;
}
static void float2bool(ScalarConstant *r, const ScalarConstant *a)
{
  r->i = (a->f != 0);
}
static void bool2float(ScalarConstant *r, const ScalarConstant *a)
{
  r->f = a->i ? 1.0f : 0.0f;
}

static Operations int_ops = {
  ICONST_OP,
  int_neg, int_not, 0,
  int_add, int_sub, int_mul, int_div, int_mod,
  int_and, int_or, int_xor, 0, 0,
  int_shr, int_shl,
  int_lt, int_gt, int_le, int_ge, int_eq, int_ne,
  { 0, 0, int2float, copy, 0, int2float, copy, int2bool, 0, 0, 0, 0, 0, 0, 0, 0, },
  { 0, 0, float2int, copy, 0, float2int, copy, bool2int, 0, 0, 0, 0, 0, 0, 0, 0, },
};

static Operations float_ops = {
  FCONST_OP,
  float_neg, 0, 0,
  float_add, float_sub, float_mul, float_div, 0,
  0, 0, 0, 0, 0,
  0, 0,
  float_lt, float_gt, float_le, float_ge, float_eq, float_ne,
  { 0, 0, copy, float2int, 0, copy, float2int, float2bool, 0, 0, 0, 0, 0, 0, 0, 0, },
  { 0, 0, copy, int2float, 0, copy, int2float, bool2float, 0, 0, 0, 0, 0, 0, 0, 0, },
};

static Operations bool_ops = {
  BCONST_OP,
  0, 0, bool_not,
  0, 0, 0, 0, 0,
  0, 0, 0, bool_and, bool_or,
  0, 0,
  0, 0, 0, 0, int_eq, int_ne,
  { 0, 0, bool2float, bool2int, 0, bool2float, bool2int, copy, 0, 0, 0, 0, 0, 0, 0, 0, },
  { 0, 0, float2bool, int2bool, 0, float2bool, int2bool, copy, 0, 0, 0, 0, 0, 0, 0, 0, },
};

Operations *runtime_ops[TB_LastUser+1] = {
  0,          /* No type */
  0,          /* Undefined */
  &float_ops, /* cfloat */
  &int_ops,   /* cint */
  0,          /* void */
  &float_ops, /* float */
  &int_ops,   /* int */
  &bool_ops,  /* boolean */
  /* profile defined types: these will be set up by the profile */
  0, 0, 0, 0, 0, 0, 0, 0,
};


// round a value to half (S5.10, bias=14) precision
float round_half(double v) {
  int expr;
  double mant = frexp(v, &expr);
  int rndm = (int)(mant * 2048 + 0.5);
  if (expr > 17) {
    // overflow -- build the appropriately signed infinity
    v = ldexp(mant, 500) * 2;
  } else if (expr < -23) {
    // full underflow -- build appropraitely signed zero
    v = ldexp(mant, -500);
  } else {
    if (expr < -13) {
      // underflow -- round more to show denorm
      rndm >>= -(expr - 13);
      rndm <<= -(expr - 13);
    }
    v = ldexp(rndm/2048.0, expr);
  }
  return (float)v;
}

// round/clamp to fixed (signed 2.10) precision
#define FIXED_MAX (1.9990234375)
#define FIXED_MIN (-2.0)
float round_fixed(double v) {
  int tmp;
  if (v > FIXED_MAX) return FIXED_MAX;
  if (v < FIXED_MIN) return FIXED_MIN;
  tmp = (int)(v*1024 + 0.5);
  return (float)(tmp/1024.0);
}

static void half_neg(ScalarConstant *r, const ScalarConstant *a)
{
  r->f = round_half(- a->f);
}
static void half_add(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->f = round_half(a->f + b->f);
}
static void half_sub(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->f = round_half(a->f - b->f);
}
static void half_mul(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->f = round_half(a->f * b->f);
}
static void half_div(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->f = round_half(a->f / b->f);
}
#define half_lt float_lt
#define half_gt float_gt
#define half_le float_le
#define half_ge float_ge
#define half_eq float_eq
#define half_ne float_ne

static void fixed_neg(ScalarConstant *r, const ScalarConstant *a)
{
  r->f = round_fixed(- a->f);
}
static void fixed_add(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->f = round_fixed(a->f + b->f);
}
static void fixed_sub(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->f = round_fixed(a->f - b->f);
}
static void fixed_mul(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->f = round_fixed(a->f * b->f);
}
static void fixed_div(ScalarConstant *r, const ScalarConstant *a, const ScalarConstant *b)
{
  r->f = round_fixed(a->f / b->f);
}
#define fixed_lt float_lt
#define fixed_gt float_gt
#define fixed_le float_le
#define fixed_ge float_ge
#define fixed_eq float_eq
#define fixed_ne float_ne


#define half2int float2int
#define half2float copy
#define half2bool float2bool
static void int2half(ScalarConstant *r, const ScalarConstant *a)
{
  r->f = round_half((float)a->i);
}
static void float2half(ScalarConstant *r, const ScalarConstant *a)
{
  r->f = round_half(a->f);
}
#define bool2half bool2float
#define fixed2int float2int
#define fixed2float copy
#define fixed2bool float2bool
static void int2fixed(ScalarConstant *r, const ScalarConstant *a)
{
  r->f = round_fixed((float)a->i);
}
static void float2fixed(ScalarConstant *r, const ScalarConstant *a)
{
  r->f = round_fixed(a->f);
}
#define bool2fixed bool2float
#define half2fixed float2fixed
#define fixed2half copy

static Operations half_ops = {
  HCONST_OP,
  half_neg, 0, 0,
  half_add, half_sub, half_mul, half_div, 0,
  0, 0, 0, 0, 0,
  0, 0,
  half_lt, half_gt, half_le, half_ge, half_eq, half_ne,
  { 0, 0, half2float, half2int, 0, half2float, half2int, half2bool, 0, 0, 0, 0, 0, 0, 0, 0, },
  { 0, 0, float2half, int2half, 0, float2half, int2half, bool2half, 0, 0, 0, 0, 0, 0, 0, 0, },
};

static Operations fixed_ops = {
  XCONST_OP,
  fixed_neg, 0, 0,
  fixed_add, fixed_sub, fixed_mul, fixed_div, 0,
  0, 0, 0, 0, 0,
  0, 0,
  fixed_lt, fixed_gt, fixed_le, fixed_ge, fixed_eq, fixed_ne,
  { 0, 0, fixed2float, fixed2int, 0, fixed2float, fixed2int, fixed2bool, 0, 0, 0, 0, 0, 0, 0, 0, },
  { 0, 0, float2fixed, int2fixed, 0, float2fixed, int2fixed, bool2fixed, 0, 0, 0, 0, 0, 0, 0, 0, },
};

void Hal_SetupHalfFixedTypes(int half, int fixed) {
  if (half) {
    runtime_ops[half] = &half_ops;
    half_ops.cvtTo[half] = copy;
    half_ops.cvtFrom[half] = copy;
  }
  if (fixed) {
    runtime_ops[fixed] = &fixed_ops;
    fixed_ops.cvtTo[fixed] = copy;
    fixed_ops.cvtFrom[fixed] = copy;
  }
  if (half && fixed) {
    half_ops.cvtTo[fixed] = half2fixed;
    half_ops.cvtFrom[fixed] = fixed2half;
    fixed_ops.cvtTo[half] = fixed2half;
    fixed_ops.cvtFrom[half] = half2fixed;
  }
  
}
