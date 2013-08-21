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
// compile.c
//

// In a cygnus tools window in the "gen" directory, run the following command:
//
// bison parser.y --defines -output ../src/parser.c --verbose

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>
#include <fstream>

#include "slglobals.h"

using namespace sx;
using namespace std;

/*
 * OutputHeader()
 *
 */

void CgContext::WriteOutputHeader()
{
  theHal->PrintCodeHeader();
  OutputPrintf( "%s " SX_TAG " version %d.%d.%04d%s, build date %s  %s\n", theHal->comment,
               HSL_VERSION, HSL_SUB_VERSION, HSL_SUB_SUB_VERSION, NDA_STRING, Build_Date, Build_Time);
} // OpenOutputFile

/*
 * OpenListFile()
 *
 */

void CgContext::WriteListingHeader()
{
	ListingPrintf( "%s " SX_TAG " version %d.%d.%04d%s, build date %s  %s\n", theHal->comment,
                HSL_VERSION, HSL_SUB_VERSION, HSL_SUB_SUB_VERSION, NDA_STRING, Build_Date, Build_Time);
} // OpenListFile

/*
 * PrintOptions()
 *
 */

void CgContext::PrintOptions(int argc, char **argv)
{
  int ii;
  
  if (argc > 1) {
    OutputPrintf("%s command line args:", theHal->comment);
    for (ii = 1; ii < argc; ii++)
      OutputPrintf( " %s", argv[ii]);
    OutputPrintf( "\n" );
  }
} // PrintOptions

/*
 * CloseOutputFiles()
 *
 */

int CgContext::WriteOutputFiles(const char *mess)
{
	if ( options.outputFileName ) {
		ofstream o;
		o.open( options.outputFileName );
		if ( o.is_open() ) {
			o << outStream.str();
			o << theHal->comment << " " << mess << endl;
			o.close();
		}
	}
	if ( options.listFileName ) {
		ofstream o;
		o.open( options.listFileName );
		if ( o.is_open() ) {
			o << listingStream.str();
			o << theHal->comment << " " << mess << endl;
			o.close();
		}
	}
  return 1;
} // CloseOutputFiles

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Misc Support Functions: ///////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * HasNumericSuffix() - See if there is a numeric suffix on a string.  If so, strip it off and
 *         retrun the value as an integer.
 */

int HasNumericSuffix(const char *fStr, char *root, int size, int *suffix)
{
  int val, HasSuffix, len, scale;
  char *s, ch;
  
  strncpy(root, fStr, size - 1);
  len = strlen(fStr);
  if (len >= size)
    len = size - 1;
  root[len] = 0;
  val = 0;
  HasSuffix = 0;
  scale = 1;
  s = &root[len];
  while (1) {
    ch = *--s;
    if (ch >= '0' && ch <= '9' && s >= root) {
      val = val + scale*(ch - '0');
      scale *= 10;
      HasSuffix = 1;
    } else {
      break;
    }
  }
  s[1] = '\0';
  *suffix = val;
  return HasSuffix;
} // HasNumericSuffix

///////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////// Analysis and Code Generation Control Functions ///////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * SetSymbolFlags() - Set the flags field for all symbols in this tree.
 *
 */

void SetSymbolFlags(Symbol *fSymb, int fVal)
{
  if (fSymb) {
    fSymb->flags = fVal;
    SetSymbolFlags(fSymb->left, fVal);
    SetSymbolFlags(fSymb->right, fVal);
  }
} // SetSymbolFlags

/*
 * SetSymbolFlagsList() - Walk a list of scopes and set the flags field for all symbols.
 *
 */

void SetSymbolFlagsList(Scope *fScope, int fVal)
{
  while (fScope) {
    SetSymbolFlags(fScope->symbols, fVal);
    fScope = fScope->next;
  }
} // SetSymbolFlagsList

///////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////// Various Utility Functions //////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * GetNumberedAtom() - Create an atom with the given root string and a numbered suffix.
 */

Atom GetNumberedAtom( CgContext *cg, const char *root, int number, int digits, char ch)
{
  char str[256], *s;
  Atom vname;
	int len;
  
  strcpy(str, root);
  len = strlen(str);
  if (ch != '\0') {
    str[len] = ch;
    len++;
  }
  s = &str[len + digits];
  *s = '\0';
  while (digits-- > 0) {
    *--s = '0' + number % 10;
    number /= 10;
  }
  vname = cg->GetAtom(str);
  return vname;
} // GetNumberedAtom

/*
 * GetVarExprName() - Return a string representation of the variable described by fExpr.
 */

int GetVarExprName( CgContext *cg, char *str, int size, Expr *fExpr)
{
  const char *name;
  int len = 0, len2;
  
  *str = '\0';
  switch (fExpr->kind) {
    case SYMB_N:
      switch (static_cast< Symb * >( fExpr )->op) {
        case VARIABLE_OP:
        case MEMBER_OP:
          name = cg->GetString(static_cast< Symb * >( fExpr )->symbol->name);
          len2 = strlen(name);
          if (len2 >= size)
            len2 = size - 1;
          len += len2;
          while (len2--)
            *str++ = *name++;
          *str = '\0';
          break;
      }
      break;
    case BINARY_N:
      // Put struct.member code here:
      break;
  }
  return len;
} // GetVarExprName

/*
 * GenerateIndexName() - Create an atom with the given root string representing the variable
 *         by fExpr and a numbered suffix for the index.
 */
Atom GenerateIndexName( CgContext *cg, Expr *fExpr, int index)
{
  char str[256];
  Atom vname;
  
  GetVarExprName( cg, str, 256, fExpr);
  vname = GetNumberedAtom( cg, str, index, 1, '$');
  return vname;
} // GenerateIndexeName

/*
 * ConcatStmts() - Concatenate two lists of statements.
 *
 */

Stmt *ConcatStmts(Stmt *first, Stmt *last)
{
  Stmt *lStmt;
  
  if (first) {
    if (last) {
      lStmt = first;
      while (lStmt->next)
        lStmt = lStmt->next;
      lStmt->next = last;
    }
    return first;
  } else {
    return last;
  }
} // ConcatStmts

/*
 * AppendStatements() - Append a list of statements to the end of another list.
 *
 */

void AppendStatements(StmtList *fStatements, Stmt *fStmt)
{
  if (fStmt) {
    if (fStatements->first) {
      fStatements->last->next = fStmt;
    } else {
      fStatements->first = fStmt;
    }
    while (fStmt->next)
      fStmt = fStmt->next;
    fStatements->last = fStmt;
  }
} // AppendStatements

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////// Expression Functions ////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * GenSymb() - Create a node that references a symbol.
 *
 */

Expr *GenSymb( CgContext *cg, Symbol *fSymb)
{
  Expr *lExpr;
  
  lExpr = (Expr *) NewSymbNode( cg, VARIABLE_OP, fSymb);
  return lExpr;
} // GenSymb

/*
 * GenMember() - Create a node that references a member of a struct or connector.
 *
 */

Expr *GenMember( CgContext *cg, Symbol *fSymb)
{
  Expr *lExpr;
  
  lExpr = (Expr *) NewSymbNode( cg, MEMBER_OP, fSymb);
  return lExpr;
} // GenMember

/*
 * GenMemberSelector() - Create a node that references a member of a struct or connector.
 *
 */

Expr *GenMemberSelector( CgContext *cg, Expr *sExpr, Expr *mExpr)
{
  Expr *lExpr;
  
  lExpr = (Expr *) NewBinopNode( cg, MEMBER_SELECTOR_OP, sExpr, mExpr);
  lExpr->type = mExpr->type;
  return lExpr;
} // GenMemberSelector

/*
 * GenMemberReference() - Build an expression to reference a member of a struct.
 *
 */

Expr *GenMemberReference( CgContext *cg, Expr *sExpr, Symbol *mSymb)
{
  Expr *lExpr, *mExpr;
  
  // sExpr = (Expr *) NewSymbNode(VARIABLE_OP, sSymb);
  mExpr = (Expr *) NewSymbNode( cg, MEMBER_OP, mSymb);
  lExpr = (Expr *) NewBinopNode( cg, MEMBER_SELECTOR_OP, sExpr, mExpr);
  lExpr->type = mExpr->type;
  lExpr->IsLValue = 1;
  lExpr->IsConst = lExpr->type->isConst;
  return lExpr;
} // GenMemberReference

/*
 * GenVecIndex() - Create a vector index node.
 *
 */

Expr *GenVecIndex( CgContext * cg, Expr *vexpr, Expr *xexpr, TypeBase base, int len, int len2)
{
  Expr *lExpr;
  
  lExpr = (Expr *) NewBinopNode( cg, ARRAY_INDEX_OP, vexpr, xexpr);
  static_cast< Binary * >( lExpr )->type = GetStandardType( cg, base, 0, 0);
  return lExpr;
} // GenVecIndex

/*
 * GenMatIndex() - Create a matrix index node.
 *
 */

Expr *GenMatIndex( CgContext *cg, Expr *mExpr, Expr *xexpr, TypeBase base, int len, int len2)
{
  Expr *lExpr;
  
  lExpr = (Expr *) NewBinopNode( cg, ARRAY_INDEX_OP, mExpr, xexpr);
  static_cast< Binary * >( lExpr )->type = GetStandardType( cg, base, len, 0);
  return lExpr;
} // GenMatIndex

/*
 * GenBoolConst() - Create a Boolean Constant node.
 *
 */

Expr *GenBoolConst( CgContext *cg, int fval)
{
  Expr *lExpr;
  
  lExpr = (Expr *) NewBConstNode( cg, BCONST_OP, fval, TB_Boolean);
  return lExpr;
} // GenBoolConst

/*
 * GenIntConst() - Create an integer Constant node.
 *
 */

Expr *GenIntConst( CgContext *cg, int fval)
{
  Expr *lExpr;
  
  lExpr = (Expr *) NewIConstNode( cg, ICONST_OP, fval, TB_Int);
  return lExpr;
} // GenIntConst

/*
 * GenFConstV() - Create a vector floating point Constant node.
 *
 */

Expr *GenFConstV( CgContext *cg, float *fval, int len, TypeBase base)
{
  Expr *lExpr;
  
  lExpr = (Expr *) NewFConstNodeV( cg, FCONST_V_OP, fval, len, base);
  return lExpr;
} // GenFConstV

/*
 * GenBoolNot() - Create a Boolean NOT.
 *
 */

Expr *GenBoolNot( CgContext *cg, Expr *fExpr)
{
  Expr *lExpr;
  int lsubop;
  
  lsubop = SUBOP__(TB_Boolean);
  lExpr = (Expr *) NewUnopSubNode( cg, BNOT_OP, lsubop, fExpr);
  static_cast< Unary * >( lExpr )->type = cg->BooleanType;
  return lExpr;
} // GenBoolNot

/*
 * GenBoolAssign() - Create a Boolean assignment.
 *
 */

Expr *GenBoolAssign( CgContext *cg, Expr *fVar, Expr *fExpr)
{
  Expr *lExpr;
  int lsubop;
  
  lsubop = SUBOP__(TB_Boolean);
  lExpr = (Expr *) NewBinopSubNode( cg, ASSIGN_OP, lsubop, fVar, fExpr);
  static_cast< Binary * >( lExpr )->type = cg->BooleanType;
  return lExpr;
} // GenBoolAssign

/*
 * GenSAssign() - Create a scalar assignment.
 *
 */

Expr *GenSAssign( CgContext *cg, Expr *fVar, Expr *fExpr, TypeBase base)
{
  Expr *lExpr;
  int lsubop;
  
  lsubop = SUBOP_S(base);
  lExpr = (Expr *) NewBinopSubNode( cg, ASSIGN_OP, lsubop, fVar, fExpr);
  static_cast< Binary * >( lExpr )->type = GetStandardType( cg, base, 0, 0);
  return lExpr;
} // GenSAssign

/*
 * GenVAssign() - Create a vector assignment.
 *
 */

Expr *GenVAssign( CgContext *cg, Expr *fVar, Expr *fExpr, TypeBase base, int len)
{
  Expr *lExpr;
  int lsubop;
  
  lsubop = SUBOP_V(len, base);
  lExpr = (Expr *) NewBinopSubNode( cg, ASSIGN_V_OP, lsubop, fVar, fExpr);
  static_cast< Binary * >( lExpr )->type = GetStandardType( cg, base, len, 0);
  return lExpr;
} // GenCondSVAssign

/*
 * GenMAssign() - Create a matrix assignment.
 *
 */

Expr *GenMAssign( CgContext *cg, Expr *fVar, Expr *fExpr, TypeBase base, int len, int len2)
{
  Expr *lExpr;
  
  lExpr = (Expr *) NewBinopNode( cg, ASSIGN_GEN_OP, fVar, fExpr);
  static_cast< Binary * >( lExpr )->type = GetStandardType( cg, base, len, len2);
  return lExpr;
} // GenMAssign

/*
 * GenCondSAssign() - Create a scalar conditional assignment.
 *
 */

Expr *GenCondSAssign( CgContext *cg, Expr *fVar, Expr *fCond, Expr *fExpr, TypeBase base)
{
  Expr *lExpr;
  int lsubop;
  
  lsubop = SUBOP_S(base);
  lExpr = (Expr *) NewTriopSubNode( cg, ASSIGN_COND_OP, lsubop, fVar, fCond, fExpr);
  static_cast< Trinary * >( lExpr )->type = GetStandardType( cg, base, 0, 0);
  return lExpr;
} // GenCondSAssign

/*
 * GenCondSVAssign() - Create a vector conditional assignment with a scalar Boolean argument.
 *
 */

Expr *GenCondSVAssign( CgContext *cg, Expr *fVar, Expr *fCond, Expr *fExpr, TypeBase base, int len)
{
  Expr *lExpr;
  int lsubop;
  
  lsubop = SUBOP_SV(len, base);
  lExpr = (Expr *) NewTriopSubNode( cg, ASSIGN_COND_SV_OP, lsubop, fVar, fCond, fExpr);
  static_cast< Trinary * >( lExpr )->type = GetStandardType( cg, base, len, 0);
  return lExpr;
} // GenCondSVAssign

/*
 * GenCondVAssign() - Create a vector conditional assignment with a vector Boolean argument.
 *
 */

Expr *GenCondVAssign( CgContext *cg, Expr *fVar, Expr *fCond, Expr *fExpr, TypeBase base, int len)
{
  Expr *lExpr;
  int lsubop;
  
  lsubop = SUBOP_V(len, base);
  lExpr = (Expr *) NewTriopSubNode( cg, ASSIGN_COND_V_OP, lsubop, fVar, fCond, fExpr);
  static_cast< Trinary * >( lExpr )->type = GetStandardType( cg, base, len, 0);
  return lExpr;
} // GenCondVAssign

/*
 * GenCondGenAssign() - Create a geeral conditional assignment.
 *
 */

Expr *GenCondGenAssign( CgContext *cg, Expr *fVar, Expr *fCond, Expr *fExpr)
{
  Expr *lExpr;
  int lsubop;
  
  lsubop = 0;
  lExpr = (Expr *) NewTriopSubNode( cg, ASSIGN_COND_GEN_OP, lsubop, fVar, fCond, fExpr);
  static_cast< Trinary * >( lExpr )->type = fVar->type;
  return lExpr;
} // GenCondGenAssign

/*
 * GenBoolAnd() - Create a Boolean &&.
 *
 */

Expr *GenBoolAnd( CgContext *cg, Expr *aExpr, Expr *bExpr)
{
  Expr *lExpr;
  int lsubop;
  
  lsubop = SUBOP__(TB_Boolean);
  lExpr = (Expr *) NewBinopSubNode( cg, BAND_OP, lsubop, aExpr, bExpr);
  static_cast< Binary * >( lExpr )->type = cg->BooleanType;
  return lExpr;
} // GenBoolAnd

/*
 * GenBoolAndVec() - Create a Boolean Vector &&.
 *
 */

Expr *GenBoolAndVec( CgContext *cg, Expr *aExpr, Expr *bExpr, int len)
{
  Expr *lExpr;
  int lsubop;
  
  lsubop = SUBOP_V(len, TB_Boolean);
  lExpr = (Expr *) NewBinopSubNode( cg, BAND_V_OP, lsubop, aExpr, bExpr);
  static_cast< Binary * >( lExpr )->type = GetStandardType( cg, TB_Boolean, len, 0);
  return lExpr;
} // GenBoolAndVec

/*
 * GenBoolSmear() - Smear a Boolean Vector @xxxx.
 *
 */

Expr *GenBoolSmear( CgContext *cg, Expr *fExpr, int len)
{
  Expr *lExpr;
  int lsubop, mask;
  
  mask = (0 << 6) | (0 << 4) | (0 << 2) | 0;
  lsubop = SUBOP_Z(mask, len, 0, TB_Boolean);
  lExpr = (Expr *) NewUnopSubNode( cg, SWIZZLE_Z_OP, lsubop, fExpr);
  static_cast< Binary * >( lExpr )->type = GetStandardType( cg, TB_Boolean, len, 0);
  return lExpr;
} // GenBoolSmear

/*
 * GenExprList() - Add a node to an expression list.
 *
 */

Expr *GenExprList( CgContext *cg, Expr *fExpr, Expr *nExpr, Type *ftype)
{
  Expr *lExpr, *tExpr;
  
  lExpr = (Expr *) NewBinopNode( cg, EXPR_LIST_OP, nExpr, NULL);
  lExpr->type = ftype;
  if (fExpr) {
    tExpr = fExpr;
    while (static_cast< Binary * >( tExpr )->right)
      tExpr = static_cast< Binary * >( tExpr )->right;
    static_cast< Binary * >( tExpr )->right = lExpr;
  } else {
    fExpr = lExpr;
  }
  return fExpr;
} // GenExprList

/*
 * GenVecMult() - Create a Vector Multiply
 *
 */

Expr *GenVecMult( CgContext *cg, Expr *aExpr, Expr *bExpr, TypeBase base, int len)
{
  Expr *lExpr;
  int lsubop;
  
  lsubop = SUBOP_V(len, base);
  lExpr = (Expr *) NewBinopSubNode( cg, MUL_V_OP, lsubop, aExpr, bExpr);
  static_cast< Binary * >( lExpr )->type = GetStandardType( cg, base, len, 0);
  return lExpr;
} // GenVecMult

/*
 * GenScaleVecMult() - Create a smeared Scalar-Vector Multiply
 *
 */

Expr *GenScaleVecMult( CgContext *cg, Expr *aExpr, Expr *bExpr, TypeBase base, int len)
{
  Expr *lExpr;
  int lsubop;
  
  lsubop = SUBOP_V(len, base);
  lExpr = (Expr *) NewBinopSubNode( cg, MUL_VS_OP, lsubop, aExpr, bExpr);
  static_cast< Binary * >( lExpr )->type = GetStandardType( cg, base, len, 0);
  return lExpr;
} // GenScaleVecMult

/*
 * GenVecAdd() - Create a Vector Addition
 *
 */

Expr *GenVecAdd( CgContext *cg, Expr *aExpr, Expr *bExpr, TypeBase base, int len)
{
  Expr *lExpr;
  int lsubop;
  
  lsubop = SUBOP_V(len, base);
  lExpr = (Expr *) NewBinopSubNode( cg, ADD_V_OP, lsubop, aExpr, bExpr);
  static_cast< Binary * >( lExpr )->type = GetStandardType( cg, base, len, 0);
  return lExpr;
} // GenVecAdd

/*
 * GenConvertVectorSize() - Convert a vector to another length, or to a scalar if newlen is 1.
 *        Return original expression if no conversion needed.
 */

Expr *GenConvertVectorLength( CgContext *cg, Expr *fExpr, TypeBase base, int len, int newlen)
{
  int mask, ii;
  Expr *lExpr;
  
  if (newlen != len) {
    for (ii = 0; ii < newlen; ii++) {
      if (ii < len) {
        mask |= ii << (ii*2);
      } else {
        mask |= len - 1; // Use last component if source shorter than dest
      }
    }
    if (newlen == 1)
      newlen = 0; // I.e. scalar, not array[1]
    lExpr = (Expr *) NewUnopSubNode( cg, SWIZZLE_Z_OP, SUBOP_Z(mask, newlen, len, base), fExpr);
    static_cast< Unary * >( lExpr )->type = GetStandardType( cg, base, newlen, 0);
  } else {
    lExpr = fExpr;
  }
  return lExpr;
} // GenConvertVectorLength

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////// Duplicate Functions /////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * DuplicateNode() - Duplicate a node.
 *
 */

Expr *DuplicateNode( CgContext * cg, Expr *fExpr, void *arg1, int arg2)
{
  Expr *lExpr;
  
  if (fExpr) {
    switch (fExpr->kind) {
      case SYMB_N:
        lExpr = DupSymbNode( static_cast< Symb *>( fExpr ) );
        break;
      case CONST_N:
        lExpr = DupConstNode( static_cast< Constant *>( fExpr ) );
        break;
      case UNARY_N:
        lExpr = DupUnaryNode( static_cast< Unary *>( fExpr ) );
        break;
      case BINARY_N:
        lExpr = DupBinaryNode( static_cast< Binary *>( fExpr ) );
        break;
      case TRINARY_N:
        lExpr = DupTrinaryNode( static_cast< Trinary *>( fExpr ) );
        break;
      default:
        assert(!"bad kind to DuplicateNode()");
        lExpr = fExpr;
        break;
    }
  } else {
    lExpr = NULL;
  }
  return lExpr;
} // DuplicateNode

/*
 * DupExpr() - Duplicate an expression tree.
 *
 */

Expr *DupExpr( CgContext * cg, Expr *fExpr)
{
  return PreApplyToNodes( cg, DuplicateNode, fExpr, NULL, 0);
} // DupExpr

/*
 * DuplicateStatement() - Duplicate a statement.
 *
 */

Stmt *DuplicateStatement( CgContext *cg, Stmt *fStmt, void *arg1, int arg2)
{
  Stmt *lStmt;
  
  if (fStmt) {
    switch (fStmt->kind) {
      case EXPR_STMT:
        lStmt = NewExprStmt( cg, &fStmt->loc, static_cast< ExprStmt * >( fStmt )->expr );
        break;
      case IF_STMT:
        lStmt = NewIfStmt( cg, &fStmt->loc, static_cast< IfStmt * >( fStmt )->cond,
                          static_cast< IfStmt * >( fStmt )->thenstmt, static_cast< IfStmt * >( fStmt )->elsestmt);
        break;
      case WHILE_STMT:
      case DO_STMT:
        lStmt = NewWhileStmt( cg, &fStmt->loc, static_cast< WhileStmt * >( fStmt )->kind,
                             static_cast< WhileStmt * >( fStmt )->cond, static_cast< WhileStmt * >( fStmt )->body);
        break;
      case FOR_STMT:
        lStmt = NewForStmt( cg, &fStmt->loc, static_cast< ForStmt * >( fStmt )->init,
                           static_cast< ForStmt * >( fStmt )->cond, static_cast< ForStmt * >( fStmt )->step, static_cast< ForStmt * >( fStmt )->body);
        break;
      case BLOCK_STMT:
        lStmt = NewBlockStmt( cg, &fStmt->loc, static_cast< BlockStmt * >( fStmt )->body);
        break;
      case RETURN_STMT:
        lStmt = NewReturnStmt( cg, &fStmt->loc, NULL, static_cast< ExprStmt * >( fStmt )->expr );
        break;
      case DISCARD_STMT:
        lStmt = NewDiscardStmt( cg, &fStmt->loc, static_cast< DiscardStmt * >( fStmt )->cond);
        break;
      case COMMENT_STMT:
        lStmt = NewCommentStmt( cg, &fStmt->loc,
                               cg->GetString(static_cast< CommentStmt * >( fStmt )->str));
        break;
      default:
        lStmt = fStmt;
        assert(!"DuplicateStatement() - not yet finished");
        break;
    }
    PreApplyToExpressions( cg, DuplicateNode, lStmt, arg1, arg2);
  } else {
    lStmt = NULL;
  }
  return lStmt;
} // DuplicateStatement

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////// Named Constant Substitution /////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * ConvertNamedConstantsExpr() - Replace references to names constants with a const node.
 *
 */

Expr *ConvertNamedConstantsExpr( CgContext *cg, Expr *fExpr, void *arg1, int arg2)
{
  Symbol *lSymb;
  Type *lType;
  Expr *lExpr;
  int base;
  
  lExpr = fExpr;
  if (fExpr) {
    switch (fExpr->kind) {
      case SYMB_N:
        
        // The only named constants are "true" and "false":
        
        lSymb = static_cast< Symb * >( fExpr )->symbol;
        lType = lSymb->type;
        if (lSymb->kind == SK_Constant) {
          if (IsScalar(lType)) {
            base = GetBase(lType);
            switch (base) {
              case TB_Boolean:
                lExpr = (Expr *) NewBConstNode( cg, BCONST_OP, lSymb->details.con.value,
                                               TB_Boolean);
                break;
            }
          }
        }
        break;
      case DECL_N:
      case CONST_N:
      case UNARY_N:
      case BINARY_N:
      case TRINARY_N:
        break;
      default:
        FatalError( cg, "bad kind to ConvertNamedConstantsExpr()");
        break;
    }
  } else {
    lExpr = NULL;
  }
  return lExpr;
} // ConvertNamedConstantsExpr

///////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////// Remove Empty Statements //////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * RemoveEmptyStatementsStmt() - Expand inline function calls in a statement.
 *
 */

Stmt *RemoveEmptyStatementsStmt( CgContext *cg, Stmt *fStmt, void *arg1, int arg2)
{
  if (fStmt->kind == EXPR_STMT) {
    if (!static_cast< ExprStmt * >( fStmt )->expr)
      fStmt = NULL;
  }
  return fStmt;
} // RemoveEmptyStatementsStmt

///////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////// Remove Empty Statements //////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

typedef struct DebugFunctionData_Rec {
  Symbol *color;
  Symbol *flag;
  Symbol *outConnector;
  Symbol *COLSymb;
} DebugFunctionData;

/*
 * ConvertDebugCallsStmt() - Expand inline function calls in a statement.
 *
 */

Stmt *ConvertDebugCallsStmt( CgContext *cg, Stmt *fStmt, void *arg1, int arg2)
{
  DebugFunctionData *lDebugData = (DebugFunctionData *) arg1;
  Expr *eExpr, *sExpr, *lExpr, *mExpr, *bExpr, *rExpr;
  Stmt *lStmt, *mStmt;
  Symbol *lSymb;
  
  if (fStmt->kind == EXPR_STMT) {
    
    // Look for a call to "debug(float4)":
    
    eExpr = static_cast< ExprStmt * >( fStmt )->expr;
    if (eExpr && eExpr->kind == BINARY_N && static_cast< Binary * >( eExpr )->op == FUN_BUILTIN_OP) {
      sExpr = static_cast< Binary * >( eExpr )->left;
      if (sExpr->kind == SYMB_N) {
        lSymb = static_cast< Symb * >( sExpr )->symbol;
#define BUILTIN_GROUP_NV30FP_DBG     0
        if (lSymb->details.fun.group == BUILTIN_GROUP_NV30FP_DBG &&
            lSymb->details.fun.index == 0x444)
        {
          if (arg2) {
            
            // Turn: "debug(arg);" statements into:
            //
            //     $debug-color@@(!$debug-set) = arg;
            //     $debug-set = true;
            
            rExpr = static_cast< Binary * >( static_cast< Binary * >( eExpr )->right )->left;
            mExpr = GenSymb( cg, lDebugData->flag);
            mExpr = GenBoolNot( cg, mExpr);
            lExpr = GenSymb( cg, lDebugData->color);
            lExpr = GenCondSVAssign( cg, lExpr, mExpr, rExpr, TB_Float, 4);
            lStmt = (Stmt *) NewExprStmt( cg, NULL, lExpr);
            lExpr = GenSymb( cg, lDebugData->flag);
            rExpr = GenBoolConst( cg, 1);
            lExpr = GenBoolAssign( cg, lExpr, rExpr);
            mStmt = (Stmt *) NewExprStmt( cg, NULL, lExpr);
            lStmt->next = mStmt;
            fStmt = lStmt;
          } else {
            
            // Eliminate: "debug(arg);" statements:
            
            fStmt = NULL;
          }
        }
      }
    }
  } else if (arg2 && fStmt->kind == RETURN_STMT) {
    rExpr = GenSymb( cg, lDebugData->color);
    mExpr = GenSymb( cg, lDebugData->flag);
    lExpr = GenSymb( cg, lDebugData->outConnector);
    bExpr = GenMember( cg, lDebugData->COLSymb);
    lExpr = GenMemberSelector( cg, lExpr, bExpr);
    lExpr = GenCondSVAssign( cg, lExpr, mExpr, rExpr, TB_Float, 4);
    lStmt = (Stmt *) NewExprStmt( cg, NULL, lExpr);
    lStmt->next = fStmt;
    fStmt = lStmt;
  }
  return fStmt;
} // ConvertDebugCallsStmt

/*
 * ConvertDebugCalls() - Convert calls to debug to either assignments to global variables, or
 *         delete them, depending on the value of DebugFlag.  Also defines global variables.
 *
 */

Stmt *ConvertDebugCalls( CgContext *cg, SourceLoc *loc, Scope *fScope, Stmt *fStmt, int DebugFlag)
{
  Symbol *debugColor, *debugSet;
  DebugFunctionData debugData;
  Expr *lExpr, *rExpr;
  Stmt *lStmt, *rStmt;
  StmtList lStatements;
  Type *lType;
  Atom vname, COLname;
  float fdata[4];
  
  rStmt = fStmt;
  if (DebugFlag) {
    lStatements.first = lStatements.last = NULL;
    //outputSymb = fScope->outConnector;
    
    // 1) Define global vars used by "-debug" mode:
    //
    //     float $debug-color[4];
    //     bool  $debug-set = 0.0f;
    
    vname = cg->GetAtom("$debug-color");
    lType = GetStandardType( cg, TB_Float, 4, 0);
    debugColor = DefineVar( cg, loc, fScope, vname, lType);
    vname = cg->GetAtom("$debug-set");
    debugSet = DefineVar( cg, loc, fScope, vname, cg->BooleanType);
    lExpr = GenSymb( cg, debugSet);
    rExpr = GenBoolConst( cg, 0);
    lExpr = GenBoolAssign( cg, lExpr, rExpr);
    lStmt = (Stmt *) NewExprStmt( cg, loc, lExpr);
    AppendStatements(&lStatements, lStmt);
    
    // 1A) Must initialize $debug-color to something or else the code generator
    //     gets all bent out of shape:
    //
    //     $debug-color = { 0.0f, 0.0f, 0.0f, .0f };
    
    fdata[0] = fdata[1] = fdata[2] = fdata[3] = 0.0f;
    rExpr = GenFConstV( cg, fdata, 4, TB_Float);
    lExpr = GenSymb( cg, debugColor);
    lExpr = GenVAssign( cg, lExpr, rExpr, TB_Float, 4);
    lStmt = (Stmt *) NewExprStmt( cg, loc, lExpr);
    AppendStatements(&lStatements, lStmt);
    
    // 2) Expand calls to "debug(float4);" into the following code:
    //
    //     $debug-color@@(!$debug-set) = float4;
    //     $debug-set = true;
    
    debugData.color = debugColor;
    debugData.flag = debugSet;
    debugData.outConnector = cg->theHal->varyingOut;
    assert(debugData.outConnector);
    COLname = cg->GetAtom("COL");
    lType = debugData.outConnector->type;
    assert(IsCategory(lType, TC_Connector));
    debugData.COLSymb = LookupLocalSymbol( cg, static_cast< TypeStruct * >( lType )->members, COLname);
    
    // 3) And add the following statement to the end of the program (i.e. before each
    //    "return" statement):
    //
    //     output.COL@@($debug-set) = $debug-color;
    
    lStmt = PostApplyToStatements( cg, ConvertDebugCallsStmt, fStmt, &debugData, 1);
    AppendStatements(&lStatements, lStmt);
    
    rStmt = lStatements.first;
  } else {
    rStmt = PostApplyToStatements( cg, ConvertDebugCallsStmt, fStmt, NULL, 0);
  }
  return rStmt;
} // ConvertDebugCalls

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////// Flatten Chained Assignment Statements ///////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

int IsAssignSVOp(Expr *fExpr)
{
  int lop;
  
  if (fExpr) {
    if (fExpr->kind == BINARY_N) {
      lop = static_cast< Binary * >( fExpr )->op;
      if (lop == ASSIGN_OP || lop == ASSIGN_V_OP || lop == ASSIGN_GEN_OP ||
          lop == ASSIGN_MASKED_KV_OP)
      {
        return 1;
      }
    }
  }
  return 0;
} // IsAssignSVOp

int IsAssignCondSVOp(Expr *fExpr)
{
  int lop;
  
  if (fExpr) {
    if (fExpr->kind == TRINARY_N) {
      lop = static_cast< Trinary * >( fExpr )->op;
      if (lop == ASSIGN_COND_OP || lop == ASSIGN_COND_V_OP || lop == ASSIGN_COND_SV_OP ||
          lop == ASSIGN_COND_GEN_OP)
      {
        return 1;
      }
    }
  }
  return 0;
} // IsAssignCondSVOp

int IsCastOp(Expr *fExpr)
{
  int lop;
  
  if (fExpr) {
    if (fExpr->kind == UNARY_N) {
      lop = static_cast< Unary * >( fExpr )->op;
      if (lop == CAST_CS_OP || lop == CAST_CV_OP || lop == CAST_CM_OP)
      {
        return 1;
      }
    }
  }
  return 0;
} // IsCastOp

/*
 * NewTmp() - Return a symbol expression for a temp symbol that can
 * hold the value of fExpr.
 *
 */

Symb *NewTmp( CgContext *cg, const Expr *fExpr)
{
  Symbol *tmpSym;
  Type *tmpType = DupType(fExpr->type);
	tmpType->domain = TD_Unknown;
	tmpType->qualifier = TQ_None;
  tmpSym = UniqueSymbol( cg, cg->currentScope, tmpType, SK_Variable);
  return NewSymbNode( cg, VARIABLE_OP, tmpSym);
} // NewTmp

/*
 * FlattenChainedAssignmentsStmt() - Transform chained assignments into multiple simple
 *         assignments.
 *
 *  float3 A, C;  half B;
 *
 *  Simle case:
 *
 *  A = B = C;    (which is equivanemt to:)     A = (float) ( B = (half) C ) ;
 *
 *      =                        =               =
 *    /   \                    /   \           /   \
 *  A      (f)     >>-->>     B     (h)      A      (f)
 *            \                        \               \
 *              =                        C               B'
 *            /   \
 *          B      (h)
 *                    \
 *                      C
 *
 *  where B' is a duplicate of B.
 *
 *  If B contains any function calls, they must be hoisted prior to this step.
 *
 */

Stmt *FlattenChainedAssignmentsStmt( CgContext *cg, Stmt *fStmt, void *arg1, int arg2)
{
  Stmt *bStmt, *rStmt;
  Expr *assigna, *assignb;
  Expr *pb, **ppassignb;
  
  if (fStmt->kind == EXPR_STMT) {
    rStmt = fStmt;
    assigna = static_cast< ExprStmt * >( fStmt )->expr;
    while (1) {
      if (IsAssignSVOp(assigna)) {
        ppassignb = &static_cast< Binary * >( assigna )->right;
      } else if (IsAssignCondSVOp(assigna)) {
        ppassignb = &static_cast< Trinary * >( assigna )->arg3;
      } else {
        break;
      }
      // Traverse list of type casts, if any:
      while (IsCastOp(*ppassignb))
        ppassignb = &static_cast< Unary * >( (*ppassignb) )->arg;
      if (IsAssignSVOp(*ppassignb)) {
        pb = static_cast< Binary * >( (*ppassignb) )->left;
      } else if (IsAssignCondSVOp(*ppassignb)) {
        pb = static_cast< Trinary * >( (*ppassignb) )->arg1;
      } else {
        break;
      }
      assignb = *ppassignb;
      bStmt = (Stmt *) NewExprStmt( cg, &fStmt->loc, assignb);
      *ppassignb = PreApplyToNodes( cg, DuplicateNode, pb, arg1, arg2);
      bStmt->next = rStmt;
      rStmt = bStmt;
      assigna = assignb;
    }
  } else {
    rStmt = fStmt;
  }
  return rStmt;
} // FlattenChainedAssignmentsStmt

/*
 * PutIndexEpxrsInTemps() - Puts all array indicies in temps and
 * builds a comma list of assignments of the indices to the
 * temporaries.
 *
 * A[e1]...[eN] -> t1 = e1, ..., tN = eN : A[t1]...[tN]
 */
Expr *PutIndexEpxrsInTemps( CgContext *cg, Expr *fExpr)
{
  Expr *assignments = NULL;
  Expr *assign;
  Expr *tmp;
  
  assert(IsArrayIndex(fExpr));
  if (IsArrayIndex(static_cast< Binary * >( fExpr )->left))
    assignments = PutIndexEpxrsInTemps( cg, static_cast< Binary * >( fExpr )->left);
  
  tmp = (Expr *) NewTmp( cg, static_cast< Binary * >( fExpr )->right);
  assign = NewSimpleAssignment( cg, NULL, DupNode( cg, tmp), static_cast< Binary * >( fExpr )->right, 0);
  
  if (!assignments)
    assignments = assign;
  else
    assignments = (Expr *) NewBinopNode(  cg, COMMA_OP, assignments, assign);
  
  if (IsArrayIndex(static_cast< Binary * >( fExpr )->right))
    assignments = (Expr *) NewBinopNode(  cg, COMMA_OP, assignments,
                                        PutIndexEpxrsInTemps( cg, static_cast< Binary * >( fExpr )->right));
  
  static_cast< Binary * >( fExpr )->right = tmp;
  return assignments;
} // PutIndexEpxrsInTemps

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////// Expand Increment/Decrement Expressions ////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * ExpandIncDecExpr() - Expand increment/decrement operations.
 *
 * Pre increment/decrement is the simple case:
 *
 *   ++A -> A += 1
 *
 * Post increment/decrement is a little more tricky:
 *
 *   If A is simple (i.e. not an array reference):
 *
 *     A++ -> tmp = A, A += 1, tmp
 *
 *   If A is an array reference:
 *
 *     A[i1]...[iN]++ ->
 *       tmpi1 = i1, ..., tmpiN = iN, tmpv = A[tmpi1]...[tmpiN],
 *       A[tmpi1]...[tmpiN] += 1, tmpv
 *
 */

Expr *ExpandIncDecExpr( CgContext *cg, Expr *fExpr, void *arg1, int arg2)
{
  int pre = 0;
  int newop = -1;
  Expr *oneExpr;
  Expr *result = NULL;
  
  if (fExpr->kind == UNARY_N) {
    switch (static_cast< Unary * >( fExpr )->op) {
      case PREDEC_OP:
        pre = 1;
      case POSTDEC_OP:
        newop = ASSIGNMINUS_OP;
        break;
      case PREINC_OP:
        pre = 1;
      case POSTINC_OP:
        newop = ASSIGNPLUS_OP;
        break;
    }
  }
  
  if (newop == -1)
    return fExpr;
  
  oneExpr = (Expr *) NewIConstNode( cg, ICONST_OP, 1, TB_Int);
  
  if (pre)
    result = (Expr *) NewBinopNode(  cg, (opcode)newop, static_cast< Unary * >( fExpr )->arg, oneExpr);
  else {
    Expr *idxAssigns = IsArrayIndex(static_cast< Unary * >( fExpr )->arg)
    ? PutIndexEpxrsInTemps( cg, static_cast< Unary * >( fExpr )->arg) : NULL;
    Expr *tmp = (Expr *) NewTmp( cg, static_cast< Unary * >( fExpr )->arg);
    Expr *tmpAssign = NewSimpleAssignment( cg, NULL, tmp, static_cast< Unary * >( fExpr )->arg, 0);
    Expr *incdec = (Expr *) NewBinopNode(  cg, (opcode)newop, DupNode( cg, static_cast< Unary * >( fExpr )->arg), oneExpr);
    Expr *rval = DupNode( cg, tmp);
    result = (Expr *) NewBinopNode( cg, COMMA_OP, incdec, rval);
    result = (Expr *) NewBinopNode( cg, COMMA_OP, tmpAssign, result);
    if (idxAssigns)
      result = (Expr *) NewBinopNode( cg, COMMA_OP, idxAssigns, result);
  }
  
  return result;
  
} // ExpandIncDecExpr

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////// Expand Compound Assignment Expressions ////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * ExpandCompoundAssignmentExpr() - Transform compound assignments
 * into simple assignments.
 *
 * If A is simple (i.e. not an array reference):
 *
 *   A op= B -> A = A op B
 *
 * If A is an array reference:
 *
 *   A[e1]...[eN] op= B ->
 *     tmp1 = e1, ..., tmpN = eN, A[tmp1]...[tmpN] = A[tmp1]...[tmpN] op B
 *
 */

Expr *ExpandCompoundAssignmentExpr( CgContext *cg, Expr *fExpr, void *arg1, int arg2)
{
  int newop = -1;
  int opname = 0;
  int intOnly = 0;
  Expr *lexpr, *rExpr;
  Expr *result = NULL;
  Expr *idxAssigns = NULL;
  
  if (fExpr->kind == BINARY_N) {
    switch (static_cast< Binary * >( fExpr )->op) {
      case ASSIGNMINUS_OP:
        newop = SUB_OP;
        opname = '-';
        break;
      case ASSIGNMOD_OP:
        newop = MOD_OP;
        opname = '%';
        intOnly = 1;
        break;
      case ASSIGNPLUS_OP:
        newop = ADD_OP;
        opname = '+';
        break;
      case ASSIGNSLASH_OP:
        newop = DIV_OP;
        opname = '/';
        break;
      case ASSIGNSTAR_OP:
        newop = MUL_OP;
        opname = '*';
        break;
    }
  }
  
  if (newop == -1)
    return fExpr;
  
  lexpr = static_cast< Binary * >( fExpr )->left;
  rExpr = static_cast< Binary * >( fExpr )->right;
  
  if (IsArrayIndex(lexpr))
    idxAssigns = PutIndexEpxrsInTemps( cg, lexpr);
  
  result = NewBinaryOperator( cg, NULL, newop, opname, lexpr, rExpr, intOnly);
  result = NewSimpleAssignment( cg, NULL, DupNode( cg, lexpr), result, 0);
  if (idxAssigns)
    result = (Expr *) NewBinopNode( cg, COMMA_OP, idxAssigns, result);
  
  return result;
} // ExpandCompoundAssignmentExpr

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////// Flatten Chained Assignment Statements ///////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

typedef struct MData_rec {
  Scope *scope;
  int numtemps;
  StmtList statements;
} MData;

/*
 * IsConstant()
 *
 */

int IsConstant(Expr *fExpr)
{
  switch (fExpr->kind) {
    case CONST_N:
      return 1;
    default:
      break;
  }
  return 0;
} // IsConstant

/*
 * IsVariable()
 *
 */

int IsVariable(Expr *fExpr)
{
  switch (fExpr->kind) {
    case SYMB_N:
      return 1;
    default:
      break;
  }
  return 0;
} // IsVariable

/*
 * IsCompileTimeAddress()
 *
 */

int IsCompileTimeAddress(Expr *fExpr)
{
  if (fExpr->IsLValue) {
    switch (fExpr->kind) {
      case SYMB_N:
        return 1;
      case BINARY_N:
        switch (static_cast< Binary * >( fExpr )->op) {
          case MEMBER_SELECTOR_OP:
            return IsCompileTimeAddress(static_cast< Binary * >( fExpr )->left);
          case ARRAY_INDEX_OP:
            if (IsCompileTimeAddress(static_cast< Binary * >( fExpr )->left)) {
              if (IsConstant(static_cast< Binary * >( fExpr )->right)) {
                return 1;
              }
            }
            break;
          default:
            break;
        }
        break;
      default:
        break;
    }
  }
  return 0;
} // IsCompileTimeAddress

/*
 * GetConstIndex()
 *
 */

int GetConstIndex(Expr *fExpr)
{
  switch (fExpr->kind) {
    case CONST_N:
      switch (static_cast< Constant * >( fExpr )->op) {
        case ICONST_OP:
        case BCONST_OP:
          return static_cast< Constant * >( fExpr )->val[0].i;
          break;
        case FCONST_OP:
        case HCONST_OP:
        case XCONST_OP:
          return (int) static_cast< Constant * >( fExpr )->val[0].f;
          break;
        default:
          break;
      }
      break;
    default:
      break;
  }
  return 0;
} // GetConstIndex

///////////////////////////////////////////////////////////////////////////////////////////////
////////////////////// Flatten comma expressions into statement lists /////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

static int IsComma(const Expr *fExpr)
{
  return fExpr->kind == BINARY_N && static_cast< const Binary * >( fExpr )->op == COMMA_OP;
} // IsComma

/*
 * FlattenCommasExpr()
 *
 * op (a, b) -> a, (op b)
 * (a, b) op c -> a, (b op c)
 * A[e1]...[eN] = (b, c) -> (tmp1 = e1), ..., (tmpN = eN), b, A[tmp1]...[tmpN] = c
 * a = (b, c) -> b, a = c
 * a op (b, c) -> (tmpa = a), b, (tmpa op c)
 * (a, b) ? c : d -> a, (b ? c : d)
 * a ? (b, c) : d -> (tmpa = a), b, (tmpa ? c : d)
 * a ? b : (c, d) -> (tmpa = a), (tmpb = b), c, (tmpa ? tmpb : d)
 */

static Expr *FlattenCommasExpr( CgContext *cg, Expr *fExpr, void *arg1, int arg2)
{
  switch (fExpr->kind) {
    case UNARY_N:
      if (IsComma(static_cast< Unary * >( fExpr )->arg)) {
        Expr *comma = static_cast< Unary * >( fExpr )->arg;
        static_cast< Unary * >( fExpr )->arg = static_cast< Binary * >( comma )->right;
        fExpr = (Expr *) NewBinopNode( cg, COMMA_OP, static_cast< Binary * >( comma )->left, fExpr);
      }
      break;
    case BINARY_N: {
      Binary *bin = static_cast< Binary* >( fExpr );
      if (IsComma(bin->left)) {
        Expr *comma = bin->left;
        bin->left = static_cast< Binary * >( comma )->right;
        fExpr = (Expr *) NewBinopNode( cg, COMMA_OP, static_cast< Binary * >( comma )->left, (Expr *) bin);
      }
      if (bin->right && IsComma(bin->right)) {
        // Assignments/lvalues are special
        if (bin->op == ASSIGN_OP || bin->op == ASSIGN_V_OP) {
          Expr *idxAssigns = IsArrayIndex(bin->left)
          ? PutIndexEpxrsInTemps( cg, bin->left) : NULL;
          fExpr = (Expr *) NewBinopNode( cg, COMMA_OP, static_cast< Binary * >( bin->right )->left,
                                        fExpr);
          bin->right = static_cast< Binary * >( bin->right )->right;
          if (idxAssigns)
            fExpr = (Expr *) NewBinopNode( cg, COMMA_OP, idxAssigns, fExpr);
        } else if (bin->op != COMMA_OP) {
          // no need to lift comma ops above a comma op
          Expr *comma = bin->right;
          Expr *tmp = (Expr *) NewTmp( cg, bin->left);
          Expr *assign = NewSimpleAssignment( cg, NULL, tmp, bin->left, 0);
          bin->left = DupNode( cg, tmp);
          bin->right = static_cast< Binary * >( comma )->right;
          fExpr = (Expr *) NewBinopNode( cg, COMMA_OP, static_cast< Binary * >( comma )->left, (Expr *) bin);
          fExpr = (Expr *) NewBinopNode( cg, COMMA_OP, assign, fExpr);
        }
      }
      break;
    }
    case TRINARY_N: {
      Trinary *tri = static_cast< Trinary* >( fExpr );
      if (IsComma(tri->arg1)) {
        Expr *comma = tri->arg1;
        tri->arg1 = static_cast< Binary * >( comma )->right;
        fExpr = (Expr *) NewBinopNode( cg, COMMA_OP, static_cast< Binary * >( comma )->left, (Expr *) tri);
      }
      if (IsComma(tri->arg2)) {
        Expr *comma = tri->arg2;
        Expr *tmp = (Expr *) NewTmp( cg, tri->arg1);
        Expr *assign = NewSimpleAssignment( cg, NULL, tmp, tri->arg1, 0);
        tri->arg1 = DupNode( cg, tmp);
        tri->arg2 = static_cast< Binary * >( comma )->right;
        fExpr = (Expr *) NewBinopNode( cg, COMMA_OP, static_cast< Binary * >( comma )->left, (Expr *) tri);
        fExpr = (Expr *) NewBinopNode( cg, COMMA_OP, assign, fExpr);
      }
      if (IsComma(tri->arg3)) {
        Expr *comma = tri->arg3;
        Expr *tmp1 = (Expr *) NewTmp( cg, tri->arg1);
        Expr *tmp2 = (Expr *) NewTmp( cg, tri->arg2);
        Expr *assign1 = NewSimpleAssignment( cg, NULL, tmp1, tri->arg1, 0);
        Expr *assign2 = NewSimpleAssignment( cg, NULL, tmp2, tri->arg2, 0);
        tri->arg1 = DupNode( cg, tmp1);
        tri->arg2 = DupNode( cg, tmp2);
        tri->arg3 = static_cast< Binary * >( comma )->right;
        fExpr = (Expr *) NewBinopNode( cg, COMMA_OP, static_cast< Binary * >( comma )->left, (Expr *) tri);
        fExpr = (Expr *) NewBinopNode( cg, COMMA_OP, assign2, fExpr);
        fExpr = (Expr *) NewBinopNode( cg, COMMA_OP, assign1, fExpr);
      }
      break;
    }
  }
  
  return fExpr;
} // FlattenCommasExpr

/*
 * LinearizeCommasExpr() - Gets rid of all top-level comma expressions
 * by pulling them out into an expression list (returned through
 * arg1).
 *
 */

static Expr *LinearizeCommasExpr( CgContext *cg, Expr *fExpr, void *arg1, int arg2)
{
  Stmt ** fStmts = (Stmt **) arg1;
  while (IsComma(fExpr)) {
    Stmt * fStmt = (Stmt *) NewExprStmt( cg, NULL, static_cast< Binary * >( fExpr )->left);
    *fStmts = ConcatStmts(*fStmts, fStmt);
    fExpr = static_cast< Binary * >( fExpr )->right;
  }
  
  return fExpr;
} // LinearizeCommasExpr

/*
 * FlattenCommas()
 *
 */

static Stmt *FlattenCommasStmt( CgContext *cg, Stmt *fStmt, void *arg1, int arg2)
{
  Stmt *preCommaStmts = NULL;
  PreApplyToExpressionsLocal( cg, FlattenCommasExpr, fStmt, NULL, 0);
  PreApplyToExpressionsLocal( cg, LinearizeCommasExpr, fStmt, &preCommaStmts, 0);
  return ConcatStmts( preCommaStmts, fStmt);
} // FlattenCommas

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////// Chop Matrices up into Vectors ///////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * DeconstructMatricesExpr()
 *
 */

Expr *DeconstructMatricesExpr( CgContext *cg, Expr *fExpr, void *arg1, int arg2)
{
  Symbol *lSymb;
  MData *mdata;
  Type *lType;
  int base, len, len2;
  Atom vname;
	int index;
  
  mdata = (MData *) arg1;
  switch (fExpr->kind) {
    case BINARY_N:
      switch (static_cast< Binary * >( fExpr )->op) {
        case ARRAY_INDEX_OP:
          lType = static_cast< Binary * >( fExpr )->type;
          if (IsMatrix(static_cast< Binary * >( fExpr )->left->type, &len, &len2)) {
            base = GetBase(lType);
            if (IsConstant(static_cast< Binary * >( fExpr )->right)) {
              if (IsVariable(static_cast< Binary * >( fExpr )->left)) {
                index = GetConstIndex(static_cast< Binary * >( fExpr )->right);
                vname = GenerateIndexName( cg, static_cast< Binary * >( fExpr )->left, index);
                lSymb = LookupLocalSymbol( cg, mdata->scope, vname);
                if (!lSymb)
                  lSymb = DefineVar( cg, &lSymb->loc, mdata->scope, vname, lType);
                fExpr = GenSymb( cg, lSymb);
              } else {
                SemanticError( cg, NULL, ERROR___MATRIX_NOT_SIMPLE);
              }
            } else {
              SemanticError( cg, NULL, ERROR___ARRAY_INDEX_NOT_CONSTANT);
            }
          }
          break;
        default:
          break;
      }
      break;
    default:
      break;
  }
  return fExpr;
} // DeconstructMatricesExpr

/*
 * DeconstructMatricesStmt()
 *
 */

Stmt *DeconstructMatricesStmt( CgContext *cg, Stmt *fStmt, void *arg1, int arg2)
{
  MData *mdata;
  
  mdata = (MData *) arg1;
  mdata->statements.first = mdata->statements.last = NULL;
  PostApplyToExpressionsLocal( cg, DeconstructMatricesExpr, fStmt, arg1, arg2);
  if (mdata->statements.first) {
    mdata->statements.last->next = fStmt;
    fStmt = mdata->statements.first;
  }
  return fStmt;
} // DeconstructMatricesStmt

/*
 * DeconstructMatrices()
 *
 */

Stmt *DeconstructMatrices( CgContext *cg, Scope *fscope, Stmt *fStmt)
{
  MData mdata;
  
  InternalError( cg, NULL, ERROR___NO_MATRIX_DECONSTRUCTION);
  mdata.scope = fscope;
  mdata.numtemps = 0;
  fStmt = PreApplyToStatements( cg, DeconstructMatricesStmt, fStmt, &mdata, 0);
  return fStmt;
} // DeconstructMatrices

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Flatten Struct Assignments ////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * AssignStructMembers() - Assign individual memebers of one struct to another.
 *
 */

static void AssignStructMembers( CgContext *cg, StmtList *fStatements, int fop, Type *fType, Expr *varExpr,
                                Expr *valExpr, Expr *condExpr, int VectorCond)
{
  Expr *lExpr, *mExpr, *rExpr;
  TypeBase base;
	int len, len2;
  Symbol *lSymb;
  Type *lType;
  Stmt *lStmt;
  
  lSymb = static_cast< TypeStruct * >( fType )->members->symbols;
  while (lSymb) {
    len = len2 = 0;
    lType = lSymb->type;
    if (IsScalar(lType) || IsVector(lType, &len) || IsMatrix(lType, &len, &len2)) {
      base = GetBase(lType);
      lExpr = DupExpr( cg, varExpr);
      mExpr = GenMember( cg, lSymb);
      lExpr = GenMemberSelector( cg, lExpr, mExpr);
      rExpr = DupExpr( cg, valExpr);
      mExpr = GenMember( cg, lSymb);
      rExpr = GenMemberSelector( cg, rExpr, mExpr);
      if (condExpr != NULL) {
        if (len2 > 0) {
          lExpr = GenCondGenAssign( cg, lExpr, rExpr, condExpr);
        } else if (len > 0) {
          if (VectorCond) {
            lExpr = GenCondSVAssign( cg, lExpr, rExpr, condExpr, base, len);
          } else {
            lExpr = GenCondVAssign( cg, lExpr, rExpr, condExpr, base, len);
          }
        } else {
          lExpr = GenCondSAssign( cg, lExpr, rExpr, condExpr, base);
        }
        lStmt = (Stmt *) NewExprStmt( cg, NULL, lExpr);
      } else {
        if (len2 > 0) {
          lExpr = GenMAssign( cg, lExpr, rExpr, base, len, len2);
        } else if (len > 0) {
          lExpr = GenVAssign( cg, lExpr, rExpr, base, len);
        } else {
          lExpr = GenSAssign( cg, lExpr, rExpr, base);
        }
        lStmt = (Stmt *) NewExprStmt( cg, NULL, lExpr);
      }
      AppendStatements(fStatements, lStmt);
    } else {
      switch (GetCategory(lType)) {
        case TC_Struct:
          lExpr = DupExpr( cg, varExpr);
          mExpr = GenMember( cg, lSymb);
          lExpr = GenMemberSelector( cg, lExpr, mExpr);
          rExpr = DupExpr( cg, valExpr);
          mExpr = GenMember( cg, lSymb);
          rExpr = GenMemberSelector( cg, rExpr, mExpr);
          AssignStructMembers( cg, fStatements, fop, lType, lExpr, rExpr, condExpr, VectorCond);
          break;
        case TC_Array:
          // XYZZY Not Done Yet XYZZY //
          break;
        default:
          break;
      }
    }
    lSymb = lSymb->next;
  }
} // AssignStructMembers

/*
 * FlattenStructAssignment() - Convert struct assignments into multiple assignments of members.
 *
 */

Stmt *FlattenStructAssignment( CgContext *cg, Stmt *fStmt, void *arg1, int flevel)
{
  Stmt *rStmt;
  Expr *eExpr, *lExpr, *rExpr, *cExpr;
  int lop, lsubop;
  StmtList lStatements;
  Type *lType;
  
  if (fStmt) {
    switch (fStmt->kind) {
      case EXPR_STMT:
        eExpr = static_cast< ExprStmt * >( fStmt )->expr;
        if (IsAssignSVOp(eExpr)) {
          lType = static_cast< Binary * >( eExpr )->type;
          if (IsStruct(lType)) {
            if (IsAssignCondSVOp(eExpr)) {
              lop = static_cast< Trinary * >( eExpr )->op;
              lExpr = static_cast< Trinary * >( eExpr )->arg1;
              cExpr = static_cast< Trinary * >( eExpr )->arg2;
              rExpr = static_cast< Trinary * >( eExpr )->arg3;
              if (IsScalar(cExpr->type)) {
                AssignStructMembers( cg, &lStatements, lop, lType, lExpr, rExpr, cExpr, 0);
              } else {
                AssignStructMembers( cg, &lStatements, lop, lType, lExpr, rExpr, cExpr, 1);
              }
              rStmt = lStatements.first;
            } else {
              lop = static_cast< Binary * >( eExpr )->op;
              lExpr = static_cast< Binary * >( eExpr )->left;
              rExpr = static_cast< Binary * >( eExpr )->right;
              lsubop = static_cast< Binary * >( eExpr )->subop;
              lStatements.first = NULL;
              lStatements.last = NULL;
              AssignStructMembers( cg, &lStatements, lop, lType, lExpr, rExpr, NULL, 0);
              rStmt = lStatements.first;
            }
          } else {
            rStmt = fStmt;
          }
        } else {
          rStmt = fStmt;
        }
        break;
      default:
        rStmt = fStmt;
        break;
    }
  } else {
    rStmt = fStmt;
  }
  return rStmt;
} // FlattenStructAssignment

/*
 * FlattenStructAssignments() - Convert struct assignments into multiple member assignments.
 *
 */

static Stmt *FlattenStructAssignments( CgContext *cg, Scope *fScope, Stmt *fStmt)
{
  Stmt *lStmt;
  
  lStmt = PreApplyToStatements( cg, FlattenStructAssignment, fStmt, NULL, 0);
  return lStmt;
} // FlattenStructAssignments


///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////// Flatten If Statements ///////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * FlattenIfStatementsStmt() - Convert if statements into conditional assignments.
 *
 *     if (A > B)
 *         C = D;
 *     else
 *         E = F;
 *
 * becomes:
 *
 *     $if1 = A > B;
 *     C@@($if1) = D;
 *     E@@(!$if1) = F;
 *
 * and:
 *
 *     if (A > B)
 *         if (C > D)
 *             E = F;
 *         else
 *             G = H;
 *     else
 *         if (J > K)
 *             L = M;
 *         else
 *             N = P;
 *
 * becomes:
 *
 *     $if1 = A > B;
 *     $ife2 = C > D;
 *     $if2 = $if1 && $ife2;
 *     E@@($if2) = F;
 *     $if2 = $if1 && !$ife2;
 *     G@@($if2) = H;
 *     $ife2 = J > K;
 *     $if2 = !$if1 && $ife2;
 *     L@@($if2) = M;
 *     $if2 = !$if1 && !$ife2;
 *     N@@($if2) = P;
 *
 * Existing conditional assignments:
 *
 *     A@@XZ = B;
 *     C@@(D) = E;
 *
 * become:
 *
 *     A@@({ $if1, 0, $if1, 0 }) = B;
 *     C@@($if1@xyzw && D) = E;     or:        C@@($if1 && D) = E;
 *
 * Issues:
 *
 *   1) "out" parameters to function calls: not a problem if function calls
 *      have been previously inlined.
 *   2) Assumes "chained" assignments have already been split into simple assignments.
 *   3) Assumes all large asignments (structs, matrices, vectors > 4) have been eliminated.
 *
 */

struct FlattenIf {
  Scope *funScope;
  Symbol *ifSymbTotal;  // Current combined if value: "$if2" for level 2, NULL for level 0.
  Symbol *ifSymbParent; // Enclosing if's combined value: "$if1" for level 2, NULL for level <= 1.
  Symbol *ifSymbLocal;  // Current level's simple if value:  "$ife2" for level 2
};

static Stmt *FlattenIfStatementsStmt( CgContext *cg, Stmt *fStmt, void *arg1, int flevel)
{
#define IF_ROOT "$if"
#define IF_ROOT_E "$iflocal"
  
  Stmt *rStmt, *lStmt, *nStmt;
  Expr *eExpr, *lExpr, *rExpr, *ifvar, *nExpr, *mExpr, *tExpr;
  int level, lop, lsubop, nop, nsubop, mask, len, base, ii;
	Atom vname;
  FlattenIf *lFlatten;
  Symbol *lSymbTotal, *lSymbLocal, *ifSymbTotalSave, *ifSymbParentSave;
  StmtList lStatements;
  Type *lType;
  
  if (fStmt) {
    lFlatten = (FlattenIf *) arg1;
    level = flevel >= 0 ? flevel : -flevel;
    switch (fStmt->kind) {
      case IF_STMT:
        
        // Find $if1 variable(s) for this level:
        
        vname = GetNumberedAtom( cg, IF_ROOT, level + 1, 1, '\0');
        lSymbTotal = LookupSymbol( cg, lFlatten->funScope, vname);
        if (!lSymbTotal) {
          lSymbTotal = DefineVar( cg, &fStmt->loc, lFlatten->funScope, vname, cg->BooleanType);
        }
        if (level > 0) {
          vname = GetNumberedAtom( cg, IF_ROOT_E, level + 1, 1, '\0');
          lSymbLocal = LookupSymbol( cg, lFlatten->funScope, vname);
          if (!lSymbLocal) {
            lSymbLocal = DefineVar( cg, &fStmt->loc, lFlatten->funScope, vname, cg->BooleanType);
          }
        } else {
          lSymbLocal = lSymbTotal;
        }
        
        // Create assignment statement for local expression:
        
        lStatements.first = NULL;
        lStatements.last = NULL;
        lExpr = GenSymb( cg, lSymbLocal);
        lExpr = GenBoolAssign( cg, lExpr, static_cast< IfStmt * >( fStmt )->cond);
        lStmt = (Stmt *) NewExprStmt( cg, &fStmt->loc, lExpr);
        AppendStatements( &lStatements, lStmt);
        
        ifSymbTotalSave = lFlatten->ifSymbTotal;
        ifSymbParentSave = lFlatten->ifSymbParent;
        lFlatten->ifSymbParent = lFlatten->ifSymbLocal;
        lFlatten->ifSymbLocal = lSymbLocal;
        lFlatten->ifSymbTotal = lSymbTotal;
        
        // Compute effective Boolean expression if necessary:
        
        if (level > 0) {
          lExpr = GenSymb( cg, lFlatten->ifSymbParent);
          if (flevel == -1) {
            // Top level if's don't create a negated value:
            lExpr = GenBoolNot( cg, lExpr);
          }
          rExpr = GenSymb( cg, lSymbLocal);
          rExpr = GenBoolAnd( cg, lExpr, rExpr);
          lExpr = GenSymb( cg, lFlatten->ifSymbTotal);
          lExpr = GenBoolAssign( cg, lExpr, rExpr);
          lStmt = (Stmt *) NewExprStmt( cg, &fStmt->loc, lExpr);
          AppendStatements( &lStatements, lStmt);
        }
        
        // Walk sub-statements and transform assignments into conditional assignments:
        
        lStmt = static_cast< IfStmt * >( fStmt )->thenstmt;
        static_cast< IfStmt * >( fStmt )->thenstmt = NULL;
        while (lStmt) {
          nStmt = lStmt->next;
          lStmt->next = NULL;
          lStmt = FlattenIfStatementsStmt( cg, lStmt, arg1, level + 1);
          AppendStatements( &lStatements, lStmt);
          lStmt = nStmt;
        }
        if (static_cast< IfStmt * >( fStmt )->elsestmt) {
          
          // Compute effective Boolean expression if necessary:
          
          if (level > 0) {
            lExpr = GenSymb( cg, lFlatten->ifSymbParent);
            if (flevel == -1)
              lExpr = GenBoolNot( cg, lExpr);
            rExpr = GenSymb( cg, lSymbLocal);
            rExpr = GenBoolNot( cg, rExpr);
            rExpr = GenBoolAnd( cg, lExpr, rExpr);
            lExpr = GenSymb( cg, lFlatten->ifSymbTotal);
            lExpr = GenBoolAssign( cg, lExpr, rExpr);
            lStmt = (Stmt *) NewExprStmt( cg, &fStmt->loc, lExpr);
            AppendStatements( &lStatements, lStmt);
          }
          lStmt = static_cast< IfStmt * >( fStmt )->elsestmt;
          static_cast< IfStmt * >( fStmt )->elsestmt = NULL;
          while (lStmt) {
            nStmt = lStmt->next;
            lStmt->next = NULL;
            lStmt = FlattenIfStatementsStmt( cg, lStmt, arg1, -(level + 1));
            AppendStatements( &lStatements, lStmt);
            lStmt = nStmt;
          }
        }
        lFlatten->ifSymbTotal = ifSymbTotalSave;
        lFlatten->ifSymbLocal = lFlatten->ifSymbParent;
        lFlatten->ifSymbParent = ifSymbParentSave;
        rStmt = lStatements.first;
        break;
      case EXPR_STMT:
        if (level > 0) {
          eExpr = static_cast< ExprStmt * >( fStmt )->expr;
          if (IsAssignSVOp(eExpr)) {
            lExpr = static_cast< Binary * >( eExpr )->left;
            rExpr = static_cast< Binary * >( eExpr )->right;
            lop = static_cast< Binary * >( eExpr )->op;
            lsubop = static_cast< Binary * >( eExpr )->subop;
            lType = static_cast< Binary * >( eExpr )->type;
            ifvar = GenSymb( cg, lFlatten->ifSymbTotal);
            if (flevel == -1)
              ifvar = GenBoolNot( cg, ifvar);
            if (lop == ASSIGN_MASKED_KV_OP) {
              mask = SUBOP_GET_MASK(lsubop);
              len = SUBOP_GET_S(lsubop);
              base = SUBOP_GET_T(lsubop);
              // Create vector of $if/FALSE values:
              mExpr = NULL;
              for (ii = 0; ii < len; ii++) {
                if (mask & 1) {
                  tExpr = GenSymb( cg, lFlatten->ifSymbTotal);
                } else {
                  tExpr = GenBoolConst( cg, 0);
                }
                mExpr = GenExprList( cg, mExpr, tExpr, cg->BooleanType);
                mask >>= 1;
              }
              ifvar = (Expr *) NewUnopSubNode( cg, VECTOR_V_OP, SUBOP_V(len, TB_Boolean), mExpr);
              static_cast< Unary * >( ifvar )->type = GetStandardType( cg, TB_Boolean, len, 0);
              nop = ASSIGN_COND_V_OP;
              nsubop = SUBOP_V(len, base);
            } else {
              // Normal assign.  Convert it to simple conditional assignment:
              switch (lop) {
                case ASSIGN_OP:
                  nop = ASSIGN_COND_OP;
                  nsubop = lsubop;
                  break;
                case ASSIGN_V_OP:
                  nop = ASSIGN_COND_SV_OP;
                  nsubop = lsubop;
                  break;
                case ASSIGN_GEN_OP:
                  nop = ASSIGN_COND_GEN_OP;
                  nsubop = lsubop;
                  break;
                default:
                  assert(0);
                  break;
              }
            }
            nExpr = (Expr *) NewTriopSubNode(  cg, (opcode)nop, nsubop, lExpr, ifvar, rExpr);
            static_cast< Trinary * >( nExpr )->type = lType;
            static_cast< ExprStmt * >( fStmt )->expr = nExpr;
            rStmt = fStmt;
          } else if (IsAssignCondSVOp(eExpr)) {
            switch (static_cast< Trinary * >( eExpr )->op) {
              case ASSIGN_COND_OP:
              case ASSIGN_COND_SV_OP:
              case ASSIGN_COND_GEN_OP:
                ifvar = GenSymb( cg, lFlatten->ifSymbTotal);
                if (flevel == -1)
                  ifvar = GenBoolNot( cg, ifvar);
                static_cast< Trinary * >( eExpr )->arg2 = GenBoolAnd( cg, static_cast< Trinary * >( eExpr )->arg2, ifvar);
                break;
              case ASSIGN_COND_V_OP:
                lsubop = static_cast< Trinary * >( eExpr )->subop;
                len = SUBOP_GET_S(lsubop);
                // Create vector of $if values:
                mExpr = NULL;
                for (ii = 0; ii < len; ii++) {
                  tExpr = GenSymb( cg, lFlatten->ifSymbTotal);
                  if (flevel == -1)
                    tExpr = GenBoolNot( cg, tExpr);
                  mExpr = GenExprList( cg, mExpr, tExpr, cg->BooleanType);
                }
                ifvar = (Expr *) NewUnopSubNode( cg, VECTOR_V_OP, SUBOP_V(len, TB_Boolean), mExpr);
                static_cast< Unary * >( ifvar )->type = GetStandardType( cg, TB_Boolean, len, 0);
                static_cast< Trinary * >( eExpr )->arg2 = GenBoolAndVec( cg, static_cast< Trinary * >( eExpr )->arg2, ifvar, len);
                break;
              default:
                assert(0);
                break;
            }
            rStmt = fStmt;
          } else {
            rStmt = fStmt;
          }
        } else {
          rStmt = fStmt;
        }
        break;
      case BLOCK_STMT:
        if (level > 0) {
          lStatements.first = NULL;
          lStatements.last = NULL;
          lStmt = static_cast< BlockStmt * >( fStmt )->body;
          while (lStmt) {
            nStmt = lStmt->next;
            lStmt->next = NULL;
            lStmt = FlattenIfStatementsStmt( cg, lStmt, arg1, flevel);
            AppendStatements( &lStatements, lStmt);
            lStmt = nStmt;
          }
          rStmt = lStatements.first;
        } else {
          rStmt = fStmt;
        }
        break;
      case DISCARD_STMT:
        if (level > 0) {
          ifvar = GenSymb( cg, lFlatten->ifSymbTotal);
          if (flevel == -1)
            ifvar = GenBoolNot( cg, ifvar);
          if (static_cast< Unary * >( static_cast< DiscardStmt * >( fStmt )->cond )->arg) {
            lType = static_cast< DiscardStmt * >( fStmt )->cond->type;
            if (IsVector(lType, &len)) {
              ifvar = GenBoolSmear( cg, ifvar, len);
              ifvar = GenBoolAndVec( cg, ifvar, static_cast< Unary * >( static_cast< DiscardStmt * >( fStmt )->cond )->arg, len);
            } else {
              ifvar = GenBoolAnd( cg, ifvar, static_cast< Unary * >( static_cast< DiscardStmt * >( fStmt )->cond )->arg);
            }
          }
          static_cast< Unary * >( static_cast< DiscardStmt * >( fStmt )->cond )->arg = ifvar;
        }
        rStmt = fStmt;
        break;
      default:
        rStmt = fStmt;
        break;
    }
  } else {
    rStmt = fStmt;
  }
  return rStmt;
} // FlattenIfStatementsStmt

/*
 * FlattenIfStatements() - Convert if statements into conditional assignments.
 *
 */

static Stmt *FlattenIfStatements( CgContext *cg, Scope *fScope, Stmt *fStmt)
{
  FlattenIf lFlatten;
  Stmt *lStmt;
  
  lFlatten.funScope = fScope;
  lFlatten.ifSymbTotal = NULL;
  lFlatten.ifSymbParent = NULL;
  lFlatten.ifSymbLocal = NULL;
  lStmt = PreApplyToStatements( cg, FlattenIfStatementsStmt, fStmt, &lFlatten, 0);
  return lStmt;
} // FlattenIfStatements

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Output Support Functions //////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * lOutputVendorVersion() - Output vendor and profile version.
 *
 */

static void lOutputVendorVersion( CgContext *cg, Hal *fHal)
{
  
  // 1. Profile:
  
  cg->OutputPrintf( "%svendor %s\n", fHal->comment, fHal->vendor);
  
  // 2. Program:
  
  cg->OutputPrintf( "%sversion %s\n", fHal->comment, fHal->version);
  
} // lOutputVendorVersion

static void lPrintUniformVariableDescription(CgContext *cg, const char *symbolName, Type *fType,
                                             Binding *fBind, int bindOffset,
                                             const char *semanticName, int paramNo)
{
  char str1[100], str2[100], newSymbolName[1024], newSemanticName[1024];
  int category, len, len2, ii;
  Symbol *lSymb;
  
  category = GetCategory(fType);
  switch (category) {
    case TC_Scalar:
    case TC_Array:
      if (IsScalar(fType) || IsVector(fType, &len) || IsMatrix(fType, &len, &len2)) {
        FormatTypeStringRT( cg, str1, sizeof(str1), str2, sizeof(str1), fType, 1);
        cg->OutputPrintf( "%svar %s ", cg->theHal->comment, str1 );
        cg->OutputPrintf( "%s%s : %s", symbolName, str2, semanticName );
        if (fBind) {
          switch (fBind->kind) {
            case BK_REGARRAY:
              cg->OutputPrintf( " : %s[%d]", cg->GetString(fBind->name), fBind->num + bindOffset );
              ii = GetQuadRegSize(fType);
              if (ii > 1)
                cg->OutputPrintf( ", %d", ii );
              break;
            case BK_TEXUNIT:
              cg->OutputPrintf( " : texunit %d", fBind->num );
              break;
            case BK_NONE:
              cg->OutputPrintf( " : " );
              break;
            default:
              cg->OutputPrintf( " : %s", "?????" );
              break;
          }
        } else {
          // No binding
          cg->OutputPrintf( " : ");
        }
        cg->OutputPrintf( " : %d : %d", paramNo, 1 );
        cg->OutputPrintf( "\n" );
      } else {
        for (ii = 0; ii < static_cast< TypeArray * >( fType )->numels; ii++) {
          sprintf(newSymbolName, "%s[%d]", symbolName, ii);
          if (semanticName && semanticName[0] != '\0') {
            sprintf(newSemanticName, "%s[%d]", semanticName, ii);
          } else {
            newSemanticName[0] = '\0';
          }
          lPrintUniformVariableDescription( cg, newSymbolName, static_cast< TypeArray * >( fType )->eltype, fBind,
                                           bindOffset, newSemanticName, paramNo);
          bindOffset += GetQuadRegSize(static_cast< TypeArray * >( fType )->eltype);
        }
      }
      break;
    case TC_Struct:
      lSymb = static_cast< TypeStruct * >( fType )->members->symbols;
      while (lSymb) {
        sprintf(newSymbolName, "%s.%s", symbolName, cg->GetString(lSymb->name));
        if (semanticName && semanticName[0] != '\0') {
          sprintf(newSemanticName, "%s.%s", semanticName, cg->GetString(lSymb->name));
        } else {
          newSemanticName[0] = '\0';
        }
        lPrintUniformVariableDescription( cg, newSymbolName, lSymb->type, fBind,
                                         bindOffset + (lSymb->details.var.addr >> 2), newSemanticName, paramNo);
        lSymb = lSymb->next;
      }
      break;
    default:
      cg->OutputPrintf( "%s *** unknown category 0x%08x ***\n", cg->theHal->comment, category );
      break;
  }
} // lPrintUniformVariableDescription

static void lPrintVaryingVariableDescription(CgContext *cg, const char *symbolName,
                                             Symbol *fSymb, const char *scopeName, int paramNo)
{
  char str1[100], str2[100], newSymbolName[1024];
  int category, len, len2;
	Atom mname, semantics;
  const char *bindingRegName;
  Binding *lBind;
  Symbol *lSymb;
  Type *lType;
  
  lType = fSymb->type;
  if (GetCategory(lType) == TC_Function) {
    lType = static_cast< TypeFunction * >( lType )->rettype;
    semantics = 0;
    lBind = NULL;
  } else {
    semantics = fSymb->details.var.semantics;
    lBind = fSymb->details.var.bind;
  }
  category = GetCategory(lType);
  switch (category) {
    case TC_Scalar:
    case TC_Array:
      if (IsScalar(lType) || IsVector(lType, &len) || IsMatrix(lType, &len, &len2)) {
        FormatTypeStringRT( cg, str1, sizeof(str1), str2, sizeof(str1), lType, 1);
        if (lBind) {
          if (lBind->properties & BIND_HIDDEN)
            return;
          mname = semantics.IsValid() ? semantics : lBind->name;
          switch (lBind->kind) {
            case BK_CONNECTOR:
              bindingRegName = cg->GetString(lBind->name);
              break;
            default:
              bindingRegName = "?????";
              break;
          }
        } else {
          // No binding
          bindingRegName = "";
        }
        cg->OutputPrintf( "%svar %s %s%s", cg->theHal->comment, str1, symbolName, str2 );
        cg->OutputPrintf( " : %s_%s", scopeName, cg->GetString(mname) );
        cg->OutputPrintf( " : %s", bindingRegName );
        cg->OutputPrintf( " : %d", paramNo );
        cg->OutputPrintf( " : %d", 1 );
        cg->OutputPrintf( "\n" );
      } else {
        // Can't have a varying array!  No way to specify bindings!
        cg->OutputPrintf( "%s *** Can't have varying nonpacked array ***\n", cg->theHal->comment );
      }
      break;
    case TC_Struct:
      lSymb = static_cast< TypeStruct * >( lType )->members->symbols;
      while (lSymb) {
        if (*symbolName != '\0') {
          sprintf(newSymbolName, "%s.%s", symbolName, cg->GetString(lSymb->name));
        } else {
          strcpy(newSymbolName, cg->GetString(lSymb->name));
        }
        lPrintVaryingVariableDescription( cg, newSymbolName, lSymb, scopeName, paramNo);
        lSymb = lSymb->next;
      }
      break;
    default:
      cg->OutputPrintf( "%s *** unknown category 0x%08x ***\n", cg->theHal->comment, category);
      break;
  }
} // lPrintVaryingVariableDescription

/*
 * lOutputParameterTypes()
 *
 */

static void lOutputParameterTypes(CgContext *cg, Symbol *program)
{
  Symbol *lSymb;
  SymbolList *lGlobals;
  UniformSemantic *lUniform;
  const char *semanticName, *symbolName, *varyingScopeName;
  int paramNo;
  Type *lType, *retType;
  
  // 1. Profile:
  
  cg->OutputPrintf( "%sprofile %s\n", cg->theHal->comment, cg->GetString(cg->theHal->profileName));
  
  // 2. Program:
  
  cg->OutputPrintf( "%sprogram %s\n", cg->theHal->comment, cg->GetString(program->name));
  
  
  // 3. Uniform semantic table:
  
  lUniform = cg->theHal->uniforms;
  while (lUniform) {
    cg->OutputPrintf( "%ssemantic ", cg->theHal->comment);
    if (lUniform->gname.IsValid() )
      cg->OutputPrintf( "%s.", cg->GetString(lUniform->gname));
    cg->OutputPrintf( "%s", cg->GetString(lUniform->vname));
    if (lUniform->semantic.IsValid() )
      cg->OutputPrintf( " : %s", cg->GetString(lUniform->semantic));
    cg->OutputPrintf( "\n");
    lUniform = lUniform->next;
  }
  
  // 4. Global uniform variables:
  
  lGlobals = cg->theHal->uniformGlobal;
  while (lGlobals) {
    lSymb = lGlobals->symb;
    if (lSymb) {
      if (lSymb->details.var.semantics.IsValid()) {
        semanticName = cg->GetString(lSymb->details.var.semantics);
      } else {
        semanticName = "";
      }
      symbolName = cg->GetString(lSymb->name);
      lPrintUniformVariableDescription(cg, symbolName, lSymb->type,
                                       lSymb->details.var.bind, 0, semanticName, -1);
    }
    lGlobals = lGlobals->next;
  }
  
  // 5. Parameters:
  
  paramNo = 0;
  lSymb = program->details.fun.params;
  while (lSymb) {
    symbolName = cg->GetString(lSymb->name);
    if (GetDomain(lSymb->type) == TD_Uniform) {
      if (lSymb->details.var.semantics.IsValid()) {
        semanticName = cg->GetString(lSymb->details.var.semantics);
      } else {
        semanticName = "";
      }
      lPrintUniformVariableDescription(cg, symbolName, lSymb->type,
                                       lSymb->details.var.bind, 0, semanticName, paramNo);
    } else {
      if (GetQualifiers(lSymb->type) == TQ_Out) {
        varyingScopeName = "sx";
      } else {
        varyingScopeName = "sx";
      }
      lPrintVaryingVariableDescription(cg, symbolName, lSymb, varyingScopeName, paramNo);
    }
    paramNo++;
    lSymb = lSymb->next;
  }
  
  // 6. Function return value:
  
  lType = program->type;
  retType = static_cast< TypeFunction * >( lType )->rettype;
  if (GetCategory(retType) == TC_Struct) {
    lPrintVaryingVariableDescription(cg, "", program, "sx", -1);
  }
  
  cg->OutputPrintf( "\n" );
} // lOutputParameterTypes

/*
 * lOutputConstantBindings() - Output the Constant bindings in the Hal.
 *
 */

static void lOutputConstantBindings( CgContext *cg, Hal *fHal)
{
  BindingList *lBindList;
  Binding *lBind;
  int ii;
  
  lBindList = fHal->constantBindings;
  while (lBindList) {
    lBind = lBindList->binding;
    if (lBind->kind == BK_CONSTANT) {
      cg->OutputPrintf( "%sconst %s[%d] =", fHal->comment,
                       cg->GetString(lBind->name), lBind->num);
      for (ii = 0; ii < lBind->size; ii++)
        cg->OutputPrintf( " %.7g", lBind->val[ii]);
      cg->OutputPrintf( "\n");
    }
    lBindList = lBindList->next;
  }
} // lOutputConstantBindings

/*
 * lOutputDefaultBindings() - Output the Constant bindings in the Hal.
 *
 */

static void lOutputDefaultBindings( CgContext *cg, Hal *fHal)
{
  BindingList *lBindList;
  Binding *lBind;
  int ii;
  
  lBindList = fHal->defaultBindings;
  while (lBindList) {
    lBind = lBindList->binding;
    if (lBind->kind == BK_DEFAULT) {
      cg->OutputPrintf( "%sdefault ", fHal->comment);
      if ( lBind->gname.IsValid() )
        cg->OutputPrintf( " %s.", cg->GetString(lBind->gname));
      cg->OutputPrintf( "%s =", cg->GetString(lBind->lname));
      for (ii = 0; ii < lBind->size; ii++)
        cg->OutputPrintf( " %.7g", lBind->val[ii]);
      cg->OutputPrintf( "\n");
    }
    lBindList = lBindList->next;
  }
} // lOutputDefaultBindings

void CgContext::OutputBindings( Hal *fHal, Symbol *program )
{
  lOutputVendorVersion( this, fHal);
  lOutputParameterTypes( this, program);
  lOutputConstantBindings( this, fHal);
  lOutputDefaultBindings( this, fHal);
} // OutputBindings

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////// Main Compile Control ////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * CompileProgram() - The final call from the parser.  If there weren't any errors, perform
 *         optimizations, register allocation, code generation, etc.
 *
 * Returns: Number of errors.
 *
 */

int CgContext::CompileProgram(SourceLoc *loc, Scope *fScope)
{
  Symbol *program;
  Scope *lScope;
  Stmt *lStmt;
  
  if (GetErrorCount( this ) == 0) {
    if (fScope->programs) {
      theHal->globalScope = fScope;
      program = fScope->programs->symb;
      BuildSemanticStructs( this, loc, fScope, program);
      CheckFunctionDefinitions( this, loc, fScope, program);
      if (GetErrorCount( this ) != 0)
        goto done;
      
      // Convert to Basic Blocks Format goes here...
      
      // Inline appropriate function calls...
      
      lScope = program->details.fun.locals;
      lStmt = program->details.fun.statements;
      lStmt = ConcatStmts(fScope->initStmts, lStmt);
      if (GetErrorCount( this ) == 0) {
        lStmt = ExpandInlineFunctionCalls( this, lScope, lStmt, NULL);
        PostApplyToExpressions( this, CheckForHiddenVaryingReferences, lStmt, NULL, 0);
        if (options.dumpParseTree || options.dumpNodeTree) {
          program->details.fun.statements = lStmt;
          Printf("=======================================================================\n");
          Printf("After inlining functions:\n");
          Printf("=======================================================================\n");
					Writer wr( this, std::cout );
					wr.WriteSymbolTree( program->details.fun.locals->symbols );
					if (options.dumpParseTree) {
						wr.WriteFunction( program );
					}
					if (options.dumpNodeTree) {
						BWriter bwr( this, std::cout );
						bwr.WriteFunction( program );
					}
          Printf("=======================================================================\n");
        }
        
        CheckConnectorUsageMain( this, program, lStmt);
        lStmt = ConvertDebugCalls( this, loc, lScope, lStmt, options.debugMode);
        PostApplyToExpressions( this, ExpandIncDecExpr, lStmt, NULL, 0);
        PostApplyToExpressions( this, ExpandCompoundAssignmentExpr, lStmt, NULL, 0);
        lStmt = PostApplyToStatements( this, FlattenCommasStmt, lStmt, NULL, 0);
        lStmt = PostApplyToStatements( this, RemoveEmptyStatementsStmt, lStmt, NULL, 0);
        lStmt = PostApplyToStatements( this, FlattenChainedAssignmentsStmt, lStmt, NULL, 0);
        PostApplyToExpressions( this, ConvertNamedConstantsExpr, lStmt, NULL, 0);
        if (theHal->GetCapsBit(CAPS_DECONSTRUCT_MATRICES))
          lStmt = DeconstructMatrices(this, lScope, lStmt);
        lStmt = FlattenStructAssignments( this, lScope, lStmt);
        if (!theHal->GetCapsBit(CAPS_DONT_FLATTEN_IF_STATEMENTS))
          lStmt = FlattenIfStatements( this, lScope, lStmt);
        
        // Optimizations:
        
        PostApplyToExpressions( this, ConstantFoldNode, lStmt, NULL, 0);
        
        // Lots more optimization stuff goes here...
        
        if (options.dumpFinalTree) {
          program->details.fun.statements = lStmt;
          Printf("=======================================================================\n");
          Printf("Final program:\n");
          Printf("=======================================================================\n");
					Writer wr( this, std::cout );
          wr.WriteSymbolTree( program->details.fun.locals->symbols );
          wr.WriteFunction( program );
					if (options.dumpNodeTree) {
						BWriter bwr( this, std::cout );
						bwr.WriteFunction( program );
					}
          Printf("=======================================================================\n");
        }
      }
      
      program->details.fun.statements = lStmt;
      if (!theHal->GetCapsBit(CAPS_LATE_BINDINGS))
        OutputBindings( theHal, program );
      if (GetErrorCount( this ) == 0) {
        if (!options.noCodeGen)
          theHal->GenerateCode(loc, fScope, program);
      }
      
      if (options.errorMode)
        CheckAllErrorsGenerated( this );
      
    } else {
      SemanticError( this, loc, ERROR___NO_PROGRAM);
    }
  }
done:
  return GetErrorCount( this );
} // CompileProgram

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////// End of compile.c //////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
