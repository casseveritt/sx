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
// support.c
//

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>

#include "slglobals.h"

using namespace std;

#undef PICK
#define PICK(a, b, c, d, e) d

const NodeKind NodeKind[] = {
  OPCODE_TABLE
};

#undef PICK

#define PICK(a, b, c, d, e) Atom( c, b )

Atom const opcode_atom[] = {
  OPCODE_TABLE
};

#undef PICK


#if SX_STRINGIFY_TO_DEBUG
void Expr::StringifyToDebugString( CgContext *cg ) const {
	if ( this == NULL ) {
		return;
	}
	if ( kind == UNARY_N ) {
		const Unary * u = static_cast< const Unary * > ( this );
		const string *sa = u->arg ? u->arg->Stringify() : NULL;
		string op = cg->GetString( opcode_atom[ u->op ] );
		debugString = op + string("( ") + (sa ? *sa : string("NULL")) + " )";
	} else if ( kind == BINARY_N ) {
		const Binary * b = static_cast< const Binary * > ( this );
		const string *sl = b->left ? b->left->Stringify() : NULL;
		const string *sr = b->right ? b->right->Stringify() : NULL;
		string op = cg->GetString( opcode_atom[ b->op ] );
		debugString = op + string("( ") + (sl ? *sl : string("NULL")) + string(", ") + (sr ? *sr : string("NULL")) + " )";
	} else if ( kind == TRINARY_N ) {
		const Trinary * t = static_cast< const Trinary * > ( this );
		const string *sa1 = t->arg1 ? t->arg1->Stringify() : NULL;
		const string *sa2 = t->arg2 ? t->arg2->Stringify() : NULL;
		const string *sa3 = t->arg3 ? t->arg3->Stringify() : NULL;
		string op = cg->GetString( opcode_atom[ t->op ] );
		debugString = op + string("( ") +
		(sa1 ? *sa1 : string("NULL")) + string(", ") +
		(sa2 ? *sa2 : string("NULL")) + string(", ") +
		(sa3 ? *sa3 : string("NULL")) + string(", ") +
		" )";
	} else if ( kind == SYMB_N ) {
		const Symb * s = static_cast< const Symb * > ( this );
		string name = cg->GetString( s->symbol->name );
		string op = cg->GetString( opcode_atom[ s->op ] );
		debugString = op + string("( ") + name + " )";
	} else {
		debugString = "Expr::Stringify(), bitches.";
	}
}
#endif

set< Decl * > liveDecls;

void PrintLiveDecls( CgContext *cg ) {
	bool once = false;
	for ( set< Decl * >::iterator it = liveDecls.begin(); it != liveDecls.end(); ++it ) {
		Decl *d = *it;
		if ( ! once ) {
			cg->Printf( "Live Decls:\n" );
			once = true;
		}
		cg->Printf( "  Decl %s from %s:%d\n", d->name.s, d->loc.file.s, d->loc.line );
	}
}

Decl::Decl() : next( NULL ), symb( NULL ), params( NULL ), initexpr( NULL ) {
	kind = DECL_N;
	liveDecls.insert( this );
}

Decl::~Decl() {
	liveDecls.erase( this );
	if ( next ) {
		delete next;
	}
	if ( params ) {
		delete params;
	}
}


/*
 * NewDeclNode() - Create a new declaration node.
 *
 */

Decl *NewDeclNode( CgContext *cg, SourceLoc *loc, Atom atom, DeclType *type)
{
  Decl *pdecl;
  
  pdecl = new Decl();
  pdecl->kind = DECL_N;
  pdecl->loc = *loc;
  pdecl->name = atom;
  pdecl->semantics = 0;
  pdecl->type = *type;
  pdecl->next = NULL;
  pdecl->symb = NULL;
  pdecl->params = NULL;
  pdecl->initexpr = NULL;
  return pdecl;
} // NewDeclNode

/*
 * NewSymbNode() - Create a new symbol node.
 *
 */

Symb *NewSymbNode( CgContext *cg, opcode op, Symbol *fSymb)
{
  Symb *psymb;
  
  assert(NodeKind[op] == SYMB_N);
  psymb = new Symb();
  psymb->kind = SYMB_N;
  psymb->type = fSymb->type;
  psymb->IsLValue = 1;
	psymb->IsConst = psymb->type->isConst;
  psymb->HasSideEffects = 0;
  psymb->op = op;
  psymb->symbol = fSymb;
  return psymb;
} // NewSymbNode

/*
 * NewIConstNode() - Create a new integer Constant node.
 *
 */

Constant *NewIConstNode( CgContext *cg, opcode op, int fval, TypeBase base)
{
  Constant *pconst;
  
  assert(NodeKind[op] == CONST_N);
  pconst = new Constant();
  pconst->kind = CONST_N;
  pconst->type = GetStandardType( cg, base, 0, 0);
  pconst->IsLValue = 0;
  pconst->IsConst = 0;
  pconst->HasSideEffects = 0;
  pconst->op = op;
  pconst->subop = SUBOP__(base);
  pconst->val[0].i = fval;
  pconst->tempptr[0] = 0;
  return pconst;
} // NewIConstNode

/*
 * NewBConstNode() - Create a new Boolean Constant node.
 *
 */

Constant *NewBConstNode( CgContext *cg, opcode op, int fval, TypeBase base)
{
  Constant *pconst;
  
  assert(NodeKind[op] == CONST_N);
  pconst = new Constant();
  pconst->kind = CONST_N;
  pconst->type = GetStandardType( cg, base, 0, 0);
  pconst->IsLValue = 0;
  pconst->IsConst = 0;
  pconst->HasSideEffects = 0;
  pconst->op = op;
  pconst->subop = SUBOP__(base);
  pconst->val[0].i = fval;
  pconst->tempptr[0] = 0;
  return pconst;
} // NewBConstNode

/*
 * NewFConstNode() - Create a new floating point Constant node.
 *
 */

Constant *NewFConstNode( CgContext *cg, opcode op, float fval, TypeBase base)
{
  Constant *pconst;
  
  assert(NodeKind[op] == CONST_N);
  pconst = new Constant();
  pconst->kind = CONST_N;
  pconst->type = GetStandardType( cg, base, 0, 0);
  pconst->IsLValue = 0;
  pconst->IsConst = 0;
  pconst->HasSideEffects = 0;
  pconst->op = op;
  pconst->subop = SUBOP__(base);
  pconst->val[0].f = fval;
  pconst->tempptr[0] = 0;
  return pconst;
} // NewFConstNode

/*
 * NewFConstNodeV() - Create a new floating point Constant vector node.
 *
 */

Constant *NewFConstNodeV( CgContext *cg, opcode op, float *fval, int len, TypeBase base)
{
  Constant *pconst;
  int ii;
  
  assert(NodeKind[op] == CONST_N);
  pconst = new Constant();
  pconst->kind = CONST_N;
  pconst->type = GetStandardType( cg, base, len, 0);
  pconst->IsLValue = 0;
  pconst->IsConst = 0;
  pconst->HasSideEffects = 0;
  pconst->op = op;
  pconst->subop = SUBOP_V(len, base);
  for (ii = 0; ii < len; ii++)
    pconst->val[ii].f = fval[ii];
  pconst->tempptr[0] = 0;
  return pconst;
} // NewFConstNodeV

/*
 * NewUnopNode() - Create a Unary op node.
 *
 */

Unary *NewUnopNode( CgContext *cg, opcode op, Expr *arg)
{
  Unary *pun;
  
  assert(NodeKind[op] == UNARY_N);
  pun = new Unary();
  pun->kind = UNARY_N;
  pun->type = cg->UndefinedType;
  pun->IsLValue = 0;
  pun->IsConst = 0;
  pun->HasSideEffects = 0;
  if (arg)
    pun->HasSideEffects = arg->HasSideEffects;
  pun->op = op;
  pun->subop = 0;
  pun->arg = arg;
  pun->tempptr[0] = 0;
  return pun;
} // NewUnopNode

/*
 * NewUnopSubNode() - Create a Unary op node.
 *
 */

Unary *NewUnopSubNode( CgContext *cg, opcode op, int subop, Expr *arg)
{
  Unary *pun;
  
  assert(NodeKind[op] == UNARY_N);
  pun = new Unary();
  pun->kind = UNARY_N;
  pun->type = cg->UndefinedType;
  pun->IsLValue = 0;
  pun->IsConst = 0;
  pun->HasSideEffects = 0;
  if (arg)
    pun->HasSideEffects = arg->HasSideEffects;
  pun->op = op;
  pun->subop = subop;
  pun->arg = arg;
  pun->tempptr[0] = 0;
  return pun;
} // NewUnopSubNode

/*
 * NewBinopNode() - Create a Binary op node.
 *
 */

Binary *NewBinopNode( CgContext *cg, opcode op, Expr *left, Expr *right)
{
  Binary *pbin;
  
  assert(NodeKind[op] == BINARY_N);
  pbin = new Binary();
  pbin->kind = BINARY_N;
  pbin->type = cg->UndefinedType;
  pbin->IsLValue = 0;
  pbin->IsConst = 0;
  pbin->HasSideEffects = 0;
  if (left)
    pbin->HasSideEffects = left->HasSideEffects;
  if (right)
    pbin->HasSideEffects |= right->HasSideEffects;
  pbin->op = op;
  pbin->subop = 0;
  pbin->left = left;
  pbin->right = right;
  pbin->tempptr[0] = 0;
  pbin->tempptr[1] = 0;
  return pbin;
} // NewBinopNode

/*
 * NewBinopSubNode() - Create a Binary op node.
 *
 */

Binary *NewBinopSubNode( CgContext *cg, opcode op, int subop, Expr *left, Expr *right)
{
  Binary *pbin;
  
  assert(NodeKind[op] == BINARY_N);
  pbin = new Binary();
  pbin->kind = BINARY_N;
  pbin->type = cg->UndefinedType;
  pbin->IsLValue = 0;
  pbin->IsConst = 0;
  pbin->HasSideEffects = 0;
  if (left)
    pbin->HasSideEffects = left->HasSideEffects;
  if (right)
    pbin->HasSideEffects |= right->HasSideEffects;
  pbin->op = op;
  pbin->subop = subop;
  pbin->left = left;
  pbin->right = right;
  pbin->tempptr[0] = 0;
  pbin->tempptr[1] = 0;
  return pbin;
} // NewBinopSubNode

/*
 * NewTriopNode() - Create a Trinary op node.
 *
 */

Trinary *NewTriopNode( CgContext *cg, opcode op, Expr *arg1, Expr *arg2, Expr *arg3)
{
  Trinary *ptri;
  
  assert(NodeKind[op] == TRINARY_N);
  ptri = new Trinary();
  ptri->kind = TRINARY_N;
  ptri->type = cg->UndefinedType;
  ptri->IsLValue = 0;
  ptri->IsConst = 0;
  ptri->HasSideEffects = 0;
  if (arg1)
    ptri->HasSideEffects = arg1->HasSideEffects;
  if (arg2)
    ptri->HasSideEffects |= arg2->HasSideEffects;
  if (arg3)
    ptri->HasSideEffects |= arg3->HasSideEffects;
  ptri->op = op;
  ptri->subop = 0;
  ptri->arg1 = arg1;
  ptri->arg2 = arg2;
  ptri->arg3 = arg3;
  ptri->tempptr[0] = 0;
  ptri->tempptr[1] = 0;
  ptri->tempptr[2] = 0;
  return ptri;
} // NewTriopNode

/*
 * NewTriopSubNode() - Create a Trinary op node.
 *
 */

Trinary *NewTriopSubNode( CgContext *cg, opcode op, int subop, Expr *arg1, Expr *arg2, Expr *arg3)
{
  Trinary *ptri;
  
  assert(NodeKind[op] == TRINARY_N);
  ptri = new Trinary();
  ptri->kind = TRINARY_N;
  ptri->type = cg->UndefinedType;
  ptri->IsLValue = 0;
  ptri->IsConst = 0;
  ptri->HasSideEffects = 0;
  if (arg1)
    ptri->HasSideEffects = arg1->HasSideEffects;
  if (arg2)
    ptri->HasSideEffects |= arg2->HasSideEffects;
  if (arg3)
    ptri->HasSideEffects |= arg3->HasSideEffects;
  ptri->op = op;
  ptri->subop = subop;
  ptri->arg1 = arg1;
  ptri->arg2 = arg2;
  ptri->arg3 = arg3;
  ptri->tempptr[0] = 0;
  ptri->tempptr[1] = 0;
  ptri->tempptr[2] = 0;
  return ptri;
} // NewTriopSubNode

/*
 * DupSymbNode() - Duplicate a symb op node.
 *
 */

Symb *DupSymbNode(const Symb *fsymb)
{
  Symb *lsymb;
  
  lsymb = new Symb();
  *lsymb = *fsymb;
  return lsymb;
} // DupSymbNode

/*
 * DupConstNode() - Duplicate a Constant op node.
 *
 */

Constant *DupConstNode(const Constant *fconst)
{
  Constant *lconst;
  
  lconst = new Constant();
  *lconst = *fconst;
  return lconst;
} // DupConstNode

/*
 * DupUnaryNode() - Duplicate a Unary op node.
 *
 */

Unary *DupUnaryNode(const Unary *fun)
{
  Unary *lun;
  
  lun = new Unary();
  *lun = *fun;
  return lun;
} // DupUnaryNode

/*
 * DupBinaryNode() - Duplicate a Binary op node.
 *
 */

Binary *DupBinaryNode(const Binary *fbin)
{
  Binary *lbin;
  
  lbin = new Binary();
  *lbin = *fbin;
  return lbin;
} // DupBinaryNode

/*
 * DupTrinaryNode() - Duplicate a Trinary op node.
 *
 */

Trinary *DupTrinaryNode(const Trinary *ftri)
{
  Trinary *ltri;
  
  ltri = new Trinary();
  *ltri = *ftri;
  return ltri;
} // DupTrinaryNode


/*
 * DupNode() - Duplicate a expression node.
 *
 */

Expr *DupNode( CgContext *cg, const Expr *fExpr)
{
  switch (fExpr->kind) {
    case SYMB_N: return DupSymbNode(static_cast< const Symb * >( fExpr ));
    case CONST_N: return DupConstNode(static_cast< const Constant * >( fExpr ));
    case UNARY_N: return DupUnaryNode(static_cast< const Unary * >( fExpr ));
    case BINARY_N: return DupBinaryNode(static_cast< const Binary * >( fExpr ));
    case TRINARY_N: return DupTrinaryNode(static_cast< const Trinary * >( fExpr ));
  }
  
  FatalError( cg, "unsupported node type in DupNode");
  return NULL;
} // DupNode


/*
 * NewExprStmt() - Create an expression statement.
 *
 */

ExprStmt *NewExprStmt(  CgContext *cg, SourceLoc *loc, Expr *fExpr)
{
  ExprStmt *lStmt;
  
  lStmt = new ExprStmt( *loc );
  lStmt->expr = fExpr;
  return lStmt;
} // NewExprStmt

/*
 * NewIfStmt() - Create an expression statement.
 *
 */

IfStmt *NewIfStmt( CgContext *cg, SourceLoc *loc, Expr *fExpr, Stmt *thenstmt, Stmt *elsestmt)
{
  IfStmt *lStmt;
  
  lStmt = new IfStmt( *loc );
  lStmt->cond = fExpr;
  lStmt->thenstmt = thenstmt;
  lStmt->elsestmt = elsestmt;
  return lStmt;
} // NewIfStmt

/*
 * NewIfStmt() - Create an expression statement.
 *
 */

IfStmt *SetThenElseStmts( CgContext *cg, SourceLoc *loc, Stmt *ifstmt, Stmt *thenstmt, Stmt *elsestmt)
{
  IfStmt *lStmt;
  
  lStmt = (IfStmt *) ifstmt;
  assert(lStmt->kind == IF_STMT);
  lStmt->thenstmt = thenstmt;
  lStmt->elsestmt = elsestmt;
  return lStmt;
} // NewIfStmt

/*
 * NewWhileStmt() - Create a while statement.
 *
 */

WhileStmt *NewWhileStmt( CgContext *cg, SourceLoc *loc, StmtKind kind, Expr *fExpr, Stmt *body)
{
  WhileStmt *lStmt;
  
  lStmt = new WhileStmt( kind, *loc );
  lStmt->cond = fExpr;
  lStmt->body = body;
  return lStmt;
} // NewWhileStmt

/*
 * NewForStmt() - Create a for statement.
 *
 */

ForStmt *NewForStmt( CgContext *cg, SourceLoc *loc, Stmt *fexpr1, Expr *fexpr2, Stmt *fexpr3, Stmt *body)
{
  ForStmt *lStmt;
  
  lStmt = new ForStmt( *loc );
  lStmt->init = fexpr1;
  lStmt->cond = fexpr2;
  lStmt->step = fexpr3;
  lStmt->body = body;
  return lStmt;
} // NewForStmt

/*
 * NewBlockStmt() - Create a block statement.
 *
 */

BlockStmt *NewBlockStmt( CgContext *cg, SourceLoc *loc, Stmt *fStmt)
{
  BlockStmt *lStmt;
  
  lStmt = new BlockStmt( *loc );
  lStmt->body = fStmt;
  return lStmt;
} // NewBlockStmt


ReturnStmt::ReturnStmt( const SourceLoc & loc ) : Stmt( RETURN_STMT, loc ), expr( NULL ) {
  
}

ReturnStmt::~ReturnStmt() {
	delete expr;
}


/*
 * NewReturnStmt() - Create an expression statement.
 *
 */

ReturnStmt *NewReturnStmt( CgContext *cg, SourceLoc *loc, Scope *fScope, Expr *fExpr)
{
  ReturnStmt *lStmt;
  Expr *lExpr;
  
  if (fScope) {
    while (fScope->level > 2)
      fScope = fScope->next;
    fScope->HasReturnStmt = 1;
    if (fScope->returnType) {
      if (fScope->returnType == cg->VoidType) {
        if (fExpr) {
          SemanticError( cg, loc, ERROR___VOID_FUN_RETURNS_VALUE);
        }
      } else if (fScope->returnType != cg->UndefinedType) {
        if (ConvertType( cg, fExpr, fScope->returnType, fExpr->type, &lExpr, 0, 0)) {
          fExpr = lExpr;
        } else {
          SemanticError( cg, loc, ERROR___RETURN_EXPR_INCOMPAT);
        }
      }
    }
  }
  lStmt = new ReturnStmt( *loc );
  lStmt->expr = fExpr;
  return lStmt;
} // NewReturnStmt

/*
 * NewDiscardStmt() - Create a discard statement.
 *
 */

DiscardStmt *NewDiscardStmt( CgContext *cg, SourceLoc *loc, Expr *fExpr)
{
  DiscardStmt *lStmt;
  int len;
  
  lStmt = new DiscardStmt( *loc );
  if (fExpr && IsVector(fExpr->type, &len)) {
    /* empty */ ;
  } else {
    len = 0;
  }
  fExpr = (Expr *) NewUnopSubNode( cg, KILL_OP, SUBOP_V(len, TB_Boolean), fExpr);
  lStmt->cond = fExpr;
  return lStmt;
} // NewDiscardStmt

/*
 * NewCommentStmt() - Create a comment statement.
 *
 */

CommentStmt *NewCommentStmt( CgContext *cg, SourceLoc *loc, const char *str)
{
  CommentStmt *lStmt;
  
  lStmt = new CommentStmt( *loc );
  lStmt->str = cg->GetAtom(str);
  return lStmt;
} // NewCommentStmt

/************************************* DeclType functions: *************************************/

/*
 * GetTypePointer() - Strange function that returns a pointer to the type defined by it's
 *         argument.  There are 2 cases:
 *
 * A) IsDerived is TRUE:  This type is a stack-frame resident copy of another type.
 *          It has been modified by a qualifier, etc., and does not have a copy in the heap.
 *          Copy the contents into a freshly malloc'ed type and return it's address.
 * B) IsDerived is FALSE: This type is the same as that pointed to by "base".  Return "base".
 */

Type *GetTypePointer( CgContext *cg, SourceLoc *loc, const DeclType *fDtype)
{
  Type *pType;
  
  if (fDtype) {
    if (fDtype->IsDerived) {
			if (cg->theHal->CheckDeclarators(loc, fDtype)) {
        ; /* empty statement */
			}
      pType = DupType( fDtype->GetType() );
      pType->properties &= ~(TYPE_MISC_TYPEDEF | TYPE_MISC_PACKED_KW);
      pType->size = cg->theHal->GetSizeof(pType);
    } else {
      pType = fDtype->basetype;
    }
  } else {
    pType = cg->UndefinedType;
  }
  return pType;
} // GetTypePointer

/*
 * SetDType() - Set the fields of a DeclType to match a type.
 *
 */

DeclType *SetDType(DeclType *fDtype, Type *fType)
{
  fDtype->basetype = fType;
  fDtype->IsDerived = 0;
  fDtype->numNewDims = 0;
  fDtype->storageClass = SC_Unknown;
  fDtype->SetType( fType ); // need to make sure I'm not leaking fType here...
  return fDtype;
} // SetDType


/*
 * SetTypeCategory() - Set the category of a type.  Issue an error if it's already set to a
 *         conflicting category.
 *
 * Returns: TRUE if O.K.
 *
 */

bool SetTypeCategory( CgContext *cg, SourceLoc *loc, Atom atom, DeclType *fType, TypeCategory category, int Force)
{
  TypeCategory lcategory;
  
  lcategory = GetCategory( fType->GetType() );
  if (Force || lcategory == TC_None) {
		Type *to = fType->GetType();
		Type *tn = NewType( cg,to->name, to->base, category, to->domain, to->qualifier, to->isConst, to->properties, to->size );
		fType->SetType( tn );
		delete tn;
    fType->IsDerived = 1;
  } else {
    if (lcategory != category) {
      SemanticError( cg, loc, ERROR_S_CONFLICTING_DECLARATION, cg->GetString(atom));
      return false;
    }
  }
  return true;
} // SetTypeCategory

/*
 * SetTypeQualifiers() - Set a type's qualifier bits.  Issue an error if any bit is already set.
 *
 * Returns: TRUE if O.K.
 *
 */

bool SetTypeQualifiers( CgContext *cg, SourceLoc *loc, DeclType *fType, TypeQualifier qualifier, bool isconst)
{
  TypeQualifier lqualifier;
	bool & isConst = fType->GetType()->isConst;
  
	assert( qualifier >= 0 && qualifier <= 10 );
  
  lqualifier = GetQualifiers( fType->GetType() );
  if ( ( isConst && isconst ) || lqualifier != TQ_None ) {
    SemanticWarning( cg, loc, WARNING___QUALIFIER_SPECIFIED_TWICE);
  }
	isConst = isconst;
  if ( qualifier != TQ_None ) {
    fType->GetType()->qualifier = qualifier;
    fType->IsDerived = 1;
    if ( qualifier == TQ_Out && isConst )
    {
      SemanticError( cg, loc, ERROR___CONST_OUT_INVALID);
    }
  }
  return true;
} // SetTypeCategory

/*
 * SetTypeDomain() - Set the domain of a type.  Issue an error if it's already set to a
 *         conflicting domain.
 *
 * Returns: TRUE if O.K.
 *
 */

bool SetTypeDomain( CgContext *cg, SourceLoc *loc, DeclType *fType, TypeDomain domain)
{
  TypeDomain ldomain;
  
  ldomain = GetDomain( fType->GetType() );
  if (ldomain == TD_Unknown) {
		fType->GetType()->domain = domain;
    fType->IsDerived = 1;
  } else {
    if (ldomain == domain) {
      SemanticWarning( cg, loc, WARNING___DOMAIN_SPECIFIED_TWICE);
    } else {
      SemanticError( cg, loc, ERROR___CONFLICTING_DOMAIN);
      return false;
    }
  }
  return true;
} // SetTypeDomain

/*
 * SetTypeMisc() - Set a bit in the misc field a type.  Issue an error if it's already set.
 *
 * Returns: TRUE if O.K.
 *
 */

int SetTypeMisc( CgContext *cg, SourceLoc *loc, DeclType *fType, int misc)
{
  if (fType) {
    if (fType->GetType()->properties & misc) {
      SemanticError( cg, loc, ERROR___REPEATED_TYPE_ATTRIB);
      return 0;
    }
    if (misc & ~TYPE_MISC_TYPEDEF)
      fType->IsDerived = 1;
    fType->GetType()->properties |= misc;
    return 1;
  }
  return 0;
} // SetTypeMisc

/*
 * SetTypePacked() - Add the PAKED attribute to a type specifier.  Issue an error if it's already set.
 *
 * Returns: TRUE if O.K.
 *
 */

int SetTypePacked( CgContext *cg, SourceLoc *loc, DeclType *fType)
{
  if (fType) {
		if (fType->GetType()->properties & TYPE_MISC_PACKED_KW) {
			SemanticError( cg, loc, ERROR___REPEATED_TYPE_ATTRIB);
			return 0;
    }
    fType->GetType()->properties |= TYPE_MISC_PACKED | TYPE_MISC_PACKED_KW;
    return 1;
  }
  return 0;
} // SetTypePacked

/*
 * SetStorageClass() - Set the storage class of a type.  Issue an error if it's already set to
 *         a conflicting value.
 *
 * Returns: TRUE if O.K.
 *
 */

int SetStorageClass( CgContext *cg, SourceLoc *loc, DeclType *fType, int storage)
{
  fType->GetType()->properties;
  if (fType->storageClass == SC_Unknown) {
    fType->storageClass = (StorageClass) storage;
  } else {
    if (fType->storageClass == (StorageClass) storage) {
      SemanticError( cg, loc, ERROR___STORAGE_SPECIFIED_TWICE);
      return 0;
    } else {
      SemanticError( cg, loc, ERROR___CONFLICTING_STORAGE);
      return 0;
    }
  }
  return 1;
} // SetStorageClass

/********************************** Parser Semantic Rules: ***********************************/

/*
 * Initializer() - Create an EXPR_LIST_OP node with an expression argument.
 *
 */

Expr *Initializer( CgContext *cg, SourceLoc *loc, Expr *fExpr)
{
  Expr *lExpr;
  
  lExpr = (Expr *) NewBinopNode( cg, EXPR_LIST_OP, fExpr, NULL);
  return lExpr;
} // Initilaizer

/*
 * InitializerList() - Add an expression to a list of expressions.  Either can be NULL.
 *
 * Assumes that the nodes on list are EXPR_LIST_OP Binary nodes.
 *
 */

Expr *InitializerList( CgContext *cg, SourceLoc *loc, Expr *list, Expr *last)
{
  Expr *lExpr;
  
  if (list) {
    if (last) {
      lExpr = list;
      while (static_cast< Binary * >( lExpr )->right)
        lExpr = static_cast< Binary * >( lExpr )->right;
      static_cast< Binary * >( lExpr )->right = last;
    }
    return list;
  } else {
    return last;
  }
} // InitializerList

/*
 * ArgumentList() - Add an actual argument to a list of parameters.
 *
 */

Expr *ArgumentList( CgContext *cg, SourceLoc *loc, Expr *flist, Expr *fExpr)
{
  Expr *lExpr, *nExpr;
  
  nExpr = (Expr *) NewBinopNode( cg, FUN_ARG_OP, fExpr, NULL);
  nExpr->type = fExpr->type;
  nExpr->IsLValue = IsLValue(fExpr);
	nExpr->IsConst = nExpr->type->isConst;
  if (flist) {
    lExpr = flist;
    while (static_cast< Binary * >( lExpr )->right)
      lExpr = static_cast< Binary * >( lExpr )->right;
    static_cast< Binary * >( lExpr )->right = nExpr;
    return flist;
  } else {
    return nExpr;
  }
} // ArgumentList

/*
 * ExpressionList() - Add an expression to the end of a list of expressions..
 *
 */

Expr *ExpressionList( CgContext *cg, SourceLoc *loc, Expr *fList, Expr *fExpr)
{
  Expr *lExpr, *nExpr;
  
  nExpr = (Expr *) NewBinopNode( cg, EXPR_LIST_OP, fExpr, NULL);
  nExpr->type = fExpr->type;
  if (fList) {
    lExpr = fList;
    while (static_cast< Binary * >( lExpr )->right)
      lExpr = static_cast< Binary * >( lExpr )->right;
    static_cast< Binary * >( lExpr )->right = nExpr;
    return fList;
  } else {
    return nExpr;
  }
} // ExpressionList

/*
 * AddDecl() - Add a declaration to a list of declarations.  Either can be NULL.
 *
 */

Decl *AddDecl( CgContext *cg, Decl *first, Decl *last)
{
  Decl *lDecl;
  
  if (first) {
    if (last) {
      lDecl = first;
      while (lDecl->next)
        lDecl = lDecl->next;
      lDecl->next = last;
    }
    return first;
  } else {
    return last;
  }
} // AddDecl

/*
 * AddStmt() - Add a list of statements to then end of another list.  Either can be NULL.
 *
 */

Stmt *AddStmt( CgContext *cg, Stmt *first, Stmt *last)
{
  if (first) {
    if (last) {
      Stmt *lStmt = first;
      while ( lStmt->next ) {
				lStmt = lStmt->next;
			}
			lStmt->next = last;
    }
    return first;
  } else {
    return last;
  }
} // AddStmt

/*
 * CheckStatement() - See if this statement is supported by the target profile.
 *
 */

Stmt *CheckStmt( CgContext *cg, Stmt *fStmt)
{
  // Can't do it here.  Must wait until we know which functions are being used.
  //if (fStmt)
  //    theHal->CheckStatement(&fStmt->loc, fStmt);
  return fStmt;
} // CheckStmt

/*
 * Function_Definition_Header() - Combine function <declaration_specifiers> and <declarator>.
 *
 */

Decl *Function_Definition_Header( CgContext *cg, SourceLoc *loc, Decl *fDecl)
{
  Symbol *lSymb = fDecl->symb;
  Symbol *formals;
  int ccount;
  int InProgram;
  int category, domain, qualifiers;
  Type *retType;
  
  if (IsFunction(lSymb)) {
    if (fDecl->type.GetType()->properties & TYPE_MISC_ABSTRACT_PARAMS) {
      SemanticError( cg, loc, ERROR_S_ABSTRACT_NOT_ALLOWED,
                    cg->GetString(fDecl->name));
    }
    if (lSymb->details.fun.statements) {
      SemanticError( cg, loc, ERROR_S_FUN_ALREADY_DEFINED,
                    cg->GetString(fDecl->name));
    }
    if (cg->theHal->entryName == fDecl->name) {
      InProgram = 1;
      fDecl->type.GetType()->properties |= TYPE_MISC_PROGRAM;
      lSymb->details.fun.locals->pid = cg->theHal->pid;
    } else {
      InProgram = 0;
    }
    retType = static_cast< TypeFunction * >( lSymb->type )->rettype;
    ccount = 0;
    formals = lSymb->details.fun.params;
    while (formals) {
      category = GetCategory(formals->type);
      domain = GetDomain(formals->type);
      qualifiers = GetQualifiers(formals->type);
      if (qualifiers == TQ_Out)
        lSymb->details.fun.HasOutParams = 1;
      formals = formals->next;
    }
#if 000 // Can't warn anymore -- could be writing to a global variable
    if (!lSymb->details.fun.HasOutParams && IsVoid(retType)) {
      SemanticWarning(loc, WARNING_S_VOID_FUN_HAS_NO_OUT_ARGS,
                      cg->GetString(fDecl->name));
    }
#endif
    PushScope( cg, lSymb->details.fun.locals);
  } else {
    SemanticError( cg, loc, ERROR_S_NOT_A_FUN,
                  cg->GetString(fDecl->name));
    PushScope( cg, new Scope( cg ));
  }
  cg->currentScope->funindex = ++cg->nextFunctionIndex;
  return fDecl;
} // Function_Definition_Header

/*
 * lCheckInitializationData() - Check data in an init_declarator for compatibility
 *         with variable.
 */

static int lCheckInitializationData( CgContext *cg, SourceLoc *loc, Type *vType, Expr *dExpr, int IsGlobal)
{
  int category, base, ii, vlen, subop;
  Expr *lExpr, *tExpr;
  
  if (!dExpr || !vType)
    return 0;
  base = GetBase(vType);
  category = GetCategory(vType);
  switch (category) {
    default:
    case TC_None:
      return 0;
    case TC_Scalar:
      if (dExpr->kind == BINARY_N && static_cast< Binary * >( dExpr )->op == EXPR_LIST_OP) {
        lExpr = FoldConstants( cg, static_cast< Binary * >( dExpr )->left);
        if (lExpr->kind == CONST_N) {
          if (ConvertType( cg, lExpr, vType, static_cast< Constant * >( lExpr )->type, &tExpr, 1, 0)) {
            static_cast< Binary * >( dExpr )->left = tExpr;
            return 1;
          } else {
            SemanticError( cg, loc, ERROR___INVALID_INITIALIZATION);
            return 0;
          }
        } else {
#if 000 // RSG
          if (IsGlobal) {
            SemanticError( cg, loc, ERROR___NON_CONST_INITIALIZATION);
            return 0;
          } else {
#endif // RSG
            return 1;
#if 000 // RSG
          }
#endif // RSG
        }
      } else {
        SemanticError( cg, loc, ERROR___INVALID_INITIALIZATION);
        return 0;
      }
    case TC_Array:
      vlen = static_cast< TypeArray * >( vType )->numels;
      if (dExpr->kind == BINARY_N && static_cast< Binary * >( dExpr )->op == EXPR_LIST_OP) {
        lExpr = static_cast< Binary * >( dExpr )->left;
        if (!lExpr) {
          SemanticError( cg, loc, ERROR___TOO_LITTLE_DATA);
          return 0;
        }
        if (lExpr->kind == BINARY_N && static_cast< Binary * >( lExpr )->op == EXPR_LIST_OP) {
          for (ii = 0; ii < vlen; ii++) {
            if (lExpr) {
              if (lExpr->kind == BINARY_N && static_cast< Binary * >( lExpr )->op == EXPR_LIST_OP) {
                if (lCheckInitializationData( cg, loc, static_cast< TypeArray * >( vType )->eltype, lExpr, IsGlobal)) {
                  /* O.K. */
                } else {
                  return 0;
                }
              } else {
                SemanticError( cg, loc, ERROR___INVALID_INITIALIZATION);
                return 0;
              }
            } else {
              SemanticError( cg, loc, ERROR___TOO_LITTLE_DATA);
              return 0;
            }
            lExpr = static_cast< Binary * >( lExpr )->right;
          }
          if (lExpr) {
            SemanticError( cg, loc, ERROR___TOO_MUCH_DATA);
          } else {
            subop = SUBOP_V(vlen, GetBase(vType));
            static_cast< Binary * >( dExpr )->left = (Expr *) NewUnopSubNode( cg, VECTOR_V_OP, subop, static_cast< Binary * >( dExpr )->left);
            static_cast< Unary * >( static_cast< Binary * >( dExpr )->left )->type = GetStandardType( cg, GetBase(vType), vlen, 0);
            return 1;
          }
        } else {
          if (ConvertType( cg, lExpr, vType, lExpr->type, &tExpr, 0, 0)) {
            static_cast< Binary * >( dExpr )->left = tExpr;
            return 1;
          } else {
            SemanticError( cg, loc, ERROR___INCOMPAT_TYPE_INIT);
            return 0;
          }
        }
      } else {
        SemanticError( cg, loc, ERROR___INVALID_INITIALIZATION);
      }
      return 0;
    case TC_Function:
    case TC_Struct:
    case TC_Connector:
      SemanticError( cg, loc, ERROR___INVALID_INITIALIZATION);
      return 0;
  }
} // lCheckInitializationData

/*
 * Param_Init_Declarator() - Process a parameter initialization declarator.
 *
 */

Decl *Param_Init_Declarator( CgContext *cg, SourceLoc *loc, Scope *fScope, Decl *fDecl, Expr *fExpr)
{
  Type *lType;
  
  if (fDecl) {
    lType = fDecl->type.GetType();
    if (IsVoid(lType)) {
      SemanticError( cg, loc, ERROR_S_VOID_TYPE_INVALID,
                    cg->GetString(fDecl->name));
    }
    if (GetCategory(lType) == TC_Function) {
      SemanticError( cg, loc, ERROR_S_FUN_TYPE_INVALID,
                    cg->GetString(fDecl->name));
    }
    if (fExpr) {
      if (GetDomain(lType) != TD_Uniform) {
        SemanticError( cg, loc, ERROR_S_NON_UNIFORM_PARAM_INIT,
                      cg->GetString(fDecl->name));
      }
      if (lCheckInitializationData( cg, loc, lType, fExpr, 0)) {
        fDecl->initexpr = fExpr;
      }
    }
  }
  return fDecl;
} // Param_Init_Declarator

/*
 * Init_Declarator() - Set initial value and/or semantics for this declarator.
 *
 */

Stmt *Init_Declarator( CgContext *cg, SourceLoc *loc, Scope *fScope, Decl *fDecl, Expr *fExpr)
{
  int category, len, len2, base;
  Stmt *lStmt = NULL;
  int IsGlobal, IsStatic, IsUniform, IsParam, DontAssign;
  Symbol *lSymb;
  Expr *lExpr;
  Type *lType;
  
  if (fDecl) {
    lSymb = fDecl->symb;
    if (fExpr) {
      if (lSymb->kind != SK_Variable) {
        SemanticError( cg, loc, ERROR_S_INIT_NON_VARIABLE,
                      cg->GetString(lSymb->name));
      } else if (fScope->IsStructScope) {
        SemanticError( cg, loc, ERROR_S_INIT_STRUCT_MEMBER,
                      cg->GetString(lSymb->name));
      } else if (lSymb->storageClass == SC_Extern) {
        SemanticError( cg, loc, ERROR_S_INIT_EXTERN,
                      cg->GetString(lSymb->name));
      } else {
        lType = lSymb->type;
        IsGlobal = fScope->level <= 1;
        IsStatic = lSymb->storageClass == SC_Static;
        IsUniform = GetDomain(lType) == TD_Uniform;
        IsParam = lSymb->properties & SYMB_IS_PARAMETER;
        if (IsGlobal && !IsStatic) {
          DontAssign = 1;
        } else if (IsParam) {
          DontAssign = 1;
        } else {
          DontAssign = 0;
        }
        if (lCheckInitializationData( cg, loc, lType, fExpr, IsGlobal)) {
          category = GetCategory(lType);
          base = GetBase(lType);
          switch (category) {
            default:
            case TC_None:
              SemanticError( cg, loc, ERROR___INVALID_INITIALIZATION);
              break;
            case TC_Scalar:
              assert(fExpr->kind == BINARY_N && static_cast< Binary * >( fExpr )->op == EXPR_LIST_OP);
              if (DontAssign) {
                lSymb->details.var.init = fExpr;
              } else {
                lExpr = (Expr *) NewSymbNode( cg, VARIABLE_OP, lSymb);
                lStmt = NewSimpleAssignmentStmt( cg, loc, lExpr, static_cast< Binary * >( fExpr )->left, 1);
              }
              break;
            case TC_Array:
              if (IsVector(lType, &len) || IsMatrix(lType, &len, &len2)) {
                assert(fExpr->kind == BINARY_N && static_cast< Binary * >( fExpr )->op == EXPR_LIST_OP);
                if (DontAssign) {
                  lSymb->details.var.init = fExpr;
                } else {
                  lExpr = (Expr *) NewSymbNode( cg, VARIABLE_OP, lSymb);
                  lStmt = NewSimpleAssignmentStmt( cg, loc, lExpr, static_cast< Binary * >( fExpr )->left, 1);
                }
              } else {
                SemanticError( cg, loc, ERROR___ARRAY2_INIT_NOT_DONE);
              }
              break;
          }
        }
      }
    } else {
			lStmt = NewExprStmt( cg, cg->tokenLoc, fDecl );
			fDecl = NULL; // Probably need to fix this for assignment decls -- Cass
		}
    if (lSymb->kind == SK_Function) {
      lSymb = lSymb->details.fun.params;
      while (lSymb) {
        if (lSymb->kind == SK_Variable) {
          if (lSymb->details.var.semantics.IsValid())
            SemanticWarning( cg, loc, WARNING_S_FORWARD_SEMANTICS_IGNORED,
                            cg->GetString(lSymb->name));
        }
        lSymb = lSymb->next;
      }
    }
  }
	delete fDecl; // is this the last use of the Decl?
  return lStmt;
} // Init_Declarator

/*
 * Declarator() - Process a declarator.
 *
 */

Decl *Declarator( CgContext *cg, SourceLoc *loc, Decl *fDecl, Atom semantics)
{
  Symbol *lSymb, *params;
  Scope *lScope;
  Type *lType;
  
  if (cg->currentScope->InFormalParameters) {
    /*
     * Don't add formal parameters to the symbol table until we're
     * sure that we're in a function declaration.
     *
     */
    if (fDecl->type.storageClass != SC_Unknown)
      SemanticError( cg, &fDecl->loc, ERROR_S_STORAGE_NOT_ALLOWED,
                    cg->GetString(fDecl->name));
    fDecl->semantics = semantics;
  } else {
    lSymb = LookupLocalSymbol( cg, cg->currentScope, fDecl->name);
    if (!lSymb) {
      lType = GetTypePointer( cg, &fDecl->loc, &fDecl->type);
      if (IsVoid(lType)) {
        SemanticError( cg, &fDecl->loc, ERROR_S_VOID_TYPE_INVALID,
                      cg->GetString(fDecl->name));
      }
      if (fDecl->type.GetType()->properties & TYPE_MISC_TYPEDEF) {
        lSymb = DefineTypedef( cg, loc, cg->currentScope, fDecl->name, lType);
        if (semantics.IsValid())
          SemanticError( cg, loc, ERROR_S_SEMANTICS_NON_VARIABLE,
                        cg->GetString(fDecl->name));
        if (fDecl->type.storageClass != SC_Unknown)
          SemanticError( cg, &fDecl->loc, ERROR_S_STORAGE_NOT_ALLOWED_TYPEDEF,
                        cg->GetString(fDecl->name));
      } else {
        if (GetQualifiers(fDecl->type.GetType()) == TQ_InOut) {
          SemanticError( cg, &fDecl->loc, ERROR_S_IN_OUT_PARAMS_ONLY,
                        cg->GetString(fDecl->name));
        }
        if (GetCategory(fDecl->type.GetType()) == TC_Function) {
          lScope = new Scope( cg );
          params = AddFormalParamDecls( cg, lScope, fDecl->params);
          lSymb = DeclareFunc( cg, &fDecl->loc, cg->currentScope, NULL, fDecl->name, lType, lScope, params);
          if (semantics.IsValid())
            SemanticError( cg, loc, ERROR_S_SEMANTICS_NON_VARIABLE,
                          cg->GetString(fDecl->name));
        } else {
          if (fDecl->type.GetType()->properties & TYPE_MISC_INTERNAL) {
            SemanticError( cg, &fDecl->loc, ERROR_S_INTERNAL_FOR_FUN,
                          cg->GetString(fDecl->name));
          }
          if (fDecl->type.GetType()->properties & TYPE_MISC_INLINE) {
            SemanticError( cg, &fDecl->loc, ERROR_S_INLINE_FOR_FUN,
                          cg->GetString(fDecl->name));
          }
          if (IsUnsizedArray(lType)) {
            SemanticError( cg, &fDecl->loc, ERROR_S_UNSIZED_ARRAY,
                          cg->GetString(fDecl->name));
          }
          if (IsCategory(lType, TC_Array) && !IsPacked(lType)) {
            if (!cg->theHal->GetCapsBit(CAPS_INDEXED_ARRAYS)) {
              // XYZZY - This test needs to be moved to later to support multiple profiles
              SemanticError( cg, &fDecl->loc, ERROR_S_UNPACKED_ARRAY,
                            cg->GetString(fDecl->name));
            }
          }
          lSymb = DefineVar( cg, loc, cg->currentScope, fDecl->name, lType);
          lSymb->storageClass = fDecl->type.storageClass;
          if (semantics.IsValid()) {
            if (cg->currentScope->IsStructScope) {
              cg->currentScope->HasSemantics = 1;
            } else {
              if (cg->currentScope->level > 1) {
                SemanticError( cg, &fDecl->loc, ERROR_S_NO_LOCAL_SEMANTICS,
                              cg->GetString(fDecl->name));
              } else if (fDecl->type.storageClass == SC_Static) {
                SemanticError( cg, &fDecl->loc, ERROR_S_STATIC_CANT_HAVE_SEMANTICS,
                              cg->GetString(fDecl->name));
#if 000 // RSG -- Not sure if this is true.  Do non-static global variables with semantics have to be declared "uniform"?
              } else if (GetDomain(&fDecl->type.GetType()) != TD_Uniform) {
                SemanticError(&fDecl->loc, ERROR_S_NON_STATIC_SEM_NOT_UNIFORM,
                              cg->GetString(fDecl->name));
#endif // RSG
              }
            }
            lSymb->details.var.semantics = semantics;
          }
          if (cg->currentScope->level == 1) {
            if (fDecl->type.storageClass != SC_Static &&
                GetDomain(fDecl->type.GetType()) != TD_Varying)
            {
              lSymb->properties |= SYMB_NEEDS_BINDING;
            }
          }
          if (cg->currentScope->IsStructScope)
            AddParameter( cg, cg->currentScope, lSymb);
        }
      }
    } else {
      if (GetCategory(fDecl->type.GetType()) == TC_Function) {
        lType = GetTypePointer( cg, &fDecl->loc, &fDecl->type);
        lScope = new Scope( cg );
        params = AddFormalParamDecls( cg, lScope, fDecl->params);
        lSymb = DeclareFunc( cg, &fDecl->loc, cg->currentScope, lSymb, fDecl->name, lType, lScope, params);
        lSymb->storageClass = fDecl->type.storageClass;
        if (semantics.IsValid())
          SemanticError( cg, loc, ERROR_S_SEMANTICS_NON_VARIABLE,
                        cg->GetString(fDecl->name));
      } else {
        if (!IsTypeBase(fDecl->type.GetType(), TB_UndefinedType)) {
          SemanticError( cg, &fDecl->loc, ERROR_S_NAME_ALREADY_DEFINED,
                        cg->GetString(fDecl->name));
        }
      }
    }
    fDecl->symb = lSymb;
  }
  return fDecl;
} // Declarator

/*
 * lInsertDimension() - Insert a dimension below dims levels.
 *
 */

static int lInsertDimension( CgContext *cg, SourceLoc *loc, DeclType *fDtype, int dims, int fnumels, int Packed)
{
  int lnumels, lproperties;
  Type *lType, *elType;
  
  if (dims == 0) {
    fDtype->IsDerived = 0;
    lnumels = fnumels;
  } else {
    lType = fDtype->GetType();
    if (IsArray(lType)) {
      lnumels = static_cast< TypeArray * >( lType )->numels;
      lproperties = fDtype->GetType()->properties & TYPE_MISC_MASK;
      //lsize = static_cast< TypeArray * >( lType )->size;
      elType = static_cast< TypeArray * >( lType )->eltype;
      fDtype->SetType( elType );
      if (!lInsertDimension( cg, loc, fDtype, dims - 1, fnumels, Packed))
        return 0;  // error encountered below
      fDtype->GetType()->properties |= lproperties;
    } else {
      return 0;
    }
  }
  lType = GetTypePointer( cg, loc, fDtype);
  SetTypeCategory( cg, loc, 0, fDtype, TC_Array, 1);
  static_cast< TypeArray * >( fDtype->GetType() )->eltype = lType;
  static_cast< TypeArray * >( fDtype->GetType() )->numels = lnumels;
  fDtype->numNewDims = dims + 1;
	if (Packed) {
		fDtype->GetType()->properties |= TYPE_MISC_PACKED;
	} else {
		fDtype->GetType()->properties &= ~TYPE_MISC_PACKED;
	}
  fDtype->IsDerived = 1;
  return 1;
} // lInsertDimension

/*
 * Array_Declarator() - Declare an array of this type.
 *
 */

Decl *Array_Declarator( CgContext *cg, SourceLoc *loc, Decl *fDecl, int size, int Empty)
{
  DeclType *lDtype;
  Type *lType;
  int dims;
  
  lDtype = &fDecl->type;
  if (size <= 0 && !Empty) {
    SemanticError( cg, loc, ERROR___DIMENSION_LT_1);
    size = 1;
  }
  if (IsVoid(lDtype->GetType()))
    SemanticError( cg, loc, ERROR___ARRAY_OF_VOID);
  switch (GetCategory(lDtype->GetType())) {
    case TC_Scalar:
      lType = lDtype->basetype;
      SetTypeCategory( cg, loc, 0, lDtype, TC_Array, 1);
      static_cast< TypeArray * >( lDtype->GetType() )->eltype = lType;
      static_cast< TypeArray * >( lDtype->GetType() )->numels = size;
      lDtype->numNewDims = 1;
      break;
    case TC_Array:
      dims = lDtype->numNewDims;
      lInsertDimension( cg, loc, lDtype, dims, size, lDtype->GetType()->properties & TYPE_MISC_PACKED_KW);
      // if (TotalNumberDimensions > MAX_ARRAY_DIMENSIONS)
      //    SemanticError( cg, loc, ERROR_D_EXCEEDS_MAX_DIMS, MAX_ARRAY_DIMENSIONS);
      break;
    case TC_Function:
      SemanticError( cg, loc, ERROR___ARRAY_OF_FUNS);
      break;
    case TC_Struct:
      lType = GetTypePointer( cg, loc, lDtype);
      (*lDtype) = DeclType( cg, fDecl->name, lType, TC_Array);
      lDtype->GetType()->domain = lType->domain;
      lDtype->GetType()->qualifier = lType->qualifier;
      lDtype->GetType()->isConst = lType->isConst;
      static_cast< TypeArray * >( lDtype->GetType() )->eltype = lType;
      static_cast< TypeArray * >( lDtype->GetType() )->numels = size;
      lDtype->numNewDims = 1;
      break;
    default:
      InternalError( cg, loc, 999, "ArrayDeclarator(): unknown category");
      break;
  }
  lDtype->IsDerived = 1;
  return fDecl;
} // Array_Declarator

/*
 * AddFormalParamDecls() - Add a list of formal parameter declarations to a function
 *         definition's scope.
 */

Symbol *AddFormalParamDecls( CgContext *cg, Scope *fScope, Decl *params)
{
  Symbol *lSymb, *first = NULL, *last;
  Type *lType;
  
  while (params) {
    lSymb = LookupLocalSymbol( cg, fScope, params->name);
    if (lSymb) {
      SemanticError( cg, &params->loc, ERROR_S_PARAM_NAME_TWICE,
                    cg->GetString(params->name));
    } else {
      lSymb = AddSymbol( cg, &params->loc, fScope, params->name,
                        GetTypePointer( cg, &params->loc, &params->type), SK_Variable);
      lSymb->properties |= SYMB_IS_PARAMETER;
      lSymb->details.var.semantics = params->semantics;
      lSymb->details.var.init = params->initexpr;
      if (first) {
        last->next = lSymb;
      } else {
        first = lSymb;
      }
      last = lSymb;
      lType = lSymb->type;
      if (IsCategory(lType, TC_Array) && !IsPacked(lType)) {
        if (!cg->theHal->GetCapsBit(CAPS_INDEXED_ARRAYS)) {
          SemanticError( cg, &params->loc, ERROR_S_UNPACKED_ARRAY,
                        cg->GetString(params->name));
        }
      }
    }
    params = params->next;
  }
  return first;
} // AddFormalParamDecls

/*
 * SetFunTypeParams() - Build a list of types and set this function type's abstract parameter types.
 *
 */

Decl *SetFunTypeParams( CgContext *cg, Scope *fScope, Decl *func, Decl *params, Decl *actuals)
{
  TypeList *formals, *prev, *lType;
  
  fScope->InFormalParameters--;
  formals = prev = NULL;
  while (params) {
    lType = new TypeList();
    lType->next = NULL;
    lType->type = GetTypePointer( cg, &params->loc, &params->type);
    if (formals) {
      prev->next = lType;
    } else {
      formals = lType;
    }
    prev = lType;
    params = params->next;
  }
  if (func && IsCategory(func->type.GetType(), TC_Function)) {
    static_cast< TypeFunction * >( func->type.GetType() )->paramtypes = formals;
    func->type.IsDerived = 1;
  }
  if (actuals) {
    func->params = actuals;
  } else {
    if (!cg->currentScope->HasVoidParameter)
      func->type.GetType()->properties |= TYPE_MISC_ABSTRACT_PARAMS;
  }
  return func;
} // SetFunTypeParams


/*
 * FunctionDeclHeader()
 *
 */

Decl *FunctionDeclHeader( CgContext *cg, SourceLoc *loc, Scope *fScope, Decl *func)
{
  Type *rtnType = GetTypePointer( cg, cg->tokenLoc, &func->type);
  
  if (IsUnsizedArray(rtnType))
    SemanticError( cg, loc, ERROR_S_UNSIZED_ARRAY, cg->GetString(func->name));
  func->type = DeclType( cg, func->name, NULL, TC_Function);
  cg->currentScope->InFormalParameters++;
  func->type.GetType()->properties |= rtnType->properties & (TYPE_MISC_INLINE | TYPE_MISC_INTERNAL);
  rtnType->properties &= ~(TYPE_MISC_INLINE | TYPE_MISC_INTERNAL);
  static_cast< TypeFunction * >( func->type.GetType() )->paramtypes = NULL;
  static_cast< TypeFunction * >( func->type.GetType() )->rettype = rtnType;
  func->type.IsDerived = 1;
  return func;
} // FunctionDeclHeader

/*
 * StructHeader() - Process a struct header.
 *
 */

Type *StructHeader( CgContext *cg, SourceLoc *loc, Scope *fScope, Atom cType, Atom tag)
{
  Symbol *lSymb;
  Type *lType;
  
  if (tag.IsValid()) {
    lSymb = LookupTag( cg, fScope, tag);
    if (!lSymb) {
      lSymb = AddTag( cg, loc, fScope, tag, TC_Struct);
      static_cast< TypeStruct * >( lSymb->type )->tag = tag;
      static_cast< TypeStruct * >( lSymb->type )->semantics = cType;
      static_cast< TypeStruct * >( lSymb->type )->variety = CID_NONE_ID;
    }
    lType = lSymb->type;
    if (!IsCategory(lType, TC_Struct)) {
      SemanticError( cg, loc, ERROR_S_TAG_IS_NOT_A_STRUCT, cg->GetString(tag));
      lType = cg->UndefinedType;
    }
  } else {
    lType = NewType( cg,cg->GetAtom("<unknown-struct>"), TB_UndefinedType, TC_Struct, TD_Unknown, TQ_None, false, 0, 0);
  }
  return lType;
} // StructOrConnectorHeader

/*
 * DefineVar() - Define a new variable in the current scope.
 *
 */

Symbol *DefineVar( CgContext *cg, SourceLoc *loc, Scope *fScope, Atom atom, Type *fType)
{
  Symbol *lSymb;
  
  lSymb = AddSymbol( cg, loc, fScope, atom, fType, SK_Variable);
  return lSymb;
} // DefineVar

/*
 * DefineTypedef() - Define a new type name in the current scope.
 *
 */

Symbol *DefineTypedef( CgContext *cg, SourceLoc *loc, Scope *fScope, Atom atom, Type *fType)
{
  return AddSymbol( cg, loc, fScope, atom, fType, SK_Typedef);
} // DefineTypedef

/*
 * DeclareFunc() - Declare an identifier as a function in the scope fScope.  If it's already
 *         in the symbol table check the overloading rules to make sure that it either
 *         A) matches a previous declaration exactly, or B) is unambiguously resolvable.
 */

Symbol *DeclareFunc( CgContext *cg, SourceLoc *loc, Scope *fScope, Symbol *fSymb, Atom atom, Type *fType,
                    Scope *locals, Symbol *params)
{
  int DiffParamTypes, DiffParamQualifiers, DiffParamCount, DiffReturnType;
  TypeList *oldArgType, *newArgType;
  Symbol *lSymb;
  int index, group, OK;
  
  if (fSymb) {
    if (GetCategory(fSymb->type) != TC_Function) {
      SemanticError( cg, loc, ERROR_S_NAME_ALREADY_DEFINED, cg->GetString(atom));
      lSymb = fSymb;
    } else {
      OK = 1;
      lSymb = fSymb;
      while (lSymb) {
        if (GetCategory(fSymb->type) != TC_Function) {
          InternalError( cg, loc, ERROR_S_SYMBOL_TYPE_NOT_FUNCTION,
                        cg->GetString(lSymb->name));
          return fSymb;
        }
        DiffParamTypes = DiffParamQualifiers = DiffParamCount = DiffReturnType = 0;
        if (!IsSameUnqualifiedType(static_cast< TypeFunction * >( lSymb->type )->rettype, static_cast< TypeFunction * >( fType )->rettype))
          DiffReturnType = 1;
        oldArgType = static_cast< TypeFunction * >( lSymb->type )->paramtypes;
        newArgType = static_cast< TypeFunction * >( fType )->paramtypes;
        while (newArgType && oldArgType) {
          if (!IsSameUnqualifiedType(oldArgType->type, newArgType->type)) {
            DiffParamTypes = 1;
          } else if (GetQualifiers(oldArgType->type) != GetQualifiers(newArgType->type)) {
            DiffParamQualifiers = 1;
          }
          oldArgType = oldArgType->next;
          newArgType = newArgType->next;
        }
        if (newArgType || oldArgType)
          DiffParamCount = 1;
        if (!DiffParamCount && !DiffParamTypes) {
          if (DiffParamQualifiers) {
            SemanticError( cg, loc, ERROR_S_OVERLOAD_DIFF_ONLY_QUALS,
                          cg->GetString(atom));
            OK = 0;
            break;
          }
          if (DiffReturnType) {
            SemanticError( cg, loc, ERROR_S_OVERLOAD_DIFF_ONLY_RETURN,
                          cg->GetString(atom));
            OK = 0;
            break;
          }
          break; // Found the matching function
        }
        lSymb = lSymb->details.fun.overload;
      }
      if (OK) {
        if (DiffParamCount || DiffParamTypes) {
          lSymb = NewSymbol( cg, loc, fScope, atom, fType, SK_Function);
          lSymb->details.fun.params = params;
          lSymb->details.fun.locals = locals;
          lSymb->details.fun.overload = fSymb->details.fun.overload;
          fSymb->details.fun.overload = lSymb;
          if (GetCategory(fType) == TC_Function) {
            locals->returnType = static_cast< TypeFunction * >( fType )->rettype;
          } else {
            locals->returnType = cg->UndefinedType;
          }
        } else {
          if (!(lSymb->properties & SYMB_IS_DEFINED)) {
            // Overwrite previous definitions if this function is not yet defined.
            // Prototype parameter names are ignored.
            lSymb->details.fun.params = params;
            lSymb->details.fun.locals = locals;
          } else {
            // Declarator for a function that's already been defined.  Not an error.
          }
        }
      } else {
        // Found a function that differs only by qualifiers or return type.  Error arleady issued.
        // lSymb = fSymb;
      }
    }
  } else {
    lSymb = AddSymbol( cg, loc, fScope, atom, fType, SK_Function);
    lSymb->details.fun.params = params;
    lSymb->details.fun.locals = locals;
    if (GetCategory(fType) == TC_Function) {
      locals->returnType = static_cast< TypeFunction * >( fType )->rettype;
    } else {
      locals->returnType = cg->UndefinedType;
    }
  }
  if (lSymb->type->properties & TYPE_MISC_INTERNAL) {
    index = cg->theHal->CheckInternalFunction(lSymb, &group);
    if (index) {
      //
      // lSymb->InternalIndex = index; etc.
      //
      lSymb->properties |= SYMB_IS_DEFINED | SYMB_IS_BUILTIN;
      lSymb->details.fun.group = group;
      lSymb->details.fun.index = index;
    } else {
      SemanticError( cg, loc, ERROR_S_INVALID_INTERNAL_FUNCTION, cg->GetString(atom));
    }
  }
  
  return lSymb;
} // DeclareFunc

/*
 * DefineFunction() - Set the body of the function "func" to the statements in "body".
 *
 */

void DefineFunction( CgContext *cg, SourceLoc *loc, Scope *fScope, Decl *func, Stmt *body)
{
  Symbol *lSymb = func->symb;
  SymbolList *lSymbList;
  Scope *globals;
  Type *lType;
  
  if (IsFunction(lSymb)) {
    if (body) {
      if (lSymb->properties & SYMB_IS_DEFINED) {
        SemanticError( cg, loc, ERROR_S_FUN_ALREADY_DEFINED,
                      cg->GetString(lSymb->name));
      } else {
        lSymb->properties |= SYMB_IS_DEFINED;
        lSymb->details.fun.statements = body;
        lType = lSymb->type;
        if ((lType->properties & TYPE_MISC_INLINE) ||
            cg->theHal->GetCapsBit(CAPS_INLINE_ALL_FUNCTIONS))
        {
          lSymb->properties |= SYMB_IS_INLINE_FUNCTION;
        }
      }
      if (!fScope->HasReturnStmt && !IsVoid(fScope->returnType)) {
        SemanticError( cg, loc, ERROR_S_FUNCTION_HAS_NO_RETURN,
                      cg->GetString(lSymb->name));
      }
      if (func->type.GetType()->properties & TYPE_MISC_PROGRAM) {
        globals = fScope->parent;
        if (!globals->programs) {
          lSymbList = new SymbolList();
          lSymbList->next = globals->programs;
          lSymbList->symb = lSymb;
          globals->programs = lSymbList;
        } else {
          SemanticError( cg, loc, ERROR_S_ONE_PROGRAM,
                        cg->GetString(globals->programs->symb->name));
        }
      }
    } else {
      SemanticError( cg, loc, ERROR_S_NO_STATEMENTS, cg->GetString(func->name));
    }
    if (cg->options.dumpParseTree) {
			Writer wr( cg, std::cout );
      wr.WriteScopeDeclarations();
      wr.WriteFunction( lSymb );
    }
  }
	delete func;
} // DefineFunction

/*
 * GlobalInitStatements()
 *
 */

int GlobalInitStatements( CgContext *cg, Scope *fScope, Stmt *fStmt)
{
  Stmt *lStmt;
  
  if (fStmt) {
    if (fScope->initStmts) {
      lStmt = fScope->initStmts;
      while (lStmt->next)
        lStmt = lStmt->next;
      lStmt->next = fStmt;
    } else {
      fScope->initStmts = fStmt;
    }
  }
  return 1;
} // GlobalInitStatements

/*
 * BasicVariable() - A variable identifier has been encountered.
 *
 */

Expr *BasicVariable( CgContext *cg, SourceLoc *loc, Atom name)
{
  Symbol *lSymb;
  
  lSymb = LookupSymbol( cg, cg->currentScope, name);
  if (!lSymb) {
    SemanticError( cg, loc, ERROR_S_UNDEFINED_VAR, cg->GetString(name));
    lSymb = DefineVar( cg, loc, cg->currentScope, name, cg->UndefinedType);
  }
  return (Expr *) NewSymbNode( cg, VARIABLE_OP, lSymb);
} // BasicVariable

/*
 * IsLValue() - Is this expression an l-value?
 *
 */

int IsLValue(const Expr *fExpr)
{
  if (fExpr) {
    return fExpr->IsLValue;
  } else {
    return 0;
  }
} // IsLValue

/*
 * IsConst() - Is this expression an l-value?
 *
 */

int IsConst(const Expr *fExpr)
{
  if (fExpr) {
    return fExpr->IsConst;
  } else {
    return 0;
  }
} // IsConst

/*
 * IsArrayIndex() - Is this expression an array index expression?
 *
 */

int IsArrayIndex(const Expr *fExpr)
{
  if (fExpr) {
    return fExpr->kind == BINARY_N && static_cast< const Binary * >( fExpr )->op == ARRAY_INDEX_OP;
  } else {
    return 0;
  }
} // IsArrayIndex

/*
 * lIsBaseCastValid() - Is it O.K. to cast the base type fromBase to toBase?
 *
 */

static bool lIsBaseCastValid( CgContext *cg, TypeBase toBase, TypeBase fromBase, bool isExplicit)
{
  if (toBase == TB_NoType || fromBase == TB_NoType)
    return false;
  if (toBase == TB_Void || fromBase == TB_Void)
    return false;
  if (toBase == fromBase)
    return true;
  if (cg->theHal->IsValidScalarCast(toBase, fromBase, isExplicit)) {
    return true;
  } else {
    return false;
  }
} // lIsBaseCastValid

/*
 * ConvertType() - Type cast fExpr from fromType to toType if needed.  Ignore qualifiers.
 *
 * If "result" is NULL just check validity of cast; don't allocate cast operator node.
 *
 */

int ConvertType( CgContext *cg, Expr *fExpr, Type *toType, Type *fromType, Expr **result, int IgnorePacked, bool isExplicit)
{
  TypeCategory fcategory, tcategory;
  TypeBase fbase, tbase;
  Type *feltype, *teltype;
  Unary *unnode;
  int ToPacked, FromPacked;
  
  ToPacked = (toType->properties & TYPE_MISC_PACKED) != 0;
  FromPacked = (fromType->properties & TYPE_MISC_PACKED) != 0;
  if (IsSameUnqualifiedType(toType, fromType) &&
      ((ToPacked == FromPacked) || IgnorePacked))
  {
    if (result)
      *result = fExpr;
    return 1;
  } else {
    fcategory = GetCategory(fromType);
    tcategory = GetCategory(toType);
    if (fcategory == tcategory) {
      switch (fcategory) {
        case TC_Scalar:
          fbase = GetBase(fromType);
          tbase = GetBase(toType);
          if (lIsBaseCastValid( cg, tbase, fbase, isExplicit)) {
            if (result) {
              unnode = NewUnopSubNode( cg, CAST_CS_OP, SUBOP_CS(tbase, fbase), fExpr);
              unnode->type = GetStandardType( cg, tbase, 0, 0);
              unnode->HasSideEffects = fExpr->HasSideEffects;
              *result = (Expr *) unnode;
            }
            return 1;
          } else {
            return 0;
          }
          break;
        case TC_Array:
          if (static_cast< TypeArray * >( toType )->numels != static_cast< TypeArray * >( fromType )->numels)
            return 0;
          if (static_cast< TypeArray * >( toType )->numels > 4)
            return 0;
          if (!IgnorePacked && (ToPacked != FromPacked))
            return 0;
          feltype = static_cast< TypeArray * >( fromType )->eltype;
          teltype = static_cast< TypeArray * >( toType )->eltype;
          fcategory = GetCategory(feltype);
          tcategory = GetCategory(teltype);
          if (tcategory != TC_Scalar || fcategory != TC_Scalar)
            return 0;
          fbase = GetBase(feltype);
          tbase = GetBase(teltype);
          if (lIsBaseCastValid( cg, tbase, fbase, isExplicit)) {
            if (result) {
              unnode = NewUnopSubNode( cg, CAST_CV_OP, SUBOP_CV(tbase, static_cast< TypeArray * >( toType )->numels, fbase), fExpr);
              unnode->type = GetStandardType( cg, tbase, static_cast< TypeArray * >( toType )->numels, 0);
              unnode->HasSideEffects = fExpr->HasSideEffects;
              *result = (Expr *) unnode;
            }
            return 1;
          } else {
            return 0;
          }
          break;
        default:
          return 0;
      }
    } else {
      return 0;
    }
  }
} // ConvertType

/*
 * CastScalarVectorMatrix() - Cast a scalar, vector, or matrix expression.
 *
 * Scalar: len = 0.
 * Vector: len >= 1 and len2 = 0
 * Matrix: len >= 1 and len2 >= 1
 *
 * len = 1 means "float f[1]" not "float f"
 *
 */

Expr *CastScalarVectorMatrix( CgContext *cg, Expr *fExpr, TypeBase fbase, TypeBase tbase, int len, int len2)
{
  int op, subop;
  Expr *lExpr;
  
  if (len == 0) {
    op = CAST_CS_OP;
    subop = SUBOP_CS(tbase, fbase);
  } else if (len2 == 0) {
    op = CAST_CV_OP;
    subop = SUBOP_CV(tbase, len, fbase);
  } else {
    op = CAST_CM_OP;
    subop = SUBOP_CM(len2, tbase, len, fbase);
  }
  lExpr = (Expr *) NewUnopSubNode(  cg, (opcode)op, subop, fExpr);
  lExpr->type = GetStandardType( cg, tbase, len, len2);
  return lExpr;
} // CastScalarVectorMatrix

/*
 * ConvertNumericOperands() - Convert two scalar, vector, or matrix expressions to the same type
 *         for use in an expression.  Number of dimensions and lengths may differ.
 *
 * Returns: base type of resulting values.
 *
 */

TypeBase ConvertNumericOperands(CgContext *cg, int baseop, Expr **lExpr, Expr **rexpr, TypeBase lbase, TypeBase rbase,
                                int llen, int rlen, int llen2, int rlen2)
{
  TypeBase nbase;
  
  nbase = cg->theHal->GetBinOpBase(baseop, lbase, rbase, llen, rlen);
  if (nbase != lbase)
    *lExpr = CastScalarVectorMatrix( cg, *lExpr, lbase, nbase, llen, llen2);
  if (nbase != rbase)
    *rexpr = CastScalarVectorMatrix( cg, *rexpr, rbase, nbase, rlen, rlen2);
  return nbase;
} // ConvertNumericOperands

/*
 * CheckBooleanExpr()
 *
 */

Expr *CheckBooleanExpr( CgContext *cg, SourceLoc *loc, Expr *fExpr, int AllowVector)
{
  int len = 0, HasError = 0;
  Type *lType, *leltype;
  
  lType = leltype = fExpr->type;
  if (IsScalar(lType)) {
    if (!IsBoolean(lType)) {
      SemanticError( cg, loc, ERROR___BOOL_EXPR_EXPECTED);
      HasError = 1;
    }
  } else if (IsVector(lType, &len)) {
    leltype = static_cast< TypeArray * >( lType )->eltype;
    if (AllowVector) {
      if (len > 4) {
        SemanticError( cg, loc, ERROR___VECTOR_EXPR_LEN_GR_4);
        HasError = 1;
        len = 4;
      }
      if (!IsBoolean(lType)) {
        SemanticError( cg, loc, ERROR___BOOL_EXPR_EXPECTED);
        HasError = 1;
      }
    } else {
      SemanticError( cg, loc, ERROR___SCALAR_BOOL_EXPR_EXPECTED);
      HasError = 1;
    }
  } else {
    SemanticError( cg, loc, ERROR___BOOL_EXPR_EXPECTED);
    HasError = 1;
  }
  if (HasError)
    fExpr->type = GetStandardType( cg, TB_Boolean, len, 0);
  return fExpr;
} // CheckBooleanExpr

/*
 * NewUnaryOperator() - See if this is a valid Unary operation.  Return a new node with the
 *         proper operator description.
 *
 * Valid operators are:
 *
 *     op      arg1    arg2   result
 *   ------   ------  ------  ------
 *   NEG      scalar  scalar  scalar
 *   NEG_V    vector  vector  vector
 *
 */

Expr *NewUnaryOperator(CgContext *cg, SourceLoc *loc, int fop, Atom name, Expr *fExpr, int IntegralOnly)
{
  int lop, subop = 0, HasError = 0, len = 0;
  TypeBase lbase;
  Type *lType, *eltype;
  Unary *result = NULL;
  int MustBeBoolean, OK = 0;
  
  lop = fop;
  MustBeBoolean = fop == BNOT_OP ? 1 : 0;
  lType = eltype = fExpr->type;
  if (IsScalar(lType)) {
    subop = 0;
  } else if (IsVector(lType, &len)) {
    eltype = static_cast< TypeArray * >( lType )->eltype;
    lop = fop + OFFSET_V_OP;
    subop = SUBOP_V(len, 0);
  } else {
    SemanticError( cg, loc, ERROR_S_INVALID_OPERANDS, cg->GetString(name));
    HasError = 1;
  }
  if (!HasError) {
    if (len > 4) {
      SemanticError( cg, loc, ERROR_S_VECTOR_OPERAND_GR_4, cg->GetString(name));
    } else {
      lbase = GetBase(lType);
      SUBOP_SET_T(subop, lbase);
      if (MustBeBoolean) {
        if (lbase == TB_Boolean) {
          OK = 1;
        } else {
          SemanticError( cg, loc, ERROR___BOOL_EXPR_EXPECTED);
        }
      } else {
        if (cg->theHal->IsNumericBase(lbase)) {
          if (IntegralOnly) {
            if (cg->theHal->IsIntegralBase(lbase)) {
              OK = 1;
            } else {
              SemanticError( cg, loc, ERROR_S_OPERANDS_NOT_INTEGRAL, cg->GetString(name));
            }
          } else {
            OK = 1;
          }
        } else {
          SemanticError( cg, loc, ERROR_S_OPERANDS_NOT_NUMERIC, cg->GetString(name));
        }
      }
      if (OK) {
        result = NewUnopSubNode(  cg, (opcode)lop, subop, fExpr);
        result->type = GetStandardType( cg, lbase, len, 0);
      }
    }
  }
  if (!result) {
    result = NewUnopSubNode(  cg, (opcode)lop, 0, fExpr);
    result->type = cg->UndefinedType;
  }
  return (Expr *) result;
} // NewUnaryOperator

/*
 * NewBinaryOperator() - See if this is a valid Binary operation.  Return a new node with the
 *         proper operator description.
 *
 * Valid operators are:
 *
 *     op      arg1    arg2   result
 *   ------   ------  ------  ------
 *   MUL      scalar  scalar  scalar
 *   MUL_V    vector  vector  vector
 *   MUL_SV*  scalar  vector  vector
 *   MUL_VS*  vector  scalar  vector
 *
 *    *only allowed for smearing operators MUL, DIV, ADD, SUB.
 */

Expr *NewBinaryOperator( CgContext *cg, SourceLoc *loc, int fop, Atom name, Expr *lExpr, Expr *rexpr, int IntegralOnly)
{
  int lop, subop = 0, HasError = 0, llen = 0, rlen = 0, nlen;
  TypeBase lbase, rbase, nbase;
  Type *lType, *rtype, *leltype, *reltype;
  Binary *result = NULL;
  int CanSmear;
  
  lop = fop;
  CanSmear = fop == MUL_OP || fop == DIV_OP || fop == ADD_OP || fop == SUB_OP ? 1 : 0;
  lType = leltype = lExpr->type;
  rtype = reltype = rexpr->type;
  if (IsScalar(lType)) {
    if (IsScalar(rtype)) {
      subop = 0;
    } else if (IsVector(rtype, &rlen)) {
      if (CanSmear) {
        reltype = static_cast< TypeArray * >( rtype )->eltype;
        lop = fop + OFFSET_SV_OP;
        subop = SUBOP_SV(rlen, 0);
      } else {
        SemanticError( cg, loc, ERROR_S_SCALAR_OP_VECTOR_INVALID, cg->GetString(name));
        HasError = 1;
      }
    } else {
      SemanticError( cg, loc, ERROR_S_INVALID_OPERANDS, cg->GetString(name));
      HasError = 1;
    }
  } else if (IsVector(lType, &llen)) {
    leltype = static_cast< TypeArray * >( lType )->eltype;
    if (IsScalar(rtype)) {
      if (CanSmear) {
        lop = fop + OFFSET_VS_OP;
        subop = SUBOP_VS(llen, 0);
      } else {
        SemanticError( cg, loc, ERROR_S_VECTOR_OP_SCALAR_INVALID, cg->GetString(name));
        HasError = 1;
      }
    } else {
      if (IsVector(rtype, &rlen)) {
        reltype = static_cast< TypeArray * >( rtype )->eltype;
        lop = fop + OFFSET_V_OP;
        subop = SUBOP_VS(llen, 0);
        if (llen != rlen) {
          SemanticError( cg, loc, ERROR_S_VECTOR_OPERANDS_DIFF_LEN, cg->GetString(name));
          HasError = 1;
        }
      } else {
        SemanticError( cg, loc, ERROR_S_INVALID_OPERANDS, cg->GetString(name));
        HasError = 1;
      }
    }
  } else {
    SemanticError( cg, loc, ERROR_S_INVALID_OPERANDS, cg->GetString(name));
    HasError = 1;
  }
  if (!HasError) {
    if (llen > 4 || rlen > 4) {
      SemanticError( cg, loc, ERROR_S_VECTOR_OPERAND_GR_4, cg->GetString(name));
    } else {
      lbase = GetBase(lType);
      rbase = GetBase(rtype);
      if (cg->theHal->IsNumericBase(lbase) && cg->theHal->IsNumericBase(rbase)) {
        nbase = ConvertNumericOperands( cg, fop, &lExpr, &rexpr, lbase, rbase, llen, rlen, 0, 0);
        SUBOP_SET_T(subop, nbase);
        nlen = llen > rlen ? llen : rlen;
        result = NewBinopSubNode(  cg, (opcode)lop, subop, lExpr, rexpr);
        result->type = GetStandardType( cg, nbase, nlen, 0);
        if (IntegralOnly && !cg->theHal->IsIntegralBase(nbase)) {
          SemanticError( cg, loc, ERROR_S_OPERANDS_NOT_INTEGRAL, cg->GetString(name));
        }
      } else {
        SemanticError( cg, loc, ERROR_S_OPERANDS_NOT_NUMERIC, cg->GetString(name));
      }
    }
  }
  if (!result) {
    result = NewBinopSubNode(  cg, (opcode)lop, 0, lExpr, rexpr);
    result->type = cg->UndefinedType;
  }
  return (Expr *) result;
} // NewBinaryOperator

/*
 * NewBinaryBooleanOperator() - See if this is a valid Binary Boolean operator.  Return a new
 *         node with the proper operator description.
 *
 * Valid operators are:
 *
 *     op      arg1    arg2   result
 *   ------   ------  ------  ------
 *   BAND     scalar  scalar  scalar
 *   BAND_V   vector  vector  vector
 *
 */

Expr *NewBinaryBooleanOperator( CgContext *cg, SourceLoc *loc, int fop, Atom name, Expr *lExpr, Expr *rexpr)
{
  int lop, subop = 0, HasError = 0, llen = 0, rlen = 0;
  int lbase, rbase;
  Type *lType, *rtype, *leltype, *reltype;
  Binary *result = NULL;
  
  lop = fop;
  lType = leltype = lExpr->type;
  rtype = reltype = rexpr->type;
  if (IsScalar(lType)) {
    if (IsScalar(rtype)) {
      subop = SUBOP__(TB_Boolean);
    } else {
      SemanticError( cg, loc, ERROR_S_INVALID_OPERANDS, cg->GetString(name));
      HasError = 1;
    }
  } else if (IsVector(lType, &llen)) {
    leltype = static_cast< TypeArray * >( lType )->eltype;
    if (IsVector(rtype, &rlen)) {
      reltype = static_cast< TypeArray * >( rtype )->eltype;
      lop = fop + OFFSET_V_OP;
      subop = SUBOP_V(llen, TB_Boolean);
      if (llen != rlen) {
        SemanticError( cg, loc, ERROR_S_VECTOR_OPERANDS_DIFF_LEN, cg->GetString(name));
        HasError = 1;
      }
    } else {
      SemanticError( cg, loc, ERROR_S_INVALID_OPERANDS, cg->GetString(name));
      HasError = 1;
    }
  } else {
    SemanticError( cg, loc, ERROR_S_INVALID_OPERANDS, cg->GetString(name));
    HasError = 1;
  }
  if (!HasError) {
    if (llen > 4) {
      SemanticError( cg, loc, ERROR_S_VECTOR_OPERAND_GR_4, cg->GetString(name));
    } else {
      lbase = GetBase(lType);
      rbase = GetBase(rtype);
      if (lbase == TB_Boolean && rbase == TB_Boolean) {
        result = NewBinopSubNode(  cg, (opcode)lop, subop, lExpr, rexpr);
        result->type = GetStandardType( cg, TB_Boolean, llen, 0);
      } else {
        SemanticError( cg, loc, ERROR_S_OPERANDS_NOT_BOOLEAN, cg->GetString(name));
      }
      if (lExpr->HasSideEffects || rexpr->HasSideEffects) {
        SemanticError( cg, loc, ERROR_S_OPERANDS_HAVE_SIDE_EFFECTS, cg->GetString(name));
      }
    }
  }
  if (!result) {
    result = NewBinopSubNode(  cg, (opcode)lop, 0, lExpr, rexpr);
    result->type = cg->UndefinedType;
  }
  return (Expr *) result;
} // NewBinaryBooleanOperator

/*
 * NewBinaryComparisonOperator() - See if this is a valid Binary comparison.  Return a new node
 *         with the proper operator description.
 *
 * Valid operators are:
 *
 *    op     arg1    arg2   result
 *   ----   ------  ------  ------
 *   LT     scalar  scalar  scalar
 *   LT_V   vector  vector  vector
 *
 */

Expr *NewBinaryComparisonOperator( CgContext *cg, SourceLoc *loc, int fop, Atom name, Expr *lExpr, Expr *rexpr)
{
  int lop, subop = 0, HasError = 0, llen = 0, rlen = 0, nlen = 0;
  TypeBase lbase, rbase, nbase;
  Type *lType, *rtype, *leltype, *reltype;
  Binary *result = NULL;
  
  lop = fop;
  lType = leltype = lExpr->type;
  rtype = reltype = rexpr->type;
  if (IsScalar(lType)) {
    if (IsScalar(rtype)) {
      subop = 0;
    } else if (IsVector(rtype, &rlen)) {
      reltype = static_cast< TypeArray * >( rtype )->eltype;
      lop = fop + OFFSET_SV_OP;
      subop = SUBOP_SV(rlen, 0);
    } else {
      SemanticError( cg, loc, ERROR_S_INVALID_OPERANDS, cg->GetString(name));
      HasError = 1;
    }
  } else if (IsVector(lType, &llen)) {
    leltype = static_cast< TypeArray * >( lType )->eltype;
    if (IsScalar(rtype)) {
      lop = fop + OFFSET_VS_OP;
      subop = SUBOP_VS(llen, 0);
    } else if (IsVector(rtype, &rlen)) {
      reltype = static_cast< TypeArray * >( rtype )->eltype;
      lop = fop + OFFSET_V_OP;
      subop = SUBOP_V(llen, 0);
      if (llen != rlen) {
        SemanticError( cg, loc, ERROR_S_VECTOR_OPERANDS_DIFF_LEN, cg->GetString(name));
        HasError = 1;
      }
      nlen = llen;
    } else {
      SemanticError( cg, loc, ERROR_S_INVALID_OPERANDS, cg->GetString(name));
      HasError = 1;
    }
  } else {
    SemanticError( cg, loc, ERROR_S_INVALID_OPERANDS, cg->GetString(name));
    HasError = 1;
  }
  if (!HasError) {
    if (nlen > 4) {
      SemanticError( cg, loc, ERROR_S_VECTOR_OPERAND_GR_4, cg->GetString(name));
    } else {
      lbase = GetBase(lType);
      rbase = GetBase(rtype);
      if (cg->theHal->IsNumericBase(lbase) && cg->theHal->IsNumericBase(rbase)) {
        nbase = ConvertNumericOperands( cg, fop, &lExpr, &rexpr, lbase, rbase, llen, rlen, 0, 0);
        SUBOP_SET_T(subop, nbase);
        nlen = llen > rlen ? llen : rlen;
        result = NewBinopSubNode(  cg, (opcode)lop, subop, lExpr, rexpr);
        result->type = GetStandardType( cg, TB_Boolean, nlen, 0);
      } else if (lbase == TB_Boolean && rbase == TB_Boolean) {
        subop = SUBOP_V(nlen, TB_Boolean);
        result = NewBinopSubNode(  cg, (opcode)lop, subop, lExpr, rexpr);
        result->type = GetStandardType( cg, TB_Boolean, nlen, 0);
      } else {
        SemanticError( cg, loc, ERROR_S_OPERANDS_NOT_NUMERIC, cg->GetString(name));
      }
    }
  }
  if (!result) {
    result = NewBinopSubNode(  cg, (opcode)lop, 0, lExpr, rexpr);
    result->type = cg->UndefinedType;
  }
  return (Expr *) result;
} // NewBinaryComparisonOperator

/*
 * NewConditionalOperator() - Check the types of the components of a conditional expression.
 *         Return a new node with the proper operator description.
 *
 * Valid forma are:
 *
 *     op       cond    exp1    exp2   result
 *   -------   ------  ------  ------  ------
 *   COND      scalar  scalar  scalar  scalar
 *   COND_SV   scalar  vector  vector  vector
 *   COND_V    vector  vector  vector  vector
 *   COND_GEN  scalar   type    type    type
 */

Expr *NewConditionalOperator( CgContext *cg, SourceLoc *loc, Expr *bexpr, Expr *lExpr, Expr *rexpr)
{
  int lop, subop, blen = 0, llen = 0, rlen = 0, nlen = 0;
  int HasError = 0, LIsNumeric, LIsBoolean, LIsSimple;
  TypeBase lbase, rbase, nbase;
	TypeCategory category;
  Type *btype, *lType, *rtype, *beltype, *leltype, *reltype;
  Type *resulttype = cg->UndefinedType;
  Trinary *result = NULL;
  
	nlen; // variable not unused now
	
  // Type of conditional expression is checked elsewhere.
  
  lop = COND_OP;
  subop = 0;
  btype = beltype = bexpr->type;
  lType = leltype = lExpr->type;
  rtype = reltype = rexpr->type;
  lbase = GetBase(leltype);
  rbase = GetBase(reltype);
  LIsNumeric = cg->theHal->IsNumericBase(lbase) & cg->theHal->IsNumericBase(rbase);
  LIsBoolean = (lbase == TB_Boolean) & (rbase == TB_Boolean);
  LIsSimple = LIsNumeric | LIsBoolean;
  if (LIsSimple) {
    
    // 1) Numeric
    
    if (IsScalar(btype)) {
      
      // 1A) Scalar ? Scalar : Scalar
      
      if (IsScalar(lType)) {
        if (IsScalar(rtype)) {
          // O.K.
        } else {
          SemanticError( cg, loc, ERROR___QSTN_SCALAR_3RD_OPND_EXPECTED);
          HasError = 1;
        }
        
        // 1B) Scalar ? Vector : Vector
        
      } else if (IsVector(lType, &llen)) {
        leltype = static_cast< TypeArray * >( lType )->eltype;
        if (IsVector(rtype, &rlen)) {
          reltype = static_cast< TypeArray * >( rtype )->eltype;
          lbase = GetBase(leltype);
          rbase = GetBase(reltype);
          lop = COND_SV_OP;
          subop = SUBOP_SV(llen, 0);
        } else {
          SemanticError( cg, loc, ERROR___QSTN_VECTOR_3RD_OPND_EXPECTED);
          HasError = 1;
        }
        
        // 1C) Scalar ? Array : Array >>--->> Treat as non-numeric case
        
      } else {
        LIsSimple = 0; // Check type compatibility later
      }
    } else if (IsVector(btype, &blen)) {
      
      // 1D) Vector ? Vector : Vector
      
      if (IsVector(lType, &llen) && IsVector(rtype, &rlen)) {
        lop = COND_V_OP;
        subop = SUBOP_SV(llen, 0);
        leltype = static_cast< TypeArray * >( lType )->eltype;
        reltype = static_cast< TypeArray * >( rtype )->eltype;
        lbase = GetBase(leltype);
        rbase = GetBase(reltype);
      } else {
        SemanticError( cg, loc, ERROR___QSTN_VECTOR_23_OPNDS_EXPECTED);
        HasError = 1;
      }
    } else {
      SemanticError( cg, loc, ERROR___QSTN_INVALID_1ST_OPERAND);
      HasError = 1;
    }
  }
  if (!LIsSimple) {
    
    // 2) Not numeric - must be same type.  Requires scalar condition.
    
    if (IsScalar(btype)) {
      if (IsSameUnqualifiedType(lType, rtype)) {
        lop = COND_GEN_OP;
        resulttype = lType;
      } else {
        SemanticError( cg, loc, ERROR___QSTN_23_OPNDS_INCOMPAT);
        HasError = 1;
      }
    } else {
      SemanticError( cg, loc, ERROR___QSTN_1ST_OPERAND_NOT_SCALAR);
      HasError = 1;
    }
  }
  if (!HasError) {
    if (lExpr->HasSideEffects || rexpr->HasSideEffects) {
      SemanticError( cg, loc, ERROR_S_OPERANDS_HAVE_SIDE_EFFECTS, "?:");
    }
    if (LIsSimple) {
      nbase = ConvertNumericOperands( cg, COND_OP, &lExpr, &rexpr, lbase, rbase, llen, rlen, 0, 0);
      if (llen == rlen && (blen == 0 || blen == llen)) {
        SUBOP_SET_T(subop, nbase);
        result = NewTriopSubNode(  cg, (opcode)lop, subop, bexpr, lExpr, rexpr);
        result->type = GetStandardType( cg, nbase, llen, 0);
      } else {
        SemanticError( cg, loc, ERROR_S_VECTOR_OPERANDS_DIFF_LEN, "\"? :\"");
        HasError = 1;
      }
    } else {
      category = GetCategory(lType);
      if ((category == TC_Scalar ||
           category == TC_Array ||
           category == TC_Struct) &&
          !IsVoid(lType))
      {
        result = NewTriopSubNode(  cg, (opcode)lop, 0, bexpr, lExpr, rexpr);
        result->type = lType;
      } else {
        SemanticError( cg, loc, ERROR___QSTN_23_OPNDS_INVALID);
        HasError = 1;
      }
    }
  }
  if (!result) {
    result = NewTriopSubNode(  cg, (opcode)lop, 0, bexpr, lExpr, rexpr);
    result->type = cg->UndefinedType;
  }
  return (Expr *) result;
} // NewConditionalOperator

/*
 * NewSwizzleOperator() - See if this is a valid swizzle operation.  Return a new node with the
 *         proper operator description.
 *
 */

Expr *NewSwizzleOperator( CgContext *cg, SourceLoc *loc, Expr *fExpr, Atom ident)
{
  int HasError = 0, len = 0, ii, maxi, tmask, mask = 0, mlen = 0, LIsLValue;
	TypeBase base;
  Type *ftype, *feltype;
  Unary *result = NULL;
  
  mask = GetSwizzleOrWriteMask( cg, loc, ident, &LIsLValue, &mlen);
  ftype = fExpr->type;
  if (IsScalar(ftype)) {
    feltype = ftype;
    maxi = 0;
  } else if (IsVector(ftype, &len)) {
    feltype = static_cast< TypeArray * >( ftype )->eltype;
    maxi = len - 1;
    if (len > 4) {
      SemanticError( cg, loc, ERROR_S_VECTOR_OPERAND_GR_4, ".");
      HasError = 1;
    }
  } else {
    SemanticError( cg, loc, ERROR_S_OPERANDS_NOT_SCALAR_VECTOR, ".");
    HasError = 1;
  }
  if (!HasError) {
    base = GetBase(feltype);
    tmask = mask;
    for (ii = 0; ii < mlen; ii++) {
      if ((tmask & 0x3) > maxi) {
        SemanticError( cg, loc, ERROR_S_SWIZZLE_MASK_EL_MISSING,
                      cg->GetString(ident));
        HasError = 1;
        break;
      }
      tmask >>= 2;
    }
    if (!HasError) {
      if (mlen == 1)
        mlen = 0; // I.e. scalar, not array[1]
      result = NewUnopSubNode( cg, SWIZZLE_Z_OP, SUBOP_Z(mask, mlen, len, base), fExpr);
      result->type = GetStandardType( cg, base, mlen, 0);
      result->IsLValue = LIsLValue & fExpr->IsLValue;
      result->IsConst = result->IsLValue & fExpr->IsConst;
    }
  }
  if (!result) {
    result = NewUnopSubNode( cg, SWIZZLE_Z_OP, 0, fExpr);
    result->type = cg->UndefinedType;
  }
  return (Expr *) result;
} // NewSwizzleOperator

/*
 * NewMatrixSwizzleOperator() - See if this is a valid matrix swizzle operation.  Return a new
 *         node with the proper operator description.
 */

Expr *NewMatrixSwizzleOperator( CgContext *cg, SourceLoc *loc, Expr *fExpr, Atom ident)
{
  int HasError = 0, len = 0, len2 = 0, ii, maxi, tmask, mask = 0, mlen = 0, LIsLValue;
	TypeBase base;
  Type *ftype, *feltype;
  Unary *result = NULL;
  
  mask = GetMatrixSwizzleOrWriteMask( cg, loc, ident, &LIsLValue, &mlen);
  ftype = fExpr->type;
  if (IsMatrix(ftype, &len, &len2)) {
    feltype = static_cast< TypeArray * >( ftype )->eltype;
    maxi = len - 1;
    if (len > 4 || len2 > 4) {
      SemanticError( cg, loc, ERROR_S_MATRIX_OPERAND_GR_4, ".");
      HasError = 1;
    }
  } else {
    SemanticError( cg, loc, ERROR_S_OPERANDS_NOT_MATRIX, ".");
    HasError = 1;
  }
  if (!HasError) {
    base = GetBase(feltype);
    tmask = mask;
    for (ii = 0; ii < mlen; ii++) {
      if ((tmask & 0x3) >= len || ((tmask >> 2) & 0x3) >= len2) {
        SemanticError( cg, loc, ERROR_S_SWIZZLE_MASK_EL_MISSING,
                      cg->GetString(ident));
        HasError = 1;
        break;
      }
      tmask >>= 4;
    }
    if (!HasError) {
      if (mlen == 1)
        mlen = 0; // I.e. scalar, not array[1]
      result = NewUnopSubNode( cg, SWIZMAT_Z_OP, SUBOP_ZM(mask, mlen, len2, len, base), fExpr);
      result->type = GetStandardType( cg, base, mlen, 0);
      result->IsLValue = LIsLValue & fExpr->IsLValue;
      result->IsConst = result->IsLValue & fExpr->IsConst;
    }
  }
  if (!result) {
    result = NewUnopSubNode( cg, SWIZMAT_Z_OP, 0, fExpr);
    result->type = cg->UndefinedType;
  }
  return (Expr *) result;
} // NewMatrixSwizzleOperator

/*
 * NewVectorConstructor() - Construct a vector of length 1 to 4 from the expressions in fExpr.
 *
 */

Expr *NewVectorConstructor( CgContext *cg, SourceLoc *loc, Type *fType, Expr *fExpr)
{
	TypeBase lbase, nbase;
  int len = 0, HasError = 0, size = 0,lNumeric, nNumeric, vlen, vlen2;
  Unary *result = NULL;
  Expr *lExpr;
  Type *lType, *rType;
  
  if (fType) {
    rType = fType;
    if (IsScalar(fType)) {
      size = 1;
    } else if (IsVector(fType, &vlen)) {
      size = vlen;
    } else if (IsMatrix(fType, &vlen, &vlen2)) {
      size = vlen*vlen2;
    } else {
      SemanticError( cg, loc, ERROR___INVALID_TYPE_FUNCTION);
      rType = cg->UndefinedType;
    }
  } else {
    rType = cg->UndefinedType;
  }
  lExpr = fExpr;
  while (lExpr) {
    vlen = 0;
    lType = lExpr->type;
    lbase = GetBase(lType);
    lNumeric = cg->theHal->IsNumericBase(lbase);
    if (!lNumeric && lbase != TB_Boolean) {
      SemanticError( cg, loc, ERROR___VECTOR_CONSTR_NOT_NUM_BOOL);
      HasError = 1;
      break;
    }
    if (IsScalar(lType)) {
      vlen = 1;
#if 000 // Unifdefout this to allow things like: "{ vec3, float }"
    } else if (IsVector(lType, &vlen)) {
      /* Nothing to do. */
#endif
    } else {
      SemanticError( cg, loc, ERROR___VECTOR_CONSTR_NOT_SCALAR);
      HasError = 1;
      break;
    }
    if (len == 0) {
      nbase = lbase;
      nNumeric = lNumeric;
    } else if (len + vlen <= 4) {
      if (lNumeric == nNumeric) {
        if (nNumeric) {
          nbase = cg->theHal->GetBinOpBase(VECTOR_V_OP, nbase, lbase, 0, 0);
        }
      } else {
        SemanticError( cg, loc, ERROR___MIXED_NUM_NONNUM_VECT_CNSTR);
        HasError = 1;
        break;
      }
    } else {
      SemanticError( cg, loc, ERROR___CONSTRUCTER_VECTOR_LEN_GR_4);
      HasError = 1;
      break;
    }
    len += vlen;
    lExpr = static_cast< Binary * >( lExpr )->right;
  }
  if (size && !HasError) {
    if (size > len) {
      SemanticError( cg, loc, ERROR___TOO_LITTLE_DATA_TYPE_FUN);
      HasError = 1;
    } else if (size < len) {
      SemanticError( cg, loc, ERROR___TOO_MUCH_DATA_TYPE_FUN);
      HasError = 1;
    }
  }
  if (!HasError) {
    lExpr = fExpr;
    while (lExpr) {
      lType = lExpr->type;
      lbase = GetBase(lType);
      if (lbase != nbase)
        static_cast< Binary * >( lExpr )->left = CastScalarVectorMatrix( cg, static_cast< Binary * >( lExpr )->left, lbase, nbase, 0, 0);
      lExpr = static_cast< Binary * >( lExpr )->right;
    }
    result = NewUnopSubNode( cg, VECTOR_V_OP, SUBOP_V(len, nbase), fExpr);
    result->type = GetStandardType( cg, nbase, len, 0);
  }
  if (!result) {
    result = NewUnopSubNode( cg, VECTOR_V_OP, 0, fExpr);
    result->type = rType;
  }
  return (Expr *) result;
} // NewVectorConstructor

/*
 * NewCastOperator() - Type cast "fExpr" to "ftype" if possible.
 *
 */

Expr *NewCastOperator( CgContext *cg, SourceLoc *loc, Expr *fExpr, Type *toType)
{
  Expr *lExpr;
  
  if (ConvertType( cg, fExpr, toType, fExpr->type, &lExpr, 0, 1)) {
    lExpr->type = toType;
    return lExpr;
  } else {
    SemanticError( cg, loc, ERROR___INVALID_CAST);
    return fExpr;
  }
} // NewCastOperator

/*
 * NewMemberSelectorOrSwizzleOrWriteMaskOperator() - Construct either a struct member
 *         operator,or a swizzle operator, or a writemask operator, depending upon the
 *         type of the expression "fExpr".   I think I'm gonna barf.
 */

Expr *NewMemberSelectorOrSwizzleOrWriteMaskOperator( CgContext *cg, SourceLoc *loc, Expr *fExpr, Atom ident)
{
  Type *lType = fExpr->type;
  int len, len2;
  Expr *lExpr, *mExpr;
  Symbol *lSymb;
  
  if (IsCategory(lType, TC_Struct)) {
    lSymb = LookupLocalSymbol( cg, static_cast< TypeStruct * >( lType )->members, ident);
    if (lSymb) {
      mExpr = (Expr *) NewSymbNode( cg, MEMBER_OP, lSymb);
      lExpr = (Expr *) NewBinopNode( cg, MEMBER_SELECTOR_OP, fExpr, mExpr);
      lExpr->IsLValue = fExpr->IsLValue;
      lExpr->IsConst = fExpr->IsConst;
      lExpr->type = lSymb->type;
    } else {
      SemanticError( cg, loc, ERROR_SS_NOT_A_MEMBER,
                    cg->GetString(ident), cg->GetString(static_cast< TypeStruct * >( lType )->tag));
      lExpr = fExpr;
    }
  } else if (IsScalar(lType) || IsVector(lType, &len)) {
    lExpr = NewSwizzleOperator( cg, loc, fExpr, ident);
  } else if (IsMatrix(lType, &len, &len2)) {
    lExpr = NewMatrixSwizzleOperator( cg, loc, fExpr, ident);
  } else {
    SemanticError( cg, loc, ERROR_S_LEFT_EXPR_NOT_STRUCT_ARRAY, cg->GetString(ident));
    lExpr = fExpr;
  }
  return lExpr;
} // NewMemberSelectorOrSwizzleOrWriteMaskOperator

/*
 * NewIndexOperator() - Construct an array index operator.
 *
 */

Expr *NewIndexOperator( CgContext *cg, SourceLoc *loc, Expr *fExpr, Expr *ixexpr)
{
  Expr *lExpr;
  
  if (IsCategory(fExpr->type, TC_Array)) {
    lExpr = (Expr *) NewBinopNode( cg, ARRAY_INDEX_OP, fExpr, ixexpr);
    lExpr->IsLValue = fExpr->IsLValue;
    lExpr->IsConst = fExpr->IsConst;
    lExpr->type = GetElementType( cg, fExpr->type);
  } else {
    SemanticError( cg, loc, ERROR___INDEX_OF_NON_ARRAY);
    lExpr = fExpr;
  }
  return lExpr;
} // NewIndexOperator

/*
 * lResolveOverloadedFunction() - Resolve an overloaded function call.
 *
 */

Symbol *lResolveOverloadedFunction( CgContext *cg, SourceLoc *loc, Symbol *fSymb, Expr *actuals)
{
  const int NO_MATCH = 0;
  const int EXACT_MATCH = 1;
  const int VALID_MATCH = 2;
  int paramno, numexact, numvalid, ii;
  Symbol *lSymb, *lExact, *lValid;
  TypeList *lFormals;
  
	//cg->Printf( "actuals: %s\n", actuals->Stringify()->c_str() );
	
  lSymb = fSymb;
  while (lSymb) {
    lSymb->details.fun.flags = EXACT_MATCH;
    lSymb = lSymb->details.fun.overload;
  }
  paramno = 0;
  while (actuals) {
    numexact = numvalid = 0;
    lExact = lValid = fSymb;
    lSymb = fSymb;
    while (lSymb) {
      if (lSymb->details.fun.flags) {
        lFormals = static_cast< TypeFunction * >( lSymb->type )->paramtypes;
        for (ii = 0; ii < paramno; ii++) {
          if (lFormals) {
            lFormals = lFormals->next;
          } else {
            // Ran out of formals -- kick it out.
            lSymb->details.fun.flags = NO_MATCH;
          }
        }
        if (lFormals) {
          if (IsSameUnqualifiedType(lFormals->type, actuals->type)) {
            lSymb->details.fun.flags = EXACT_MATCH;
            lExact = lSymb;
            numexact++;
          } else {
            if (ConvertType( cg, NULL, lFormals->type, actuals->type, NULL, 0, 0)) {
              lSymb->details.fun.flags = VALID_MATCH;
              lValid = lSymb;
              numvalid++;
            } else {
              lSymb->details.fun.flags = NO_MATCH;
            }
          }
        } else {
          lSymb->details.fun.flags = NO_MATCH;
        }
      }
      lSymb = lSymb->details.fun.overload;
    }
    if (numexact == 1)
      return lExact;
    if (numvalid == 1)
      return lValid;
    if (numexact > 0) {
      if (numvalid > 0) {
        // Disqualify non-exact matches:
        lSymb = fSymb;
        while (lSymb) {
          if (lSymb->details.fun.flags == VALID_MATCH)
            lSymb->details.fun.flags = NO_MATCH;
          lSymb = lSymb->details.fun.overload;
        }
      }
    } else {
      if (numvalid == 0) {
        // Nothing matches.
        break;
      }
    }
    actuals = static_cast< Binary * >( actuals )->right;
    paramno++;
  }
  // If multiple matches still present check number of args:
  if (numexact > 0 || numvalid > 0) {
    numvalid = 0;
    lSymb = lValid = fSymb;
    while (lSymb) {
      if (lSymb->details.fun.flags) {
        lFormals = static_cast< TypeFunction * >( lSymb->type )->paramtypes;
        for (ii = 0; ii < paramno; ii++) {
          if (lFormals) {
            lFormals = lFormals->next;
          } else {
            // Ran out of formals -- shouldn't happen.
            assert(0);
          }
        }
        if (lFormals) {
          lSymb->details.fun.flags = NO_MATCH;
        } else {
          numvalid++;
          lValid = lSymb;
        }
      }
      lSymb = lSymb->details.fun.overload;
    }
    if (numvalid == 1)
      return lValid;
  }
  if (numvalid > 0) {
    SemanticError( cg, loc, ERROR_S_AMBIGUOUS_FUN_REFERENCE, cg->GetString(fSymb->name));
  } else {
    SemanticError( cg, loc, ERROR_S_NO_COMPAT_OVERLOADED_FUN, cg->GetString(fSymb->name));
  }
#if 1 // Detailed error messages - requires printing of types
  lSymb = fSymb;
  numvalid = 0;
  while (lSymb) {
    if (lSymb->details.fun.flags) {
			Writer wr( cg, std::cout );
      cg->Printf("    #%d: ", ++numvalid);
      wr.WriteType( static_cast< TypeFunction * >( lSymb->type )->rettype);
      cg->Printf(" %s", cg->GetString(lSymb->name));
      wr.WriteType( lSymb->type);
      cg->Printf("\n");
    }
    lSymb = lSymb->details.fun.overload;
  }
#endif
  return fSymb;
} // lResolveOverloadedFunction

/*
 * NewFunctionCallOperator() - Construct a function call node.  Check types of parameters,
 *         resolve overloaded function, etc.
 *
 */

Expr *NewFunctionCallOperator( CgContext *cg, SourceLoc *loc, Expr *funExpr, Expr *actuals)
{
  Binary *result = NULL;
  Type *funType, *formalType, *actualType;
  TypeList *lFormals;
  Expr *lExpr, *lActuals;
  Symbol *lSymb;
  int paramno, inout;
  int lop, lsubop = FUN_CALL_OP;
  
  funType = funExpr->type;
  if (IsCategory(funType, TC_Function)) {
    lop = FUN_CALL_OP;
    lsubop = 0;
    if (funExpr->kind == SYMB_N) {
      lSymb = static_cast< Symb * >( funExpr )->symbol;
      if (lSymb->kind == SK_Function) {
        if (lSymb->details.fun.overload) {
          lSymb = lResolveOverloadedFunction( cg, loc, lSymb, actuals);
          static_cast< Symb * >( funExpr )->symbol = lSymb;
          funType = funExpr->type = lSymb->type;
        }
        if (funType->properties & TYPE_MISC_INTERNAL) {
          lop = FUN_BUILTIN_OP;
          lsubop = (lSymb->details.fun.group << 16) | lSymb->details.fun.index;
        }
      } else {
        InternalError( cg, loc, ERROR_S_SYMBOL_NOT_FUNCTION, cg->GetString(lSymb->name));
      }
    }
    lFormals = static_cast< TypeFunction * >( funType )->paramtypes;
    lActuals = actuals;
    paramno = 0;
    while (lFormals && lActuals) {
      paramno++;
      formalType = lFormals->type;
      actualType = lActuals->type;
      inout = 0;
			TypeQualifier qualifier = GetQualifiers( formalType );
      if (( qualifier ==  TQ_In) ||
          !( qualifier == TQ_InOut))
        inout |= 1;
      if ( qualifier == TQ_Out) {
        inout |= 2;
        lExpr = static_cast< Binary * >( lActuals )->left;
        if (lExpr) {
          if (lExpr->IsLValue) {
            if (! actualType->isConst ) {
              if (IsSameUnqualifiedType(formalType, actualType) &&
                  IsPacked(formalType) == IsPacked(actualType))
              {
                SUBOP_SET_MASK(static_cast< Binary * >( lActuals )->subop, inout);
              } else {
                SemanticError( cg, loc, ERROR_D_OUT_PARAM_NOT_SAME_TYPE, paramno);
              }
            } else {
              SemanticError( cg, loc, ERROR_D_OUT_PARAM_IS_CONST, paramno);
            }
          } else {
            SemanticError( cg, loc, ERROR_D_OUT_PARAM_NOT_LVALUE, paramno);
          }
        }
      } else if (ConvertType( cg, static_cast< Binary * >( lActuals )->left, formalType, actualType, &lExpr, 0, 0)) {
        static_cast< Binary * >( lActuals )->left = lExpr;
        SUBOP_SET_MASK(static_cast< Binary * >( lActuals )->subop, inout);
      } else {
        SemanticError( cg, loc, ERROR_D_INCOMPATIBLE_PARAMETER, paramno);
      }
      lFormals = lFormals->next;
      lActuals = static_cast< Binary * >( lActuals )->right;
    }
    if (lFormals) {
      if (!IsVoid(lFormals->type))
        SemanticError( cg, loc, ERROR___TOO_FEW_PARAMS);
    } else if (lActuals) {
      SemanticError( cg, loc, ERROR___TOO_MANY_PARAMS);
    }
    result = NewBinopSubNode(  cg, (opcode)lop, lsubop, funExpr, actuals);
    result->IsLValue = 0;
    result->IsConst = 0;
    result->HasSideEffects |= lSymb->details.fun.HasOutParams;
    result->type = static_cast< TypeFunction * >( funExpr->type )->rettype;
  } else {
    SemanticError( cg, loc, ERROR___CALL_OF_NON_FUNCTION);
  }
  if (!result) {
    result = NewBinopNode( cg, FUN_CALL_OP, funExpr, actuals);
    result->type = cg->UndefinedType;
  }
  return (Expr *) result;
} // NewFunctionCallOperator

/*
 * NewSimpleAssignment() - Build a new simple assignment expression.
 *
 */

Expr *NewSimpleAssignment( CgContext *cg, SourceLoc *loc, Expr *fVar, Expr *fExpr, int InInit)
{
  int lop, subop, base, len, vqualifiers, vdomain, edomain;
  Type *vType, *eType;
  Expr *lExpr;
  
  vType = fVar->type;
  eType = fExpr->type;
  vqualifiers = GetQualifiers(vType);
  vdomain = GetDomain(vType);
  edomain = GetDomain(eType);
  if (!fVar->IsLValue)
    SemanticError(  cg, loc, ERROR___ASSIGN_TO_NON_LVALUE);
  //if ((vqualifiers & TQ_CONST) && !InInit)
  if (fVar->IsConst && !InInit)
    SemanticError(  cg, loc, ERROR___ASSIGN_TO_CONST_VALUE);
  if (vdomain == TD_Uniform && edomain == TD_Varying)
    SemanticError(  cg, loc, ERROR___ASSIGN_VARYING_TO_UNIFORM);
  if (ConvertType( cg, fExpr, vType, eType, &lExpr, InInit, 0)) {
    fExpr = lExpr;
  } else {
    if (vType != cg->UndefinedType && eType != cg->UndefinedType)
      SemanticError(  cg, loc, ERROR___ASSIGN_INCOMPATIBLE_TYPES);
  }
  base = GetBase(vType);
  if (IsScalar(vType)) {
    lop = ASSIGN_OP;
    subop = SUBOP__(base);
  } else if (IsVector(vType, &len)) {
    lop = ASSIGN_V_OP;
    subop = SUBOP_V(len, base);
  } else {
    lop = ASSIGN_GEN_OP;
    subop = SUBOP__(base);
  }
  lExpr = (Expr *) NewBinopSubNode(  cg, (opcode)lop, subop, fVar, fExpr);
  lExpr->type = vType;
  return lExpr;
} // NewSimpleAssignment

/*
 * NewSimpleAssignmentStmt() - Build a new simple assignment statement.
 *
 */

Stmt *NewSimpleAssignmentStmt(CgContext *cg, SourceLoc *loc, Expr *fVar, Expr *fExpr, int InInit)
{
  return (Stmt *) NewExprStmt( cg, loc, NewSimpleAssignment( cg, loc, fVar, fExpr, InInit));
} // NewSimpleAssignmentStmt

/*
 * NewCompoundAssignment() - Build a new simple assignment statement.
 *
 */

Stmt *NewCompoundAssignmentStmt( CgContext *cg, SourceLoc *loc, opcode op, Expr *fVar, Expr *fExpr)
{
  return (Stmt *) NewExprStmt( cg, loc, (Expr *) NewBinopNode( cg, op, fVar, fExpr));
} // NewCompoundAssignment

#if 000
/*
 * NewMaskedAssignment() - Add a new masked assignment to an assignment statement.
 *
 * "fStmt" is an assignment Stmt of the form: "expr" or "var = expr" ...
 * Returns a Stmt of the form: "var@@mask = expr" or "var@@mask = (var = expr)" ...
 *
 */

Stmt *NewMaskedAssignment(SourceLoc *loc, int mask, Expr *fExpr, Stmt *fStmt)
{
  int lop, subop, base, len;
  Expr *lExpr;
  Type *lType;
  char str[5];
  int ii, kk;
  
  if (!fExpr->IsLValue)
    SemanticError( cg, loc, ERROR___ASSIGN_TO_NON_LVALUE);
  if (ConvertType(static_cast< ExprStmt * >( fStmt )->expr, fExpr->type, static_cast< ExprStmt * >( fStmt )->expr->type,
                  &lExpr, 0)) {
    static_cast< ExprStmt * >( fStmt )->expr = lExpr;
  } else {
    SemanticError( cg, loc, ERROR___ASSIGN_INCOMPATIBLE_TYPES);
  }
  lType = fExpr->type;
  base = GetBase(lType);
  if (IsScalar(lType)) {
    SemanticError( cg, loc, ERROR___MASKED_ASSIGN_TO_VAR);
    lop = ASSIGN_OP;
    subop = SUBOP__(base);
  } else if (IsVector(lType, &len)) {
    if (len > 4) {
      SemanticError( cg, loc, ERROR_S_VECTOR_OPERAND_GR_4, "@@");
      len = 4;
    }
    if ((~0 << len) & mask) {
      kk = 0;
      for (ii = len; ii < 4; ii++) {
        if (mask & (1 << ii))
          str[kk++] = "xyzw"[ii];
      }
      str[kk] = '\0';
      SemanticError( cg, loc, ERROR_S_MASKED_ASSIGN_NON_EXISTENT, str);
      mask &= (1 << len) - 1;
    }
    lop = ASSIGN_MASKED_KV_OP;
    subop = SUBOP_KV(mask, len, base);
  } else {
    SemanticError( cg, loc, ERROR___MASKED_ASSIGN_TO_VAR);
    lop = ASSIGN_GEN_OP;
    subop = SUBOP__(base);
  }
  static_cast< ExprStmt * >( fStmt )->expr = (Expr *) NewBinopSubNode(lop, subop, fExpr, static_cast< ExprStmt * >( fStmt )->expr);
  static_cast< ExprStmt * >( fStmt )->expr->type = lType;
  return fStmt;
} // NewMaskedAssignment

/*
 * NewConditionalAssignment() - Add a new simple assignment to an assignment statement.
 *
 * "fStmt" is an assignment Stmt of the form: "expr" or "var = expr" ...
 * Returns a Stmt of the form: "var@@(cond) = expr" or "var@@(cond) = (var = expr)" ...
 *
 */

Stmt *NewConditionalAssignment(SourceLoc *loc, Expr *fcond, Expr *fExpr, Stmt *fStmt)
{
  int lop, subop, base, len, clen;
  Expr *lExpr;
  Type *lType, *ctype;
  
  if (!fExpr->IsLValue)
    SemanticError( cg, loc, ERROR___ASSIGN_TO_NON_LVALUE);
  if (ConvertType(static_cast< ExprStmt * >( fStmt )->expr, fExpr->type, static_cast< ExprStmt * >( fStmt )->expr->type,
                  &lExpr, 0)) {
    static_cast< ExprStmt * >( fStmt )->expr = lExpr;
  } else {
    SemanticError( cg, loc, ERROR___ASSIGN_INCOMPATIBLE_TYPES);
  }
  lType = fExpr->type;
  base = GetBase(lType);
  ctype = fcond->type;
  if (IsScalar(lType)) {
    if (!IsScalar(ctype))
      SemanticError( cg, loc, ERROR___SCALAR_BOOL_EXPR_EXPECTED);
    lop = ASSIGN_COND_OP;
    subop = SUBOP__(base);
  } else if (IsVector(lType, &len)) {
    if (len > 4) {
      SemanticError( cg, loc, ERROR_S_VECTOR_OPERAND_GR_4, "@@()");
      len = 4;
    }
    if (IsScalar(ctype)) {
      lop = ASSIGN_COND_SV_OP;
    } else {
      lop = ASSIGN_COND_V_OP;
      if (IsVector(ctype, &clen)) {
        if (clen != len)
          SemanticError( cg, loc, ERROR_S_VECTOR_OPERANDS_DIFF_LEN, "@@()");
      } else {
        SemanticError( cg, loc, ERROR_S_INVALID_CONDITION_OPERAND);
      }
    }
    subop = SUBOP_V(len, base);
  } else {
    lop = ASSIGN_COND_GEN_OP;
    subop = SUBOP__(base);
  }
  static_cast< ExprStmt * >( fStmt )->expr = (Expr *) NewTriopSubNode(lop, subop, fExpr, fcond, static_cast< ExprStmt * >( fStmt )->expr);
  static_cast< ExprStmt * >( fStmt )->expr->type = lType;
  return fStmt;
} // NewConditionalAssignment
#endif

/*************************************** misc: ******************************************/

/*
 * InitTempptr() -- initialize the tempptr fields of an Expr node -- designed
 *   be passed as an argument to ApplyToXXXX.  Sets tempptr[0] to its arg1
 *   argument and clears the rest of tempptr to NULL
 */

void InitTempptr( CgContext *cg, Expr *fExpr, void *arg1, int arg2)
{
  fExpr->tempptr[0] = arg1;
  memset(&fExpr->tempptr[1], 0,
         sizeof(fExpr->tempptr) - sizeof(fExpr->tempptr[0]));
} // InitTempptr

/********************************** Error checking: ******************************************/

/*
 * RecordErrorPos() - Record the fact that an error token was seen at source location LOC.
 *
 * Error tokens should be encountered in source line order, so we just add them to the end of
 * the list of error locations.
 */

void RecordErrorPos( CgContext *cg, SourceLoc *loc)
{
  ErrorLoc *eloc = new ErrorLoc();
  
  eloc->loc = *loc;
  eloc->hit = 0;
  eloc->next = NULL;
  if (cg->errorLocsFirst == NULL) {
    cg->errorLocsFirst = eloc;
  } else {
    cg->errorLocsLast->next = eloc;
  }
  cg->errorLocsLast = eloc;
  
  // If an error has been generated during parsing before the error
  // token was seen, then we mark this error token as being hit and
  // clear the error pending flag.
  
  if (cg->errorPending) {
    eloc->hit = 1;
    cg->errorPending = 0;
  }
} // RecordErrorPos

/*
 * MarkErrorPosHit() - Upon seeing an error at LOC, mark the
 * corresponding error location as hit.
 */

void MarkErrorPosHit( CgContext *cg, SourceLoc *loc)
{
  ErrorLoc *eloc;
  ErrorLoc *match = NULL;
  
  for (eloc = cg->errorLocsFirst; eloc != NULL; eloc = eloc->next) {
    if (loc->line <= eloc->loc.line) {
      match = eloc;
    } else {
      break;
    }
  }
  
  // If we found an error token location that comes after LOC, then
  // mark it as hit, otherwise, we haven't seen the error token yet,
  // so make a note that there's an error pending.
  
  if (match != NULL) {
    match->hit = 1;
  } else {
    cg->errorPending = 1;
  }
} // MakeErrorPosHit

/*
 * CheckAllErrorsGenerated() - Verify that at least one error was
 * generated in the appropriate location for each error token that
 * appeared in the program.
 */

void CheckAllErrorsGenerated( CgContext *cg )
{
  ErrorLoc *eloc;
  
  // Turn off ErrorMode so we can generate real errors for the
  // absence of errors.
  cg->options.errorMode = 0;
  
  for (eloc = cg->errorLocsFirst; eloc != NULL; eloc = eloc->next) {
    if (eloc->hit == 0) {
      SemanticError ( cg, &(eloc->loc), ERROR___NO_ERROR);
    }
  }
} // CheckAllErrorsGenerated

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////// End of support.c //////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

