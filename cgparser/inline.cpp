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
// inline.c
//

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>

#include "slglobals.h"

struct InlineFunData {
  Scope *superGlobalScope;
  Scope *globalScope;
  Scope *masterScope;
  Scope *calleeScope;
  int *nextFunInlineIndex;
  int *nextTempIndex;
  int funInlineIndex;
  int funIndex;
  StmtList statements;
  void *scratch;
};

static void AddComment( CgContext *cg, InlineFunData *fFunData, SourceLoc *loc, const char *str)
{
  Stmt *lStmt;
  
  lStmt = (Stmt *) NewCommentStmt(  cg, loc, str );
  AppendStatements(&fFunData->statements, lStmt);
} // AddComment

static Stmt *DuplicateStatementTree( CgContext *cg, Stmt *fStmt)
{
  Stmt *lStmt, *mStmt, *nStmt, *pStmt, *headStmt, *lastStmt;
  Expr *lExpr;
  
  headStmt = NULL;
  while (fStmt) {
    switch (fStmt->kind) {
      case EXPR_STMT:
        lExpr = DupExpr( cg, static_cast< ExprStmt * >( fStmt )->expr);
        lStmt = (Stmt *) NewExprStmt( cg, &fStmt->loc, lExpr);
        break;
      case IF_STMT:
        lExpr = DupExpr( cg, static_cast< IfStmt * >( fStmt )->cond);
        mStmt = DuplicateStatementTree( cg, static_cast< IfStmt * >( fStmt )->thenstmt);
        nStmt = DuplicateStatementTree( cg, static_cast< IfStmt * >( fStmt )->elsestmt);
        lStmt = (Stmt *) NewIfStmt( cg, &fStmt->loc, lExpr, mStmt, nStmt);
        break;
      case WHILE_STMT:
      case DO_STMT:
        lExpr = DupExpr( cg, static_cast< WhileStmt * >( fStmt )->cond);
        mStmt = DuplicateStatementTree( cg, static_cast< WhileStmt * >( fStmt )->body);
        lStmt = (Stmt *) NewWhileStmt( cg, &fStmt->loc, static_cast< WhileStmt * >( fStmt )->kind, lExpr, mStmt);
        break;
      case FOR_STMT:
        mStmt = DuplicateStatementTree( cg, static_cast< ForStmt * >( fStmt )->init);
        lExpr = DupExpr( cg, static_cast< ForStmt * >( fStmt )->cond);
        nStmt = DuplicateStatementTree( cg, static_cast< ForStmt * >( fStmt )->step);
        pStmt = DuplicateStatementTree( cg, static_cast< ForStmt * >( fStmt )->body);
        lStmt = (Stmt *) NewForStmt( cg, &fStmt->loc, mStmt, lExpr, nStmt, pStmt);
        break;
      case BLOCK_STMT:
        mStmt = DuplicateStatementTree( cg, static_cast< BlockStmt * >( fStmt )->body);
        lStmt = (Stmt *) NewBlockStmt( cg, &fStmt->loc, mStmt);
        break;
      case RETURN_STMT:
        lExpr = DupExpr( cg, static_cast< ExprStmt * >( fStmt )->expr);
        lStmt = (Stmt *) NewReturnStmt( cg, &fStmt->loc, NULL, lExpr);
        break;
      case DISCARD_STMT:
        lExpr = DupExpr( cg, static_cast< DiscardStmt * >( fStmt )->cond);
        lStmt = (Stmt *) NewDiscardStmt( cg, &fStmt->loc, lExpr);
        break;
      case COMMENT_STMT:
        lStmt = (Stmt *) NewCommentStmt( cg, &fStmt->loc,
                                        cg->GetString(static_cast< CommentStmt * >( fStmt )->str));
        break;
      default:
        lStmt = fStmt;
        assert(!"DuplicateStatementTree() - not yet finished");
        break;
    }
    if (headStmt) {
      lastStmt->next = lStmt;
    } else {
      headStmt = lStmt;
    }
    lastStmt = lStmt;
    fStmt = fStmt->next;
  }
  return headStmt;
} // DuplicateStatementTree

/*
 * ConvertReturnStatement() - Convert return statements into assignments.
 *
 */

static Stmt *ConvertReturnStatement( CgContext *cg, Stmt *fStmt, void *arg1, int arg2)
{
  Stmt *lStmt;
  Symbol *retSymb;
  Expr *lExpr;
  
  if (fStmt) {
    switch (fStmt->kind) {
      case RETURN_STMT:
        if (arg1) {
          retSymb = (Symbol *) arg1;
          lExpr = (Expr *) NewSymbNode( cg, VARIABLE_OP, retSymb);
          lStmt = NewSimpleAssignmentStmt( cg, cg->pLastSourceLoc, lExpr, static_cast< ExprStmt * >( fStmt )->expr, 1);
        } else {
          lStmt = NULL;
        }
        break;
      default:
        lStmt = fStmt;
        break;
    }
  } else {
    lStmt = NULL;
  }
  return lStmt;
} // ConvertReturnStatement

#define NOT_DUPLICATED          0
#define ALREADY_DUPLICATED      1
#define IN_MASTER_SCOPE         2
#define IN_GLOBAL_SCOPE         3
#define IN_SUPER_GLOBAL_SCOPE   4

/*
 * ConvertLocalReferences() - Convert return statements into assignments.  Assume that this
 *         can be done in place since we're working with a copy of the original symb structs.
 *
 */

static Expr *ConvertLocalReferences( CgContext *cg, Expr *fExpr, void *arg1, int arg2)
{
  InlineFunData *lFunData = (InlineFunData *) arg1;
  Expr *lExpr = fExpr;
  Symbol *lSymb, *nSymb;
  Type *lType;
  int qualifiers;
  Atom name;
  
  lExpr = fExpr;
  if (fExpr) {
    switch (fExpr->kind) {
      case SYMB_N:
        lSymb = static_cast< Symb * >( lExpr )->symbol;
        if (static_cast< Symb * >( fExpr )->op == VARIABLE_OP && lSymb->kind == SK_Variable) {
          switch (lSymb->flags) {
            case NOT_DUPLICATED:
              name = GetNumberedAtom( cg, cg->GetString(lSymb->name), lFunData->funInlineIndex, 4, '-');
              if (LookupLocalSymbol( cg, lFunData->masterScope, name)) {
                InternalError( cg, cg->pLastSourceLoc, 9999, "Name \"%s\"-%04d shouldn't be defined, but is!",
                              cg->GetString(lSymb->name), lFunData->funInlineIndex);
                if (cg->options.dumpParseTree) {
                  InternalError( cg, cg->pLastSourceLoc, 9999, "*** Scope %d definitions ***",
                                lFunData->masterScope->level);
                  Writer wr( cg, std::cout );
                  wr.WriteSymbolTree( lFunData->masterScope->symbols);
                  InternalError( cg, cg->pLastSourceLoc, 9999, "*** End of Scope %d ***",
                                lFunData->masterScope->level);
                }
              }
              // Remove qualifiers if any present:
              lType = lSymb->type;
              qualifiers = GetQualifiers(lType);
              if (qualifiers) {
                lType = DupType(lType);
                lType->qualifier = TQ_None;
              }
              nSymb = DefineVar( cg, &lSymb->loc, lFunData->masterScope, name, lType);
              nSymb->properties = lSymb->properties;
              nSymb->flags = IN_MASTER_SCOPE;
              lSymb->tempptr = (void *) nSymb;
              lSymb->flags = ALREADY_DUPLICATED;
              break;
            case IN_MASTER_SCOPE:
            case IN_GLOBAL_SCOPE:
            case IN_SUPER_GLOBAL_SCOPE:
              nSymb = lSymb;
              break;
            case ALREADY_DUPLICATED:
              nSymb = (Symbol *) lSymb->tempptr;
              break;
            default:
              FatalError( cg, "Bad scope in ConvertLocalReferences()");
              break;
          }
          static_cast< Symb * >( lExpr )->symbol = nSymb;
        }
        break;
      case CONST_N:
      case UNARY_N:
      case BINARY_N:
      case TRINARY_N:
        break;
      case DECL_N:
      default:
        assert(!"bad kind to ConvertLocalReferences()");
        break;
    }
  }
  return lExpr;
} // ConvertLocalReferences

/*
 * ExpandInlineFunction() - Expand a function inline by duplicating it's statements.
 *
 */

static void ExpandInlineFunction( CgContext *cg, InlineFunData *fFunData, Symbol *funSymb, Symbol *retSymb, Expr *fActuals)
{
  Stmt *body;
  Stmt *nStmt;
  Expr *lExpr, *lActual;
  Symbol *lFormal;
  StmtList newStmts;
  
  // Bump function counter:
  
  fFunData->funInlineIndex = (*fFunData->nextFunInlineIndex)++;
  
  // Duplicate the body of the function:
  
  body = DuplicateStatementTree( cg, funSymb->details.fun.statements);
  
  // Change return statements into assignments:
  
  body = PostApplyToStatements( cg, ConvertReturnStatement, body, retSymb, 0);
  
  // Add numbered copies of expanded function's local variables to current scope,
  // and convert any references in copied body of function to new symbols:
  
  SetSymbolFlagsList(cg->scopeList/*globalScope*/, NOT_DUPLICATED);
  SetSymbolFlags(fFunData->masterScope->symbols, IN_MASTER_SCOPE);
  SetSymbolFlags(fFunData->globalScope->symbols, IN_GLOBAL_SCOPE);
  SetSymbolFlags(fFunData->superGlobalScope->symbols, IN_SUPER_GLOBAL_SCOPE);
  
  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  // !!! BEGIN !!! Temporary patch for "texobj" parameters!!! Don't assign!!!
#define TB_TEXOBJ_FP30   (TB_FirstUser + 2)
  lFormal = funSymb->details.fun.params;
  lActual = fActuals;
  while (lFormal) {
    if (GetBase(lFormal->type) == TB_TEXOBJ_FP30) {
      assert(static_cast< Binary * >( lActual )->left->kind == SYMB_N);
      lFormal->tempptr = (void *) static_cast< Symb * >( static_cast< Binary * >( lActual )->left )->symbol;
      lFormal->flags = ALREADY_DUPLICATED;
    }
    lFormal = lFormal->next;
    lActual = static_cast< Binary * >( lActual )->right;
  }
  // !!! END !!! Temporary patch for "texobj" parameters!!! Don't assign!!!
  //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  PostApplyToExpressions( cg, ConvertLocalReferences, body, fFunData, 0);
  
  // Assign actual parameter expressions to parameters:
  
  newStmts.first = newStmts.last = NULL;
  lFormal = funSymb->details.fun.params;
  lActual = fActuals;
  while (lFormal) {
    assert(lActual->kind == BINARY_N);
    assert(static_cast< Binary * >( lActual )->op == FUN_ARG_OP);
    if (((GetQualifiers(lFormal->type) == TQ_In) ||
         !(GetQualifiers(lFormal->type) == TQ_Out)) &&
        //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        // !!! BEGIN !!! Temporary patch for "texobj" parameters!!! Don't assign!!!
        (GetBase(lFormal->type) != TB_TEXOBJ_FP30) &&
        // !!! Temporary patch for "texobj" parameters!!! Don't assign!!!
        //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        1)
    {
      lExpr = (Expr *) NewSymbNode( cg, VARIABLE_OP, lFormal);
      lExpr = ConvertLocalReferences( cg, lExpr, fFunData, 0);
      nStmt = NewSimpleAssignmentStmt(  cg, cg->pLastSourceLoc, lExpr, static_cast< Binary * >( lActual )->left, 1);
      AppendStatements(&newStmts, nStmt);
    }
    lFormal = lFormal->next;
    lActual = static_cast< Binary * >( lActual )->right;
  }
  AppendStatements(&newStmts, body);
  
  // Build assignment statements to copy "out" param's final values to actual parameters:
  
  lFormal = funSymb->details.fun.params;
  lActual = fActuals;
  while (lFormal) {
    if (GetQualifiers(lFormal->type) & TQ_Out) {
      lExpr = (Expr *) NewSymbNode( cg, VARIABLE_OP, lFormal);
      lExpr = ConvertLocalReferences( cg, lExpr, fFunData, 0);
      nStmt = NewSimpleAssignmentStmt( cg, cg->pLastSourceLoc, static_cast< Binary * >( lActual )->left, lExpr, 0);
      AppendStatements(&newStmts, nStmt);
    }
    lFormal = lFormal->next;
    lActual = static_cast< Binary * >( lActual )->right;
  }
  
  // Recursively expands calls in newly inlined finction:
  
  body = ExpandInlineFunctionCalls( cg, funSymb->details.fun.locals, newStmts.first, fFunData);
  
  // Do some more stuff here...
  
  // Append the new statements:
  
  AppendStatements(&fFunData->statements, body);
  
} // ExpandInlineFunction

/*
 * ExpandInlineFunctionCallsNode() - Expand inline function calls in an expression.
 *
 */

static Expr *ExpandInlineFunctionCallsNode( CgContext *cg, Expr *fExpr, void *arg1, int arg2)
{
#define TEMP_ROOT      "$temp"
  
  InlineFunData *lFunData = (InlineFunData *) arg1;
  Symbol *lSymb, *retSymb;
  Expr *lExpr;
  Type *funType, *retType;
  Atom vname;
  
  switch (fExpr->kind) {
    case DECL_N:
    case SYMB_N:
    case CONST_N:
    case UNARY_N:
      break;
    case BINARY_N:
      if (static_cast< Binary * >( fExpr )->op == FUN_CALL_OP) {
        lExpr = static_cast< Binary * >( fExpr )->left;
        if (lExpr->kind == SYMB_N) {
          lSymb = static_cast< Symb * >( lExpr )->symbol;
          assert(IsFunction(lSymb));
          if (IsInline(lSymb)) {
            funType = lSymb->type;
            retType = static_cast< TypeFunction * >( funType )->rettype;
            if (!IsVoid(retType)) {
              vname = GetNumberedAtom( cg, TEMP_ROOT, (*lFunData->nextTempIndex)++, 4, '\0');
              retSymb = DefineVar( cg, &lSymb->loc, lFunData->masterScope, vname, retType);
            } else {
              retSymb = NULL;
            }
            if (cg->options.comments) {
              AddComment( cg, lFunData, cg->pLastSourceLoc, "Begin inline function");
              AddComment( cg, lFunData, cg->pLastSourceLoc, cg->GetString(lSymb->name));
            }
            ExpandInlineFunction( cg, lFunData, lSymb, retSymb, static_cast< Binary * >( fExpr )->right);
            if (cg->options.comments) {
              AddComment( cg, lFunData, cg->pLastSourceLoc, "End inline function");
              AddComment( cg, lFunData, cg->pLastSourceLoc, cg->GetString(lSymb->name));
            }
            if (retSymb) {
              fExpr = (Expr *) NewSymbNode( cg, VARIABLE_OP, retSymb);
            } else {
              fExpr = NULL; // function returning void
            }
          } else {
            // Not done yet:  Must continue to traverse call tree to find other
            // functions that are called and inline their calls as needed.
            // Or some such stuff...
          }
        }
      }
    case TRINARY_N:
      break;
    default:
      assert(!"bad kind to ExpandInlineFunctionCallsNode()");
      break;
  }
  return fExpr;
} // ExpandInlineFunctionCallsNode

/*
 * ExpandInlineFunctionCallsStmt() - Expand inline function calls in a statement.
 *
 */

static Stmt *ExpandInlineFunctionCallsStmt( CgContext *cg, Stmt *fStmt, void *arg1, int arg2)
{
  InlineFunData *lFunData = (InlineFunData *) arg1;
  
  lFunData->statements.first = NULL;
  lFunData->statements.last = NULL;
  PostApplyToExpressionsLocal( cg, ExpandInlineFunctionCallsNode, fStmt, arg1, arg2);
  if (lFunData->statements.first) {
    lFunData->statements.last->next = fStmt;
    fStmt = lFunData->statements.first;
  }
  lFunData->statements.first = NULL;
  lFunData->statements.last = NULL;
  return fStmt;
} // ExpandInlineFunctionCallsStmt

/*
 * ExpandInlineFunctionCalls() - Recursively walk a program'm call graph from main
 *         expanding function calls that have the attribute "inline".
 *
 * Assumes no recursion in inlined functions.
 *
 */

Stmt *ExpandInlineFunctionCalls( CgContext *cg, Scope *fscope, Stmt *body, InlineFunData *fFunData)
{
  int funcount = 0, tempcount = 0;
  InlineFunData lFunData;
  Stmt *lStmt;
  
  if (fFunData) {
    lFunData.superGlobalScope = fFunData->superGlobalScope;
    lFunData.globalScope = fFunData->globalScope;
    lFunData.masterScope = fFunData->masterScope;
    lFunData.calleeScope = fscope;
    lFunData.nextFunInlineIndex = fFunData->nextFunInlineIndex;
    lFunData.nextTempIndex = fFunData->nextTempIndex;
    lFunData.funInlineIndex = fFunData->funInlineIndex;
  } else {
    lFunData.superGlobalScope = fscope->parent->parent;
    lFunData.globalScope = fscope->parent;
    lFunData.masterScope = fscope;
    lFunData.calleeScope = NULL;
    lFunData.nextFunInlineIndex = &funcount;
    lFunData.nextTempIndex = &tempcount;
    lFunData.funInlineIndex = 999;
  }
  lFunData.funIndex = 0;
  lFunData.statements.first = NULL;
  lFunData.statements.last = NULL;
  lFunData.scratch = NULL;
  
  lStmt = PostApplyToStatements( cg, ExpandInlineFunctionCallsStmt, body, &lFunData, 0);
  return lStmt;
  
} // ExpandInlineFunctionCalls

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////// End of inline.c ///////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
