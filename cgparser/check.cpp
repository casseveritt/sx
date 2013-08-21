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
// check.c
//

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "slglobals.h"
static int CheckFunctionDefinition( CgContext *cg, Scope *fScope, Symbol *funSymb, int IsProgram);

#define NOT_CHECKED     0
#define BEING_CHECKED   1
#define ALREADY_CHECKED 2

/*
 * CheckSymbolTree()
 *
 */

static int CheckSymbolTree( CgContext *cg, Scope *fScope, Symbol *fSymb, int IsProgram)
{
  int count = 0;
  
  if (fSymb) {
    cg->theHal->CheckDefinition(&fSymb->loc, fSymb->name, fSymb->type);
    count += CheckSymbolTree( cg, fScope, fSymb->left, IsProgram);
    count += CheckSymbolTree( cg, fScope, fSymb->right, IsProgram);
  }
  return count;
} // CheckSymbolTree

/*
 * CheckParamsAndLocals() - Check this functions format parameters and local variables
 *         for unallowed things.
 */

static int CheckParamsAndLocals( CgContext *cg, Symbol *funSymb, int IsProgram)
{
  Scope *lScope;
  int count = 0;
  
  lScope = funSymb->details.fun.locals;
  count += CheckSymbolTree( cg, lScope, lScope->symbols, IsProgram);
  return count;
} // CheckParamsAndLocals

/*
 * BuildProgramReturnAssignments() - Insert a series of assignment statements before each return
 *         statement to set the values of the program's result for these memeners.  (Should only
 *         be applied to the main program.)  Deletes the return statement.
 */

struct BuildReturnAssignments {
  Scope *globalScope;
  Symbol *program;
};

static Stmt *BuildProgramReturnAssignments( CgContext *cg, Stmt *fStmt, void *arg1, int arg2)
{
  struct BuildReturnAssignments *lstr;
  Symbol *program, *lSymb, *voutVar, *outSymb, *retSymb;
  Type *lType, *rettype;
  Expr *lExpr, *rexpr, *returnVar, *outputVar;
  Scope *lScope, *gScope, *voutScope;
  Stmt *lStmt, *stmtlist;
  int len;
	Atom lname;
  
  if (fStmt->kind == RETURN_STMT) {
    lstr = (struct BuildReturnAssignments *) arg1;
    gScope = lstr->globalScope;
    program = lstr->program;
    lType = program->type;
    rettype = static_cast< TypeFunction * >( lType )->rettype;
    TypeCategory category = rettype->category;
    if (IsVoid(rettype)) {
      fStmt = NULL;
    } else {
      if (category == TC_Struct) {
        stmtlist = NULL;
        voutVar = cg->theHal->varyingOut;
        voutScope = static_cast< TypeStruct * >( voutVar->type )->members;
        lScope = static_cast< TypeStruct * >( rettype )->members;
        lSymb = lScope->symbols;
        while (lSymb) {
          // Create an assignment statement of the bound variable to the $vout member:
          lname = lSymb->details.var.semantics.IsValid() ? lSymb->details.var.semantics : lSymb->name;
          outSymb = LookupLocalSymbol(cg, voutScope, lname);
          retSymb = LookupLocalSymbol(cg, lScope, lSymb->name);
          if (outSymb && retSymb) {
            // outSymb may not be in the symbol table if it's a "hidden" register.
            returnVar = DupExpr( cg, static_cast< ReturnStmt * >( fStmt )->expr);
            outputVar = (Expr *) NewSymbNode( cg, VARIABLE_OP, voutVar);
            lExpr = GenMemberReference( cg, outputVar, outSymb);
            rexpr = GenMemberReference( cg, returnVar, retSymb);
            if (IsScalar(lSymb->type) || IsVector(lSymb->type, &len)) {
              lStmt = NewSimpleAssignmentStmt( cg, &program->loc, lExpr, rexpr, 0);
              stmtlist = ConcatStmts(stmtlist, lStmt);
            } else {
              FatalError( cg, "Return of unsupported type");
              // xxx
            }
          }
          lSymb = lSymb->next;
        }
				delete fStmt;
        fStmt = stmtlist;
      } else {
        // Already reported:
        // SemanticError(&program->loc, ERROR_S_PROGRAM_MUST_RETURN_STRUCT,
        //               cg->GetString(program->name));
      }
    }
  }
  return fStmt;
} // BuildProgramReturnAssignments

/*
 * CheckNodeForUndefinedFunctions() - Check an expression nodefor calls to undefined functions.
 *
 */

static Expr *CheckNodeForUndefinedFunctions( CgContext *cg, Expr *fExpr, void *arg1, int arg2)
{
  Symbol *lSymb;
  Expr *lExpr;
  int *count = (int *) arg1;
  
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
          if (IsFunction(lSymb)) {
            if (!(lSymb->properties & SYMB_IS_DEFINED)) {
              SemanticError( cg, cg->pLastSourceLoc, ERROR_S_CALL_UNDEF_FUN,
                            cg->GetString(lSymb->name));
              count++;
            } else {
              if (lSymb->flags == BEING_CHECKED) {
                SemanticError( cg, cg->pLastSourceLoc, ERROR_S_RECURSION,
                              cg->GetString(lSymb->name));
                count++;
              } else {
                CheckFunctionDefinition( cg, NULL, lSymb, 0);
              }
            }
          }
        }
      }
      break;
    case TRINARY_N:
      break;
    default:
      assert(!"bad kind to CheckNodeForUndefinedFunctions()");
      break;
  }
  return fExpr;
} // CheckNodeForUndefinedFunctions

/*
 * CheckExpressionForUndefinedFunctions() - Check an expression for calls to undefined functions.
 *
 */

static Expr *CheckExpressionForUndefinedFunctions(CgContext *cg, Expr *fExpr, void *arg1, int arg2)
{
  PostApplyToNodes( cg, CheckNodeForUndefinedFunctions, fExpr, arg1, arg2);
  return fExpr;
} // CheckExpressionForUndefinedFunctions

/*
 * CheckNodeForUnsupportedOperators() - Check a node for operators not supported
 *         in the target profile.
 */

static Expr *CheckNodeForUnsupportedOperators( CgContext *cg, Expr *fExpr, void *arg1, int arg2)
{
  int *count = (int *) arg1;
  
  switch (fExpr->kind) {
    case DECL_N:
    case SYMB_N:
    case CONST_N:
      break;
    case UNARY_N:
      if (!cg->theHal->IsValidOperator(cg->pLastSourceLoc, opcode_atom[static_cast< Unary * >( fExpr )->op], static_cast< Unary * >( fExpr )->op,
                                       static_cast< Unary * >( fExpr )->subop))
      {
        *count++;
      }
      break;
    case BINARY_N:
      if (!cg->theHal->IsValidOperator(cg->pLastSourceLoc, opcode_atom[static_cast< Binary * >( fExpr )->op], static_cast< Binary * >( fExpr )->op,
                                       static_cast< Binary * >( fExpr )->subop))
      {
        *count++;
      }
      break;
    case TRINARY_N:
      if (!cg->theHal->IsValidOperator(cg->pLastSourceLoc, opcode_atom[static_cast< Trinary * >( fExpr )->op], static_cast< Trinary * >( fExpr )->op,
                                       static_cast< Trinary * >( fExpr )->subop))
      {
        *count++;
      }
      break;
    default:
      assert(!"bad kind to CheckNodeForUnsupportedOperators()");
      break;
  }
  return fExpr;
} // CheckNodeForUnsupportedOperators

/*
 * CheckForUnsupportedVariables() - Check for references to object that are not supported
 *         by the target profile.
 */

static Expr *CheckForUnsupportedVariables( CgContext *cg, Expr *fExpr, void *arg1, int arg2)
{
  int *count = (int *) arg1;
  
	count; // now variable is not unused
  switch (fExpr->kind) {
    case SYMB_N:
      if (static_cast< Symb * >( fExpr )->op == VARIABLE_OP) {
        ///lSymb = static_cast< Symb * >( fExpr )->symbol;
      }
      break;
    case DECL_N:
    case CONST_N:
    case UNARY_N:
    case BINARY_N:
    case TRINARY_N:
      break;
    default:
      assert(!"bad kind to CheckForUnsupportedVariables()");
      break;
  }
  return fExpr;
} // CheckForUnsupportedVariables

/*
 * CheckForGlobalUniformReferences() - Check for references to previously unreferenced non-static
 *         uniform global variables.  These must have explicit or implied semantics.  Add them to
 *         to the $uniform connector and insert an initialization statement at the start of main.
 */

static Expr *CheckForGlobalUniformReferences( CgContext *cg, Expr *fExpr, void *arg1, int arg2)
{
  int category, domain, qualifiers;
  Symbol *lSymb;
  Type *lType;
  int gname;
  
  switch (fExpr->kind) {
    case SYMB_N:
      if (static_cast< Symb * >( fExpr )->op == VARIABLE_OP) {
        lSymb = static_cast< Symb * >( fExpr )->symbol;
        lType = static_cast< Symb * >( fExpr )->type;
        category = GetCategory(lType);
        domain = GetDomain(lSymb->type);
        qualifiers = GetQualifiers(lSymb->type);
        if (lSymb->properties & SYMB_NEEDS_BINDING) {
          // This is a non-static global and has not yet been bound
          gname = 0;
          BindDefaultSemantic( cg, lSymb, category, gname);
        }
      }
      break;
    default:
      break;
  }
  return fExpr;
} // CheckForGlobalUniformReferences

/*
 * CheckForReturnStmts() - Issue an error if a return statement is encountered.
 *
 */

static Stmt *CheckForReturnStmts( CgContext *cg, Stmt *fStmt, void *arg1, int arg2)
{
  if (fStmt->kind == RETURN_STMT)
    SemanticError( cg, &fStmt->loc, ERROR___RETURN_NOT_LAST);
  return fStmt;
} // CheckForReturnStmts

/*
 * CheckForUnsupportedStatements() - Issue an error if an unsupported statement is encountered.
 *
 */

static Stmt *CheckForUnsupportedStatements( CgContext *cg, Stmt *fStmt, void *arg1, int arg2)
{
	int * arg1i = (int *)arg1; // Fixed C++ error. -Cass
  if (fStmt) {
    if (!cg->theHal->CheckStatement(&fStmt->loc, fStmt))
      ++arg1i;
  }
  return fStmt;
} // CheckForUnsupportedStatements

/*
 * BindUnboundUniformMembers() - Bind any members that are currently unbound.  Must be a
 *         uniform pseudo-connector.
 */

static void BindUnboundUniformMembers( CgContext *cg, SymbolList *fList)
{
  Symbol *lSymb;
  Binding *lBind;
  
  while (fList != NULL) {
    lSymb = fList->symb;
    if (lSymb) {
      lBind = lSymb->details.var.bind;
      if (lBind && !(lBind->properties & BIND_IS_BOUND)) {
        if (!cg->theHal->BindUniformUnbound(&lSymb->loc, lSymb, lBind)) {
          SemanticWarning( cg, &lSymb->loc, WARNING_S_CANT_BIND_UNIFORM_VAR,
                          cg->GetString(lSymb->name));
        }
      }
    }
    fList = fList->next;
  }
} // BindUnboundUniformMembers

/*
 * CheckFunctionDefinition()
 *
 */

static int CheckFunctionDefinition( CgContext *cg, Scope *fScope, Symbol *funSymb, int IsProgram)
{
  int count = 0;
  Stmt *lStmt;
  
  if (funSymb->flags == NOT_CHECKED) {
    funSymb->flags = BEING_CHECKED;
    lStmt = funSymb->details.fun.statements;
    CheckParamsAndLocals( cg, funSymb, IsProgram);
    if (IsProgram) {
      struct BuildReturnAssignments lstr;
      
      lstr.globalScope = fScope;
      lstr.program = funSymb;
      lStmt = PreApplyToStatements( cg, BuildProgramReturnAssignments, lStmt, &lstr, 0);
    }
    ApplyToTopExpressions( cg, CheckExpressionForUndefinedFunctions, lStmt, &count, 0);
    PostApplyToExpressions( cg, CheckNodeForUnsupportedOperators, lStmt, &count, 0);
    PostApplyToExpressions( cg, CheckForUnsupportedVariables, lStmt, &count, 0);
    PostApplyToExpressions( cg, CheckForGlobalUniformReferences, lStmt, 0, 0);
    PreApplyToStatements( cg, CheckForUnsupportedStatements, lStmt, &count, 0);
    if (cg->theHal->GetCapsBit(CAPS_RESTRICT_RETURNS)) {
      while (lStmt) {
        if (lStmt->next)
          CheckForReturnStmts(cg, lStmt, NULL, 0);
        PostApplyToChildStatements( cg, CheckForReturnStmts, lStmt, NULL, 0);
        lStmt = lStmt->next;
      }
    }
    funSymb->flags = ALREADY_CHECKED;
  }
  return count;
} // CheckFunctionDefinition

/*
 * CheckFunctionDefinitions() - Walk a function and check for errors:
 *     1. see if any functions it calls aren't defined,
 *     2. detect recursion,
 *     3. detect early return statements,
 *     4. check for unsupported operators in the target profile.
 *     5. Build uniform and varying pseudo structs: $vin. $vout. $main, $global
 */

int CheckFunctionDefinitions( CgContext *cg, SourceLoc *loc, Scope *fScope, Symbol *program)
{
  int count;
  
  SetSymbolFlagsList(fScope, NOT_CHECKED);
  count = CheckFunctionDefinition( cg, fScope, program, 1);
  SetStructMemberOffsets( cg, cg->theHal->varyingIn->type);
  SetStructMemberOffsets( cg, cg->theHal->varyingOut->type);
  BindUnboundUniformMembers( cg, cg->theHal->uniformParam);
  BindUnboundUniformMembers( cg, cg->theHal->uniformGlobal);
  return count;
} // CheckFunctionDefinitions

///////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////// Check Connector Usage ////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * CheckConnectorUsage() - Check connector usage for illegal references.
 *
 */

static Expr *CheckConnectorUsage( CgContext *cg, Expr *fExpr, void *arg1, int arg2)
{
  int WeAreWriting = arg2;
  Symbol *lSymb;
  Expr *lExpr;
  Binding *lBind;
  
  lExpr = fExpr;
  if (fExpr) {
    switch (fExpr->kind) {
      case DECL_N:
				break;
      case SYMB_N:
        lSymb = static_cast< Symb * >( lExpr )->symbol;
        if (lSymb->properties & SYMB_IS_CONNECTOR_REGISTER) {
          if (!(lSymb->properties & (SYMB_CONNECTOR_CAN_WRITE | SYMB_CONNECTOR_CAN_READ))) {
            SemanticError( cg, cg->pLastSourceLoc, ERROR_S_CMEMBER_NOT_VISIBLE,
                          cg->GetString(lSymb->name));
          } else {
            if (WeAreWriting) {
              if (!(lSymb->properties & SYMB_CONNECTOR_CAN_WRITE)) {
                SemanticError( cg, cg->pLastSourceLoc, ERROR_S_CMEMBER_NOT_WRITABLE,
                              cg->GetString(lSymb->name));
              }
            } else {
              if (!(lSymb->properties & SYMB_CONNECTOR_CAN_READ)) {
                SemanticError( cg, cg->pLastSourceLoc, ERROR_S_CMEMBER_NOT_READABLE,
                              cg->GetString(lSymb->name));
              }
            }
          }
        }
        break;
      case CONST_N:
        break;
      case UNARY_N:
        static_cast< Unary * >( fExpr )->arg = CheckConnectorUsage( cg, static_cast< Unary * >( fExpr )->arg, arg1, arg2);
        break;
      case BINARY_N:
        switch (static_cast< Binary * >( fExpr )->op) {
          case MEMBER_SELECTOR_OP:
            lExpr = static_cast< Binary * >( fExpr )->right;
            if (lExpr && lExpr->kind == SYMB_N && WeAreWriting) {
              // Mark connector registers that are written.
              lSymb = static_cast< Symb * >( lExpr )->symbol;
              lBind = lSymb->details.var.bind;
              if (lBind)
                lBind->properties |= BIND_WAS_WRITTEN;
            }
            static_cast< Binary * >( fExpr )->left = CheckConnectorUsage( cg, static_cast< Binary * >( fExpr )->left, arg1, arg2);
            static_cast< Binary * >( fExpr )->right = CheckConnectorUsage( cg, static_cast< Binary * >( fExpr )->right, arg1, arg2);
            lExpr = fExpr;
            break;
          case ASSIGN_OP:
          case ASSIGN_V_OP:
          case ASSIGN_GEN_OP:
          case ASSIGN_MASKED_KV_OP:
            static_cast< Binary * >( fExpr )->left = CheckConnectorUsage( cg, static_cast< Binary * >( fExpr )->left, arg1, 1);
            static_cast< Binary * >( fExpr )->right = CheckConnectorUsage( cg, static_cast< Binary * >( fExpr )->right, arg1, 0);
            break;
          case FUN_ARG_OP:
            arg2 = SUBOP_GET_MASK(static_cast< Binary * >( fExpr )->subop) & 2 ? 1 : 0;
            static_cast< Binary * >( fExpr )->left = CheckConnectorUsage( cg, static_cast< Binary * >( fExpr )->left, arg1, arg2);
            static_cast< Binary * >( fExpr )->right = CheckConnectorUsage( cg, static_cast< Binary * >( fExpr )->right, arg1, 0);
            break;
          default:
            static_cast< Binary * >( fExpr )->left = CheckConnectorUsage( cg, static_cast< Binary * >( fExpr )->left, arg1, arg2);
            static_cast< Binary * >( fExpr )->right = CheckConnectorUsage( cg, static_cast< Binary * >( fExpr )->right, arg1, arg2);
            break;
        }
        break;
      case TRINARY_N:
        switch (static_cast< Binary * >( fExpr )->op) {
          case ASSIGN_COND_OP:
          case ASSIGN_COND_V_OP:
          case ASSIGN_COND_SV_OP:
          case ASSIGN_COND_GEN_OP:
            static_cast< Trinary * >( fExpr )->arg1 = CheckConnectorUsage( cg, static_cast< Trinary * >( fExpr )->arg1, arg1, 1);
            static_cast< Trinary * >( fExpr )->arg2 = CheckConnectorUsage( cg, static_cast< Trinary * >( fExpr )->arg2, arg1, 0);
            static_cast< Trinary * >( fExpr )->arg3 = CheckConnectorUsage( cg, static_cast< Trinary * >( fExpr )->arg3, arg1, 0);
            break;
          default:
            static_cast< Trinary * >( fExpr )->arg1 = CheckConnectorUsage( cg, static_cast< Trinary * >( fExpr )->arg1, arg1, arg2);
            static_cast< Trinary * >( fExpr )->arg2 = CheckConnectorUsage( cg, static_cast< Trinary * >( fExpr )->arg2, arg1, arg2);
            static_cast< Trinary * >( fExpr )->arg3 = CheckConnectorUsage( cg, static_cast< Trinary * >( fExpr )->arg3, arg1, arg2);
            break;
        }
        break;
      default:
        FatalError( cg, "bad kind to CheckConnectorUsage()");
        break;
    }
  } else {
    lExpr = NULL;
  }
  return lExpr;
} // CheckConnectorUsage

/*
 * CheckConnectorUsageMain() - Check connector usage for illegal references.
 *
 */

void CheckConnectorUsageMain( CgContext *cg, Symbol *program, Stmt *fStmt)
{
  Symbol *outConn, *lSymb;
  Type *cType;
  int len, cid;
  Binding *lBind;
  
  outConn = cg->theHal->varyingOut;
  if (!outConn || !outConn->type)
    return;
  cType = outConn->type;
  cid = static_cast< TypeStruct * >( cType )->variety;
  len = cg->theHal->GetConnectorRegister(cid, 1, Atom(), NULL);
  ApplyToTopExpressions( cg, CheckConnectorUsage, fStmt, NULL, 0);
  lSymb = static_cast< TypeStruct * >( cg->theHal->varyingOut->type )->members->symbols;
  // This doesn't work!  The output value is always written by the return statement!  RSG
  while (lSymb) {
    lBind = lSymb->details.var.bind;
    if (lBind) {
      if ((lBind->properties & BIND_WRITE_REQUIRED) &&
          !(lBind->properties & BIND_WAS_WRITTEN))
      {
        SemanticWarning( cg, &program->loc, WARNING_S_CMEMBER_NOT_WRITTEN,
                        cg->GetString( lBind->name ));
      }
    }
    lSymb = lSymb->next;
  }
} // CheckConnectorUsageMain

///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * CheckForHiddenVaryingReferences() - Check for references to varying l-values with the
 *         "hidden" semantic bit set.  These shouldn't be referenced in this profile.
 */

Expr *CheckForHiddenVaryingReferences( CgContext *cg, Expr *fExpr, void *arg1, int arg2)
{
  Binding *lBind;
  Symbol *lSymb;
  Type *lType;
  
  switch (fExpr->kind) {
    case SYMB_N:
      if (static_cast< Symb * >( fExpr )->op == VARIABLE_OP || static_cast< Symb * >( fExpr )->op == MEMBER_OP) {
        lSymb = static_cast< Symb * >( fExpr )->symbol;
        if (lSymb->kind == SK_Variable) {
          lType = static_cast< Symb * >( fExpr )->type;
          lBind = lSymb->details.var.bind;
          if (lBind && lBind->properties & BIND_HIDDEN) {
            SemanticError( cg, cg->pLastSourceLoc, ERROR_SS_VAR_SEMANTIC_NOT_VISIBLE,
                          cg->GetString(lSymb->name),
                          cg->GetString(lBind->name));
          }
        }
      }
      break;
    default:
      break;
  }
  return fExpr;
} // CheckForHiddenVaryingReferences

///////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////// End of check.c ///////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
