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
// semantic.c
//

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "slglobals.h"

/*
 * GetVectorConst()
 *
 */

static void GetVectorConst( CgContext *cg, float *fVal, Expr *fExpr)
{
  int Oops = 0;
  Constant *pconst;
  Binary *pbin;
  Unary *pun;
  int ii;
  
  for (ii = 0; ii < 4; ii++)
    fVal[ii] = 0.0f;
  if (fExpr) {
    switch (fExpr->kind) {
      case UNARY_N:
        pun = (Unary *) fExpr;
        switch (pun->op) {
            //case FCONST_V_OP:
            //    for (ii = 0; ii < SUBOP_GET_S1(pun->subop); ii++)
            //        fVal[ii] = 1.1f;
            //    break;
          case VECTOR_V_OP:
            Oops = 2;
            break;
          default:
            Oops = 1;
            break;
        }
        break;
      case BINARY_N:
        pbin = (Binary *) fExpr;
        switch (pbin->op) {
          case EXPR_LIST_OP:
            if (pbin->right == NULL &&
                pbin->left->kind == CONST_N)
            {
              pconst = (Constant *) pbin->left;
              switch (pconst->op) {
                case FCONST_V_OP:
                case HCONST_V_OP:
                case XCONST_V_OP:
                  for (ii = 0; ii < SUBOP_GET_S1(pconst->subop); ii++)
                    fVal[ii] = pconst->val[ii].f;
                  break;
                case ICONST_V_OP:
                case BCONST_V_OP:
                  for (ii = 0; ii < SUBOP_GET_S1(pconst->subop); ii++)
                    fVal[ii] = (float) pconst->val[ii].i;
                  break;
                case FCONST_OP:
                case HCONST_OP:
                case XCONST_OP:
                  fVal[0] = pconst->val[0].f;
                  break;
                case ICONST_OP:
                case BCONST_OP:
                  fVal[0] = (float) pconst->val[0].i;
                  break;
                default:
                  Oops = 4;
                  break;
              }
            } else {
              Oops = 3;
            }
            break;
          default:
            Oops = 1;
            break;
        }
        break;
      default:
        Oops = 1;
        break;
    }
  }
  if (Oops)
    SemanticWarning( cg, cg->tokenLoc, 9999, "*** GetVectorConst() not finished ***");
} // GetVectorConst

/*
 * lNewUniformSemantic() - Add an entry to the uniform semantic table.
 *
 */
static void lNewUniformSemantic( CgContext *cg, Atom gname, Symbol *fSymb, Atom semantics)
{
  UniformSemantic *lUniform, *nUniform;
  
  lUniform = new UniformSemantic(gname, fSymb->name, semantics);
  nUniform = cg->theHal->uniforms;
  if (nUniform) {
    while (nUniform->next)
      nUniform = nUniform->next;
    nUniform->next = lUniform;
  } else {
    cg->theHal->uniforms = lUniform;
  }
} // lNewUniformSemantic

/*
 * lBindUniformVariable() - Bind a uniform variable to $uniform connector.  Record semantics
 *         value if present.
 */

static int lBindUniformVariable( CgContext *cg, Symbol *fSymb, Atom gname, int IsParameter)
{
  TypeCategory category;
	TypeDomain domain;
	TypeQualifier qualifiers;
	int OK;
  BindingTree *ltree;
  Binding *lBind;
  SymbolList **lList, *mList, *nList;
  
  OK = 0;
  category = GetCategory(fSymb->type);
  domain = GetDomain(fSymb->type);
  qualifiers = GetQualifiers(fSymb->type);
  switch (category) {
    case TC_Scalar:
    case TC_Array:
    case TC_Struct:
      lNewUniformSemantic( cg, gname, fSymb, fSymb->details.var.semantics);
      if (IsParameter) {
        lList = &cg->theHal->uniformParam;
      } else {
        lList = &cg->theHal->uniformGlobal;
      }
      lBind = new Binding( BK_NONE, gname, fSymb->name); // LEAK? -Cass
      lBind->properties = BIND_INPUT | BIND_UNIFORM;
      ltree = LookupBinding( cg, gname, fSymb->name);
      if (ltree) {
        if (cg->theHal->BindUniformPragma(&fSymb->loc, fSymb, lBind, &ltree->binding)) {
          if (!(lBind->properties & BIND_UNIFORM)) {
            SemanticError( cg, &fSymb->loc, ERROR_S_NON_UNIF_BIND_TO_UNIF_VAR,
                          cg->GetString(fSymb->name));
            lList = NULL;
          }
        } else {
          SemanticError( cg, &fSymb->loc, ERROR_S_INCOMPATIBLE_BIND_DIRECTIVE,
                        cg->GetString(fSymb->name));
          lList = NULL;
        }
      }
      if (lList && !(lBind->properties & BIND_HIDDEN)) {
        OK = 1;
        fSymb->details.var.bind = lBind;
        nList = new SymbolList(); // LEAK? -Cass
        nList->next = NULL;
        nList->symb = fSymb;
        if (*lList) {
          mList = *lList;
          while (mList->next != NULL)
            mList = mList->next;
          mList->next = nList;
        } else {
          *lList = nList;
        }
      } else {
        delete lBind;
      }
      break;
    default:
      SemanticError( cg, &fSymb->loc, ERROR_S_ILLEGAL_TYPE_UNIFORM_VAR,
                    cg->GetString(fSymb->name));
      break;
  }
  return OK;
} // lBindUniformVariable

/*
 * lBindVaryingVariable() - Bind a variable to $vin, $vout, or $uniform connector.
 *
 */

static Symbol *lBindVaryingVariable( CgContext *cg, Symbol *fSymb, Atom gname, int IsOutVal, int IsStructMember,
                                    Atom structSemantics)
{
  TypeCategory category;
	TypeDomain domain;
	TypeQualifier qualifiers;
  Symbol *lSymb, *mSymb;
  BindingTree *ltree;
  Binding *lBind = NULL;
  Scope *lScope;
  Atom lname;
  
  lSymb = NULL;
  category = GetCategory(fSymb->type);
  domain = GetDomain(fSymb->type);
  qualifiers = GetQualifiers(fSymb->type);
  switch (category) {
    case TC_Scalar:
    case TC_Array:
      lScope = NULL;
      lname = 0;
      lBind = new Binding( BK_NONE, gname, fSymb->name ); // LEAK? -Cass
      ltree = LookupBinding( cg, gname, fSymb->name);
      if (fSymb->details.var.semantics.IsValid()) {
        if (ltree) {
          SemanticWarning( cg, &fSymb->loc, WARNING_S_SEMANTICS_AND_BINDING,
                          cg->GetString(fSymb->name));
        }
        lname = fSymb->details.var.semantics;
        if (cg->theHal->BindVaryingSemantic(&fSymb->loc, fSymb, lname, lBind, IsOutVal)) {
          if (lBind->properties & BIND_INPUT) {
            if (IsOutVal) {
              SemanticError( cg, &fSymb->loc, ERROR_S_OUT_QUALIFIER_IN_SEMANTIC,
                            cg->GetString(fSymb->name));
            }
            lScope = static_cast< TypeStruct * >( cg->theHal->varyingIn->type )->members;
          } else {
            if (!IsOutVal) {
              SemanticError( cg, &fSymb->loc, ERROR_S_IN_QUALIFIER_OUT_SEMANTIC,
                            cg->GetString(fSymb->name));
            }
            lScope = static_cast< TypeStruct * >( cg->theHal->varyingOut->type )->members;
          }
        } else {
          SemanticError( cg, &fSymb->loc, ERROR_S_UNKNOWN_SEMANTICS,
                        cg->GetString(fSymb->name));
        }
      } else {
        // If no semantics, check for #pragma bind directive.
        lname = fSymb->name;
        if (ltree) {
          if (cg->theHal->BindVaryingPragma(&fSymb->loc, fSymb, lBind, &ltree->binding, IsOutVal)) {
            if (lBind->properties & BIND_UNIFORM) {
              SemanticError( cg, &fSymb->loc, ERROR_S_UNIF_BIND_TO_NON_UNIF_VAR,
                            cg->GetString(fSymb->name));
            } else {
              if (lBind->properties & BIND_INPUT) {
                lScope = static_cast< TypeStruct * >( cg->theHal->varyingIn->type )->members;
              } else {
                lScope = static_cast< TypeStruct * >( cg->theHal->varyingOut->type )->members;
              }
            }
          } else {
            SemanticError( cg, &fSymb->loc, ERROR_S_INCOMPATIBLE_BIND_DIRECTIVE,
                          cg->GetString(fSymb->name));
          }
        } else {
          // If no semantics or #pragma bind, get default binding from profile if it allows them:
          if (cg->theHal->BindVaryingUnbound(&fSymb->loc, fSymb, lname, structSemantics, lBind, IsOutVal)) {
            if (IsOutVal) {
              lScope = static_cast< TypeStruct * >( cg->theHal->varyingOut->type )->members;
            } else {
              lScope = static_cast< TypeStruct * >( cg->theHal->varyingIn->type )->members;
            }
          } else {
            SemanticError( cg, &fSymb->loc, ERROR_S_SEMANTIC_NOT_DEFINED_VOUT,
                          cg->GetString(fSymb->name));
          }
        }
      }
      if (lScope) {
        lBind->lname = lname;
        fSymb->details.var.bind = lBind;
        if (!(lBind->properties & BIND_HIDDEN)) {
          lSymb = LookupLocalSymbol( cg, lScope, lname);
          if (lSymb) {
            // Already defined - second use of this name.
          } else {
            lSymb = AddSymbol( cg, &fSymb->loc, lScope, lname, fSymb->type, SK_Variable);
            lSymb->details.var.bind = new Binding( *lBind );
            if (lScope->symbols != lSymb) {
              mSymb = lScope->symbols;
              while (mSymb->next)
                mSymb = mSymb->next;
              mSymb->next = lSymb;
            }
          }
        }
      } else {
        delete lBind;
      }
      break;
    case TC_Struct:
      SemanticError( cg, &fSymb->loc, ERROR_S_NESTED_SEMANTIC_STRUCT,
                    cg->GetString(fSymb->name));
      break;
    default:
      SemanticError( cg, &fSymb->loc, ERROR_S_ILLEGAL_PARAM_TO_MAIN,
                    cg->GetString(fSymb->name));
      break;
  }
  return lSymb;
} // lBindVaryingVariable

/*
 * lVerifyConnectorDirection() - Verify that this connector name is valid for the current
 *         profile and is of the appropriate direction.
 */

static void lVerifyConnectorDirection( CgContext *cg, SourceLoc *loc, Atom semantics, int IsOutParam)
{
  int cid, uses;
  
  // If connector semantics present make sure that connector direction matches parameter's:
  
  if (semantics.IsValid()) {
    cid = cg->theHal->GetConnectorID(semantics);
    if (cid) {
      uses = cg->theHal->GetConnectorUses(cid, cg->theHal->pid);
      if (IsOutParam) {
        if (!(uses & CONNECTOR_IS_OUTPUT))
          SemanticError( cg, loc, ERROR_S_CONNECT_FOR_INPUT,
                        cg->GetString(semantics));
      } else {
        if (!(uses & CONNECTOR_IS_INPUT))
          SemanticError( cg, loc, ERROR_S_CONNECT_FOR_OUTPUT,
                        cg->GetString(semantics));
      }
    } else {
      SemanticError( cg, loc, ERROR_S_CONNECTOR_TYPE_INVALID,
                    cg->GetString(semantics));
    }
  }
} // lVerifyConnectorDirection

/*
 * BuildSemanticStructs() - Build the three global semantic type structure,  Check main for
 *         type errors in its arguments.
 */

void BuildSemanticStructs( CgContext *cg, SourceLoc *loc, Scope *fScope, Symbol *program)
{
  TypeCategory category;
	TypeDomain domain;
	TypeQualifier qualifiers;
	int len, rlen;
  Scope *vinScope, *voutScope, *lScope;
  Type *vinType, *voutType;
  Symbol *vinVar, *voutVar;
  Atom vinTag, voutTag;
  Symbol *formal, *member, *lSymb;
  Expr *lExpr, *rExpr, *vExpr;
  Type *lType, *rettype;
  StmtList instmts, outstmts;
  Binding *lBind;
  int IsOutParam;
  float lVal[4];
  Stmt *lStmt;
  
  // Define pseudo type structs for semantics:
  
  vinScope = new Scope( cg );
  vinScope->HasSemantics = 1;
  vinScope->level = 1;
  vinScope->IsStructScope = 1;
  voutScope = new Scope( cg );
  voutScope->HasSemantics = 1;
  voutScope->level = 1;
  voutScope->IsStructScope = 1;
  
  vinTag = cg->GetAtom("$vin");
  vinType = StructHeader( cg, loc, fScope, 0, vinTag);
  static_cast< TypeStruct *>( vinType )->members = vinScope;
  cg->theHal->varyingIn = vinVar = DefineVar( cg, loc, fScope, vinTag, vinType);
  //vinTypedef = DefineTypedef(loc, fScope, vinTag, vinType); // Not sure this is neessary
  voutTag = cg->GetAtom("$vout");
  voutType = StructHeader( cg, loc, fScope, 0, voutTag);
  static_cast< TypeStruct * >( voutType )->members = voutScope;
  cg->theHal->varyingOut = voutVar = DefineVar( cg, loc, fScope, voutTag, voutType);
  //voutTypedef = DefineTypedef(loc, fScope, voutTag, voutType); // Not sure this is neessary
  
  instmts.first = instmts.last = NULL;
  outstmts.first = outstmts.last = NULL;
  
  // Walk list of formals creating semantic struct members for all parameters:
  
  formal = program->details.fun.params;
  while (formal) {
    category = GetCategory(formal->type);
    domain = GetDomain(formal->type);
    qualifiers = GetQualifiers(formal->type);
    if (qualifiers == TQ_InOut)
      SemanticError( cg, &formal->loc, ERROR_S_MAIN_PARAMS_CANT_BE_INOUT,
                    cg->GetString(formal->name));
    if (domain == TD_Uniform) {
      if (qualifiers == TQ_Out) {
        SemanticError( cg, &formal->loc, ERROR_S_UNIFORM_ARG_CANT_BE_OUT,
                      cg->GetString(formal->name));
      }
      switch (category) {
        case TC_Scalar:
        case TC_Array:
        case TC_Struct:
          if (lBindUniformVariable( cg, formal, program->name, 1) && formal->details.var.init) {
            formal->details.var.init = FoldConstants( cg, formal->details.var.init);
            GetVectorConst( cg, lVal, formal->details.var.init);
            lBind = new Binding( BK_DEFAULT, Atom(), formal->name );
            lBind->count = 4;
            for ( int i = 0; i < 4; i++ ) {
              lBind->val[i] = lVal[i];
            }
            cg->theHal->AddDefaultBinding(lBind);
          }
          break;
        default:
          SemanticError( cg, &formal->loc, ERROR_S_ILLEGAL_PARAM_TO_MAIN,
                        cg->GetString(formal->name));
          break;
      }
    } else {
      IsOutParam = (qualifiers == TQ_Out);
      switch (category) {
        case TC_Scalar:
        case TC_Array:
          lSymb = lBindVaryingVariable( cg, formal, program->name, IsOutParam, 0, 0);
          if (lSymb) {
            lBind = lSymb->details.var.bind;
            if (lBind && !(lBind->properties & BIND_HIDDEN)) {
              lExpr = GenSymb( cg, formal);
              if (IsScalar(formal->type) || IsVector(formal->type, &len)) {
                if (lBind->properties & BIND_INPUT) {
                  // Assign $vin member to bound variable:
                  vExpr = (Expr *) NewSymbNode( cg, VARIABLE_OP, vinVar);
                  rExpr = GenMemberReference( cg, vExpr, lSymb);
                  if (IsVector(lSymb->type, &rlen))
                    rExpr = GenConvertVectorLength( cg, rExpr, GetBase(lSymb->type), rlen, len);
                  lStmt = NewSimpleAssignmentStmt( cg, &program->loc, lExpr, rExpr, 0);
                  AppendStatements(&instmts, lStmt);
                } else {
                  // Assign bound variable to $vout member:
                  vExpr = (Expr *) NewSymbNode( cg, VARIABLE_OP, voutVar);
                  rExpr = GenMemberReference( cg, vExpr, lSymb);
                  if (IsVector(lSymb->type, &rlen))
                    lExpr = GenConvertVectorLength( cg, lExpr, GetBase(formal->type), len, rlen);
                  lStmt = NewSimpleAssignmentStmt( cg, &program->loc, rExpr, lExpr, 0);
                  AppendStatements(&outstmts, lStmt);
                }
              } else {
                FatalError( cg, "Parameter of unsupported type");
                // xxx
              }
            }
          }
          break;
        case TC_Struct:
          lType = formal->type;
          lVerifyConnectorDirection( cg, &formal->loc, static_cast< TypeStruct * >( lType )->semantics, IsOutParam);
          lScope = static_cast< TypeStruct * >( lType )->members;
          member = lScope->symbols;
          while (member) {
            lSymb = lBindVaryingVariable( cg, member, static_cast< TypeStruct * >( lType )->tag, IsOutParam, 1,
                                         static_cast< TypeStruct * >( lType )->semantics);
            if (lSymb) {
              lBind = lSymb->details.var.bind;
              if (lBind && !(lBind->properties & BIND_HIDDEN)) {
                lExpr = GenMemberReference( cg, NewSymbNode( cg, VARIABLE_OP, formal), member);
                if (IsScalar(member->type) || IsVector(member->type, &len)) {
                  if (lBind->properties & BIND_INPUT) {
                    // Assign $vin member to bound variable:
                    vExpr = (Expr *) NewSymbNode( cg, VARIABLE_OP, vinVar);
                    rExpr = GenMemberReference( cg, vExpr, lSymb);
                    if (IsVector(lSymb->type, &rlen))
                      rExpr = GenConvertVectorLength( cg, rExpr, GetBase(lSymb->type), rlen, len);
                    lStmt = NewSimpleAssignmentStmt( cg, &program->loc, lExpr, rExpr, 0);
                    AppendStatements(&instmts, lStmt);
                  } else {
                    // Assign bound variable to $vout member:
                    vExpr = (Expr *) NewSymbNode( cg, VARIABLE_OP, voutVar);
                    rExpr = GenMemberReference( cg, vExpr, lSymb);
                    if (IsVector(lSymb->type, &rlen))
                      lExpr = GenConvertVectorLength( cg, lExpr, GetBase(member->type), len, rlen);
                    lStmt = NewSimpleAssignmentStmt( cg, &program->loc, rExpr, lExpr, 0);
                    AppendStatements(&outstmts, lStmt);
                  }
                } else {
                  FatalError( cg, "Parameter of unsupported type");
                  // xxx
                }
              }
            }
            member = member->next;
          }
          break;
        default:
          SemanticError( cg, &formal->loc, ERROR_S_ILLEGAL_PARAM_TO_MAIN,
                        cg->GetString(formal->name));
          break;
      }
    }
    formal = formal->next;
  }
  
  // Add return value's semantics to the $vout connector:
  
  lType = program->type;
  rettype = static_cast< TypeFunction * >( lType )->rettype;
  category = GetCategory(rettype);
  if (!IsVoid(rettype)) {
    if (category == TC_Struct) {
      lVerifyConnectorDirection( cg, &program->loc, static_cast< TypeStruct * >( rettype )->semantics, 1);
      lScope = static_cast< TypeStruct * >( rettype )->members;
      member = lScope->symbols;
      while (member) {
        lSymb = lBindVaryingVariable( cg, member, static_cast< TypeStruct * >( rettype )->tag, 1, 1,
                                     static_cast< TypeStruct * >( rettype )->semantics);
        member = member->next;
      }
    } else {
      SemanticError( cg, &program->loc, ERROR_S_PROGRAM_MUST_RETURN_STRUCT,
                    cg->GetString(program->name));
    }
  }
  
  // Set the output connector variety:
  
  static_cast< TypeStruct * >( voutType )->variety = cg->theHal->outcid;
  
  // Append initial and final assignment statements to beginning and end of main:
  
  program->details.fun.statements = ConcatStmts(instmts.first, program->details.fun.statements);
  program->details.fun.statements = ConcatStmts(program->details.fun.statements, outstmts.first);
  
} // BuildSemanticStructs


void BindDefaultSemantic( CgContext *cg, Symbol *lSymb, int category, int gname)
{
  Binding *lBind;
  float lVal[4];
  
  switch (category) {
    case TC_Scalar:
    case TC_Array:
    case TC_Struct:
      gname = 0;
      if (lBindUniformVariable( cg, lSymb, gname, 0) && lSymb->details.var.init) {
        lSymb->details.var.init = FoldConstants( cg, lSymb->details.var.init);
        GetVectorConst( cg, lVal, lSymb->details.var.init);
        lBind = new Binding( BK_DEFAULT, Atom(), lSymb->name );
        lBind->count = 4;
        for ( int i = 0; i < 4; i++ ) {
          lBind->val[i] = lVal[i];
        }
        cg->theHal->AddDefaultBinding(lBind);
      }
      break;
    default:
      SemanticError( cg, &lSymb->loc, ERROR_S_NON_STATIC_GLOBAL_TYPE_ERR,
                    cg->GetString(lSymb->name));
  }
  lSymb->properties &= ~SYMB_NEEDS_BINDING;
} // BindDefaultSemantic
