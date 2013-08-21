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
// support_iter.c
//
// Routines to iterate over the Expr/Stmt graph
//

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "slglobals.h"

/*
 * ApplyToNodes() - Walk an expression tree and apply "pre" and "post" to
 * each node.  pre is applied in prefix order, and post in postfix
 *
 */
Expr *ApplyToNodes(CgContext *cg, ToExpr pre, ToExpr post, Expr *fExpr, void *arg1, int arg2)
{
  if (fExpr) {
    if (pre) fExpr = pre( cg, fExpr, arg1, arg2);
    switch (fExpr->kind) {
      case DECL_N:
      case SYMB_N:
      case CONST_N:
        break;
      case UNARY_N:
        static_cast< Unary * >( fExpr )->arg = ApplyToNodes( cg, pre, post, static_cast< Unary * >( fExpr )->arg, arg1, arg2);
        break;
      case BINARY_N:
        static_cast< Binary * >( fExpr )->left = ApplyToNodes( cg, pre, post, static_cast< Binary * >( fExpr )->left, arg1, arg2);
        static_cast< Binary * >( fExpr )->right = ApplyToNodes( cg, pre, post, static_cast< Binary * >( fExpr )->right, arg1, arg2);
        break;
      case TRINARY_N:
        static_cast< Trinary * >( fExpr )->arg1 = ApplyToNodes( cg, pre, post, static_cast< Trinary * >( fExpr )->arg1, arg1, arg2);
        static_cast< Trinary * >( fExpr )->arg2 = ApplyToNodes( cg, pre, post, static_cast< Trinary * >( fExpr )->arg2, arg1, arg2);
        static_cast< Trinary * >( fExpr )->arg3 = ApplyToNodes( cg, pre, post, static_cast< Trinary * >( fExpr )->arg3, arg1, arg2);
        break;
      default:
        assert(!"bad kind to ApplyToNodes()");
        break;
    }
    if (post) fExpr = post( cg, fExpr, arg1, arg2);
  }
  return fExpr;
} // ApplyToNodes

/*
 * ApplyToExpressions() - Walk a source tree and apply "fun" to each node in each
 *         expression in prefix order.
 */

void ApplyToExpressions(CgContext *cg, ToExpr pre, ToExpr post, Stmt *fStmt, void *arg1, int arg2)
{
  while (fStmt) {
    cg->lastSourceLoc = fStmt->loc;
    switch (fStmt->kind) {
      case EXPR_STMT:
        static_cast< ExprStmt * >( fStmt )->expr = ApplyToNodes( cg, pre, post, static_cast< ExprStmt * >( fStmt )->expr, arg1, arg2);
        break;
      case IF_STMT:
        static_cast< IfStmt * >( fStmt )->cond = ApplyToNodes( cg, pre, post, static_cast< IfStmt * >( fStmt )->cond, arg1, arg2);
        ApplyToExpressions( cg, pre, post, static_cast< IfStmt * >( fStmt )->thenstmt, arg1, arg2);
        ApplyToExpressions( cg, pre, post, static_cast< IfStmt * >( fStmt )->elsestmt, arg1, arg2);
        break;
      case WHILE_STMT:
      case DO_STMT:
        static_cast< WhileStmt * >( fStmt )->cond = ApplyToNodes( cg, pre, post, static_cast< WhileStmt * >( fStmt )->cond, arg1, arg2);
        ApplyToExpressions( cg, pre, post, static_cast< WhileStmt * >( fStmt )->body, arg1, arg2);
        break;
      case FOR_STMT:
        ApplyToExpressions( cg, pre, post, static_cast< ForStmt * >( fStmt )->init, arg1, arg2);
        static_cast< ForStmt * >( fStmt )->cond = ApplyToNodes( cg, pre, post, static_cast< ForStmt * >( fStmt )->cond, arg1, arg2);
        ApplyToExpressions( cg, pre, post, static_cast< ForStmt * >( fStmt )->body, arg1, arg2);
        ApplyToExpressions( cg, pre, post, static_cast< ForStmt * >( fStmt )->step, arg1, arg2);
        break;
      case BLOCK_STMT:
        ApplyToExpressions( cg, pre, post, static_cast< BlockStmt * >( fStmt )->body, arg1, arg2);
        break;
      case RETURN_STMT:
        static_cast< ReturnStmt * >( fStmt )->expr = ApplyToNodes( cg, pre, post, static_cast< ReturnStmt * >( fStmt )->expr, arg1, arg2);
        break;
      case DISCARD_STMT:
        static_cast< DiscardStmt * >( fStmt )->cond = ApplyToNodes( cg, pre, post, static_cast< DiscardStmt * >( fStmt )->cond, arg1, arg2);
        break;
      case COMMENT_STMT:
        break;
      default:
        assert(0);
        break;
    }
    fStmt = fStmt->next;
  }
} // ApplyToExpressions

/*
 * ApplyToExpressionsLocal() - Apply a function to each node in the expressions contained in
 *         a single statement.
 */

void ApplyToExpressionsLocal(CgContext *cg, ToExpr pre, ToExpr post, Stmt *fStmt, void *arg1, int arg2)
{
  if (fStmt) {
    cg->lastSourceLoc = fStmt->loc;
    switch ( fStmt->kind ) {
      case EXPR_STMT:
        static_cast< ExprStmt * >( fStmt )->expr = ApplyToNodes( cg, pre, post, static_cast< ExprStmt * >( fStmt )->expr, arg1, arg2);
        break;
      case IF_STMT:
        static_cast< IfStmt * >( fStmt )->cond = ApplyToNodes( cg, pre, post, static_cast< IfStmt * >( fStmt )->cond, arg1, arg2);
        break;
      case WHILE_STMT:
      case DO_STMT:
        static_cast< WhileStmt * >( fStmt )->cond = ApplyToNodes( cg, pre, post, static_cast< WhileStmt * >( fStmt )->cond, arg1, arg2);
        break;
      case FOR_STMT:
        static_cast< ForStmt * >( fStmt )->cond = ApplyToNodes( cg, pre, post, static_cast< ForStmt * >( fStmt )->cond, arg1, arg2);
        break;
      case BLOCK_STMT:
        break;
      case RETURN_STMT:
        static_cast< ReturnStmt * >( fStmt )->expr = ApplyToNodes( cg, pre, post, static_cast< ReturnStmt * >( fStmt )->expr, arg1, arg2);
        break;
      case DISCARD_STMT:
        static_cast< DiscardStmt * >( fStmt )->cond = ApplyToNodes( cg, pre, post, static_cast< DiscardStmt * >( fStmt )->cond, arg1, arg2);
        break;
      case COMMENT_STMT:
        break;
      default:
        assert(0);
        break;
    }
    //fStmt = static_cast< ExprStmt * >( fStmt )->next;
  }
} // ApplyToExpressionsLocal

/*
 * ApplyToTopExpressions() - Walk a source tree and apply a function to each expression.
 *
 */

void ApplyToTopExpressions( CgContext *cg, ToExpr fun, Stmt *fStmt, void *arg1, int arg2)
{
  while (fStmt) {
    cg->lastSourceLoc = fStmt->loc;
    switch ( fStmt->kind ) {
      case EXPR_STMT:
        static_cast< ExprStmt * >( fStmt )->expr = fun( cg, static_cast< ExprStmt * >( fStmt )->expr, arg1, arg2);
        break;
      case IF_STMT:
        static_cast< IfStmt * >( fStmt )->cond = fun( cg, static_cast< IfStmt * >( fStmt )->cond, arg1, arg2);
        ApplyToTopExpressions( cg, fun, static_cast< IfStmt * >( fStmt )->thenstmt, arg1, arg2);
        ApplyToTopExpressions( cg, fun, static_cast< IfStmt * >( fStmt )->elsestmt, arg1, arg2);
        break;
      case WHILE_STMT:
      case DO_STMT:
        static_cast< WhileStmt * >( fStmt )->cond = fun( cg, static_cast< WhileStmt * >( fStmt )->cond, arg1, arg2);
        ApplyToTopExpressions( cg, fun, static_cast< WhileStmt * >( fStmt )->body, arg1, arg2);
        break;
      case FOR_STMT:
        ApplyToTopExpressions( cg, fun, static_cast< ForStmt * >( fStmt )->init, arg1, arg2);
        static_cast< ForStmt * >( fStmt )->cond = fun( cg, static_cast< ForStmt * >( fStmt )->cond, arg1, arg2);
        ApplyToTopExpressions( cg, fun, static_cast< ForStmt * >( fStmt )->body, arg1, arg2);
        ApplyToTopExpressions( cg, fun, static_cast< ForStmt * >( fStmt )->step, arg1, arg2);
        break;
      case BLOCK_STMT:
        ApplyToTopExpressions( cg, fun, static_cast< BlockStmt * >( fStmt )->body, arg1, arg2);
        break;
      case RETURN_STMT:
        static_cast< ReturnStmt * >( fStmt )->expr = fun( cg, static_cast< ReturnStmt * >( fStmt )->expr, arg1, arg2);
        break;
      case DISCARD_STMT:
        static_cast< DiscardStmt * >( fStmt )->cond = fun( cg, static_cast< DiscardStmt * >( fStmt )->cond, arg1, arg2);
        break;
      case COMMENT_STMT:
        break;
      default:
        assert(0);
        break;
    }
    fStmt = fStmt->next;
  }
} // ApplyToTopExpressions

/*
 * ApplyToStatements() - Walk a source tree and apply a transformations to
 *    each statememt
 */

Stmt *ApplyToStatements(CgContext *cg, ToStmt pre, ToStmt post, Stmt *fStmt, void *arg1, int arg2)
{
  Stmt *head = NULL, *last = NULL, *lStmt, *next, *rest = fStmt;
  
  while (fStmt) {
    // Transform each statement into a possible NULL list of statements:
    // Prepend any statements returned to the list to be processed, and
    // remember what the next one to be done is (rest), so we don't
    // rerun pre on any of the returned statements directly.
    if (pre && rest == fStmt) {
      cg->lastSourceLoc = fStmt->loc;
      rest = fStmt->next;
      fStmt->next = NULL;
      lStmt = pre( cg, fStmt, arg1, arg2);
      if (lStmt) {
        fStmt = lStmt;
        while (lStmt->next && lStmt->next != rest) {
          lStmt = lStmt->next;
        }
        lStmt->next = rest;
      } else {
        // Nothing returned - go to next statement:
        fStmt = rest;
        continue;
      }
    }
    
    // Now apply transformation to substatements:
    
    switch ( fStmt->kind ) {
      case EXPR_STMT:
        break;
      case IF_STMT:
        static_cast< IfStmt * >( fStmt )->thenstmt = ApplyToStatements( cg, pre, post, static_cast< IfStmt * >( fStmt )->thenstmt, arg1, arg2);
        static_cast< IfStmt * >( fStmt )->elsestmt = ApplyToStatements( cg, pre, post, static_cast< IfStmt * >( fStmt )->elsestmt, arg1, arg2);
        break;
      case WHILE_STMT:
      case DO_STMT:
        static_cast< WhileStmt * >( fStmt )->body = ApplyToStatements( cg, pre, post, static_cast< WhileStmt * >( fStmt )->body, arg1, arg2);
        break;
      case FOR_STMT:
        static_cast< ForStmt * >( fStmt )->init = ApplyToStatements( cg, pre, post, static_cast< ForStmt * >( fStmt )->init, arg1, arg2);
        static_cast< ForStmt * >( fStmt )->body = ApplyToStatements( cg, pre, post, static_cast< ForStmt * >( fStmt )->body, arg1, arg2);
        static_cast< ForStmt * >( fStmt )->step = ApplyToStatements( cg, pre, post, static_cast< ForStmt * >( fStmt )->step, arg1, arg2);
        break;
      case BLOCK_STMT:
        static_cast< BlockStmt * >( fStmt )->body = ApplyToStatements( cg, pre, post, static_cast< BlockStmt * >( fStmt )->body, arg1, arg2);
        break;
      case RETURN_STMT:
      case DISCARD_STMT:
      case COMMENT_STMT:
        break;
      default:
        assert(0);
        break;
    }
    
    // Append any statements returned by "post" to the end of the list:
    
    next = fStmt->next;
    if (post) {
      cg->lastSourceLoc = fStmt->loc;
      lStmt = post( cg, fStmt, arg1, arg2);
    } else {
      lStmt = fStmt;
    }
    if (lStmt) {
      if (head) {
        last->next = lStmt;
      } else {
        head = lStmt;
      }
      last = lStmt;
      while (last->next && last->next != next) {
        last = last->next;
      }
      last->next = NULL;
    }
    fStmt = next;
  }
  return head;
} // ApplyToStatements

/*
 * PostApplyToChildStatements() - Apply a postfix order transformation to each child
 *         statememt of this statement.
 */

void PostApplyToChildStatements( CgContext *cg, ToStmt fun, Stmt *fStmt, void *arg1, int arg2)
{
  if (fStmt) {
    
    // Apply a transformation to each nested statement, but not the top level statements:
    
    cg->lastSourceLoc = fStmt->loc;
    switch ( fStmt->kind ) {
      case EXPR_STMT:
        break;
      case IF_STMT:
        static_cast< IfStmt * >( fStmt )->thenstmt = PostApplyToStatements( cg, fun, static_cast< IfStmt * >( fStmt )->thenstmt, arg1, arg2);
        static_cast< IfStmt * >( fStmt )->elsestmt = PostApplyToStatements( cg, fun, static_cast< IfStmt * >( fStmt )->elsestmt, arg1, arg2);
        break;
      case WHILE_STMT:
      case DO_STMT:
        static_cast< WhileStmt * >( fStmt )->body = PostApplyToStatements( cg, fun, static_cast< WhileStmt * >( fStmt )->body, arg1, arg2);
        break;
      case FOR_STMT:
        static_cast< ForStmt * >( fStmt )->init = PostApplyToStatements( cg, fun, static_cast< ForStmt * >( fStmt )->init, arg1, arg2);
        static_cast< ForStmt * >( fStmt )->body = PostApplyToStatements( cg, fun, static_cast< ForStmt * >( fStmt )->body, arg1, arg2);
        static_cast< ForStmt * >( fStmt )->step = PostApplyToStatements( cg, fun, static_cast< ForStmt * >( fStmt )->step, arg1, arg2);
        break;
      case BLOCK_STMT:
        static_cast< BlockStmt * >( fStmt )->body = PostApplyToStatements( cg, fun, static_cast< BlockStmt * >( fStmt )->body, arg1, arg2);
        break;
      case RETURN_STMT:
      case DISCARD_STMT:
      case COMMENT_STMT:
        break;
      default:
        assert(0);
        break;
    }
  }
} // PostApplyToChildStatements

