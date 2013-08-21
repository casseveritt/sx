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
// binding.c
//

#include <stdlib.h>
#include <stdio.h>

#include "slglobals.h"

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////// Connector and Parameter Binding Functions: //////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * NewBindingTree()
 *
 */

BindingTree *NewBindingTree(SourceLoc *loc, BindingKind kind, Atom gname, Atom lname )
{
  BindingTree *lTree;
  
  lTree = new BindingTree( kind, gname, lname );
  lTree->loc = *loc;
  lTree->loc.line = 0;
  return lTree;
} // NewBindingTree

/*
 * NewConnectorBindingTree()
 *
 */

BindingTree *NewConnectorBindingTree(SourceLoc *loc, Atom cname, Atom mname, Atom rname)
{
  BindingTree *lTree = NewBindingTree(loc, BK_CONNECTOR, cname, mname);
  return lTree;
} // NewConnectorBindingTree

/*
 * NewRegArrayBindingTree()
 *
 */

BindingTree *NewRegArrayBindingTree(SourceLoc *loc, Atom pname, Atom aname, Atom rname, int regno, int count)
{
  BindingTree *lTree = NewBindingTree(loc, BK_REGARRAY, pname, aname );
	lTree->binding.name = rname;
	lTree->binding.num = regno;
	lTree->binding.count = count;
  return lTree;
} // NewRegArrayBindingTree

/*
 * NewTexunitBindingTree()
 *
 */

BindingTree *NewTexunitBindingTree(SourceLoc *loc, Atom pname, Atom aname, int unitno)
{
  BindingTree *lTree = NewBindingTree(loc, BK_TEXUNIT, pname, aname );
	lTree->binding.num = unitno;
  return lTree;
} // NewTexunitBindingTree

/*
 * NewConstDefaultBindingTree()
 *
 */

BindingTree *NewConstDefaultBindingTree(SourceLoc *loc, BindingKind kind, Atom pname, Atom aname, int count, float *fval)
{
  BindingTree *lTree = NewBindingTree( loc, kind, pname, aname );
	count = count < 1 ? 1 : count;
	count = count > 4 ? 4 : count;
	lTree->binding.count = count;
	for ( int i = 0; i < count; i++ ) {
		lTree->binding.val[i] = fval[i];
	}
  return lTree;
} // NewConstDefaultBindingTree

/*
 * LookupBinding()
 *
 */

BindingTree *LookupBinding( CgContext *cg, Atom gname, Atom lname)
{
  BindingTree *lTree;
  
  lTree = cg->bindings;
  while (lTree) {
    if (lTree->binding.gname == gname) {
      do {
        if (lTree->binding.lname == lname) {
          return lTree;
        } else {
          lTree = lTree->nextm;
        }
      } while (lTree);
      return NULL;
    }
    lTree = lTree->nextc;
  }
  return NULL;
} // LookupBinding

/*
 * AddBinding()
 *
 */

void AddBinding( CgContext *cg, BindingTree *fTree)
{
  BindingTree *lTree;
  
  lTree = cg->bindings;
  while (lTree) {
    if (lTree->binding.gname == fTree->binding.gname)
      break;
    lTree = lTree->nextc;
  }
  if (lTree) {
    fTree->nextm = lTree->nextm;
    lTree->nextm = fTree;
  } else {
    fTree->nextc = cg->bindings;
    cg->bindings = fTree;
  }
} // AddBinding

/*
 * DefineConnectorBinding() - Define a binding between a member of a connector and a hw reg.
 *
 * #pragma bind <conn-id> "." <memb-id> "=" <reg-id>
 *
 */

void DefineConnectorBinding( CgContext *cg, SourceLoc *loc, Atom cname, Atom mname, Atom rname)
{
  BindingTree *lTree;
  
  lTree = LookupBinding( cg, cname, mname);
  if (lTree) {
    SemanticError( cg, loc, ERROR_SSSD_DUPLICATE_BINDING,
                  cg->GetString(cname), cg->GetString(mname),
                  cg->GetString(lTree->loc.file), lTree->loc.line);
    return;
  }
  lTree = NewConnectorBindingTree( loc, cname, mname, rname);
  AddBinding(  cg, lTree);
} // DefineConnectorBinding

/*
 * DefineRegArrayBinding() - Define a binding between a member of a connector and a hw reg.
 *
 * #pragma bind <prog-id> "." <arg-id> "=" <reg-id>
 *
 */

void DefineRegArrayBinding( CgContext *cg, SourceLoc *loc, Atom pname, Atom aname, Atom rname, int index, int count)
{
  BindingTree *lTree;
  
  lTree = LookupBinding( cg, pname, aname);
  if (lTree) {
    SemanticError( cg, loc, ERROR_SSSD_DUPLICATE_BINDING,
                  cg->GetString(pname), cg->GetString(aname),
                  cg->GetString(lTree->loc.file), lTree->loc.line);
    return;
  }
  lTree = NewRegArrayBindingTree(loc, pname, aname, rname, index, count);
  AddBinding( cg, lTree);
} // DefineRegArrayBinding

/*
 * DefineTexunitBinding() - Define a binding between a member of a connector and a hw reg.
 *
 * #pragma bind <prog-id> "." <arg-id> "=" <reg-id> <i-const> [ <i-const> ]
 *
 */

void DefineTexunitBinding( CgContext *cg, SourceLoc *loc, Atom pname, Atom aname, int unitno)
{
  BindingTree *lTree;
  
  lTree = LookupBinding( cg, pname, aname);
  if (lTree) {
    SemanticError( cg, loc, ERROR_SSSD_DUPLICATE_BINDING,
                  cg->GetString(pname), cg->GetString(aname),
                  cg->GetString(lTree->loc.file), lTree->loc.line);
    return;
  }
  lTree = NewTexunitBindingTree(loc, pname, aname, unitno);
  AddBinding( cg, lTree);
} // DefineTexunitBinding

/*
 * DefineConstantBinding() - Define a binding between a member of a connector and a hw reg.
 *
 * #pragma bind <prog-id> "." <arg-id> "=" "Constant" <simple-float-Expr>+
 *
 */

void DefineConstantBinding( CgContext *cg, SourceLoc *loc, Atom pname, Atom aname, int count, float *fval)
{
  BindingTree *lTree;
  
  lTree = LookupBinding( cg, pname, aname);
  if (lTree) {
    SemanticError( cg, loc, ERROR_SSSD_DUPLICATE_BINDING,
                  cg->GetString(pname), cg->GetString(aname),
                  cg->GetString(lTree->loc.file), lTree->loc.line);
    return;
  }
  lTree = NewConstDefaultBindingTree(loc, BK_CONSTANT, pname, aname, count, fval);
  AddBinding( cg, lTree);
} // DefineConstantBinding

/*
 * DefineDefaultBinding() - Define a binding between a member of a connector and a hw reg.
 *
 * #pragma bind <prog-id> "." <arg-id> "=" "default" <simple-float-Expr>+
 *
 */

void DefineDefaultBinding( CgContext *cg, SourceLoc *loc, Atom pname, Atom aname, int count, float *fval)
{
  BindingTree *lTree;
  
  lTree = LookupBinding( cg, pname, aname);
  if (lTree) {
    SemanticError( cg, loc, ERROR_SSSD_DUPLICATE_BINDING,
                  cg->GetString(pname), cg->GetString(aname),
                  cg->GetString(lTree->loc.file), lTree->loc.line);
    return;
  }
  lTree = NewConstDefaultBindingTree(loc, BK_DEFAULT, pname, aname, count, fval);
  AddBinding( cg, lTree);
} // DefineDefaultBinding


///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////// End of binding.c //////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
