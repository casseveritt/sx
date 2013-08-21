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
// hal.c
//

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "slglobals.h"

/*
 static void InitHal_Hal(Hal *);
 static int GetCapsBit_Hal(int bitNumber);
 static int GetConnectorUses_Hal(int cid, int pid);
 static int GetConnectorRegister_Hal(int cid, int ByIndex, Atom ratom, Binding *fBind);
 static TypeBase GetFloatSuffixBase_Hal(SourceLoc *loc, int suffix);
 static int GetSizeof_Hal(Type *fType);
 static int GetAlignment_Hal(Type *fType);
 static int CheckDeclarators_Hal(SourceLoc *loc, const DeclType *fDtype);
 static int CheckDefinition_Hal(SourceLoc *loc, Atom name, const Type *fType);
 static int CheckStatement_Hal(SourceLoc *loc, Stmt *fstmt);
 static int CheckInternalFunction_Hal(Symbol *fSymb, int *group);
 static int IsNumericBase_Hal(int fBase);
 static int IsIntegralBase_Hal(int fBase);
 static int IsTexobjBase_Hal(int fBase);
 static int IsValidRuntimeBase_Hal(int fBase);
 static int IsValidScalarCast_Hal(int toBase, int fromBase, int Explicit);
 static int IsValidOperator_Hal(SourceLoc *loc, Atom name, int op, int suobp);
 static TypeBase GetBinOpBase_Hal(int lop, TypeBase lbase, TypeBase rbase, int llen, int rlen);
 static int ConvertConstant_Hal(const ScalarConstant *fval, TypeBase fbase, TypeBase tbase,
 Expr **fexpr);
 static int BindUniformUnbound_Hal(SourceLoc *loc, Symbol *fSymb, Binding *lBind);
 static int BindUniformPragma_Hal(SourceLoc *loc, Symbol *fSymb, Binding *lBind,
 const Binding *fBind);
 static int BindVaryingSemantic_Hal(SourceLoc *loc, Symbol *fSymb, Atom semantic,
 Binding *fBind, int IsOutVal);
 static int BindVaryingPragma_Hal(SourceLoc *loc, Symbol *fSymb, Binding *lBind,
 const Binding *fBind, int IsOutVal);
 static int BindVaryingUnbound_Hal(SourceLoc *loc, Symbol *fSymb, Atom name, Atom connector,
 Binding *fBind, int IsOutVal);
 static int PrintCodeHeader_Hal(FILE *out);
 static int GenerateCode_Hal(SourceLoc *loc, Scope *fScope, Symbol *program);
 */

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////// Profile Manager: //////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * RegisterProfile() - Add a profile to the list of available profiles.
 *
 */
#define MAX_PROFILES 32
int numProfiles = 0;
Profile profiles[ MAX_PROFILES ];

Profile *RegisterProfile(Hal * (*halFactory)( CgContext *cg, Atom entryName), const char * profileName )
{
	if ( numProfiles >= ( MAX_PROFILES - 1 ) ) {
		return NULL;
	}
	Profile p;
	p.name = profileName;
	p.halFactory = halFactory;
	profiles[ numProfiles ] = p;
	numProfiles++;
	return &profiles[ numProfiles - 1 ];
} // RegisterProfile

/*
 * EnumerateProfiles()
 *
 */

Profile *EnumerateProfiles(int index)
{
	return ( 0 <= index && index < numProfiles ) ? &profiles[ index ] : NULL;
} // EnumerateProfiles


///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////// Default Language Hal ////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * Hal::Hal()
 *
 */

Hal::Hal( CgContext * hCg, Atom hEntryName )
: cg( hCg )
, entryName( hEntryName )
, vendor( NULL )
, version( NULL )
, semantics( NULL )
, numSemantics( 0 )
, incid( CID_NONE_ID )
, inputCRegs( NULL )
, numInputCRegs( 0 )
, outcid( CID_NONE_ID )
, outputCRegs( NULL )
, numOutputCRegs( 0 )
, comment( NULL )
{
	profileName = cg->GetAtom( "default");
  
	pid = 0;
	nextUnboundVinReg = 0;
	lastUnboundVinReg = -1;
	nextUnboundVoutReg = 0;
	lastUnboundVoutReg = -1;
  
	// Defined by compiler front end:
  
	globalScope = NULL;
	varyingIn = NULL;
	varyingOut = NULL;
	uniformParam = NULL;
	uniformGlobal = NULL;
	uniforms = NULL;
  
	constantBindings = NULL;
	defaultBindings = NULL;
  
	// Define default comment start string:
  
	comment = "#";
}


/*
 * InitHal()
 *
 */

int InitHal( CgContext *cg, const char *profileName, const char *entryName)
{
	for ( int i = 0; i < numProfiles; i++ ) {
		if ( !strcmp( profiles[i].name, profileName) ) {
			cg->theHal = profiles[ i ].halFactory( cg, cg->GetAtom( entryName ) );
			return 1;
		}
	}
	return 0;
} // InitHal



Hal::~Hal() {
	delete uniforms;
	// the symbols are not owned by the SymbolList
	while( uniformParam ) {
		SymbolList * next = uniformParam->next;
		delete uniformParam;
		uniformParam = next;
	}
	while( uniformGlobal ) {
		SymbolList * next = uniformGlobal->next;
		delete uniformGlobal;
		uniformGlobal = next;
	}
}

/*
 * AddConstantBinding() - Add a Constant binding to the program.
 *
 */

void Hal::AddConstantBinding(Binding *fBind)
{
  BindingList *lBindList, *nBindList;
  
  nBindList = new BindingList();
  nBindList->next = NULL;
  nBindList->binding = fBind;
  lBindList = constantBindings;
  if (lBindList) {
    while (lBindList->next)
      lBindList = lBindList->next;
    lBindList->next = nBindList;
  } else {
    constantBindings = nBindList;
  }
} // AddConstantBinding

/*
 * AddDefaultBinding() - Add a default binding to the program.
 *
 */

void Hal::AddDefaultBinding(Binding *fBind)
{
  BindingList *lBindList, *nBindList;
  
  nBindList = new BindingList();
  nBindList->next = NULL;
  nBindList->binding = fBind;
  lBindList = defaultBindings;
  if (lBindList) {
    while (lBindList->next)
      lBindList = lBindList->next;
    lBindList->next = nBindList;
  } else {
    defaultBindings = nBindList;
  }
} // AddDefaultBinding

/*
 * LookupConnector() - Lookup a connector descriptor by cid.
 *
 */

ConnectorDescriptor *LookupConnector(ConnectorDescriptor *connectors, int cid, int num)
{
  int ii;
  
  for (ii = 0; ii < num; ii++) {
    if (cid == connectors[ii].cid)
      return &connectors[ii];
  }
  return NULL;
} // LookupConnector

/*
 * SetSymbolConnectorBinding()
 *
 */

void SetSymbolConnectorBinding(Binding *fBind, ConnectorRegisters *fConn)
{
  fBind->properties = BIND_IS_BOUND;
  fBind->kind = BK_CONNECTOR;
  if (fConn->properties & REG_WRITE_REQUIRED)
    fBind->properties |= BIND_WRITE_REQUIRED;
  // tBind->gname set elsewhere
  // tBind->lname set elsewhere
  fBind->base = fConn->base;
  fBind->size = fConn->size;
  fBind->name = fConn->name;
  fBind->num = fConn->regno;
} // SetSymbolConnectorBinding

/*
 * GetFloatSuffixBase() - Check for profile-specific limitations of floating point
 *         suffixes and return the base for this suffix.
 *
 */

TypeBase Hal::GetFloatSuffixBase(SourceLoc *loc, int suffix)
{
  switch (suffix) {
    case ' ':
      return TB_Cfloat;
    case 'f':
      return TB_Float;
    default:
      SemanticError( cg, loc, ERROR_C_UNSUPPORTED_FP_SUFFIX, suffix);
      return TB_UndefinedType;
  }
} // GetFloatSuffixBase_Hal

/*
 * GetSizeof() - Return a profile specific size for this scalar, vector, or matrix type.
 *         Used for defining struct member offsets for use by code generator.
 */

int Hal::GetSizeof(Type *fType)
{
  int category, size, alignment, len, len2;
  
  if (fType) {
    category = GetCategory(fType);
    switch (category) {
      case TC_Scalar:
      case TC_Struct:
      case TC_Connector:
        size = fType->size;
        break;
      case TC_Array:
        if (IsVector(fType, &len)) {
          size = len;
        } else if (IsMatrix(fType, &len, &len2)) {
          if (len2 > len) {
            size = len*4;
          } else {
            size = len2*4;
          }
        } else {
          size = cg->theHal->GetSizeof(static_cast< TypeArray * >( fType )->eltype);
          alignment = cg->theHal->GetAlignment(static_cast< TypeArray * >( fType )->eltype);
          size = ((size + alignment - 1)/alignment)*alignment*static_cast< TypeArray * >( fType )->numels;
        }
        break;
      case TC_Function:
      default:
        size = 0;
        break;
    }
  } else {
    size = 0;
  }
  return size;
} // GetSizeof

/*
 * GetAlignment() - Return a profile specific alignment for this type.
 *         Used for defining struct member offsets for use by code generator.
 */

int Hal::GetAlignment(Type *fType)
{
  int category, alignment;
  
  if (fType) {
    if (cg->theHal->IsTexobjBase(GetBase(fType))) {
      alignment = 4;
    } else {
      category = GetCategory(fType);
      switch (category) {
        case TC_Scalar:
          alignment = 4;
          break;
        case TC_Struct:
        case TC_Array:
          alignment = 4;
          break;
        case TC_Function:
        default:
          alignment = 1;
          break;
      }
    }
  } else {
    alignment = 1;
  }
  return alignment;
} // GetAlignment

/*
 * CheckDeclarators() - Check for profile-specific limitations of declarators.
 *
 */

bool Hal::CheckDeclarators(SourceLoc *loc, const DeclType *fDtype)
{
  int numdims = 0;
  const Type *lType;
  
  lType = fDtype->GetType();
  while (GetCategory(lType) == TC_Array) {
    if (static_cast< const TypeArray * >( lType )->numels == 0 && numdims > 0) {
      SemanticError( cg, loc, ERROR___LOW_DIM_UNSPECIFIED);
      return false;
    }
    if (static_cast< const TypeArray * >( lType )->numels > 4 && IsPacked(lType)) {
      SemanticError( cg, loc, ERROR___PACKED_DIM_EXCEEDS_4);
      return false;
    }
    lType = static_cast< const TypeArray * >( lType )->eltype;
    numdims++;
  }
  if (numdims > 3) {
    SemanticError( cg, loc, ERROR___NUM_DIMS_EXCEEDS_3);
    return false;
  }
  return true;
} // CheckDeclarators

/*
 * IsValidScalarCast() - Is it valid to typecast a scalar from fromBase to toBase?.
 *
 */

bool Hal::IsValidScalarCast(TypeBase toBase, TypeBase fromBase, bool isExplicit)
{
  bool answer;
  
  switch (toBase) {
    case TB_Boolean:
    case TB_Float:
    case TB_Int:
      switch (fromBase) {
        case TB_Cfloat:
        case TB_Cint:
        case TB_Float:
        case TB_Int:
          answer = true;
          break;
        case TB_Boolean:
          answer = (toBase == TB_Boolean) || isExplicit;
          break;
        default:
          answer = false;
          break;
      }
      break;
    case TB_Cfloat:
    case TB_Cint:
    default:
      answer = false;
      break;
  }
  return answer;
} // IsValidScalarCast

/*
 * IsNumericBase() - Is fBase a numeric type?
 *
 */

bool Hal::IsNumericBase(TypeBase fBase)
{
  bool answer;
  
  switch (fBase) {
    case TB_Cfloat:
    case TB_Cint:
    case TB_Float:
    case TB_Int:
      answer = true;
      break;
    default:
      answer = false;
      break;
  }
  return answer;
} // IsNumericBase

/*
 * IsIntegralBase() - Is fBase an integral type?
 *
 */

bool Hal::IsIntegralBase(TypeBase fBase)
{
  bool answer;
  
  switch (fBase) {
    case TB_Cint:
    case TB_Int:
      answer = true;
      break;
    default:
      answer = false;
      break;
  }
  return answer;
} // IsIntegralBase


/*
 * IsValidRuntimeBase() - Are runtime variables with a base of fBase supported?
 *         In other words, can a non-const variable of this base be declared in this profile?
 *
 */

bool Hal::IsValidRuntimeBase(TypeBase fBase)
{
  bool answer;
  
  switch (fBase) {
    case TB_Float:
      answer = true;
      break;
    default:
      answer = false;
      break;
  }
  return answer;
} // IsValidRuntimeBase

/*
 * GetBinOpBase() - Return the base type for this Binary operation.
 *
 */

TypeBase Hal::GetBinOpBase(int lop, TypeBase lbase, TypeBase rbase, int llen, int rlen)
{
  TypeBase result;
  
  switch (lop) {
    case VECTOR_V_OP:
    case MUL_OP:
    case DIV_OP:
    case MOD_OP:
    case ADD_OP:
    case SUB_OP:
    case SHL_OP:
    case SHR_OP:
    case LT_OP:
    case GT_OP:
    case LE_OP:
    case GE_OP:
    case EQ_OP:
    case NE_OP:
    case AND_OP:
    case XOR_OP:
    case OR_OP:
    case COND_OP:
      if (lbase == rbase) {
        result = lbase;
      } else if (lbase == TB_Float || rbase == TB_Float) {
        result = TB_Float;
      } else if (lbase == TB_Cfloat || rbase == TB_Cfloat) {
        if (lbase == TB_Int || rbase == TB_Int) {
          result = TB_Float;
        } else {
          result = TB_Cfloat;
        }
      } else {
        result = TB_Int;
      }
      break;
    default:
      result = TB_NoType;
      break;
  };
  return result;
} // GetBinOpBase

/*
 * ConvertConstant() - Convert a numeric scalar Constant from one base type to another.
 *
 */

bool Hal::ConvertConstant(const ScalarConstant *fval, TypeBase fbase, TypeBase tbase, Expr **fexpr)
{
  Expr *lexpr = NULL;
  
  switch (fbase) {
    case TB_Cfloat:
    case TB_Float:
      switch (tbase) {
        case TB_Cfloat:
        case TB_Float:
          lexpr = (Expr *) NewFConstNode( cg, FCONST_OP, fval->f, tbase);
          *fexpr = lexpr;
          break;
        case TB_Cint:
        case TB_Int:
          lexpr = (Expr *) NewIConstNode( cg, ICONST_OP, (int) fval->f, tbase);
          *fexpr = lexpr;
          break;
        default:
          return false;
      }
      break;
    case TB_Cint:
    case TB_Int:
      switch (tbase) {
        case TB_Cfloat:
        case TB_Float:
          lexpr = (Expr *) NewFConstNode( cg, FCONST_OP, (float) fval->i, tbase);
          *fexpr = lexpr;
          break;
        case TB_Cint:
        case TB_Int:
          lexpr = (Expr *) NewIConstNode( cg, ICONST_OP, fval->i, tbase);
          *fexpr = lexpr;
          break;
        default:
          return false;
      }
      break;
    default:
      return false;
  }
  return true;
} // ConvertConstant


///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////// End of hal.c ////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
