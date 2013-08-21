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
// symbols.c
//

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "slglobals.h"

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Symbol Table Variables: ///////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////


static Atom baseTypeNames[TB_LastUser + 1] = { 0 };
static Type *baseTypes[TB_LastUser + 1] = { NULL };

/************************************ Type Name Error Support ********************************/

/*
 * SetScalarTypeName() - Set a scalar type name.
 *
 */

void SetScalarTypeName(int base, Atom name, Type *fType)
{
  if (base >= 0 && base <= TB_LastUser) {
    baseTypeNames[base] = name;
    baseTypes[base] = fType;
  }
} // SetScalarTypeName

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Symbol Table Fuctions: ////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

void CgContext::StartGlobalScope()
{
  // Create user's global scope:
  globalScope = new Scope( this );
  PushScope( this, globalScope);
} // StartGlobalScope

/*
 * FreeSymbolTree()
 *
 */

static void FreeSybolTree( CgContext *cg, Symbol * s ) {
	if ( s == NULL ) {
		return;
	}
	FreeSybolTree( cg, s->left );
	FreeSybolTree( cg, s->right );
	if ( s->kind == SK_Function ) {
		while( s ) {
			Symbol * ovld = s->details.fun.overload;
			Stmt * st = s->details.fun.statements;
			while ( st ) {
				Stmt * next = st->next;
				delete st;
				st = next;
			}
			delete s;
			s = ovld;
		}
	}
	delete s;
}


static void unlinkScope( CgContext *cg, void *_scope) {
  Scope *scope = (Scope *)_scope;
  
  if (scope->next)
    scope->next->prev = scope->prev;
  if (scope->prev)
    scope->prev->next = scope->next;
  else
    cg->scopeList = scope->next;
}


Scope::Scope( CgContext *sCg )
: cg( sCg )
, next( NULL ), prev( NULL ), parent( NULL )
, funScope( NULL ), symbols( NULL ), tags( NULL )
, params( NULL ), returnType( NULL ), level( 0 )
, funindex( 0 ), InFormalParameters( 0 )
, HasVoidParameter( false ), HasReturnStmt( false )
, IsStructScope( false ), HasSemantics( false )
, pid( PID_NONE_ID ), programs( NULL )
, initStmts( NULL )
{
	next = cg->scopeList;
	if ( next ) {
		next->prev = this;
	}
	cg->scopeList = this;
}

Scope::~Scope() {
  
	FreeSybolTree( cg, symbols );
	FreeSybolTree( cg, tags );
  
	while( programs ) {
		SymbolList *next = programs->next;
		delete programs;
		programs = next;
	}
  
}



/*
 * PushScope()
 *
 */

void PushScope( CgContext *cg, Scope *fScope)
{
  Scope *lScope;
  
  if (cg->currentScope) {
    fScope->level = cg->currentScope->level + 1;
    if (fScope->level == 1) {
      if (! cg->globalScope) {
        /* HACK - CTD -- if globalScope==NULL and level==1, we're
         * defining a function in the superglobal scope.  Things
         * will break if we leave the level as 1, so we arbitrarily
         * set it to 2 */
        fScope->level = 2;
      }
    }
    if (fScope->level >= 2) {
      lScope = fScope;
      while (lScope->level > 2)
        lScope = lScope->next;
      fScope->funScope = lScope;
    }
  } else {
    fScope->level = 0;
  }
  fScope->parent = cg->currentScope;
  cg->currentScope = fScope;
} // PushScope

/*
 * PopScope()
 *
 */

Scope *PopScope( CgContext *cg )
{
  Scope *lScope;
  
  lScope = cg->currentScope;
  if (cg->currentScope)
    cg->currentScope = cg->currentScope->parent;
  return lScope;
} // PopScope

/*
 * NewSymbol() - Allocate a new symbol node;
 *
 */

Symbol *NewSymbol( CgContext *cg, SourceLoc *loc, Scope *fScope, Atom name, Type *fType, SymbolKind kind)
{
  Symbol *lSymb;
  char *pch;
  int ii;
	
  lSymb = new Symbol(); //(Symbol *) mem_Alloc(fScope->pool, sizeof(Symbol));
  lSymb->name = name;
  lSymb->type = fType;
  lSymb->loc = *loc;
  lSymb->kind = kind;
  
  // Clear union area:
  pch = (char *) &lSymb->details;
  for (ii = 0; ii < sizeof(lSymb->details); ii++)
    *pch++ = 0;
  return lSymb;
} // NewSymbol

/*
 * lAddToTree() - Using a Binary tree is not a good idea for basic atom values because they
 *         are generated in order.  We'll fix this later (by reversing the bit pattern).
 */

static void lAddToTree( CgContext *cg, Symbol **fSymbols, Symbol *fSymb, Type *fType)
{
  Symbol *lSymb = *fSymbols;
  
  if (lSymb) {
    Atom f = fSymb->name;
    while (lSymb) {
      Atom l = lSymb->name;
      if (l == f) {
        InternalError( cg, cg->tokenLoc, 9999, "symbol \"%s\" already in table",
                      cg->GetString(fSymb->name));
        break;
      } else {
        if (f < l) {
          if (lSymb->left) {
            lSymb = lSymb->left;
          } else {
            lSymb->left = fSymb;
            break;
          }
        } else {
          if (lSymb->right) {
            lSymb = lSymb->right;
          } else {
            lSymb->right = fSymb;
            break;
          }
        }
      }
    }
  } else {
    *fSymbols = fSymb;
  }
} // lAddToTree


/*
 * AddSymbol() - Add a variable, type, or function name to a scope.
 *
 */

Symbol *AddSymbol( CgContext *cg, SourceLoc *loc, Scope *fScope, Atom atom, Type *fType, SymbolKind kind)
{
  Symbol *lSymb;
	
  if (!fScope)
    fScope = cg->currentScope;
  lSymb = NewSymbol( cg, loc, fScope, atom, fType, kind);
  lAddToTree( cg, &fScope->symbols, lSymb, fType);
  return lSymb;
} // AddSymbol

/*
 * UniqueSymbol() - Add a symbol to fScope that is different from
 * every other symbol.  Useful for compiler generated temporaries.
 *
 */

Symbol *UniqueSymbol( CgContext *cg, Scope *fScope, Type *fType, SymbolKind kind)
{
  static int nextTmp = 0;
  static SourceLoc tmpLoc = { 0, 0 };
  char buf[256];
  Atom atom;
  
  sprintf(buf, "@TMP%d", nextTmp++);
  atom = cg->GetAtom(buf);
  return AddSymbol( cg, &tmpLoc, fScope, atom, fType, kind);
} // UniqueSymbol


/*
 * AddTag() - Add a tag name to a scope.
 *
 */

Symbol *AddTag( CgContext *cg, SourceLoc *loc, Scope *fScope, Atom atom, TypeCategory category)
{
  Symbol *lSymb;
  Type *pType;
  
  if (!fScope)
    fScope = cg->currentScope;
  pType = NewType( cg,atom, TB_NoType, category, TD_Unknown, TQ_None, false, 0, 0);
  static_cast< TypeStruct * >( pType )->unqualifiedtype = pType;
  lSymb = NewSymbol( cg, loc, fScope, atom, pType, SK_Tag);
  lAddToTree( cg, &fScope->tags, lSymb, pType);
  return lSymb;
} // AddTag

/*********************************************************************************************/
/***************************************** Type Functions ************************************/
/*********************************************************************************************/

/*
 * NewType() - Allocate a new type struct.
 *
 */

Type *NewType( CgContext *cg, Atom n, TypeBase b, TypeCategory c, TypeDomain d, TypeQualifier q, bool isConst, int properties, int size)
{
	switch( c ) {
		case TC_Scalar:
			return new TypeScalar( cg, n, b, d, q, isConst, properties, size );
		case TC_Array:
			return new TypeArray( cg, n, b, d, q, isConst, properties, size );
		case TC_Struct:
			return new TypeStruct( cg, n, b, d, q, isConst, properties, size );
		case TC_Function:
			return new TypeFunction( cg, n, b, d, q, isConst, properties, size );
		default:
			assert( 0 && "invalid Type category");
			break;
	}
  
  return NULL;
} // NewType

/*
 * DupType() - Duplicate a type struct.
 *
 */

Type *DupType(const Type *t)
{
	if ( t ) {
		switch( GetCategory( t ) ) {
      case TC_Scalar:
			{
				TypeScalar *r = new TypeScalar( t->cg, t->name, t->base, t->domain, t->qualifier, t->isConst, t->properties, t->size );
				return r;
			}
      case TC_Array:
			{
				const TypeArray *ta = static_cast< const TypeArray * >( t );
				TypeArray *r = new TypeArray( t->cg, t->name, t->base, t->domain, t->qualifier, t->isConst, t->properties, t->size );
				r->eltype = ta->eltype;
				r->numels = ta->numels;
				return r;
			}
      case TC_Struct:
			{
				const TypeStruct *ts = static_cast< const TypeStruct * >( t );
				TypeStruct *r = new TypeStruct( t->cg, t->name, t->base, t->domain, t->qualifier, t->isConst, t->properties, t->size );
				r->allocated = ts->allocated; // pointer copy - what kind of memory is this pointing to?
				r->csize = ts->csize;
				r->HasSemantics = ts->HasSemantics;
				r->loc = ts->loc;
				r->members = ts->members;
				r->semantics = ts->semantics;
				r->tag = ts->tag;
				r->unqualifiedtype = ts->unqualifiedtype; // deep copy?
				r->variety = ts->variety;
				return r;
			}
      case TC_Function:
			{
				const TypeFunction *tf = static_cast< const TypeFunction * >( t );
				TypeFunction *r = new TypeFunction( t->cg, t->name, t->base, t->domain, t->qualifier, t->isConst, t->properties, t->size );
				r->rettype = tf->rettype;
				// list copy
				TypeList *rtl = tf->paramtypes;
				TypeList *ltl = NULL;
				while( rtl ) {
					TypeList *ntl = new TypeList(*rtl);
					if ( r->paramtypes == NULL ) {
						r->paramtypes = ntl;
					} else {
						ltl->next = ntl;
					}
					ltl = ntl;
					rtl = rtl->next;
				}
				
				return r;
			}
      default:
        assert( 0 && "invalid Type category");
        break;
		}
	}
	return NULL;
} // DupType

/*
 * NewPackedArrayType() - Define a new packed (vector) array type.
 *
 */

Type *NewPackedArrayType( CgContext *cg, Type *elType, int numels, int properties)
{
  Type *lType;
  
  lType = NewType( cg,elType->name, GetBase( elType ), TC_Array, TD_Unknown, TQ_None, elType->isConst, TYPE_MISC_PACKED | properties, 0);
	TypeArray *arrType = static_cast< TypeArray * >( lType );
  arrType->eltype = elType;
  arrType->numels = numels;
  arrType->size = cg->theHal->GetSizeof(lType);
  return lType;
} // NewPakedArrayType

/*************************************** Category Functions **********************************/

/*
 * IsCategory() - See if a type is of the given category.
 *
 */

bool IsCategory(const Type *fType, TypeCategory category)
{
  return ( fType && ( fType->category == category ) );
} // IsCategory

/*
 * IsTypeBase() - See if a type is of the given base.
 *
 */

bool IsTypeBase(const Type *fType, TypeBase base)
{
  return ( fType && ( fType->base == base ) );
} // IsTypeBase

/*
 * IsVoid() - Returns TRUE if a void type.
 *
 */

bool IsVoid(const Type *fType)
{
  return ( fType && ( fType->properties & TYPE_MISC_VOID ) );
} // IsVoid

/*
 * IsBoolean() - Returns TRUE if a Boolean type.
 *
 */

bool IsBoolean(const Type *fType)
{
  return ( fType && ( fType->base == TB_Boolean ) );
} // IsBoolean

/*
 * IsScalar() - Returns TRUE if a scalar type.
 *
 */

bool IsScalar(const Type *fType)
{
  return ( fType && (fType->category == TC_Scalar ) );
} // IsScalar

/*
 * IsArray() - Returns TRUE if a packed or unpacked array type.
 *
 */

bool IsArray(const Type *fType)
{
  return ( fType && ( fType->category == TC_Array ) );
} // IsScalar

/*
 * IsVector() - Returns TRUE if a vector type.
 *
 */

bool IsVector(const Type *fType, int *len)
{
  if (fType &&
      fType->category == TC_Array &&
      (fType->properties & TYPE_MISC_PACKED) )
	{
    const TypeArray * arrType = static_cast< const TypeArray * >( fType );
		if( !IsArray( arrType->eltype ) )
		{
			if (len) {
				*len = arrType->numels;
			}
			return true;
		}
  }
  return false;
} // IsVector

/*
 * IsMatrix() - Returns TRUE if a matrix type.
 *
 */

bool IsMatrix(const Type *fType, int *len, int *len2)
{
  if (fType &&
      fType->category == TC_Array &&
      (fType->properties & TYPE_MISC_PACKED) &&
      IsVector(static_cast< const TypeArray * >( fType )->eltype, len))
  {
    if (len2)
      *len2 = static_cast< const TypeArray * >( fType )->numels;
    return true;
  }
	return false;
} // IsMatrix

/*
 * IsUnsizedArray() - Returns TRUE if an array with an unspecified number of elements.
 *
 */

bool IsUnsizedArray(const Type *fType)
{
  return ( GetCategory(fType) == TC_Array &&
          static_cast< const TypeArray * >( fType )->numels == 0);
} // IsUnsizedArray

/*
 * IsStruct() - Returns TRUE if a struct.
 *
 */

bool IsStruct(const Type *fType)
{
  return ( fType && GetCategory(fType) == TC_Struct );
} // IsStruct

/*
 * IsProgram() - See if a type is a program.
 *
 */

bool IsProgram(const Type *fType)
{
  return ( fType && ( fType->properties & TYPE_MISC_PROGRAM ) );
} // IsProgram

/*
 * IsPacked()
 *
 */

bool IsPacked(const Type *fType)
{
  return ( fType && ( fType->properties & TYPE_MISC_PACKED ) );
} // IsPacked

/*
 * IsSameUnqualifiedType() - Returns TRUE if the unqualified types aType and bType are the same.
 *
 */

bool IsSameUnqualifiedType(const Type *aType, const Type *bType)
{
  //const int UnQMask = TB_MASK | TC_MASK ; // 020122 // | TD_MASK;
  
  if (aType == bType) {
    return 1;
  } else {
    if ( aType->base == bType->base && aType->category == bType->category ) {
      // }&& aType->qualifier == bType->qualifier ) { // Why was I checking qualifier for unqualified compare?
      switch ( aType->category ) {
        case TC_Scalar:
          return true;
        case TC_Array:
				{
					const TypeArray * a = static_cast< const TypeArray * >( aType );
					const TypeArray * b = static_cast< const TypeArray * >( bType );
					if ( a->numels == b->numels) {
						// Should we check for Packed here??? I think so!
						return IsSameUnqualifiedType( a->eltype, b->eltype);
					}
				}
          break;
        case TC_Function:
          break;
        case TC_Struct:
				{
					const TypeStruct * a = static_cast< const TypeStruct * >( aType );
					const TypeStruct * b = static_cast< const TypeStruct * >( bType );
					if ( a->unqualifiedtype == b->unqualifiedtype ) {
						return true;
					}
				}
        default:
          break;
      }
    }
  }
  return false;
} // IsSameUnqualifiedType

/*
 * IsTypedef() - See if a symbol is a typedef.
 *
 */

bool IsTypedef(const Symbol *fSymb)
{
  return (fSymb && fSymb->kind == SK_Typedef);
} // IsTypedef

/*
 * IsFunction() - See if a symbol is a function.
 *
 */

bool IsFunction(const Symbol *fSymb)
{
  return (fSymb && fSymb->kind == SK_Function);
} // IsFunction

/*
 * IsInline() - See if a symbol is an inline function.
 *
 */

bool IsInline(const Symbol *fSymb)
{
  return (fSymb && fSymb->kind == SK_Function && (fSymb->properties & SYMB_IS_INLINE_FUNCTION));
} // IsInline

/*
 * GetBase() - Return the base attributes of a type.
 *
 */

TypeBase GetBase(const Type *fType)
{
  if (fType) {
    return fType->base;
  } else {
    return TB_NoType;
  }
} // GetBase

/*
 * GetCategory() - Return the categpry of a type.
 *
 */

TypeCategory GetCategory(const Type *fType)
{
  if (fType) {
    return fType->category;
  } else {
    return TC_None;
  }
} // GetCategory

/*
 * GetDomain() - Return the domain of a type.
 *
 */

TypeDomain GetDomain(const Type *fType)
{
  if (fType) {
    return fType->domain;
  } else {
    return TD_Unknown;
  }
} // GetDomain

/*
 * GetQualifiers() - Return a type's qualifiers.
 *
 */

TypeQualifier GetQualifiers(const Type *fType)
{
  if (fType) {
    return fType->qualifier;
  } else {
    return TQ_None;
  }
} // GetQualifiers

/*
 * GetQuadRegSize() - Return the number of quad registers required to hold an object
 *         of this type.  Minimum size is 1.
 */

int GetQuadRegSize(const Type *fType)
{
  if (fType) {
    return ( fType->size + 3) >> 2;
  } else {
    return 1;
  }
} // GetQuadRegSize

/*********************************************************************************************/
/************************************ Symbol Semantic Functions ******************************/
/*********************************************************************************************/

/*
 * LookupLocalSymbol()
 *
 */

Symbol *LookupLocalSymbol( CgContext *cg, Scope *fScope, Atom atom)
{
  Symbol *lSymb;
  Atom rname, ratom;
  
  ratom = atom; // no reverse anymore
  if (!fScope)
    fScope = cg->currentScope;
  lSymb = fScope->symbols;
  while (lSymb) {
    rname = lSymb->name; // no reverse anymore
    if (rname == ratom) {
      return lSymb;
    } else {
      if (rname > ratom) {
        lSymb = lSymb->left;
      } else {
        lSymb = lSymb->right;
      }
    }
  }
  return NULL;
} // LookupLocalSymbol

/*
 * LookupLocalSymbolBySemanticName() - Lookup a symbol in a local tree by the : semantic name.
 *
 * Note:  The tree is not ordered for this lookup so the next field is used.  This only works
 * for structs and other scopes that maintain this list.
 *
 * Note: There can be multiple matches.  Returns the first.
 *
 */

Symbol *LookupLocalSymbolBySemanticName(Scope *fScope, Atom atom)
{
  Symbol *lSymb;
  
  if (!fScope)
    return NULL;
  lSymb = fScope->symbols;
  while (lSymb) {
    if (lSymb->kind == SK_Variable) {
      if (lSymb->details.var.semantics == atom)
        return lSymb;
    }
    lSymb = lSymb->next;
  }
  return NULL;
} // LookupLocalSymbolBySemanticName

/*
 * LookupLocalSymbolByBindingName() - Lookup a symbol in a local tree by the lname in the
 *         semantic binding structure.
 *
 * Note:  The tree is not ordered for this lookup so the next field is used.  This only works
 * for structs and other scopes that maintain this list.
 *
 * Note: There can be multiple matches.  Returns the first.
 *
 */

Symbol *LookupLocalSymbolByBindingName(Scope *fScope, Atom atom)
{
  Symbol *lSymb;
  
  if (!fScope)
    return NULL;
  lSymb = fScope->symbols;
  while (lSymb) {
    if (lSymb->kind == SK_Variable && lSymb->details.var.bind) {
      if (lSymb->details.var.bind->lname == atom)
        return lSymb;
    }
    lSymb = lSymb->next;
  }
  return NULL;
} // LookupLocalSymbolByBindingName

/*
 * LookupLocalTag()
 *
 */

Symbol *LookupLocalTag( CgContext *cg, Scope *fScope, Atom atom)
{
  Symbol *lSymb;
  Atom n, a;
  
  a = atom; // no reverse anymore
  if (!fScope)
    fScope = cg->currentScope;
  lSymb = fScope->tags;
  while (lSymb) {
    n = lSymb->name; // no reverse anymore
    if ( n == a ) {
      return lSymb;
    } else {
      if (a < n) {
        lSymb = lSymb->left;
      } else {
        lSymb = lSymb->right;
      }
    }
  }
  return NULL;
} // LookupLocalTag

/*
 * LookupSymbol()
 *
 */

Symbol *LookupSymbol( CgContext *cg, Scope *fScope, Atom atom)
{
  Symbol *lSymb;
  
  if (!fScope)
    fScope = cg->currentScope;
  while (fScope) {
    lSymb = LookupLocalSymbol( cg, fScope, atom);
    if (lSymb)
      return lSymb;
    fScope = fScope->parent;
  }
  return NULL;
} // LookupSymbol

/*
 * LookupTag()
 *
 */

Symbol *LookupTag( CgContext *cg, Scope *fScope, Atom atom)
{
  Symbol *lSymb;
  
  if (!fScope)
    fScope = cg->currentScope;
  while (fScope) {
    lSymb = LookupLocalTag( cg, fScope, atom);
    if (lSymb)
      return lSymb;
    fScope = fScope->parent;
  }
  return NULL;
} // LookupTag

/*
 * LookupTypeSymbol()
 *
 */

Type *LookupTypeSymbol( CgContext *cg, Scope *fScope, Atom atom)
{
  Symbol *lSymb;
  Type *lType;
  
  lSymb = LookupSymbol( cg, fScope, atom);
  if (lSymb) {
    if (!IsTypedef(lSymb)) {
      InternalError( cg, cg->tokenLoc, ERROR_S_NAME_NOT_A_TYPE,
                    cg->GetString(atom));
      return cg->UndefinedType;
    }
    lType = lSymb->type;
    if (lType) {
      return lType;
    } else {
      return cg->UndefinedType;
    }
  } else {
    InternalError( cg, cg->tokenLoc, ERROR_S_TYPE_NAME_NOT_FOUND,
                  cg->GetString(atom));
    return cg->UndefinedType;
  }
} // LookupTypeSymbol

/*
 * GetStandardType()
 *
 * Scalar: len = 0.
 * Vector: len >= 1 and len2 = 0
 * Matrix: len >= 1 and len2 >= 1
 *
 * len = 1 means "float f[1]" not "float f"
 * Vector and matrix types are PACKED.
 *
 */

Type *GetStandardType(CgContext *cg, TypeBase tbase, int tlen, int tlen2)
{
  Type *lType, *nType;
  
  if (tbase >= 0 && tbase <= TB_LastUser) {
    lType = baseTypes[tbase];
    if (tlen > 0) {
      // Put these in a table, too!!! XYZZY !!!
      nType = NewType( cg, lType->name, tbase, TC_Array, TD_Unknown, TQ_None, false, TYPE_MISC_PACKED, 0);
			nType->base = tbase;
      TypeArray * arrType = static_cast< TypeArray * >( nType );
			arrType->eltype = lType;
      arrType->numels = tlen;
      arrType->size = cg->theHal->GetSizeof(nType);
      lType = nType;
      if (tlen2 > 0) {
        // Put these in a table, too!!! XYZZY !!!
        nType = NewType( cg, lType->name, tbase, TC_Array, TD_Unknown, TQ_None, false, TYPE_MISC_PACKED, 0);
				TypeArray * arrType = static_cast< TypeArray * >( nType );
        arrType->eltype = lType;
        arrType->numels = tlen2;
        arrType->size = cg->theHal->GetSizeof(nType);
        lType = nType;
      }
    }
  } else {
    lType = cg->UndefinedType;
  }
  return lType;
} // GetStandardType

/*
 * GetElementType() - Return a pointer to the type of elements stored in this array.
 *
 */

Type *GetElementType( CgContext *cg, const Type *fType)
{
  Type *lType;
  
  if (GetCategory(fType) == TC_Array) {
    lType = static_cast< const TypeArray * >( fType )->eltype;
#if 0000
    if ((fType->isConst) &&
        !(lType->isConst))
    {
      lType = DupType(lType);
      lType->isConst = true;
    }
#endif
  } else {
    InternalError( cg, cg->tokenLoc, ERROR___TYPE_NOT_ARRAY);
    lType = cg->UndefinedType;
  }
  return lType;
} // GetElementType

/*
 * SetMemberOffsets() - Assign offsets to members for use by code generators.
 *
 */

void SetStructMemberOffsets( CgContext *cg, Type *fType)
{
  int addr, size, alignment;
  Symbol *lSymb;
  
  addr = 0;
  lSymb = static_cast< TypeStruct * >( fType )->members->symbols;
  while (lSymb) {
    alignment = cg->theHal->GetAlignment(lSymb->type);
    size = cg->theHal->GetSizeof(lSymb->type);
    addr = ((addr + alignment - 1)/alignment)*alignment;
    lSymb->details.var.addr = addr;
    addr += size;
    lSymb = lSymb->next;
  }
  fType->size = ((addr + 3)/4)*4;
} // SetStructMemberOffsets

/*
 * SetStructMembers() - Set the member tree of a structure.
 *
 */

Type *SetStructMembers( CgContext *cg, SourceLoc *loc, Type *fType, Scope *members)
{
  Symbol *lSymb;
  const char *tagname;
  
  if (fType) {
    if (static_cast< TypeStruct * >( fType )->members) {
      SemanticError( cg, loc, ERROR_SSD_STRUCT_ALREADY_DEFINED,
                    cg->GetString(static_cast< TypeStruct * >( fType )->tag),
                    cg->GetString(static_cast< TypeStruct * >( fType )->loc.file),
                    static_cast< TypeStruct * >( fType )->loc.line);
    } else {
      if (static_cast< TypeStruct * >( fType )->tag.IsValid()) {
        tagname = cg->GetString(static_cast< TypeStruct * >( fType )->tag);
      } else {
        tagname = "<no-name>";
      }
      static_cast< TypeStruct * >( fType )->members = members;
      static_cast< TypeStruct * >( fType )->loc = *loc;
      static_cast< TypeStruct * >( fType )->HasSemantics = members->HasSemantics;
      SetStructMemberOffsets( cg, fType);
      if (static_cast< TypeStruct * >( fType )->tag.IsValid()) {
        lSymb = LookupLocalSymbol( cg, cg->currentScope, static_cast< TypeStruct * >( fType )->tag);
        if (!lSymb) {
          lSymb = DefineTypedef( cg, loc, cg->currentScope, static_cast< TypeStruct * >( fType )->tag, fType);
        } else {
          if (IsCategory(fType, TC_Struct)) {
            if (!IsCategory(lSymb->type, TC_Struct)) {
              SemanticError( cg, loc, ERROR_S_NAME_ALREADY_DEFINED, tagname);
            }
          }
        }
      }
    }
  }
  return fType;
} // SetStructMembers

/*
 * AddParameter() - Add a parameter to a function's formal parameter list, or a member to a
 *         struct or connector's member list.
 */

void AddParameter( CgContext *cg, Scope *fScope, Symbol *param)
{
  Symbol *lSymb = fScope->params;
  
  if (lSymb) {
    while (lSymb->next)
      lSymb = lSymb->next;
    lSymb->next = param;
  } else {
    fScope->params = param;
  }
} // AddParameter

///////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////// Various Support Functions: /////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * GetSwizzleOrWriteMask() - Build a swizzle mask out of the letters in an identifier.
 *
 */

int GetSwizzleOrWriteMask( CgContext *cg, SourceLoc *loc, Atom atom, int *FIsLValue, int *flen)
{
  const char *s, *t;
  int len, bit, mask, bits;
  int groups, group;
  int LIsLValue;
  char ch;
  
  s = t = cg->GetString(atom);
  len = mask = bits = groups = 0;
  LIsLValue = 1;
  while (*s) {
    ch = *s++;
    switch (ch) {
      case 'x':
        bit = 0;
        group = 1;
        break;
      case 'y':
        bit = 1;
        group = 1;
        break;
      case 'z':
        bit = 2;
        group = 1;
        break;
      case 'w':
        bit = 3;
        group = 1;
        break;
      case 'r':
        bit = 0;
        group = 2;
        break;
      case 'g':
        bit = 1;
        group = 2;
        break;
      case 'b':
        bit = 2;
        group = 2;
        break;
      case 'a':
        bit = 3;
        group = 2;
        break;
      default:
        SemanticError( cg, loc, ERROR_CS_SK_InvalidWIZZLE_CHAR, ch, t);
        return mask;
        break;
    }
    mask |= bit << len*2;
    bit = 1 << bit;
    if (bits & bit)
      LIsLValue = 0;
    bits |= bit;
    if (groups && groups != group) {
      SemanticError( cg, loc, ERROR_CS_SK_InvalidWIZZLE_CHAR, ch, t);
      return mask;
    }
    groups |= group;
    len++;
  }
  if (len > 4)
    SemanticError( cg, loc, ERROR_S_SWIZZLE_TOO_LONG, t);
  if (FIsLValue)
    *FIsLValue = LIsLValue;
  if (flen)
    *flen = len;
  return mask;
} // GetSwizzleOrWriteMask

/*
 * GetMatrixSwizzleOrWriteMask() - Build a matrix swizzle mask out of the letters in an identifier.
 *
 */

int GetMatrixSwizzleOrWriteMask( CgContext *cg, SourceLoc *loc, Atom atom, int *FIsLValue, int *flen)
{
  const char *s, *t;
  int len, bit, mask, bits, base;
  int LIsLValue, Error;
  char lch, ch;
  
  s = t = cg->GetString(atom);
  len = mask = bits = 0;
  LIsLValue = 1;
  if (s[0] == '_' && s[1] != '\0') {
    Error = 0;
    if (s[1] == 'm') {
      base = 0;
    } else {
      base = 1;
    }
    while (*s) {
      ch = lch = *s++;
      if (ch == '_') {
        if (base == 0) {
          if (*s++ != 'm') {
            Error = 1;
            break;
          }
        }
        lch = *s++;
        ch = lch - base;
        if (ch >= '0' && ch <= '3') {
          bit = (ch - '0') << 2;
          lch = *s++;
          ch = lch - base;
          if (ch >= '0' && ch <= '3') {
            bit = bit | (ch - '0');
            mask |= bit << len*4;
            bit = 1 << bit;
            if (bit & bits)
              LIsLValue = 0;
            bits |= bit;
            len++;
          } else {
            Error = 1;
            break;
          }
        } else {
          Error = 1;
          break;
        }
      } else {
        Error = 1;
        break;
      }
    }
  } else {
    lch = *s;
    Error = 1;
  }
  if (Error) {
    SemanticError( cg, loc, ERROR_CS_SK_InvalidWIZZLE_CHAR, lch, t);
  }
  if (len > 4)
    SemanticError( cg, loc, ERROR_S_SWIZZLE_TOO_LONG, t);
  if (FIsLValue)
    *FIsLValue = LIsLValue;
  if (flen)
    *flen = len;
  return mask;
} // GetMatrixSwizzleOrWriteMask

/*
 * GetBaseTypeNameString() - Return a pointer to a string representation of a base type name.
 *
 */

const char *GetBaseTypeNameString( CgContext *cg, int base)
{
  if (base >= 0 && base <= TB_LastUser) {
    return cg->GetString(baseTypeNames[base]);
  } else {
    return "*** bad base value ***";
  }
} // GetBaseTypeNameString

/*
 * ClearSymbolTempptr() - Clear the tempptr for all symbols in this tree.
 *
 */

static void ClearSymbolTempptr(Symbol *fSymb)
{
  if (fSymb) {
    fSymb->tempptr = NULL;
    ClearSymbolTempptr(fSymb->left);
    ClearSymbolTempptr(fSymb->right);
  }
} // ClearSymbolTempptr

/*
 * ClearSymbolTempptrList() - Walk a list of scopes and
 *                            clear tempptr field for all symbols.
 *
 */

static void ClearSymbolTempptrList(Scope *fScope)
{
  while (fScope) {
    ClearSymbolTempptr(fScope->symbols);
    fScope = fScope->next;
  }
} // ClearSymbolTempptrList

/*
 * ClearAllSymbolTempptr
 *
 */

void ClearAllSymbolTempptr( CgContext *cg )
{
  ClearSymbolTempptrList( cg->scopeList);
} // ClearSymbolTempptr


/*
 * ClearSymbolTempptr2() - Clear the tempptr2 for all symbols in this tree.
 *
 */

static void ClearSymbolTempptr2(Symbol *fSymb)
{
  if (fSymb) {
    fSymb->tempptr2 = NULL;
    ClearSymbolTempptr2(fSymb->left);
    ClearSymbolTempptr2(fSymb->right);
  }
} // ClearSymbolTempptr2

/*
 * ClearSymbolTempptr2List() - Walk a list of scopes and
 *                            clear tempptr field for all symbols.
 *
 */

static void ClearSymbolTempptr2List(Scope *fScope)
{
  while (fScope) {
    ClearSymbolTempptr2(fScope->symbols);
    fScope = fScope->next;
  }
} // ClearSymbolTempptr2List

/*
 * ClearAllSymbolTempptr2
 *
 */

void ClearAllSymbolTempptr2( CgContext * cg )
{
  ClearSymbolTempptr2List( cg->scopeList);
} // ClearSymbolTempptr2


// Type

Type::Type( CgContext *tCg, Atom tName, TypeBase tBase, TypeCategory tCategory, TypeDomain tDomain, TypeQualifier tQualifier, bool isconst, int tProps, int tSize )
: cg( tCg ), name( tName ), base( tBase ), category( tCategory ), domain( tDomain ), qualifier( tQualifier )
, isConst( isconst ), properties( tProps ), size( tSize )
{
	sub.type = this;
	cg->AddType( this );
}
Type::~Type() {
	cg->RemoveType( this );
}

void CgContext::AddType( Type * type ) {
	assert( types.count( type ) == 0 );
	types.insert( type );
}

void CgContext::RemoveType( Type * type ) {
	assert( types.count( type ) > 0 );
	types.erase( type );
}


// Symbol

Symbol::~Symbol() {
	switch( kind ) {
		case SK_Macro:
			delete details.mac.body;
			break;
		case SK_Variable:
			delete details.var.init;
			delete details.var.bind;
			break;
		default:
			break;
	}
}


TypeFunction::~TypeFunction() {
	delete paramtypes;
}

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////// End of symbols.c //////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
