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
// glslesv_hal.c
//

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "slglobals.h"
#include "glslesv_hal.h"

#include <map>
using namespace std;


#define NUMELS(x) (sizeof(x) / sizeof((x)[0]))

// These define all the input connector registers for this profile
// you can have multiple names that refer to the same register number
static ConnectorRegisters inputCRegs_glslesv[] = {
  ConnectorRegisters( "ATTR0",  0, TB_Float, REG_AP2V_ATTR0,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR1",  0, TB_Float, REG_AP2V_ATTR1,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR2",  0, TB_Float, REG_AP2V_ATTR2,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR3",  0, TB_Float, REG_AP2V_ATTR3,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR4",  0, TB_Float, REG_AP2V_ATTR4,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR5",  0, TB_Float, REG_AP2V_ATTR5,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR6",  0, TB_Float, REG_AP2V_ATTR6,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR7",  0, TB_Float, REG_AP2V_ATTR7,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR8",  0, TB_Float, REG_AP2V_ATTR8,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR9",  0, TB_Float, REG_AP2V_ATTR9,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR10", 0, TB_Float, REG_AP2V_ATTR10, 4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR11", 0, TB_Float, REG_AP2V_ATTR11, 4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR12", 0, TB_Float, REG_AP2V_ATTR12, 4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR13", 0, TB_Float, REG_AP2V_ATTR13, 4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR14", 0, TB_Float, REG_AP2V_ATTR14, 4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR15", 0, TB_Float, REG_AP2V_ATTR15, 4, REG_ALLOC | REG_INPUT ),
};

static ConnectorRegisters outputCRegs_glslesv[] = {
  // These are output register names
  ConnectorRegisters( "HPOS",  0, TB_Float, REG_V2FR_HPOS,  4, REG_RESERVED | REG_OUTPUT | REG_WRITE_REQUIRED ),
  ConnectorRegisters( "COL0",  0, TB_Float, REG_V2FR_COL0,  4, REG_RESERVED | REG_OUTPUT ),
  ConnectorRegisters( "COL1",  0, TB_Float, REG_V2FR_COL1,  4, REG_RESERVED | REG_OUTPUT ),
  ConnectorRegisters( "TEX0",  0, TB_Float, REG_V2FR_TEX0,  4, REG_ALLOC | REG_OUTPUT ),
  ConnectorRegisters( "TEX1",  0, TB_Float, REG_V2FR_TEX1,  4, REG_ALLOC | REG_OUTPUT ),
  ConnectorRegisters( "TEX2",  0, TB_Float, REG_V2FR_TEX2,  4, REG_ALLOC | REG_OUTPUT ),
  ConnectorRegisters( "TEX3",  0, TB_Float, REG_V2FR_TEX3,  4, REG_ALLOC | REG_OUTPUT ),
  ConnectorRegisters( "FOGC",  0, TB_Float, REG_V2FR_FOGC,  1, REG_RESERVED | REG_OUTPUT ),
  ConnectorRegisters( "PSIZ",  0, TB_Float, REG_V2FR_PSIZ,  1, REG_RESERVED | REG_OUTPUT )
};


// Semantics:
enum { AP2V_GROUP = 0, V2FR_GROUP = 1, };

static SemanticsDescriptor Semantics_glslesv[] = {
  // These are semantics that can be attached to varying variables and
  // parameters.  They usually correspond to the input and output registers
  // defined above, but don't have to.  You can add multiple names for the
  // the same thing as aliases
  // Varying input semantics:
  SemanticsDescriptor( "ATTRIB",   TB_Float, 4, REG_AP2V_ATTR0, 16, AP2V_GROUP, SEM_IN | SEM_VARYING ),
  // Varying output semantics:
  SemanticsDescriptor( "POSITION", TB_Float, 4, REG_V2FR_HPOS, 1, V2FR_GROUP, SEM_OUT | SEM_VARYING ),
  SemanticsDescriptor( "FOG",      TB_Float, 1, REG_V2FR_FOGC, 0, V2FR_GROUP, SEM_OUT | SEM_VARYING ),
  SemanticsDescriptor( "COLOR",    TB_Float, 4, REG_V2FR_COL0, 2, V2FR_GROUP, SEM_OUT | SEM_VARYING ),
  SemanticsDescriptor( "PSIZE",    TB_Float, 1, REG_V2FR_PSIZ, 0, V2FR_GROUP, SEM_OUT | SEM_VARYING ),
  SemanticsDescriptor( "TEXCOORD", TB_Float, 4, REG_V2FR_TEX0, 4, V2FR_GROUP, SEM_OUT | SEM_VARYING )
};


// These are the connector types which refer to the register names above
static ConnectorDescriptor connectors_glslesv[] = {
  ConnectorDescriptor( CID_GLSLESV_IN_NAME,  0, CID_GLSLESV_IN_ID,  CONNECTOR_IS_INPUT,  NUMELS(inputCRegs_glslesv),  inputCRegs_glslesv ),
  
  ConnectorDescriptor( CID_GLSLESV_OUT_NAME, 0, CID_GLSLESV_OUT_ID, CONNECTOR_IS_OUTPUT, NUMELS(outputCRegs_glslesv), outputCRegs_glslesv )
};

///////////////////////////////////////////////////////////////////////////////
/////////////////////////// Glsles output Program ////////////////////////////
///////////////////////////////////////////////////////////////////////////////


struct GlslesvHal : public Hal {
private:
  void PrintFunctions( CgContext *cg, Symbol *symb, Symbol *program );
  
public:
  GlslesvHal( CgContext *cg, Atom hEntryName );
  
	// pure methods
	virtual bool RegisterNames();
	virtual int GetConnectorID(Atom);
	virtual Atom GetConnectorAtom(int);
	virtual bool PrintCodeHeader();
	virtual bool GenerateCode(SourceLoc *loc, Scope *fScope, Symbol *program);
  
	// non-pure methods
	virtual int GetConnectorUses( int cid, int pid );
	virtual bool GetConnectorRegister(int cid, int ByIndex, Atom ratom, Binding *fBind);
	virtual bool GetCapsBit( int bitNumber );
  
	virtual bool CheckInternalFunction( Symbol *fSymb, int *group ) { return true; }
  virtual bool BindUniformUnbound( SourceLoc *loc, Symbol *fSymb, Binding *fBind )
	{ fBind->properties |= BIND_IS_BOUND;  return true; }
	virtual bool BindVaryingSemantic(SourceLoc *loc, Symbol *fSymb, Atom semantic, Binding *fBind, int IsOutVal);
	
	map< TypeBase, string > baseTypeNames;
};


/*
 * InitHal_glsles()
 */

GlslesvHal::GlslesvHal( CgContext *cg, Atom hEntryName )
: Hal( cg, hEntryName )
{
	profileName = cg->GetAtom( "glslesv" );
  vendor = VENDOR_STRING_GLSLESV;
  version = VERSION_STRING_GLSLESV;
  
  semantics = Semantics_glslesv;
  numSemantics = NUMELS(Semantics_glslesv);
  
  incid = CID_GLSLESV_IN_ID;
  inputCRegs = inputCRegs_glslesv;
  numInputCRegs = NUMELS(inputCRegs_glslesv);
  
  outcid = CID_GLSLESV_OUT_ID;
  outputCRegs = outputCRegs_glslesv;
  numOutputCRegs = NUMELS(outputCRegs_glslesv);
  
  comment = "//";
	
} // InitHal_glsles




/*
 * RegisterNames()
 */

bool GlslesvHal::RegisterNames()
{
  int i, j;
  
  // Add atoms for connectors and connector registers.
  for (i = 0; i < NUMELS(connectors_glslesv); i++) {
    ConnectorDescriptor * conn = &connectors_glslesv[i];
    conn->name = cg->GetAtom(conn->sname);
    for (j = 0; j < conn->numregs; j++)
      conn->registers[j].name = cg->GetAtom(conn->registers[j].sname);
  }
  
  return true;
}


/*
 * GetConnectorID()
 */

int GlslesvHal::GetConnectorID( Atom name )
{
  int i;
  
	for (i = 0; i < NUMELS(connectors_glslesv); i++) {
		if ( name == connectors_glslesv[i].name ) {
      return connectors_glslesv[i].cid;
		}
	}
  
  return 0;
}


/*
 * GetConnectorAtom()
 */

Atom GlslesvHal::GetConnectorAtom( int cid )
{
  const ConnectorDescriptor *conn = LookupConnector( connectors_glslesv, cid, NUMELS (connectors_glslesv) );
  return conn ? conn->name : Atom();
}

/*
 * GetConnectorUses()
 */

int GlslesvHal::GetConnectorUses( int cid, int pid )
{
  const ConnectorDescriptor *conn = LookupConnector( connectors_glslesv, cid, NUMELS(connectors_glslesv) );
  return conn ? conn->properties : 0;
}

/*
 * GetConnectorRegister()
 */

bool GlslesvHal::GetConnectorRegister(int cid, int ByIndex, Atom ratom, Binding *fBind)
{
  int i;
  ConnectorDescriptor *conn;
  ConnectorRegisters *regs;
  
  conn = LookupConnector(connectors_glslesv, cid, NUMELS(connectors_glslesv));
  if (! conn)
    return false;
  
  regs = conn->registers;
  
  if (! regs)
    return false;
  
  for (i = 0; i < conn->numregs; i++) {
    if (ratom == regs[i].name) {
      SetSymbolConnectorBinding(fBind, &regs[i]);
      return true;
    }
  }
  
  return false;
} // GetConnectorRegister

/*
 * GetCapsBit() - Return an integer value representing the capabilities of this profile.
 */

bool GlslesvHal::GetCapsBit( int bitNumber )
{
  switch (bitNumber) {
    case CAPS_INDEXED_ARRAYS:
    case CAPS_DONT_FLATTEN_IF_STATEMENTS:
      return true;
    default:
      return false;
  }
} // GetCapsBit


/*
 * BindVaryingSemantic()
 */

bool GlslesvHal::BindVaryingSemantic(SourceLoc *loc, Symbol *fSymb, Atom semantic, Binding *fBind, int IsOutVal)
{
  TypeBase base;
  int ii, index, len;
  bool HasSuffix;
  bool IsFloating;
  SemanticsDescriptor *semantics;
  char root[128];
  const char *pname, *match;
  Type *lType;
  
  pname = cg->GetString(semantic);
  HasSuffix = HasNumericSuffix(pname, root, 128, &index);
  semantics = cg->theHal->semantics;
  for (ii = 0; ii < cg->theHal->numSemantics; ii++, semantics++) {
    match = semantics->numregs > 0 ? root : pname;
    if (!strcmp(match, semantics->sname)) {
      if (semantics->numregs > 0) {
        if (index >= semantics->numregs) {
          SemanticError( cg, loc, ERROR_S_SEMANTICS_INDEX_TOO_BIG, pname);
          return false;
        }
      } else {
        index = 0;
      }
      
      // Found a match.  See if the type is compatible:
      
      lType = fSymb->type;
      if (IsScalar(lType)) {
        len = 1;
      } else if (IsVector(lType, &len)) {
      } else {
        SemanticError( cg, loc, ERROR_S_SEM_VAR_NOT_SCALAR_VECTOR,
                      cg->GetString(fSymb->name));
        return false;
      }
      base = GetBase(lType);
      IsFloating = (base == TB_Float);
      if (!IsFloating)
        return false;
      
      if (semantics->properties & SEM_VARYING) {
        fBind->kind = BK_CONNECTOR;
        fBind->name = semantic;
        fBind->num = semantics->regno + index;
        fSymb->properties |= SYMB_IS_CONNECTOR_REGISTER;
      } else {
        fBind->kind = BK_SEMANTIC;
        fBind->name = semantic;
        fBind->num = 0;
      }
      fBind->properties |= BIND_IS_BOUND | BIND_VARYING;
      if (semantics->properties & SEM_HIDDEN)
        fBind->properties |= BIND_HIDDEN;
      fSymb->properties |= SYMB_CONNECTOR_CAN_READ; // Obsolete
      fSymb->properties |= SYMB_CONNECTOR_CAN_WRITE; // Obsolete
      if (semantics->properties & SEM_IN)
        fBind->properties |= BIND_INPUT;
      if (semantics->properties & SEM_OUT)
        fBind->properties |= BIND_OUTPUT;
      if (semantics->properties & SEM_REQUIRED)
        fBind->properties |= BIND_WRITE_REQUIRED;
      // fBind->none,gname set elsewhere
      // fBind->lname set elsewhere
      fBind->base = semantics->base;
      fBind->size = semantics->size;
      return true;
    }
  }
  return false;
} // BindVaryingSemantic

/*
 * PrintCodeHeader()
 */

bool GlslesvHal::PrintCodeHeader()
{
  cg->OutputPrintf( "%s glsles output by " SX_TAG "\n", comment );
  return true;
} // PrintCodeHeader


void GlslesvHal::PrintFunctions( CgContext *cg, Symbol *symb, Symbol *program )
{
  
}

struct GlslesWriter : public sx::Writer {
  
  map<string, string> typeNameRemap;
  Atom vin, vout, POSITION;
	GlslesWriter( CgContext * wCg, ostream & wO ) : sx::Writer( wCg, wO ) {
    vin = cg->GetAtomTable()->LookupAdd( "$vin" );
    vout = cg->GetAtomTable()->LookupAdd( "$vout" );
    POSITION = cg->GetAtomTable()->LookupAdd( "POSITION" );
    typeNameRemap[ "float2" ] = "vec2";
    typeNameRemap[ "float3" ] = "vec3";
    typeNameRemap[ "float4" ] = "vec4";
    typeNameRemap[ "int2" ]   = "ivec2";
    typeNameRemap[ "int3" ]   = "ivec3";
    typeNameRemap[ "int4" ]   = "ivec4";
    typeNameRemap[ "bool2" ]  = "bvec2";
    typeNameRemap[ "bool3" ]  = "bvec3";
    typeNameRemap[ "bool4" ]  = "bvec4";
    typeNameRemap[ "float2x2" ] = "mat2";
    typeNameRemap[ "float3x3" ] = "mat3";
    typeNameRemap[ "float4x4" ] = "mat4";
  }
	
  void WriteBaseType( Type *fType ) {
		TypeCategory category = GetCategory(fType);
		TypeBase base;
    int len, len2;
    switch (category) {
      case TC_Scalar:
        base = GetBase(fType);
        Printf("%s", GetBaseTypeNameString( cg, base ) );
        break;
      case TC_Array:
        base = GetBase(fType);
        if( IsMatrix( fType, &len, &len2 ) ) {
          char name[64];
          sprintf( name, "%s%dx%d", GetBaseTypeNameString( cg, base ), len2, len );
          Printf( "%s", typeNameRemap[ name ].c_str() );
        } else if (IsVector(fType, &len)) {
          char name[64];
          sprintf( name, "%s%d", GetBaseTypeNameString( cg, base), len );
          Printf( "%s", typeNameRemap[ name ].c_str() );
        } else {
          WriteType( static_cast< TypeArray * >( fType )->eltype );
          Printf("[%d]", static_cast< TypeArray * >( fType )->numels);
        }
        break;
      case TC_Struct:
        if (static_cast< TypeStruct * >( fType )->tag.IsValid()) {
          Printf("%s", cg->GetString(static_cast< TypeStruct * >( fType )->tag));
        }
        break;
      default:
        Printf("<<category=%02x>>", category);
        break;
    }
  }
  
	void WriteType( Type *fType )
	{
		TypeQualifier qualifiers;
		TypeDomain domain;
		Atom cid;
		
		if (fType) {
			qualifiers = GetQualifiers(fType);
			if ( fType->isConst )
				Printf("const ");
			if (qualifiers == TQ_In)
				Printf("in ");
			if (qualifiers == TQ_Out)
				Printf("out ");
			
			domain = GetDomain(fType);
			switch (domain) {
				case TD_Unknown:
					break;
				case TD_Uniform:
					Printf("uniform ");
					break;
				case TD_Varying:
					Printf("varying ");
					break;
				default:
					Printf("<<domain=%02x>>", domain);
					break;
			}
      
      WriteBaseType( fType );
			//Printf(" ");
		} else {
			Printf("<<NULL-TYPE>>");
		}
	} // PrintType
  
  void WriteVin( Symbol *vin ) {
    Symbol *s = static_cast< TypeStruct * >( vin->type )->members->symbols;
    Printf( "// program input\n" );
    while( s ) {
      Printf( "attribute " );
      WriteType( s->type );
      Printf( " sx_%s;\n", s->name.s );
      s = s->next;
    }
    Printf( "\n" );
  }
  
  void WriteVout( Symbol *vout ) {
    Symbol *s = static_cast< TypeStruct * >( vout->type )->members->symbols;
    Printf( "// program output\n" );
    while( s ) {
      if( s->name == POSITION ) {
        Printf( "// varying vec4 gl_Position;\n" );
        s = s->next;
        continue;
      }
      Printf( "varying " );
      WriteType( s->type );
      Printf( " sx_%s;\n", s->name.s );
      s = s->next;
    }
    Printf( "\n" );
  }
  
	void WriteVariable( Symbol *s ) {
		// Don't write out the variables whose names begin with $, I guess.
		if ( s->name == vin ) {
      WriteVin( s );
			return;
		} else if ( s->name == vout ) {
      WriteVout( s );
			return;
		}
		WriteType( s->type );
		Printf( " %s;\n", s->name.s );
    return;
	}
	
	void WriteMembers( Symbol *s ) {
		while( s ) {
			switch( s->kind ) {
				case SK_Variable:
					WriteVariable( s );
					break;
				default:
					break;
			}
			s = s->next;
		}
	}
	
	void WriteTypedef( Symbol *s ) {
		if( s->type->category == TC_Struct ) {
			Printf( "struct %s {\n", s->name.s );
			Indent();
			WriteMembers( static_cast<TypeStruct *>(s->type)->members->symbols );
			
			Unindent();
			Printf( "};\n\n", s->name.s );
		} else {
			Printf( "%s;   // WriteTypedef()\n", s->name.s );
		}
		
	}
  
	void WriteConst( Constant * c ) {
		switch( c->op ) {
			case FCONST_V_OP: {
        WriteBaseType( c->type );
				Printf( "( %.6f", c->val[0].f );
				int len = SUBOP_GET_S( c->subop);
				for ( int ii = 1; ii < len; ii++ ) {
					Printf( ", %.6f", c->val[ii].f );
				}
				Printf(" )");
			}
				break;
			default:
				Writer::WriteExpr( c );
				break;
		}
	}
	
	void WriteUnary( Unary * unary ) {
    
		switch ( unary->op ) {
			case VECTOR_V_OP:
			{
        WriteBaseType( unary->type );
				Printf( "( " );
				Writer::WriteExpr( unary->arg );
				Printf( " )" );
			}
				break;
			default:
				Writer::WriteExpr( unary );
				break;
		}
		
	}
	
	void WriteBinary( Binary * binary ) {
    
		switch ( binary->op ) {
			case MEMBER_SELECTOR_OP:
			{
        Symbol *sl = static_cast< Symb * >( binary->left )->symbol;
        if( sl->name == vin || sl->name == vout ) {
          Symbol *sr = static_cast< Symb * >( binary->right )->symbol;
          if( sr->name == POSITION ) {
            Printf( "gl_Position" );
          } else {
            Printf( "sx_%s", static_cast< Symb * >( binary->right )->symbol->name.s );
          }
          break;
        }
			}
			default:
				Writer::WriteExpr( binary );
				break;
		}
		
	}
	
	void WriteDecl( Decl * decl ) {
		WriteType( decl->type.GetType() );
		Printf( " %s", decl->name.s );
	}
	
	virtual void WriteExpr( Expr *expr ) {
    
		switch( expr->kind ) {
      case DECL_N:
				WriteDecl( static_cast< Decl * >( expr ) );
        break;
			case CONST_N:
				WriteConst( static_cast< Constant * >( expr ) );
				break;
			case UNARY_N:
				WriteUnary( static_cast< Unary * >( expr ) );
				break;
			case BINARY_N:
				WriteBinary( static_cast< Binary * >( expr ) );
				break;
			default:
				Writer::WriteExpr( expr );
				break;
		}
	}
  
	// Should this be part of the Writer base class?
	void WriteFunctions( Symbol *symbols, Symbol * program ) {
		if (symbols) {
			WriteFunctions( symbols->left, program );
			switch( symbols->kind ) {
				case SK_Function:
				{
					Symbol *fSymb = symbols;
					while ( fSymb ) {
						if ( fSymb == program ) {
							WriteEntry( fSymb );
						} else {
							WriteFunction( fSymb );
						}
						fSymb = fSymb->details.fun.overload;
					}
				}
					break;
				case SK_Typedef:
					WriteTypedef( symbols );
					break;
				case SK_Variable:
					WriteVariable( symbols );
					break;
				default:
					Printf( "What the hell is this? (%s)\n", symbols->name.s );
					break;
			}
			WriteFunctions( symbols->right, program );
		}
	}
	
	void WriteEntry( Symbol * entry ) {
		assert(entry);
		
		// TODO: emit aggregate type declarations properly
		Symbol *params = entry->details.fun.params;
		Printf( "\n" );
		Printf( "// Cg entry point params must be declared in global scope prior to a void main() \n\n" );
    
		while (params) {
			const char *pname;
			pname = cg->GetString(params->name);
      if( params->kind == SK_Variable ) {
        if( GetDomain( params->type ) == TD_Uniform ) {
          WriteType( params->type );
        } else {
          WriteBaseType( params->type );
        }
        Printf( " " );
        WriteSymbol( params );
        Printf( ";\n" );
      }
			params = params->next;
		}
		
		// whatever the entrypoint may be called, it must be "main" is GLSL
		Printf( "\n" );
		Printf("void main()\n{\n");
		WriteStmtList( entry->details.fun.statements );
		Printf("} // main\n");
		Printf( "\n" );
	}
  
};


/*
 * GenerateCode() - Generates human-readable glsles output form of source code.
 */

bool GlslesvHal::GenerateCode( SourceLoc *loc, Scope *fScope, Symbol *program )
{
	GlslesWriter gw( cg, cg->outStream );
  
  // main takes no args, so we need to output the arg list first
  // find main, emit it's args in the global scope, then after other
  // functions are emitted, emit main
  
	gw.WriteFunctions( fScope->symbols, program );
  
	return true;
} // GenerateCode


/*
 * CreateGlslesvHal() - the glsles profile constructor
 */

Hal * CreateGlslesvHal( CgContext *cg, Atom entryName ) {
	return new GlslesvHal( cg, entryName );
}



///////////////////////////////////////////////////////////////////////////////
//////////////////////// End of glsles_hal.c /////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
