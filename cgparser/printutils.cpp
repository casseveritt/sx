
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
// printutils.c
//

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "slglobals.h"

///////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////// Debug Printing Functions: //////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

#undef PICK
#define PICK(a, b, c, d, e) b

const char *opcode_name[] = {
	OPCODE_TABLE
};

#undef PICK
#define PICK(a, b, c, d, e) e

const subopkind subop_table[] = {
	OPCODE_TABLE
};

#undef PICK

namespace sx {
	
	void Writer::Indent() {
		indent = indent + "  ";
	}
	void Writer::Unindent() {
		assert( indent.size() >= 2 );
		indent = indent.substr( 0, indent.size() - 2 );
	}
	
	void Writer::Printf( const char *fmt, ... ) {
		char str[16384];
		va_list args;
		va_start( args, fmt );
		sxVsprintf( str, fmt, args );
		va_end( args );
		cg->OutputPrintf( "%s%s", indentNext ? indent.c_str() : "", str );
		cg->FlushOutput();
		indentNext = fmt[ strlen( fmt ) - 1 ] == '\n';
	}
	
	/*
	 * FormatTypeString() - Build a printable string of a type.
	 *
	 * Arrays are shown as: "packed float[4]" instead of "float4".
	 *
	 */
	void FormatTypeString( CgContext *cg, char *name, int size, char *name2, int size2, Type *fType)
	{
		TypeQualifier qualifiers;
		TypeCategory category;
		TypeBase base;
		Atom cid;
		char tname[32];
		
		strcpy(name2, "");
		if (fType) {
			strcpy(name, "");
			
			base = GetBase(fType);
			
			qualifiers = GetQualifiers(fType);
			if ( fType->isConst )
				strcat(name, "const ");
			if (qualifiers == TQ_InOut) {
				strcat(name, "inout ");
			} else {
				if (qualifiers == TQ_In)
					strcat(name, "in ");
				if (qualifiers == TQ_Out)
					strcat(name, "out ");
			}
			
			category = GetCategory(fType);
			switch (category) {
				case TC_None:
					strcat(name, "<<category=NONE>>");
					break;
				case TC_Scalar:
					strcat(name, GetBaseTypeNameString( cg, base));
					break;
				case TC_Array:
					FormatTypeString( cg, name, size, name2, size2, static_cast< TypeArray * >( fType )->eltype);
					sprintf(tname, "[%d]", static_cast< TypeArray * >( fType )->numels);
					strcat(name2, tname);
					break;
				case TC_Function:
					strcat(name, "FUNCTION");
					break;
				case TC_Struct:
					strcat(name, "struct ");
					strcat(name, cg->GetString(static_cast< TypeStruct * >( fType )->tag));
					break;
				case TC_Connector:
					cid = cg->theHal->GetConnectorAtom(static_cast< TypeStruct * >( fType )->variety);
					strcat(name, cg->GetString(cid));
					strcat(name, " connector ");
					strcat(name, cg->GetString(static_cast< TypeStruct * >( fType )->tag));
					break;
				default:
					strcat(name, "<<bad-category>>");
					break;
			}
		} else {
			strcpy(name, "<<NULL>>");
		}
	} // FormatTypeString
	
	
	/*
	 * FormatTypeStringRT() - Build a printable string of a type for export to the run-time.
	 *
	 * Arrays are shown as predefined typedefs: "float4" instead of "packed float[4]"
	 *
	 */
	std::vector< std::string > FormatTypeStringRT( CgContext *cg, Type *fType, bool unqualified ) {
		char name1[100];
		char name2[100];
		FormatTypeStringRT( cg, name1, sizeof( name1 ), name2, sizeof( name2 ), fType, unqualified );
		std::vector<std::string> types;
		types.push_back( name1 );
		if( name2[0] ) {
			types.push_back( name2 );
		}
		return types;
	}
	
	void FormatTypeStringRT( CgContext *cg, char *name, int size, char *name2, int size2, Type *fType, int Unqualified)
	{
		TypeQualifier qualifiers;
		TypeCategory category;
		TypeBase base;
		Atom cid;
		char tname[32];
		int len, len2;
		
		strcpy(name2, "");
		if (fType) {
			strcpy(name, "");
			
			base = GetBase(fType);
			
			if (!Unqualified) {
				qualifiers = GetQualifiers(fType);
				if ( fType->isConst )
					strcat(name, "const ");
				if ( qualifiers == TQ_InOut) {
					strcat(name, "inout ");
				} else {
					if (qualifiers == TQ_In)
						strcat(name, "in ");
					if (qualifiers == TQ_Out)
						strcat(name, "out ");
				}
			}
			
			category = GetCategory(fType);
			switch (category) {
				case TC_None:
					strcat(name, "<<category=NONE>>");
					break;
				case TC_Scalar:
					strcat(name, GetBaseTypeNameString( cg, base));
					break;
				case TC_Array:
					if (IsMatrix(fType, &len, &len2)) {
						strcat(name, GetBaseTypeNameString( cg, base));
						sprintf(tname, "%dx%d", len2, len);
						strcat(name, tname);
					} else if (IsVector(fType, &len)) {
						strcat(name, GetBaseTypeNameString( cg, base));
						tname[0] = '0' + len;
						tname[1] = '\0';
						strcat(name, tname);
					} else {
						FormatTypeStringRT( cg, name, size, name2, size2, static_cast< TypeArray * >( fType )->eltype, Unqualified);
						sprintf(tname, "[%d]", static_cast< TypeArray * >( fType )->numels);
						strcat(name2, tname);
					}
					break;
				case TC_Function:
					strcat(name, "FUNCTION");
					break;
				case TC_Struct:
					strcat(name, "struct ");
					strcat(name, cg->GetString(static_cast< TypeStruct * >( fType )->tag));
					break;
				case TC_Connector:
					cid = cg->theHal->GetConnectorAtom(static_cast< TypeStruct * >( fType )->variety);
					strcat(name, cg->GetString(cid));
					strcat(name, " connector ");
					strcat(name, cg->GetString(static_cast< TypeStruct * >( fType )->tag));
					break;
				default:
					strcat(name, "<<bad-category>>");
					break;
			}
		} else {
			strcpy(name, "<<NULL>>");
		}
	} // FormatTypeStringRT
	
	/*
	 * PrintType()
	 *
	 */
	
	void Writer::WriteType( Type *fType )
	{
		TypeBase base;
		TypeCategory category;
		TypeQualifier qualifiers;
		TypeDomain domain;
		Atom cid;
		TypeList *lTypeList;
		
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
			
			category = GetCategory(fType);
			switch (category) {
				case TC_None:
					Printf("<<category=NONE>>");
					break;
				case TC_Scalar:
					base = GetBase(fType);
					Printf("%s", GetBaseTypeNameString( cg, base));
					break;
				case TC_Array:
					WriteType( static_cast< TypeArray * >( fType )->eltype );
					Printf("[%d]", static_cast< TypeArray * >( fType )->numels);
					break;
				case TC_Function:
					Printf("( ");
					lTypeList = static_cast< TypeFunction * >( fType )->paramtypes;
					while (lTypeList) {
						WriteType( lTypeList->type );
						lTypeList = lTypeList->next;
						if (lTypeList)
							Printf(", ");
					}
					Printf(" )");
					break;
				case TC_Struct:
					if (static_cast< TypeStruct * >( fType )->tag.IsValid()) {
						Printf("struct %s", cg->GetString(static_cast< TypeStruct * >( fType )->tag));
					} else {
						Printf("struct");
					}
					break;
				case TC_Connector:
					if (static_cast< TypeStruct * >( fType )->tag.IsValid()) {
						cid = cg->theHal->GetConnectorAtom(static_cast< TypeStruct * >( fType )->variety);
						Printf("%s connector %s",
                   cg->GetString(cid),
                   cg->GetString(static_cast< TypeStruct * >( fType )->tag));
					} else {
						Printf("connector");
					}
					break;
				default:
					Printf("<<category=%02x>>", category);
					break;
			}
			//Printf(" ");
		} else {
			Printf("<<NULL-TYPE>>");
		}
	} // PrintType
	
	/*
	 * PrintSymbolTree()
	 *
	 */
	
	void Writer::WriteSymbolTree( Symbol *fSymb)
	{
		Symbol *lSymb;
		int DoType;
		
		if (fSymb) {
			DoType = 1;
			WriteSymbolTree( fSymb->left);
			switch (fSymb->kind) {
				case SK_Typedef:
					Printf("TYP: %s : %d:%d", cg->GetString(fSymb->name),
                 cg->theHal->GetSizeof(fSymb->type), cg->theHal->GetAlignment(fSymb->type));
					break;
				case SK_Variable:
					if (fSymb->properties & SYMB_IS_PARAMETER) {
						Printf("PAR: %s ", cg->GetString(fSymb->name));
					} else {
						Printf("VAR: %s ", cg->GetString(fSymb->name));
					}
					break;
				case SK_Constant:
					Printf("CON: %s ", cg->GetString(fSymb->name));
					break;
				case SK_Function:
					lSymb = fSymb;
					while (lSymb) {
						if (lSymb == fSymb) {
							Printf("FUN");
						} else {
							Printf("   ");
						}
						Printf(": %s ", cg->GetString(fSymb->name));
						WriteType( lSymb->type );
						if (! ( lSymb->properties & SYMB_IS_DEFINED ) )
							Printf(" UNDEFINED");
						if (lSymb->properties & SYMB_IS_BUILTIN)
							Printf(" BUILTIN");
						if (lSymb->properties & SYMB_IS_INLINE_FUNCTION)
							Printf(" INLINE");
						Printf("\n");
						lSymb = lSymb->details.fun.overload;
					}
					DoType = 0;
					break;
				default:
					Printf("???%04x???: %s ", fSymb->kind, cg->GetString(fSymb->name));
					break;
			}
			if (DoType) {
				WriteType( fSymb->type );
				Printf("\n");
			}
			WriteSymbolTree( fSymb->right);
		}
	} // PrintSymbolTree
	
	/*
	 * PrintScopeDeclarations()
	 *
	 */
	
	void Writer::WriteScopeDeclarations()
	{
		Printf("*** Scope %d definitions: ***\n", cg->currentScope->level);
		WriteSymbolTree( cg->currentScope->symbols);
		Printf("*** End of Scope %d ***\n\n", cg->currentScope->level);
	} // PrintScopeDeclarations
	
  
  void Writer::WriteSymbol( Symbol * sym ) {
    Printf("%s", cg->GetString( sym->name ) );
  }
  
  
	/*
	 * lPrintExpr()
	 *
	 */
	
	void Writer::WriteExpr( Expr *fexpr)
	{
		int ii, mask, len;
		unsigned int uval;
		char s[16], tag;
		
		switch (fexpr->kind) {
			case SYMB_N:
        WriteSymbol( static_cast< Symb * >( fexpr )->symbol );
				break;
			case CONST_N:
				switch (static_cast< Constant * >( fexpr )->op) {
					case ICONST_OP:
						Printf("%d", static_cast< Constant * >( fexpr )->val[0].i);
						break;
					case ICONST_V_OP:
						Printf("{ %d", static_cast< Constant * >( fexpr )->val[0].i);
						len = SUBOP_GET_S(static_cast< Constant * >( fexpr )->subop);
						for (ii = 1; ii < len; ii++)
							Printf(", %d", static_cast< Constant * >( fexpr )->val[ii].i);
						Printf(" }");
						break;
					case BCONST_OP:
						if (static_cast< Constant * >( fexpr )->val[0].i == 0) {
							Printf("false");
						} else if (static_cast< Constant * >( fexpr )->val[0].i == 1) {
							Printf("true");
						} else {
							Printf("<<BBCONST=%d>>", static_cast< Constant * >( fexpr )->val[0].i);
						}
						break;
					case BCONST_V_OP:
						Printf("{ ");
						len = SUBOP_GET_S(static_cast< Constant * >( fexpr )->subop);
						for (ii = 0; ii < len; ii++)
							if (ii) Printf(", ");
						if (static_cast< Constant * >( fexpr )->val[ii].i == 0) {
							Printf("false");
						} else if (static_cast< Constant * >( fexpr )->val[ii].i == 1) {
							Printf("true");
						} else {
							Printf("<<BBCONST=%d>>", static_cast< Constant * >( fexpr )->val[ii].i);
						}
						Printf(" }");
						break;
					case FCONST_OP:
						Printf("%.6gf", static_cast< Constant * >( fexpr )->val[0].f);
						break;
					case HCONST_OP:
						Printf("%.6gh", static_cast< Constant * >( fexpr )->val[0].f);
						break;
					case XCONST_OP:
						Printf("%.6gx", static_cast< Constant * >( fexpr )->val[0].f);
						break;
					case FCONST_V_OP:
						tag = 'f';
						goto floatvec;
					case HCONST_V_OP:
						tag = 'h';
						goto floatvec;
					case XCONST_V_OP:
						tag = 'x';
					floatvec:
						Printf("{ %.6g%c", static_cast< Constant * >( fexpr )->val[0].f, tag);
						len = SUBOP_GET_S(static_cast< Constant * >( fexpr )->subop);
						for (ii = 1; ii < len; ii++)
							Printf(", %.6g%c", static_cast< Constant * >( fexpr )->val[ii].f, tag);
						Printf(" }");
						break;
				}
				break;
			case UNARY_N:
				switch (static_cast< Unary * >( fexpr )->op) {
					case CAST_CS_OP:
						Printf("(%s) ", GetBaseTypeNameString( cg, SUBOP_GET_T2(static_cast< Unary * >( fexpr )->subop)));
						break;
					case CAST_CV_OP:
						Printf("(%s [%d]) ",
                   GetBaseTypeNameString( cg, SUBOP_GET_T2( static_cast< Unary * >( fexpr )->subop)),
                   SUBOP_GET_S1(static_cast< Unary * >( fexpr )->subop));
						break;
					case CAST_CM_OP:
						Printf("(%s [%d][%d]) ",
                   GetBaseTypeNameString( cg, SUBOP_GET_T2( static_cast< Unary * >( fexpr )->subop)),
                   SUBOP_GET_S2(static_cast< Unary * >( fexpr )->subop), SUBOP_GET_S1(static_cast< Unary * >( fexpr )->subop));
						break;
					case VECTOR_V_OP:
						Printf("{ ");
						break;
					case NEG_OP:
					case NEG_V_OP:
						Printf("-");
						break;
					case POS_OP:
					case POS_V_OP:
						Printf("+");
						break;
					case NOT_OP:
					case NOT_V_OP:
						Printf("~");
						break;
					case BNOT_OP:
					case BNOT_V_OP:
						Printf("!");
						break;
					case SWIZZLE_Z_OP:
					case SWIZMAT_Z_OP:
						break;
					case PREDEC_OP:
						Printf("--");
						break;
					case PREINC_OP:
						Printf("++");
						break;
				}
				WriteExpr( static_cast< Unary * >( fexpr )->arg);
				switch (static_cast< Unary * >( fexpr )->op) {
					case SWIZZLE_Z_OP:
						mask = SUBOP_GET_MASK(static_cast< Unary * >( fexpr )->subop);
						ii = SUBOP_GET_S2(static_cast< Unary * >( fexpr )->subop);
						if (ii == 0)
							ii = 1; // var.x is scalar, not array[1]
						s[ii] = '\0';
						while (ii > 0) {
							ii--;
							s[ii] = "xyzw"[(mask >> ii*2) & 3];
						}
						Printf(".%s", s);
						break;
					case SWIZMAT_Z_OP:
						mask = SUBOP_GET_MASK16(static_cast< Unary * >( fexpr )->subop);
						ii = SUBOP_GET_T2(static_cast< Unary * >( fexpr )->subop);
						if (ii == 0)
							ii = 1; // var.x is scalar, not array[1]
						s[ii*3] = '\0';
						while (ii > 0) {
							ii--;
							uval = (mask >> ii*4) & 15;
							s[ii*3] = '_';
							s[ii*3 + 1] = '0' + ((uval >> 2) & 3);
							s[ii*3 + 2] = '0' + (uval & 3);
						}
						Printf(".m%s", s);
						break;
					case VECTOR_V_OP:
						Printf(" }");
						break;
					case POSTDEC_OP:
						Printf("--");
						break;
					case POSTINC_OP:
						Printf("++");
						break;
				}
				break;
			case BINARY_N:
				WriteExpr( static_cast< Binary * >( fexpr )->left);
				switch (static_cast< Binary * >( fexpr )->op) {
					case MEMBER_SELECTOR_OP:
						Printf(".");
						break;
					case ARRAY_INDEX_OP:
						Printf("[ ");
						break;
					case FUN_CALL_OP:
					case FUN_BUILTIN_OP:
						Printf("( ");
						break;
					case FUN_ARG_OP:
					case EXPR_LIST_OP:
						if (static_cast< Binary * >( fexpr )->right)
							Printf(", ");
						break;
					case MUL_OP:
					case MUL_SV_OP:
					case MUL_VS_OP:
					case MUL_V_OP:
						Printf("*");
						break;
					case DIV_OP:
					case DIV_SV_OP:
					case DIV_VS_OP:
					case DIV_V_OP:
						Printf("/");
						break;
					case MOD_OP:
					case MOD_SV_OP:
					case MOD_VS_OP:
					case MOD_V_OP:
						Printf("%%");
						break;
					case ADD_OP:
					case ADD_SV_OP:
					case ADD_VS_OP:
					case ADD_V_OP:
						Printf(" + ");
						break;
					case SUB_OP:
					case SUB_SV_OP:
					case SUB_VS_OP:
					case SUB_V_OP:
						Printf(" - ");
						break;
					case SHL_OP:
					case SHL_V_OP:
						Printf(" << ");
						break;
					case SHR_OP:
					case SHR_V_OP:
						Printf(" >> ");
						break;
					case LT_OP:
					case LT_SV_OP:
					case LT_VS_OP:
					case LT_V_OP:
						Printf(" < ");
						break;
					case GT_OP:
					case GT_SV_OP:
					case GT_VS_OP:
					case GT_V_OP:
						Printf(" > ");
						break;
					case LE_OP:
					case LE_SV_OP:
					case LE_VS_OP:
					case LE_V_OP:
						Printf(" <= ");
						break;
					case GE_OP:
					case GE_SV_OP:
					case GE_VS_OP:
					case GE_V_OP:
						Printf(" >= ");
						break;
					case EQ_OP:
					case EQ_SV_OP:
					case EQ_VS_OP:
					case EQ_V_OP:
						Printf(" == ");
						break;
					case NE_OP:
					case NE_SV_OP:
					case NE_VS_OP:
					case NE_V_OP:
						Printf(" != ");
						break;
					case AND_OP:
					case AND_SV_OP:
					case AND_VS_OP:
					case AND_V_OP:
						Printf(" & ");
						break;
					case XOR_OP:
					case XOR_SV_OP:
					case XOR_VS_OP:
					case XOR_V_OP:
						Printf(" ^ ");
						break;
					case OR_OP:
					case OR_SV_OP:
					case OR_VS_OP:
					case OR_V_OP:
						Printf(" | ");
						break;
					case BAND_OP:
					case BAND_SV_OP:
					case BAND_VS_OP:
					case BAND_V_OP:
						Printf(" && ");
						break;
					case BOR_OP:
					case BOR_SV_OP:
					case BOR_VS_OP:
					case BOR_V_OP:
						Printf(" || ");
						break;
					case ASSIGN_OP:
					case ASSIGN_V_OP:
					case ASSIGN_GEN_OP:
						Printf(" = ");
						break;
					case ASSIGNMINUS_OP:
						Printf(" -= ");
						break;
					case ASSIGNMOD_OP:
						Printf(" %%= ");
						break;
					case ASSIGNPLUS_OP:
						Printf(" += ");
						break;
					case ASSIGNSLASH_OP:
						Printf(" /= ");
						break;
					case ASSIGNSTAR_OP:
						Printf(" *= ");
						break;
					case ASSIGN_MASKED_KV_OP:
						Printf("@@");
						mask = SUBOP_GET_MASK(static_cast< Binary * >( fexpr )->subop);
						for (ii = 3; ii >= 0; ii--) {
							if (mask & 1)
								Printf("%c", "wzyx"[ii]);
							mask >>= 1;
						}
						Printf(" = ");
						break;
					case COMMA_OP:
						Printf(" , ");
						break;
					default:
						Printf("<!BINOP=%d>", static_cast< Binary * >( fexpr )->op);
						break;
				}
				if (static_cast< Binary * >( fexpr )->right)
					WriteExpr( static_cast< Binary * >( fexpr )->right);
				switch (static_cast< Binary * >( fexpr )->op) {
					case ARRAY_INDEX_OP:
						Printf(" ]");
						break;
					case FUN_CALL_OP:
					case FUN_BUILTIN_OP:
						Printf(" )");
						break;
					default:
						break;
				}
				break;
			case TRINARY_N:
				WriteExpr( static_cast< Trinary * >( fexpr )->arg1);
				switch (static_cast< Trinary * >( fexpr )->op) {
					case COND_OP:
					case COND_V_OP:
					case COND_SV_OP:
					case COND_GEN_OP:
						Printf(" ? ");
						if (static_cast< Trinary * >( fexpr )->arg2)
							WriteExpr( static_cast< Trinary * >( fexpr )->arg2);
						Printf(" : ");
						if (static_cast< Trinary * >( fexpr )->arg3)
							WriteExpr( static_cast< Trinary * >( fexpr )->arg3);
						break;
					case ASSIGN_COND_OP:
					case ASSIGN_COND_V_OP:
					case ASSIGN_COND_SV_OP:
					case ASSIGN_COND_GEN_OP:
						Printf("@@(");
						if (static_cast< Trinary * >( fexpr )->arg2)
							WriteExpr( static_cast< Trinary * >( fexpr )->arg2);
						Printf(") = ");
						if (static_cast< Trinary * >( fexpr )->arg3)
							WriteExpr( static_cast< Trinary * >( fexpr )->arg3);
						break;
					default:
						Printf("<!TRIOP=%d>", static_cast< Trinary * >( fexpr )->op);
						break;
				}
				break;
			default:
				Printf("<!NODEKIND=%d>", fexpr->kind);
				break;
		}
	} // lPrintExpr
	
	
	
	void Writer::WriteStmtList( Stmt *fstmt, const char *fcomment)
	{
		Indent();
		while (fstmt) {
			WriteStmt( fstmt, fcomment);
			fstmt = fstmt->next;
		}
		Unindent();
	}
	
	/*
	 * lPrintStmt()
	 *
	 */
	
	void Writer::WriteStmt( Stmt *fstmt, const char *fcomment )
	{
		Stmt *lstmt;
		
		switch ( fstmt->kind ) {
			case EXPR_STMT:
				if (static_cast< ExprStmt * >( fstmt )->expr) {
					WriteExpr( static_cast< ExprStmt * >( fstmt )->expr);
				} else {
					Printf("/* empty statement */");
				}
				Printf(";\n");
				break;
			case IF_STMT:
				Printf("if (");
				WriteExpr( static_cast< IfStmt * >( fstmt )->cond);
				Printf(")\n");
				WriteStmtList( static_cast< IfStmt * >( fstmt )->thenstmt, NULL);
				if (static_cast< IfStmt * >( fstmt )->elsestmt) {
					Printf("else\n");
					WriteStmtList( static_cast< IfStmt * >( fstmt )->elsestmt, NULL);
				}
				break;
			case WHILE_STMT:
				Printf("while (");
				WriteExpr( static_cast< WhileStmt * >( fstmt )->cond);
				Printf(")\n");
				WriteStmtList( static_cast< WhileStmt * >( fstmt )->body, NULL);
				break;
			case DO_STMT:
				Printf("do\n");
				WriteStmtList( static_cast< WhileStmt * >( fstmt )->body, NULL);
				Printf("while (");
				WriteExpr( static_cast< WhileStmt * >( fstmt )->cond);
				Printf(");\n");
				break;
			case FOR_STMT:
				Printf("for (");
				lstmt = static_cast< ForStmt * >( fstmt )->init;
				if (lstmt) {
					while (lstmt) {
						if (static_cast< ExprStmt * >( lstmt )->kind == EXPR_STMT) {
							WriteExpr( static_cast< ExprStmt * >( lstmt )->expr);
						} else {
							Printf("*** BAD STMT KIND ***");
						}
						if (static_cast< ExprStmt * >( lstmt )->next)
							Printf(", ");
						lstmt = static_cast< ExprStmt * >( lstmt )->next;
					}
				}
				Printf(";");
				if (static_cast< ForStmt * >( fstmt )->cond) {
					Printf(" ");
					WriteExpr( static_cast< ForStmt * >( fstmt )->cond);
				}
				Printf(";");
				lstmt = static_cast< ForStmt * >( fstmt )->step;
				if (lstmt) {
					Printf(" ");
					while (lstmt) {
						if (static_cast< ExprStmt * >( lstmt )->kind == EXPR_STMT) {
							WriteExpr( static_cast< ExprStmt * >( lstmt )->expr);
						} else {
							Printf("*** BAD STMT KIND ***");
						}
						if (static_cast< ExprStmt * >( lstmt )->next)
							Printf(", ");
						lstmt = static_cast< ExprStmt * >( lstmt )->next;
					}
				}
				Printf(")\n");
				WriteStmtList( static_cast< ForStmt * >( fstmt )->body, NULL);
				break;
			case BLOCK_STMT:
				Printf("{\n");
				WriteStmtList( static_cast< BlockStmt * >( fstmt )->body, NULL);
				if (fcomment) {
					Printf("} // %s\n", fcomment);
				} else {
					Printf("}\n");
				}
				break;
			case RETURN_STMT:
				Printf("return");
				if (static_cast< ReturnStmt * >( fstmt )->expr) {
					Printf(" ");
					WriteExpr( static_cast< ReturnStmt * >( fstmt )->expr);
				}
				Printf(";\n");
				break;
			case DISCARD_STMT:
				Printf("discard");
				if (static_cast< DiscardStmt * >( fstmt )->cond) {
					Printf(" ");
					WriteExpr( static_cast< DiscardStmt * >( fstmt )->cond);
				}
				Printf(";\n");
				break;
			case COMMENT_STMT:
				Printf("// %s\n", cg->GetString(static_cast< CommentStmt * >( fstmt )->str));
				break;
			default:
				Printf("<!BadStmt-0x%2x>\n", static_cast< ExprStmt * >( fstmt )->kind);
		}
	} // lPrintStmt
	
	
	
	/*
	 * PrintFunction()
	 *
	 */
	
	void Writer::WriteFunction( Symbol *symb )
	{
		const char *sname, *pname;
		char tname[100], uname[100];
		Symbol *params;
		
		if (symb) {
			sname = cg->GetString(symb->name);
			if (symb->kind == SK_Function) {
				if (symb->type) {
					FormatTypeString( cg, tname, sizeof tname, uname, sizeof uname, static_cast< TypeFunction * >( symb->type )->rettype);
				} else {
					strcpy(tname, "NULL");
				}
				Printf("%s %s%s( ",tname, sname, uname);
				params = symb->details.fun.params;
				while (params) {
					pname = cg->GetString(params->name);
					FormatTypeString( cg, tname, sizeof tname, uname, sizeof uname, params->type);
					Printf("%s %s%s",tname, pname, uname);
					params = params->next;
					if (params)
						Printf(", ");
				}
				Printf(" )\n");
				Printf("{\n");
				Indent();
				WriteStmtList( symb->details.fun.statements, "function");
				Unindent();
				Printf("} // %s\n", sname);
			} else {
				Printf("PrintFunction: Symbol \"%s\" not a function\n", sname);
			}
		} else {
			Printf("<<NULL-Function-Symbol>>\n");
		}
	} // PrintFunction
	
	///////////////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////// End of printutils.c ////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////
	
	
	
	Writer * CreateBWriter( CgContext *cg, std::ostream & o ) {
		return new BWriter( cg, o );
	}
	
	void BWriter::WriteExpr( Expr *fexpr, int level )
	{
		char s[32];
		int subop, mask, ii, nn, bval;
		int OK = 1, HasString = 0;
		
		if (fexpr) {
			switch (fexpr->kind) {
				case DECL_N:
					Printf("DECLARATION");
					OK = 0;
					break;
				case SYMB_N:
					Printf("S ");
					break;
				case CONST_N:
					Printf("C ");
					break;
				case UNARY_N:
					Printf("U ");
					break;
				case BINARY_N:
					Printf("B ");
					break;
				case TRINARY_N:
					Printf("T ");
					break;
				default:
					Printf("<kind=%02x>", fexpr->kind);
					OK = 0;
					break;
			}
			if (OK) {
				Printf("%c%c", static_cast< Symb * >( fexpr )->IsLValue ? 'L' : ' ',
               fexpr->HasSideEffects ? '+' : ' ');
				subop = static_cast< Constant * >( fexpr )->subop;
				switch (subop_table[static_cast< Constant * >( fexpr )->op]) {
					case SUB_NONE:
						Printf("- - - ");
						mask = ~0;
						break;
					case SUB_S:
						Printf("- - %1x ", SUBOP_GET_T(subop));
						mask = 0x0000000f;
						break;
					case SUB_V:
					case SUB_VS:
					case SUB_SV:
						Printf("- %1x %1x ", SUBOP_GET_S(subop), SUBOP_GET_T(subop));
						mask = 0x000000ff;
						break;
					case SUB_M:
					case SUB_VM:
					case SUB_MV:
						Printf("%1x %1x %1x ", SUBOP_GET_S2(subop), SUBOP_GET_S1(subop),
                   SUBOP_GET_T1(subop));
						mask = 0x0000f0ff;
						break;
					case SUB_Z:
						Printf("%1x %1x %1x ", SUBOP_GET_S2(subop), SUBOP_GET_S1(subop),
                   SUBOP_GET_T1(subop));
						mask = SUBOP_GET_MASK(subop);
						nn = SUBOP_GET_S2(subop);
						if (nn == 0)
							nn = 1; // var.x is scalar, not array[1]
						s[nn] = '\0';
						for (ii = 0; ii < nn; ii++)
							s[ii] = "xyzw"[(mask >> ii*2) & 3];
						mask = 0x00fff0ff;
						HasString = 1;
						break;
					case SUB_ZM:
						Printf("%1x %1x %1x %1x ", SUBOP_GET_S2(subop), SUBOP_GET_T2(subop),
                   SUBOP_GET_S1(subop), SUBOP_GET_T1(subop));
						mask = SUBOP_GET_MASK16(subop);
						nn = SUBOP_GET_T2(subop);
						if (nn == 0)
							nn = 1; // var.x is scalar, not array[1]
						s[nn*3] = '\0';
						for (ii = 0; ii < nn; ii++) {
							s[ii*3] = '_';
							s[ii*3 + 1] = '0' + ((mask >> (ii*4 + 2)) & 3);
							s[ii*3 + 2] = '0' + ((mask >> ii*4) & 3);
						}
						mask = 0xffffffff;
						HasString = 1;
						break;
					case SUB_CS:
						Printf("%1x - %1x ", SUBOP_GET_T2(subop), SUBOP_GET_T1(subop));
						mask = 0x00000f0f;
						break;
					case SUB_CV:
						Printf("%1x %1x %1x ", SUBOP_GET_T2(subop), SUBOP_GET_S1(subop),
                   SUBOP_GET_T1(subop));
						mask = 0x00000fff;
						break;
					case SUB_CM:
						Printf("%1x %1x %1x %1x ", SUBOP_GET_S2(subop), SUBOP_GET_T2(subop),
                   SUBOP_GET_S1(subop), SUBOP_GET_T1(subop));
						mask = 0x0000ffff;
						break;
					case SUB_KV:
						Printf("- %1x %1x ", SUBOP_GET_S(subop), SUBOP_GET_T(subop));
						mask = SUBOP_GET_MASK(subop) & 0xf;
						for (ii = 0; ii < 4; ii++)
							Printf("%c", (mask >> ii) & 1 ? "xyzw"[ii] : '-');
						Printf(" ");
						mask = 0x000f00ff;
						break;
					default:
						mask = 0;
						break;
				}
				if (subop & ~mask)
					Printf("<<non-zero:%08x>> ", subop & ~mask);
				Printf("%-6s ", opcode_name[static_cast< Constant * >( fexpr )->op]);
				switch (fexpr->kind) {
					case SYMB_N:
						Printf("\"%s\"", cg->GetString(static_cast< Symb * >( fexpr )->symbol->name));
						break;
					case CONST_N:
						switch (static_cast< Constant * >( fexpr )->op) {
							case ICONST_OP:
								Printf("%d", static_cast< Constant * >( fexpr )->val[0].i);
								break;
							case ICONST_V_OP:
								nn = SUBOP_GET_S(subop);
								Printf("{ ");
								for (ii = 0; ii < nn; ii++) {
									if (ii > 0)
										Printf(", ");
									Printf("%d", static_cast< Constant * >( fexpr )->val[ii].i);
								}
								Printf(" }");
								break;
							case BCONST_OP:
								bval = static_cast< Constant * >( fexpr )->val[0].i;
								if (bval == 0) {
									Printf("false");
								} else if (bval == 1) {
									Printf("true");
								} else {
									Printf("<<bad-bool-%08x>>", static_cast< Constant * >( fexpr )->val[0].i);
								}
								break;
							case BCONST_V_OP:
								nn = SUBOP_GET_S(subop);
								Printf("{ ");
								for (ii = 0; ii < nn; ii++) {
									if (ii > 0)
										Printf(", ");
									bval = static_cast< Constant * >( fexpr )->val[ii].i;
									if (bval == 0) {
										Printf("false");
									} else if (bval == 1) {
										Printf("true");
									} else {
										Printf("<<bad-bool-%08x>>", static_cast< Constant * >( fexpr )->val[ii].i);
									}
								}
								Printf(" }");
								break;
							case FCONST_OP:
							case HCONST_OP:
							case XCONST_OP:
								Printf("%1.6g", static_cast< Constant * >( fexpr )->val[0].f);
								break;
							case FCONST_V_OP:
							case HCONST_V_OP:
							case XCONST_V_OP:
								nn = SUBOP_GET_S(subop);
								Printf("{ ");
								for (ii = 0; ii < nn; ii++) {
									if (ii > 0)
										Printf(", ");
									Printf("%1.6g", static_cast< Constant * >( fexpr )->val[ii].f);
								}
								Printf(" }");
								break;
							default:
								Printf("UNKNOWN-CONSTANT");
								break;
						}
						break;
					case UNARY_N:
						break;
					case BINARY_N:
						break;
					case TRINARY_N:
						break;
				}
				if (HasString)
					Printf(" %s", s);
				Printf(" ");
				WriteType( fexpr->type );
				Printf("\n");
				switch (fexpr->kind) {
					case SYMB_N:
						break;
					case CONST_N:
						break;
					case UNARY_N:
						WriteExpr( static_cast< Unary * >( fexpr )->arg, level + 1);
						break;
					case BINARY_N:
						WriteExpr( static_cast< Binary * >( fexpr )->left, level + 1);
						WriteExpr( static_cast< Binary * >( fexpr )->right, level + 1);
						break;
					case TRINARY_N:
						WriteExpr( static_cast< Trinary * >( fexpr )->arg1, level + 1);
						WriteExpr( static_cast< Trinary * >( fexpr )->arg2, level + 1);
						WriteExpr( static_cast< Trinary * >( fexpr )->arg3, level + 1);
						break;
				}
			} else {
				Printf("\n");
			}
		} else {
			Printf("NULL\n");
		}
	} // lBPrintExpression
	
	void BWriter::WriteStmtList( Stmt *fstmt, const char *comment )
	{
		while( fstmt ) {
			WriteStmt( fstmt );
			fstmt = fstmt->next;
		}
	}
	
	void BWriter::WriteStmt( Stmt *fstmt, const char *comment )
	{
		Stmt *lstmt;
		
		switch ( fstmt->kind ) {
			case EXPR_STMT:
				if (static_cast< ExprStmt * >( fstmt )->expr) {
					WriteExpr( static_cast< ExprStmt * >( fstmt )->expr );
				} else {
					Printf("/* empty statement */\n");
				}
				break;
			case IF_STMT:
				Printf("if\n");
				WriteExpr( static_cast< IfStmt * >( fstmt )->cond );
				Printf("then\n");
				WriteStmtList( static_cast< IfStmt * >( fstmt )->thenstmt );
				if ( static_cast< IfStmt * >( fstmt )->elsestmt ) {
					Printf("else\n");
					WriteStmtList( static_cast< IfStmt * >( fstmt )->elsestmt );
				}
				break;
			case WHILE_STMT:
				Printf("while\n");
				WriteExpr( static_cast< WhileStmt * >( fstmt )->cond );
				WriteStmtList( static_cast< WhileStmt * >( fstmt )->body );
				break;
			case DO_STMT:
				Printf("do\n");
				WriteStmtList( static_cast< WhileStmt * >( fstmt )->body );
				Printf("while\n");
				WriteExpr( static_cast< WhileStmt * >( fstmt )->cond );
				break;
			case FOR_STMT:
				Printf("for\n");
				lstmt = static_cast< ForStmt * >( fstmt )->init;
				if (lstmt) {
					WriteStmtList( static_cast< ForStmt * >( fstmt )->init );
				}
				Printf("for-cond\n");
				if (static_cast< ForStmt * >( fstmt )->cond) {
					WriteExpr( static_cast< ForStmt * >( fstmt )->cond );
				}
				Printf("for-step\n");
				lstmt = static_cast< ForStmt * >( fstmt )->step;
				if (lstmt) {
					WriteStmtList( static_cast< ForStmt * >( fstmt )->step );
				}
				Printf("for-body\n");
				WriteStmtList( static_cast< ForStmt * >( fstmt )->body );
				break;
			case BLOCK_STMT:
				Printf("{\n");
				WriteStmtList( static_cast< BlockStmt * >( fstmt )->body );
				Printf("}\n");
				break;
			case RETURN_STMT:
				Printf("return\n");
				if ( static_cast< ReturnStmt * >( fstmt )->expr ) {
					WriteExpr( static_cast< ReturnStmt * >( fstmt )->expr );
				}
				break;
			case DISCARD_STMT:
				Printf("discard\n");
				if ( static_cast< DiscardStmt * >( fstmt )->cond )
					WriteExpr( static_cast< DiscardStmt * >( fstmt )->cond );
				break;
			case COMMENT_STMT:
				Printf( "// %s\n", cg->GetString(static_cast< CommentStmt * >( fstmt )->str ) );
				break;
			default:
				Printf("<!BadStmt-0x%2x>\n", static_cast< ExprStmt * >( fstmt )->kind );
		}
	} // lBPrintStmt
	
	/*
	 * BPrintFunction()
	 *
	 */
	
	void BWriter::WriteFunction( Symbol *symb )
	{
		const char *sname;
		
		if (symb) {
			sname = cg->GetString(symb->name);
			if (symb->kind == SK_Function) {
				Printf("{\n");
				WriteStmtList( symb->details.fun.statements, 0);
				Printf("} // %s\n", sname);
			} else {
				Printf("BPrintFunction: Symbol \"%s\" not a function\n", sname);
			}
		} else {
			Printf("<<NULL-Function-Symbol>>\n");
		}
	} // BPrintFunction
	
}