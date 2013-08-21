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
// cgstruct.c
//

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "common.h"

#include <iostream>

#include "slglobals.h"

using namespace sx;
using namespace std;

#if _MSC_VER
# define WIN32_LEAN_AND_MEAN
# include <windows.h>
# undef AddAtom
//extern "C" void OutputDebugStringA( const char *str );
#endif


static const struct {
	int val;
	const char *str;
} tokens[] = {
	AND_SY,         "&&",
	ASSIGNMINUS_SY, "-=",
	ASSIGNMOD_SY,   "%=",
	ASSIGNPLUS_SY,  "+=",
	ASSIGNSLASH_SY, "/=",
	ASSIGNSTAR_SY,  "*=",
	ASM_SY,         "asm",
	BOOLEAN_SY,     "bool",
	BREAK_SY,       "break",
	CASE_SY,        "case",
	COLONCOLON_SY,  "::",
	CONST_SY,       "const",
	CONTINUE_SY,    "continue",
	DEFAULT_SY,     "default",
	DISCARD_SY,     "discard",
	DO_SY,          "do",
	EQ_SY,          "==",
	ELSE_SY,        "else",
	EXTERN_SY,      "extern",
	FLOAT_SY,       "float",
	FLOATCONST_SY,  "<float-const>",
	FLOATHCONST_SY, "<floath-const>",
	FLOATXCONST_SY, "<floatx-const>",
	FOR_SY,         "for",
	GE_SY,          ">=",
	GG_SY,          ">>",
	GOTO_SY,        "goto",
	IDENT_SY,       "<ident>",
	IF_SY,          "if",
	IN_SY,          "in",
	INLINE_SY,      "inline",
	INOUT_SY,       "inout",
	INT_SY,         "int",
	INTCONST_SY,    "<int-const>",
	INTERNAL_SY,    "__internal",
	LE_SY,          "<=",
	LL_SY,          "<<",
	MINUSMINUS_SY,  "--",
	NE_SY,          "!=",
	OR_SY,          "||",
	OUT_SY,         "out",
	PACKED_SY,      "__packed",
	PACKED_SY,      "packed",
	PLUSPLUS_SY,    "++",
	RETURN_SY,      "return",
	STATIC_SY,      "static",
	STRUCT_SY,      "struct",
	STRCONST_SY,    "<string-const>",
	SWITCH_SY,      "switch",
	THIS_SY,        "this",
	TYPEDEF_SY,     "typedef",
	TYPEIDENT_SY,   "<type-ident>",
	UNIFORM_SY,     "uniform",
	VOID_SY,        "void",
	WHILE_SY,       "while",
};


static int eof_scan( YYSTYPE * pyylval, CgContext *cg, InputSrc *is) { return EOF; }
static int eof_getch( CgContext *cg, InputSrc *is) { return EOF; }
static void noop( CgContext *cg, InputSrc *in, int ch) {}

static InputSrc eof_inputsrc = InputSrc( NULL, 0, &eof_scan, &eof_getch, &noop, 0, 0 );


/*
 * InitCgStruct() - Initilaize the CG structure.
 *
 */

CgContext::CgContext( Options & opt )
: pLastSourceLoc( NULL ), options( opt ), bindings( NULL ), allProfiles( NULL )
, theHal( NULL ), tokenLoc( NULL ), currentInput( NULL ), errorCount( 0 )
, warningCount( 0 ), lineCount( 0 ), allowSemanticParseErrors( 0 )
, outFlushPoint( 0 ), listingFlushPoint( 0 )
, currentScope( NULL )
, errorLocsFirst( NULL ), errorLocsLast( NULL ), errorPending( 0 )
, scopeList( NULL ), globalScope( NULL )
{
  // Initialize public members:
  
  pLastSourceLoc = &lastSourceLoc;
	atable = CreateAtomTable();
	for ( int i = 0; i < sizeof( tokens ) / sizeof( tokens[0] ); i++ ) {
		atable->AddAtom( tokens[i].val, tokens[i].str );
	}
  
	if ( ! InitHal( this, opt.profileString, opt.entryName ) ) {
		Printf(SX_TAG ": hal initialization failure.\n");
		return;
	}
  
	PushScope( this, new Scope( this ) );
	UndefinedType = NewType( this, atable->LookupAdd( "<undefined-type>" ), TB_UndefinedType, TC_Scalar, TD_Unknown, TQ_None, false, 0, 0);
	CFloatType = NewType( this, atable->LookupAdd( "const float" ), TB_Cfloat, TC_Scalar, TD_Unknown, TQ_None, true, 0, 1);
	CIntType = NewType( this, atable->LookupAdd( "const int" ), TB_Cint, TC_Scalar, TD_Unknown, TQ_None, true, 0, 1);
	VoidType = NewType( this, atable->LookupAdd( "void" ), TB_Void, TC_Scalar, TD_Unknown, TQ_None, false, TYPE_MISC_VOID, 0);
	FloatType = NewType( this, atable->LookupAdd( "float" ), TB_Float, TC_Scalar, TD_Unknown, TQ_None, false, 0, 1);
	IntType = NewType( this, atable->LookupAdd( "int" ), TB_Int, TC_Scalar, TD_Unknown, TQ_None, false, 0, 1);
	BooleanType = NewType( this, atable->LookupAdd( "bool" ), TB_Boolean, TC_Scalar, TD_Unknown, TQ_None, false, 0, 1);
  
	CFloat1Type = NewPackedArrayType( this, CFloatType, 1, 0);
	CFloat2Type = NewPackedArrayType( this, CFloatType, 2, 0);
	CFloat3Type = NewPackedArrayType( this, CFloatType, 3, 0);
	CFloat4Type = NewPackedArrayType( this, CFloatType, 4, 0);
	CInt1Type = NewPackedArrayType( this, CIntType, 1, 0);
	CInt2Type = NewPackedArrayType( this, CIntType, 2, 0);
	CInt3Type = NewPackedArrayType( this, CIntType, 3, 0);
	CInt4Type = NewPackedArrayType( this, CIntType, 4, 0);
	Float1Type = NewPackedArrayType( this, FloatType, 1, 0);
	Float2Type = NewPackedArrayType( this, FloatType, 2, 0);
	Float3Type = NewPackedArrayType( this, FloatType, 3, 0);
	Float4Type = NewPackedArrayType( this, FloatType, 4, 0);
	Int1Type = NewPackedArrayType( this, IntType, 1, 0);
	Int2Type = NewPackedArrayType( this, IntType, 2, 0);
	Int3Type = NewPackedArrayType( this, IntType, 3, 0);
	Int4Type = NewPackedArrayType( this, IntType, 4, 0);
	Boolean1Type = NewPackedArrayType( this, BooleanType, 1, 0);
	Boolean2Type = NewPackedArrayType( this, BooleanType, 2, 0);
	Boolean3Type = NewPackedArrayType( this, BooleanType, 3, 0);
	Boolean4Type = NewPackedArrayType( this, BooleanType, 4, 0);
  
	SourceLoc dummyLoc;
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("cfloat"), CFloatType, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("cint"), CIntType, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, VOID_SY, VoidType, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, FLOAT_SY, FloatType, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, INT_SY, IntType, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, BOOLEAN_SY, BooleanType, SK_Typedef);
  
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("cfloat1"), CFloat1Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("cfloat2"), CFloat2Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("cfloat3"), CFloat3Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("cfloat4"), CFloat4Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("cint1"), CInt1Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("cint2"), CInt2Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("cint3"), CInt3Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("cint4"), CInt4Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("float1"), Float1Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("float2"), Float2Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("float3"), Float3Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("float4"), Float4Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("int1"), Int1Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("int2"), Int2Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("int3"), Int3Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("int4"), Int4Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("bool1"), Boolean1Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("bool2"), Boolean2Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("bool3"), Boolean3Type, SK_Typedef);
	AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("bool4"), Boolean4Type, SK_Typedef);
  
	FalseSymb = AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("false"), BooleanType, SK_Constant);
	TrueSymb = AddSymbol( this, &dummyLoc, currentScope, atable->LookupAdd("true"), BooleanType, SK_Constant);
	FalseSymb->details.con.value = 0;
	TrueSymb->details.con.value = 1;
  
	SetScalarTypeName(TB_NoType, atable->LookupAdd("***no-base-type***"), UndefinedType);
	SetScalarTypeName(TB_UndefinedType, atable->LookupAdd("***undefined-base-type***"), UndefinedType);
	SetScalarTypeName(TB_Cfloat, atable->LookupAdd("cfloat"), CFloatType);
	SetScalarTypeName(TB_Cint, atable->LookupAdd("cint"), CIntType);
	SetScalarTypeName(TB_Void, atable->LookupAdd("void"), VoidType);
	SetScalarTypeName(TB_Float, atable->LookupAdd("float"), FloatType);
	SetScalarTypeName(TB_Int, atable->LookupAdd("int"), IntType);
	SetScalarTypeName(TB_Boolean, atable->LookupAdd("bool"), BooleanType);
  
	::Atom name = atable->LookupAdd("***unknown-profile-base-type***");
	for (int ii = TB_FirstUser; ii <= TB_LastUser; ii++) {
		SetScalarTypeName(ii, name, UndefinedType);
	}
  
	// Add profile specific symbols and types:
  
	theHal->RegisterNames();
	atable->Add("<*** end hal specific atoms ***>");
  
	// Initialize misc. other globals:
  
	currentDeclTypeSpecs.basetype = UndefinedType;
	currentDeclTypeSpecs.IsDerived = 0;
	currentDeclTypeSpecs.SetType( UndefinedType );
  
	// Add various atoms needed by the CPP line scanner:
	if( ! InitCPP( this, options.profileString ) )
		return;
  
	mostRecentToken = 0;
	tokenLoc = &ltokenLoc;
  
	ltokenLoc.file = 0;
	ltokenLoc.line = 0;
	errorCount = 0;
	warningCount = 0;
	lineCount = 0;
	allowSemanticParseErrors = 0;
  
	currentInput = &eof_inputsrc;
} // InitCgStruct


CgContext::~CgContext() {
  
	// free scanner
	FinalCPP( this );
	if( warningCount || errorCount || ! options.quiet ) {
		listingStream << lineCount << " lines";
		if( warningCount ) {
			listingStream << ", " << warningCount << " warnings";
		}
		listingStream << ", " << errorCount << " errors." << std::endl;
	}
  
	// free scopes
	while ( scopeList ) {
		Scope *nextScope = scopeList->next;
		delete scopeList;
		scopeList = nextScope;
	}
  
	void PrintLiveDecls( CgContext *cg );
	PrintLiveDecls( this );
  
	currentDeclTypeSpecs.SetType( NULL );
  
	WriteOutputFiles("End of program");
  
  
	// clean up anything not deleted by the destructor of its owners...
	while( types.size() > 0 ) {
		Type * t = *types.begin();
		delete t;
		types.erase( t );
	}
  
	delete theHal;
	delete atable;
}


void CgContext::Printf( const char *fmt, ... ) {
	char str[16384];
	va_list args;
	va_start( args, fmt );
	sxVsprintf( str, fmt, args );
	va_end( args );
#if _MSC_VER
	OutputDebugStringA( str );
#endif
	printf( "%s", str );
}

void CgContext::OutputPrintf( const char *fmt, ... ) {
	char str[16384];
	va_list args;
	va_start( args, fmt );
	sxVsprintf( str, fmt, args );
	va_end( args );
#if _MSC_VER
	OutputDebugStringA( str );
#endif
	outStream << str;
	if ( options.outputFileName == NULL ) {
		string s = outStream.str().substr( outFlushPoint );
		cout << s;
		outFlushPoint = outStream.str().size();
	}
}


void CgContext::OutputVPrintf( const char *fmt, va_list args ) {
	char str[16384];
	sxVsprintf( str, fmt, args );
#if _MSC_VER
	OutputDebugStringA( str );
#endif
	outStream << str;
	if ( options.outputFileName == NULL ) {
		string s = outStream.str().substr( outFlushPoint );
		cout << s;
		outFlushPoint = outStream.str().size();
	}
}

void CgContext::ListingPrintf( const char *fmt, ... ) {
	char str[16384];
	va_list args;
	va_start( args, fmt );
	sxVsprintf( str, fmt, args );
	va_end( args );
#if _MSC_VER
	OutputDebugStringA( str );
#endif
	listingStream << str;
	if ( options.listFileName == NULL ) {
		string s = listingStream.str().substr( listingFlushPoint );
		cout << s;
		listingFlushPoint = listingStream.str().size();
	}
}

void CgContext::ListingVPrintf( const char *fmt, va_list args ) {
	char str[16384];
	sxVsprintf( str, fmt, args );
#if _MSC_VER
	OutputDebugStringA( str );
#endif
  //	if ( options.listFileName == NULL ) {
  //		cout << str;
  //	}
	listingStream << str;
}

void CgContext::DebugPrintf( const char *fmt, ... ) {
	char str[16384];
	va_list args;
	va_start( args, fmt );
	sxVsprintf( str, fmt, args );
	va_end( args );
#if _MSC_VER
	OutputDebugStringA( str );
#endif
	fprintf( stderr, "%s", str );
}


void CgContext::FlushOutput() {
	OutputPrintf("");
	ListingPrintf("");
}

///////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////// End of cgstruct.c //////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
