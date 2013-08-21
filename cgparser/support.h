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
 Except as Expressly stated in this notice, no other rights or licenses
 express or implied, are granted by NVIDIA herein, including but not
 limited to any patent rights that may be infringed by your derivative
 works or by other works in which the NVIDIA Software may be
 incorporated. No hardware is licensed hereunder.
 
 THE NVIDIA SOFTWARE IS BEING PROVIDED ON AN "AS IS" BASIS, WITHOUT
 WARRANTIES OR CONDITIONS OF ANY KIND, EITHER ExprESS OR IMPLIED,
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
// support.h
//

#if !defined(__SUPPORT_H)
#define __SUPPORT_H 1

#include <string>
#include <assert.h>

#define SX_TAG "sx"

// Typedefs for things defined here in "support.h":

typedef int spec;

struct Stmt;

struct StmtList {
  Stmt *first;
  Stmt *last;
};

enum StmtKind {
  EXPR_STMT, IF_STMT, WHILE_STMT, DO_STMT, FOR_STMT,
  BLOCK_STMT, RETURN_STMT, DISCARD_STMT, COMMENT_STMT,
  LAST_STMTKIND
};

enum NodeKind {
  DECL_N=LAST_STMTKIND, SYMB_N, CONST_N, UNARY_N, BINARY_N, TRINARY_N,
  LAST_NODEKIND,
};

enum subopkind {
  SUB_NONE, SUB_S, SUB_V, SUB_VS, SUB_SV, SUB_M, SUB_VM, SUB_MV,
  SUB_Z, SUB_ZM, SUB_CS, SUB_CV, SUB_CM, SUB_KV,
};

#define OPCODE_TABLE \
PICK( VARIABLE_OP, "var",    IDENT_SY, SYMB_N, SUB_NONE ), \
PICK( MEMBER_OP,   "member", IDENT_SY, SYMB_N, SUB_NONE ), \
\
PICK( ICONST_OP,   "iconst",  0, CONST_N, SUB_S ), \
PICK( ICONST_V_OP, "iconstv", 0, CONST_N, SUB_V ), \
PICK( BCONST_OP,   "bconst",  0, CONST_N, SUB_S ), \
PICK( BCONST_V_OP, "bconstv", 0, CONST_N, SUB_V ), \
PICK( FCONST_OP,   "fconst",  0, CONST_N, SUB_S ), \
PICK( FCONST_V_OP, "fconstv", 0, CONST_N, SUB_V ), \
PICK( HCONST_OP,   "hconst",  0, CONST_N, SUB_S ), \
PICK( HCONST_V_OP, "hconstv", 0, CONST_N, SUB_V ), \
PICK( XCONST_OP,   "xconst",  0, CONST_N, SUB_S ), \
PICK( XCONST_V_OP, "xconstv", 0, CONST_N, SUB_V ), \
\
PICK( VECTOR_V_OP,  "vector",  0,   UNARY_N, SUB_V  ), \
PICK( SWIZZLE_Z_OP, "swizzle", '.', UNARY_N, SUB_Z  ), \
PICK( SWIZMAT_Z_OP, "swizmat", '.', UNARY_N, SUB_ZM ), \
PICK( CAST_CS_OP,   "cast",    '(', UNARY_N, SUB_CS ), \
PICK( CAST_CV_OP,   "castv",   '(', UNARY_N, SUB_CV ), \
PICK( CAST_CM_OP,   "castm",   '(', UNARY_N, SUB_CM ), \
PICK( NEG_OP,       "neg",     '-', UNARY_N, SUB_S  ), \
PICK( NEG_V_OP,     "negv",    '-', UNARY_N, SUB_V  ), \
PICK( POS_OP,       "pos",     '+', UNARY_N, SUB_S  ), \
PICK( POS_V_OP,     "posv",    '+', UNARY_N, SUB_V  ), \
PICK( NOT_OP,       "not",     '~', UNARY_N, SUB_S  ), \
PICK( NOT_V_OP,     "notv",    '~', UNARY_N, SUB_V  ), \
PICK( BNOT_OP,      "bnot",    '!', UNARY_N, SUB_S  ), \
PICK( BNOT_V_OP,    "bnotv",   '!', UNARY_N, SUB_V  ), \
\
PICK( KILL_OP,      "kill",    DISCARD_SY, UNARY_N, SUB_S ), \
\
PICK( PREDEC_OP,    "predec",  MINUSMINUS_SY, UNARY_N, SUB_S ), \
PICK( PREINC_OP,    "preinc",  PLUSPLUS_SY,   UNARY_N, SUB_S ), \
PICK( POSTDEC_OP,   "postdec", MINUSMINUS_SY, UNARY_N, SUB_S ), \
PICK( POSTINC_OP,   "postinc", PLUSPLUS_SY,   UNARY_N, SUB_S ), \
\
PICK( MEMBER_SELECTOR_OP, "mselect", '.',   BINARY_N, SUB_NONE ), \
PICK( ARRAY_INDEX_OP,     "index",   '[',   BINARY_N, SUB_NONE ), \
PICK( FUN_CALL_OP,        "call",    '(',   BINARY_N, SUB_NONE ), \
PICK( FUN_BUILTIN_OP,     "builtin", 0,     BINARY_N, SUB_NONE ), \
PICK( FUN_ARG_OP,         "arg",     0,     BINARY_N, SUB_NONE ), \
PICK( EXPR_LIST_OP,       "list",    0,     BINARY_N, SUB_NONE ), \
PICK( MUL_OP,             "mul",     '*',   BINARY_N, SUB_S    ), \
PICK( MUL_V_OP,           "mulv",    '*',   BINARY_N, SUB_V    ), \
PICK( MUL_SV_OP,          "mulsv",   '*',   BINARY_N, SUB_SV   ), \
PICK( MUL_VS_OP,          "mulvs",   '*',   BINARY_N, SUB_VS   ), \
PICK( DIV_OP,             "div",     '/',   BINARY_N, SUB_S    ), \
PICK( DIV_V_OP,           "divv",    '/',   BINARY_N, SUB_V    ), \
PICK( DIV_SV_OP,          "divsv",   '/',   BINARY_N, SUB_SV   ), \
PICK( DIV_VS_OP,          "divvs",   '/',   BINARY_N, SUB_VS   ), \
PICK( MOD_OP,             "mod",     '%',   BINARY_N, SUB_S    ), \
PICK( MOD_V_OP,           "modv",    '%',   BINARY_N, SUB_V    ), \
PICK( MOD_SV_OP,          "modsv",   '%',   BINARY_N, SUB_SV   ), \
PICK( MOD_VS_OP,          "modvs",   '%',   BINARY_N, SUB_VS   ), \
PICK( ADD_OP,             "add",     '+',   BINARY_N, SUB_S    ), \
PICK( ADD_V_OP,           "addv",    '+',   BINARY_N, SUB_V    ), \
PICK( ADD_SV_OP,          "addsv",   '+',   BINARY_N, SUB_SV   ), \
PICK( ADD_VS_OP,          "addvs",   '+',   BINARY_N, SUB_VS   ), \
PICK( SUB_OP,             "sub",     '-',   BINARY_N, SUB_S    ), \
PICK( SUB_V_OP,           "subv",    '-',   BINARY_N, SUB_V    ), \
PICK( SUB_SV_OP,          "subsv",   '-',   BINARY_N, SUB_SV   ), \
PICK( SUB_VS_OP,          "subvs",   '-',   BINARY_N, SUB_VS   ), \
PICK( SHL_OP,             "shl",     LL_SY, BINARY_N, SUB_S    ), \
PICK( SHL_V_OP,           "shlv",    LL_SY, BINARY_N, SUB_V    ), \
PICK( SHR_OP,             "shr",     GG_SY, BINARY_N, SUB_S    ), \
PICK( SHR_V_OP,           "shrv",    GG_SY, BINARY_N, SUB_V    ), \
PICK( LT_OP,              "lt",      '<',   BINARY_N, SUB_S    ), \
PICK( LT_V_OP,            "ltv",     '<',   BINARY_N, SUB_V    ), \
PICK( LT_SV_OP,           "ltsv",    '<',   BINARY_N, SUB_SV   ), \
PICK( LT_VS_OP,           "ltvs",    '<',   BINARY_N, SUB_VS   ), \
PICK( GT_OP,              "gt",      '>',   BINARY_N, SUB_S    ), \
PICK( GT_V_OP,            "gtv",     '>',   BINARY_N, SUB_V    ), \
PICK( GT_SV_OP,           "gtsv",    '>',   BINARY_N, SUB_SV   ), \
PICK( GT_VS_OP,           "gtvs",    '>',   BINARY_N, SUB_VS   ), \
PICK( LE_OP,              "le",      LE_SY, BINARY_N, SUB_S    ), \
PICK( LE_V_OP,            "lev",     LE_SY, BINARY_N, SUB_V    ), \
PICK( LE_SV_OP,           "lesv",    LE_SY, BINARY_N, SUB_SV   ), \
PICK( LE_VS_OP,           "levs",    LE_SY, BINARY_N, SUB_VS   ), \
PICK( GE_OP,              "ge",      GE_SY, BINARY_N, SUB_S    ), \
PICK( GE_V_OP,            "gev",     GE_SY, BINARY_N, SUB_V    ), \
PICK( GE_SV_OP,           "gesv",    GE_SY, BINARY_N, SUB_SV   ), \
PICK( GE_VS_OP,           "gevs",    GE_SY, BINARY_N, SUB_VS   ), \
PICK( EQ_OP,              "eq",      EQ_SY, BINARY_N, SUB_S    ), \
PICK( EQ_V_OP,            "eqv",     EQ_SY, BINARY_N, SUB_V    ), \
PICK( EQ_SV_OP,           "eqsv",    EQ_SY, BINARY_N, SUB_SV   ), \
PICK( EQ_VS_OP,           "eqvs",    EQ_SY, BINARY_N, SUB_VS   ), \
PICK( NE_OP,              "ne",      NE_SY, BINARY_N, SUB_S    ), \
PICK( NE_V_OP,            "nev",     NE_SY, BINARY_N, SUB_V    ), \
PICK( NE_SV_OP,           "nesv",    NE_SY, BINARY_N, SUB_SV   ), \
PICK( NE_VS_OP,           "nevs",    NE_SY, BINARY_N, SUB_VS   ), \
PICK( AND_OP,             "and",     '&',   BINARY_N, SUB_S    ), \
PICK( AND_V_OP,           "andv",    '&',   BINARY_N, SUB_V    ), \
PICK( AND_SV_OP,          "andsv",   '&',   BINARY_N, SUB_SV   ), \
PICK( AND_VS_OP,          "andvs",   '&',   BINARY_N, SUB_VS   ), \
PICK( XOR_OP,             "xor",     '^',   BINARY_N, SUB_S    ), \
PICK( XOR_V_OP,           "xorv",    '^',   BINARY_N, SUB_V    ), \
PICK( XOR_SV_OP,          "xorsv",   '^',   BINARY_N, SUB_SV   ), \
PICK( XOR_VS_OP,          "xorvs",   '^',   BINARY_N, SUB_VS   ), \
PICK( OR_OP,              "or",      '|',   BINARY_N, SUB_S    ), \
PICK( OR_V_OP,            "orv",     '|',   BINARY_N, SUB_V    ), \
PICK( OR_SV_OP,           "orsv",    '|',   BINARY_N, SUB_SV   ), \
PICK( OR_VS_OP,           "orvs",    '|',   BINARY_N, SUB_VS   ), \
PICK( BAND_OP,            "band",    AND_SY, BINARY_N, SUB_S    ), \
PICK( BAND_V_OP,          "bandv",   AND_SY, BINARY_N, SUB_V    ), \
PICK( BAND_SV_OP,         "bandsv",  AND_SY, BINARY_N, SUB_SV   ), \
PICK( BAND_VS_OP,         "bandvs",  AND_SY, BINARY_N, SUB_VS   ), \
PICK( BOR_OP,             "bor",     OR_SY, BINARY_N, SUB_S    ), \
PICK( BOR_V_OP,           "borv",    OR_SY, BINARY_N, SUB_V    ), \
PICK( BOR_SV_OP,          "borsv",   OR_SY, BINARY_N, SUB_SV   ), \
PICK( BOR_VS_OP,          "borvs",   OR_SY, BINARY_N, SUB_VS   ), \
PICK( ASSIGN_OP,          "assign",  '=',   BINARY_N, SUB_S    ), \
PICK( ASSIGN_V_OP,        "assignv", '=',   BINARY_N, SUB_V    ), \
PICK( ASSIGN_GEN_OP,      "assigngen", '=', BINARY_N, SUB_NONE ), \
PICK( ASSIGN_MASKED_KV_OP, "assignm", '=',  BINARY_N, SUB_KV  ), \
\
PICK( ASSIGNMINUS_OP,     "assign-", ASSIGNMINUS_SY, BINARY_N, SUB_S ), \
PICK( ASSIGNMOD_OP,       "assign%", ASSIGNMOD_SY,   BINARY_N, SUB_S ), \
PICK( ASSIGNPLUS_OP,      "assign+", ASSIGNPLUS_SY,  BINARY_N, SUB_S ), \
PICK( ASSIGNSLASH_OP,     "assign/", ASSIGNSLASH_SY, BINARY_N, SUB_S ), \
PICK( ASSIGNSTAR_OP,      "assign*", ASSIGNSTAR_SY,  BINARY_N, SUB_S ), \
PICK( COMMA_OP,           "comma",   ',', BINARY_N,  SUB_NONE ), \
\
PICK( COND_OP,            "cond",    '?', TRINARY_N, SUB_S    ), \
PICK( COND_V_OP,          "condv",   '?', TRINARY_N, SUB_V    ), \
PICK( COND_SV_OP,         "condsv",  '?', TRINARY_N, SUB_SV   ), \
PICK( COND_GEN_OP,        "condgen", '?', TRINARY_N, SUB_NONE ), \
PICK( ASSIGN_COND_OP,     "assc",    '@', TRINARY_N, SUB_S    ), \
PICK( ASSIGN_COND_V_OP,   "asscv",   '@', TRINARY_N, SUB_V    ), \
PICK( ASSIGN_COND_SV_OP,  "asscsc",  '@', TRINARY_N, SUB_VS   ), \
PICK( ASSIGN_COND_GEN_OP, "asscgen", '@', TRINARY_N, SUB_NONE ),


// Description of opcode classes:
//
// SSSS:  Size or number of values in an operand:
//        0:      scalar or other (like struct)
//        1 to 4: vector or array dimension
// TTTT:  Base type (see type properties)
//
// _:  0000 0000 0000 0000 0000 0000 0000 TTTT
// V:  0000 0000 0000 0000 0000 0000 SSSS TTTT
// VS: 0000 0000 0000 0000 0000 0000 SSSS TTTT
// SV: 0000 0000 0000 0000 0000 0000 SSSS TTTT
// M:  --- Not used yet, reserved for matrix inner product ---
// VM: 0000 0000 0000 0000 SSS2 0000 SSS1 TTTT - Vector length SSS2, Mat size SSS2 by SSS1
// MV: 0000 0000 0000 0000 SSS2 0000 SSS1 TTTT - Vector length SSS1, Mat size SSS2 by SSS1
// Z:  0000 0000 MMMM-MMMM SSS2 0000 SSS1 TTTT - Swizzle vector/scalar size SSS2 to SSS1
//                                              CTD -- the above appears to be wrong;
//                                              should be SSS1 to SSS2
// ZM: MMMM-MMMM-MMMM-MMMM SSS2 SSSR SSS1 TTTT - Swizzle matrix size SSS2 by SSS1 to SSSR
// CS: 0000 0000 0000 0000 0000 TTT2 0000 TTT1 - Cast scalar type base1 to base2
// CV: 0000 0000 0000 0000 0000 TTT2 SSS1 TTT1 - Cast vector
// CM: 0000 0000 0000 0000 SSS2 TTT2 SSS1 TTT1 - Case matrix
// KV: 0000 0000 0000 MMMM 0000 0000 SSSS TTTT - Masked vector write

#undef PICK
#define PICK(a, b, c, d, e) a

enum opcode {
  OPCODE_TABLE
  LAST_OPCODE,
  // Some useful offsets:
  OFFSET_S_OP = 0, OFFSET_V_OP = 1, OFFSET_SV_OP = 2, OFFSET_VS_OP = 3,
};

#undef PICK

extern const char *opcode_name[];
extern const Atom opcode_atom[];

#define SUBOP__(base)                                                   \
((base) & 15)

#define SUBOP_S(base)                                                   \
((base) & 15)

#define SUBOP_V(size, base)                                             \
((((size) & 15) << 4) | ((base) & 15))

#define SUBOP_VS(size, base)                                            \
((((size) & 15) << 4) | ((base) & 15))

#define SUBOP_SV(size, base)                                            \
((((size) & 15) << 4) | ((base) & 15))

#define SUBOP_MV(size2, size1, base)                                    \
((((size2) & 15) << 12) | (((size1) & 15) << 4) | ((base) & 15))

#define SUBOP_VM(size2, size1, base)                                    \
((((size2) & 15) << 12) | (((size1) & 15) << 4) | ((base) & 15))

#define SUBOP_Z(mask, size2, size1, base)                               \
(((mask) << 16) | (((size2) & 15) << 12) | (((size1) & 15) << 4) | ((base) & 15))

#define SUBOP_ZM(mask, sizer, size2, size1, base)                               \
(((mask) << 16) | (((size2) & 15) << 12) | (((sizer) & 15) << 8) | (((size1) & 15) << 4) | ((base) & 15))

#define SUBOP_CS(base2, base1)                                          \
((((base2) & 15) << 8) | ((base1) & 15))

#define SUBOP_CV(base2, size, base1)                                    \
(((((base2) & 15) << 8) | ((size) & 15) << 4) | ((base1) & 15))

#define SUBOP_CM(size2, base2, size1, base1)                            \
((((size2) & 15) << 12) | (((base2) & 15) << 8) | (((size1) & 15) << 4) | ((base1) & 15))

#define SUBOP_SET_T(subop, base)  ((subop) = ((subop) & ~15) | ((base) & 15))

#define SUBOP_SET_MASK(subop, mask)  ((subop) = ((subop) & ~0x00ff0000) | (((mask) & 0xff) << 16))

#define SUBOP_KV(mask, size, base)                                      \
((((mask) & 15) << 16) | (((size) & 15) << 4) | ((base) & 15))

#define SUBOP_GET_T1(subop)     ((subop) & 15)
#define SUBOP_GET_S1(subop)     (((subop) >> 4) & 15)
#define SUBOP_GET_T2(subop)     (((subop) >> 8) & 15)
#define SUBOP_GET_S2(subop)     (((subop) >> 12) & 15)
#define SUBOP_GET_S(subop)      (((subop) >> 4) & 15)
#define SUBOP_GET_T(subop)      ((subop) & 15)
#define SUBOP_GET_MASK(subop)   (((subop) >> 16) & 0xff)
#define SUBOP_GET_MASK16(subop) (((subop) >> 16) & 0xffff)

#define PRAGMA_HASH_STR "#"
#define COMMENT_CPP_STR "//"

struct DeclType {
	DeclType() : basetype( NULL ), IsDerived( 0 ), numNewDims( 0 ), type( NULL ) {
	}
	DeclType( const DeclType & rhs )
  : basetype( rhs.basetype ), IsDerived( rhs.IsDerived )
  , numNewDims( rhs.numNewDims ), storageClass( rhs.storageClass )
  , type( NULL )
	{
		SetType( rhs.type );
	}
	DeclType( CgContext *cg, Atom name, Type *baseType, TypeCategory category )
  : basetype( baseType ), IsDerived( 1 ), numNewDims( 0 ), storageClass( SC_Unknown )
	{
		type = NewType( cg, name, TB_UndefinedType, category, TD_Unknown, TQ_None, false, 0, 0 );
	}
	~DeclType() {
		delete type;
	}
	const DeclType & operator = ( const DeclType & rhs ) {
		basetype = rhs.basetype;
		IsDerived = rhs.IsDerived;
		numNewDims = rhs.numNewDims;
		storageClass = rhs.storageClass;
		SetType( rhs.type );
		return *this;
	}
  // Possibly derived, stack-resident pointer to and copy of a type.
  Type *basetype;     // Pointer to non-derived
  int IsDerived;      // TRUE if anything has been altered
  int numNewDims;     // Number of new dimensions added for this declarator
  StorageClass storageClass;   // Aplied to variables when defined, not part of the type
	void SetType( const Type * newType ) {
		Type * oldType = type;
		type = DupType( newType );
		delete oldType;
	}
	Type * GetType() {
		return type;
	}
	const Type * GetType() const {
		return type;
	}
  
private:
  Type *type;         // Local copy of type
};

struct Expr;


struct Expr;
struct Decl;
struct Symb;
struct Constant;
struct Unary;
struct Binary;
struct Trinary;

union ExprSubclass {
	Expr * expr;
	Decl * decl;
	Symb * symol;
	Constant * constant;
	Unary * unary;
	Binary * binary;
	Trinary * trinary;
};

#define SX_STRINGIFY_TO_DEBUG 0

struct Expr {
	Expr() : kind( LAST_NODEKIND ), type( NULL ), IsLValue( 0 ), IsConst( 0 ), HasSideEffects( 0 ), op( LAST_OPCODE ), subop( 0 )  {
		sub.expr = this;
		tempptr[0] = tempptr[1] = tempptr[2] = tempptr[3] = NULL;
	}
	virtual ~Expr() {}
	NodeKind kind;
  Type *type;
	ExprSubclass sub;
  int IsLValue;
  int IsConst;
  int HasSideEffects;
  void *tempptr[4];  // Used by backends
	opcode op;
	int subop;
  
#if SX_STRINGIFY_TO_DEBUG
	mutable std::string debugString;
	const std::string * Stringify( CgContext *cg ) const {
		StringifyToDebugString( cg );
		return &debugString;
	}
	void StringifyToDebugString( CgContext *cg ) const;
#endif
  
	void * operator new( size_t );
	void operator delete( void * );
};

struct Decl : public Expr {
private:
	Decl( const Decl & rhs ) {}
public:
	Decl();
	virtual ~Decl();
  // For declaration parsing
	SourceLoc loc;
  Atom name;           // Symbol name atom
  Atom semantics;
  DeclType type;         // Type collected while parsing
  Decl *next;
  Symbol *symb;       // Symbol table definition of actual object
  Decl *params;       // Actual paramaters to function declaration
  Expr *initexpr;     // Initializer
};

struct Symb : public Expr {
  Symbol *symbol;
};

union ScalarConstant {
  float f;
  int i;
};

struct Constant : public Expr {
	Constant() {
		val[0].i = val[1].i = val[2].i = val[3].i = 0;
	}
  ScalarConstant val[4];
};

struct Unary : public Expr {
	Unary() : arg( NULL ) {}
	virtual ~Unary() {
		delete arg;
	}
  Expr *arg;
};

struct Binary : public Expr {
	Binary() : left( NULL ), right( NULL ) {}
	virtual ~Binary() {
		delete left;
		delete right;
	}
  Expr *left, *right;
};

struct Trinary : public Expr {
	Trinary() : arg1( NULL ), arg2( NULL ), arg3( NULL ) {}
	virtual ~Trinary() {
		delete arg1;
		delete arg2;
		delete arg3;
	}
  Expr *arg1, *arg2, *arg3;
};

/*
 union Expr_rec {
 Exprhead common;
 Symb sym;
 Constant co;
 Unary un;
 Binary bin;
 Trinary tri;
 };
 */

struct Stmt {
	Stmt( StmtKind sKind, const SourceLoc & sLoc ) : kind( sKind ), next( NULL ), loc( sLoc ) {}
	virtual ~Stmt() {}
	StmtKind kind;
  Stmt *next;
  SourceLoc loc;
  
	void * operator new( size_t );
	void operator delete( void * );
};


inline void DeleteStatementList( Stmt * s ) {
	while ( s ) {
		Stmt * n = s->next;
		delete s;
		s = n;
	}
}

struct ExprStmt : public Stmt {
	ExprStmt( const SourceLoc & loc ) : Stmt( EXPR_STMT, loc ), expr( NULL ) {}
	virtual ~ExprStmt() {
		delete expr;
	}
  Expr *expr;
};

struct IfStmt : public Stmt {
	IfStmt( const SourceLoc & loc ) : Stmt( IF_STMT, loc ), cond( NULL ), thenstmt( NULL ), elsestmt( NULL ) {}
	virtual ~IfStmt() {
		delete cond;
		delete thenstmt;
		delete elsestmt;
	}
  Expr *cond;
  Stmt *thenstmt;
  Stmt *elsestmt;
};

struct WhileStmt : public Stmt {
	WhileStmt( StmtKind kind, const SourceLoc & loc ) : Stmt( kind, loc ), cond( NULL ), body( NULL ) {}
	virtual ~WhileStmt() {
		delete cond;
		delete body;
	}
  Expr *cond;
  Stmt *body;
};

struct ForStmt : public Stmt {
	ForStmt( const SourceLoc & loc ) : Stmt( FOR_STMT, loc ), init( NULL ), cond( NULL ), step( NULL ), body( NULL ) {}
	virtual ~ForStmt() {
		delete init;
		delete cond;
		delete step;
		delete body;
	}
  Stmt *init;
  Expr *cond;
  Stmt *step;
  Stmt *body;
};

struct BlockStmt : public Stmt {
	BlockStmt( const SourceLoc & loc ) : Stmt( BLOCK_STMT, loc ), body( NULL ) {}
	virtual ~BlockStmt() {
		DeleteStatementList( body );
	}
  Stmt *body;
};

struct ReturnStmt : public Stmt {
	ReturnStmt( const SourceLoc & loc );
	virtual ~ReturnStmt();
  Expr *expr;
};

struct DiscardStmt : public Stmt {
	DiscardStmt( const SourceLoc & loc ) : Stmt( COMMENT_STMT, loc ), cond( NULL ) {}
	virtual ~DiscardStmt() {
		delete cond;
	}
  Expr *cond;
};

struct CommentStmt : public Stmt {
	CommentStmt(  const SourceLoc & loc ) : Stmt( COMMENT_STMT, loc ) {}
  Atom str;
};

/*
 union Stmt_rec {
 common_stmt commonst;
 ExprStmt Exprst;
 IfStmt ifst;
 WhileStmt whilest;
 ForStmt forst;
 BlockStmt blockst;
 ReturnStmt returnst;
 DiscardStmt discardst;
 CommentStmt commentst;
 };
 */

Decl *NewDeclNode( CgContext *cg, SourceLoc *loc, Atom atom, DeclType *type);
Symb *NewSymbNode( CgContext *cg, opcode op, Symbol *fSymb);
Constant *NewIConstNode( CgContext *cg, opcode op, int fval, TypeBase base);
Constant *NewBConstNode( CgContext *cg, opcode op, int fval, TypeBase base);
Constant *NewFConstNode( CgContext *cg, opcode op, float fval, TypeBase base);
Constant *NewFConstNodeV( CgContext *cg, opcode op, float *fval, int len, TypeBase base);
Unary *NewUnopNode( CgContext *cg, opcode op, Expr *arg);
Unary *NewUnopSubNode( CgContext *cg, opcode op, int subop, Expr *arg);
Binary *NewBinopNode( CgContext *cg, opcode op, Expr *left, Expr *right);
Binary *NewBinopSubNode( CgContext *cg, opcode op, int subop, Expr *left, Expr *right);
Trinary *NewTriopNode( CgContext *cg, opcode op, Expr *arg1, Expr *arg2, Expr *arg3);
Trinary *NewTriopSubNode( CgContext *cg, opcode op, int subop, Expr *arg1, Expr *arg2, Expr *arg3);

Symb *DupSymbNode(const Symb *fSymb);
Constant *DupConstNode(const Constant *fconst);
Unary *DupUnaryNode(const Unary *fun);
Binary *DupBinaryNode(const Binary *fbin);
Trinary *DupTrinaryNode(const Trinary *ftri);
Expr *DupNode( CgContext *cg, const Expr *fExpr);

ExprStmt *NewExprStmt( CgContext *cg, SourceLoc *loc, Expr *fExpr);
IfStmt *NewIfStmt( CgContext *cg, SourceLoc *loc, Expr *fExpr, Stmt *thenStmt, Stmt *elseStmt);
IfStmt *SetThenElseStmts( CgContext *cg, SourceLoc *loc, Stmt *ifStmt, Stmt *thenStmt, Stmt *elseStmt);
WhileStmt *NewWhileStmt( CgContext *cg, SourceLoc *loc, StmtKind kind, Expr *fExpr, Stmt *body);
ForStmt *NewForStmt( CgContext *cg, SourceLoc *loc, Stmt *fExpr1, Expr *fExpr2, Stmt *fExpr3, Stmt *body);
BlockStmt *NewBlockStmt( CgContext *cg, SourceLoc *loc, Stmt *fStmt);
ReturnStmt *NewReturnStmt( CgContext *cg, SourceLoc *loc, Scope *fScope, Expr *fExpr);
DiscardStmt *NewDiscardStmt( CgContext *cg, SourceLoc *loc, Expr *fExpr);
CommentStmt *NewCommentStmt( CgContext *cg, SourceLoc *loc, const char *str);

/************************************* DeclType functions: *************************************/

Type *GetTypePointer( CgContext *cg, SourceLoc *loc, const DeclType *fDtype);
DeclType *SetDType(DeclType *fDtype, Type *fType);

bool SetTypeCategory( CgContext *cg, SourceLoc *loc, Atom atom, DeclType *fType, TypeCategory category, int Force);
bool SetTypeQualifiers( CgContext *cg, SourceLoc *loc, DeclType *fType, TypeQualifier qualifiers, bool isconst );
bool SetTypeDomain( CgContext *cg, SourceLoc *loc, DeclType *fType, TypeDomain domain);
int SetTypeMisc( CgContext *cg, SourceLoc *loc, DeclType *fType, int misc);
int SetTypePacked( CgContext *cg, SourceLoc *loc, DeclType *fType);
int SetStorageClass( CgContext *cg, SourceLoc *loc, DeclType *fType, int storage);

/********************************** Parser Semantic Rules: ***********************************/

Expr *Initializer( CgContext *cg, SourceLoc *loc, Expr *fExpr);
Expr *InitializerList( CgContext *cg, SourceLoc *loc, Expr *list, Expr *fExpr);
Expr *ArgumentList( CgContext *cg, SourceLoc *loc, Expr *flist, Expr *fExpr);
Expr *ExpressionList( CgContext *cg, SourceLoc *loc, Expr *flist, Expr *fExpr);
Decl *AddDecl( CgContext *cg, Decl *first, Decl *last);
Stmt *AddStmt( CgContext *cg, Stmt *first, Stmt *last);
Stmt *CheckStmt( CgContext *cg, Stmt *fStmt);
Decl *Function_Definition_Header( CgContext *cg, SourceLoc *loc, struct Decl *fDecl);
Decl *Param_Init_Declarator( CgContext *cg, SourceLoc *loc, Scope *fScope, Decl *fDecl, Expr *fExpr);
Stmt *Init_Declarator( CgContext *cg, SourceLoc *loc, Scope *fScope, Decl *fDecl, Expr *fExpr);
Decl *Declarator( CgContext *cg, SourceLoc *loc, Decl *fDecl, Atom semantics);
Decl *Array_Declarator( CgContext *cg, SourceLoc *loc, Decl *fDecl, int size, int Empty);

Symbol *AddFormalParamDecls( CgContext *cg, Scope *fScope, Decl *params);
Decl *SetFunTypeParams( CgContext *cg, Scope *fScope, Decl *func, Decl *params, Decl *actuals);
Decl *FunctionDeclHeader( CgContext *cg, SourceLoc *loc, Scope *fScope, Decl *func);

Type *StructHeader( CgContext *cg, SourceLoc *loc, Scope *fScope, Atom ctype, Atom tag);

Symbol *DefineVar( CgContext *cg, SourceLoc *loc, Scope *fScope, Atom atom, Type *fType);
Symbol *DefineTypedef( CgContext *cg, SourceLoc *loc, Scope *fScope, Atom atom, Type *fType);
Symbol *DeclareFunc( CgContext *cg, SourceLoc *loc, Scope *fScope, Symbol *fSymb, Atom atom, Type *fType,
                    Scope *locals, Symbol *params);

void DefineFunction( CgContext *cg, SourceLoc *loc, Scope *fScope, struct Decl *func, Stmt *body);
int GlobalInitStatements( CgContext *cg, Scope *fScope, Stmt *fStmt);

Expr *BasicVariable( CgContext *cg, SourceLoc *loc, Atom name);

int IsLValue(const Expr *fExpr);
int IsConst(const Expr *fExpr);
int IsArrayIndex(const Expr *fExpr);
int ConvertType( CgContext *cg, Expr *fExpr, Type *toType, Type *fromType, Expr **result, int IgnorePacked,
                bool isExplicit);
Expr *CastScalarVectorMatrix( CgContext *cg, Expr *fExpr, TypeBase fbase, TypeBase tbase, int len, int len2);
TypeBase ConvertNumericOperands( CgContext *cg, int baseop, Expr **lexpr, Expr **rexpr, TypeBase lbase, TypeBase rbase,
                                int llen, int rlen, int llen2, int rlen2);
Expr *CheckBooleanExpr( CgContext *cg, SourceLoc *loc, Expr *fExpr, int AllowVector);

Expr *NewUnaryOperator( CgContext *cg, SourceLoc *loc, int fop, Atom name, Expr *fExpr, int IntegralOnly);
Expr *NewBinaryOperator( CgContext *cg, SourceLoc *loc, int fop, Atom name, Expr *lExpr, Expr *rExpr,
                        int IntegralOnly);
Expr *NewBinaryBooleanOperator( CgContext *cg, SourceLoc *loc, int fop, Atom name, Expr *lExpr, Expr *rExpr);
Expr *NewBinaryComparisonOperator( CgContext *cg, SourceLoc *loc, int fop, Atom name, Expr *lExpr, Expr *rExpr);
Expr *NewConditionalOperator( CgContext *cg, SourceLoc *loc, Expr *bExpr, Expr *lExpr, Expr *rExpr);
Expr *NewSwizzleOperator( CgContext *cg, SourceLoc *loc, Expr *fExpr, int ident);
Expr *NewMatrixSwizzleOperator( CgContext *cg, SourceLoc *loc, Expr *fExpr, int ident);
Expr *NewVectorConstructor( CgContext *cg, SourceLoc *loc, Type *fType, Expr *fExpr);
Expr *NewCastOperator( CgContext *cg, SourceLoc *loc, Expr *fExpr, Type *toType);
Expr *NewMemberSelectorOrSwizzleOrWriteMaskOperator( CgContext *cg, SourceLoc *loc, Expr *fExpr, Atom ident);
Expr *NewIndexOperator( CgContext *cg, SourceLoc *loc, Expr *fExpr, Expr *ixExpr);
Expr *NewFunctionCallOperator( CgContext *cg, SourceLoc *loc, Expr *funExpr, Expr *actuals);

Expr *NewSimpleAssignment(  CgContext *cg, SourceLoc *loc, Expr *fvar, Expr *fExpr, int InInit);
Stmt *NewSimpleAssignmentStmt( CgContext *cg, SourceLoc *loc, Expr *fvar, Expr *fExpr, int InInit);
Stmt *NewCompoundAssignmentStmt( CgContext *cg, SourceLoc *loc, opcode op, Expr *fvar, Expr *fExpr);
/***
 Stmt *NewMaskedAssignment(SourceLoc *loc, int mask, Expr *fExpr, Stmt *fStmt);
 Stmt *NewConditionalAssignment(SourceLoc *loc, Expr *fCond, Expr *fExpr, Stmt *fStmt);
 ***/

/********************************** Traversal Routeins: ******************************************/

typedef Expr *(*ToExpr)( CgContext *, Expr *, void *, int );
typedef Stmt *(*ToStmt)( CgContext *, Stmt *, void *, int );

Expr *ApplyToNodes( CgContext * cg, ToExpr pre, ToExpr post, Expr *fExpr, void *arg1, int arg2);
void ApplyToExpressions( CgContext * cg, ToExpr pre, ToExpr post, Stmt *fStmt, void *arg1, int arg2);
void ApplyToExpressionsLocal( CgContext * cg, ToExpr pre, ToExpr post, Stmt *fStmt, void *arg1, int arg2);
void ApplyToTopExpressions( CgContext * cg, ToExpr fun, Stmt *fStmt, void *arg1, int arg2);
Stmt *ApplyToStatements( CgContext * cg, ToStmt pre, ToStmt post, Stmt *fStmt, void *arg1, int arg2);

inline Expr *PreApplyToNodes( CgContext * cg, ToExpr pre, Expr *fExpr, void *arg1, int arg2 ) { return ApplyToNodes( cg, pre, NULL, fExpr, arg1, arg2 ); }
inline Expr *PostApplyToNodes( CgContext * cg, ToExpr post, Expr *fExpr, void *arg1, int arg2 ) { return ApplyToNodes( cg, NULL, post, fExpr, arg1, arg2 ); }
inline void PreApplyToExpressions( CgContext * cg, ToExpr pre, Stmt *fStmt, void *arg1, int arg2 ) { ApplyToExpressions( cg, pre, NULL, fStmt, arg1, arg2 ); }
inline void PostApplyToExpressions( CgContext * cg, ToExpr post, Stmt *fStmt, void *arg1, int arg2 ) { ApplyToExpressions( cg, NULL, post, fStmt, arg1, arg2 ); }
inline void PreApplyToExpressionsLocal( CgContext * cg, ToExpr pre, Stmt *fStmt, void *arg1, int arg2) { ApplyToExpressionsLocal( cg, pre, NULL, fStmt, arg1, arg2 ); }
inline void PostApplyToExpressionsLocal( CgContext * cg, ToExpr post, Stmt *fStmt, void *arg1, int arg2) { ApplyToExpressionsLocal( cg, NULL, post, fStmt, arg1, arg2 ); }
inline Stmt *PreApplyToStatements( CgContext * cg, ToStmt pre, Stmt *fStmt, void *arg1, int arg2 ) { return ApplyToStatements( cg, pre, NULL, fStmt, arg1, arg2 ); }
inline Stmt *PostApplyToStatements( CgContext * cg, ToStmt post, Stmt *fStmt, void *arg1, int arg2 ) { return ApplyToStatements( cg, NULL, post, fStmt, arg1, arg2 ); }

void PostApplyToChildStatements( CgContext * cg, ToStmt fun, Stmt *fStmt, void *arg1, int arg2);

/************************************** misc: ************************************************/
void InitTempptr( CgContext *cg, Expr *e, void *arg1, int arg2);
/********************************** Error checking: ******************************************/

void RecordErrorPos( CgContext *cg, SourceLoc *loc);
void MarkErrorPosHit(CgContext *cg, SourceLoc *loc);
void CheckAllErrorsGenerated( CgContext *cg );

struct ErrorLoc
{
	ErrorLoc() : hit( 0 ), next( NULL )
	{}
	
	// The source location at which the error token was encountered.
	SourceLoc loc;
	// 1 iff an error was found on this line or before this line after
	// the previous error token.
	int hit;
	ErrorLoc * next;
	
	void * operator new( size_t );
	void operator delete( void * );
};


#endif // !defined(__SUPPORT_H)
