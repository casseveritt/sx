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
// symbols.h
//

#if !defined(__SYMBOLS_H)
#define __SYMBOLS_H 1

#include "type.h"

enum SymbolKind {
  SK_Invalid, SK_Variable, SK_Typedef, SK_Function, SK_Constant, SK_Tag, SK_Macro,
};

enum StorageClass {
  SC_Unknown, SC_Auto, SC_Static, SC_Extern,
};

#define SYMB_IS_PARAMETER           0x000001    // Symbol is a formal parameter
#define SYMB_IS_DEFINED             0x000002    // Symbol is defined.  Currently only used for functions.
#define SYMB_IS_BUILTIN             0x000004    // Symbol is a built-in function.
#define SYMB_IS_INLINE_FUNCTION     0x000008    // Symbol is a function that will be inlined.
#define SYMB_IS_CONNECTOR_REGISTER  0x000010    // Symbol is a connector hw register
#define SYMB_CONNECTOR_CAN_READ     0x000020    // Symbol is a readable connector hw register
#define SYMB_CONNECTOR_CAN_WRITE    0x000040    // Symbol is a writable connector hw register
#define SYMB_NEEDS_BINDING          0x000080    // Symbol is a non-static global and has not yet been bound

// Typedefs for things defined in "support.h":

// Typedefs for things defined here in "symbols.h":

struct FunSymbol;
struct Symbol;
struct Type;

struct Stmt;
struct Expr;

struct SymbolList {
	//char bloat[35];
  SymbolList *next;
  Symbol *symb;
  
	void * operator new( size_t );
	void operator delete( void * );
};

struct Scope {
	Scope( CgContext *sCg );
	~Scope();
	
	CgContext *cg;
  Scope *next, *prev;     // doubly-linked list of all scopes
  Scope *parent;
  Scope *funScope;        // Points to base scope of enclosing function
  Symbol *symbols;
  Symbol *tags;
  Symbol *params;
  Type *returnType;
  int level;              // 0 = super globals, 1 = globals, etc.
  int funindex;           // Identifies which function contains scope
  int InFormalParameters; // > 0 when parsing formal parameters.
	bool HasVoidParameter;
	bool HasReturnStmt;
  bool IsStructScope;
  bool HasSemantics;       // Struct scope has members with semantics
  int pid;                // Program type id
  // Only used at global scope (level 1):
  SymbolList *programs;   // List of programs for this compilation.
  Stmt *initStmts;        // Global initialization statements.
  
	void * operator new( size_t );
	void operator delete( void * );
};

struct TypeList {
	TypeList() : next( NULL ), type( NULL ) {}
	~TypeList() {
		delete next;
		// does not own type pointer
	}
  TypeList *next;
  Type *type;
	
	void * operator new( size_t );
	void operator delete( void * );
};

struct Type;
struct TypeScalar;
struct TypeArray;
struct TypeStruct;
struct TypeFunction;

union TypeSubclass {
	Type * type;
	TypeScalar * scalar;
	TypeArray * array;
	TypeStruct * strct;
	TypeFunction * function;
};

struct Type {
protected:
	Type( const Type & t ) : category( t.category ) {}
public:
	Type( CgContext *tCg, Atom tName, TypeBase tBase, TypeCategory tCategory, TypeDomain tDomain, TypeQualifier tQualifier, bool isconst, int tProps, int tSize );
	virtual ~Type();
  
	CgContext *cg;
	TypeSubclass sub;
	Atom name;
	TypeBase base;
	const TypeCategory category;
	TypeDomain domain;
	TypeQualifier qualifier;
	bool isConst;
  int properties;
  int size;
  
	void * operator new( size_t );
	void operator delete( void * );
};

struct TypeScalar : public Type {
private:
	TypeScalar( const TypeScalar & t ) : Type( t ) {}
public:
	TypeScalar( CgContext *cg, Atom tName, TypeBase tBase, TypeDomain tDomain, TypeQualifier tQualifier, bool isconst, int tProps, int tSize )
  : Type( cg, tName, tBase, TC_Scalar, tDomain, tQualifier, isconst, tProps, tSize ) {}
};

struct TypeArray : public Type {
	TypeArray( CgContext *cg, Atom tName, TypeBase tBase, TypeDomain tDomain, TypeQualifier tQualifier, bool isconst, int tProps, int tSize )
  : Type( cg, tName, tBase, TC_Array, tDomain, tQualifier, isconst, tProps, tSize ), eltype( NULL ), numels( 0 ) {}
  Type *eltype;
  int numels;
};

struct TypeStruct : public Type { // for structs and connectors
	TypeStruct( CgContext *cg, Atom tName, TypeBase tBase, TypeDomain tDomain, TypeQualifier tQualifier, bool isconst, int tProps, int tSize )
  : Type( cg, tName, tBase, TC_Struct, tDomain, tQualifier, isconst, tProps, tSize )
  , unqualifiedtype( NULL ), members( NULL ), variety( 0 ), allocated( NULL )
  , csize( 0 ), tempptr( NULL )
	{}
  Type *unqualifiedtype;
  Scope *members;
  SourceLoc loc;
  Atom tag;          // struct or connector tag
  Atom semantics;
  int variety;      // connector variety
  int HasSemantics; // set if any members have semantics
  char *allocated;  // set if corresponding register has been bound
  int csize;
  void *tempptr;    // temp for FP30 backend connectors: dagnode* to DOP_VARYING
};

struct TypeFunction : public Type {
	TypeFunction( CgContext *cg, Atom tName, TypeBase tBase, TypeDomain tDomain, TypeQualifier tQualifier, bool isconst, int tProps, int tSize )
  : Type( cg, tName, tBase, TC_Function, tDomain, tQualifier, isconst, tProps, tSize ), rettype( NULL ), paramtypes( NULL )
	{
		;
	}
	virtual ~TypeFunction();
  Type *rettype;
  TypeList *paramtypes;
};

// Symbol table is a simple Binary tree.

struct FunSymbol {
  Scope *locals;
  Symbol *params;
  Stmt *statements;
  Symbol *overload;   // List of overloaded versions of this function
  int flags;          // Used when resolving overloaded reference
  short group;        // Built-in function group
  short index;        // Built-in function index
  char HasOutParams;
};

struct VarSymbol {
  int addr;           // Address or member offset
  Atom semantics;
  Binding *bind;
  Expr *init;   // For initialized non-static globals
};

struct ConstSymbol {
  int value;          // Constant value: 0 = false, 1 = true
};

#include "cpp.h"        // to get MacroSymbol def

struct Symbol {
	Symbol()
  : left( NULL ), right( NULL ), next( NULL ), type( NULL )
  , kind( SK_Invalid ), properties( 0 ), storageClass( SC_Unknown )
  , flags( 0 ), tempptr( NULL ), tempptr2( NULL )
	{}
	~Symbol();
  
	Symbol *left, *right;
  Symbol *next;
  Atom name;       // Name atom
  Type *type;     // Type descriptor
  SourceLoc loc;
  SymbolKind kind;
  int properties; // Symbol properties
  StorageClass storageClass;
  int flags;      // Temp for various semantic uses
  void *tempptr;  // DAG rewrite temp: Expr*, for SSA Expr. that writes this sym
  void *tempptr2; // DAG rewrite temp: dagnode* to DOP_UNIFORM, for input var
  struct Details { // FIXME: Does this bloat need to be addressed?
    FunSymbol fun;
    VarSymbol var;
    ConstSymbol con;
    MacroSymbol mac;
  } details;
  
	void * operator new( size_t );
	void operator delete( void * );
};


void SetScalarTypeName(int base, Atom name, Type *fType);

void PushScope(CgContext *cg, Scope *fScope);
Scope *PopScope(CgContext *cg ); // implicitly frees scope
Symbol *NewSymbol(CgContext *cg, SourceLoc *loc, Scope *fScope, Atom name, Type *fType, SymbolKind kind);
Symbol *AddSymbol(CgContext *cg, SourceLoc *loc, Scope *fScope, Atom atom, Type *fType, SymbolKind kind);
Symbol *UniqueSymbol(CgContext *cg, Scope *fScope, Type *fType, SymbolKind kind);
Symbol *AddTag(CgContext *cg, SourceLoc *loc, Scope *fScope, Atom atom, TypeCategory category);
Symbol *LookupLocalSymbol(CgContext *cg, Scope *fScope, Atom atom);
Symbol *LookupLocalSymbolBySemanticName(CgContext *cg, Scope *fScope, Atom atom);
Symbol *LookupLocalSymbolByBindingName(CgContext *cg, Scope *fScope, Atom atom);
Symbol *LookupLocalTag(CgContext *cg, Scope *fScope, Atom atom);
Symbol *LookupSymbol(CgContext *cg, Scope *fScope, Atom atom);
Symbol *LookupTag(CgContext *cg, Scope *fScope, Atom atom);
Type *LookupTypeSymbol( CgContext *cg, Scope *fScope, Atom atom);

void InitType(Type *fType);

Type *NewType( CgContext *cg, Atom n, TypeBase b, TypeCategory c, TypeDomain d, TypeQualifier q, bool isConst, int properties, int size);
Type *DupType( const Type *fType);
Type *NewPackedArrayType(CgContext *cg, Type *elType, int numels, int properties);

bool IsCategory(const Type *fType, TypeCategory category);
bool IsTypeBase(const Type *fType, TypeBase base);
bool IsVoid(const Type *fType);
bool IsBoolean(const Type *fType);
bool IsArray(const Type *fType);
bool IsScalar(const Type *fType);
bool IsVector(const Type *fType, int *len);
bool IsMatrix(const Type *fType, int *len, int *len2);
bool IsUnsizedArray(const Type *fType);
bool IsStruct(const Type *fType);
bool IsProgram(const Type *fType);
bool IsPacked(const Type *fType);
bool IsSameUnqualifiedType(const Type *aType, const Type *bType);
bool IsTypedef(const Symbol *fSymb);
bool IsFunction(const Symbol *fSymb);
bool IsInline(const Symbol *fSymb);
TypeBase GetBase(const Type *fType);
TypeCategory GetCategory(const Type *fType);
TypeDomain GetDomain(const Type *fType);
TypeQualifier GetQualifiers(const Type *fType);
int GetQuadRegSize(const Type *fType);
void ClearTypeMisc(Type *fType, int misc);

Type *GetStandardType(CgContext *cg, TypeBase tbase, int tlen, int tlen2);
Type *GetElementType( CgContext *cg, const Type *fType);

void SetStructMemberOffsets(CgContext *cg, Type *fType);
Type *SetStructMembers(CgContext *cg, SourceLoc *loc, Type *fType, Scope *members);

void AddParameter(CgContext *cg, Scope *fScope, Symbol *param);

int GetSwizzleOrWriteMask(CgContext *cg, SourceLoc *loc, Atom atom, int *FIsLValue, int *flen);
int GetMatrixSwizzleOrWriteMask(CgContext *cg, SourceLoc *loc, Atom atom, int *FIsLValue, int *flen);
const char *GetBaseTypeNameString(CgContext *cg, int base);

void ClearAllSymbolTempptr( CgContext * cg );
void ClearAllSymbolTempptr2( CgContext *cg );

#endif // !defined(__SYMBOLS_H)

