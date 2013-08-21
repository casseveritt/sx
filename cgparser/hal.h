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
// hal.h
//

#if !defined(__Hal_H)
#define __Hal_H 1

// Typedefs for things defined here in "hal.h":

struct Hal;
struct Profile;

// Profile and connector IDs for non-programs and non-connectors:

#define PID_NONE_ID    0
#define CID_NONE_ID    0
#define CID_INVALID_ID 1 // Marks connector as invalid to prevent multiple errors

// Connector capabilities bits returned by GetConnectorUses:

#define CONNECTOR_IS_USELESS  0x0000
#define CONNECTOR_IS_INPUT    0x0001
#define CONNECTOR_IS_OUTPUT   0x0002

//#define REG_NONE            0x0000
#define REG_ALLOC           0x0001
#define REG_RESERVED        0x0002
#define REG_HIDDEN          0x0004
#define REG_WRITE_REQUIRED  0x0008
#define REG_INPUT           0x0010
#define REG_OUTPUT          0x0020

#define CAPS_INLINE_ALL_FUNCTIONS       1
#define CAPS_GLOBAL_DEBUG_VEC4          2
#define CAPS_RESTRICT_RETURNS           3
#define CAPS_DECONSTRUCT_MATRICES       4
#define CAPS_LATE_BINDINGS              5
#define CAPS_INDEXED_ARRAYS             6
#define CAPS_DONT_FLATTEN_IF_STATEMENTS 7

struct Profile {
	Profile() : halFactory( NULL ) {}
	const char *name;
  Hal *(*halFactory)( CgContext *cg, Atom entryPoint );
};

// Hal version of connector register description:
//{ "ATTR0",  0, FLT, REG_AP2V_ATTR0,  4, REG_ALLOC | REG_INPUT, },

struct ConnectorRegisters {
	ConnectorRegisters() : sname( NULL ), base( TB_NoType ), regno( 0 ), size( 0 ), properties( 0 ) {}
	ConnectorRegisters( const char *crSName, Atom crName, TypeBase crBase, int crRegno, int crSize, int crProperties )
	: sname( crSName ), name( crName ), base( crBase ), regno( crRegno ), size( crSize ), properties( crProperties )
	{}
  const char *sname;
  Atom name;    // atom
  TypeBase base;
  int regno;
  int size;
  int properties;
};

struct ConnectorDescriptor {
	ConnectorDescriptor() : sname( NULL ), cid( 0 ), properties( 0 ), numregs( 0 ), registers( NULL ) {}
	ConnectorDescriptor( const char *cdSName, Atom cdName, int cdCid, int cdProperties, int cdNumregs, ConnectorRegisters * cdRegisters )
	: sname( cdSName ), name( cdName ), cid( cdCid ), properties( cdProperties), numregs( cdNumregs ), registers( cdRegisters )
	{}
  const char *sname;
  Atom name;   // atom
  int cid;
  int properties;
  int numregs;
  ConnectorRegisters *registers;
};

// Hal version of "semantics"  descriptions:

enum SemanticProperties {
  SEM_IN =         1,
	SEM_OUT =        2,
	SEM_UNIFORM =    4,
	SEM_VARYING =    8,
	SEM_HIDDEN =    16,
  SEM_EXCLUSIVE = 32,
	SEM_REQUIRED =  64
};

struct SemanticsDescriptor {
	SemanticsDescriptor() : sname( NULL ), base( TB_NoType ), size( 0 ), regno( 0 ), numregs( 0 ), reggroup( 0 ), properties( 0 ) {}
	SemanticsDescriptor( const char * sdSName, TypeBase sdBase, int sdSize, int sdRegno, int sdNumregs, int sdReggroup, int sdProperties )
	: sname( sdSName ), base( sdBase ), size( sdSize ), regno( sdRegno ), numregs( sdNumregs ), reggroup( sdReggroup ), properties( sdProperties )
	{}
  const char *sname;
  TypeBase base;
  int size;
  int regno;
  int numregs;
  int reggroup;
  int properties;
};


struct Hal {
private:
	Hal( const Hal & h ) {} // no copies
	const Hal & operator= ( const Hal & h ) { return *this; } // no assignment
public:
	Hal( CgContext * hCg, Atom hEntryName );
	virtual ~Hal();
	
	// non-virtual methods
	void AddConstantBinding(Binding *fBind);
	void AddDefaultBinding(Binding *fBind);
  
  
	// pure methods
  virtual bool RegisterNames() = 0;
  virtual int GetConnectorID(Atom) = 0;
  virtual Atom GetConnectorAtom(int) = 0;
  virtual bool PrintCodeHeader() = 0;
  virtual bool GenerateCode(SourceLoc *loc, Scope *fScope, Symbol *program) = 0;
  
	// non-pure methods
	virtual bool GetCapsBit(int bitNumber) { return false; }
	virtual int GetConnectorUses(int, int) { return CONNECTOR_IS_USELESS; }
	virtual bool GetConnectorRegister(int cid, int ByIndex, Atom ratom, Binding *fBind) { return false; }
	virtual TypeBase GetFloatSuffixBase(SourceLoc *loc, int suffix);
	virtual int GetSizeof(Type *fType);
	virtual int GetAlignment(Type *fType);
	virtual bool CheckDeclarators(SourceLoc *loc, const DeclType *fDtype);
	virtual bool CheckDefinition(SourceLoc *loc, Atom name, const Type *fType) { return true; }
	virtual bool CheckStatement(SourceLoc *loc, Stmt *fstmt) { return true; }
	virtual bool CheckInternalFunction(Symbol *fSymb, int *group) { return false; }
	virtual bool IsValidScalarCast(TypeBase toBase, TypeBase fromBase, bool isExplicit);
	virtual bool IsValidOperator(SourceLoc *loc, Atom name, int op, int subop) { return true; }
	virtual bool IsNumericBase(TypeBase fBase);
	virtual bool IsIntegralBase(TypeBase fBase);
	virtual bool IsTexobjBase(TypeBase fBase) { return false; }
	virtual bool IsValidRuntimeBase(TypeBase fBase);
	virtual TypeBase GetBinOpBase(int lop, TypeBase lbase, TypeBase rbase, int llen, int rlen);
	virtual bool ConvertConstant(const ScalarConstant *fval, TypeBase fbase, TypeBase tbase, Expr **fexpr);
	virtual bool BindUniformUnbound(SourceLoc *loc, Symbol *fSymb, Binding *lBind) { return false; }
	virtual bool BindUniformPragma(SourceLoc *loc, Symbol *fSymb, Binding *lBind, const Binding *fBind) { return false; }
	virtual bool BindVaryingSemantic(SourceLoc *loc, Symbol *fSymb, Atom semantic, Binding *fBind, int IsOutVal) { return false; }
	virtual bool BindVaryingPragma(SourceLoc *loc, Symbol *fSymb, Binding *lBind, const Binding *fBind, int IsOutVal) { return false; }
	virtual bool BindVaryingUnbound(SourceLoc *loc, Symbol *fSymb, Atom name, Atom semantic, Binding *fBind, int IsOutVal) { return false; }
  
	CgContext * cg;
  // Profile specific data members:
  
  const char *vendor;
  const char *version;
  
  Atom profileName;
  int pid;
  Atom entryName;
  
  SemanticsDescriptor *semantics;
  int numSemantics;
  
  int incid;
  ConnectorRegisters *inputCRegs;
  int numInputCRegs;
  
  int nextUnboundVinReg;
  int lastUnboundVinReg;
  
  int outcid;
  ConnectorRegisters *outputCRegs;
  int numOutputCRegs;
  
  int nextUnboundVoutReg;
  int lastUnboundVoutReg;
  
  // Program specific data:
  
  Scope *globalScope;
  Symbol *varyingIn;
  Symbol *varyingOut;
  SymbolList *uniformParam;
  SymbolList *uniformGlobal;
  UniformSemantic *uniforms;
  
  BindingList *constantBindings;
  BindingList *defaultBindings;
  
  // Misc data
  
  const char *comment;
  
	void * operator new( size_t );
	void operator delete( void * );
};

Profile *RegisterProfile(Hal * (*halFactory)( CgContext *cg, Atom entryName), const char *profileName );
Profile *EnumerateProfiles(int index);

int InitHal( CgContext *cg, const char *profileName, const char *entryName);

void SetSymbolConnectorBinding(Binding *fBind, ConnectorRegisters *fConn);
ConnectorDescriptor *LookupConnector(ConnectorDescriptor *connectors, int cid, int num);


#endif // !defined(__Hal_H)
