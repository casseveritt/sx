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
// compile.h
//

#if !defined(__COMPILE_H)
#define __COMPILE_H 1

#include <sstream>
#include <set>


struct Options {
	Options()
  : profileString( NULL ), entryName( NULL )
  , sourceFileName( NULL ), outputFileName( NULL ), listFileName( NULL )
  , debugMode( false ), errorMode( false ), noCodeGen( false ), noWarnings( false )
  , quiet( false ), printVersion( false ), noStdlib( false ), allowLongPrograms( false )
  , positionInvariant( false ), tokenize( false ), dumpAtomTable( false ), traceScanner( false )
  , dumpParseTree( false ), dumpFinalTree( false ), dumpNodeTree( false ), trapOnError( false )
  , comments( false ), debugLevel( 0 )
	{}
	
  const char *profileString;
  const char *entryName;
  const char *sourceFileName;
  const char *outputFileName;
  const char *listFileName;
  bool debugMode;
	bool errorMode;
  bool noCodeGen;
  bool noWarnings;
  bool quiet;
  bool printVersion;
  bool noStdlib;
  bool allowLongPrograms;
  bool positionInvariant;
  // Tools:
  bool tokenize;
  // Debug The Compiler options:
  bool dumpAtomTable;
  bool traceScanner;
  bool dumpParseTree;
  bool dumpFinalTree;
  bool dumpNodeTree;
	bool trapOnError;
  bool comments;
	int debugLevel;             // Code generator debug level
};

struct ErrorLoc;
struct Scope;

struct CgContext {
	CgContext( Options & opt );
	~CgContext();
  
  // Public members
  SourceLoc *pLastSourceLoc;  // Set at the start of each statement by the tree walkers
  Options options;            // Compile options and parameters
  BindingTree *bindings;      // #pragma bind bindings
  Profile *allProfiles;     // List of supported profiles
  Hal *theHal;              // Current profile's Hal
  
  // Private members
  SourceLoc lastSourceLoc;
  
  // Scanner data:
  
  SourceLoc *tokenLoc;        // Source location of most recent token seen by the scanner
  Atom mostRecentToken;        // Most recent token seen by the scanner
  InputSrc *currentInput;
  
  
  // Private members:
  SourceLoc ltokenLoc;
  int errorCount;
  int warningCount;
  int lineCount;
  int allowSemanticParseErrors;// Allow exactly one after each parse error.
	
	// flush to stdout output sent directly to streams - for debugging
	size_t outFlushPoint;
	size_t listingFlushPoint;
	void FlushOutput();
	
	
	std::ostringstream outStream;
	std::ostringstream listingStream;
	
  
	void PrintOptions( int argc, char **argv );
	
	void WriteOutputHeader();
	void WriteListingHeader();
	int WriteOutputFiles( const char *mess );
  
	int CompileProgram( SourceLoc *loc, Scope *fScope );
	void OutputBindings( Hal *fHal, Symbol *program );
  
	void StartGlobalScope();
  
	void Printf( const char *fmt, ... );
	void OutputPrintf( const char *fmt, ... );
	void OutputVPrintf( const char *fmt, va_list args );
	void ListingPrintf( const char *fmt, ... );
	void ListingVPrintf( const char *fmt, va_list args );
	void DebugPrintf( const char *fmt, ... );
  
	// convenience methods
	const char *GetString( const Atom & a ) {
		return atable->GetAtomString( a );
	}
	Atom GetAtom( const char * str ) {
		return atable->LookupAdd( str );
	}
	AtomTable * GetAtomTable() {
		return atable;
	}
  
	// tracking type instances
	void AddType( Type * type );
	void RemoveType( Type * type );
	std::set< Type * > types;
  
	Scope *currentScope;
  
	// Beginning and end of list of error locations, sorted by line.
	ErrorLoc * errorLocsFirst;
	ErrorLoc * errorLocsLast;
	int errorPending;
  
  
	Scope *scopeList;
	Scope *globalScope;
	int nextFunctionIndex;
  
	Type *UndefinedType;
	Type *CFloatType;
	Type *CIntType;
	Type *VoidType;
	Type *FloatType;
	Type *IntType;
	Type *BooleanType;
  
	Type *CFloat1Type;
	Type *CFloat2Type;
	Type *CFloat3Type;
	Type *CFloat4Type;
  
	Type *CInt1Type;
	Type *CInt2Type;
	Type *CInt3Type;
	Type *CInt4Type;
  
	Type *Float1Type;
	Type *Float2Type;
	Type *Float3Type;
	Type *Float4Type;
  
	Type *Int1Type;
	Type *Int2Type;
	Type *Int3Type;
	Type *Int4Type;
  
	Type *Boolean1Type;
	Type *Boolean2Type;
	Type *Boolean3Type;
	Type *Boolean4Type;
  
	Symbol *FalseSymb;
	Symbol *TrueSymb;
  
	DeclType currentDeclTypeSpecs;
  
	void * operator new( size_t );
	void operator delete( void * );
private:
	AtomTable *atable;
};

void SetSymbolFlags( Symbol *fSymb, int fVal );
void SetSymbolFlagsList( Scope *fScope, int fVal );
int HasNumericSuffix( const char *fStr, char *root, int size, int *suffix );
Atom GetNumberedAtom( CgContext *cg, const char *root, int number, int digits, char ch );
Stmt *ConcatStmts( Stmt *first, Stmt *last );
void AppendStatements( StmtList *fStatements, Stmt *fStmt );
Expr *GenSymb(  CgContext *cg, Symbol *fSymb );
Expr *GenMember(  CgContext *cg, Symbol *fSymb );
Expr *GenMemberSelector(  CgContext *cg, Expr *sexpr, Expr *mExpr );
Expr *GenMemberReference(  CgContext *cg, Expr *sexpr, Symbol *mSymb );
Expr *GenVecIndex(  CgContext *cg, Expr *vexpr, Expr *xexpr, TypeBase base, int len, int len2 );
Expr *GenMatIndex(  CgContext *cg, Expr *mExpr, Expr *xexpr, TypeBase base, int len, int len2 );
Expr *GenBoolConst(  CgContext *cg, int fval );
Expr *GenIntConst(  CgContext *cg, int fval );
Expr *GenFConstV(  CgContext *cg, float *fval, int len, TypeBase base );
Expr *GenConvertVectorLength(  CgContext *cg, Expr *fExpr, TypeBase base, int len, int newlen );
Expr *DupExpr(  CgContext *cg, Expr *fExpr );


#endif // !defined(__COMPILE_H)
