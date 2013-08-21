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
// printutils.h
//

#if !defined(__PRINTUTILS_H)
#define __PRINTUTILS_H 1

#include <ostream>
#include <vector>
#include <string>

namespace sx {
  
	std::vector< std::string > FormatTypeStringRT( CgContext *cg, Type *fType, bool unqualified );
  
	void FormatTypeString( CgContext *cg, char *name, int size, char *name2, int size2, Type *fType);
	void FormatTypeStringRT( CgContext *cg, char *name, int size, char *name2, int size2, Type *fType, int Unqualified);
  
	struct Writer {
		Writer( CgContext * wCg, std::ostream & wO ) : cg( wCg ), o( wO ) {}
		virtual ~Writer() {}
    
		virtual void WriteType( Type * type );
		virtual void WriteSymbolTree( Symbol * sym );
		virtual void WriteScopeDeclarations();
    
		virtual void WriteExpr( Expr *expr );
		virtual void WriteStmt( Stmt *stmt, const char *comment = NULL );
		virtual void WriteStmtList( Stmt *stmt, const char *comment = NULL );
		virtual void WriteFunction( Symbol *sym );
    
    virtual void WriteSymbol( Symbol * sym );
    
		virtual void Printf( const char *, ... );
		virtual void Indent();
		virtual void Unindent();
    
		CgContext * cg;
		std::ostream & o;
		std::string indent;
		bool indentNext;
	};
  
	struct BWriter : public Writer {
		BWriter( CgContext * wCg, std::ostream & wO ) : Writer( wCg, wO ) {}
		
		virtual void WriteExpr( Expr *expr ) { WriteExpr( expr, 0 ); }
		virtual void WriteStmt( Stmt *stmt, const char *comment = NULL );
		virtual void WriteStmtList( Stmt *stmt, const char *comment = NULL );
		virtual void WriteFunction( Symbol *sym );
		
		void WriteExpr( Expr *expr, int level );
	};
	
}

#endif // !(defined(__PRINTUTILS_H)

