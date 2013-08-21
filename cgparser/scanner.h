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
// scanner.h
//

#if !defined(__SCANNER_H)
#define __SCANNER_H 1

#define MAX_SYMBOL_NAME_LEN 128
#define MAX_STRING_LEN 512

// Not really atom table stuff but needed first...

struct SourceLoc {
	Atom file;
  unsigned short line;
};

struct CgContext;
int yyparse ( CgContext * cg );

void yyerror( CgContext * cg, const char *s);
int yylex( YYSTYPE * pyylval, CgContext * cg );

struct CgContext;



struct InputSrc {
	InputSrc( CgContext * iCg ) : cg( iCg ), prev( NULL ), scan( NULL ), getch( NULL ), ungetch( NULL ), line( 0 ) {}
	InputSrc( CgContext * iCg, InputSrc * isPrev, int (*isScan)( YYSTYPE *, CgContext *, InputSrc *),
           int (*isGetch)( CgContext *, InputSrc *), void (*isUngetch)( CgContext *, InputSrc *,int), Atom isName, int isLine )
	: cg( iCg ), prev( isPrev ), scan( isScan ), getch( isGetch ), ungetch( isUngetch ), name( isName ), line( isLine )
	{}
	virtual ~InputSrc() {}
	CgContext   *cg;
  InputSrc	*prev;
  int			(*scan)( YYSTYPE *, CgContext *, InputSrc *);
  int			(*getch)( CgContext *, InputSrc *);
  void		(*ungetch)( CgContext *, InputSrc *, int);
  Atom		name;  /* atom */
  int			line;
  
	void * operator new( size_t );
	void operator delete( void * );
};

int SetInputFile( CgContext *cg, const char *fname);
int ScanFromString( CgContext *cg, char *);
Atom scan_include_name( CgContext *cg );

void SemanticParseError( CgContext *cg, SourceLoc *loc, int num, const char *mess, ...);
void SemanticError( CgContext *cg, SourceLoc *loc, int num, const char *mess, ...);
void InternalError( CgContext *cg, SourceLoc *loc, int num, const char *mess, ...);
void SemanticWarning( CgContext *cg, SourceLoc *loc, int num, const char *mess, ...);
void InformationalNotice( CgContext *cg, SourceLoc *loc, int num, const char *mess, ...);
void FatalError( CgContext *cg, const char *mess, ...);

int GetErrorCount( CgContext *cg );

#endif // !(defined(__SCANNER_H)

