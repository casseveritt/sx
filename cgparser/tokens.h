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
// tokens.h
//

#if !defined(__TOKENS_H)
#define __TOKENS_H 1

#define EOF_SY (-1)

#include <stdio.h>
#include "memory.h"
using namespace sx;

struct TokenBlock;

struct TokenStream {
	TokenStream() : next( NULL ), name( NULL ), head( NULL ), current( NULL ), allocated( true ) {}
	TokenStream( TokenStream * tsNext, const char * tsName, TokenBlock *tbHead, TokenBlock *tbCurrent )
	: next( tsNext ), name( tsName ), head( tbHead ), current( tbCurrent ), allocated( false )
	{}
	~TokenStream();
  
	TokenStream *next;
  const char *name;
  TokenBlock *head;
  TokenBlock *current;
	bool allocated;
  
	void * operator new( size_t );
	void operator delete( void * );
};

struct TokenBlock {
	TokenBlock() : next( NULL ), current( 0 ), count( 0 ), max( 256 ), allocated( true ) {
		data = static_cast< unsigned char * >( Alloc( max ) );
	}
	TokenBlock( TokenBlock *tbNext, int tbCurrent, int tbCount, int tbMax, unsigned char *tbData )
	: next( tbNext ), current( tbCurrent ), count( tbCount ), max( tbMax ), data( tbData ), allocated( false )
	{}
	~TokenBlock() {
		delete next;
		if ( allocated ) {
			Free( data );
		}
	}
  TokenBlock *next;
  int current;
  int count;
  int max;
  unsigned char *data;
	const bool allocated;
  
	void * operator new( size_t );
	void operator delete( void * );
};

extern TokenStream stdlib_cg_stream;

int InitTokenStreams(CgContext *Cg);

TokenStream *NewTokenStream(const char *name);
void DeleteTokenStream(TokenStream *pTok);
void RecordToken( YYSTYPE &yylval, CgContext *cg, TokenStream *pTok, int token);
void RewindTokenStream(TokenStream *pTok);
int ReadToken( YYSTYPE &yylval, CgContext *cg, TokenStream *pTok);
int ReadFromTokenStream( CgContext *cg, TokenStream *pTok, Atom name, bool startGlobalScope );
void UngetToken( YYSTYPE & yylval, CgContext *cg, int);

#if defined(CGC_ENABLE_TOOLS)

void TokenizeInput( YYSTYPE & yylval, CgContext *cg );
void DumpTokenStream( YYSTYPE & yylval, CgContext *cg, FILE *, TokenStream *);

#endif // defined(CGC_ENABLE_TOOLS)

#endif // !defined(__TOKENS_H)
