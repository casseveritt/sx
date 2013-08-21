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
// memory.cpp
//

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <map>
#include <vector>

#include "slglobals.h"

using namespace std;
using namespace sx;

namespace {
	struct MemInfo {
		MemInfo() : size( 0 ) {}
		MemInfo( const char *miType, int miSize ) : type( miType ), size( miSize ) {}
		const char * type;
		int size;
	};
  
	struct Bin {
		Bin() : count( 0 ), sum( 0 ) {}
		int count;
		int sum;
		vector< void * > ptrs;
	};
  
	map< void *, MemInfo > *allocs;
  
	void Initialize() {
		if ( allocs != NULL ) {
			return;
		}
		allocs = new map< void *, MemInfo >;
	}
  
}

namespace sx {
  
	void * Alloc( size_t bytes, const char *memtype ) {
		static int recursed = 0;
		recursed++;
		Initialize();
		void *ptr = malloc( bytes );
		assert( allocs->count( ptr ) == 0 );
		if ( memtype == NULL ) {
			memtype = "<misc>";
		}
		if ( recursed == 1 ) {
			(*allocs)[ ptr ] = MemInfo( memtype, bytes );
		}
		recursed--;
		return ptr;
	}
  
	void Free( void * ptr ) {
		Initialize();
		if ( ptr == NULL ) {
			return;
		}
		assert( allocs->count( ptr ) > 0 );
		allocs->erase( ptr );
		free( ptr );
	}
  
	void PrintType( CgContext *cg, const char *type, void *ptr ) {
		if ( strcmp( type, "xSymbol" ) == 0 ) {
			Symbol *s = static_cast< Symbol * >( ptr );
			cg->Printf( "    %s\n", s->name.s );
		} else if( strcmp( type, "xSymbolList" ) == 0 ) {
			SymbolList *sl = static_cast< SymbolList * >( ptr );
			cg->Printf( "    %p\n", sl );
		} else if( strcmp( type, "Stmt" ) == 0 ) {
			Stmt *s = static_cast< Stmt * >( ptr );
			cg->Printf( "    %d\n", s->kind );
		}
	}
  
	void PrintAllocationStats() {
		Initialize();
		int sum = 0;
		map< const char *, Bin > bins;
		for( map< void *, MemInfo>::iterator it = allocs->begin(); it != allocs->end(); ++it ) {
			Bin & bin = bins[ it->second.type ];
			bin.count++;
			bin.sum += it->second.size;
			if ( bin.ptrs.size() < 5 ) {
				bin.ptrs.push_back( it->first );
			}
			sum += it->second.size;
		}
		for( map< const char *, Bin>::iterator it = bins.begin(); it != bins.end(); ++it ) {
			const char * type = it->first;
			Bin & bin = it->second;
			printf( "  %s is using %d allocations for %d bytes.\n", type, bin.count, bin.sum );
			for ( int i = 0; i < (int)bin.ptrs.size(); i++ ) {
				//PrintType( cg, type, bin.ptrs[i] );
			}
		}
		printf( "sx is using %d allocations for %d bytes.\n", (int)allocs->size(), sum );
	}
}

// type-specific newdel

#define NEWDEL( type )                          \
void * type :: operator new( size_t sz ) {  \
return Alloc( sz, #type );              \
}                                           \
void type :: operator delete( void *ptr ) { \
Free( ptr );                            \
}                                           \

NEWDEL( Binding )
NEWDEL( BindingList )
NEWDEL( BindingTree )
NEWDEL( CgContext )
//NEWDEL( Decl )
NEWDEL( ErrorLoc )
NEWDEL( Expr )
NEWDEL( Hal )
NEWDEL( InputSrc )
NEWDEL( Scope )
NEWDEL( Stmt )
NEWDEL( Symbol )
NEWDEL( SymbolList )
NEWDEL( TokenBlock )
NEWDEL( TokenStream )
NEWDEL( Type )
NEWDEL( TypeList )
NEWDEL( UniformSemantic )



