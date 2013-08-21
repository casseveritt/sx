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

#include <assert.h>
#include <string.h>
#include <vector>
#include "slglobals.h"

using namespace std;


struct AtomTableSTL : public AtomTable {
	AtomTableSTL( ) {
	}
  
	virtual ~AtomTableSTL() {
		Clear();
	}
  
	virtual void Clear() {
		for ( int i = 0; i < (int)table.size(); i++ ) {
			sx::Free( const_cast< char * >( table[i].s ) );
		}
		table.clear();
	}
  
	Atom Lookup( const char *s ) {
		for ( int i = 0; i < (int) table.size(); i++ ) {
			if ( strcmp( s, table[i].s ) == 0 ) {
				return table[i];
			}
		}
		return Atom();
	}
  
	virtual void AddAtom( int val, const char *str ) {
		char * s = static_cast< char * >( sx::Alloc( strlen( str ) + 1 ) );
		strcpy( s, str );
		table.push_back( Atom( val, s ) );
	}
  
	virtual Atom Add( const char * s) {
		Atom atom = Lookup( s );
		assert( atom.IsValid() == false ); // don't add stuff that's already there
		AddAtom( (int)table.size() + 1024, s );
		return table.back();
	}
  
	virtual void Print() {
	}
  
	virtual Atom LookupAdd( const char *s ) {
		Atom atom = Lookup( s );
		if ( atom.IsValid() == false ) {
			atom = Add( s );
		}
		return atom;
	}
  
	virtual const char * GetAtomString( Atom a ) {
		return a.s;
	}
  
	vector< Atom > table;
};


AtomTable * CreateAtomTable() {
	return new AtomTableSTL();
}


