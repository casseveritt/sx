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
// binding.h
//

#if !defined(__BINDING_H)
#define __BINDING_H 1

#include "type.h"

enum BindingKind {
  BK_NONE, BK_CONNECTOR, BK_TEXUNIT, BK_REGARRAY, BK_CONSTANT, BK_DEFAULT, BK_SEMANTIC,
};

// Properties bits:

#define BIND_IS_BOUND           0x0001
#define BIND_HIDDEN             0x0002
#define BIND_UNIFORM            0x0004
#define BIND_VARYING            0x0008
#define BIND_INPUT              0x0010
#define BIND_OUTPUT             0x0020
#define BIND_WRITE_REQUIRED     0x0040
#define BIND_WAS_WRITTEN        0x0080

struct Binding {
	Binding( BindingKind bKind, Atom gName, Atom lName )
  : kind( bKind )
  , properties(0)
  , gname( gName ), lname( lName )
  , base( TB_NoType ), size(0)
  , num(0), count(0)
	{
		val[0] = val[1] = val[2] = val[3] = 0;
	}
	BindingKind kind;
	int properties;     // Properties
	Atom gname;          // Global name
	Atom lname;          // Local name
	TypeBase base;           // type base
	int size;           // num of elements
  
	Atom name;
	int num;
	int count;
	float val[4];       // Values
  
	void * operator new( size_t );
	void operator delete( void * );
};

/*
 union Binding_Rec {
 BindingNone none;
 BindingConnector conn;
 BindingRegArray reg;
 BindingTexunit texunit;
 BindingConstDefault constdef;
 BindingSemantic sem;
 };
 */

struct BindingList {
  BindingList *next;
  Binding *binding;
  
	void * operator new( size_t );
	void operator delete( void * );
};

struct BindingTree {
	BindingTree( BindingKind btKind, Atom gName, Atom lName)
  : binding( btKind, gName, lName )
  , nextc( NULL ), nextm( NULL )
	{}
  
	Binding binding;    // Binding info.
  BindingTree *nextc; // Next connector name in a list
  BindingTree *nextm; // Next member in this connector
  SourceLoc loc;      // Source location of #pragma bind
  
	void * operator new( size_t );
	void operator delete( void * );
};

struct UniformSemantic {
	UniformSemantic( Atom gName, Atom vName, Atom semanticName ) : next( NULL ), gname( gName ), vname( vName ), semantic( semanticName ) {}
	~UniformSemantic() {
		delete next;
	}
  UniformSemantic *next;
  Atom gname;
  Atom vname;
  Atom semantic;
  
	void * operator new( size_t );
	void operator delete( void * );
};

BindingTree *LookupBinding( CgContext *cg, Atom gname, Atom lname);

void DefineConnectorBinding( CgContext *cg, SourceLoc *loc, Atom cname, Atom mname, Atom rname);
void DefineRegArrayBinding( CgContext *cg, SourceLoc *loc, Atom pname, Atom aname, Atom rname, int regno,
                           int count);
void DefineTexunitBinding( CgContext *cg, SourceLoc *loc, Atom pname, Atom aname, int unitno);
void DefineConstantBinding( CgContext *cg, SourceLoc *loc, Atom pname, Atom aname, int count, float *fval);
void DefineDefaultBinding( CgContext *cg, SourceLoc *loc, Atom pname, Atom aname, int count, float *fval);

#endif // !defined(__BINDING_H)

