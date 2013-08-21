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
// generic_hal.c
//

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <iostream>

#include "slglobals.h"
#include "generic_hal.h"

#define NUMELS(x) (sizeof(x) / sizeof((x)[0]))

// These define all the input connector registers for this profile
// you can have multiple names that refer to the same register number
static ConnectorRegisters inputCRegs_generic[] = {
  ConnectorRegisters( "ATTR0",  0, TB_Float, REG_AP2V_ATTR0,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR1",  0, TB_Float, REG_AP2V_ATTR1,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR2",  0, TB_Float, REG_AP2V_ATTR2,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR3",  0, TB_Float, REG_AP2V_ATTR3,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR4",  0, TB_Float, REG_AP2V_ATTR4,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR5",  0, TB_Float, REG_AP2V_ATTR5,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR6",  0, TB_Float, REG_AP2V_ATTR6,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR7",  0, TB_Float, REG_AP2V_ATTR7,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR8",  0, TB_Float, REG_AP2V_ATTR8,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR9",  0, TB_Float, REG_AP2V_ATTR9,  4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR10", 0, TB_Float, REG_AP2V_ATTR10, 4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR11", 0, TB_Float, REG_AP2V_ATTR11, 4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR12", 0, TB_Float, REG_AP2V_ATTR12, 4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR13", 0, TB_Float, REG_AP2V_ATTR13, 4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR14", 0, TB_Float, REG_AP2V_ATTR14, 4, REG_ALLOC | REG_INPUT ),
  ConnectorRegisters( "ATTR15", 0, TB_Float, REG_AP2V_ATTR15, 4, REG_ALLOC | REG_INPUT ),
};

static ConnectorRegisters outputCRegs_generic[] = {
  // These are output register names
  ConnectorRegisters( "HPOS",  0, TB_Float, REG_V2FR_HPOS,  4, REG_RESERVED | REG_OUTPUT | REG_WRITE_REQUIRED ),
  ConnectorRegisters( "COL0",  0, TB_Float, REG_V2FR_COL0,  4, REG_RESERVED | REG_OUTPUT ),
  ConnectorRegisters( "COL1",  0, TB_Float, REG_V2FR_COL1,  4, REG_RESERVED | REG_OUTPUT ),
  ConnectorRegisters( "TEX0",  0, TB_Float, REG_V2FR_TEX0,  4, REG_ALLOC | REG_OUTPUT ),
  ConnectorRegisters( "TEX1",  0, TB_Float, REG_V2FR_TEX1,  4, REG_ALLOC | REG_OUTPUT ),
  ConnectorRegisters( "TEX2",  0, TB_Float, REG_V2FR_TEX2,  4, REG_ALLOC | REG_OUTPUT ),
  ConnectorRegisters( "TEX3",  0, TB_Float, REG_V2FR_TEX3,  4, REG_ALLOC | REG_OUTPUT ),
  ConnectorRegisters( "FOGC",  0, TB_Float, REG_V2FR_FOGC,  1, REG_RESERVED | REG_OUTPUT ),
  ConnectorRegisters( "PSIZ",  0, TB_Float, REG_V2FR_PSIZ,  1, REG_RESERVED | REG_OUTPUT )
};


// Semantics:
enum { AP2V_GROUP = 0, V2FR_GROUP = 1, };

static SemanticsDescriptor Semantics_generic[] = {
  // These are semantics that can be attached to varying variables and
  // parameters.  They usually correspond to the input and output registers
  // defined above, but don't have to.  You can add multiple names for the
  // the same thing as aliases
  // Varying input semantics:
  SemanticsDescriptor( "ATTRIB",   TB_Float, 4, REG_AP2V_ATTR0, 16, AP2V_GROUP, SEM_IN | SEM_VARYING ),
  // Varying output semantics:
  SemanticsDescriptor( "POSITION", TB_Float, 4, REG_V2FR_HPOS, 1, V2FR_GROUP, SEM_OUT | SEM_VARYING ),
  SemanticsDescriptor( "FOG",      TB_Float, 1, REG_V2FR_FOGC, 0, V2FR_GROUP, SEM_OUT | SEM_VARYING ),
  SemanticsDescriptor( "COLOR",    TB_Float, 4, REG_V2FR_COL0, 2, V2FR_GROUP, SEM_OUT | SEM_VARYING ),
  SemanticsDescriptor( "PSIZE",    TB_Float, 1, REG_V2FR_PSIZ, 0, V2FR_GROUP, SEM_OUT | SEM_VARYING ),
  SemanticsDescriptor( "TEXCOORD", TB_Float, 4, REG_V2FR_TEX0, 4, V2FR_GROUP, SEM_OUT | SEM_VARYING )
};


// These are the connector types which refer to the register names above
static ConnectorDescriptor connectors_generic[] = {
  ConnectorDescriptor( CID_GENERIC_IN_NAME,  0, CID_GENERIC_IN_ID,  CONNECTOR_IS_INPUT,  NUMELS(inputCRegs_generic),  inputCRegs_generic ),
  
  ConnectorDescriptor( CID_GENERIC_OUT_NAME, 0, CID_GENERIC_OUT_ID, CONNECTOR_IS_OUTPUT, NUMELS(outputCRegs_generic), outputCRegs_generic )
};


///////////////////////////////////////////////////////////////////////////////
/////////////////////////// Generic output Program ////////////////////////////
///////////////////////////////////////////////////////////////////////////////


struct GenericHal : public Hal {
	GenericHal( CgContext *cg, Atom hEntryName );
  
	// pure methods
	virtual bool RegisterNames();
	virtual int GetConnectorID(Atom);
	virtual Atom GetConnectorAtom(int);
	virtual bool PrintCodeHeader();
	virtual bool GenerateCode(SourceLoc *loc, Scope *fScope, Symbol *program);
  
	// non-pure methods
	virtual int GetConnectorUses( int cid, int pid );
	virtual bool GetConnectorRegister(int cid, int ByIndex, Atom ratom, Binding *fBind);
	virtual bool GetCapsBit( int bitNumber );
  
	virtual bool CheckInternalFunction( Symbol *fSymb, int *group ) { return true; }
  virtual bool BindUniformUnbound( SourceLoc *loc, Symbol *fSymb, Binding *fBind )
	{ fBind->properties |= BIND_IS_BOUND;  return true; }
	virtual bool BindVaryingSemantic(SourceLoc *loc, Symbol *fSymb, Atom semantic, Binding *fBind, int IsOutVal);
  
	// Profile specific data members:
  
};


/*
 * InitHal_generic()
 */

GenericHal::GenericHal( CgContext *cg, Atom hEntryName )
: Hal( cg, hEntryName )
{
	profileName = cg->GetAtom( "generic" );
  vendor = VENDOR_STRING_GENERIC;
  version = VERSION_STRING_GENERIC;
  
  semantics = Semantics_generic;
  numSemantics = NUMELS(Semantics_generic);
  
  incid = CID_GENERIC_IN_ID;
  inputCRegs = inputCRegs_generic;
  numInputCRegs = NUMELS(inputCRegs_generic);
  
  outcid = CID_GENERIC_OUT_ID;
  outputCRegs = outputCRegs_generic;
  numOutputCRegs = NUMELS(outputCRegs_generic);
  
  comment = "//";
  
} // InitHal_generic




/*
 * RegisterNames()
 */

bool GenericHal::RegisterNames()
{
  int i, j;
  
  // Add atoms for connectors and connector registers.
  for (i = 0; i < NUMELS(connectors_generic); i++) {
    ConnectorDescriptor * conn = &connectors_generic[i];
    conn->name = cg->GetAtom(conn->sname);
    for (j = 0; j < conn->numregs; j++)
      conn->registers[j].name = cg->GetAtom(conn->registers[j].sname);
  }
  
  return true;
}


/*
 * GetConnectorID()
 */

int GenericHal::GetConnectorID( Atom name )
{
  int i;
  
	for (i = 0; i < NUMELS(connectors_generic); i++) {
		if ( name == connectors_generic[i].name ) {
      return connectors_generic[i].cid;
		}
	}
  
  return 0;
}


/*
 * GetConnectorAtom()
 */

Atom GenericHal::GetConnectorAtom( int cid )
{
  const ConnectorDescriptor *conn = LookupConnector( connectors_generic, cid, NUMELS (connectors_generic) );
  return conn ? conn->name : Atom();
}

/*
 * GetConnectorUses()
 */

int GenericHal::GetConnectorUses( int cid, int pid )
{
  const ConnectorDescriptor *conn = LookupConnector( connectors_generic, cid, NUMELS(connectors_generic) );
  return conn ? conn->properties : 0;
}

/*
 * GetConnectorRegister()
 */

bool GenericHal::GetConnectorRegister(int cid, int ByIndex, Atom ratom, Binding *fBind)
{
  int i;
  ConnectorDescriptor *conn;
  ConnectorRegisters *regs;
  
  conn = LookupConnector(connectors_generic, cid, NUMELS(connectors_generic));
  if (! conn)
    return false;
  
  regs = conn->registers;
  
  if (! regs)
    return false;
  
  for (i = 0; i < conn->numregs; i++) {
    if (ratom == regs[i].name) {
      SetSymbolConnectorBinding(fBind, &regs[i]);
      return true;
    }
  }
  
  return false;
} // GetConnectorRegister

/*
 * GetCapsBit() - Return an integer value representing the capabilities of this profile.
 */

bool GenericHal::GetCapsBit( int bitNumber )
{
  switch (bitNumber) {
    case CAPS_INDEXED_ARRAYS:
    case CAPS_DONT_FLATTEN_IF_STATEMENTS:
      return true;
    default:
      return false;
  }
} // GetCapsBit


/*
 * BindVaryingSemantic()
 */

bool GenericHal::BindVaryingSemantic(SourceLoc *loc, Symbol *fSymb, Atom semantic, Binding *fBind, int IsOutVal)
{
  int ii, index, len, HasSuffix, base, IsFloating;
  SemanticsDescriptor *semantics;
  char root[128];
  const char *pname, *match;
  Type *lType;
  
  pname = cg->GetString(semantic);
  HasSuffix = HasNumericSuffix(pname, root, 128, &index);
  semantics = cg->theHal->semantics;
  for (ii = 0; ii < cg->theHal->numSemantics; ii++, semantics++) {
    match = semantics->numregs > 0 ? root : pname;
    if (!strcmp(match, semantics->sname)) {
      if (semantics->numregs > 0) {
        if (index >= semantics->numregs) {
          SemanticError( cg, loc, ERROR_S_SEMANTICS_INDEX_TOO_BIG, pname);
          return false;
        }
      } else {
        index = 0;
      }
      
      // Found a match.  See if the type is compatible:
      
      lType = fSymb->type;
      if (IsScalar(lType)) {
        len = 1;
      } else if (IsVector(lType, &len)) {
      } else {
        SemanticError( cg, loc, ERROR_S_SEM_VAR_NOT_SCALAR_VECTOR,
                      cg->GetString(fSymb->name));
        return false;
      }
      base = GetBase(lType);
      IsFloating = (base == TB_Float);
      if (!IsFloating)
        return false;
      
      if (semantics->properties & SEM_VARYING) {
        fBind->kind = BK_CONNECTOR;
        fBind->name = semantic;
        fBind->num = semantics->regno + index;
        fSymb->properties |= SYMB_IS_CONNECTOR_REGISTER;
      } else {
        fBind->kind = BK_SEMANTIC;
        fBind->name = semantic;
        fBind->num = 0;
      }
      fBind->properties |= BIND_IS_BOUND | BIND_VARYING;
      if (semantics->properties & SEM_HIDDEN)
        fBind->properties |= BIND_HIDDEN;
      fSymb->properties |= SYMB_CONNECTOR_CAN_READ; // Obsolete
      fSymb->properties |= SYMB_CONNECTOR_CAN_WRITE; // Obsolete
      if (semantics->properties & SEM_IN)
        fBind->properties |= BIND_INPUT;
      if (semantics->properties & SEM_OUT)
        fBind->properties |= BIND_OUTPUT;
      if (semantics->properties & SEM_REQUIRED)
        fBind->properties |= BIND_WRITE_REQUIRED;
      // fBind->none,gname set elsewhere
      // fBind->lname set elsewhere
      fBind->base = semantics->base;
      fBind->size = semantics->size;
      return true;
    }
  }
  return false;
} // BindVaryingSemantic

/*
 * PrintCodeHeader()
 */

bool GenericHal::PrintCodeHeader()
{
  cg->OutputPrintf( "%s Generic output by " SX_TAG "\n", comment);
  return true;
} // PrintCodeHeader


static void PrintFunctions( Writer & wr, Writer & bwr, Symbol *symb )
{
  Symbol *fSymb;
  if (symb) {
    PrintFunctions( wr, bwr, symb->left);
    if (IsFunction(symb)) {
      fSymb = symb;
      while (fSymb) {
        wr.WriteFunction( fSymb);
        bwr.WriteFunction( fSymb);
        fSymb = fSymb->details.fun.overload;
      }
    }
    PrintFunctions( wr, bwr, symb->right);
  }
}

/*
 * GenerateCode() - Generates human-readable generic output form of source code.
 */

bool GenericHal::GenerateCode( SourceLoc *loc, Scope *fScope, Symbol *program )
{
	Writer wr( cg, std::cout );
	BWriter bwr( cg, std::cout );
  PrintFunctions( wr, bwr, fScope->symbols);
  return true;
} // GenerateCode


/*
 * CreateGenericHal() - the generic profile constructor
 */

Hal * CreateGenericHal( CgContext *cg, Atom entryName ) {
	return new GenericHal( cg, entryName );
}



///////////////////////////////////////////////////////////////////////////////
//////////////////////// End of generic_hal.c /////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
