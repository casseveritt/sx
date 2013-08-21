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
// cpp.c
//

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "slglobals.h"

using namespace sx;

static Atom bindAtom = 0;
static Atom constAtom = 0;
static Atom defaultAtom = 0;
static Atom defineAtom = 0;
static Atom definedAtom = 0;
static Atom elseAtom = 0;
static Atom elifAtom = 0;
static Atom endifAtom = 0;
static Atom ifAtom = 0;
static Atom ifdefAtom = 0;
static Atom ifndefAtom = 0;
static Atom includeAtom = 0;
static Atom lineAtom = 0;
static Atom pragmaAtom = 0;
static Atom texunitAtom = 0;
static Atom undefAtom = 0;
static Atom __LINE__Atom = 0;
static Atom __FILE__Atom = 0;


static Scope *macros = 0;
#define MAX_MACRO_ARGS  64

static int ifdepth = 0; /* depth of #if nesting -- used to detect invalid
                         * #else/#endif */
static SourceLoc ifloc; /* outermost #if */

int InitCPP( CgContext *cg, const char * profileString )
{
  char        buffer[64], *t;
  const char  *f;
  // Add various atoms needed by the CPP line scanner:
  bindAtom = cg->GetAtom("bind");
  constAtom = cg->GetAtom("const");
  defaultAtom = cg->GetAtom("default");
  defineAtom = cg->GetAtom("define");
  definedAtom = cg->GetAtom("defined");
  elifAtom = cg->GetAtom("elif");
  elseAtom = cg->GetAtom("else");
  endifAtom = cg->GetAtom("endif");
  ifAtom = cg->GetAtom("if");
  ifdefAtom = cg->GetAtom("ifdef");
  ifndefAtom = cg->GetAtom("ifndef");
  includeAtom = cg->GetAtom("include");
  lineAtom = cg->GetAtom("line");
  pragmaAtom = cg->GetAtom("pragma");
  texunitAtom = cg->GetAtom("texunit");
  undefAtom = cg->GetAtom("undef");
  __LINE__Atom = cg->GetAtom("__LINE__");
  __FILE__Atom = cg->GetAtom("__FILE__");
  macros = new Scope( cg );
  strcpy(buffer, "PROFILE_");
  t = buffer + strlen(buffer);
  f = profileString;
  while ((isalnum(*f) || *f == '_') && t < buffer + sizeof(buffer) - 1)
    *t++ = toupper(*f++);
  *t = 0;
  PredefineMacro( cg, buffer);
  return 1;
} // InitCPP

int FinalCPP( CgContext *cg )
{
  if (ifdepth)
    SemanticWarning( cg, &ifloc, WARNING___CPP_IF_MISMATCH, "if");
  return 1;
}

static int CPPdefine( YYSTYPE & yylval, CgContext * cg )
{
  int token;
	Atom args[MAX_MACRO_ARGS];
	int argc;
	Atom name;
  MacroSymbol mac;
  Symbol *symb;
  SourceLoc dummyLoc;
  
  memset(&mac, 0, sizeof(mac));
  token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
  if (token != IDENT_SY) {
    SemanticError( cg, cg->tokenLoc, ERROR___CPP_SYNTAX, "define");
    return token;
  }
  name = yylval.sc_ident;
  token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
  if (token == '(' && !yylval.sc_int) {
    // gather arguments
    argc = 0;
    do {
      token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
      if (argc == 0 && token == ')') break;
      if (token != IDENT_SY) {
        SemanticError( cg, cg->tokenLoc, ERROR___CPP_SYNTAX, "define");
        return token;
      }
      if (argc < MAX_MACRO_ARGS)
        args[argc++] = yylval.sc_ident;
      token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
    } while (token == ',');
    if (token != ')') {
      SemanticError( cg, cg->tokenLoc, ERROR___CPP_SYNTAX, "define");
      return token;
    }
    mac.argc = argc;
    mac.args = new int[ argc ]; //(int *)mem_Alloc(macros->pool, argc * sizeof(int));
    memcpy(mac.args, args, argc * sizeof(int));
    token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
  }
  mac.body = NewTokenStream(cg->GetString(name));
  while (token != '\n') {
    while (token == '\\') {
      token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
      if (token == '\n')
        token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
      else
        RecordToken( yylval, cg, mac.body, '\\');
    }
    RecordToken( yylval, cg, mac.body, token);
    token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
  };
  
  symb = LookupSymbol( cg, macros, name);
  if (symb) {
    if (!symb->details.mac.undef) {
      // already defined -- need to make sure they are identical
      if (symb->details.mac.argc != mac.argc) goto error;
      for (argc=0; argc < mac.argc; argc++)
        if (symb->details.mac.args[argc] != mac.args[argc])
          goto error;
      RewindTokenStream(symb->details.mac.body);
      RewindTokenStream(mac.body);
      do {
        int old_lval, old_token;
        old_token = ReadToken( yylval, cg, symb->details.mac.body);
        old_lval = yylval.sc_int;
        token = ReadToken( yylval, cg, mac.body);
        if (token != old_token || yylval.sc_int != old_lval) {
        error:
          SemanticWarning( cg, cg->tokenLoc, WARNING___CPP_MACRO_REDEFINED,
                          cg->GetString(name));
          break; }
      } while (token > 0);
    }
    FreeMacro( cg, &symb->details.mac);
  } else {
    dummyLoc.file = 0;
    dummyLoc.line = 0;
    symb = AddSymbol( cg, &dummyLoc, macros, name, 0, SK_Macro);
  }
  symb->details.mac = mac;
  return '\n';
} // CPPdefine

static int CPPundef( YYSTYPE & yylval, CgContext * cg )
{
  int token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
  Symbol *symb;
  
  if (token != IDENT_SY) goto error;
  symb = LookupSymbol( cg, macros, yylval.sc_ident);
  if (symb) {
    symb->details.mac.undef = 1;
  }
  token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
  if (token != '\n') {
  error:
    SemanticError( cg, cg->tokenLoc, ERROR___CPP_SYNTAX, "undef");
  }
  return token;
} // CPPundef

static int CPPif( YYSTYPE & yylval, CgContext * cg );

/* CPPelse -- skip forward to appropriate spot.  This is actually used
 ** to skip to and #endif after seeing an #else, AND to skip to a #else,
 ** #elif, or #endif after a #if/#ifdef/#ifndef/#elif test was false
 */

static int CPPelse( YYSTYPE & yylval, CgContext * cg, int matchelse)
{
  Atom atom;
	int depth = 0;
  int token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
  while (token > 0) {
    while (token != '\n')
      token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
    if ((token = cg->currentInput->scan( &yylval, cg, cg->currentInput)) != '#')
      continue;
    if ((token = cg->currentInput->scan( &yylval, cg, cg->currentInput)) != IDENT_SY)
      continue;
    atom = yylval.sc_ident;
    if (atom == ifAtom || atom == ifdefAtom || atom == ifndefAtom)
      depth++;
    else if (atom == endifAtom) {
      if (--depth < 0) {
        if (ifdepth) ifdepth--;
        break;
      }
    }
    else if (matchelse && depth == 0) {
      if (atom == elseAtom)
        break;
      else if (atom == elifAtom) {
        /* we decrement ifdepth here, because CPPif will increment
         * it and we really want to leave it alone */
        if (ifdepth) ifdepth--;
        return CPPif( yylval, cg );
      }
    }
  };
  return token;
}

enum eval_prec {
  MIN_PREC,
  COND, LOGOR, LOGAND, OR, XOR, AND, EQUAL, RELATION, SHIFT, ADD, MUL, UNARY,
  MAX_PREC
};

static int op_logor(int a, int b) { return a || b; }
static int op_logand(int a, int b) { return a && b; }
static int op_or(int a, int b) { return a | b; }
static int op_xor(int a, int b) { return a ^ b; }
static int op_and(int a, int b) { return a & b; }
static int op_eq(int a, int b) { return a == b; }
static int op_ne(int a, int b) { return a != b; }
static int op_ge(int a, int b) { return a >= b; }
static int op_le(int a, int b) { return a <= b; }
static int op_gt(int a, int b) { return a > b; }
static int op_lt(int a, int b) { return a < b; }
static int op_shl(int a, int b) { return a << b; }
static int op_shr(int a, int b) { return a >> b; }
static int op_add(int a, int b) { return a + b; }
static int op_sub(int a, int b) { return a - b; }
static int op_mul(int a, int b) { return a * b; }
static int op_div(int a, int b) { return a / b; }
static int op_mod(int a, int b) { return a % b; }
static int op_pos(int a) { return a; }
static int op_neg(int a) { return -a; }
static int op_cmpl(int a) { return ~a; }
static int op_not(int a) { return !a; }

struct BinaryOperation {
  int token, prec, (*op)(int, int);
} binop[] = {
  { OR_SY, LOGOR, op_logor },
  { AND_SY, LOGAND, op_logand },
  { '|', OR, op_or },
  { '^', XOR, op_xor },
  { '&', AND, op_and },
  { EQ_SY, EQUAL, op_eq },
  { NE_SY, EQUAL, op_ne },
  { '>', RELATION, op_gt },
  { GE_SY, RELATION, op_ge },
  { '<', RELATION, op_lt },
  { LE_SY, RELATION, op_le },
  { LL_SY, SHIFT, op_shl },
  { GG_SY, SHIFT, op_shr },
  { '+', ADD, op_add },
  { '-', ADD, op_sub },
  { '*', MUL, op_mul },
  { '/', MUL, op_div },
  { '%', MUL, op_mod },
};

struct UnaryOperation {
  int token, (*op)(int);
} unop[] = {
  { '+', op_pos },
  { '-', op_neg },
  { '~', op_cmpl },
  { '!', op_not },
};

#define ALEN(A) (sizeof(A)/sizeof(A[0]))

int eval( YYSTYPE & yylval, CgContext * cg, int token, int prec, int *res, int *err)
{
  int         i, val;
  Symbol      *s;
  
  if (token == IDENT_SY) {
    if (yylval.sc_ident == definedAtom) {
      int needclose = 0;
      
      token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
      if (token == '(') {
        needclose = 1;
        token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
      }
      if (token != IDENT_SY)
        goto error;
      *res = (s = LookupSymbol( cg, macros, yylval.sc_ident))
      ? !s->details.mac.undef : 0;
      token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
      if (needclose) {
        if (token != ')')
          goto error;
        token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
      }
    } else if (MacroExpand( yylval, cg, yylval.sc_ident )) {
      token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
      return eval( yylval, cg, token, prec, res, err);
    } else {
      goto error;
    }
  } else if (token == INTCONST_SY) {
    *res = yylval.sc_int;
    token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
  } else if (token == '(') {
    token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
    token = eval( yylval, cg, token, MIN_PREC, res, err);
    if (!*err) {
      if (token != ')')
        goto error;
      token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
    }
  } else {
    for (i = ALEN(unop) - 1; i >= 0; i--) {
      if (unop[i].token == token)
        break;
    }
    if (i >= 0) {
      token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
      token = eval( yylval, cg, token, UNARY, res, err);
      *res = unop[i].op(*res);
    } else {
      goto error;
    }
  }
  while (!*err) {
    if (token == ')' || token == '\n') break;
    for (i = ALEN(binop) - 1; i >= 0; i--) {
      if (binop[i].token == token)
        break;
    }
    if (i < 0 || binop[i].prec <= prec)
      break;
    val = *res;
    token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
    token = eval(  yylval, cg, token, binop[i].prec, res, err);
    *res = binop[i].op(val, *res);
  }
  return token;
error:
  SemanticError( cg, cg->tokenLoc, ERROR___CPP_SYNTAX, "if");
  *err = 1;
  *res = 0;
  return token;
} // eval

static int CPPif( YYSTYPE & yylval, CgContext * cg ) {
  int token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
  int res = 0, err = 0;
  
  if (!ifdepth++)
    ifloc = *cg->tokenLoc;
  token = eval(  yylval, cg, token, MIN_PREC, &res, &err);
  if (token != '\n') {
    SemanticError( cg, cg->tokenLoc, ERROR___CPP_SYNTAX, "if");
  } else if (!res && !err) {
    token = CPPelse( yylval, cg, 1);
  }
  return token;
} // CPPif

static int CPPifdef( YYSTYPE & yylval, CgContext * cg, int defined)
{
  int token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
  Atom name = yylval.sc_ident;
  ifdepth++;
  if (token != IDENT_SY) {
    SemanticError( cg, cg->tokenLoc, ERROR___CPP_SYNTAX,
                  defined ? "ifdef" : "ifndef");
  } else {
    Symbol *s = LookupSymbol( cg, macros, name);
    if (((s && !s->details.mac.undef) ? 1 : 0) != defined)
      token = CPPelse( yylval, cg, 1);
  }
  return token;
} // CPPifdef

static int CPPinclude( YYSTYPE & yylval, CgContext * cg )
{
  int tok = cg->currentInput->scan( &yylval, cg, cg->currentInput);
  Atom file;
  if (tok == STRCONST_SY) {
    file = yylval.sc_ident;
  } else if (tok == '<') {
    file = scan_include_name( cg );
  } else {
    SemanticError( cg, cg->tokenLoc, ERROR___CPP_SYNTAX, "include");
  }
  while (tok != '\n')
    tok = cg->currentInput->scan( &yylval, cg, cg->currentInput);
  SetInputFile( cg, cg->GetString(file));
  return '\n';
} // CPPinclude

static int CPPline( YYSTYPE & yylval, CgContext * cg, int token) {
  if (token == INTCONST_SY) {
    int line = yylval.sc_int;
    token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
    if (token == STRCONST_SY) {
      cg->currentInput->name = yylval.sc_ident;
      cg->currentInput->line = line - 1; // Will get bumped by one.
      token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
    }
  }
  return token;
}

static int CPPpragma( YYSTYPE & yylval, CgContext * cg )
{
  Atom identa, identb, identc;
  int ival, HasIval, NegSign, numfvals;
  float fval[4];
  int token;
  int err;
#define NEXTTOKEN       \
/* get the next token, while expanding macros */        \
do {                                                    \
token = cg->currentInput->scan( &yylval, cg, cg->currentInput);         \
} while (token == IDENT_SY && MacroExpand( yylval, cg, yylval.sc_ident))
  
  NEXTTOKEN;
  if (token == IDENT_SY && yylval.sc_ident == bindAtom) {
    
    // Parse:  # "pragma" "bind" <conn-id> "." <memb-id> "=" <reg-id>
    //         # "pragma" "bind" <prog-id> "." <parm-id> "=" <reg-id> <i-const>
    //         # "pragma" "bind" <prog-id> "." <parm-id> "=" "texunit" <i-const>
    //         # "pragma" "bind" <prog-id> "." <memb-id> "=" "const" <number>+
    //         # "pragma" "bind" <prog-id> "." <memb-id> "=" "default" <number>+
    //
    //  <number> ::= [ "-" ] ( <i-const> | <f-const> )
    
    err = 0;
    NEXTTOKEN;
    if (token == IDENT_SY) {
      identa = yylval.sc_ident;
      NEXTTOKEN;
      if (token == '.') {
        NEXTTOKEN;
      } else {
        err = 1;
      }
      if (token == IDENT_SY) {
        identb = yylval.sc_ident;
        NEXTTOKEN;
        if (token == '=') {
          NEXTTOKEN;
        } else {
          err = 1;
        }
        if (token == IDENT_SY) {
          identc = yylval.sc_ident;
          NEXTTOKEN;
        } else {
          err = 1;
        }
        numfvals = 0;
        HasIval = 0;
        while (token == INTCONST_SY ||
               token == CFLOATCONST_SY ||
               token == '-'
               ) {
          if (token == '-') {
            NegSign = 1;
            NEXTTOKEN;
          } else {
            NegSign = 0;
          }
          if (token == INTCONST_SY) {
            if (numfvals == 0 && !NegSign) {
              ival = yylval.sc_int;
              HasIval = 1;
            }
            if (NegSign)
              yylval.sc_int = -yylval.sc_int;
            if (numfvals < 4) {
              fval[numfvals] = (float) yylval.sc_int;
              numfvals++;
            } else {
              err = 1;
              break;
            }
            NEXTTOKEN;
          } else if (token == CFLOATCONST_SY) {
            if (NegSign)
              yylval.sc_fval = -yylval.sc_fval;
            if (numfvals < 4) {
              fval[numfvals] = yylval.sc_fval;
              numfvals++;
            } else {
              err = 1;
              break;
            }
            NEXTTOKEN;
          } else {
            err = 1;
            break;
          }
        }
        if (!err) {
          if (identc == texunitAtom) {
            if (HasIval && numfvals == 1) {
              DefineTexunitBinding( cg, cg->tokenLoc, identa, identb, ival);
            } else {
              err = 1;
            }
          } else if (identc == constAtom) {
            if (numfvals > 0) {
              DefineConstantBinding( cg, cg->tokenLoc, identa, identb, numfvals, fval);
            } else {
              err = 1;
            }
          } else if (identc == defaultAtom) {
            if (numfvals > 0) {
              DefineDefaultBinding( cg, cg->tokenLoc, identa, identb, numfvals, fval);
            } else {
              err = 1;
            }
          } else if (HasIval) {
            DefineRegArrayBinding( cg, cg->tokenLoc, identa, identb, identc, ival, 0);
          } else {
            DefineConnectorBinding( cg, cg->tokenLoc, identa, identb, identc);
          }
        }
      } else {
        err = 1;
      }
    } else {
      err = 1;
    }
    if (err == 1) {
      SemanticError( cg, cg->tokenLoc, ERROR___CPP_BIND_PRAGMA_ERROR);
    }
  }
  return token;
} // CPPpragma

void readCPPline( YYSTYPE & yylval, CgContext * cg )
{
  int token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
  if (token == IDENT_SY) {
    if (yylval.sc_ident == defineAtom) {
      token = CPPdefine( yylval, cg );
    } else if (yylval.sc_ident == elseAtom) {
      if (!ifdepth)
        SemanticWarning( cg, cg->tokenLoc, WARNING___CPP_IF_MISMATCH, "else");
      token = CPPelse( yylval, cg, 0);
    } else if (yylval.sc_ident == elifAtom) {
      if (!ifdepth)
        SemanticWarning( cg, cg->tokenLoc, WARNING___CPP_IF_MISMATCH, "elif");
      token = CPPelse( yylval, cg, 0);
    } else if (yylval.sc_ident == endifAtom) {
      if (!ifdepth)
        SemanticWarning( cg, cg->tokenLoc, WARNING___CPP_IF_MISMATCH, "endif");
      else
        ifdepth--;
    } else if (yylval.sc_ident == ifAtom) {
      token = CPPif( yylval, cg );
    } else if (yylval.sc_ident == ifdefAtom) {
      token = CPPifdef( yylval, cg, 1);
    } else if (yylval.sc_ident == ifndefAtom) {
      token = CPPifdef( yylval, cg, 0);
    } else if (yylval.sc_ident == includeAtom) {
      token = CPPinclude( yylval, cg );
    } else if (yylval.sc_ident == lineAtom) {
      token = CPPline( yylval, cg, cg->currentInput->scan( &yylval, cg, cg->currentInput));
    } else if (yylval.sc_ident == pragmaAtom) {
      token = CPPpragma( yylval, cg );
    } else if (yylval.sc_ident == undefAtom) {
      token = CPPundef( yylval, cg );
    } else {
      SemanticError( cg, cg->tokenLoc, ERROR___CPP_UNKNOWN_DIRECTIVE,
                    cg->GetString(yylval.sc_ident));
    }
  } else if (token == INTCONST_SY) {
    token = CPPline( yylval, cg, token);
  }
  while (token != '\n' && token != 0 /* EOF */) {
    token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
  }
} // readCPPline

void FreeMacro(CgContext *cg, MacroSymbol *s) {
  DeleteTokenStream(s->body);
}

static int eof_scan( YYSTYPE *pyylval, CgContext *cg, InputSrc *in) { return -1; }
static int eof_getch( CgContext *cg, InputSrc *in) { return -1; }
static void noop( CgContext *cg, InputSrc *in, int ch) { }

static void PushEofSrc( CgContext * cg ) {
  InputSrc *in = new InputSrc(  cg );
  in->scan = eof_scan;
  in->getch = eof_getch;
  in->ungetch = noop;
  in->prev = cg->currentInput;
  cg->currentInput = in;
}

static void PopEofSrc( CgContext * cg ) {
  if (cg->currentInput->scan == eof_scan) {
    InputSrc *in = cg->currentInput;
    cg->currentInput = in->prev;
		delete in;
  }
}

static TokenStream *PrescanMacroArg( YYSTYPE & yylval, CgContext * cg, TokenStream *a) {
  int token;
  TokenStream *n;
  RewindTokenStream(a);
  do {
    token = ReadToken( yylval, cg, a);
    if (token == IDENT_SY && LookupSymbol( cg, macros, yylval.sc_ident))
      break;
  } while (token > 0);
  if (token <= 0) return a;
  n = NewTokenStream("macro arg");
  PushEofSrc( cg );
  ReadFromTokenStream( cg, a, 0, 0);
  while ((token = cg->currentInput->scan( &yylval, cg, cg->currentInput)) > 0) {
    if (token == IDENT_SY && MacroExpand( yylval, cg, yylval.sc_ident))
      continue;
    RecordToken( yylval, cg, n, token);
  }
  PopEofSrc( cg );
  DeleteTokenStream(a);
  return n;
} // PrescanMacroArg

struct MacroInputSrc : public InputSrc {
	MacroInputSrc( CgContext *cg ) : InputSrc( cg ), mac( NULL ), args( NULL ) {}
  MacroSymbol *mac;
  TokenStream **args;
};

/* macro_scan ---
 ** return the next token for a macro expanion, handling macro args
 */
static int macro_scan( YYSTYPE * pyylval, CgContext * cg, MacroInputSrc *in) {
	YYSTYPE &yylval = *pyylval;
  int i;
  int token = ReadToken( yylval, cg, in->mac->body);
  if (token == IDENT_SY) {
    for (i = in->mac->argc-1; i>=0; i--)
      if (in->mac->args[i] == yylval.sc_ident) break;
    if (i >= 0) {
      ReadFromTokenStream( cg, in->args[i], yylval.sc_ident, 0);
      return cg->currentInput->scan( &yylval, cg, cg->currentInput);
    }
  }
  if (token > 0) return token;
  in->mac->busy = 0;
  cg->currentInput = in->prev;
  if (in->args) {
    for (i=in->mac->argc-1; i>=0; i--)
      DeleteTokenStream(in->args[i]);
		delete [] in->args;
  }
	delete in;
  return cg->currentInput->scan( &yylval, cg, cg->currentInput);
} // macro_scan

/* MacroExpand
 ** check an identifier (atom) to see if it a macro that should be expanded.
 ** If it is, push an InputSrc that will produce the appropriate expandsion
 ** and return TRUE.  If not, return FALSE.
 */

int MacroExpand( YYSTYPE & yylval, CgContext * cg, Atom atom)
{
  Symbol              *sym = LookupSymbol( cg, macros, atom);
  MacroInputSrc       *in;
  SourceLoc           loc = *cg->tokenLoc;
  int                 i, token, depth;
  
  if (atom == __LINE__Atom) {
    yylval.sc_int = cg->currentInput->line;
    UngetToken( yylval, cg, INTCONST_SY);
    return 1;
  }
  if (atom == __FILE__Atom) {
    yylval.sc_ident = cg->currentInput->name;
    UngetToken( yylval, cg, STRCONST_SY);
    return 1;
  }
  if (!sym || sym->details.mac.undef) return 0;
  if (sym->details.mac.busy) return 0;        // no recursive expansions
  in = new MacroInputSrc( cg );
  in->scan = ( int(*)( YYSTYPE *, CgContext *, InputSrc *) ) macro_scan;
  in->line = cg->currentInput->line;
  in->name = cg->currentInput->name;
  in->mac = &sym->details.mac;
  if (sym->details.mac.args) {
    token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
    if (token != '(') {
      UngetToken( yylval, cg, token);
      yylval.sc_ident = atom;
      return 0;
    }
    in->args = static_cast< TokenStream ** >( Alloc( in->mac->argc * sizeof( TokenStream * ) ) ) ;// new TokenStream *[ in->mac->argc ];
    for (i=0; i<in->mac->argc; i++)
      in->args[i] = NewTokenStream("macro arg");
    for (i=0; i<in->mac->argc; i++) {
      depth = 0;
      while(1) {
        token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
        if (token <= 0) {
          SemanticError( cg, &loc, ERROR___CPP_MACRO_EOF,
                        cg->GetString(atom));
          return 1;
        }
        if (depth == 0 && (token == ',' || token == ')')) break;
        if (token == '(') depth++;
        if (token == ')') depth--;
        RecordToken( yylval, cg, in->args[i], token);
      }
      if (token == ')') {
        i++;
        break;
      }
    }
    if (i < in->mac->argc) {
      SemanticError( cg, &loc, ERROR___CPP_MACRO_TOOFEW,
                    cg->GetString(atom));
    } else if (token != ')') {
      while (token >= 0 && (depth > 0 || token != ')')) {
        if (token == ')') depth--;
        token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
        if (token == '(') depth++;
      }
      if (token <= 0) {
        SemanticError( cg, &loc, ERROR___CPP_MACRO_EOF,
                      cg->GetString(atom));
        return 1;
      }
      SemanticError( cg, &loc, ERROR___CPP_MACRO_TOOMANY,
                    cg->GetString(atom));
    }
    for (i=0; i<in->mac->argc; i++) {
      in->args[i] = PrescanMacroArg( yylval, cg, in->args[i]);
    }
  }
#if 0
  cg->Printf("  <%s:%d>found macro %s\n", cg->GetString(loc.file),
             loc.line, cg->GetString(atom));
  for (i=0; i<in->mac->argc; i++) {
    cg->Printf("\targ %s = '", cg->GetString(in->mac->args[i]));
    DumpTokenStream(stdout, in->args[i]);
    cg->Printf("'\n");
  }
#endif
  in->prev = cg->currentInput;
  sym->details.mac.busy = 1;
  RewindTokenStream(sym->details.mac.body);
  cg->currentInput = in;
  return 1;
} // MacroExpand

/* PredefineMacro
 ** define a macro (no args) based on the input string, as from a -D
 ** argument.  The string is either "NAME=expansion" or just "NAME", which
 ** is equivalent to "NAME=1".  Return TRUE if everything is ok, or
 ** FALSE if the string is malformed.
 */
int PredefineMacro( CgContext * cg, char *def) {
  char *name = def;
  MacroSymbol mac;
  Symbol *symb;
  SourceLoc dummyLoc;
	YYSTYPE yylval;
  
  while (isalnum(*def) || *def == '_') def++;
  if (def != name && *def == '=') {
    *def = 0;
    def++;
  } else if (def == name || *def != 0) {
    return 0;
  } else {
    def = 0;
  }
  memset(&mac, 0, sizeof(mac));
  mac.body = NewTokenStream(name);
  if (def) {
    int     token;
    PushEofSrc( cg );
    ScanFromString( cg, def);
    while ((token = cg->currentInput->scan( &yylval, cg, cg->currentInput)) > 0)
      RecordToken( yylval, cg, mac.body, token);
    PopEofSrc( cg );
  } else {
    yylval.sc_int = 1;
    RecordToken( yylval, cg, mac.body, INTCONST_SY);
  }
  symb = LookupSymbol( cg, macros, cg->GetAtom(name));
  if (symb) {
    FreeMacro( cg, &symb->details.mac);
  } else {
    dummyLoc.file = 0;
    dummyLoc.line = 0;
    symb = AddSymbol( cg, &dummyLoc, macros, cg->GetAtom(name), 0, SK_Macro);
  }
  symb->details.mac = mac;
  if (def) {
    // undo change to arg string
    def[-1] = '=';
  }
  return 1;
} // PredefineMacro
