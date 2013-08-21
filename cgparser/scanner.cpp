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
// scanner.c
//

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if _MSC_VER
# define WIN32_LEAN_AND_MEAN
# include <windows.h>
# undef max
//extern "C" void OutputDebugStringA( const char *str );
#endif


#if 0
#include <ieeefp.h>
#else
#define isinff(x) (((*(long *)&(x) & 0x7f800000L)==0x7f800000L) && \
((*(long *)&(x) & 0x007fffffL)==0000000000L))
#endif

#include "slglobals.h"

const char *Build_Date = __DATE__;
const char *Build_Time = __TIME__;

struct FileInputSrc : public InputSrc {
	FileInputSrc( CgContext *cg ) : InputSrc( cg ), fd( NULL ), save_cnt( 0 ) {
		save[0] = save[1] = save[2] = 0;
	}
	virtual ~FileInputSrc() {
		if ( fd ) {
			fclose( fd );
			fd = NULL;
		}
	}
  FILE                *fd;
  char                save_cnt;
  char                save[3];
};

struct StringInputSrc : public InputSrc {
	StringInputSrc( CgContext * cg) : InputSrc( cg ), p( NULL )
	{}
  char *p;
};

static int eof_scan( YYSTYPE * pyylval, CgContext *cg, InputSrc *is) { return EOF; }
static int eof_getch( CgContext *cg, InputSrc *is) { return EOF; }
static void noop( CgContext *cg, InputSrc *in, int ch) {}

static InputSrc eof_inputsrc = InputSrc( NULL, 0, &eof_scan, &eof_getch, &noop, 0, 0 );

static int byte_scan( YYSTYPE * pyylval, CgContext *, InputSrc *);
static int nextchar( CgContext *, FileInputSrc *);
static void ungetchar( CgContext *, FileInputSrc *, int);

#define EOL_SY '\n'

#if _MSC_VER
#define DBG_BREAKPOINT() DebugBreak()
#else
#define DBG_BREAKPOINT()
#endif

int SetInputFile( CgContext *cg, const char *fname)
{
  FileInputSrc *in = new FileInputSrc( cg );
  if (fname) {
    //if (!cg->options.quiet)
    //    cg->Printf("%s\n", fname);
    in->name = cg->GetAtom(fname);
    in->fd = fopen(fname, "r");
    if (!in->fd) {
      cg->Printf(SX_TAG ": cannot open input file \"%s\"\n", fname);
      char cwd[128];
      getcwd(cwd, sizeof(cwd));
      cg->Printf(SX_TAG ": cwd = \"%s\"\n", cwd);
			delete in;
      return 0;
    }
  } else {
    in->fd = stdin;
    in->name = cg->GetAtom("<stdin>");
  }
  in->line = 1;
  in->scan = byte_scan;
  in->getch = (int (*)( CgContext *, InputSrc *)) nextchar;
  in->ungetch = (void (*)( CgContext *, InputSrc *, int)) ungetchar;
  in->prev = cg->currentInput;
  cg->currentInput = in;
  return 1;
} // SetInputFile

static int str_getch( CgContext *cg, StringInputSrc *in)
{
  if (*in->p)
    return *in->p++;
  cg->currentInput = in->prev;
	delete in;
  return ' ';
} // str_getch

static void str_ungetch( StringInputSrc *in, int ch) {
  if (in->p[-1] == ch) in->p--;
} // str_ungetch

int ScanFromString( CgContext *cg, char *s)
{
  StringInputSrc *in = new StringInputSrc( cg );
  in->p = s;
  in->line = 1;
  in->scan = byte_scan;
  in->getch = (int (*)( CgContext *, InputSrc *))str_getch;
  in->ungetch = (void (*)( CgContext *,InputSrc *, int))str_ungetch;
  in->prev = cg->currentInput;
  cg->currentInput = in;
  return 1;
} // ScanFromString;

int GetErrorCount( CgContext * cg )
{
  return cg->errorCount;
} // GetErrorCount

/*
 * bumpErrorCount() - Useful for setting breakpoints when debugging.
 *
 */

void bumpErrorCount( CgContext *cg )
{
  cg->errorCount++;
  if (cg->options.trapOnError) {
    DBG_BREAKPOINT();
  }
} // bumbErrorCount

/*
 * bumpWarningCount() - Useful for setting breakpoints when debugging.
 *
 */

void bumpWarningCount( CgContext * cg )
{
  cg->warningCount++;
  if (cg->options.trapOnError) {
    DBG_BREAKPOINT();
  }
} // bumpWarningCount

// Called by yyparse on an error:

void yyerror( CgContext *cg, const char *s)
{
  if (!cg->options.errorMode) {
    if (cg->ltokenLoc.file.IsValid()) {
      cg->ListingPrintf( "%s(%d) : error C0000: ",
                        cg->GetString(cg->ltokenLoc.file), cg->ltokenLoc.line);
    } else {
      cg->ListingPrintf( "(%d) : error C0000: ", cg->currentInput->line);
    }
    cg->ListingPrintf( "%s at token \"%s\"\n", s,
                      cg->GetString(cg->mostRecentToken));
    cg->allowSemanticParseErrors = 1;
  }
  bumpErrorCount( cg );
} // yyerror

/*
 * SemanticParseError() - Compiler generated semantic error inside an error rule.
 *
 */

void SemanticParseError( CgContext *cg, SourceLoc *loc, int num, const char *mess, ...)
{
  va_list args;
  
	if ( loc == NULL ) {
		loc = cg->pLastSourceLoc;
	}
  
  if (cg->allowSemanticParseErrors) {
    if (!cg->options.errorMode) {
      if (loc->file.IsValid()) {
        cg->ListingPrintf( "%s(%d) : error C%04d: ",
                          cg->GetString(loc->file), loc->line, num);
      } else {
        cg->ListingPrintf( "(%d) : error C%04d: ", loc->line, num);
      }
      va_start(args, mess);
      cg->ListingVPrintf( mess, args);
      va_end(args);
      cg->ListingPrintf( "\n");
      bumpErrorCount( cg );
    } else {
      MarkErrorPosHit( cg, loc);
    }
    
    cg->allowSemanticParseErrors = 0;
  }
} // SemanticParseError

/*
 * SemanticError() - Compiler generated semantic error.
 *
 */

void SemanticError( CgContext *cg, SourceLoc *loc, int num, const char *mess, ...)
{
  va_list args;
  
	if ( loc == NULL ) {
		loc = cg->pLastSourceLoc;
	}
  
  if (!cg->options.errorMode) {
    if (loc->file.IsValid()) {
      cg->ListingPrintf( "%s(%d) : error C%04d: ",
                        cg->GetString(loc->file), loc->line, num);
    } else {
      cg->ListingPrintf( "(%d) : error C%04d: ", loc->line, num);
    }
    va_start(args, mess);
    cg->ListingVPrintf( mess, args);
    va_end(args);
    cg->ListingPrintf( "\n");
    bumpErrorCount( cg );
  } else {
    MarkErrorPosHit( cg, loc);
  }
} // SemanticError

/*
 * InternalError() - Internal compiler error.
 *
 */

void InternalError( CgContext *cg, SourceLoc *loc, int num, const char *mess, ...)
{
  va_list args;
  
	if ( loc == NULL ) {
		loc = cg->pLastSourceLoc;
	}
  
  if (loc->file.IsValid()) {
    cg->ListingPrintf( "%s(%d) : error C%04d: ",
                      cg->GetString(loc->file), loc->line, num);
  } else {
    cg->ListingPrintf( "(%d) : error C%04d: ", loc->line, num);
  }
  va_start(args, mess);
  cg->ListingVPrintf( mess, args);
  va_end(args);
  cg->ListingPrintf( "\n");
  bumpErrorCount( cg );
} // InternalError

/*
 * FatalError() - Fatal internal compiler error.
 *
 */

void FatalError( CgContext *cg, const char *mess, ...)
{
  va_list args;
  
  cg->ListingPrintf( "(%d) : fatal error C9999: ", cg->lineCount);
  va_start(args, mess);
  cg->ListingVPrintf( mess, args);
  va_end(args);
  cg->ListingPrintf( "\n");
  bumpErrorCount( cg );
  cg->WriteOutputFiles("Compilation terminated due to fatal error");
  exit(9999);
} // InternalError

/*
 * SemanticWarning() - Compiler generated semantic warning.
 *
 */

void SemanticWarning( CgContext *cg, SourceLoc *loc, int num, const char *mess, ...)
{
  va_list args;
  
	if ( loc == NULL ) {
		loc = cg->pLastSourceLoc;
	}
  
  if (!cg->options.noWarnings) {
    if (!cg->options.errorMode) {
      if (loc->file.IsValid()) {
        cg->ListingPrintf( "%s(%d) : warning C%04d: ",
                          cg->GetString(loc->file), loc->line, num);
      } else {
        cg->ListingPrintf( "(%d) : warning C%04d: ", loc->line, num);
      }
      va_start(args, mess);
      cg->ListingVPrintf( mess, args);
      va_end(args);
      cg->ListingPrintf( "\n");
    }
    bumpWarningCount( cg );
  }
} // SemanticWarning

/*
 * InformationalNotice() - Print a message from the compiler.
 *
 */

void InformationalNotice( CgContext *cg, SourceLoc *loc, int num, const char *mess, ...)
{
  va_list args;
  
	if ( loc == NULL ) {
		loc = cg->pLastSourceLoc;
	}
  
  if (!cg->options.noWarnings) {
    if (!cg->options.errorMode) {
      if (loc->file.IsValid()) {
        cg->ListingPrintf( "%s(%d) : notice C%04d: ",
                          cg->GetString(loc->file), loc->line, num);
      } else {
        cg->ListingPrintf( "(%d) : notice C%04d: ", loc->line, num);
      }
      va_start(args, mess);
      cg->ListingVPrintf( mess, args);
      va_end(args);
      cg->ListingPrintf( "\n");
    }
  }
} // InformationalNotice

// The scanner:

static int nextchar( CgContext *cg, FileInputSrc *in)
{
  int ch;
  
  if (in->save_cnt) {
    ch = in->save[--in->save_cnt];
  } else {
    ch = getc(in->fd);
    if (ch == EOF) {
      cg->currentInput = in->prev;
      delete in;
      return '\n';
    }
  }
  if (ch == '\n') {
    cg->lineCount++;
    in->line++;
  }
  
  return ch;
} // nextchar

static void ungetchar( CgContext *cg, FileInputSrc *in, int ch)
{
  if (in == cg->currentInput) {
    if (in->save_cnt < sizeof(in->save))
      in->save[in->save_cnt++] = ch;
    if (ch == '\n') {
      in->line--;
      cg->lineCount--;
    }
  }
  
} // ungetchar

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Floating point constants: /////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * lBuildFloatValue() - Quick and dirty conversion to floating point.  Since all
 *         we need is single precision this should be quite precise.
 */

static float lBuildFloatValue( CgContext *cg, const char *str, int len, int expr)
{
  double val, expval, ten;
  int ii, llen, absexp;
  float rv;
  
  val = 0.0;
  llen = len;
  for (ii = 0; ii < len; ii++)
    val = val*10.0 + (str[ii] - '0');
  if (expr != 0) {
    absexp = expr > 0 ? expr : -expr;
    expval = 1.0f;
    ten = 10.0;
    while (absexp) {
      if (absexp & 1)
        expval *= ten;
      ten *= ten;
      absexp >>= 1;
    }
    if (expr >= 0) {
      val *= expval;
    } else {
      val /= expval;
    }
  }
  rv = (float)val;
  if (isinff(rv)) {
    SemanticError( cg, cg->tokenLoc, ERROR___FP_CONST_OVERFLOW);
  }
  return rv;
} // lBuildFloatValue

/*
 * lFloatConst() - Scan a floating point Constant.  Assumes that the scanner
 *         has seen at least one digit, followed by either a decimal '.' or the
 *         letter 'e'.
 */

static int lFloatConst( YYSTYPE & yylval, CgContext *cg, char *str, int len, int ch)
{
  int HasDecimal, declen, expr, ExpSign;
  float lval;
  
  HasDecimal = 0;
  declen = 0;
  expr = 0;
  if (ch == '.') {
    HasDecimal = 1;
    ch = cg->currentInput->getch( cg, cg->currentInput);
    while (ch >= '0' && ch <= '9') {
      if (len < MAX_SYMBOL_NAME_LEN) {
        declen++;
        if (len > 0 || ch != '0') {
          str[len] = ch;
          len++;
        }
        ch = cg->currentInput->getch( cg, cg->currentInput);
      } else {
        SemanticError( cg, cg->tokenLoc, ERROR___FP_CONST_TOO_LONG);
        len = 1;
      }
    }
  }
  
  // Exponent:
  
  if (ch == 'e' || ch == 'E') {
    ExpSign = 1;
    ch = cg->currentInput->getch( cg, cg->currentInput);
    if (ch == '+') {
      ch = cg->currentInput->getch( cg, cg->currentInput);
    } else if (ch == '-') {
      ExpSign = -1;
      ch = cg->currentInput->getch( cg, cg->currentInput);
    }
    if (ch >= '0' && ch <= '9') {
      while (ch >= '0' && ch <= '9') {
        expr = expr*10 + ch - '0';
        ch = cg->currentInput->getch( cg, cg->currentInput);
      }
    } else {
      SemanticError( cg, cg->tokenLoc, ERROR___ERROR_IN_EXPONENT);
    }
    expr *= ExpSign;
  }
  
  if (len == 0) {
    lval = 0.0f;
  } else {
    lval = lBuildFloatValue( cg, str, len, expr - declen);
  }
  
  // Suffix:
  
  yylval.sc_fval = lval;
  if (ch == 'h') {
    return FLOATHCONST_SY;
  } else {
    if (ch == 'x') {
      return FLOATXCONST_SY;
    } else {
      if (ch == 'f') {
        return FLOATCONST_SY;
      } else {
        cg->currentInput->ungetch( cg, cg->currentInput, ch);
        return CFLOATCONST_SY;
      }
    }
  }
} // lFloatConst

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////// Normal Scanner //////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

static int byte_scan( YYSTYPE * pyylval, CgContext *cg, InputSrc *in)
{
	YYSTYPE &yylval = *pyylval;
  char symbol_name[MAX_SYMBOL_NAME_LEN + 1];
  char string_val[MAX_STRING_LEN + 1];
  int AlreadyComplained;
  int len, ch, ii, ival;
  
  for (;;) {
    yylval.sc_int = 0;
    ch = cg->currentInput->getch( cg, cg->currentInput);
    if( cg->currentInput == NULL ) {
      return 0;
    }
    while (ch == ' ' || ch == '\t' || ch == '\r') {
      yylval.sc_int = 1;
      ch = cg->currentInput->getch( cg, cg->currentInput);
    }
    cg->ltokenLoc.file = cg->currentInput->name;
    cg->ltokenLoc.line = cg->currentInput->line;
    switch (ch) {
      default:
        return ch; // Single character token
      case EOF:
        return 0;
      case 'A': case 'B': case 'C': case 'D': case 'E':
      case 'F': case 'G': case 'H': case 'I': case 'J':
      case 'K': case 'L': case 'M': case 'N': case 'O':
      case 'P': case 'Q': case 'R': case 'S': case 'T':
      case 'U': case 'V': case 'W': case 'X': case 'Y':
      case 'Z': case '_':
      case 'a': case 'b': case 'c': case 'd': case 'e':
      case 'f': case 'g': case 'h': case 'i': case 'j':
      case 'k': case 'l': case 'm': case 'n': case 'o':
      case 'p': case 'q': case 'r': case 's': case 't':
      case 'u': case 'v': case 'w': case 'x': case 'y':
      case 'z':
        len = 0;
        do {
          if (len < MAX_SYMBOL_NAME_LEN) {
            symbol_name[len] = ch;
            len++;
            ch = cg->currentInput->getch( cg, cg->currentInput);
          }
        } while ((ch >= 'a' && ch <= 'z') ||
                 (ch >= 'A' && ch <= 'Z') ||
                 (ch >= '0' && ch <= '9') ||
                 ch == '_');
        symbol_name[len] = '\0';
        cg->currentInput->ungetch( cg, cg->currentInput, ch);
        yylval.sc_ident = cg->GetAtom(symbol_name);
        return IDENT_SY;
        break;
      case '0':
        ch = cg->currentInput->getch( cg, cg->currentInput);
        if (ch == 'x' || ch == 'X') {
          ch = cg->currentInput->getch( cg, cg->currentInput);
          if ((ch >= '0' && ch <= '9') ||
              (ch >= 'A' && ch <= 'F') ||
              (ch >= 'a' && ch <= 'f'))
          {
            AlreadyComplained = 0;
            ival = 0;
            do {
              if (ival <= 0x0fffffff) {
                if (ch >= '0' && ch <= '9') {
                  ii = ch - '0';
                } else if (ch >= 'A' && ch <= 'F') {
                  ii = ch - 'A' + 10;
                } else {
                  ii = ch - 'a' + 10;
                }
                ival = (ival << 4) | ii;
              } else {
                if (!AlreadyComplained)
                  SemanticError( cg, cg->tokenLoc, ERROR___HEX_CONST_OVERFLOW);
                AlreadyComplained = 1;
              }
              ch = cg->currentInput->getch( cg, cg->currentInput);
            } while ((ch >= '0' && ch <= '9') ||
                     (ch >= 'A' && ch <= 'F') ||
                     (ch >= 'a' && ch <= 'f'));
          } else {
            SemanticError( cg, cg->tokenLoc, ERROR___ERROR_IN_HEX_CONSTANT);
          }
          cg->currentInput->ungetch( cg, cg->currentInput, ch);
          yylval.sc_int = ival;
          return INTCONST_SY;
        } else if (ch >= '0' && ch <= '7') { // octal integer constants
          AlreadyComplained = 0;
          ival = 0;
          do {
            if (ival <= 0x1fffffff) {
              ii = ch - '0';
              ival = (ival << 3) | ii;
            } else {
              if (!AlreadyComplained)
                SemanticError( cg, cg->tokenLoc, ERROR___OCT_CONST_OVERFLOW);
              AlreadyComplained = 1;
            }
            ch = cg->currentInput->getch( cg, cg->currentInput);
          } while (ch >= '0' && ch <= '7');
          cg->currentInput->ungetch( cg, cg->currentInput, ch);
          yylval.sc_int = ival;
          return INTCONST_SY;
        } else {
          cg->currentInput->ungetch( cg, cg->currentInput, ch);
          ch = '0';
        }
        // Fall through...
      case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        len = 0;
        do {
          if (len < MAX_SYMBOL_NAME_LEN) {
            if (len > 0 || ch != '0') {
              symbol_name[len] = ch;
              len++;
            }
            ch = cg->currentInput->getch( cg, cg->currentInput);
          }
        } while (ch >= '0' && ch <= '9');
        if (ch == '.' || ch == 'e' || ch == 'f' || ch == 'h' || ch == 'x') {
          return lFloatConst( yylval, cg, symbol_name, len, ch);
        } else {
          cg->currentInput->ungetch( cg, cg->currentInput, ch);
          ival = 0;
          AlreadyComplained = 0;
          for (ii = 0; ii < len; ii++) {
            ch = symbol_name[ii] - '0';
            if (ival > 214748364 || ival == 214748364 && ch >= 8) {
              if (!AlreadyComplained)
                SemanticError( cg, cg->tokenLoc, ERROR___INTEGER_CONST_OVERFLOW);
              AlreadyComplained = 1;
            }
            ival = ival*10 + ch;
          }
          yylval.sc_int = ival;
          return INTCONST_SY;
        }
        break;
      case '-':
        ch = cg->currentInput->getch( cg, cg->currentInput);
        if (ch == '-') {
          return MINUSMINUS_SY;
        } else if (ch == '=') {
          return ASSIGNMINUS_SY;
        } else {
          cg->currentInput->ungetch( cg, cg->currentInput, ch);
          return '-';
        }
      case '+':
        ch = cg->currentInput->getch( cg, cg->currentInput);
        if (ch == '+') {
          return PLUSPLUS_SY;
        } else if (ch == '=') {
          return ASSIGNPLUS_SY;
        } else {
          cg->currentInput->ungetch( cg, cg->currentInput, ch);
          return '+';
        }
      case '*':
        ch = cg->currentInput->getch( cg, cg->currentInput);
        if (ch == '=') {
          return ASSIGNSTAR_SY;
        } else {
          cg->currentInput->ungetch( cg, cg->currentInput, ch);
          return '*';
        }
      case '%':
        ch = cg->currentInput->getch( cg, cg->currentInput);
        if (ch == '=') {
          return ASSIGNMOD_SY;
        } else {
          cg->currentInput->ungetch( cg, cg->currentInput, ch);
          return '%';
        }
      case ':':
        ch = cg->currentInput->getch( cg, cg->currentInput);
        if (ch == ':') {
          return COLONCOLON_SY;
        } else {
          cg->currentInput->ungetch( cg, cg->currentInput, ch);
          return ':';
        }
      case '=':
        ch = cg->currentInput->getch( cg, cg->currentInput);
        if (ch == '=') {
          return EQ_SY;
        } else {
          cg->currentInput->ungetch( cg, cg->currentInput, ch);
          return '=';
        }
      case '!':
        ch = cg->currentInput->getch( cg, cg->currentInput);
        if (ch == '=') {
          return NE_SY;
        } else {
          cg->currentInput->ungetch( cg, cg->currentInput, ch);
          return '!';
        }
      case '|':
        ch = cg->currentInput->getch( cg, cg->currentInput);
        if (ch == '|') {
          return OR_SY;
        } else {
          cg->currentInput->ungetch( cg, cg->currentInput, ch);
          return '|';
        }
      case '&':
        ch = cg->currentInput->getch( cg, cg->currentInput);
        if (ch == '&') {
          return AND_SY;
        } else {
          cg->currentInput->ungetch( cg, cg->currentInput, ch);
          return '&';
        }
      case '<':
        ch = cg->currentInput->getch( cg, cg->currentInput);
        if (ch == '<') {
          return LL_SY;
        } else {
          if (ch == '=') {
            return LE_SY;
          } else {
            cg->currentInput->ungetch( cg, cg->currentInput, ch);
            return '<';
          }
        }
      case '>':
        ch = cg->currentInput->getch( cg, cg->currentInput);
        if (ch == '>') {
          return GG_SY;
        } else {
          if (ch == '=') {
            return GE_SY;
          } else {
            cg->currentInput->ungetch( cg, cg->currentInput, ch);
            return '>';
          }
        }
      case '.':
        ch = cg->currentInput->getch( cg, cg->currentInput);
        if (ch >= '0' && ch <= '9') {
          cg->currentInput->ungetch( cg, cg->currentInput, ch);
          return lFloatConst( yylval, cg, symbol_name, 0, '.');
        } else {
          if (ch == '.') {
            return -1; // Special EOF hack
          } else {
            cg->currentInput->ungetch( cg, cg->currentInput, ch);
            return '.';
          }
        }
      case '/':
        ch = cg->currentInput->getch( cg, cg->currentInput);
        if (ch == '/') {
          do {
            ch = cg->currentInput->getch( cg, cg->currentInput);
          } while (ch != '\n' && ch != EOF);
          if (ch == EOF)
            return -1;
          return '\n';
        } else if (ch == '*') {
          int nlcount = 0;
          ch = cg->currentInput->getch( cg, cg->currentInput);
          do {
            while (ch != '*') {
              if (ch == '\n') nlcount++;
              if (ch == EOF) {
                SemanticError( cg, cg->tokenLoc, ERROR___EOF_IN_COMMENT);
                return -1;
              }
              ch = cg->currentInput->getch( cg, cg->currentInput);
            }
            ch = cg->currentInput->getch( cg, cg->currentInput);
            if (ch == EOF) {
              SemanticError( cg, cg->tokenLoc, ERROR___EOF_IN_COMMENT);
              return -1;
            }
          } while (ch != '/');
          if (nlcount) {
            return '\n';
          }
          // Go try it again...
        } else if (ch == '=') {
          return ASSIGNSLASH_SY;
        } else {
          cg->currentInput->ungetch( cg, cg->currentInput, ch);
          return '/';
        }
        break;
      case '"':
        len = 0;
        ch = cg->currentInput->getch( cg, cg->currentInput);
        while (ch != '"' && ch != '\n' && ch != EOF) {
          if (ch == '\\') {
            ch = cg->currentInput->getch( cg, cg->currentInput);
            if (ch == '\n' || ch == EOF) {
              break;
            }
          }
          if (len < MAX_STRING_LEN) {
            string_val[len] = ch;
            len++;
            ch = cg->currentInput->getch( cg, cg->currentInput);
          }
        };
        string_val[len] = '\0';
        if (ch == '"') {
          yylval.sc_ident = cg->GetAtom(string_val);
          return STRCONST_SY;
        } else {
          SemanticError( cg, cg->tokenLoc, ERROR___CPP_EOL_IN_STRING);
          return ERROR_SY;
        }
    }
  }
} // byte_scan

Atom scan_include_name( CgContext *cg )
{
  char buf[MAX_STRING_LEN + 1];
  int len, ch;
  
  if (!cg->currentInput->getch) return 0;
  len = 0;
  while ((ch = cg->currentInput->getch( cg, cg->currentInput)) > 0 &&
         ch != '\n' && ch != '>'
         ) {
    if (len < MAX_STRING_LEN)
      buf[len++] = ch;
  }
  buf[len] = 0;
  if (ch == '\n') cg->currentInput->ungetch( cg, cg->currentInput, ch);
  return cg->GetAtom(buf);
} // scan_include_name;

int yylex( YYSTYPE *pyylval, CgContext *cg )
{
	YYSTYPE & yylval = * pyylval;
  static int last_token = '\n';
  int token;
  
  for(;;) {
    token = cg->currentInput->scan( &yylval, cg, cg->currentInput);
    
    if (token == '#' && last_token == '\n') {
      readCPPline( yylval, cg );
      continue;
    }
    last_token = token;
    
    // expand macros
    if (token == IDENT_SY && MacroExpand( yylval, cg, yylval.sc_ident))
      continue;
    
    // convert IDENTs to reserved words or TYPEIDENT as appropriate
    if (token == IDENT_SY) {
      cg->mostRecentToken = yylval.sc_ident;
      if (yylval.sc_ident.a >= FIRST_USER_TOKEN_SY) {
        Symbol *pSymb = LookupSymbol( cg, NULL, yylval.sc_ident);
        if (pSymb && IsTypedef(pSymb))
          token = TYPEIDENT_SY;
      } else {
        token = yylval.sc_ident.a;
      }
    } else {
      cg->mostRecentToken = token;
    }
    
    if (cg->options.traceScanner) {
      if (token >= 127)
        cg->Printf("token = %s", cg->GetString(token));
      else if (token >= 32)
        cg->Printf("token = <%c>", token);
      else if (token == '\n')
        cg->Printf("token = <\\n>");
      else if (token > 0)
        cg->Printf("token = <\\0%o>", token);
      else
        cg->Printf("token = <EOF>");
      switch (token) {
        case IDENT_SY:
        case TYPEIDENT_SY:
        case STRCONST_SY:
          cg->Printf(" = \"%s\"", cg->GetString(yylval.sc_ident));
          break;
        case CFLOATCONST_SY:
        case FLOATCONST_SY:
        case FLOATHCONST_SY:
        case FLOATXCONST_SY:
          cg->Printf(" = %9.6g", yylval.sc_fval);
          break;
        case INTCONST_SY:
          cg->Printf(" = %d", yylval.sc_int);
          break;
      }
      cg->Printf("\n");
    }
    
    if (token == '\n') {
      continue;
    }
    
    return token;
  }
} // yylex

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////// End of scanner.c //////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

