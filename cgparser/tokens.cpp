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
// tokens.c
//

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "slglobals.h"

///////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////// Preprocessor and Token Recorder and Playback: ////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * idstr()
 * Copy a string to a malloc'ed block and convert it into something suitable
 * for an ID
 *
 */

char *idstr(const char *fstr)
{
  size_t len;
  char *str, *t;
  const char *f;
  
  len = strlen(fstr);
  str = new char[ len + 1 ];
  for (f=fstr, t=str; *f; f++) {
    if (isalnum(*f)) *t++ = *f;
    else if (*f == '.' || *f == '/') *t++ = '_';
  }
  *t = 0;
  return str;
} // idstr

/*
 * lCompressToken() -  Rename a token so that most will fit in one byte.
 *
 */

static int lCompressToken(int token)
{
  return token;
} // lCompressToken

/*
 * lExpandToken() -  Expand a token into what the scanner wants.
 *
 */

static int lExpandToken(int token)
{
  return token;
} // lExpandToken

/*
 * lNewBlock()
 *
 */

static TokenBlock *lNewBlock(TokenStream *fTok)
{
  TokenBlock *lBlock;
  
  lBlock = new TokenBlock();
  lBlock->max = 256;
  lBlock->next = NULL;
  if (fTok->head) {
    fTok->current->next = lBlock;
  } else {
    fTok->head = lBlock;
  }
  fTok->current = lBlock;
  return lBlock;
} // lNewBlock

/*
 * lAddByte()
 *
 */

static void lAddByte(TokenStream *fTok, unsigned char fVal)
{
  TokenBlock *lBlock;
  
  lBlock = fTok->current;
  if (lBlock->count >= lBlock->max)
    lBlock = lNewBlock(fTok);
  lBlock->data[lBlock->count++] = fVal;
} // lAddByte

/*
 * lAdd4Bytes()
 *
 */

static void lAdd4Bytes(TokenStream *fTok, unsigned char *fVal)
{
  TokenBlock *lBlock;
  unsigned char *pc;
  
  lBlock = fTok->current;
  if (lBlock->count + 4 > lBlock->max)
    lBlock = lNewBlock(fTok);
  pc = &lBlock->data[lBlock->count];
  lBlock->count += 4;
  pc[0] = fVal[0];
  pc[1] = fVal[1];
  pc[2] = fVal[2];
  pc[3] = fVal[3];
} // lAdd4Bytes

/*
 * lReadByte() - Get the next byte from a stream.
 *
 */

static int lReadByte(TokenStream *pTok)
{
  TokenBlock *lBlock;
  int lval = -1;
  
  lBlock = pTok->current;
  if (lBlock) {
    if (lBlock->current >= lBlock->count) {
      lBlock = lBlock->next;
      if (lBlock)
        lBlock->current = 0;
      pTok->current = lBlock;
    }
    if (lBlock)
      lval = lBlock->data[lBlock->current++];
  }
  return lval;
} // lReadByte

/*
 * lRead4Bytes()
 *
 */

static void lRead4Bytes(TokenStream *fTok, unsigned char *fVal)
{
  fVal[0] = (unsigned char ) lReadByte(fTok);
  fVal[1] = (unsigned char ) lReadByte(fTok);
  fVal[2] = (unsigned char ) lReadByte(fTok);
  fVal[3] = (unsigned char ) lReadByte(fTok);
} // lRead4Bytes

/////////////////////////////////////// Global Functions://////////////////////////////////////

/*
 * InitTokenStreams()
 *
 */

int InitTokenStreams(CgContext *Cg)
{
  return 1;
} // InitTokenStreams

/*
 * NewTokenStream()
 *
 */

TokenStream *NewTokenStream(const char *name)
{
  TokenStream *pTok;
  
  pTok = new TokenStream();
  pTok->name = idstr(name);
  lNewBlock(pTok);
  return pTok;
} // NewTokenStream

/*
 * DeleteTokenStream()
 *
 */

void DeleteTokenStream(TokenStream *pTok)
{
  TokenBlock *pBlock, *nBlock;
  
  if (pTok) {
    pBlock = pTok->head;
    while (pBlock) {
      nBlock = pBlock->next;
			delete pBlock;
      pBlock = nBlock;
    }
		if (pTok->name) {
			delete [] pTok->name;
		}
		delete pTok;
  }
} // DeleteTokenStream

/*
 * RecordToken() - Add a token to the end of a list for later playback or printout.
 *
 */

void RecordToken( YYSTYPE & yylval, CgContext *cg, TokenStream *pTok, int token)
{
  const char *s;
  
  // token = lCompressToken(token);
  if (token > 256)
    lAddByte(pTok, (unsigned char)((token & 0x7f) + 0x80));
  else
    lAddByte(pTok, (unsigned char)(token & 0x7f));
  switch (token) {
    case IDENT_SY:
    case TYPEIDENT_SY:
    case STRCONST_SY:
      s = cg->GetString(yylval.sc_ident);
      while (*s)
        lAddByte(pTok, (unsigned char) *s++);
      lAddByte(pTok, 0);
      break;
    case CFLOATCONST_SY:
    case FLOATCONST_SY:
    case FLOATHCONST_SY:
    case FLOATXCONST_SY:
      lAdd4Bytes(pTok, (unsigned char *) &yylval.sc_fval);
      break;
    case INTCONST_SY:
      lAdd4Bytes(pTok, (unsigned char *) &yylval.sc_int);
      break;
    case '(':
      lAddByte(pTok, (unsigned char)(yylval.sc_int ? 1 : 0));
    default:
      break;
  }
} // RecordToken

/*
 * RewindTokenStream() - Reset a token stream in preperation for reading.
 *
 */

void RewindTokenStream(TokenStream *pTok)
{
  if (pTok->head) {
    pTok->current = pTok->head;
    pTok->current->current = 0;
  }
} // RewindTokenStream

/*
 * ReadToken() - Read the next token from a stream.
 *
 */

int ReadToken( YYSTYPE &yylval, CgContext *cg, TokenStream *pTok)
{
  char symbol_name[MAX_SYMBOL_NAME_LEN + 1];
  char string_val[MAX_STRING_LEN + 1];
  int ltoken, len;
  char ch;
  
  ltoken = lReadByte(pTok);
  if (ltoken >= 0) {
    if (ltoken > 127)
      ltoken += 128;
    //ltoken = lExpandToken(ltoken);
    switch (ltoken) {
      case IDENT_SY:
      case TYPEIDENT_SY:
        len = 0;
        ch = lReadByte(pTok);
        while ((ch >= 'a' && ch <= 'z') ||
               (ch >= 'A' && ch <= 'Z') ||
               (ch >= '0' && ch <= '9') ||
               ch == '_')
        {
          if (len < MAX_SYMBOL_NAME_LEN) {
            symbol_name[len] = ch;
            len++;
            ch = lReadByte(pTok);
          }
        }
        symbol_name[len] = '\0';
        assert(ch == '\0');
        yylval.sc_ident = cg->GetAtom(symbol_name);
        return IDENT_SY;
        break;
      case STRCONST_SY:
        len = 0;
        while ((ch = lReadByte(pTok)) != 0)
          if (len < MAX_STRING_LEN)
            string_val[len++] = ch;
        string_val[len] = 0;
        yylval.sc_ident = cg->GetAtom(string_val);
        break;
      case CFLOATCONST_SY:
      case FLOATCONST_SY:
      case FLOATHCONST_SY:
      case FLOATXCONST_SY:
        lRead4Bytes(pTok, (unsigned char *) &yylval.sc_fval);
        break;
      case INTCONST_SY:
        lRead4Bytes(pTok, (unsigned char *) &yylval.sc_int);
        break;
      case '(':
        yylval.sc_int = lReadByte(pTok);
        break;
    }
    return ltoken;
  }
  return EOF_SY;
} // ReadToken

struct TokenInputSrc : public InputSrc {
	TokenInputSrc( CgContext * cg ) : InputSrc( cg ), tokens( NULL ), startGlobalScopeAtEnd( false ) {}
  TokenStream         *tokens;
	bool startGlobalScopeAtEnd;
};

static int scan_token( YYSTYPE & yylval, CgContext *cg, TokenInputSrc *in)
{
  int token = ReadToken( yylval, cg, in->tokens);
  cg->tokenLoc->file = cg->currentInput->name;
  cg->tokenLoc->line = cg->currentInput->line;
  if (token == '\n') {
    in->line++;
    //cg->Printf("    end of line %d\n", tokenloc->line);
    return token;
  }
  if (token > 0) return token;
  cg->currentInput = in->prev;
  bool doGlobal = in->startGlobalScopeAtEnd;
	if ( doGlobal ) {
		cg->StartGlobalScope();
	}
	delete in;
  return cg->currentInput->scan( &yylval, cg, cg->currentInput);
}

int ReadFromTokenStream( CgContext *cg, TokenStream *ts, Atom name, bool startGlobalScope )
{
  TokenInputSrc *in = new TokenInputSrc( cg );
  in->name = name;
  in->prev = cg->currentInput;
  in->scan = (int (*)( YYSTYPE *, CgContext *, InputSrc *))scan_token;
  in->line = 1;
  in->tokens = ts;
  in->startGlobalScopeAtEnd = startGlobalScope;
  RewindTokenStream(ts);
  cg->currentInput = in;
  return 1;
}

struct UngotToken : public InputSrc {
	UngotToken( CgContext *cg ) : InputSrc( cg ), token( 0 ) {}
  int         token;
  YYSTYPE     lval;
};

static int reget_token( YYSTYPE * pyylval, CgContext *cg, UngotToken *t)
{
	YYSTYPE & yylval = *pyylval;
  int token = t->token;
  yylval = t->lval;
  cg->currentInput = t->prev;
	delete t;
  return token;
}

void UngetToken( YYSTYPE & yylval, CgContext *cg, int token) {
  UngotToken *t = new UngotToken( cg );
  t->token = token;
  t->lval = yylval;
  t->scan = ( int (*)( YYSTYPE *, CgContext *, InputSrc *) )reget_token;
  t->prev = cg->currentInput;
  t->name = cg->currentInput->name;
  t->line = cg->currentInput->line;
  cg->currentInput = t;
}

TokenStream::~TokenStream() {
	if ( allocated ) {
		delete next;
		delete head;
		delete [] name;
	}
}


///////////////////////////////////// Tokenize Input File: ////////////////////////////////////

#if defined(CGC_ENABLE_TOOLS)

void TokenizeInput( YYSTYPE & yylval, CgContext *cg )
{
  int ltoken, index, count;
  
  TokenStream *RecordedTokens = NewTokenStream( cg->options.sourceFileName );
  while ( (ltoken = cg->currentInput->scan( &yylval, cg, cg->currentInput) ) > 0 && ltoken != ERROR_SY ) {
    RecordToken( yylval, cg, RecordedTokens, ltoken );
  }
  
  // Debug print stuff to screen:
  
#if 0
  RewindTokenStream(RecordedTokens);
  ltoken = ReadToken(RecordedTokens);
  while (ltoken != EOF_SY && ltoken != ERROR_SY) {
    if (ltoken >= 127)
      cg->Printf("token = %s", cg->GetString(ltoken));
    else if (ltoken >= 32)
      cg->Printf("token = <%c>", ltoken);
    else if (ltoken == '\n')
      cg->Printf("token = <\\n>");
    else if (ltoken > 0)
      cg->Printf("token = <\\0%o>", ltoken);
    else
      cg->Printf("token = <EOF>");
    switch (ltoken) {
      case IDENT_SY:
      case TYPEIDENT_SY:
      case STRCONST_SY:
        cg->Printf(" = \"%s\"", cg->GetString(yylval.sc_ident));
        break;
      case CFLOATCONST_SY:
      case FLOATCONST_SY:
      case FLOATHCONST_SY:
      case FLOATXCONST_SY:
        cg->Printf(" = %g9.6", yylval.sc_fval);
        break;
      case INTCONST_SY:
        cg->Printf(" = %d", yylval.sc_int);
        break;
    }
    cg->Printf("\n");
    ltoken = ReadToken(RecordedTokens);
  }
#endif
  
  // Dump file to screen as a C initialization statement:
  
  cg->Printf("// automatically generated from %s -- do no edit\n\n",
             cg->options.sourceFileName);
  cg->Printf("#include <stdio.h>\n");
  cg->Printf("#include \"slglobals.h\"\n\n");
  cg->Printf("unsigned char %s_tokendata[] = {\n", RecordedTokens->name);
  RewindTokenStream(RecordedTokens);
  ltoken = lReadByte(RecordedTokens);
  index = count = 0;
  while (ltoken != EOF_SY && ltoken != ERROR_SY) {
    if (index == 0)
      cg->Printf("    ");
    cg->Printf("0x%02x,", ltoken);
    count++;
    index++;
    if (index >= 16) {
      cg->Printf("\n");
      index = 0;
    }
    ltoken = lReadByte(RecordedTokens);
  }
  if (index > 0)
    cg->Printf("\n");
  cg->Printf("};");
  cg->Printf("\n");
  cg->Printf("TokenBlock %s_blockdata = TokenBlock(\n", RecordedTokens->name);
  cg->Printf("    NULL, // next\n");
  cg->Printf("    0,    // current\n");
  cg->Printf("    %d, // count\n", count);
  cg->Printf("    %d, // max\n", count);
  cg->Printf("    %s_tokendata // data\n", RecordedTokens->name);
  cg->Printf(");\n");
  
  cg->Printf("TokenStream %s_stream = TokenStream(\n", RecordedTokens->name);
  cg->Printf("    NULL, // next\n"
             "    \"%s\", // name\n", cg->options.sourceFileName);
  cg->Printf("    &%s_blockdata, // head\n", RecordedTokens->name);
  cg->Printf("    &%s_blockdata // current\n", RecordedTokens->name);
  cg->Printf(");\n");
  
} // TokenizeInput

void DumpTokenStream( YYSTYPE & yylval, CgContext *cg, FILE *fp, TokenStream *s) {
  int token;
  
  if (fp == 0) fp = stdout;
  RewindTokenStream(s);
  while ((token = ReadToken( yylval, cg, s)) > 0) {
    switch (token) {
      case IDENT_SY:
      case TYPEIDENT_SY:
        cg->Printf("%s ", cg->GetString(yylval.sc_ident));
        break;
      case STRCONST_SY:
        cg->Printf("\"%s\"", cg->GetString(yylval.sc_ident));
        break;
      case CFLOATCONST_SY:
      case FLOATCONST_SY:
      case FLOATHCONST_SY:
      case FLOATXCONST_SY:
        cg->Printf("%g9.6 ", yylval.sc_fval);
        break;
      case INTCONST_SY:
        cg->Printf("%d ", yylval.sc_int);
        break;
      default:
        if (token >= 127)
          cg->Printf("%s ", cg->GetString(token));
        else
          cg->Printf("%c", token);
        break;
    }
  }
}
#endif // defined(CGC_ENABLE_TOOLS)

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////// End of tokens.c ///////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
