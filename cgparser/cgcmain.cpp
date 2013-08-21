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
// cgcmain.c
//

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>

#include "slglobals.h"

// hal constructors
Hal * CreateGenericHal( CgContext *cg, Atom entryName );
Hal * CreateGlslesvHal( CgContext *cg, Atom entryName );

int CommandLineArgs( Options & opt, int argc, char **argv, CgContext *cg = NULL );

int main(int argc, char **argv)
{
  const char *copyright = "(c) 2001-2011 NVIDIA Corp.";
  
	copyright;
  
	// register profiles
	RegisterProfile( CreateGenericHal, "generic" );
	RegisterProfile( CreateGlslesvHal, "glslesv" );
  
	Options opt;
	if ( ! CommandLineArgs( opt, argc, argv ) )
		return 1;
  
	CgContext *cg = new CgContext( opt );
  
  if ( ! CommandLineArgs( opt, argc, argv, cg ) )
    return 1;
  if ( ! InitTokenStreams( cg ) )
    return 1;
  if ( ! SetInputFile( cg, cg->options.sourceFileName ) )
    return 1;
  if ( cg->options.printVersion ) {
    cg->Printf(SX_TAG ": version %d.%d.%04d%s, build date %s %s.\n",
               HSL_VERSION, HSL_SUB_VERSION, HSL_SUB_SUB_VERSION, NDA_STRING, Build_Date, Build_Time);
  }
#if defined(CGC_ENABLE_TOOLS)
  if (cg->options.tokenize) {
		YYSTYPE yylval;
    TokenizeInput( yylval, cg );
  } else
#endif // defined(CGC_ENABLE_TOOLS)
  {
		cg->WriteListingHeader();
		cg->WriteOutputHeader();
    cg->PrintOptions(argc, argv);
    if (!cg->options.noStdlib) {
      if (!ReadFromTokenStream( cg, &stdlib_cg_stream, cg->GetAtom("<stdlib>"), true ))
      {
        return 1;
      }
    } else
      cg->StartGlobalScope();
#if defined(_DEBUG) || defined(__GNUC__)
    yyparse( cg );
#else
    try {
      yyparse( cg );
    } catch( ... ) {
      FatalError(cg, "*** exception during compilation ***");
    }
#endif
    cg->CompileProgram( cg->tokenLoc, cg->currentScope);
		//sx::PrintAllocationStats();
    while (cg->currentScope) {
			if (cg->options.dumpParseTree) {
				Writer wr( cg, std::cout );
				wr.WriteScopeDeclarations();
			}
      PopScope( cg );
    }
    if (cg->options.dumpAtomTable)
      cg->GetAtomTable()->Print();
  }
	int numerrors = cg->errorCount;
	delete cg;
	//sx::PrintAllocationStats();
  return numerrors;
} // main

void PrintHelp( CgContext *cg )
{
  Profile *lProfile;
  int ii;
  
  cg->Printf("usage: " SX_TAG " [-quiet] [-nocode] [-nostdlib] [-longprogs] [-v] [-Dmacro[=value]] \n           [-profile id] [-entry id] [-o ofile] [file.cg]\n");
#if defined(CGC_DEBUG_THE_COMPILER)
  cg->Printf("           [-atom] [-scan] [-tree] [-node] [-final] [-trap] [-comments] [-cdbg0]\n");
#endif
  cg->Printf("supported profiles:\n");
  ii = 0;
  while ((lProfile = EnumerateProfiles(ii++)))
    cg->Printf("    \"%s\"\n", lProfile->name );
} // PrintHelp

int CommandLineArgs( Options &opt, int argc, char **argv, CgContext *cg )
{
  int ii;
  
  for (ii = 1; ii < argc; ii++) {
    if (argv[ii][0] == '-') {
      if (!strcmp(argv[ii], "-debug")) {
        if (cg == NULL)
          opt.debugMode = true;
      } else if (!strcmp(argv[ii], "-quiet") || !strcmp(argv[ii], "-q")) {
        if (cg == NULL)
          opt.quiet = true;
      } else if (!strcmp(argv[ii], "-nocode")) {
        if (cg == NULL)
          opt.noCodeGen = true;
      } else if (!strcmp(argv[ii], "-nowarn")) {
        if (cg == NULL)
          opt.noWarnings = true;
      } else if (!strcmp(argv[ii], "-nostdlib")) {
        if (cg == NULL)
          opt.noStdlib = true;
      } else if (!strcmp(argv[ii], "-error")) {
        if (cg == NULL)
          opt.errorMode = true;
      } else if (!strcmp(argv[ii], "-longprogs")) {
        if (cg == NULL)
          opt.allowLongPrograms = true;
      } else if (!strcmp(argv[ii], "-posinv")) {
        if (cg == NULL)
          opt.positionInvariant = true;
      } else if (!strcmp(argv[ii], "-v")) {
        if (cg == NULL)
          opt.printVersion = true;
#if defined(CGC_ENABLE_TOOLS)
      } else if (!strcmp(argv[ii], "-tokenize")) {
        if (cg == NULL)
          opt.tokenize = true;
#endif // defined(CGC_ENABLE_TOOLS)
#if defined(CGC_DEBUG_THE_COMPILER)
      } else if (!strcmp(argv[ii], "-atom")) {
        if (cg == NULL)
          opt.dumpAtomTable = true;
      } else if (!strcmp(argv[ii], "-scan")) {
        if (cg == NULL)
          opt.traceScanner = true;
      } else if (!strcmp(argv[ii], "-tree")) {
        if (cg == NULL)
          opt.dumpParseTree = true;
      } else if (!strcmp(argv[ii], "-node")) {
        if (cg == NULL)
          opt.dumpNodeTree = true;
      } else if (!strcmp(argv[ii], "-final")) {
        if (cg == NULL)
          opt.dumpFinalTree = true;
      } else if (!strcmp(argv[ii], "-trap")) {
        if (cg == NULL)
          opt.trapOnError = true;
      } else if (!strcmp(argv[ii], "-comments")) {
        if (cg == NULL)
          opt.comments = true;
      } else if (!strcmp(argv[ii], "-cdbg0")) {
        if (cg == NULL)
          opt.debugLevel = 0;
      } else if (!strcmp(argv[ii], "-cdbg1")) {
        if (cg == NULL)
          opt.debugLevel = 1;
      } else if (!strcmp(argv[ii], "-cdbg2")) {
        if (cg == NULL)
          opt.debugLevel = 2;
      } else if (!strcmp(argv[ii], "-cdbg3")) {
        if (cg == NULL)
          opt.debugLevel = 3;
#endif // defined(CGC_DEBUG_THE_COMPILER)
      } else if (!strcmp(argv[ii], "-o")) {
        ii++;
        if (cg == NULL) {
          if (ii < argc) {
            opt.outputFileName = argv[ii];
          } else {
            cg->Printf(SX_TAG ": missing output file after \"-o\"\n");
            return 0;
          }
        }
      } else if (!strcmp(argv[ii], "-l")) {
        ii++;
        if (cg == NULL) {
          if (ii < argc) {
            opt.listFileName = argv[ii];
          } else {
            cg->Printf(SX_TAG ": missing listing file after \"-l\"\n");
            return 0;
          }
        }
      } else if (!strcmp(argv[ii], "-help") || !strcmp(argv[ii], "-h")) {
        if (cg != NULL)
          PrintHelp( cg );
      } else if (!strcmp(argv[ii], "-profile")) {
        ii++;
        if (cg == NULL) {
          if (ii < argc) {
            if (opt.profileString) {
              cg->Printf(SX_TAG ": multiple profiles\n");
              return 0;
            }
            opt.profileString = argv[ii];
          } else {
            cg->Printf(SX_TAG ": missing profile name after \"-profile\"\n");
            return 0;
          }
        }
      } else if (!strcmp(argv[ii], "-entry")) {
        ii++;
        if (cg == NULL) {
          if (ii < argc) {
            if (opt.entryName) {
              cg->Printf(SX_TAG ": multiple entries\n");
              return 0;
            }
            opt.entryName = argv[ii];
          } else {
            cg->Printf(SX_TAG ": missing entry name after \"-entry\"\n");
            return 0;
          }
        }
      } else if (argv[ii][1] == 'D') {
        if (cg != NULL) {
          if (!PredefineMacro( cg, argv[ii]+2)) {
            cg->Printf(SX_TAG ": bad argument: \"%s\"\n", argv[ii]);
            return 0;
          }
        }
      } else {
        if (cg != NULL) {
          cg->Printf(SX_TAG ": bad argument: \"%s\"\n", argv[ii]);
          PrintHelp( cg );
          return 0;
        }
      }
    } else {
      if (cg != NULL) {
        if ( cg->options.sourceFileName) {
          cg->Printf(SX_TAG ": multiple input files: \"%s\" \"%s\" ...\n",
                     cg->options.sourceFileName, argv[ii]);
          return 0;
        } else {
          cg->options.sourceFileName = argv[ii];
        }
      }
    }
  }
  if (cg == NULL) {
    if (opt.profileString == NULL) {
      opt.profileString = "generic";
    }
    if (opt.entryName == NULL)
      opt.entryName = "main";
  }
	/*
   if (cg != NULL) {
   if (!InitHal( cg, opt.profileString, opt.entryName)) {
   cg->Printf(SX_TAG ": hal initialization failure.\n");
   return 0;
   }
   }
   */
  return 1;
} // CommandLineArgs

