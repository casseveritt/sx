/* A Bison parser, made by GNU Bison 2.4.2.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
 
 Copyright (C) 1984, 1989-1990, 2000-2006, 2009-2010 Free Software
 Foundation, Inc.
 
 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
 part or all of the Bison parser skeleton and distribute that work
 under terms of your choice, so long as that work isn't itself a
 parser generator using the skeleton or a modified version thereof
 as a parser skeleton.  Alternatively, if you modify or redistribute
 the parser skeleton itself, you may (at your option) remove this
 special exception, which will cause the skeleton and the resulting
 Bison output files to be licensed under the GNU General Public
 License without this special exception.
 
 This special exception was added by the Free Software Foundation in
 version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
 simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
 infringing on user name space.  This should be done even for local
 variables, as they might otherwise be expanded by user macros.
 There are some unavoidable exceptions within include files to
 define necessary library symbols; they are noted "INFRINGES ON
 USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 1 "parser.y"

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
 NVIDIA Software, with or without modifications, in source and/or binary
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

#include <stdio.h>
#include <stdlib.h>
#include <iostream>

#define NO_PARSER 1
#include "slglobals.h"



/* Line 189 of yacc.c  */
#line 127 "parser.cpp"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
/* Put the tokens into the symbol table, so that GDB and other debuggers
 know about them.  */
enum yytokentype {
  AND_SY = 258,
  ASM_SY = 259,
  ASSIGNMINUS_SY = 260,
  ASSIGNMOD_SY = 261,
  ASSIGNPLUS_SY = 262,
  ASSIGNSLASH_SY = 263,
  ASSIGNSTAR_SY = 264,
  BOOLEAN_SY = 265,
  BREAK_SY = 266,
  CASE_SY = 267,
  CFLOATCONST_SY = 268,
  COLONCOLON_SY = 269,
  CONST_SY = 270,
  CONTINUE_SY = 271,
  DEFAULT_SY = 272,
  DISCARD_SY = 273,
  DO_SY = 274,
  EQ_SY = 275,
  ELSE_SY = 276,
  ERROR_SY = 277,
  EXTERN_SY = 278,
  FLOAT_SY = 279,
  FLOATCONST_SY = 280,
  FLOATHCONST_SY = 281,
  FLOATXCONST_SY = 282,
  FOR_SY = 283,
  GE_SY = 284,
  GG_SY = 285,
  GOTO_SY = 286,
  IDENT_SY = 287,
  IF_SY = 288,
  IN_SY = 289,
  INLINE_SY = 290,
  INOUT_SY = 291,
  INT_SY = 292,
  INTCONST_SY = 293,
  INTERNAL_SY = 294,
  LE_SY = 295,
  LL_SY = 296,
  MINUSMINUS_SY = 297,
  NE_SY = 298,
  OR_SY = 299,
  OUT_SY = 300,
  PACKED_SY = 301,
  PLUSPLUS_SY = 302,
  RETURN_SY = 303,
  STATIC_SY = 304,
  STRCONST_SY = 305,
  STRUCT_SY = 306,
  SWITCH_SY = 307,
  TEXOBJ_SY = 308,
  THIS_SY = 309,
  TYPEDEF_SY = 310,
  TYPEIDENT_SY = 311,
  UNIFORM_SY = 312,
  VARYING_SY = 313,
  VOID_SY = 314,
  WHILE_SY = 315,
  FIRST_USER_TOKEN_SY = 316
};
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 229 "parser.cpp"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
|| defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
|| defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
|| defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
|| defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
/* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
/* The OS might guarantee only one guard page at the bottom of the stack,
 and a page size can be as small as 4096 bytes.  So we cannot safely
 invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
 to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
&& ! ((defined YYMALLOC || defined malloc) \
&& (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
|| defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
|| defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
&& (! defined __cplusplus \
|| (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
 N elements.  */
# define YYSTACK_BYTES(N) \
((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
+ YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
 not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
__builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
do					\
{					\
YYSIZE_T yyi;				\
for (yyi = 0; yyi < (Count); yyi++)	\
(To)[yyi] = (From)[yyi];		\
}					\
while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
 local variables YYSIZE and YYSTACKSIZE give the old and new number of
 elements in the stack, and YYPTR gives the new location of the
 stack.  Advance YYPTR to a properly aligned location for the next
 stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
do									\
{									\
YYSIZE_T yynewbytes;						\
YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
Stack = &yyptr->Stack_alloc;					\
yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
yyptr += yynewbytes / sizeof (*yyptr);				\
}									\
while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  46
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1357

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  86
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  89
/* YYNRULES -- Number of rules.  */
#define YYNRULES  209
/* YYNRULES -- Number of states.  */
#define YYNSTATES  330

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   316

#define YYTRANSLATE(YYX)						\
((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
  0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,    77,     2,     2,     2,    81,    82,     2,
  72,    71,    79,    75,    63,    76,    74,    80,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,    66,    62,
  67,    64,    68,    85,     2,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,    69,     2,    70,    83,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,    73,    84,    65,    78,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
  2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
  5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
  15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
  25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
  35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
  45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
  55,    56,    57,    58,    59,    60,    61
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
 YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
  0,     0,     3,     5,     8,    10,    12,    15,    19,    22,
  25,    27,    30,    32,    35,    38,    41,    44,    47,    50,
  52,    55,    58,    61,    64,    67,    70,    72,    76,    78,
  82,    84,    86,    88,    90,    92,    94,    96,    98,   100,
  102,   104,   106,   108,   110,   112,   114,   116,   121,   126,
  128,   130,   133,   138,   140,   142,   144,   146,   149,   151,
  152,   157,   158,   161,   163,   166,   168,   172,   174,   179,
  183,   187,   191,   194,   195,   200,   204,   206,   210,   213,
  218,   219,   221,   223,   227,   229,   233,   238,   240,   244,
  246,   250,   252,   254,   256,   260,   265,   267,   270,   273,
  277,   282,   287,   288,   290,   292,   296,   298,   302,   304,
  307,   310,   313,   316,   319,   322,   324,   329,   331,   335,
  339,   343,   345,   349,   353,   355,   359,   363,   365,   369,
  373,   377,   381,   383,   387,   391,   393,   397,   399,   403,
  405,   409,   411,   415,   417,   421,   423,   429,   431,   433,
  437,   440,   444,   446,   448,   450,   452,   454,   456,   458,
  460,   462,   464,   467,   471,   476,   479,   484,   489,   493,
  496,   498,   500,   502,   505,   507,   509,   512,   514,   518,
  520,   524,   528,   532,   536,   540,   546,   554,   564,   570,
  580,   582,   584,   585,   587,   591,   593,   594,   598,   601,
  603,   605,   607,   609,   611,   613,   615,   617,   619,   621
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
  87,     0,    -1,    88,    -1,    87,    88,    -1,    89,    -1,
  145,    -1,    91,    62,    -1,    91,    94,    62,    -1,    22,
  62,    -1,    92,   116,    -1,    92,    -1,    55,    92,    -1,
  93,    -1,    97,    92,    -1,    99,    92,    -1,    98,    92,
  -1,   101,    92,    -1,   100,    92,    -1,    46,    92,    -1,
  96,    -1,    93,    97,    -1,    93,    99,    -1,    93,    98,
  -1,    93,   101,    -1,    93,   100,    -1,    93,    46,    -1,
  95,    -1,    94,    63,    95,    -1,   112,    -1,   112,    64,
  121,    -1,    37,    -1,    24,    -1,    59,    -1,    10,    -1,
  53,    -1,   102,    -1,   171,    -1,     1,    -1,    15,    -1,
  57,    -1,    49,    -1,    23,    -1,    35,    -1,    39,    -1,
  34,    -1,    45,    -1,    36,    -1,   104,   103,   107,    65,
  -1,   106,   103,   107,    65,    -1,   104,    -1,   155,    -1,
  51,   105,    -1,    51,   105,    66,   170,    -1,   173,    -1,
  171,    -1,    51,    -1,   108,    -1,   107,   108,    -1,    89,
  -1,    -1,    67,   110,   111,    68,    -1,    -1,   111,    89,
  -1,   113,    -1,   113,   109,    -1,   114,    -1,   114,    66,
  170,    -1,   173,    -1,   114,    69,    38,    70,    -1,   114,
  69,    70,    -1,   115,   117,    71,    -1,   115,   119,    71,
  -1,   114,    72,    -1,    -1,   116,    69,    38,    70,    -1,
  116,    69,    70,    -1,   118,    -1,   117,    63,   118,    -1,
  91,   112,    -1,    91,   112,    64,   121,    -1,    -1,   120,
  -1,    90,    -1,   120,    63,    90,    -1,   144,    -1,    73,
  122,    65,    -1,    73,   122,    63,    65,    -1,   121,    -1,
  122,    63,   121,    -1,   124,    -1,   169,    14,   124,    -1,
  172,    -1,   123,    -1,   174,    -1,    72,   144,    71,    -1,
  96,    72,   129,    71,    -1,   125,    -1,   126,    47,    -1,
  126,    42,    -1,   126,    74,   168,    -1,   126,    69,   144,
  70,    -1,   126,    72,   127,    71,    -1,    -1,   128,    -1,
  144,    -1,   128,    63,   144,    -1,   144,    -1,   129,    63,
  144,    -1,   126,    -1,    47,   130,    -1,    42,   130,    -1,
  75,   130,    -1,    76,   130,    -1,    77,   130,    -1,    78,
  130,    -1,   130,    -1,    72,    90,    71,   131,    -1,   131,
  -1,   132,    79,   131,    -1,   132,    80,   131,    -1,   132,
  81,   131,    -1,   132,    -1,   133,    75,   132,    -1,   133,
  76,   132,    -1,   133,    -1,   134,    41,   133,    -1,   134,
  30,   133,    -1,   134,    -1,   135,    67,   134,    -1,   135,
  68,   134,    -1,   135,    40,   134,    -1,   135,    29,   134,
  -1,   135,    -1,   136,    20,   135,    -1,   136,    43,   135,
  -1,   136,    -1,   137,    82,   136,    -1,   137,    -1,   138,
  83,   137,    -1,   138,    -1,   139,    84,   138,    -1,   139,
  -1,   140,     3,   139,    -1,   140,    -1,   141,    44,   140,
  -1,   141,    -1,   143,    85,   144,    66,   142,    -1,   141,
  -1,   142,    -1,   146,   157,    65,    -1,   146,    65,    -1,
  91,   112,    73,    -1,   148,    -1,   149,    -1,   154,    -1,
  150,    -1,   159,    -1,   161,    -1,   151,    -1,   167,    -1,
  152,    -1,   162,    -1,    18,    62,    -1,    18,   144,    62,
  -1,   153,   148,    21,   148,    -1,   153,   147,    -1,   153,
  148,    21,   149,    -1,    33,    72,   163,    71,    -1,   155,
  157,   156,    -1,   155,   156,    -1,    73,    -1,    65,    -1,
  158,    -1,   157,   158,    -1,    89,    -1,   147,    -1,   160,
  62,    -1,    62,    -1,   126,    64,   144,    -1,   144,    -1,
  126,     5,   144,    -1,   126,     6,   144,    -1,   126,     7,
  144,    -1,   126,     8,   144,    -1,   126,     9,   144,    -1,
  60,    72,   163,    71,   148,    -1,    19,   147,    60,    72,
  163,    71,    62,    -1,    28,    72,   164,    62,   166,    62,
  164,    71,   148,    -1,    60,    72,   163,    71,   149,    -1,
  28,    72,   164,    62,   166,    62,   164,    71,   149,    -1,
  144,    -1,   165,    -1,    -1,   160,    -1,   165,    63,   160,
  -1,   163,    -1,    -1,    48,   144,    62,    -1,    48,    62,
  -1,   173,    -1,   173,    -1,   173,    -1,    56,    -1,   173,
  -1,    32,    -1,    38,    -1,    13,    -1,    25,    -1,    26,
  -1,    27,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
  0,   234,   234,   235,   242,   244,   247,   249,   251,   255,
  262,   264,   269,   271,   273,   275,   277,   279,   281,   286,
  288,   290,   292,   294,   296,   298,   302,   304,   308,   310,
  318,   320,   322,   324,   326,   328,   330,   332,   345,   353,
  361,   363,   371,   373,   381,   383,   385,   394,   396,   398,
  402,   407,   409,   413,   414,   417,   421,   422,   425,   444,
  444,   449,   450,   457,   459,   463,   465,   469,   471,   473,
  475,   477,   481,   486,   487,   489,   506,   508,   512,   514,
  519,   520,   523,   529,   542,   544,   546,   550,   552,   564,
  566,   570,   578,   579,   580,   582,   590,   591,   593,   595,
  597,   599,   604,   605,   608,   610,   614,   616,   624,   625,
  627,   629,   631,   633,   635,   643,   647,   655,   656,   658,
  660,   668,   669,   671,   679,   680,   682,   690,   691,   693,
  695,   697,   705,   706,   708,   716,   717,   725,   726,   734,
  735,   743,   744,   752,   753,   761,   762,   766,   774,   785,
  787,   791,   799,   800,   803,   804,   805,   806,   807,   808,
  811,   812,   819,   821,   829,   833,   835,   839,   847,   849,
  853,   857,   867,   868,   872,   873,   881,   882,   886,   888,
  890,   892,   894,   896,   898,   906,   908,   910,   914,   916,
  921,   925,   927,   930,   931,   945,   947,   954,   956,   964,
  967,   970,   973,   976,   979,   982,   984,   988,   992,   996
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
 First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "AND_SY", "ASM_SY", "ASSIGNMINUS_SY",
  "ASSIGNMOD_SY", "ASSIGNPLUS_SY", "ASSIGNSLASH_SY", "ASSIGNSTAR_SY",
  "BOOLEAN_SY", "BREAK_SY", "CASE_SY", "CFLOATCONST_SY", "COLONCOLON_SY",
  "CONST_SY", "CONTINUE_SY", "DEFAULT_SY", "DISCARD_SY", "DO_SY", "EQ_SY",
  "ELSE_SY", "ERROR_SY", "EXTERN_SY", "FLOAT_SY", "FLOATCONST_SY",
  "FLOATHCONST_SY", "FLOATXCONST_SY", "FOR_SY", "GE_SY", "GG_SY",
  "GOTO_SY", "IDENT_SY", "IF_SY", "IN_SY", "INLINE_SY", "INOUT_SY",
  "INT_SY", "INTCONST_SY", "INTERNAL_SY", "LE_SY", "LL_SY",
  "MINUSMINUS_SY", "NE_SY", "OR_SY", "OUT_SY", "PACKED_SY", "PLUSPLUS_SY",
  "RETURN_SY", "STATIC_SY", "STRCONST_SY", "STRUCT_SY", "SWITCH_SY",
  "TEXOBJ_SY", "THIS_SY", "TYPEDEF_SY", "TYPEIDENT_SY", "UNIFORM_SY",
  "VARYING_SY", "VOID_SY", "WHILE_SY", "FIRST_USER_TOKEN_SY", "';'", "','",
  "'='", "'}'", "':'", "'<'", "'>'", "'['", "']'", "')'", "'('", "'{'",
  "'.'", "'+'", "'-'", "'!'", "'~'", "'*'", "'/'", "'%'", "'&'", "'^'",
  "'|'", "'?'", "$accept", "compilation_unit", "external_declaration",
  "declaration", "abstract_declaration", "declaration_specifiers",
  "abstract_declaration_specifiers", "abstract_declaration_specifiers2",
  "init_declarator_list", "init_declarator", "type_specifier",
  "type_qualifier", "type_domain", "storage_class", "function_specifier",
  "in_out", "struct_or_connector_specifier", "struct_compound_header",
  "struct_or_connector_header", "struct_identifier",
  "untagged_struct_header", "struct_declaration_list",
  "struct_declaration", "annotation", "$@1", "annotation_decl_list",
  "declarator", "semantic_declarator", "basic_declarator",
  "function_decl_header", "abstract_declarator", "parameter_list",
  "parameter_declaration", "abstract_parameter_list",
  "non_empty_abstract_parameter_list", "initializer", "initializer_list",
  "variable", "basic_variable", "primary_expression", "postfix_expression",
  "actual_argument_list", "non_empty_argument_list", "expression_list",
  "unary_expression", "cast_expression", "multiplicative_expression",
  "additive_expression", "shift_expression", "relational_expression",
  "equality_expression", "AND_expression", "exclusive_OR_expression",
  "inclusive_OR_expression", "logical_AND_expression",
  "logical_OR_expression", "conditional_expression", "conditional_test",
  "expression", "function_definition", "function_definition_header",
  "statement", "balanced_statement", "dangling_statement",
  "discard_statement", "if_statement", "dangling_if", "if_header",
  "compound_statement", "compound_header", "compound_tail",
  "block_item_list", "block_item", "expression_statement",
  "expression_statement2", "iteration_statement", "dangling_iteration",
  "boolean_scalar_expression", "for_expression_opt", "for_expression",
  "boolean_expression_opt", "return_statement", "member_identifier",
  "scope_identifier", "semantics_identifier", "type_identifier",
  "variable_identifier", "identifier", "constant", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
 token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
  0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
  265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
  275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
  285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
  295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
  305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
  315,   316,    59,    44,    61,   125,    58,    60,    62,    91,
  93,    41,    40,   123,    46,    43,    45,    33,   126,    42,
  47,    37,    38,    94,   124,    63
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
  0,    86,    87,    87,    88,    88,    89,    89,    89,    90,
  91,    91,    92,    92,    92,    92,    92,    92,    92,    93,
  93,    93,    93,    93,    93,    93,    94,    94,    95,    95,
  96,    96,    96,    96,    96,    96,    96,    96,    97,    98,
  99,    99,   100,   100,   101,   101,   101,   102,   102,   102,
  103,   104,   104,   105,   105,   106,   107,   107,   108,   110,
  109,   111,   111,   112,   112,   113,   113,   114,   114,   114,
  114,   114,   115,   116,   116,   116,   117,   117,   118,   118,
  119,   119,   120,   120,   121,   121,   121,   122,   122,   123,
  123,   124,   125,   125,   125,   125,   126,   126,   126,   126,
  126,   126,   127,   127,   128,   128,   129,   129,   130,   130,
  130,   130,   130,   130,   130,   131,   131,   132,   132,   132,
  132,   133,   133,   133,   134,   134,   134,   135,   135,   135,
  135,   135,   136,   136,   136,   137,   137,   138,   138,   139,
  139,   140,   140,   141,   141,   142,   142,   143,   144,   145,
  145,   146,   147,   147,   148,   148,   148,   148,   148,   148,
  149,   149,   150,   150,   151,   152,   152,   153,   154,   154,
  155,   156,   157,   157,   158,   158,   159,   159,   160,   160,
  160,   160,   160,   160,   160,   161,   161,   161,   162,   162,
  163,   164,   164,   165,   165,   166,   166,   167,   167,   168,
  169,   170,   171,   172,   173,   174,   174,   174,   174,   174
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
  0,     2,     1,     2,     1,     1,     2,     3,     2,     2,
  1,     2,     1,     2,     2,     2,     2,     2,     2,     1,
  2,     2,     2,     2,     2,     2,     1,     3,     1,     3,
  1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
  1,     1,     1,     1,     1,     1,     1,     4,     4,     1,
  1,     2,     4,     1,     1,     1,     1,     2,     1,     0,
  4,     0,     2,     1,     2,     1,     3,     1,     4,     3,
  3,     3,     2,     0,     4,     3,     1,     3,     2,     4,
  0,     1,     1,     3,     1,     3,     4,     1,     3,     1,
  3,     1,     1,     1,     3,     4,     1,     2,     2,     3,
  4,     4,     0,     1,     1,     3,     1,     3,     1,     2,
  2,     2,     2,     2,     2,     1,     4,     1,     3,     3,
  3,     1,     3,     3,     1,     3,     3,     1,     3,     3,
  3,     3,     1,     3,     3,     1,     3,     1,     3,     1,
  3,     1,     3,     1,     3,     1,     5,     1,     1,     3,
  2,     3,     1,     1,     1,     1,     1,     1,     1,     1,
  1,     1,     2,     3,     4,     2,     4,     4,     3,     2,
  1,     1,     1,     2,     1,     1,     2,     1,     3,     1,
  3,     3,     3,     3,     3,     5,     7,     9,     5,     9,
  1,     1,     0,     1,     3,     1,     0,     3,     2,     1,
  1,     1,     1,     1,     1,     1,     1,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
 STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
 means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
  0,    37,    33,    38,     0,    41,    31,    44,    42,    46,
  30,    43,    45,     0,    40,    55,    34,     0,   202,    39,
  32,     0,     2,     4,     0,    10,    12,    19,     0,     0,
  0,     0,     0,    35,    49,     0,     5,     0,    36,     8,
  18,   204,    51,    54,    53,    11,     1,     3,     6,     0,
  26,    28,    63,    65,     0,    67,    25,    20,    22,    21,
  24,    23,    13,    15,    14,    17,    16,   170,     0,    50,
  0,   206,     0,     0,   207,   208,   209,     0,     0,   205,
  0,     0,     0,     0,   177,   150,     0,     0,     0,     0,
  0,   174,     0,    19,    92,    89,    96,   108,   115,   117,
  121,   124,   127,   132,   135,   137,   139,   141,   143,   145,
  148,     0,   179,   175,   152,   153,   155,   158,   160,     0,
  154,     0,     0,   172,   156,     0,   157,   161,   159,     0,
  91,   203,    93,     0,     7,     0,     0,   151,    59,    64,
  0,     0,    72,    82,     0,    73,     0,    76,     0,    81,
  58,     0,    56,     0,   162,     0,   108,     0,     0,     0,
  0,     0,   110,   109,   198,     0,     0,     0,    73,     0,
  111,   112,   113,   114,    28,     0,     0,     0,     0,     0,
  0,    98,    97,     0,     0,     0,     0,     0,     0,     0,
  0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
  0,     0,     0,     0,     0,     0,   165,   152,   171,   169,
  0,   149,   173,   176,     0,    52,   201,    27,     0,    29,
  84,    61,    66,     0,    69,    78,     9,     0,    70,    71,
  0,    47,    57,    48,   163,     0,   193,     0,   191,   190,
  0,   197,     0,     0,    94,     0,   106,   180,   181,   182,
  183,   184,   178,     0,     0,   103,   104,    99,   199,   118,
  119,   120,   122,   123,   126,   125,   131,   130,   128,   129,
  133,   134,   136,   138,   140,   142,   144,     0,     0,   168,
  90,   203,    87,     0,     0,    68,     0,     0,    77,    83,
  0,     0,     0,   167,     0,   116,     0,    95,   100,   101,
  0,     0,   164,   166,     0,    85,    60,    62,    79,     0,
  75,     0,   195,     0,   194,   185,   188,   107,   105,   146,
  86,    88,    74,     0,     0,   186,     0,     0,   187,   189
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
  -1,    21,    22,    91,   143,    92,    25,    26,    49,    50,
  155,    28,    29,    30,    31,    32,    33,    68,    34,    42,
  35,   151,   152,   139,   221,   284,   174,    52,    53,    54,
  226,   146,   147,   148,   149,   219,   283,    94,    95,    96,
  156,   254,   255,   245,    98,    99,   100,   101,   102,   103,
  104,   105,   106,   107,   108,   109,   110,   111,   112,    36,
  37,   113,   114,   115,   116,   117,   118,   119,   120,   121,
  209,   122,   123,   124,   125,   126,   127,   240,   237,   238,
  313,   128,   257,   129,   215,    38,   130,   131,   132
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
 STATE-NUM.  */
#define YYPACT_NINF -242
static const yytype_int16 yypact[] =
{
  1224,  -242,  -242,  -242,   -17,  -242,  -242,  -242,  -242,  -242,
  -242,  -242,  -242,  1298,  -242,    19,  -242,  1298,  -242,  -242,
  -242,  1173,  -242,  -242,    -8,  -242,   143,  -242,  1298,  1298,
  1298,  1298,  1298,  -242,   -12,   -12,  -242,   278,  -242,  -242,
  -242,  -242,    -1,  -242,  -242,  -242,  -242,  -242,  -242,   -15,
  -242,    10,    27,    73,   213,  -242,  -242,  -242,  -242,  -242,
  -242,  -242,  -242,  -242,  -242,  -242,  -242,  -242,  1224,  -242,
  1224,  -242,   630,   546,  -242,  -242,  -242,    28,    46,  -242,
  924,   924,   672,    57,  -242,  -242,   485,   924,   924,   924,
  924,  -242,    -8,    59,  -242,  -242,  -242,    96,  -242,  -242,
  0,    51,     5,     9,    44,    -9,    52,    60,   156,   -30,
  -242,    76,  -242,  -242,  -242,  -242,  -242,  -242,  -242,   546,
  -242,   347,   416,  -242,  -242,   105,  -242,  -242,  -242,   155,
  -242,   157,  -242,   142,  -242,   142,   714,  -242,  -242,  -242,
  142,   -20,  -242,  -242,   142,   144,    49,  -242,   104,   118,
  -242,  1062,  -242,  1113,  -242,    59,    21,   123,   124,   756,
  966,   966,  -242,  -242,  -242,   128,   966,   120,  -242,   126,
  -242,  -242,  -242,  -242,   134,   966,   966,   966,   966,   966,
  966,  -242,  -242,   966,   966,   798,   142,   966,   966,   966,
  966,   966,   966,   966,   966,   966,   966,   966,   966,   966,
  966,   966,   966,   966,   966,   966,  -242,   178,  -242,  -242,
  347,  -242,  -242,  -242,   142,  -242,  -242,  -242,   714,  -242,
  -242,  -242,  -242,   135,  -242,   145,   137,  1261,  -242,  -242,
  1298,  -242,  -242,  -242,  -242,   136,  -242,   148,   152,  -242,
  147,  -242,   149,   966,  -242,    54,  -242,  -242,  -242,  -242,
  -242,  -242,  -242,   146,   153,   162,  -242,  -242,  -242,  -242,
  -242,  -242,     0,     0,    51,    51,     5,     5,     5,     5,
  9,     9,    44,    -9,    52,    60,   156,   141,   546,  -242,
  -242,  -242,  -242,    56,  1011,  -242,   714,   -18,  -242,  -242,
  966,   840,   966,  -242,   546,  -242,   966,  -242,  -242,  -242,
  966,   966,  -242,  -242,   588,  -242,  -242,  -242,  -242,   160,
  -242,   163,  -242,   164,  -242,  -242,  -242,  -242,  -242,  -242,
  -242,  -242,  -242,   176,   882,  -242,   168,   546,  -242,  -242
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
  -242,  -242,   198,     1,   -82,     6,    12,  -242,  -242,   108,
  2,   214,   218,   219,   220,   225,  -242,   221,  -242,  -242,
  -242,   183,  -125,  -242,  -242,  -242,   -16,  -242,  -242,  -242,
  -242,  -242,    33,  -242,  -242,  -208,  -242,  -242,    40,  -242,
  -37,  -242,  -242,  -242,   106,  -130,   -44,   -42,     7,   -36,
  61,    62,    63,    64,    67,  -242,   -28,  -242,   -69,  -242,
  -242,   -57,  -114,  -241,  -242,  -242,  -242,  -242,  -242,   122,
  65,   159,  -111,  -242,  -152,  -242,  -242,  -157,   -50,  -242,
  -242,  -242,  -242,  -242,   138,   261,  -242,    -3,  -242
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
 positive, shift that token.  If negative, reduce the rule which
 number is the opposite.  If zero, do what YYDEFACT says.
 If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -201
static const yytype_int16 yytable[] =
{
  97,    23,    27,   157,   167,   207,    24,   236,    51,   242,
  282,   212,    44,   165,   204,    27,   158,   169,   223,    27,
  309,    55,    23,    27,    41,    40,   232,    24,   232,    45,
  27,    27,    27,    27,    27,   192,    97,   303,   194,    93,
  62,    63,    64,    65,    66,    39,   193,   134,   135,   195,
  224,    41,   310,   316,    48,  -147,    27,   259,   260,   261,
  144,    67,   206,   181,   198,   133,   145,   220,   182,   150,
  27,   150,    27,   200,   136,    18,   196,   197,   308,   187,
  188,   189,    97,   137,    97,    97,   329,   199,    93,    55,
  184,   239,   169,   185,   138,   186,   321,   239,   168,   212,
  159,   176,   177,   178,   179,   180,   246,   247,   248,   249,
  250,   251,   227,   295,   252,   253,   256,   296,   160,   304,
  228,   305,    97,    93,    93,   297,   190,   191,   225,   166,
  216,   175,    55,   311,   312,   201,   277,   216,   181,   140,
  314,    55,   141,   182,   202,   142,   262,   263,   289,   220,
  264,   265,   150,    27,   150,    27,    69,    69,     3,   203,
  183,   205,   270,   271,   302,   184,     5,   213,   185,   214,
  186,  -200,   236,    97,    41,   229,   -10,     7,     8,     9,
  315,   230,    11,   258,   235,   234,   162,   163,    12,    56,
  241,   243,    14,   170,   171,   172,   173,   244,   136,   278,
  19,   266,   267,   268,   269,   285,   287,   301,   290,   286,
  291,   281,    93,   328,     1,   292,   298,   220,   293,    47,
  294,   239,   239,     2,   299,   300,   324,   317,     3,    27,
  322,   318,    27,   144,   323,   220,     5,     6,   325,   327,
  57,    97,   168,   217,    58,    59,    60,     7,     8,     9,
  10,    61,    11,   153,   280,    97,    70,    97,    12,    13,
  288,   272,    14,   273,    15,   274,    16,   275,    17,    18,
  19,   276,    20,   319,   326,   279,    43,     0,   222,     1,
  210,     0,     0,     0,   -80,   307,    27,    97,     2,     0,
  97,    71,     0,     3,     0,     0,    72,    73,     0,     0,
  4,     5,     6,    74,    75,    76,    77,     0,     0,     0,
  41,    78,     7,     8,     9,    10,    79,    11,     0,     0,
  80,     0,     0,    12,    13,    81,    82,    14,     0,    15,
  0,    16,     0,    17,    18,    19,     0,    20,    83,     0,
  84,     0,     0,    85,     0,     0,     0,     0,     1,     0,
  86,    67,     0,    87,    88,    89,    90,     2,     0,     0,
  71,     0,     3,     0,     0,    72,    73,     0,     0,     4,
  5,     6,    74,    75,    76,    77,     0,     0,     0,    41,
  78,     7,     8,     9,    10,    79,    11,     0,     0,    80,
  0,     0,    12,    13,    81,    82,    14,     0,    15,     0,
  16,     0,    17,    18,    19,     0,    20,    83,     0,    84,
  0,     0,   208,     0,     0,     0,     0,     1,     0,    86,
  67,     0,    87,    88,    89,    90,     2,     0,     0,    71,
  0,     3,     0,     0,    72,    73,     0,     0,     4,     5,
  6,    74,    75,    76,    77,     0,     0,     0,    41,    78,
  7,     8,     9,    10,    79,    11,     0,     0,    80,     0,
  0,    12,    13,    81,    82,    14,     0,    15,     0,    16,
  0,    17,    18,    19,     0,    20,    83,     0,    84,     0,
  0,   211,     0,     0,     0,     0,     1,     0,    86,    67,
  0,    87,    88,    89,    90,     2,     0,     0,    71,     0,
  3,     0,     0,     0,     0,     0,     0,     0,     5,     6,
  74,    75,    76,     0,     0,     0,     0,    41,     0,     7,
  8,     9,    10,    79,    11,     0,     0,    80,     0,     0,
  12,    13,    81,     0,    14,     0,    15,     0,    16,     0,
  0,    18,    19,     0,    20,     0,     0,     1,     0,     0,
  0,     0,     0,     0,     0,     0,     2,    86,     0,    71,
  87,    88,    89,    90,    72,    73,     0,     0,     0,     0,
  6,    74,    75,    76,    77,     0,     0,     0,    41,    78,
  0,     0,     0,    10,    79,     0,     0,     0,    80,     1,
  0,     0,     0,    81,    82,     0,     0,    15,     2,    16,
  0,    71,    18,     0,     0,    20,    83,     0,    84,     0,
  0,     0,     6,    74,    75,    76,     0,     0,    86,    67,
  41,    87,    88,    89,    90,    10,    79,     0,     0,     0,
  80,     1,     0,     0,     0,    81,     0,     0,     0,    15,
  2,    16,     0,    71,    18,     0,     0,    20,     0,     0,
  0,     0,     0,   320,     6,    74,    75,    76,     0,     0,
  86,   218,    41,    87,    88,    89,    90,    10,    79,     0,
  0,     0,    80,     1,     0,     0,     0,    81,     0,     0,
  0,    15,     2,    16,     0,    71,    18,     0,     0,    20,
  0,     0,   154,     0,     0,     0,     6,    74,    75,    76,
  0,     0,    86,     0,    41,    87,    88,    89,    90,    10,
  79,     0,     0,     0,    80,     1,     0,     0,     0,    81,
  0,     0,     0,    15,     2,    16,     0,    71,    18,     0,
  0,    20,     0,     0,   164,     0,     0,     0,     6,    74,
  75,    76,     0,     0,    86,     0,    41,    87,    88,    89,
  90,    10,    79,     0,     0,     0,    80,     1,     0,     0,
  0,    81,     0,     0,     0,    15,     2,    16,     0,    71,
  18,     0,     0,    20,     0,     0,     0,     0,     0,     0,
  6,    74,    75,    76,     0,     0,    86,   218,    41,    87,
  88,    89,    90,    10,    79,     0,     0,     0,    80,     1,
  0,     0,     0,    81,     0,     0,     0,    15,     2,    16,
  0,    71,    18,     0,     0,    20,     0,     0,  -192,     0,
  0,     0,     6,    74,    75,    76,     0,     0,    86,     0,
  41,    87,    88,    89,    90,    10,    79,     0,     0,     0,
  80,     1,     0,     0,     0,    81,     0,     0,     0,    15,
  2,    16,     0,    71,    18,     0,     0,    20,     0,     0,
  0,     0,     0,     0,     6,    74,    75,    76,     0,  -102,
  86,     0,    41,    87,    88,    89,    90,    10,    79,     0,
  0,     0,    80,     1,     0,     0,     0,    81,     0,     0,
  0,    15,     2,    16,     0,    71,    18,     0,     0,    20,
  0,     0,  -196,     0,     0,     0,     6,    74,    75,    76,
  0,     0,    86,     0,    41,    87,    88,    89,    90,    10,
  79,     0,     0,     0,    80,     1,     0,     0,     0,    81,
  0,     0,     0,    15,     2,    16,     0,    71,    18,     0,
  0,    20,     0,     0,     0,     0,     0,     0,     6,    74,
  75,    76,     0,  -192,    86,     0,    41,    87,    88,    89,
  90,    10,    79,     0,     0,     0,    80,     1,     0,     0,
  0,    81,     0,     0,     0,    15,     2,    16,     0,    71,
  18,     0,     0,    20,     0,     0,     0,     0,     0,     0,
  6,    74,    75,    76,     0,     0,   161,     0,    41,    87,
  88,    89,    90,    10,    79,     0,     0,     0,    80,     0,
  0,     0,     1,    81,     0,     0,     0,    15,     0,    16,
  0,     2,    18,     0,     0,    20,     3,     0,     0,     0,
  0,     0,     0,     4,     5,     6,     0,     0,    86,     0,
  0,    87,    88,    89,    90,     7,     8,     9,    10,     0,
  11,     0,     0,     0,     0,     0,    12,    13,     0,     0,
  14,     0,    15,     1,    16,     0,    17,    18,    19,     0,
  20,     0,     2,     0,     0,     0,     0,     3,     0,   306,
  0,     0,     0,     0,     4,     5,     6,     0,     0,     0,
  0,     0,     0,     0,     0,     0,     7,     8,     9,    10,
  0,    11,     0,     0,     0,     0,     0,    12,    13,     0,
  0,    14,     0,    15,     1,    16,     0,    17,    18,    19,
  0,    20,     0,     2,     0,     0,     0,   231,     3,     0,
  0,     0,     0,     0,     0,     4,     5,     6,     0,     0,
  0,     0,     0,     0,     0,     0,     0,     7,     8,     9,
  10,     0,    11,     0,     0,     0,     0,     0,    12,    13,
  0,     0,    14,     0,    15,     0,    16,     0,    17,    18,
  19,     0,    20,    46,     1,     0,     0,     0,   233,     0,
  0,     0,     0,     2,     0,     0,     0,     0,     3,     0,
  0,     0,     0,     0,     0,     4,     5,     6,     0,     0,
  0,     0,     0,     0,     0,     0,     0,     7,     8,     9,
  10,     0,    11,     0,     0,     0,     0,     0,    12,    13,
  0,     0,    14,     0,    15,     1,    16,     0,    17,    18,
  19,     0,    20,     0,     2,     0,     0,     0,     0,     3,
  0,     0,     0,     0,     0,     0,     4,     5,     6,     0,
  0,     0,     0,     0,     0,     0,     0,     0,     7,     8,
  9,    10,     1,    11,     0,     0,     0,     0,     0,    12,
  13,     2,     0,    14,     0,    15,     3,    16,     0,    17,
  18,    19,     0,    20,     5,     6,     0,     0,     0,     0,
  0,     0,     0,     0,     0,     7,     8,     9,    10,     1,
  11,     0,     0,     0,     0,     0,    12,    13,     2,     0,
  14,     0,    15,     3,    16,     0,    17,    18,    19,     0,
  20,     5,     6,     0,     0,     0,     0,     0,     0,     0,
  0,     0,     7,     8,     9,    10,     0,    11,     0,     0,
  0,     0,     0,    12,    13,     0,     0,    14,     0,    15,
  0,    16,     0,     0,    18,    19,     0,    20
};

static const yytype_int16 yycheck[] =
{
  37,     0,     0,    72,    86,   119,     0,   159,    24,   166,
  218,   122,    15,    82,    44,    13,    73,    86,    38,    17,
  38,    24,    21,    21,    32,    13,   151,    21,   153,    17,
  28,    29,    30,    31,    32,    30,    73,   278,    29,    37,
  28,    29,    30,    31,    32,    62,    41,    62,    63,    40,
  70,    32,    70,   294,    62,    85,    54,   187,   188,   189,
  54,    73,   119,    42,    20,    66,    54,   136,    47,    68,
  68,    70,    70,    82,    64,    56,    67,    68,   286,    79,
  80,    81,   119,    73,   121,   122,   327,    43,    86,    92,
  69,   160,   161,    72,    67,    74,   304,   166,    86,   210,
  72,     5,     6,     7,     8,     9,   175,   176,   177,   178,
  179,   180,    63,   243,   183,   184,   185,    63,    72,    63,
  71,    65,   159,   121,   122,    71,    75,    76,   144,    72,
  133,    72,   135,   290,   291,    83,   205,   140,    42,    66,
  292,   144,    69,    47,    84,    72,   190,   191,   230,   218,
  192,   193,   151,   151,   153,   153,    34,    35,    15,     3,
  64,    85,   198,   199,   278,    69,    23,    62,    72,    14,
  74,    14,   324,   210,    32,    71,    32,    34,    35,    36,
  294,    63,    39,   186,    60,    62,    80,    81,    45,    46,
  62,    71,    49,    87,    88,    89,    90,    71,    64,    21,
  57,   194,   195,   196,   197,    70,    69,    66,    72,    64,
  62,   214,   210,   327,     1,    63,    70,   286,    71,    21,
  71,   290,   291,    10,    71,    63,    62,   296,    15,   227,
  70,   300,   230,   227,    71,   304,    23,    24,    62,    71,
  26,   278,   230,   135,    26,    26,    26,    34,    35,    36,
  37,    26,    39,    70,   214,   292,    35,   294,    45,    46,
  227,   200,    49,   201,    51,   202,    53,   203,    55,    56,
  57,   204,    59,   301,   324,   210,    15,    -1,   140,     1,
  121,    -1,    -1,    -1,    71,   284,   284,   324,    10,    -1,
  327,    13,    -1,    15,    -1,    -1,    18,    19,    -1,    -1,
  22,    23,    24,    25,    26,    27,    28,    -1,    -1,    -1,
  32,    33,    34,    35,    36,    37,    38,    39,    -1,    -1,
  42,    -1,    -1,    45,    46,    47,    48,    49,    -1,    51,
  -1,    53,    -1,    55,    56,    57,    -1,    59,    60,    -1,
  62,    -1,    -1,    65,    -1,    -1,    -1,    -1,     1,    -1,
  72,    73,    -1,    75,    76,    77,    78,    10,    -1,    -1,
  13,    -1,    15,    -1,    -1,    18,    19,    -1,    -1,    22,
  23,    24,    25,    26,    27,    28,    -1,    -1,    -1,    32,
  33,    34,    35,    36,    37,    38,    39,    -1,    -1,    42,
  -1,    -1,    45,    46,    47,    48,    49,    -1,    51,    -1,
  53,    -1,    55,    56,    57,    -1,    59,    60,    -1,    62,
  -1,    -1,    65,    -1,    -1,    -1,    -1,     1,    -1,    72,
  73,    -1,    75,    76,    77,    78,    10,    -1,    -1,    13,
  -1,    15,    -1,    -1,    18,    19,    -1,    -1,    22,    23,
  24,    25,    26,    27,    28,    -1,    -1,    -1,    32,    33,
  34,    35,    36,    37,    38,    39,    -1,    -1,    42,    -1,
  -1,    45,    46,    47,    48,    49,    -1,    51,    -1,    53,
  -1,    55,    56,    57,    -1,    59,    60,    -1,    62,    -1,
  -1,    65,    -1,    -1,    -1,    -1,     1,    -1,    72,    73,
  -1,    75,    76,    77,    78,    10,    -1,    -1,    13,    -1,
  15,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    23,    24,
  25,    26,    27,    -1,    -1,    -1,    -1,    32,    -1,    34,
  35,    36,    37,    38,    39,    -1,    -1,    42,    -1,    -1,
  45,    46,    47,    -1,    49,    -1,    51,    -1,    53,    -1,
  -1,    56,    57,    -1,    59,    -1,    -1,     1,    -1,    -1,
  -1,    -1,    -1,    -1,    -1,    -1,    10,    72,    -1,    13,
  75,    76,    77,    78,    18,    19,    -1,    -1,    -1,    -1,
  24,    25,    26,    27,    28,    -1,    -1,    -1,    32,    33,
  -1,    -1,    -1,    37,    38,    -1,    -1,    -1,    42,     1,
  -1,    -1,    -1,    47,    48,    -1,    -1,    51,    10,    53,
  -1,    13,    56,    -1,    -1,    59,    60,    -1,    62,    -1,
  -1,    -1,    24,    25,    26,    27,    -1,    -1,    72,    73,
  32,    75,    76,    77,    78,    37,    38,    -1,    -1,    -1,
  42,     1,    -1,    -1,    -1,    47,    -1,    -1,    -1,    51,
  10,    53,    -1,    13,    56,    -1,    -1,    59,    -1,    -1,
  -1,    -1,    -1,    65,    24,    25,    26,    27,    -1,    -1,
  72,    73,    32,    75,    76,    77,    78,    37,    38,    -1,
  -1,    -1,    42,     1,    -1,    -1,    -1,    47,    -1,    -1,
  -1,    51,    10,    53,    -1,    13,    56,    -1,    -1,    59,
  -1,    -1,    62,    -1,    -1,    -1,    24,    25,    26,    27,
  -1,    -1,    72,    -1,    32,    75,    76,    77,    78,    37,
  38,    -1,    -1,    -1,    42,     1,    -1,    -1,    -1,    47,
  -1,    -1,    -1,    51,    10,    53,    -1,    13,    56,    -1,
  -1,    59,    -1,    -1,    62,    -1,    -1,    -1,    24,    25,
  26,    27,    -1,    -1,    72,    -1,    32,    75,    76,    77,
  78,    37,    38,    -1,    -1,    -1,    42,     1,    -1,    -1,
  -1,    47,    -1,    -1,    -1,    51,    10,    53,    -1,    13,
  56,    -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,
  24,    25,    26,    27,    -1,    -1,    72,    73,    32,    75,
  76,    77,    78,    37,    38,    -1,    -1,    -1,    42,     1,
  -1,    -1,    -1,    47,    -1,    -1,    -1,    51,    10,    53,
  -1,    13,    56,    -1,    -1,    59,    -1,    -1,    62,    -1,
  -1,    -1,    24,    25,    26,    27,    -1,    -1,    72,    -1,
  32,    75,    76,    77,    78,    37,    38,    -1,    -1,    -1,
  42,     1,    -1,    -1,    -1,    47,    -1,    -1,    -1,    51,
  10,    53,    -1,    13,    56,    -1,    -1,    59,    -1,    -1,
  -1,    -1,    -1,    -1,    24,    25,    26,    27,    -1,    71,
  72,    -1,    32,    75,    76,    77,    78,    37,    38,    -1,
  -1,    -1,    42,     1,    -1,    -1,    -1,    47,    -1,    -1,
  -1,    51,    10,    53,    -1,    13,    56,    -1,    -1,    59,
  -1,    -1,    62,    -1,    -1,    -1,    24,    25,    26,    27,
  -1,    -1,    72,    -1,    32,    75,    76,    77,    78,    37,
  38,    -1,    -1,    -1,    42,     1,    -1,    -1,    -1,    47,
  -1,    -1,    -1,    51,    10,    53,    -1,    13,    56,    -1,
  -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,    24,    25,
  26,    27,    -1,    71,    72,    -1,    32,    75,    76,    77,
  78,    37,    38,    -1,    -1,    -1,    42,     1,    -1,    -1,
  -1,    47,    -1,    -1,    -1,    51,    10,    53,    -1,    13,
  56,    -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,
  24,    25,    26,    27,    -1,    -1,    72,    -1,    32,    75,
  76,    77,    78,    37,    38,    -1,    -1,    -1,    42,    -1,
  -1,    -1,     1,    47,    -1,    -1,    -1,    51,    -1,    53,
  -1,    10,    56,    -1,    -1,    59,    15,    -1,    -1,    -1,
  -1,    -1,    -1,    22,    23,    24,    -1,    -1,    72,    -1,
  -1,    75,    76,    77,    78,    34,    35,    36,    37,    -1,
  39,    -1,    -1,    -1,    -1,    -1,    45,    46,    -1,    -1,
  49,    -1,    51,     1,    53,    -1,    55,    56,    57,    -1,
  59,    -1,    10,    -1,    -1,    -1,    -1,    15,    -1,    68,
  -1,    -1,    -1,    -1,    22,    23,    24,    -1,    -1,    -1,
  -1,    -1,    -1,    -1,    -1,    -1,    34,    35,    36,    37,
  -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    -1,
  -1,    49,    -1,    51,     1,    53,    -1,    55,    56,    57,
  -1,    59,    -1,    10,    -1,    -1,    -1,    65,    15,    -1,
  -1,    -1,    -1,    -1,    -1,    22,    23,    24,    -1,    -1,
  -1,    -1,    -1,    -1,    -1,    -1,    -1,    34,    35,    36,
  37,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
  -1,    -1,    49,    -1,    51,    -1,    53,    -1,    55,    56,
  57,    -1,    59,     0,     1,    -1,    -1,    -1,    65,    -1,
  -1,    -1,    -1,    10,    -1,    -1,    -1,    -1,    15,    -1,
  -1,    -1,    -1,    -1,    -1,    22,    23,    24,    -1,    -1,
  -1,    -1,    -1,    -1,    -1,    -1,    -1,    34,    35,    36,
  37,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
  -1,    -1,    49,    -1,    51,     1,    53,    -1,    55,    56,
  57,    -1,    59,    -1,    10,    -1,    -1,    -1,    -1,    15,
  -1,    -1,    -1,    -1,    -1,    -1,    22,    23,    24,    -1,
  -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    34,    35,
  36,    37,     1,    39,    -1,    -1,    -1,    -1,    -1,    45,
  46,    10,    -1,    49,    -1,    51,    15,    53,    -1,    55,
  56,    57,    -1,    59,    23,    24,    -1,    -1,    -1,    -1,
  -1,    -1,    -1,    -1,    -1,    34,    35,    36,    37,     1,
  39,    -1,    -1,    -1,    -1,    -1,    45,    46,    10,    -1,
  49,    -1,    51,    15,    53,    -1,    55,    56,    57,    -1,
  59,    23,    24,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
  -1,    -1,    34,    35,    36,    37,    -1,    39,    -1,    -1,
  -1,    -1,    -1,    45,    46,    -1,    -1,    49,    -1,    51,
  -1,    53,    -1,    -1,    56,    57,    -1,    59
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
 symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
  0,     1,    10,    15,    22,    23,    24,    34,    35,    36,
  37,    39,    45,    46,    49,    51,    53,    55,    56,    57,
  59,    87,    88,    89,    91,    92,    93,    96,    97,    98,
  99,   100,   101,   102,   104,   106,   145,   146,   171,    62,
  92,    32,   105,   171,   173,    92,     0,    88,    62,    94,
  95,   112,   113,   114,   115,   173,    46,    97,    98,    99,
  100,   101,    92,    92,    92,    92,    92,    73,   103,   155,
  103,    13,    18,    19,    25,    26,    27,    28,    33,    38,
  42,    47,    48,    60,    62,    65,    72,    75,    76,    77,
  78,    89,    91,    96,   123,   124,   125,   126,   130,   131,
  132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
  142,   143,   144,   147,   148,   149,   150,   151,   152,   153,
  154,   155,   157,   158,   159,   160,   161,   162,   167,   169,
  172,   173,   174,    66,    62,    63,    64,    73,    67,   109,
  66,    69,    72,    90,    91,    92,   117,   118,   119,   120,
  89,   107,   108,   107,    62,    96,   126,   144,   147,    72,
  72,    72,   130,   130,    62,   144,    72,    90,    92,   144,
  130,   130,   130,   130,   112,    72,     5,     6,     7,     8,
  9,    42,    47,    64,    69,    72,    74,    79,    80,    81,
  75,    76,    30,    41,    29,    40,    67,    68,    20,    43,
  82,    83,    84,     3,    44,    85,   147,   148,    65,   156,
  157,    65,   158,    62,    14,   170,   173,    95,    73,   121,
  144,   110,   170,    38,    70,   112,   116,    63,    71,    71,
  63,    65,   108,    65,    62,    60,   160,   164,   165,   144,
  163,    62,   163,    71,    71,   129,   144,   144,   144,   144,
  144,   144,   144,   144,   127,   128,   144,   168,   173,   131,
  131,   131,   132,   132,   133,   133,   134,   134,   134,   134,
  135,   135,   136,   137,   138,   139,   140,   144,    21,   156,
  124,   173,   121,   122,   111,    70,    64,    69,   118,    90,
  72,    62,    63,    71,    71,   131,    63,    71,    70,    71,
  63,    66,   148,   149,    63,    65,    68,    89,   121,    38,
  70,   163,   163,   166,   160,   148,   149,   144,   144,   142,
  65,   121,    70,    71,    62,    62,   164,    71,   148,   149
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
 to ease the transition to the new meaning of YYERROR, for GCC.
 Once GCC version 2 has supplanted version 1, this can go.  However,
 YYFAIL appears to be in use.  Nevertheless, it is formally deprecated
 in Bison 2.4.2's NEWS entry, where a plan to phase it out is
 discussed.  */

#define YYFAIL		goto yyerrlab
#if defined YYFAIL
/* This is here to suppress warnings from the GCC cpp's
 -Wunused-macros.  Normally we don't worry about that warning, but
 some users do, and we want to make it easy for users to remove
 YYFAIL uses, which will produce warnings from Bison 2.5.  */
#endif

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
if (yychar == YYEMPTY && yylen == 1)				\
{								\
yychar = (Token);						\
yylval = (Value);						\
yytoken = YYTRANSLATE (yychar);				\
YYPOPSTACK (1);						\
goto yybackup;						\
}								\
else								\
{								\
yyerror (cg, YY_("syntax error: cannot back up")); \
YYERROR;							\
}								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
 If N is 0, then set CURRENT to the empty location which ends
 the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
do									\
if (YYID (N))                                                    \
{								\
(Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
(Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
(Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
(Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
}								\
else								\
{								\
(Current).first_line   = (Current).last_line   =		\
YYRHSLOC (Rhs, 0).last_line;				\
(Current).first_column = (Current).last_column =		\
YYRHSLOC (Rhs, 0).last_column;				\
}								\
while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
 This macro was not mandated originally: define only if we know
 we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
fprintf (File, "%d.%d-%d.%d",			\
(Loc).first_line, (Loc).first_column,	\
(Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (&yylval, YYLEX_PARAM)
#else
# define YYLEX yylex (&yylval, cg)
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
if (yydebug)					\
YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
if (yydebug)								  \
{									  \
YYFPRINTF (stderr, "%s ", Title);					  \
yy_symbol_print (stderr,						  \
Type, Value, cg); \
YYFPRINTF (stderr, "\n");						  \
}									  \
} while (YYID (0))


/*--------------------------------.
 | Print this symbol on YYOUTPUT.  |
 `--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
|| defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, CgContext *cg)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep, cg)
FILE *yyoutput;
int yytype;
YYSTYPE const * const yyvaluep;
CgContext *cg;
#endif
{
  if (!yyvaluep)
    return;
  YYUSE (cg);
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
    YYUSE (yyoutput);
# endif
    switch (yytype)
  {
    default:
      break;
  }
}


/*--------------------------------.
 | Print this symbol on YYOUTPUT.  |
 `--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
|| defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, CgContext *cg)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep, cg)
FILE *yyoutput;
int yytype;
YYSTYPE const * const yyvaluep;
CgContext *cg;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
    else
      YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);
      
      yy_symbol_value_print (yyoutput, yytype, yyvaluep, cg);
      YYFPRINTF (yyoutput, ")");
      }

/*------------------------------------------------------------------.
 | yy_stack_print -- Print the state stack from its BOTTOM up to its |
 | TOP (included).                                                   |
 `------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
|| defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
yytype_int16 *yybottom;
yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
  {
    int yybot = *yybottom;
    YYFPRINTF (stderr, " %d", yybot);
  }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
if (yydebug)							\
yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
 | Report that the YYRULE is going to be reduced.  |
 `------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
|| defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule, CgContext *cg)
#else
static void
yy_reduce_print (yyvsp, yyrule, cg)
YYSTYPE *yyvsp;
int yyrule;
CgContext *cg;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
  {
    YYFPRINTF (stderr, "   $%d = ", yyi + 1);
    yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
                     &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       , cg);
    YYFPRINTF (stderr, "\n");
  }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
if (yydebug)				\
yy_reduce_print (yyvsp, Rule, cg); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
 multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
 if the built-in stack extension method is used).
 
 Do not make this value too large; the results are undefined if
 YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
 evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
|| defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
 YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
|| defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
char *yydest;
const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;
  
  while ((*yyd++ = *yys++) != '\0')
    continue;
  
  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
 quotes and backslashes, so that it's suitable for yyerror.  The
 heuristic is that double-quoting is unnecessary unless the string
 contains an apostrophe, a comma, or backslash (other than
 backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
 null, do not copy; instead, return the length of what the result
 would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
  {
    YYSIZE_T yyn = 0;
    char const *yyp = yystr;
    
    for (;;)
      switch (*++yyp)
	  {
      case '\'':
      case ',':
        goto do_not_strip_quotes;
        
      case '\\':
        if (*++yyp != '\\')
          goto do_not_strip_quotes;
        /* Fall through.  */
      default:
        if (yyres)
          yyres[yyn] = *yyp;
        yyn++;
        break;
        
      case '"':
        if (yyres)
          yyres[yyn] = '\0';
        return yyn;
	  }
  do_not_strip_quotes: ;
  }
  
  if (! yyres)
    return yystrlen (yystr);
  
  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
 YYCHAR while in state YYSTATE.  Return the number of bytes copied,
 including the terminating null byte.  If YYRESULT is null, do not
 copy anything; just return the number of bytes that would be
 copied.  As a special case, return 0 if an ordinary "syntax error"
 message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
 size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];
  
  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
  {
    int yytype = YYTRANSLATE (yychar);
    YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
    YYSIZE_T yysize = yysize0;
    YYSIZE_T yysize1;
    int yysize_overflow = 0;
    enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
    char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
    int yyx;
    
# if 0
    /* This is so xgettext sees the translatable formats that are
     constructed on the fly.  */
    YY_("syntax error, unexpected %s");
    YY_("syntax error, unexpected %s, expecting %s");
    YY_("syntax error, unexpected %s, expecting %s or %s");
    YY_("syntax error, unexpected %s, expecting %s or %s or %s");
    YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
    char *yyfmt;
    char const *yyf;
    static char const yyunexpected[] = "syntax error, unexpected %s";
    static char const yyexpecting[] = ", expecting %s";
    static char const yyor[] = " or %s";
    char yyformat[sizeof yyunexpected
                  + sizeof yyexpecting - 1
                  + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
                     * (sizeof yyor - 1))];
    char const *yyprefix = yyexpecting;
    
    /* Start YYX at -YYN if negative to avoid negative indexes in
     YYCHECK.  */
    int yyxbegin = yyn < 0 ? -yyn : 0;
    
    /* Stay within bounds of both yycheck and yytname.  */
    int yychecklim = YYLAST - yyn + 1;
    int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
    int yycount = 1;
    
    yyarg[0] = yytname[yytype];
    yyfmt = yystpcpy (yyformat, yyunexpected);
    
    for (yyx = yyxbegin; yyx < yyxend; ++yyx)
      if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
      {
        if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
          yycount = 1;
          yysize = yysize0;
          yyformat[sizeof yyunexpected - 1] = '\0';
          break;
	      }
        yyarg[yycount++] = yytname[yyx];
        yysize1 = yysize + yytnamerr (0, yytname[yyx]);
        yysize_overflow |= (yysize1 < yysize);
        yysize = yysize1;
        yyfmt = yystpcpy (yyfmt, yyprefix);
        yyprefix = yyor;
      }
    
    yyf = YY_(yyformat);
    yysize1 = yysize + yystrlen (yyf);
    yysize_overflow |= (yysize1 < yysize);
    yysize = yysize1;
    
    if (yysize_overflow)
      return YYSIZE_MAXIMUM;
    
    if (yyresult)
    {
      /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
      char *yyp = yyresult;
      int yyi = 0;
      while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyf += 2;
        }
	      else
        {
          yyp++;
          yyf++;
        }
	    }
    }
    return yysize;
  }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
 | Release the memory associated to this symbol.  |
 `-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
|| defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, CgContext *cg)
#else
static void
yydestruct (yymsg, yytype, yyvaluep, cg)
const char *yymsg;
int yytype;
YYSTYPE *yyvaluep;
CgContext *cg;
#endif
{
  YYUSE (yyvaluep);
  YYUSE (cg);
  
  if (!yymsg)
    yymsg = "Deleting";
    YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);
    
    switch (yytype)
  {
      
    default:
      break;
  }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (CgContext *cg);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */





/*-------------------------.
 | yyparse or yypush_parse.  |
 `-------------------------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
|| defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
|| defined __cplusplus || defined _MSC_VER)
int
yyparse (CgContext *cg)
#else
int
yyparse (cg)
CgContext *cg;
#endif
#endif
{
  /* The lookahead symbol.  */
  int yychar;
  
  /* The semantic value of the lookahead symbol.  */
  YYSTYPE yylval;
  
  /* Number of syntax errors so far.  */
  int yynerrs;
  
  int yystate;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  
  /* The stacks and their tools:
   `yyss': related to states.
   `yyvs': related to semantic values.
   
   Refer to the stacks thru separate pointers, to allow yyoverflow
   to reallocate them elsewhere.  */
  
  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss;
  yytype_int16 *yyssp;
  
  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs;
  YYSTYPE *yyvsp;
  
  YYSIZE_T yystacksize;
  
  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
   action routines.  */
  YYSTYPE yyval;
  
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif
  
#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))
  
  /* The number of symbols on the RHS of the reduced rule.
   Keep to zero when no symbol should be popped.  */
  int yylen = 0;
  
  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;
  
  YYDPRINTF ((stderr, "Starting parse\n"));
  
  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  
  /* Initialize stack pointers.
   Waste one element of value and location stack
   so that they stay on the same level as the state stack.
   The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;
  
  goto yysetstate;
  
  /*------------------------------------------------------------.
   | yynewstate -- Push a new state, which is found in yystate.  |
   `------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
   have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;
  
yysetstate:
  *yyssp = yystate;
  
  if (yyss + yystacksize - 1 <= yyssp)
  {
    /* Get the current used size of the three stacks, in elements.  */
    YYSIZE_T yysize = yyssp - yyss + 1;
    
#ifdef yyoverflow
    {
      /* Give user a chance to reallocate the stack.  Use copies of
       these so that the &'s don't force the real ones into
       memory.  */
      YYSTYPE *yyvs1 = yyvs;
      yytype_int16 *yyss1 = yyss;
      
      /* Each stack pointer address is followed by the size of the
       data in use in that stack, in bytes.  This used to be a
       conditional around just the two extra args, but that might
       be undefined if yyoverflow is a macro.  */
      yyoverflow (YY_("memory exhausted"),
                  &yyss1, yysize * sizeof (*yyssp),
                  &yyvs1, yysize * sizeof (*yyvsp),
                  &yystacksize);
      
      yyss = yyss1;
      yyvs = yyvs1;
    }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
    goto yyexhaustedlab;
# else
    /* Extend the stack our own way.  */
    if (YYMAXDEPTH <= yystacksize)
      goto yyexhaustedlab;
    yystacksize *= 2;
    if (YYMAXDEPTH < yystacksize)
      yystacksize = YYMAXDEPTH;
      
    {
      yytype_int16 *yyss1 = yyss;
      union yyalloc *yyptr =
      (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
      if (! yyptr)
        goto yyexhaustedlab;
      YYSTACK_RELOCATE (yyss_alloc, yyss);
      YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
      if (yyss1 != yyssa)
        YYSTACK_FREE (yyss1);
        }
# endif
#endif /* no yyoverflow */
    
    yyssp = yyss + yysize - 1;
    yyvsp = yyvs + yysize - 1;
    
    YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                (unsigned long int) yystacksize));
    
    if (yyss + yystacksize - 1 <= yyssp)
      YYABORT;
  }
  
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  
  if (yystate == YYFINAL)
    YYACCEPT;
  
  goto yybackup;
  
  /*-----------.
   | yybackup.  |
   `-----------*/
yybackup:
  
  /* Do appropriate processing given the current state.  Read a
   lookahead token if we need one and don't already have one.  */
  
  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;
  
  /* Not known => get a lookahead token if don't already have one.  */
  
  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
  {
    YYDPRINTF ((stderr, "Reading a token: "));
    yychar = YYLEX;
  }
  
  if (yychar <= YYEOF)
  {
    yychar = yytoken = YYEOF;
    YYDPRINTF ((stderr, "Now at end of input.\n"));
  }
  else
  {
    yytoken = YYTRANSLATE (yychar);
    YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
  }
  
  /* If the proper action on seeing token YYTOKEN is to reduce or to
   detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
  {
    if (yyn == 0 || yyn == YYTABLE_NINF)
      goto yyerrlab;
    yyn = -yyn;
    goto yyreduce;
  }
  
  /* Count tokens shifted since error; after three, turn off error
   status.  */
  if (yyerrstatus)
    yyerrstatus--;
  
  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  
  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  
  yystate = yyn;
  *++yyvsp = yylval;
  
  goto yynewstate;
  
  
  /*-----------------------------------------------------------.
   | yydefault -- do the default action for the current state.  |
   `-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;
  
  
  /*-----------------------------.
   | yyreduce -- Do a reduction.  |
   `-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];
  
  /* If YYLEN is nonzero, implement the default value of the action:
   `$$ = $1'.
   
   Otherwise, the following line sets YYVAL to garbage.
   This behavior is undocumented and Bison
   users should not rely upon it.  Assigning to YYVAL
   unconditionally makes the parser a bit smaller, and it avoids a
   GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];
  
  
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
  {
    case 4:
      
      /* Line 1464 of yacc.c  */
#line 243 "parser.y"
    { (yyval.dummy) = GlobalInitStatements( cg, cg->currentScope, (yyvsp[(1) - (1)].sc_stmt)); ;}
      break;
      
    case 6:
      
      /* Line 1464 of yacc.c  */
#line 248 "parser.y"
    { (yyval.sc_stmt) = NULL; ;}
      break;
      
    case 7:
      
      /* Line 1464 of yacc.c  */
#line 250 "parser.y"
    { (yyval.sc_stmt) = (yyvsp[(2) - (3)].sc_stmt); ;}
      break;
      
    case 8:
      
      /* Line 1464 of yacc.c  */
#line 252 "parser.y"
    { RecordErrorPos( cg, cg->tokenLoc); (yyval.sc_stmt) = NULL; ;}
      break;
      
    case 9:
      
      /* Line 1464 of yacc.c  */
#line 256 "parser.y"
    { (yyval.sc_decl) = (yyvsp[(2) - (2)].sc_decl); ;}
      break;
      
    case 10:
      
      /* Line 1464 of yacc.c  */
#line 263 "parser.y"
    { (yyval.sc_type) = (yyvsp[(1) - (1)].sc_type); ;}
      break;
      
    case 11:
      
      /* Line 1464 of yacc.c  */
#line 265 "parser.y"
    { SetTypeMisc( cg, cg->tokenLoc, &cg->currentDeclTypeSpecs, TYPE_MISC_TYPEDEF); (yyval.sc_type) = (yyvsp[(2) - (2)].sc_type); ;}
      break;
      
    case 12:
      
      /* Line 1464 of yacc.c  */
#line 270 "parser.y"
    { (yyval.sc_type) = (yyvsp[(1) - (1)].sc_type); ;}
      break;
      
    case 13:
      
      /* Line 1464 of yacc.c  */
#line 272 "parser.y"
    { SetTypeQualifiers( cg, cg->tokenLoc, &cg->currentDeclTypeSpecs, TQ_None, true); (yyval.sc_type) = cg->currentDeclTypeSpecs; ;}
      break;
      
    case 14:
      
      /* Line 1464 of yacc.c  */
#line 274 "parser.y"
    { SetStorageClass( cg, cg->tokenLoc, &cg->currentDeclTypeSpecs, (yyvsp[(1) - (2)].sc_int)); (yyval.sc_type) = cg->currentDeclTypeSpecs; ;}
      break;
      
    case 15:
      
      /* Line 1464 of yacc.c  */
#line 276 "parser.y"
    { SetTypeDomain( cg, cg->tokenLoc, &cg->currentDeclTypeSpecs, (TypeDomain)(yyvsp[(1) - (2)].sc_int)); (yyval.sc_type) = cg->currentDeclTypeSpecs; ;}
      break;
      
    case 16:
      
      /* Line 1464 of yacc.c  */
#line 278 "parser.y"
    { SetTypeQualifiers( cg, cg->tokenLoc, &cg->currentDeclTypeSpecs, (TypeQualifier)(yyvsp[(1) - (2)].sc_int), false); (yyval.sc_type) = cg->currentDeclTypeSpecs; ;}
      break;
      
    case 17:
      
      /* Line 1464 of yacc.c  */
#line 280 "parser.y"
    { SetTypeMisc( cg, cg->tokenLoc, &cg->currentDeclTypeSpecs, (yyvsp[(1) - (2)].sc_int)); (yyval.sc_type) = cg->currentDeclTypeSpecs; ;}
      break;
      
    case 18:
      
      /* Line 1464 of yacc.c  */
#line 282 "parser.y"
    { SetTypePacked( cg, cg->tokenLoc, &cg->currentDeclTypeSpecs); (yyval.sc_type) = cg->currentDeclTypeSpecs; ;}
      break;
      
    case 19:
      
      /* Line 1464 of yacc.c  */
#line 287 "parser.y"
    { (yyval.sc_type) = *SetDType(&cg->currentDeclTypeSpecs, (yyvsp[(1) - (1)].sc_ptype)); ;}
      break;
      
    case 20:
      
      /* Line 1464 of yacc.c  */
#line 289 "parser.y"
    { SetTypeQualifiers( cg, cg->tokenLoc, &cg->currentDeclTypeSpecs, TQ_None, true); (yyval.sc_type) = cg->currentDeclTypeSpecs; ;}
      break;
      
    case 21:
      
      /* Line 1464 of yacc.c  */
#line 291 "parser.y"
    { SetStorageClass( cg, cg->tokenLoc, &cg->currentDeclTypeSpecs, (yyvsp[(2) - (2)].sc_int)); (yyval.sc_type) = cg->currentDeclTypeSpecs; ;}
      break;
      
    case 22:
      
      /* Line 1464 of yacc.c  */
#line 293 "parser.y"
    { SetTypeDomain( cg, cg->tokenLoc, &cg->currentDeclTypeSpecs, (TypeDomain)(yyvsp[(2) - (2)].sc_int)); (yyval.sc_type) = cg->currentDeclTypeSpecs; ;}
      break;
      
    case 23:
      
      /* Line 1464 of yacc.c  */
#line 295 "parser.y"
    { SetTypeQualifiers( cg, cg->tokenLoc, &cg->currentDeclTypeSpecs, (TypeQualifier)(yyvsp[(2) - (2)].sc_int), false); (yyval.sc_type) = cg->currentDeclTypeSpecs; ;}
      break;
      
    case 24:
      
      /* Line 1464 of yacc.c  */
#line 297 "parser.y"
    { SetTypeMisc( cg, cg->tokenLoc, &cg->currentDeclTypeSpecs, (yyvsp[(2) - (2)].sc_int)); (yyval.sc_type) = cg->currentDeclTypeSpecs; ;}
      break;
      
    case 25:
      
      /* Line 1464 of yacc.c  */
#line 299 "parser.y"
    { SetTypePacked( cg, cg->tokenLoc, &cg->currentDeclTypeSpecs); (yyval.sc_type) = cg->currentDeclTypeSpecs; ;}
      break;
      
    case 26:
      
      /* Line 1464 of yacc.c  */
#line 303 "parser.y"
    { (yyval.sc_stmt) = (yyvsp[(1) - (1)].sc_stmt); ;}
      break;
      
    case 27:
      
      /* Line 1464 of yacc.c  */
#line 305 "parser.y"
    { (yyval.sc_stmt) = AddStmt( cg, (yyvsp[(1) - (3)].sc_stmt), (yyvsp[(3) - (3)].sc_stmt)); ;}
      break;
      
    case 28:
      
      /* Line 1464 of yacc.c  */
#line 309 "parser.y"
    { (yyval.sc_stmt) = Init_Declarator( cg, cg->tokenLoc, cg->currentScope, (yyvsp[(1) - (1)].sc_decl), NULL); ;}
      break;
      
    case 29:
      
      /* Line 1464 of yacc.c  */
#line 311 "parser.y"
    { (yyval.sc_stmt) = Init_Declarator( cg, cg->tokenLoc, cg->currentScope, (yyvsp[(1) - (3)].sc_decl), (yyvsp[(3) - (3)].sc_expr)); ;}
      break;
      
    case 30:
      
      /* Line 1464 of yacc.c  */
#line 319 "parser.y"
    { (yyval.sc_ptype) = LookupTypeSymbol( cg, NULL, INT_SY); ;}
      break;
      
    case 31:
      
      /* Line 1464 of yacc.c  */
#line 321 "parser.y"
    { (yyval.sc_ptype) = LookupTypeSymbol( cg, NULL, FLOAT_SY); ;}
      break;
      
    case 32:
      
      /* Line 1464 of yacc.c  */
#line 323 "parser.y"
    { (yyval.sc_ptype) = LookupTypeSymbol( cg, NULL, VOID_SY); ;}
      break;
      
    case 33:
      
      /* Line 1464 of yacc.c  */
#line 325 "parser.y"
    { (yyval.sc_ptype) = LookupTypeSymbol( cg, NULL, BOOLEAN_SY); ;}
      break;
      
    case 34:
      
      /* Line 1464 of yacc.c  */
#line 327 "parser.y"
    { (yyval.sc_ptype) = LookupTypeSymbol( cg, NULL, TEXOBJ_SY); ;}
      break;
      
    case 35:
      
      /* Line 1464 of yacc.c  */
#line 329 "parser.y"
    { (yyval.sc_ptype) = (yyvsp[(1) - (1)].sc_ptype); ;}
      break;
      
    case 36:
      
      /* Line 1464 of yacc.c  */
#line 331 "parser.y"
    { (yyval.sc_ptype) = LookupTypeSymbol( cg, NULL, (yyvsp[(1) - (1)].sc_ident)); ;}
      break;
      
    case 37:
      
      /* Line 1464 of yacc.c  */
#line 333 "parser.y"
    {
      
      SemanticParseError( cg, cg->tokenLoc, ERROR_S_TYPE_NAME_EXPECTED,
                         cg->GetString( cg->mostRecentToken /* yychar */));
      (yyval.sc_ptype) = cg->UndefinedType;
      ;}
      break;
      
    case 38:
      
      /* Line 1464 of yacc.c  */
#line 346 "parser.y"
    { (yyval.sc_int) = 1; ;}
      break;
      
    case 39:
      
      /* Line 1464 of yacc.c  */
#line 354 "parser.y"
    { (yyval.sc_int) = TD_Uniform; ;}
      break;
      
    case 40:
      
      /* Line 1464 of yacc.c  */
#line 362 "parser.y"
    { (yyval.sc_int) = (int) SC_Static; ;}
      break;
      
    case 41:
      
      /* Line 1464 of yacc.c  */
#line 364 "parser.y"
    { (yyval.sc_int) = (int) SC_Extern; ;}
      break;
      
    case 42:
      
      /* Line 1464 of yacc.c  */
#line 372 "parser.y"
    { (yyval.sc_int) = TYPE_MISC_INLINE; ;}
      break;
      
    case 43:
      
      /* Line 1464 of yacc.c  */
#line 374 "parser.y"
    { (yyval.sc_int) = TYPE_MISC_INTERNAL; ;}
      break;
      
    case 44:
      
      /* Line 1464 of yacc.c  */
#line 382 "parser.y"
    { (yyval.sc_int) = TQ_In; ;}
      break;
      
    case 45:
      
      /* Line 1464 of yacc.c  */
#line 384 "parser.y"
    { (yyval.sc_int) = TQ_Out; ;}
      break;
      
    case 46:
      
      /* Line 1464 of yacc.c  */
#line 386 "parser.y"
    { (yyval.sc_int) = TQ_InOut; ;}
      break;
      
    case 47:
      
      /* Line 1464 of yacc.c  */
#line 395 "parser.y"
    { (yyval.sc_ptype) = SetStructMembers( cg, cg->tokenLoc, (yyvsp[(1) - (4)].sc_ptype), PopScope( cg )); ;}
      break;
      
    case 48:
      
      /* Line 1464 of yacc.c  */
#line 397 "parser.y"
    { (yyval.sc_ptype) = SetStructMembers( cg, cg->tokenLoc, (yyvsp[(1) - (4)].sc_ptype), PopScope( cg )); ;}
      break;
      
    case 49:
      
      /* Line 1464 of yacc.c  */
#line 399 "parser.y"
    { (yyval.sc_ptype) = (yyvsp[(1) - (1)].sc_ptype); ;}
      break;
      
    case 50:
      
      /* Line 1464 of yacc.c  */
#line 403 "parser.y"
    { cg->currentScope->IsStructScope = 1; (yyval.dummy) = (yyvsp[(1) - (1)].dummy); ;}
      break;
      
    case 51:
      
      /* Line 1464 of yacc.c  */
#line 408 "parser.y"
    { (yyval.sc_ptype) = StructHeader( cg, cg->tokenLoc, cg->currentScope, 0, (yyvsp[(2) - (2)].sc_ident)); ;}
      break;
      
    case 52:
      
      /* Line 1464 of yacc.c  */
#line 410 "parser.y"
    { (yyval.sc_ptype) = StructHeader( cg, cg->tokenLoc, cg->currentScope, (yyvsp[(4) - (4)].sc_ident), (yyvsp[(2) - (4)].sc_ident)); ;}
      break;
      
    case 55:
      
      /* Line 1464 of yacc.c  */
#line 418 "parser.y"
    { (yyval.sc_ptype) = StructHeader( cg, cg->tokenLoc, cg->currentScope, 0, 0); ;}
      break;
      
    case 58:
      
      /* Line 1464 of yacc.c  */
#line 426 "parser.y"
    { (yyval.sc_stmt) = (yyvsp[(1) - (1)].sc_stmt); ;}
      break;
      
    case 59:
      
      /* Line 1464 of yacc.c  */
#line 444 "parser.y"
    { PushScope( cg, new Scope( cg )); ;}
      break;
      
    case 60:
      
      /* Line 1464 of yacc.c  */
#line 445 "parser.y"
    { (yyval.sc_stmt) = (yyvsp[(3) - (4)].sc_stmt); PopScope( cg ); ;}
      break;
      
    case 61:
      
      /* Line 1464 of yacc.c  */
#line 449 "parser.y"
    { (yyval.sc_stmt) = 0; ;}
      break;
      
    case 63:
      
      /* Line 1464 of yacc.c  */
#line 458 "parser.y"
    { (yyval.sc_decl) = (yyvsp[(1) - (1)].sc_decl); ;}
      break;
      
    case 64:
      
      /* Line 1464 of yacc.c  */
#line 460 "parser.y"
    { (yyval.sc_decl) = (yyvsp[(1) - (2)].sc_decl); ;}
      break;
      
    case 65:
      
      /* Line 1464 of yacc.c  */
#line 464 "parser.y"
    { (yyval.sc_decl) = Declarator( cg, cg->tokenLoc, (yyvsp[(1) - (1)].sc_decl), 0); ;}
      break;
      
    case 66:
      
      /* Line 1464 of yacc.c  */
#line 466 "parser.y"
    { (yyval.sc_decl) = Declarator( cg, cg->tokenLoc, (yyvsp[(1) - (3)].sc_decl), (yyvsp[(3) - (3)].sc_ident)); ;}
      break;
      
    case 67:
      
      /* Line 1464 of yacc.c  */
#line 470 "parser.y"
    { (yyval.sc_decl) = NewDeclNode( cg, cg->tokenLoc, (yyvsp[(1) - (1)].sc_ident), &cg->currentDeclTypeSpecs); ;}
      break;
      
    case 68:
      
      /* Line 1464 of yacc.c  */
#line 472 "parser.y"
    { (yyval.sc_decl) = Array_Declarator( cg, cg->tokenLoc, (yyvsp[(1) - (4)].sc_decl), (yyvsp[(3) - (4)].sc_int), 0); ;}
      break;
      
    case 69:
      
      /* Line 1464 of yacc.c  */
#line 474 "parser.y"
    { (yyval.sc_decl) = Array_Declarator( cg, cg->tokenLoc, (yyvsp[(1) - (3)].sc_decl), 0 , 1); ;}
      break;
      
    case 70:
      
      /* Line 1464 of yacc.c  */
#line 476 "parser.y"
    { (yyval.sc_decl) = SetFunTypeParams( cg, cg->currentScope, (yyvsp[(1) - (3)].sc_decl), (yyvsp[(2) - (3)].sc_decl), (yyvsp[(2) - (3)].sc_decl)); ;}
      break;
      
    case 71:
      
      /* Line 1464 of yacc.c  */
#line 478 "parser.y"
    { (yyval.sc_decl) = SetFunTypeParams( cg, cg->currentScope, (yyvsp[(1) - (3)].sc_decl), (yyvsp[(2) - (3)].sc_decl), NULL); ;}
      break;
      
    case 72:
      
      /* Line 1464 of yacc.c  */
#line 482 "parser.y"
    { (yyval.sc_decl) = FunctionDeclHeader( cg, &(yyvsp[(1) - (2)].sc_decl)->loc, cg->currentScope, (yyvsp[(1) - (2)].sc_decl)); ;}
      break;
      
    case 73:
      
      /* Line 1464 of yacc.c  */
#line 486 "parser.y"
    { (yyval.sc_decl) = NewDeclNode( cg, cg->tokenLoc, 0, &cg->currentDeclTypeSpecs); ;}
      break;
      
    case 74:
      
      /* Line 1464 of yacc.c  */
#line 488 "parser.y"
    { (yyval.sc_decl) = Array_Declarator( cg, cg->tokenLoc, (yyvsp[(1) - (4)].sc_decl), (yyvsp[(3) - (4)].sc_int), 0); ;}
      break;
      
    case 75:
      
      /* Line 1464 of yacc.c  */
#line 490 "parser.y"
    { (yyval.sc_decl) = Array_Declarator( cg, cg->tokenLoc, (yyvsp[(1) - (3)].sc_decl), 0 , 1); ;}
      break;
      
    case 76:
      
      /* Line 1464 of yacc.c  */
#line 507 "parser.y"
    { (yyval.sc_decl) = (yyvsp[(1) - (1)].sc_decl); ;}
      break;
      
    case 77:
      
      /* Line 1464 of yacc.c  */
#line 509 "parser.y"
    { (yyval.sc_decl) = AddDecl( cg, (yyvsp[(1) - (3)].sc_decl), (yyvsp[(3) - (3)].sc_decl)); ;}
      break;
      
    case 78:
      
      /* Line 1464 of yacc.c  */
#line 513 "parser.y"
    { (yyval.sc_decl) = Param_Init_Declarator( cg, cg->tokenLoc, cg->currentScope, (yyvsp[(2) - (2)].sc_decl), NULL); ;}
      break;
      
    case 79:
      
      /* Line 1464 of yacc.c  */
#line 515 "parser.y"
    { (yyval.sc_decl) = Param_Init_Declarator( cg, cg->tokenLoc, cg->currentScope, (yyvsp[(2) - (4)].sc_decl), (yyvsp[(4) - (4)].sc_expr)); ;}
      break;
      
    case 80:
      
      /* Line 1464 of yacc.c  */
#line 519 "parser.y"
    { (yyval.sc_decl) = NULL; ;}
      break;
      
    case 82:
      
      /* Line 1464 of yacc.c  */
#line 524 "parser.y"
    {
      if (IsVoid( (yyvsp[(1) - (1)].sc_decl)->type.GetType() ))
        cg->currentScope->HasVoidParameter = 1;
        (yyval.sc_decl) = (yyvsp[(1) - (1)].sc_decl);
        ;}
      break;
      
    case 83:
      
      /* Line 1464 of yacc.c  */
#line 530 "parser.y"
    {
      if (cg->currentScope->HasVoidParameter || IsVoid( (yyvsp[(1) - (3)].sc_decl)->type.GetType() )) {
        SemanticError( cg, cg->tokenLoc, ERROR___VOID_NOT_ONLY_PARAM);
      }
      (yyval.sc_decl) = AddDecl( cg, (yyvsp[(1) - (3)].sc_decl), (yyvsp[(3) - (3)].sc_decl));
      ;}
      break;
      
    case 84:
      
      /* Line 1464 of yacc.c  */
#line 543 "parser.y"
    { (yyval.sc_expr) = Initializer( cg, cg->tokenLoc, (yyvsp[(1) - (1)].sc_expr)); ;}
      break;
      
    case 85:
      
      /* Line 1464 of yacc.c  */
#line 545 "parser.y"
    { (yyval.sc_expr) = Initializer( cg, cg->tokenLoc, (yyvsp[(2) - (3)].sc_expr)); ;}
      break;
      
    case 86:
      
      /* Line 1464 of yacc.c  */
#line 547 "parser.y"
    { (yyval.sc_expr) = Initializer( cg, cg->tokenLoc, (yyvsp[(2) - (4)].sc_expr)); ;}
      break;
      
    case 87:
      
      /* Line 1464 of yacc.c  */
#line 551 "parser.y"
    { (yyval.sc_expr) = InitializerList( cg, cg->tokenLoc, (yyvsp[(1) - (1)].sc_expr), NULL); ;}
      break;
      
    case 88:
      
      /* Line 1464 of yacc.c  */
#line 553 "parser.y"
    { (yyval.sc_expr) = InitializerList( cg, cg->tokenLoc, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr)); ;}
      break;
      
    case 89:
      
      /* Line 1464 of yacc.c  */
#line 565 "parser.y"
    { (yyval.sc_expr) = (yyvsp[(1) - (1)].sc_expr); ;}
      break;
      
    case 90:
      
      /* Line 1464 of yacc.c  */
#line 567 "parser.y"
    { (yyval.sc_expr) = (yyvsp[(3) - (3)].sc_expr); ;}
      break;
      
    case 91:
      
      /* Line 1464 of yacc.c  */
#line 571 "parser.y"
    { (yyval.sc_expr) = BasicVariable( cg, cg->tokenLoc, (yyvsp[(1) - (1)].sc_ident)); ;}
      break;
      
    case 94:
      
      /* Line 1464 of yacc.c  */
#line 581 "parser.y"
    { (yyval.sc_expr) = (yyvsp[(2) - (3)].sc_expr); ;}
      break;
      
    case 95:
      
      /* Line 1464 of yacc.c  */
#line 583 "parser.y"
    { (yyval.sc_expr) = NewVectorConstructor( cg, cg->tokenLoc, (yyvsp[(1) - (4)].sc_ptype), (yyvsp[(3) - (4)].sc_expr)); ;}
      break;
      
    case 97:
      
      /* Line 1464 of yacc.c  */
#line 592 "parser.y"
    { (yyval.sc_expr) = (Expr *) NewUnopNode( cg, POSTINC_OP, (yyvsp[(1) - (2)].sc_expr)); ;}
      break;
      
    case 98:
      
      /* Line 1464 of yacc.c  */
#line 594 "parser.y"
    { (yyval.sc_expr) = (Expr *) NewUnopNode( cg, POSTDEC_OP, (yyvsp[(1) - (2)].sc_expr)); ;}
      break;
      
    case 99:
      
      /* Line 1464 of yacc.c  */
#line 596 "parser.y"
    { (yyval.sc_expr) = NewMemberSelectorOrSwizzleOrWriteMaskOperator( cg, cg->tokenLoc, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_ident)); ;}
      break;
      
    case 100:
      
      /* Line 1464 of yacc.c  */
#line 598 "parser.y"
    { (yyval.sc_expr) = NewIndexOperator( cg, cg->tokenLoc, (yyvsp[(1) - (4)].sc_expr), (yyvsp[(3) - (4)].sc_expr)); ;}
      break;
      
    case 101:
      
      /* Line 1464 of yacc.c  */
#line 600 "parser.y"
    { (yyval.sc_expr) = NewFunctionCallOperator( cg, cg->tokenLoc, (yyvsp[(1) - (4)].sc_expr), (yyvsp[(3) - (4)].sc_expr)); ;}
      break;
      
    case 102:
      
      /* Line 1464 of yacc.c  */
#line 604 "parser.y"
    { (yyval.sc_expr) = NULL; ;}
      break;
      
    case 104:
      
      /* Line 1464 of yacc.c  */
#line 609 "parser.y"
    { (yyval.sc_expr) = ArgumentList( cg, cg->tokenLoc, NULL, (yyvsp[(1) - (1)].sc_expr)); ;}
      break;
      
    case 105:
      
      /* Line 1464 of yacc.c  */
#line 611 "parser.y"
    { (yyval.sc_expr) = ArgumentList( cg, cg->tokenLoc, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr)); ;}
      break;
      
    case 106:
      
      /* Line 1464 of yacc.c  */
#line 615 "parser.y"
    { (yyval.sc_expr) = ExpressionList( cg, cg->tokenLoc, NULL, (yyvsp[(1) - (1)].sc_expr)); ;}
      break;
      
    case 107:
      
      /* Line 1464 of yacc.c  */
#line 617 "parser.y"
    { (yyval.sc_expr) = ExpressionList( cg, cg->tokenLoc, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr)); ;}
      break;
      
    case 109:
      
      /* Line 1464 of yacc.c  */
#line 626 "parser.y"
    { (yyval.sc_expr) = (Expr *) NewUnopNode( cg, PREINC_OP, (yyvsp[(2) - (2)].sc_expr)); ;}
      break;
      
    case 110:
      
      /* Line 1464 of yacc.c  */
#line 628 "parser.y"
    { (yyval.sc_expr) = (Expr *) NewUnopNode( cg, PREDEC_OP, (yyvsp[(2) - (2)].sc_expr)); ;}
      break;
      
    case 111:
      
      /* Line 1464 of yacc.c  */
#line 630 "parser.y"
    { (yyval.sc_expr) = NewUnaryOperator( cg, cg->tokenLoc, POS_OP, '+', (yyvsp[(2) - (2)].sc_expr), 0); ;}
      break;
      
    case 112:
      
      /* Line 1464 of yacc.c  */
#line 632 "parser.y"
    { (yyval.sc_expr) = NewUnaryOperator( cg, cg->tokenLoc, NEG_OP, '-', (yyvsp[(2) - (2)].sc_expr), 0); ;}
      break;
      
    case 113:
      
      /* Line 1464 of yacc.c  */
#line 634 "parser.y"
    { (yyval.sc_expr) = NewUnaryOperator( cg, cg->tokenLoc, BNOT_OP, '!', (yyvsp[(2) - (2)].sc_expr), 0); ;}
      break;
      
    case 114:
      
      /* Line 1464 of yacc.c  */
#line 636 "parser.y"
    { (yyval.sc_expr) = NewUnaryOperator( cg, cg->tokenLoc, NOT_OP, '~', (yyvsp[(2) - (2)].sc_expr), 1); ;}
      break;
      
    case 116:
      
      /* Line 1464 of yacc.c  */
#line 648 "parser.y"
    { (yyval.sc_expr) = NewCastOperator( cg, cg->tokenLoc, (yyvsp[(4) - (4)].sc_expr), GetTypePointer( cg, &(yyvsp[(2) - (4)].sc_decl)->loc, &(yyvsp[(2) - (4)].sc_decl)->type)); ;}
      break;
      
    case 118:
      
      /* Line 1464 of yacc.c  */
#line 657 "parser.y"
    { (yyval.sc_expr) = NewBinaryOperator( cg, cg->tokenLoc, MUL_OP, '*', (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr), 0); ;}
      break;
      
    case 119:
      
      /* Line 1464 of yacc.c  */
#line 659 "parser.y"
    { (yyval.sc_expr) = NewBinaryOperator( cg, cg->tokenLoc, DIV_OP, '/', (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr), 0); ;}
      break;
      
    case 120:
      
      /* Line 1464 of yacc.c  */
#line 661 "parser.y"
    { (yyval.sc_expr) = NewBinaryOperator( cg, cg->tokenLoc, MOD_OP, '%', (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr), 1); ;}
      break;
      
    case 122:
      
      /* Line 1464 of yacc.c  */
#line 670 "parser.y"
    { (yyval.sc_expr) = NewBinaryOperator( cg, cg->tokenLoc, ADD_OP, '+', (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr), 0); ;}
      break;
      
    case 123:
      
      /* Line 1464 of yacc.c  */
#line 672 "parser.y"
    { (yyval.sc_expr) = NewBinaryOperator( cg, cg->tokenLoc, SUB_OP, '-', (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr), 0); ;}
      break;
      
    case 125:
      
      /* Line 1464 of yacc.c  */
#line 681 "parser.y"
    { (yyval.sc_expr) = NewBinaryOperator( cg, cg->tokenLoc, SHL_OP, LL_SY, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr), 1); ;}
      break;
      
    case 126:
      
      /* Line 1464 of yacc.c  */
#line 683 "parser.y"
    { (yyval.sc_expr) = NewBinaryOperator( cg, cg->tokenLoc, SHR_OP, GG_SY, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr), 1); ;}
      break;
      
    case 128:
      
      /* Line 1464 of yacc.c  */
#line 692 "parser.y"
    { (yyval.sc_expr) = NewBinaryComparisonOperator( cg, cg->tokenLoc, LT_OP, '<', (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr)); ;}
      break;
      
    case 129:
      
      /* Line 1464 of yacc.c  */
#line 694 "parser.y"
    { (yyval.sc_expr) = NewBinaryComparisonOperator( cg, cg->tokenLoc, GT_OP, '>', (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr)); ;}
      break;
      
    case 130:
      
      /* Line 1464 of yacc.c  */
#line 696 "parser.y"
    { (yyval.sc_expr) = NewBinaryComparisonOperator( cg, cg->tokenLoc, LE_OP, LE_SY, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr)); ;}
      break;
      
    case 131:
      
      /* Line 1464 of yacc.c  */
#line 698 "parser.y"
    { (yyval.sc_expr) = NewBinaryComparisonOperator( cg, cg->tokenLoc, GE_OP, GE_SY, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr)); ;}
      break;
      
    case 133:
      
      /* Line 1464 of yacc.c  */
#line 707 "parser.y"
    { (yyval.sc_expr) = NewBinaryComparisonOperator( cg, cg->tokenLoc, EQ_OP, EQ_SY, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr)); ;}
      break;
      
    case 134:
      
      /* Line 1464 of yacc.c  */
#line 709 "parser.y"
    { (yyval.sc_expr) = NewBinaryComparisonOperator( cg, cg->tokenLoc, NE_OP, NE_SY, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr)); ;}
      break;
      
    case 136:
      
      /* Line 1464 of yacc.c  */
#line 718 "parser.y"
    { (yyval.sc_expr) = NewBinaryOperator( cg, cg->tokenLoc, AND_OP, '&', (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr), 1); ;}
      break;
      
    case 138:
      
      /* Line 1464 of yacc.c  */
#line 727 "parser.y"
    { (yyval.sc_expr) = NewBinaryOperator( cg, cg->tokenLoc, XOR_OP, '^', (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr), 1); ;}
      break;
      
    case 140:
      
      /* Line 1464 of yacc.c  */
#line 736 "parser.y"
    { (yyval.sc_expr) = NewBinaryOperator( cg, cg->tokenLoc, OR_OP, '|', (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr), 1); ;}
      break;
      
    case 142:
      
      /* Line 1464 of yacc.c  */
#line 745 "parser.y"
    { (yyval.sc_expr) = NewBinaryBooleanOperator( cg, cg->tokenLoc, BAND_OP, AND_SY, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr)); ;}
      break;
      
    case 144:
      
      /* Line 1464 of yacc.c  */
#line 754 "parser.y"
    { (yyval.sc_expr) = NewBinaryBooleanOperator( cg, cg->tokenLoc, BOR_OP, OR_SY, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr)); ;}
      break;
      
    case 146:
      
      /* Line 1464 of yacc.c  */
#line 763 "parser.y"
    { (yyval.sc_expr) = NewConditionalOperator( cg, cg->tokenLoc, (yyvsp[(1) - (5)].sc_expr), (yyvsp[(3) - (5)].sc_expr), (yyvsp[(5) - (5)].sc_expr)); ;}
      break;
      
    case 147:
      
      /* Line 1464 of yacc.c  */
#line 767 "parser.y"
    {  (yyval.sc_expr) = CheckBooleanExpr( cg, cg->tokenLoc, (yyvsp[(1) - (1)].sc_expr), 1); ;}
      break;
      
    case 149:
      
      /* Line 1464 of yacc.c  */
#line 786 "parser.y"
    { DefineFunction( cg, cg->tokenLoc, cg->currentScope, (yyvsp[(1) - (3)].sc_decl), (yyvsp[(2) - (3)].sc_stmt)); PopScope( cg ); ;}
      break;
      
    case 150:
      
      /* Line 1464 of yacc.c  */
#line 788 "parser.y"
    { DefineFunction( cg, cg->tokenLoc, cg->currentScope, (yyvsp[(1) - (2)].sc_decl), NULL); PopScope( cg ); ;}
      break;
      
    case 151:
      
      /* Line 1464 of yacc.c  */
#line 792 "parser.y"
    { (yyval.sc_decl) = Function_Definition_Header( cg, cg->tokenLoc, (yyvsp[(2) - (3)].sc_decl)); ;}
      break;
      
    case 162:
      
      /* Line 1464 of yacc.c  */
#line 820 "parser.y"
    { (yyval.sc_stmt) = (Stmt *) NewDiscardStmt( cg, cg->tokenLoc, NULL); ;}
      break;
      
    case 163:
      
      /* Line 1464 of yacc.c  */
#line 822 "parser.y"
    { (yyval.sc_stmt) = (Stmt *) NewDiscardStmt( cg, cg->tokenLoc, CheckBooleanExpr( cg, cg->tokenLoc, (yyvsp[(2) - (3)].sc_expr), 1)); ;}
      break;
      
    case 164:
      
      /* Line 1464 of yacc.c  */
#line 830 "parser.y"
    { (yyval.sc_stmt) = (Stmt *) SetThenElseStmts( cg, cg->tokenLoc, (yyvsp[(1) - (4)].sc_stmt), (yyvsp[(2) - (4)].sc_stmt), (yyvsp[(4) - (4)].sc_stmt)); ;}
      break;
      
    case 165:
      
      /* Line 1464 of yacc.c  */
#line 834 "parser.y"
    { (yyval.sc_stmt) = (Stmt *) SetThenElseStmts( cg, cg->tokenLoc, (yyvsp[(1) - (2)].sc_stmt), (yyvsp[(2) - (2)].sc_stmt), NULL); ;}
      break;
      
    case 166:
      
      /* Line 1464 of yacc.c  */
#line 836 "parser.y"
    { (yyval.sc_stmt) = (Stmt *) SetThenElseStmts( cg, cg->tokenLoc, (yyvsp[(1) - (4)].sc_stmt), (yyvsp[(2) - (4)].sc_stmt), (yyvsp[(4) - (4)].sc_stmt)); ;}
      break;
      
    case 167:
      
      /* Line 1464 of yacc.c  */
#line 840 "parser.y"
    { (yyval.sc_stmt) = (Stmt *) NewIfStmt( cg, cg->tokenLoc, (yyvsp[(3) - (4)].sc_expr), NULL, NULL); ; ;}
      break;
      
    case 168:
      
      /* Line 1464 of yacc.c  */
#line 848 "parser.y"
    { (yyval.sc_stmt) = (Stmt *) NewBlockStmt( cg, cg->tokenLoc, (yyvsp[(2) - (3)].sc_stmt)); ;}
      break;
      
    case 169:
      
      /* Line 1464 of yacc.c  */
#line 850 "parser.y"
    { (yyval.sc_stmt) = NULL; ;}
      break;
      
    case 170:
      
      /* Line 1464 of yacc.c  */
#line 854 "parser.y"
    { PushScope( cg, new Scope( cg )); cg->currentScope->funindex = cg->nextFunctionIndex; ;}
      break;
      
    case 171:
      
      /* Line 1464 of yacc.c  */
#line 858 "parser.y"
    {
      if (cg->options.dumpParseTree) {
        Writer wr( cg, std::cout );
        wr.WriteScopeDeclarations();
      }
      PopScope( cg );
      ;}
      break;
      
    case 173:
      
      /* Line 1464 of yacc.c  */
#line 869 "parser.y"
    { (yyval.sc_stmt) = AddStmt( cg, (yyvsp[(1) - (2)].sc_stmt), (yyvsp[(2) - (2)].sc_stmt)); ;}
      break;
      
    case 175:
      
      /* Line 1464 of yacc.c  */
#line 874 "parser.y"
    { (yyval.sc_stmt) = CheckStmt( cg, (yyvsp[(1) - (1)].sc_stmt)); ;}
      break;
      
    case 177:
      
      /* Line 1464 of yacc.c  */
#line 883 "parser.y"
    { (yyval.sc_stmt) = NULL; ;}
      break;
      
    case 178:
      
      /* Line 1464 of yacc.c  */
#line 887 "parser.y"
    { (yyval.sc_stmt) = NewSimpleAssignmentStmt( cg, cg->tokenLoc, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr), 0); ;}
      break;
      
    case 179:
      
      /* Line 1464 of yacc.c  */
#line 889 "parser.y"
    { (yyval.sc_stmt) = (Stmt *) NewExprStmt( cg, cg->tokenLoc, (yyvsp[(1) - (1)].sc_expr)); ;}
      break;
      
    case 180:
      
      /* Line 1464 of yacc.c  */
#line 891 "parser.y"
    { (yyval.sc_stmt) = NewCompoundAssignmentStmt( cg, cg->tokenLoc, ASSIGNMINUS_OP, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr)); ;}
      break;
      
    case 181:
      
      /* Line 1464 of yacc.c  */
#line 893 "parser.y"
    { (yyval.sc_stmt) = NewCompoundAssignmentStmt( cg, cg->tokenLoc, ASSIGNMOD_OP, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr)); ;}
      break;
      
    case 182:
      
      /* Line 1464 of yacc.c  */
#line 895 "parser.y"
    { (yyval.sc_stmt) = NewCompoundAssignmentStmt( cg, cg->tokenLoc, ASSIGNPLUS_OP, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr)); ;}
      break;
      
    case 183:
      
      /* Line 1464 of yacc.c  */
#line 897 "parser.y"
    { (yyval.sc_stmt) = NewCompoundAssignmentStmt( cg, cg->tokenLoc, ASSIGNSLASH_OP, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr)); ;}
      break;
      
    case 184:
      
      /* Line 1464 of yacc.c  */
#line 899 "parser.y"
    { (yyval.sc_stmt) = NewCompoundAssignmentStmt( cg, cg->tokenLoc, ASSIGNSTAR_OP, (yyvsp[(1) - (3)].sc_expr), (yyvsp[(3) - (3)].sc_expr)); ;}
      break;
      
    case 185:
      
      /* Line 1464 of yacc.c  */
#line 907 "parser.y"
    { (yyval.sc_stmt) = (Stmt *) NewWhileStmt( cg, cg->tokenLoc, WHILE_STMT, (yyvsp[(3) - (5)].sc_expr), (yyvsp[(5) - (5)].sc_stmt)); ;}
      break;
      
    case 186:
      
      /* Line 1464 of yacc.c  */
#line 909 "parser.y"
    { (yyval.sc_stmt) = (Stmt *) NewWhileStmt( cg, cg->tokenLoc, DO_STMT, (yyvsp[(5) - (7)].sc_expr), (yyvsp[(2) - (7)].sc_stmt)); ;}
      break;
      
    case 187:
      
      /* Line 1464 of yacc.c  */
#line 911 "parser.y"
    { (yyval.sc_stmt) = (Stmt *) NewForStmt( cg, cg->tokenLoc, (yyvsp[(3) - (9)].sc_stmt), (yyvsp[(5) - (9)].sc_expr), (yyvsp[(7) - (9)].sc_stmt), (yyvsp[(9) - (9)].sc_stmt)); ;}
      break;
      
    case 188:
      
      /* Line 1464 of yacc.c  */
#line 915 "parser.y"
    { (yyval.sc_stmt) = (Stmt *) NewWhileStmt( cg, cg->tokenLoc, WHILE_STMT, (yyvsp[(3) - (5)].sc_expr), (yyvsp[(5) - (5)].sc_stmt)); ;}
      break;
      
    case 189:
      
      /* Line 1464 of yacc.c  */
#line 917 "parser.y"
    { (yyval.sc_stmt) = (Stmt *) NewForStmt( cg, cg->tokenLoc, (yyvsp[(3) - (9)].sc_stmt), (yyvsp[(5) - (9)].sc_expr), (yyvsp[(7) - (9)].sc_stmt), (yyvsp[(9) - (9)].sc_stmt)); ;}
      break;
      
    case 190:
      
      /* Line 1464 of yacc.c  */
#line 922 "parser.y"
    {  (yyval.sc_expr) = CheckBooleanExpr( cg, cg->tokenLoc, (yyvsp[(1) - (1)].sc_expr), 0); ;}
      break;
      
    case 192:
      
      /* Line 1464 of yacc.c  */
#line 927 "parser.y"
    { (yyval.sc_stmt) = NULL; ;}
      break;
      
    case 194:
      
      /* Line 1464 of yacc.c  */
#line 932 "parser.y"
    {
      Stmt *lstmt = (yyvsp[(1) - (3)].sc_stmt);
      if (lstmt) {
        while (static_cast< ExprStmt * >( lstmt )->next)
          lstmt = static_cast< ExprStmt * >( lstmt )->next;
          static_cast< ExprStmt * >( lstmt )->next = (yyvsp[(3) - (3)].sc_stmt);
          (yyval.sc_stmt) = (yyvsp[(1) - (3)].sc_stmt);
          } else {
            (yyval.sc_stmt) = (yyvsp[(3) - (3)].sc_stmt);
          }
      ;}
      break;
      
    case 196:
      
      /* Line 1464 of yacc.c  */
#line 947 "parser.y"
    { (yyval.sc_expr) = NULL; ;}
      break;
      
    case 197:
      
      /* Line 1464 of yacc.c  */
#line 955 "parser.y"
    { (yyval.sc_stmt) = (Stmt *) NewReturnStmt( cg, cg->tokenLoc, cg->currentScope, (yyvsp[(2) - (3)].sc_expr)); ;}
      break;
      
    case 198:
      
      /* Line 1464 of yacc.c  */
#line 957 "parser.y"
    { (yyval.sc_stmt) = (Stmt *) NewReturnStmt( cg, cg->tokenLoc, cg->currentScope, NULL); ;}
      break;
      
    case 205:
      
      /* Line 1464 of yacc.c  */
#line 983 "parser.y"
    { (yyval.sc_expr) = (Expr *) NewIConstNode( cg, ICONST_OP, (yyvsp[(1) - (1)].sc_int), TB_Cint); ;}
      break;
      
    case 206:
      
      /* Line 1464 of yacc.c  */
#line 985 "parser.y"
    { TypeBase base = cg->theHal->GetFloatSuffixBase(cg->tokenLoc, ' ');
      (yyval.sc_expr) = (Expr *) NewFConstNode( cg, FCONST_OP, (yyvsp[(1) - (1)].sc_fval), base);
      ;}
      break;
      
    case 207:
      
      /* Line 1464 of yacc.c  */
#line 989 "parser.y"
    { TypeBase base = cg->theHal->GetFloatSuffixBase(cg->tokenLoc, 'f');
      (yyval.sc_expr) = (Expr *) NewFConstNode( cg, FCONST_OP, (yyvsp[(1) - (1)].sc_fval), base);
      ;}
      break;
      
    case 208:
      
      /* Line 1464 of yacc.c  */
#line 993 "parser.y"
    { TypeBase base = cg->theHal->GetFloatSuffixBase(cg->tokenLoc, 'h');
      (yyval.sc_expr) = (Expr *) NewFConstNode( cg, FCONST_OP, (yyvsp[(1) - (1)].sc_fval), base);
      ;}
      break;
      
    case 209:
      
      /* Line 1464 of yacc.c  */
#line 997 "parser.y"
    {TypeBase base = cg->theHal->GetFloatSuffixBase(cg->tokenLoc, 'x');
      (yyval.sc_expr) = (Expr *) NewFConstNode( cg, FCONST_OP, (yyvsp[(1) - (1)].sc_fval), base);
      ;}
      break;
      
      
      
      /* Line 1464 of yacc.c  */
#line 3151 "parser.cpp"
    default: break;
  }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);
  
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  
  *++yyvsp = yyval;
  
  /* Now `shift' the result of the reduction.  Determine what state
   that goes to, based on the state we popped back to and the rule
   number reduced by.  */
  
  yyn = yyr1[yyn];
  
  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
    else
      yystate = yydefgoto[yyn - YYNTOKENS];
      
      goto yynewstate;
  
  
  /*------------------------------------.
   | yyerrlab -- here on detecting error |
   `------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
  {
    ++yynerrs;
#if ! YYERROR_VERBOSE
    yyerror (cg, YY_("syntax error"));
#else
    {
      YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
      if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
      {
        YYSIZE_T yyalloc = 2 * yysize;
        if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
          yyalloc = YYSTACK_ALLOC_MAXIMUM;
          if (yymsg != yymsgbuf)
            YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yyalloc);
            if (yymsg)
              yymsg_alloc = yyalloc;
              else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
              }
      }
      
      if (0 < yysize && yysize <= yymsg_alloc)
      {
        (void) yysyntax_error (yymsg, yystate, yychar);
        yyerror (cg, yymsg);
      }
      else
      {
        yyerror (cg, YY_("syntax error"));
        if (yysize != 0)
          goto yyexhaustedlab;
      }
    }
#endif
  }
  
  
  
  if (yyerrstatus == 3)
  {
    /* If just tried and failed to reuse lookahead token after an
     error, discard it.  */
    
    if (yychar <= YYEOF)
    {
      /* Return failure if at end of input.  */
      if (yychar == YYEOF)
        YYABORT;
    }
    else
    {
      yydestruct ("Error: discarding",
                  yytoken, &yylval, cg);
      yychar = YYEMPTY;
    }
  }
  
  /* Else will try to reuse lookahead token after shifting the error
   token.  */
  goto yyerrlab1;
  
  
  /*---------------------------------------------------.
   | yyerrorlab -- error raised explicitly by YYERROR.  |
   `---------------------------------------------------*/
yyerrorlab:
  
  /* Pacify compilers like GCC when the user code never invokes
   YYERROR and the label yyerrorlab therefore never appears in user
   code.  */
  if (/*CONSTCOND*/ 0)
    goto yyerrorlab;
  
  /* Do not reclaim the symbols of the rule which action triggered
   this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;
  
  
  /*-------------------------------------------------------------.
   | yyerrlab1 -- common code for both syntax error and YYERROR.  |
   `-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */
  
  for (;;)
  {
    yyn = yypact[yystate];
    if (yyn != YYPACT_NINF)
    {
      yyn += YYTERROR;
      if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
          break;
	    }
    }
    
    /* Pop the current state because it cannot handle the error token.  */
    if (yyssp == yyss)
      YYABORT;
    
    
    yydestruct ("Error: popping",
                yystos[yystate], yyvsp, cg);
    YYPOPSTACK (1);
    yystate = *yyssp;
    YY_STACK_PRINT (yyss, yyssp);
  }
  
  *++yyvsp = yylval;
  
  
  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);
  
  yystate = yyn;
  goto yynewstate;
  
  
  /*-------------------------------------.
   | yyacceptlab -- YYACCEPT comes here.  |
   `-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;
  
  /*-----------------------------------.
   | yyabortlab -- YYABORT comes here.  |
   `-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;
  
#if !defined(yyoverflow) || YYERROR_VERBOSE
  /*-------------------------------------------------.
   | yyexhaustedlab -- memory exhaustion comes here.  |
   `-------------------------------------------------*/
yyexhaustedlab:
  yyerror (cg, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif
  
yyreturn:
  if (yychar != YYEMPTY)
    yydestruct ("Cleanup: discarding lookahead",
                yytoken, &yylval, cg);
  /* Do not reclaim the symbols of the rule which action triggered
   this YYABORT or YYACCEPT.  */
    YYPOPSTACK (yylen);
    YY_STACK_PRINT (yyss, yyssp);
    while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, cg);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
    if (yymsg != yymsgbuf)
      YYSTACK_FREE (yymsg);
#endif
    /* Make sure YYID is used.  */
      return YYID (yyresult);
      }



/* Line 1684 of yacc.c  */
#line 1013 "parser.y"



