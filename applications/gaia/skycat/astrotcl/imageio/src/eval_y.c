
/*  A Bison parser, made from eval.y
 by  GNU Bison version 1.25
  */

#define FFBISON 1  /* Identify Bison output.  */

#define	BOOLEAN	258
#define	LONG	259
#define	DOUBLE	260
#define	STRING	261
#define	BITSTR	262
#define	FUNCTION	263
#define	BFUNCTION	264
#define	GTIFILTER	265
#define	REGFILTER	266
#define	COLUMN	267
#define	BCOLUMN	268
#define	SCOLUMN	269
#define	BITCOL	270
#define	ROWREF	271
#define	OR	272
#define	AND	273
#define	EQ	274
#define	NE	275
#define	GT	276
#define	LT	277
#define	LTE	278
#define	GTE	279
#define	POWER	280
#define	NOT	281
#define	INTCAST	282
#define	FLTCAST	283
#define	UMINUS	284

#line 1 "eval.y"

/************************************************************************/
/*                                                                      */
/*                       CFITSIO Lexical Parser                         */
/*                                                                      */
/* This file is one of 3 files containing code which parses an          */
/* arithmetic expression and evaluates it in the context of an input    */
/* FITS file table extension.  The CFITSIO lexical parser is divided    */
/* into the following 3 parts/files: the CFITSIO "front-end",           */
/* eval_f.c, contains the interface between the user/CFITSIO and the    */
/* real core of the parser; the FLEX interpreter, eval_l.c, takes the   */
/* input string and parses it into tokens and identifies the FITS       */
/* information required to evaluate the expression (ie, keywords and    */
/* columns); and, the BISON grammar and evaluation routines, eval_y.c,  */
/* receives the FLEX output and determines and performs the actual      */
/* operations.  The files eval_l.c and eval_y.c are produced from       */
/* running flex and bison on the files eval.l and eval.y, respectively. */
/* (flex and bison are available from any GNU archive: see www.gnu.org) */
/*                                                                      */
/* The grammar rules, rather than evaluating the expression in situ,    */
/* builds a tree, or Nodal, structure mapping out the order of          */
/* operations and expression dependencies.  This "compilation" process  */
/* allows for much faster processing of multiple rows.  This technique  */
/* was developed by Uwe Lammers of the XMM Science Analysis System,     */
/* although the CFITSIO implementation is entirely code original.       */
/*                                                                      */
/*                                                                      */
/* Modification History:                                                */
/*                                                                      */
/*   Kent Blackburn      c1992  Original parser code developed for the  */
/*                              FTOOLS software package, in particular, */
/*                              the fselect task.                       */
/*   Kent Blackburn      c1995  BIT column support added                */
/*   Peter D Wilson   Feb 1998  Vector column support added             */
/*   Peter D Wilson   May 1998  Ported to CFITSIO library.  User        */
/*                              interface routines written, in essence  */
/*                              making fselect, fcalc, and maketime     */
/*                              capabilities available to all tools     */
/*                              via single function calls.              */
/*   Peter D Wilson   Jun 1998  Major rewrite of parser core, so as to  */
/*                              create a run-time evaluation tree,      */
/*                              inspired by the work of Uwe Lammers,    */
/*                              resulting in a speed increase of        */
/*                              10-100 times.                           */
/*   Peter D Wilson   Jul 1998  gtifilter(a,b,c,d) function added       */
/*                                                                      */
/************************************************************************/

#define  APPROX 1.0e-7
#include "eval_defs.h"
#include "region.h"
#include <time.h>

   /*  Bison uses alloca for allocations, but VMS lacks it  */
#if defined(vms) || defined(__vms) || defined(macintosh) || defined(__WIN32__)
#define alloca malloc
#endif

   /*  Shrink the initial stack depth to keep local data <32K (mac limit)  */
   /*  yacc will allocate more space if needed, though.                    */
#define  FFINITDEPTH   100

/***************************************************************/
/*  Replace Bison's BACKUP macro with one that fixes a bug --  */
/*  must update state after popping the stack -- and allows    */
/*  popping multiple terms at one time.                        */
/***************************************************************/

#define FFNEWBACKUP(token, value)                               \
   do								\
     if (ffchar == FFEMPTY )   					\
       { ffchar = (token);                                      \
         memcpy( &fflval, &(value), sizeof(value) );            \
         ffchar1 = FFTRANSLATE (ffchar);			\
         while (fflen--) FFPOPSTACK;				\
         ffstate = *ffssp;					\
         goto ffbackup;						\
       }							\
     else							\
       { fferror ("syntax error: cannot back up"); FFERROR; }	\
   while (0)

/***************************************************************/
/*  Useful macros for accessing/testing Nodes                  */
/***************************************************************/

#define TEST(a)        if( (a)<0 ) FFERROR
#define SIZE(a)        gParse.Nodes[ a ].value.nelem
#define TYPE(a)        gParse.Nodes[ a ].type
#define PROMOTE(a,b)   if( TYPE(a) > TYPE(b) )                  \
                          b = New_Unary( TYPE(a), 0, b );       \
                       else if( TYPE(a) < TYPE(b) )             \
	                  a = New_Unary( TYPE(b), 0, a );

/*****  Internal functions  *****/

#ifdef __cplusplus
extern "C" {
#endif

static int  Alloc_Node    ( void );
static void Free_Last_Node( void );

static int  New_Const ( int returnType, void *value, long len );
static int  New_Column( int ColNum );
static int  New_Unary ( int returnType, int Op, int Node1 );
static int  New_BinOp ( int returnType, int Node1, int Op, int Node2 );
static int  New_Func  ( int returnType, funcOp Op, int nNodes,
			int Node1, int Node2, int Node3, int Node4, 
			int Node5, int Node6, int Node7 );
static int  New_Deref ( int Var,  int nDim,
			int Dim1, int Dim2, int Dim3, int Dim4, int Dim5 );
static int  New_GTI   ( char *fname, int Node1, char *start, char *stop );
static int  New_REG   ( char *fname, int NodeX, int NodeY, char *colNames );
static int  Locate_Col( Node *this );
static int  Test_Dims ( int Node1, int Node2 );

static void Allocate_Ptrs( Node *this );
static void Do_Unary     ( Node *this );
static void Do_BinOp_bit ( Node *this );
static void Do_BinOp_str ( Node *this );
static void Do_BinOp_log ( Node *this );
static void Do_BinOp_lng ( Node *this );
static void Do_BinOp_dbl ( Node *this );
static void Do_Func      ( Node *this );
static void Do_Deref     ( Node *this );
static void Do_GTI       ( Node *this );
static void Do_REG       ( Node *this );

static long Search_GTI   ( double evtTime, long nGTI, double *start,
			   double *stop, int ordered );

static char  saobox (double xcen, double ycen, double xwid, double ywid,
		     double rot,  double xcol, double ycol);
static char  ellipse(double xcen, double ycen, double xrad, double yrad,
		     double rot, double xcol, double ycol);
static char  circle (double xcen, double ycen, double rad,
		     double xcol, double ycol);
static char  bnear  (double x, double y, double tolerance);
static char  bitcmp (char *bitstrm1, char *bitstrm2);
static char  bitlgte(char *bits1, int oper, char *bits2);

static void  bitand(char *result, char *bitstrm1, char *bitstrm2);
static void  bitor (char *result, char *bitstrm1, char *bitstrm2);
static void  bitnot(char *result, char *bits);

static void  fferror(char *msg);

#ifdef __cplusplus
    }
#endif


#line 155 "eval.y"
typedef union {
    int    Node;        /* Index of Node */
    double dbl;         /* real value    */
    long   lng;         /* integer value */
    char   log;         /* logical value */
    char   str[256];    /* string value  */
} FFSTYPE;
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	FFFINAL		216
#define	FFFLAG		-32768
#define	FFNTBASE	46

#define FFTRANSLATE(x) ((unsigned)(x) <= 284 ? fftranslate[x] : 52)

static const char fftranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    42,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,    31,    35,     2,    43,
    44,    32,    29,    17,    30,     2,    33,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,    19,     2,     2,
    18,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    41,     2,    45,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,    34,     2,    24,     2,     2,     2,     2,
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
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    20,    21,    22,    23,    25,    26,    27,    28,    36,
    37,    38,    39,    40
};

#if FFDEBUG != 0
static const short ffprhs[] = {     0,
     0,     1,     4,     6,     9,    12,    15,    18,    21,    23,
    25,    29,    33,    37,    40,    44,    46,    48,    50,    52,
    56,    60,    64,    68,    72,    76,    79,    83,    87,    91,
    94,    98,   102,   106,   110,   116,   121,   128,   137,   148,
   161,   164,   167,   170,   173,   175,   177,   181,   185,   189,
   193,   197,   201,   205,   209,   213,   217,   221,   225,   229,
   233,   237,   241,   245,   249,   253,   259,   263,   267,   271,
   277,   285,   297,   313,   316,   320,   326,   336,   340,   348,
   358,   363,   370,   379,   390,   403,   406,   410,   412,   414,
   418,   422
};

static const short ffrhs[] = {    -1,
    46,    47,     0,    42,     0,    49,    42,     0,    50,    42,
     0,    51,    42,     0,    48,    42,     0,     1,    42,     0,
     7,     0,    15,     0,    48,    35,    48,     0,    48,    34,
    48,     0,    48,    29,    48,     0,    37,    48,     0,    43,
    48,    44,     0,     4,     0,     5,     0,    12,     0,    16,
     0,    49,    31,    49,     0,    49,    29,    49,     0,    49,
    30,    49,     0,    49,    32,    49,     0,    49,    33,    49,
     0,    49,    36,    49,     0,    30,    49,     0,    43,    49,
    44,     0,    49,    32,    50,     0,    50,    32,    49,     0,
     8,    44,     0,     8,    50,    44,     0,     8,    51,    44,
     0,     8,    48,    44,     0,     8,    49,    44,     0,     8,
    49,    17,    49,    44,     0,    49,    41,    49,    45,     0,
    49,    41,    49,    17,    49,    45,     0,    49,    41,    49,
    17,    49,    17,    49,    45,     0,    49,    41,    49,    17,
    49,    17,    49,    17,    49,    45,     0,    49,    41,    49,
    17,    49,    17,    49,    17,    49,    17,    49,    45,     0,
    38,    49,     0,    38,    50,     0,    39,    49,     0,    39,
    50,     0,     3,     0,    13,     0,    48,    22,    48,     0,
    48,    23,    48,     0,    48,    26,    48,     0,    48,    27,
    48,     0,    48,    25,    48,     0,    48,    28,    48,     0,
    49,    25,    49,     0,    49,    26,    49,     0,    49,    28,
    49,     0,    49,    27,    49,     0,    49,    24,    49,     0,
    49,    22,    49,     0,    49,    23,    49,     0,    51,    22,
    51,     0,    51,    23,    51,     0,    50,    21,    50,     0,
    50,    20,    50,     0,    50,    22,    50,     0,    50,    23,
    50,     0,    49,    18,    49,    19,    49,     0,     9,    49,
    44,     0,     9,    50,    44,     0,     9,    51,    44,     0,
     8,    50,    17,    50,    44,     0,     9,    49,    17,    49,
    17,    49,    44,     0,     9,    49,    17,    49,    17,    49,
    17,    49,    17,    49,    44,     0,     9,    49,    17,    49,
    17,    49,    17,    49,    17,    49,    17,    49,    17,    49,
    44,     0,    10,    44,     0,    10,     6,    44,     0,    10,
     6,    17,    49,    44,     0,    10,     6,    17,    49,    17,
     6,    17,     6,    44,     0,    11,     6,    44,     0,    11,
     6,    17,    49,    17,    49,    44,     0,    11,     6,    17,
    49,    17,    49,    17,     6,    44,     0,    50,    41,    49,
    45,     0,    50,    41,    49,    17,    49,    45,     0,    50,
    41,    49,    17,    49,    17,    49,    45,     0,    50,    41,
    49,    17,    49,    17,    49,    17,    49,    45,     0,    50,
    41,    49,    17,    49,    17,    49,    17,    49,    17,    49,
    45,     0,    37,    50,     0,    43,    50,    44,     0,     6,
     0,    14,     0,    43,    51,    44,     0,    51,    29,    51,
     0,     8,    51,    17,    51,    44,     0
};

#endif

#if FFDEBUG != 0
static const short ffrline[] = { 0,
   199,   200,   203,   204,   209,   214,   219,   224,   227,   232,
   234,   237,   240,   243,   245,   249,   251,   253,   255,   257,
   260,   263,   266,   269,   272,   275,   277,   279,   283,   287,
   297,   308,   317,   326,   363,   400,   402,   404,   406,   408,
   410,   412,   414,   416,   420,   422,   424,   427,   430,   433,
   436,   439,   442,   445,   448,   451,   454,   457,   460,   463,
   466,   469,   471,   473,   475,   478,   485,   498,   511,   522,
   538,   556,   577,   604,   608,   612,   615,   619,   623,   626,
   630,   632,   634,   636,   638,   640,   642,   646,   649,   651,
   653,   656
};
#endif


#if FFDEBUG != 0 || defined (FFERROR_VERBOSE)

static const char * const fftname[] = {   "$","error","$undefined.","BOOLEAN",
"LONG","DOUBLE","STRING","BITSTR","FUNCTION","BFUNCTION","GTIFILTER","REGFILTER",
"COLUMN","BCOLUMN","SCOLUMN","BITCOL","ROWREF","','","'='","':'","OR","AND",
"EQ","NE","'~'","GT","LT","LTE","GTE","'+'","'-'","'%'","'*'","'/'","'|'","'&'",
"POWER","NOT","INTCAST","FLTCAST","UMINUS","'['","'\\n'","'('","')'","']'","lines",
"line","bits","expr","bexpr","sexpr", NULL
};
#endif

static const short ffr1[] = {     0,
    46,    46,    47,    47,    47,    47,    47,    47,    48,    48,
    48,    48,    48,    48,    48,    49,    49,    49,    49,    49,
    49,    49,    49,    49,    49,    49,    49,    49,    49,    49,
    49,    49,    49,    49,    49,    49,    49,    49,    49,    49,
    49,    49,    49,    49,    50,    50,    50,    50,    50,    50,
    50,    50,    50,    50,    50,    50,    50,    50,    50,    50,
    50,    50,    50,    50,    50,    50,    50,    50,    50,    50,
    50,    50,    50,    50,    50,    50,    50,    50,    50,    50,
    50,    50,    50,    50,    50,    50,    50,    51,    51,    51,
    51,    51
};

static const short ffr2[] = {     0,
     0,     2,     1,     2,     2,     2,     2,     2,     1,     1,
     3,     3,     3,     2,     3,     1,     1,     1,     1,     3,
     3,     3,     3,     3,     3,     2,     3,     3,     3,     2,
     3,     3,     3,     3,     5,     4,     6,     8,    10,    12,
     2,     2,     2,     2,     1,     1,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     5,     3,     3,     3,     5,
     7,    11,    15,     2,     3,     5,     9,     3,     7,     9,
     4,     6,     8,    10,    12,     2,     3,     1,     1,     3,
     3,     5
};

static const short ffdefact[] = {     1,
     0,     0,    45,    16,    17,    88,     9,     0,     0,     0,
     0,    18,    46,    89,    10,    19,     0,     0,     0,     0,
     3,     0,     2,     0,     0,     0,     0,     8,    30,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
    26,     0,     0,    14,     0,    86,    41,    42,    43,    44,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     7,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     4,
     0,     0,     0,     0,     0,     0,     5,     0,     0,     0,
     6,    33,     0,    34,     0,    31,     0,    32,     0,    67,
    68,    69,     0,    75,     0,    78,    15,    27,    87,    90,
     0,     0,    47,    48,    51,    49,    50,    52,    13,    12,
    11,     0,    58,    59,    57,    53,    54,    56,    55,    21,
    22,    20,    23,    28,    24,    25,     0,    63,    62,    64,
    65,    29,     0,     0,     0,    60,    61,    91,     0,     0,
     0,     0,     0,     0,    14,     0,     0,     0,    36,     0,
    81,     0,     0,    35,    70,    92,     0,     0,    76,     0,
    66,     0,     0,     0,     0,     0,     0,    37,     0,    82,
     0,    71,     0,     0,    79,     0,     0,     0,     0,     0,
     0,    38,     0,    83,     0,    77,    80,     0,     0,     0,
     0,    39,     0,    84,     0,    72,     0,     0,     0,    40,
    85,     0,     0,    73,     0,     0
};

static const short ffdefgoto[] = {     1,
    23,    34,    45,    42,    43
};

static const short ffpact[] = {-32768,
   209,   -29,-32768,-32768,-32768,-32768,-32768,   250,   292,    -5,
    17,-32768,-32768,-32768,-32768,-32768,   292,   292,   292,   292,
-32768,   292,-32768,   954,   784,   943,   116,-32768,-32768,   206,
   515,    74,    97,  1009,   540,    13,    57,   -15,-32768,   -13,
    -9,   293,    33,-32768,   926,     5,    -9,     5,    -9,     5,
   247,   712,   108,    59,     7,     7,     7,     7,     7,     7,
     7,     7,     7,-32768,   292,   292,   292,   292,   292,   292,
   292,   292,   292,   292,   292,   292,   292,   292,   292,-32768,
   292,   292,   292,   292,   292,   292,-32768,    -3,    -3,    -3,
-32768,-32768,   292,-32768,   292,-32768,    -3,-32768,   292,-32768,
-32768,-32768,   292,-32768,   292,-32768,-32768,-32768,-32768,-32768,
     7,     7,    24,    24,    24,    24,    24,    24,    55,-32768,
-32768,   906,   992,   992,   992,   879,   879,   879,   879,   129,
   129,   129,   -24,     5,   -24,   -24,   315,   114,    20,   -26,
   -26,   -24,   340,    -3,    -3,    31,    31,-32768,   736,   882,
   -20,   805,   565,   825,-32768,    88,   292,   292,-32768,   292,
-32768,    70,   -19,-32768,-32768,-32768,   292,    77,-32768,   292,
   975,   365,   390,   590,     9,   615,   292,-32768,   292,-32768,
   292,-32768,    87,    94,-32768,   415,   440,   845,    81,    89,
   292,-32768,   292,-32768,   292,-32768,-32768,   465,   490,   640,
   292,-32768,   292,-32768,   292,-32768,   664,   688,   865,-32768,
-32768,   292,   760,-32768,    63,-32768
};

static const short ffpgoto[] = {-32768,
-32768,   126,    -1,    29,    19
};


#define	FFLAST		1044


static const short fftable[] = {    25,
    38,   103,     6,   105,   144,    85,    31,    35,    90,    90,
    14,    78,    28,     7,    86,    41,    79,    47,    49,    27,
    52,    15,    40,   166,   110,   183,    33,    37,   104,    26,
   106,    79,    81,    82,    83,    84,    32,    36,    39,   145,
    54,    83,    84,   111,    85,    86,    46,    48,    50,   112,
    53,    85,    61,    86,    88,    89,   101,    62,    63,    90,
    86,    90,   216,   122,   123,   124,   125,   126,   127,   128,
   129,   130,   131,   132,   133,   135,   136,   137,    88,    89,
    88,    89,   175,   142,   143,    90,    97,    90,    62,    63,
    95,   149,   189,    81,    82,    83,    84,   152,    90,   190,
   102,   153,   110,   154,   134,    85,   146,   147,   148,   138,
   139,   140,   141,    97,    86,   151,    61,    96,    88,    89,
     0,    62,    63,   150,   196,    90,    24,    81,    82,    83,
    84,   107,   197,    30,    82,    83,    84,    88,    89,    85,
    98,     0,     0,    44,    90,    85,     0,    51,    86,     0,
     0,   109,     0,     0,    86,   171,   172,    91,   173,     0,
    76,    77,   162,   163,    78,   174,     0,     0,   176,    79,
     0,     0,     0,     0,     0,   186,     0,   187,     0,   188,
   113,   114,   115,   116,   117,   118,   119,   120,   121,   198,
     0,   199,     0,   200,     0,     0,     0,     0,     0,   207,
     0,   208,     0,   209,     0,     0,     0,     0,   215,     2,
   213,     3,     4,     5,     6,     7,     8,     9,    10,    11,
    12,    13,    14,    15,    16,     0,     0,    55,    56,     0,
    57,    58,    59,    60,    61,     0,   155,   156,    17,    62,
    63,     0,     0,     0,     0,    18,    19,    20,     0,    92,
    21,    22,     3,     4,     5,     6,     7,     8,     9,    10,
    11,    12,    13,    14,    15,    16,     0,     0,    55,    56,
     0,    57,    58,    59,    60,    61,     0,     0,     0,    17,
    62,    63,     0,     0,     0,     0,    18,    19,    20,     0,
   107,     0,    22,    29,     3,     4,     5,     6,     7,     8,
     9,    10,    11,    12,    13,    14,    15,    16,     0,     0,
     0,     0,    81,    82,    83,    84,     0,     0,     0,     0,
     0,    17,     0,     0,    85,     0,     0,     0,    18,    19,
    20,   158,    65,    86,    22,     0,    66,    67,    68,    69,
    70,    71,    72,    73,    74,    75,    76,    77,     0,     0,
    78,     0,     0,     0,     0,    79,   160,    65,     0,   159,
     0,    66,    67,    68,    69,    70,    71,    72,    73,    74,
    75,    76,    77,     0,     0,    78,     0,     0,     0,     0,
    79,   177,    65,     0,   161,     0,    66,    67,    68,    69,
    70,    71,    72,    73,    74,    75,    76,    77,     0,     0,
    78,     0,     0,     0,     0,    79,   179,    65,     0,   178,
     0,    66,    67,    68,    69,    70,    71,    72,    73,    74,
    75,    76,    77,     0,     0,    78,     0,     0,     0,     0,
    79,   191,    65,     0,   180,     0,    66,    67,    68,    69,
    70,    71,    72,    73,    74,    75,    76,    77,     0,     0,
    78,     0,     0,     0,     0,    79,   193,    65,     0,   192,
     0,    66,    67,    68,    69,    70,    71,    72,    73,    74,
    75,    76,    77,     0,     0,    78,     0,     0,     0,     0,
    79,   201,    65,     0,   194,     0,    66,    67,    68,    69,
    70,    71,    72,    73,    74,    75,    76,    77,     0,     0,
    78,     0,     0,     0,     0,    79,   203,    65,     0,   202,
     0,    66,    67,    68,    69,    70,    71,    72,    73,    74,
    75,    76,    77,     0,     0,    78,     0,     0,     0,     0,
    79,    93,    65,     0,   204,     0,    66,    67,    68,    69,
    70,    71,    72,    73,    74,    75,    76,    77,     0,     0,
    78,     0,     0,     0,     0,    79,    99,    65,    94,     0,
     0,    66,    67,    68,    69,    70,    71,    72,    73,    74,
    75,    76,    77,     0,     0,    78,     0,     0,     0,     0,
    79,   168,    65,   100,     0,     0,    66,    67,    68,    69,
    70,    71,    72,    73,    74,    75,    76,    77,     0,     0,
    78,     0,     0,     0,     0,    79,   181,    65,   169,     0,
     0,    66,    67,    68,    69,    70,    71,    72,    73,    74,
    75,    76,    77,     0,     0,    78,     0,     0,     0,     0,
    79,   184,    65,   182,     0,     0,    66,    67,    68,    69,
    70,    71,    72,    73,    74,    75,    76,    77,     0,     0,
    78,     0,     0,     0,     0,    79,   205,    65,   185,     0,
     0,    66,    67,    68,    69,    70,    71,    72,    73,    74,
    75,    76,    77,     0,     0,    78,     0,     0,     0,     0,
    79,    65,     0,   206,     0,    66,    67,    68,    69,    70,
    71,    72,    73,    74,    75,    76,    77,     0,     0,    78,
     0,     0,     0,     0,    79,    65,     0,     0,   210,    66,
    67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
    77,     0,     0,    78,     0,     0,     0,     0,    79,    65,
     0,     0,   211,    66,    67,    68,    69,    70,    71,    72,
    73,    74,    75,    76,    77,     0,     0,    78,     0,     0,
     0,     0,    79,    65,     0,   108,     0,    66,    67,    68,
    69,    70,    71,    72,    73,    74,    75,    76,    77,     0,
     0,    78,     0,     0,     0,     0,    79,    65,     0,   164,
     0,    66,    67,    68,    69,    70,    71,    72,    73,    74,
    75,    76,    77,     0,     0,    78,     0,     0,     0,     0,
    79,    65,     0,   214,     0,    66,    67,    68,    69,    70,
    71,    72,    73,    74,    75,    76,    77,     0,     0,    78,
     0,   167,    65,     0,    79,    80,    66,    67,    68,    69,
    70,    71,    72,    73,    74,    75,    76,    77,     0,     0,
    78,   170,    65,     0,     0,    79,    66,    67,    68,    69,
    70,    71,    72,    73,    74,    75,    76,    77,     0,     0,
    78,   195,    65,     0,     0,    79,    66,    67,    68,    69,
    70,    71,    72,    73,    74,    75,    76,    77,     0,     0,
    78,   212,    65,     0,     0,    79,    66,    67,    68,    69,
    70,    71,    72,    73,    74,    75,    76,    77,     0,     0,
    78,    81,    82,    83,    84,    79,     0,    73,    74,    75,
    76,    77,     0,    85,    78,     0,     0,     0,     0,    79,
     0,     0,    86,    65,   157,   165,     0,    66,    67,    68,
    69,    70,    71,    72,    73,    74,    75,    76,    77,     0,
     0,    78,     0,    65,     0,     0,    79,    66,    67,    68,
    69,    70,    71,    72,    73,    74,    75,    76,    77,     0,
     0,    78,    81,    82,    83,    84,    79,     0,     0,     0,
     0,     0,     0,     0,    85,    55,    56,     0,    57,    58,
    59,    60,    61,    86,    87,     0,     0,    62,    63,     0,
     0,     0,     0,     0,     0,    64,    66,    67,    68,    69,
    70,    71,    72,    73,    74,    75,    76,    77,     0,     0,
    78,     0,     0,     0,     0,    79,    69,    70,    71,    72,
    73,    74,    75,    76,    77,     0,     0,    78,     0,     0,
    55,    56,    79,    57,    58,    59,    60,    61,     0,     0,
     0,     0,    62,    63
};

static const short ffcheck[] = {     1,
     6,    17,     6,    17,     8,    32,     8,     9,    29,    29,
    14,    36,    42,     7,    41,    17,    41,    19,    20,     1,
    22,    15,     6,    44,    44,    17,     8,     9,    44,     1,
    44,    41,    20,    21,    22,    23,     8,     9,    44,    43,
    22,    22,    23,    37,    32,    41,    18,    19,    20,    43,
    22,    32,    29,    41,    22,    23,    44,    34,    35,    29,
    41,    29,     0,    65,    66,    67,    68,    69,    70,    71,
    72,    73,    74,    75,    76,    77,    78,    79,    22,    23,
    22,    23,     6,    85,    86,    29,    17,    29,    34,    35,
    17,    93,     6,    20,    21,    22,    23,    99,    29,     6,
    44,   103,    44,   105,    76,    32,    88,    89,    90,    81,
    82,    83,    84,    17,    41,    97,    29,    44,    22,    23,
    -1,    34,    35,    95,    44,    29,     1,    20,    21,    22,
    23,    44,    44,     8,    21,    22,    23,    22,    23,    32,
    44,    -1,    -1,    18,    29,    32,    -1,    22,    41,    -1,
    -1,    44,    -1,    -1,    41,   157,   158,    42,   160,    -1,
    32,    33,   144,   145,    36,   167,    -1,    -1,   170,    41,
    -1,    -1,    -1,    -1,    -1,   177,    -1,   179,    -1,   181,
    55,    56,    57,    58,    59,    60,    61,    62,    63,   191,
    -1,   193,    -1,   195,    -1,    -1,    -1,    -1,    -1,   201,
    -1,   203,    -1,   205,    -1,    -1,    -1,    -1,     0,     1,
   212,     3,     4,     5,     6,     7,     8,     9,    10,    11,
    12,    13,    14,    15,    16,    -1,    -1,    22,    23,    -1,
    25,    26,    27,    28,    29,    -1,   111,   112,    30,    34,
    35,    -1,    -1,    -1,    -1,    37,    38,    39,    -1,    44,
    42,    43,     3,     4,     5,     6,     7,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    -1,    -1,    22,    23,
    -1,    25,    26,    27,    28,    29,    -1,    -1,    -1,    30,
    34,    35,    -1,    -1,    -1,    -1,    37,    38,    39,    -1,
    44,    -1,    43,    44,     3,     4,     5,     6,     7,     8,
     9,    10,    11,    12,    13,    14,    15,    16,    -1,    -1,
    -1,    -1,    20,    21,    22,    23,    -1,    -1,    -1,    -1,
    -1,    30,    -1,    -1,    32,    -1,    -1,    -1,    37,    38,
    39,    17,    18,    41,    43,    -1,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    -1,    -1,
    36,    -1,    -1,    -1,    -1,    41,    17,    18,    -1,    45,
    -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
    31,    32,    33,    -1,    -1,    36,    -1,    -1,    -1,    -1,
    41,    17,    18,    -1,    45,    -1,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    -1,    -1,
    36,    -1,    -1,    -1,    -1,    41,    17,    18,    -1,    45,
    -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
    31,    32,    33,    -1,    -1,    36,    -1,    -1,    -1,    -1,
    41,    17,    18,    -1,    45,    -1,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    -1,    -1,
    36,    -1,    -1,    -1,    -1,    41,    17,    18,    -1,    45,
    -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
    31,    32,    33,    -1,    -1,    36,    -1,    -1,    -1,    -1,
    41,    17,    18,    -1,    45,    -1,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    -1,    -1,
    36,    -1,    -1,    -1,    -1,    41,    17,    18,    -1,    45,
    -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
    31,    32,    33,    -1,    -1,    36,    -1,    -1,    -1,    -1,
    41,    17,    18,    -1,    45,    -1,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    -1,    -1,
    36,    -1,    -1,    -1,    -1,    41,    17,    18,    44,    -1,
    -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
    31,    32,    33,    -1,    -1,    36,    -1,    -1,    -1,    -1,
    41,    17,    18,    44,    -1,    -1,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    -1,    -1,
    36,    -1,    -1,    -1,    -1,    41,    17,    18,    44,    -1,
    -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
    31,    32,    33,    -1,    -1,    36,    -1,    -1,    -1,    -1,
    41,    17,    18,    44,    -1,    -1,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    -1,    -1,
    36,    -1,    -1,    -1,    -1,    41,    17,    18,    44,    -1,
    -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
    31,    32,    33,    -1,    -1,    36,    -1,    -1,    -1,    -1,
    41,    18,    -1,    44,    -1,    22,    23,    24,    25,    26,
    27,    28,    29,    30,    31,    32,    33,    -1,    -1,    36,
    -1,    -1,    -1,    -1,    41,    18,    -1,    -1,    45,    22,
    23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
    33,    -1,    -1,    36,    -1,    -1,    -1,    -1,    41,    18,
    -1,    -1,    45,    22,    23,    24,    25,    26,    27,    28,
    29,    30,    31,    32,    33,    -1,    -1,    36,    -1,    -1,
    -1,    -1,    41,    18,    -1,    44,    -1,    22,    23,    24,
    25,    26,    27,    28,    29,    30,    31,    32,    33,    -1,
    -1,    36,    -1,    -1,    -1,    -1,    41,    18,    -1,    44,
    -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
    31,    32,    33,    -1,    -1,    36,    -1,    -1,    -1,    -1,
    41,    18,    -1,    44,    -1,    22,    23,    24,    25,    26,
    27,    28,    29,    30,    31,    32,    33,    -1,    -1,    36,
    -1,    17,    18,    -1,    41,    42,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    -1,    -1,
    36,    17,    18,    -1,    -1,    41,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    -1,    -1,
    36,    17,    18,    -1,    -1,    41,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    -1,    -1,
    36,    17,    18,    -1,    -1,    41,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    -1,    -1,
    36,    20,    21,    22,    23,    41,    -1,    29,    30,    31,
    32,    33,    -1,    32,    36,    -1,    -1,    -1,    -1,    41,
    -1,    -1,    41,    18,    19,    44,    -1,    22,    23,    24,
    25,    26,    27,    28,    29,    30,    31,    32,    33,    -1,
    -1,    36,    -1,    18,    -1,    -1,    41,    22,    23,    24,
    25,    26,    27,    28,    29,    30,    31,    32,    33,    -1,
    -1,    36,    20,    21,    22,    23,    41,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    32,    22,    23,    -1,    25,    26,
    27,    28,    29,    41,    42,    -1,    -1,    34,    35,    -1,
    -1,    -1,    -1,    -1,    -1,    42,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    -1,    -1,
    36,    -1,    -1,    -1,    -1,    41,    25,    26,    27,    28,
    29,    30,    31,    32,    33,    -1,    -1,    36,    -1,    -1,
    22,    23,    41,    25,    26,    27,    28,    29,    -1,    -1,
    -1,    -1,    34,    35
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/local/share/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#include <malloc.h>
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
#include <malloc.h>
 #pragma alloca
#else /* not MSDOS, __TURBOC__, or _AIX */
#ifdef __hpux
#ifdef __cplusplus
extern "C" {
void *alloca (unsigned int);
};
#else /* not __cplusplus */
void *alloca ();
#endif /* not __cplusplus */
#endif /* __hpux */
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc.  */
#endif /* not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define fferrok		(fferrstatus = 0)
#define ffclearin	(ffchar = FFEMPTY)
#define FFEMPTY		-2
#define FFEOF		0
#define FFACCEPT	return(0)
#define FFABORT 	return(1)
#define FFERROR		goto fferrlab1
/* Like FFERROR except do call fferror.
   This remains here temporarily to ease the
   transition to the new meaning of FFERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define FFFAIL		goto fferrlab
#define FFRECOVERING()  (!!fferrstatus)
#define FFBACKUP(token, value) \
do								\
  if (ffchar == FFEMPTY && fflen == 1)				\
    { ffchar = (token), fflval = (value);			\
      ffchar1 = FFTRANSLATE (ffchar);				\
      FFPOPSTACK;						\
      goto ffbackup;						\
    }								\
  else								\
    { fferror ("syntax error: cannot back up"); FFERROR; }	\
while (0)

#define FFTERROR	1
#define FFERRCODE	256

#ifndef FFPURE
#define FFLEX		fflex()
#endif

#ifdef FFPURE
#ifdef FFLSP_NEEDED
#ifdef FFLEX_PARAM
#define FFLEX		fflex(&fflval, &fflloc, FFLEX_PARAM)
#else
#define FFLEX		fflex(&fflval, &fflloc)
#endif
#else /* not FFLSP_NEEDED */
#ifdef FFLEX_PARAM
#define FFLEX		fflex(&fflval, FFLEX_PARAM)
#else
#define FFLEX		fflex(&fflval)
#endif
#endif /* not FFLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef FFPURE

int	ffchar;			/*  the lookahead symbol		*/
FFSTYPE	fflval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef FFLSP_NEEDED
FFLTYPE fflloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int ffnerrs;			/*  number of parse errors so far       */
#endif  /* not FFPURE */

#if FFDEBUG != 0
int ffdebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  FFINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	FFINITDEPTH
#define FFINITDEPTH 200
#endif

/*  FFMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if FFMAXDEPTH == 0
#undef FFMAXDEPTH
#endif

#ifndef FFMAXDEPTH
#define FFMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int ffparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __ff_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__ff_memcpy (to, from, count)
     char *to;
     char *from;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__ff_memcpy (char *to, char *from, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 196 "/usr/local/share/bison.simple"

/* The user can define FFPARSE_PARAM as the name of an argument to be passed
   into ffparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef FFPARSE_PARAM
#ifdef __cplusplus
#define FFPARSE_PARAM_ARG void *FFPARSE_PARAM
#define FFPARSE_PARAM_DECL
#else /* not __cplusplus */
#define FFPARSE_PARAM_ARG FFPARSE_PARAM
#define FFPARSE_PARAM_DECL void *FFPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not FFPARSE_PARAM */
#define FFPARSE_PARAM_ARG
#define FFPARSE_PARAM_DECL
#endif /* not FFPARSE_PARAM */

int
ffparse(FFPARSE_PARAM_ARG)
     FFPARSE_PARAM_DECL
{
  register int ffstate;
  register int ffn;
  register short *ffssp;
  register FFSTYPE *ffvsp;
  int fferrstatus;	/*  number of tokens to shift before error messages enabled */
  int ffchar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	ffssa[FFINITDEPTH];	/*  the state stack			*/
  FFSTYPE ffvsa[FFINITDEPTH];	/*  the semantic value stack		*/

  short *ffss = ffssa;		/*  refer to the stacks thru separate pointers */
  FFSTYPE *ffvs = ffvsa;	/*  to allow ffoverflow to reallocate them elsewhere */

#ifdef FFLSP_NEEDED
  FFLTYPE fflsa[FFINITDEPTH];	/*  the location stack			*/
  FFLTYPE *ffls = fflsa;
  FFLTYPE *fflsp;

#define FFPOPSTACK   (ffvsp--, ffssp--, fflsp--)
#else
#define FFPOPSTACK   (ffvsp--, ffssp--)
#endif

  int ffstacksize = FFINITDEPTH;

#ifdef FFPURE
  int ffchar;
  FFSTYPE fflval;
  int ffnerrs;
#ifdef FFLSP_NEEDED
  FFLTYPE fflloc;
#endif
#endif

  FFSTYPE ffval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int fflen;

#if FFDEBUG != 0
  if (ffdebug)
    fprintf(stderr, "Starting parse\n");
#endif

  ffstate = 0;
  fferrstatus = 0;
  ffnerrs = 0;
  ffchar = FFEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  ffssp = ffss - 1;
  ffvsp = ffvs;
#ifdef FFLSP_NEEDED
  fflsp = ffls;
#endif

/* Push a new state, which is found in  ffstate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
ffnewstate:

  *++ffssp = ffstate;

  if (ffssp >= ffss + ffstacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      FFSTYPE *ffvs1 = ffvs;
      short *ffss1 = ffss;
#ifdef FFLSP_NEEDED
      FFLTYPE *ffls1 = ffls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = ffssp - ffss + 1;

#ifdef ffoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef FFLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if ffoverflow is a macro.  */
      ffoverflow("parser stack overflow",
		 &ffss1, size * sizeof (*ffssp),
		 &ffvs1, size * sizeof (*ffvsp),
		 &ffls1, size * sizeof (*fflsp),
		 &ffstacksize);
#else
      ffoverflow("parser stack overflow",
		 &ffss1, size * sizeof (*ffssp),
		 &ffvs1, size * sizeof (*ffvsp),
		 &ffstacksize);
#endif

      ffss = ffss1; ffvs = ffvs1;
#ifdef FFLSP_NEEDED
      ffls = ffls1;
#endif
#else /* no ffoverflow */
      /* Extend the stack our own way.  */
      if (ffstacksize >= FFMAXDEPTH)
	{
	  fferror("parser stack overflow");
	  return 2;
	}
      ffstacksize *= 2;
      if (ffstacksize > FFMAXDEPTH)
	ffstacksize = FFMAXDEPTH;
      ffss = (short *) alloca (ffstacksize * sizeof (*ffssp));
      __ff_memcpy ((char *)ffss, (char *)ffss1, size * sizeof (*ffssp));
      ffvs = (FFSTYPE *) alloca (ffstacksize * sizeof (*ffvsp));
      __ff_memcpy ((char *)ffvs, (char *)ffvs1, size * sizeof (*ffvsp));
#ifdef FFLSP_NEEDED
      ffls = (FFLTYPE *) alloca (ffstacksize * sizeof (*fflsp));
      __ff_memcpy ((char *)ffls, (char *)ffls1, size * sizeof (*fflsp));
#endif
#endif /* no ffoverflow */

      ffssp = ffss + size - 1;
      ffvsp = ffvs + size - 1;
#ifdef FFLSP_NEEDED
      fflsp = ffls + size - 1;
#endif

#if FFDEBUG != 0
      if (ffdebug)
	fprintf(stderr, "Stack size increased to %d\n", ffstacksize);
#endif

      if (ffssp >= ffss + ffstacksize - 1)
	FFABORT;
    }

#if FFDEBUG != 0
  if (ffdebug)
    fprintf(stderr, "Entering state %d\n", ffstate);
#endif

  goto ffbackup;
 ffbackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* ffresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  ffn = ffpact[ffstate];
  if (ffn == FFFLAG)
    goto ffdefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* ffchar is either FFEMPTY or FFEOF
     or a valid token in external form.  */

  if (ffchar == FFEMPTY)
    {
#if FFDEBUG != 0
      if (ffdebug)
	fprintf(stderr, "Reading a token: ");
#endif
      ffchar = FFLEX;
    }

  /* Convert token to internal form (in ffchar1) for indexing tables with */

  if (ffchar <= 0)		/* This means end of input. */
    {
      ffchar1 = 0;
      ffchar = FFEOF;		/* Don't call FFLEX any more */

#if FFDEBUG != 0
      if (ffdebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      ffchar1 = FFTRANSLATE(ffchar);

#if FFDEBUG != 0
      if (ffdebug)
	{
	  fprintf (stderr, "Next token is %d (%s", ffchar, fftname[ffchar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef FFPRINT
	  FFPRINT (stderr, ffchar, fflval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  ffn += ffchar1;
  if (ffn < 0 || ffn > FFLAST || ffcheck[ffn] != ffchar1)
    goto ffdefault;

  ffn = fftable[ffn];

  /* ffn is what to do for this token type in this state.
     Negative => reduce, -ffn is rule number.
     Positive => shift, ffn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (ffn < 0)
    {
      if (ffn == FFFLAG)
	goto fferrlab;
      ffn = -ffn;
      goto ffreduce;
    }
  else if (ffn == 0)
    goto fferrlab;

  if (ffn == FFFINAL)
    FFACCEPT;

  /* Shift the lookahead token.  */

#if FFDEBUG != 0
  if (ffdebug)
    fprintf(stderr, "Shifting token %d (%s), ", ffchar, fftname[ffchar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (ffchar != FFEOF)
    ffchar = FFEMPTY;

  *++ffvsp = fflval;
#ifdef FFLSP_NEEDED
  *++fflsp = fflloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (fferrstatus) fferrstatus--;

  ffstate = ffn;
  goto ffnewstate;

/* Do the default action for the current state.  */
ffdefault:

  ffn = ffdefact[ffstate];
  if (ffn == 0)
    goto fferrlab;

/* Do a reduction.  ffn is the number of a rule to reduce with.  */
ffreduce:
  fflen = ffr2[ffn];
  if (fflen > 0)
    ffval = ffvsp[1-fflen]; /* implement default value of the action */

#if FFDEBUG != 0
  if (ffdebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       ffn, ffrline[ffn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = ffprhs[ffn]; ffrhs[i] > 0; i++)
	fprintf (stderr, "%s ", fftname[ffrhs[i]]);
      fprintf (stderr, " -> %s\n", fftname[ffr1[ffn]]);
    }
#endif


  switch (ffn) {

case 3:
#line 203 "eval.y"
{;
    break;}
case 4:
#line 205 "eval.y"
{ if( ffvsp[-1].Node<0 ) {
		     fferror("Couldn't build node structure: out of memory?");
		     FFERROR;  }
		;
    break;}
case 5:
#line 210 "eval.y"
{ if( ffvsp[-1].Node<0 ) {
		     fferror("Couldn't build node structure: out of memory?");
		     FFERROR;  }
		;
    break;}
case 6:
#line 215 "eval.y"
{ if( ffvsp[-1].Node<0 ) {
		     fferror("Couldn't build node structure: out of memory?");
		     FFERROR;  } 
		;
    break;}
case 7:
#line 220 "eval.y"
{ if( ffvsp[-1].Node<0 ) {
		     fferror("Couldn't build node structure: out of memory?");
		     FFERROR;  }
		;
    break;}
case 8:
#line 224 "eval.y"
{  fferrok;  ;
    break;}
case 9:
#line 228 "eval.y"
{
                  ffval.Node = New_Const( BITSTR, ffvsp[0].str, strlen(ffvsp[0].str)+1 ); TEST(ffval.Node);
		  SIZE(ffval.Node) = strlen(ffvsp[0].str);
		;
    break;}
case 10:
#line 233 "eval.y"
{ ffval.Node = New_Column( ffvsp[0].lng ); TEST(ffval.Node); ;
    break;}
case 11:
#line 235 "eval.y"
{ ffval.Node = New_BinOp( BITSTR, ffvsp[-2].Node, '&', ffvsp[0].Node ); TEST(ffval.Node);
                  SIZE(ffval.Node) = ( SIZE(ffvsp[-2].Node)>SIZE(ffvsp[0].Node) ? SIZE(ffvsp[-2].Node) : SIZE(ffvsp[0].Node) );  ;
    break;}
case 12:
#line 238 "eval.y"
{ ffval.Node = New_BinOp( BITSTR, ffvsp[-2].Node, '|', ffvsp[0].Node ); TEST(ffval.Node);
                  SIZE(ffval.Node) = ( SIZE(ffvsp[-2].Node)>SIZE(ffvsp[0].Node) ? SIZE(ffvsp[-2].Node) : SIZE(ffvsp[0].Node) );  ;
    break;}
case 13:
#line 241 "eval.y"
{ ffval.Node = New_BinOp( BITSTR, ffvsp[-2].Node, '+', ffvsp[0].Node ); TEST(ffval.Node);
                  SIZE(ffval.Node) = SIZE(ffvsp[-2].Node) + SIZE(ffvsp[0].Node);                          ;
    break;}
case 14:
#line 244 "eval.y"
{ ffval.Node = New_Unary( BITSTR, NOT, ffvsp[0].Node ); TEST(ffval.Node);     ;
    break;}
case 15:
#line 246 "eval.y"
{ ffval.Node = ffvsp[-1].Node; ;
    break;}
case 16:
#line 250 "eval.y"
{ ffval.Node = New_Const( LONG,   &(ffvsp[0].lng), sizeof(long)   ); TEST(ffval.Node); ;
    break;}
case 17:
#line 252 "eval.y"
{ ffval.Node = New_Const( DOUBLE, &(ffvsp[0].dbl), sizeof(double) ); TEST(ffval.Node); ;
    break;}
case 18:
#line 254 "eval.y"
{ ffval.Node = New_Column( ffvsp[0].lng ); TEST(ffval.Node); ;
    break;}
case 19:
#line 256 "eval.y"
{ ffval.Node = New_Func( LONG, row_fct, 0, 0, 0, 0, 0, 0, 0, 0 ); ;
    break;}
case 20:
#line 258 "eval.y"
{ PROMOTE(ffvsp[-2].Node,ffvsp[0].Node); ffval.Node = New_BinOp( TYPE(ffvsp[-2].Node), ffvsp[-2].Node, '%', ffvsp[0].Node );
		  TEST(ffval.Node);                                                ;
    break;}
case 21:
#line 261 "eval.y"
{ PROMOTE(ffvsp[-2].Node,ffvsp[0].Node); ffval.Node = New_BinOp( TYPE(ffvsp[-2].Node), ffvsp[-2].Node, '+', ffvsp[0].Node );
		  TEST(ffval.Node);                                                ;
    break;}
case 22:
#line 264 "eval.y"
{ PROMOTE(ffvsp[-2].Node,ffvsp[0].Node); ffval.Node = New_BinOp( TYPE(ffvsp[-2].Node), ffvsp[-2].Node, '-', ffvsp[0].Node ); 
		  TEST(ffval.Node);                                                ;
    break;}
case 23:
#line 267 "eval.y"
{ PROMOTE(ffvsp[-2].Node,ffvsp[0].Node); ffval.Node = New_BinOp( TYPE(ffvsp[-2].Node), ffvsp[-2].Node, '*', ffvsp[0].Node ); 
		  TEST(ffval.Node);                                                ;
    break;}
case 24:
#line 270 "eval.y"
{ PROMOTE(ffvsp[-2].Node,ffvsp[0].Node); ffval.Node = New_BinOp( TYPE(ffvsp[-2].Node), ffvsp[-2].Node, '/', ffvsp[0].Node ); 
		  TEST(ffval.Node);                                                ;
    break;}
case 25:
#line 273 "eval.y"
{ PROMOTE(ffvsp[-2].Node,ffvsp[0].Node); ffval.Node = New_BinOp( TYPE(ffvsp[-2].Node), ffvsp[-2].Node, POWER, ffvsp[0].Node );
		  TEST(ffval.Node);                                                ;
    break;}
case 26:
#line 276 "eval.y"
{ ffval.Node = New_Unary( TYPE(ffvsp[0].Node), UMINUS, ffvsp[0].Node ); TEST(ffval.Node); ;
    break;}
case 27:
#line 278 "eval.y"
{ ffval.Node = ffvsp[-1].Node; ;
    break;}
case 28:
#line 280 "eval.y"
{ ffvsp[0].Node = New_Unary( TYPE(ffvsp[-2].Node), 0, ffvsp[0].Node );
                  ffval.Node = New_BinOp( TYPE(ffvsp[-2].Node), ffvsp[-2].Node, '*', ffvsp[0].Node ); 
		  TEST(ffval.Node);                                ;
    break;}
case 29:
#line 284 "eval.y"
{ ffvsp[-2].Node = New_Unary( TYPE(ffvsp[0].Node), 0, ffvsp[-2].Node );
                  ffval.Node = New_BinOp( TYPE(ffvsp[0].Node), ffvsp[-2].Node, '*', ffvsp[0].Node );
                  TEST(ffval.Node);                                ;
    break;}
case 30:
#line 288 "eval.y"
{ if (FSTRCMP(ffvsp[-1].str,"RANDOM(") == 0) {
                     srand( (unsigned int) time(NULL) );
                     ffval.Node = New_Func( DOUBLE, rnd_fct, 0, 0, 0, 0, 0, 0, 0, 0 );
                  } else {
                     fferror("Function() not supported");
		     FFERROR;
		  }
                  TEST(ffval.Node); 
                ;
    break;}
case 31:
#line 298 "eval.y"
{ if (FSTRCMP(ffvsp[-2].str,"SUM(") == 0) {
		     ffval.Node = New_Func( LONG, sum_fct, 1, ffvsp[-1].Node, 0, 0, 0, 0, 0, 0 );
                  } else if (FSTRCMP(ffvsp[-2].str,"NELEM(") == 0) {
                     ffval.Node = New_Const( LONG, &( SIZE(ffvsp[-1].Node) ), sizeof(long) );
		  } else {
                     fferror("Function(bool) not supported");
		     FFERROR;
		  }
                  TEST(ffval.Node); 
		;
    break;}
case 32:
#line 309 "eval.y"
{ if (FSTRCMP(ffvsp[-2].str,"NELEM(") == 0) {
                     ffval.Node = New_Const( LONG, &( SIZE(ffvsp[-1].Node) ), sizeof(long) );
		  } else {
                     fferror("Function(str) not supported");
		     FFERROR;
		  }
                  TEST(ffval.Node); 
		;
    break;}
case 33:
#line 318 "eval.y"
{ if (FSTRCMP(ffvsp[-2].str,"NELEM(") == 0) {
                     ffval.Node = New_Const( LONG, &( SIZE(ffvsp[-1].Node) ), sizeof(long) );
		  } else {
                     fferror("Function(bits) not supported");
		     FFERROR;
		  }
                  TEST(ffval.Node); 
		;
    break;}
case 34:
#line 327 "eval.y"
{ if (FSTRCMP(ffvsp[-2].str,"SUM(") == 0)
		     ffval.Node = New_Func( TYPE(ffvsp[-1].Node), sum_fct, 1, ffvsp[-1].Node,
				    0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP(ffvsp[-2].str,"NELEM(") == 0)
                     ffval.Node = New_Const( LONG, &( SIZE(ffvsp[-1].Node) ), sizeof(long) );
		  else if (FSTRCMP(ffvsp[-2].str,"ABS(") == 0)
		     ffval.Node = New_Func( 0, abs_fct, 1, ffvsp[-1].Node, 0, 0, 0, 0, 0, 0 );
                  else {
		     if( TYPE(ffvsp[-1].Node) != DOUBLE ) ffvsp[-1].Node = New_Unary( DOUBLE, 0, ffvsp[-1].Node );
                     if (FSTRCMP(ffvsp[-2].str,"SIN(") == 0)
			ffval.Node = New_Func( 0, sin_fct,  1, ffvsp[-1].Node, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP(ffvsp[-2].str,"COS(") == 0)
			ffval.Node = New_Func( 0, cos_fct,  1, ffvsp[-1].Node, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP(ffvsp[-2].str,"TAN(") == 0)
			ffval.Node = New_Func( 0, tan_fct,  1, ffvsp[-1].Node, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP(ffvsp[-2].str,"ARCSIN(") == 0)
			ffval.Node = New_Func( 0, asin_fct, 1, ffvsp[-1].Node, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP(ffvsp[-2].str,"ARCCOS(") == 0)
			ffval.Node = New_Func( 0, acos_fct, 1, ffvsp[-1].Node, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP(ffvsp[-2].str,"ARCTAN(") == 0)
			ffval.Node = New_Func( 0, atan_fct, 1, ffvsp[-1].Node, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP(ffvsp[-2].str,"EXP(") == 0)
			ffval.Node = New_Func( 0, exp_fct,  1, ffvsp[-1].Node, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP(ffvsp[-2].str,"LOG(") == 0)
			ffval.Node = New_Func( 0, log_fct,  1, ffvsp[-1].Node, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP(ffvsp[-2].str,"LOG10(") == 0)
			ffval.Node = New_Func( 0, log10_fct, 1, ffvsp[-1].Node, 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP(ffvsp[-2].str,"SQRT(") == 0)
			ffval.Node = New_Func( 0, sqrt_fct, 1, ffvsp[-1].Node, 0, 0, 0, 0, 0, 0 );
		     else {
			fferror("Function(expr) not supported");
			FFERROR;
		     }
		  }
                  TEST(ffval.Node); 
                ;
    break;}
case 35:
#line 364 "eval.y"
{ 
		   if (FSTRCMP(ffvsp[-4].str,"DEFNULL(") == 0) {
		      if( SIZE(ffvsp[-3].Node)>=SIZE(ffvsp[-1].Node) && Test_Dims( ffvsp[-3].Node, ffvsp[-1].Node ) ) {
			 PROMOTE(ffvsp[-3].Node,ffvsp[-1].Node);
			 ffval.Node = New_Func( 0, defnull_fct, 2, ffvsp[-3].Node, ffvsp[-1].Node, 0,
					0, 0, 0, 0 );
			 TEST(ffval.Node); 
		      } else {
			 fferror("Dimensions of DEFNULL arguments are not compatible");
			 FFERROR;
		      }
		   } else if (FSTRCMP(ffvsp[-4].str,"ARCTAN2(") == 0) {
		     if( TYPE(ffvsp[-3].Node) != DOUBLE ) ffvsp[-3].Node = New_Unary( DOUBLE, 0, ffvsp[-3].Node );
		     if( TYPE(ffvsp[-1].Node) != DOUBLE ) ffvsp[-1].Node = New_Unary( DOUBLE, 0, ffvsp[-1].Node );
		     if( Test_Dims( ffvsp[-3].Node, ffvsp[-1].Node ) ) {
			ffval.Node = New_Func( 0, atan2_fct, 2, ffvsp[-3].Node, ffvsp[-1].Node, 0, 0, 0, 0, 0 );
			TEST(ffval.Node); 
			if( SIZE(ffvsp[-3].Node)<SIZE(ffvsp[-1].Node) ) {
			   int i;
			   gParse.Nodes[ffval.Node].value.nelem =
			      gParse.Nodes[ffvsp[-1].Node].value.nelem;
			   gParse.Nodes[ffval.Node].value.naxis =
			      gParse.Nodes[ffvsp[-1].Node].value.naxis;
			   for( i=0; i<gParse.Nodes[ffvsp[-1].Node].value.naxis; i++ )
			      gParse.Nodes[ffval.Node].value.naxes[i] =
				 gParse.Nodes[ffvsp[-1].Node].value.naxes[i];
			}
		     } else {
			fferror("Dimensions of arctan2 arguments are not compatible");
			FFERROR;
		     }
		  } else {
                     fferror("Function(expr,expr) not supported");
		     FFERROR;
		  }
                ;
    break;}
case 36:
#line 401 "eval.y"
{ ffval.Node = New_Deref( ffvsp[-3].Node, 1, ffvsp[-1].Node,  0,  0,  0,   0 ); TEST(ffval.Node); ;
    break;}
case 37:
#line 403 "eval.y"
{ ffval.Node = New_Deref( ffvsp[-5].Node, 2, ffvsp[-3].Node, ffvsp[-1].Node,  0,  0,   0 ); TEST(ffval.Node); ;
    break;}
case 38:
#line 405 "eval.y"
{ ffval.Node = New_Deref( ffvsp[-7].Node, 3, ffvsp[-5].Node, ffvsp[-3].Node, ffvsp[-1].Node,  0,   0 ); TEST(ffval.Node); ;
    break;}
case 39:
#line 407 "eval.y"
{ ffval.Node = New_Deref( ffvsp[-9].Node, 4, ffvsp[-7].Node, ffvsp[-5].Node, ffvsp[-3].Node, ffvsp[-1].Node,   0 ); TEST(ffval.Node); ;
    break;}
case 40:
#line 409 "eval.y"
{ ffval.Node = New_Deref( ffvsp[-11].Node, 5, ffvsp[-9].Node, ffvsp[-7].Node, ffvsp[-5].Node, ffvsp[-3].Node, ffvsp[-1].Node ); TEST(ffval.Node); ;
    break;}
case 41:
#line 411 "eval.y"
{ ffval.Node = New_Unary( LONG,   INTCAST, ffvsp[0].Node );  TEST(ffval.Node);  ;
    break;}
case 42:
#line 413 "eval.y"
{ ffval.Node = New_Unary( LONG,   INTCAST, ffvsp[0].Node );  TEST(ffval.Node);  ;
    break;}
case 43:
#line 415 "eval.y"
{ ffval.Node = New_Unary( DOUBLE, FLTCAST, ffvsp[0].Node );  TEST(ffval.Node);  ;
    break;}
case 44:
#line 417 "eval.y"
{ ffval.Node = New_Unary( DOUBLE, FLTCAST, ffvsp[0].Node );  TEST(ffval.Node);  ;
    break;}
case 45:
#line 421 "eval.y"
{ ffval.Node = New_Const( BOOLEAN, &(ffvsp[0].log), sizeof(char) ); TEST(ffval.Node); ;
    break;}
case 46:
#line 423 "eval.y"
{ ffval.Node = New_Column( ffvsp[0].lng ); TEST(ffval.Node); ;
    break;}
case 47:
#line 425 "eval.y"
{ ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, EQ,  ffvsp[0].Node ); TEST(ffval.Node);
		  SIZE(ffval.Node) = 1;                                     ;
    break;}
case 48:
#line 428 "eval.y"
{ ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, NE,  ffvsp[0].Node ); TEST(ffval.Node); 
		  SIZE(ffval.Node) = 1;                                     ;
    break;}
case 49:
#line 431 "eval.y"
{ ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, LT,  ffvsp[0].Node ); TEST(ffval.Node); 
		  SIZE(ffval.Node) = 1;                                     ;
    break;}
case 50:
#line 434 "eval.y"
{ ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, LTE, ffvsp[0].Node ); TEST(ffval.Node); 
		  SIZE(ffval.Node) = 1;                                     ;
    break;}
case 51:
#line 437 "eval.y"
{ ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, GT,  ffvsp[0].Node ); TEST(ffval.Node); 
		  SIZE(ffval.Node) = 1;                                     ;
    break;}
case 52:
#line 440 "eval.y"
{ ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, GTE, ffvsp[0].Node ); TEST(ffval.Node); 
		  SIZE(ffval.Node) = 1;                                     ;
    break;}
case 53:
#line 443 "eval.y"
{ PROMOTE(ffvsp[-2].Node,ffvsp[0].Node); ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, GT,  ffvsp[0].Node );
                  TEST(ffval.Node);                                               ;
    break;}
case 54:
#line 446 "eval.y"
{ PROMOTE(ffvsp[-2].Node,ffvsp[0].Node); ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, LT,  ffvsp[0].Node );
                  TEST(ffval.Node);                                               ;
    break;}
case 55:
#line 449 "eval.y"
{ PROMOTE(ffvsp[-2].Node,ffvsp[0].Node); ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, GTE, ffvsp[0].Node );
                  TEST(ffval.Node);                                               ;
    break;}
case 56:
#line 452 "eval.y"
{ PROMOTE(ffvsp[-2].Node,ffvsp[0].Node); ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, LTE, ffvsp[0].Node );
                  TEST(ffval.Node);                                               ;
    break;}
case 57:
#line 455 "eval.y"
{ PROMOTE(ffvsp[-2].Node,ffvsp[0].Node); ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, '~', ffvsp[0].Node );
                  TEST(ffval.Node);                                               ;
    break;}
case 58:
#line 458 "eval.y"
{ PROMOTE(ffvsp[-2].Node,ffvsp[0].Node); ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, EQ,  ffvsp[0].Node );
                  TEST(ffval.Node);                                               ;
    break;}
case 59:
#line 461 "eval.y"
{ PROMOTE(ffvsp[-2].Node,ffvsp[0].Node); ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, NE,  ffvsp[0].Node );
                  TEST(ffval.Node);                                               ;
    break;}
case 60:
#line 464 "eval.y"
{ ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, EQ,  ffvsp[0].Node ); TEST(ffval.Node);
                  SIZE(ffval.Node) = 1; ;
    break;}
case 61:
#line 467 "eval.y"
{ ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, NE,  ffvsp[0].Node ); TEST(ffval.Node);
                  SIZE(ffval.Node) = 1; ;
    break;}
case 62:
#line 470 "eval.y"
{ ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, AND, ffvsp[0].Node ); TEST(ffval.Node); ;
    break;}
case 63:
#line 472 "eval.y"
{ ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, OR,  ffvsp[0].Node ); TEST(ffval.Node); ;
    break;}
case 64:
#line 474 "eval.y"
{ ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, EQ,  ffvsp[0].Node ); TEST(ffval.Node); ;
    break;}
case 65:
#line 476 "eval.y"
{ ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, NE,  ffvsp[0].Node ); TEST(ffval.Node); ;
    break;}
case 66:
#line 479 "eval.y"
{ PROMOTE(ffvsp[-4].Node,ffvsp[-2].Node); PROMOTE(ffvsp[-4].Node,ffvsp[0].Node); PROMOTE(ffvsp[-2].Node,ffvsp[0].Node);
		  ffvsp[-2].Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, LTE, ffvsp[-4].Node );
                  ffvsp[0].Node = New_BinOp( BOOLEAN, ffvsp[-4].Node, LTE, ffvsp[0].Node );
                  ffval.Node = New_BinOp( BOOLEAN, ffvsp[-2].Node, AND, ffvsp[0].Node );
                  TEST(ffval.Node);                                         ;
    break;}
case 67:
#line 486 "eval.y"
{
		   if (FSTRCMP(ffvsp[-2].str,"ISNULL(") == 0) {
		      ffval.Node = New_Func( 0, isnull_fct, 1, ffvsp[-1].Node, 0, 0,
				     0, 0, 0, 0 );
		      TEST(ffval.Node); 
                      /* Use expression's size, but return BOOLEAN */
		      TYPE(ffval.Node) = BOOLEAN;
		   } else {
		      fferror("Boolean Function(expr) not supported");
		      FFERROR;
		   }
		;
    break;}
case 68:
#line 499 "eval.y"
{
		   if (FSTRCMP(ffvsp[-2].str,"ISNULL(") == 0) {
		      ffval.Node = New_Func( 0, isnull_fct, 1, ffvsp[-1].Node, 0, 0,
				     0, 0, 0, 0 );
		      TEST(ffval.Node); 
                      /* Use expression's size, but return BOOLEAN */
		      TYPE(ffval.Node) = BOOLEAN;
		   } else {
		      fferror("Boolean Function(expr) not supported");
		      FFERROR;
		   }
		;
    break;}
case 69:
#line 512 "eval.y"
{
		   if (FSTRCMP(ffvsp[-2].str,"ISNULL(") == 0) {
		      ffval.Node = New_Func( BOOLEAN, isnull_fct, 1, ffvsp[-1].Node, 0, 0,
				     0, 0, 0, 0 );
		      TEST(ffval.Node); 
		   } else {
		      fferror("Boolean Function(expr) not supported");
		      FFERROR;
		   }
		;
    break;}
case 70:
#line 523 "eval.y"
{
		   if (FSTRCMP(ffvsp[-4].str,"DEFNULL(") == 0) {
		      if( SIZE(ffvsp[-3].Node)>=SIZE(ffvsp[-1].Node) && Test_Dims( ffvsp[-3].Node, ffvsp[-1].Node ) ) {
			 ffval.Node = New_Func( 0, defnull_fct, 2, ffvsp[-3].Node, ffvsp[-1].Node, 0,
					0, 0, 0, 0 );
			 TEST(ffval.Node); 
		      } else {
			 fferror("Dimensions of DEFNULL arguments are not compatible");
			 FFERROR;
		      }
		   } else {
		      fferror("Boolean Function(expr,expr) not supported");
		      FFERROR;
		   }
		;
    break;}
case 71:
#line 539 "eval.y"
{
		   if( SIZE(ffvsp[-5].Node)>1 || SIZE(ffvsp[-3].Node)>1 || SIZE(ffvsp[-1].Node)>1 ) {
		      fferror("Cannot use array as function argument");
		      FFERROR;
		   }
		   if( TYPE(ffvsp[-5].Node) != DOUBLE ) ffvsp[-5].Node = New_Unary( DOUBLE, 0, ffvsp[-5].Node );
		   if( TYPE(ffvsp[-3].Node) != DOUBLE ) ffvsp[-3].Node = New_Unary( DOUBLE, 0, ffvsp[-3].Node );
		   if( TYPE(ffvsp[-1].Node) != DOUBLE ) ffvsp[-1].Node = New_Unary( DOUBLE, 0, ffvsp[-1].Node );
		   if (FSTRCMP(ffvsp[-6].str,"NEAR(") == 0)
		      ffval.Node = New_Func( BOOLEAN, near_fct, 3, ffvsp[-5].Node, ffvsp[-3].Node, ffvsp[-1].Node,
				     0, 0, 0, 0 );
		   else {
		      fferror("Boolean Function not supported");
		      FFERROR;
		   }
                   TEST(ffval.Node); 
		;
    break;}
case 72:
#line 557 "eval.y"
{
		   if( SIZE(ffvsp[-9].Node)>1 || SIZE(ffvsp[-7].Node)>1 || SIZE(ffvsp[-5].Node)>1 || SIZE(ffvsp[-3].Node)>1
		       || SIZE(ffvsp[-1].Node)>1 ) {
		      fferror("Cannot use array as function argument");
		      FFERROR;
		   }
		   if( TYPE(ffvsp[-9].Node) != DOUBLE ) ffvsp[-9].Node = New_Unary( DOUBLE, 0, ffvsp[-9].Node );
		   if( TYPE(ffvsp[-7].Node) != DOUBLE ) ffvsp[-7].Node = New_Unary( DOUBLE, 0, ffvsp[-7].Node );
		   if( TYPE(ffvsp[-5].Node) != DOUBLE ) ffvsp[-5].Node = New_Unary( DOUBLE, 0, ffvsp[-5].Node );
		   if( TYPE(ffvsp[-3].Node) != DOUBLE ) ffvsp[-3].Node = New_Unary( DOUBLE, 0, ffvsp[-3].Node );
		   if( TYPE(ffvsp[-1].Node)!= DOUBLE ) ffvsp[-1].Node= New_Unary( DOUBLE, 0, ffvsp[-1].Node);
                   if (FSTRCMP(ffvsp[-10].str,"CIRCLE(") == 0)
		      ffval.Node = New_Func( BOOLEAN, circle_fct, 5, ffvsp[-9].Node, ffvsp[-7].Node, ffvsp[-5].Node, ffvsp[-3].Node,
				     ffvsp[-1].Node, 0, 0 );
		   else {
		      fferror("Boolean Function not supported");
		      FFERROR;
		   }
                   TEST(ffval.Node); 
		;
    break;}
case 73:
#line 578 "eval.y"
{
		   if( SIZE(ffvsp[-13].Node)>1 || SIZE(ffvsp[-11].Node)>1 || SIZE(ffvsp[-9].Node)>1 || SIZE(ffvsp[-7].Node)>1
		       || SIZE(ffvsp[-5].Node)>1 || SIZE(ffvsp[-3].Node)>1 || SIZE(ffvsp[-1].Node)>1 ) {
		      fferror("Cannot use array as function argument");
		      FFERROR;
		   }
		   if( TYPE(ffvsp[-13].Node) != DOUBLE ) ffvsp[-13].Node = New_Unary( DOUBLE, 0, ffvsp[-13].Node );
		   if( TYPE(ffvsp[-11].Node) != DOUBLE ) ffvsp[-11].Node = New_Unary( DOUBLE, 0, ffvsp[-11].Node );
		   if( TYPE(ffvsp[-9].Node) != DOUBLE ) ffvsp[-9].Node = New_Unary( DOUBLE, 0, ffvsp[-9].Node );
		   if( TYPE(ffvsp[-7].Node) != DOUBLE ) ffvsp[-7].Node = New_Unary( DOUBLE, 0, ffvsp[-7].Node );
		   if( TYPE(ffvsp[-5].Node)!= DOUBLE ) ffvsp[-5].Node= New_Unary( DOUBLE, 0, ffvsp[-5].Node);
		   if( TYPE(ffvsp[-3].Node)!= DOUBLE ) ffvsp[-3].Node= New_Unary( DOUBLE, 0, ffvsp[-3].Node);
		   if( TYPE(ffvsp[-1].Node)!= DOUBLE ) ffvsp[-1].Node= New_Unary( DOUBLE, 0, ffvsp[-1].Node);
		   if (FSTRCMP(ffvsp[-14].str,"BOX(") == 0)
		      ffval.Node = New_Func( BOOLEAN, box_fct, 7, ffvsp[-13].Node, ffvsp[-11].Node, ffvsp[-9].Node, ffvsp[-7].Node,
				      ffvsp[-5].Node, ffvsp[-3].Node, ffvsp[-1].Node );
		   else if (FSTRCMP(ffvsp[-14].str,"ELLIPSE(") == 0)
		      ffval.Node = New_Func( BOOLEAN, elps_fct, 7, ffvsp[-13].Node, ffvsp[-11].Node, ffvsp[-9].Node, ffvsp[-7].Node,
				      ffvsp[-5].Node, ffvsp[-3].Node, ffvsp[-1].Node );
		   else {
		      fferror("SAO Image Function not supported");
		      FFERROR;
		   }
                   TEST(ffval.Node); 
		;
    break;}
case 74:
#line 605 "eval.y"
{ /* Use defaults for all elements */
                  ffval.Node = New_GTI( "", -99, "*START*", "*STOP*" );
                  TEST(ffval.Node);                                         ;
    break;}
case 75:
#line 609 "eval.y"
{ /* Use defaults for all except filename */
                   ffval.Node = New_GTI( ffvsp[-1].str, -99, "*START*", "*STOP*" );
                   TEST(ffval.Node);                                        ;
    break;}
case 76:
#line 613 "eval.y"
{ ffval.Node = New_GTI( ffvsp[-3].str, ffvsp[-1].Node, "*START*", "*STOP*" );
                  TEST(ffval.Node);                                         ;
    break;}
case 77:
#line 616 "eval.y"
{  ffval.Node = New_GTI( ffvsp[-7].str, ffvsp[-5].Node, ffvsp[-3].str, ffvsp[-1].str );
                   TEST(ffval.Node);                                        ;
    break;}
case 78:
#line 620 "eval.y"
{ /* Use defaults for all except filename */
                   ffval.Node = New_REG( ffvsp[-1].str, -99, -99, "" );
                   TEST(ffval.Node);                                        ;
    break;}
case 79:
#line 624 "eval.y"
{  ffval.Node = New_REG( ffvsp[-5].str, ffvsp[-3].Node, ffvsp[-1].Node, "" );
                   TEST(ffval.Node);                                        ;
    break;}
case 80:
#line 627 "eval.y"
{  ffval.Node = New_REG( ffvsp[-7].str, ffvsp[-5].Node, ffvsp[-3].Node, ffvsp[-1].str );
                   TEST(ffval.Node);                                        ;
    break;}
case 81:
#line 631 "eval.y"
{ ffval.Node = New_Deref( ffvsp[-3].Node, 1, ffvsp[-1].Node,  0,  0,  0,   0 ); TEST(ffval.Node); ;
    break;}
case 82:
#line 633 "eval.y"
{ ffval.Node = New_Deref( ffvsp[-5].Node, 2, ffvsp[-3].Node, ffvsp[-1].Node,  0,  0,   0 ); TEST(ffval.Node); ;
    break;}
case 83:
#line 635 "eval.y"
{ ffval.Node = New_Deref( ffvsp[-7].Node, 3, ffvsp[-5].Node, ffvsp[-3].Node, ffvsp[-1].Node,  0,   0 ); TEST(ffval.Node); ;
    break;}
case 84:
#line 637 "eval.y"
{ ffval.Node = New_Deref( ffvsp[-9].Node, 4, ffvsp[-7].Node, ffvsp[-5].Node, ffvsp[-3].Node, ffvsp[-1].Node,   0 ); TEST(ffval.Node); ;
    break;}
case 85:
#line 639 "eval.y"
{ ffval.Node = New_Deref( ffvsp[-11].Node, 5, ffvsp[-9].Node, ffvsp[-7].Node, ffvsp[-5].Node, ffvsp[-3].Node, ffvsp[-1].Node ); TEST(ffval.Node); ;
    break;}
case 86:
#line 641 "eval.y"
{ ffval.Node = New_Unary( BOOLEAN, NOT, ffvsp[0].Node ); TEST(ffval.Node); ;
    break;}
case 87:
#line 643 "eval.y"
{ ffval.Node = ffvsp[-1].Node; ;
    break;}
case 88:
#line 647 "eval.y"
{ ffval.Node = New_Const( STRING, ffvsp[0].str, strlen(ffvsp[0].str)+1 ); TEST(ffval.Node);
                  SIZE(ffval.Node) = strlen(ffvsp[0].str);                            ;
    break;}
case 89:
#line 650 "eval.y"
{ ffval.Node = New_Column( ffvsp[0].lng ); TEST(ffval.Node); ;
    break;}
case 90:
#line 652 "eval.y"
{ ffval.Node = ffvsp[-1].Node; ;
    break;}
case 91:
#line 654 "eval.y"
{ ffval.Node = New_BinOp( STRING, ffvsp[-2].Node, '+', ffvsp[0].Node );  TEST(ffval.Node);
		  SIZE(ffval.Node) = SIZE(ffvsp[-2].Node) + SIZE(ffvsp[0].Node);                   ;
    break;}
case 92:
#line 657 "eval.y"
{ 
		  if (FSTRCMP(ffvsp[-4].str,"DEFNULL(") == 0) {
		     ffval.Node = New_Func( 0, defnull_fct, 2, ffvsp[-3].Node, ffvsp[-1].Node, 0,
				    0, 0, 0, 0 );
		     TEST(ffval.Node); 
		     if( SIZE(ffvsp[-1].Node)>SIZE(ffvsp[-3].Node) ) SIZE(ffval.Node) = SIZE(ffvsp[-1].Node);
		  }
		;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 498 "/usr/local/share/bison.simple"

  ffvsp -= fflen;
  ffssp -= fflen;
#ifdef FFLSP_NEEDED
  fflsp -= fflen;
#endif

#if FFDEBUG != 0
  if (ffdebug)
    {
      short *ssp1 = ffss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != ffssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++ffvsp = ffval;

#ifdef FFLSP_NEEDED
  fflsp++;
  if (fflen == 0)
    {
      fflsp->first_line = fflloc.first_line;
      fflsp->first_column = fflloc.first_column;
      fflsp->last_line = (fflsp-1)->last_line;
      fflsp->last_column = (fflsp-1)->last_column;
      fflsp->text = 0;
    }
  else
    {
      fflsp->last_line = (fflsp+fflen-1)->last_line;
      fflsp->last_column = (fflsp+fflen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  ffn = ffr1[ffn];

  ffstate = ffpgoto[ffn - FFNTBASE] + *ffssp;
  if (ffstate >= 0 && ffstate <= FFLAST && ffcheck[ffstate] == *ffssp)
    ffstate = fftable[ffstate];
  else
    ffstate = ffdefgoto[ffn - FFNTBASE];

  goto ffnewstate;

fferrlab:   /* here on detecting error */

  if (! fferrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++ffnerrs;

#ifdef FFERROR_VERBOSE
      ffn = ffpact[ffstate];

      if (ffn > FFFLAG && ffn < FFLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -ffn if nec to avoid negative indexes in ffcheck.  */
	  for (x = (ffn < 0 ? -ffn : 0);
	       x < (sizeof(fftname) / sizeof(char *)); x++)
	    if (ffcheck[x + ffn] == x)
	      size += strlen(fftname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (ffn < 0 ? -ffn : 0);
		       x < (sizeof(fftname) / sizeof(char *)); x++)
		    if (ffcheck[x + ffn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, fftname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      fferror(msg);
	      free(msg);
	    }
	  else
	    fferror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* FFERROR_VERBOSE */
	fferror("parse error");
    }

  goto fferrlab1;
fferrlab1:   /* here on error raised explicitly by an action */

  if (fferrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (ffchar == FFEOF)
	FFABORT;

#if FFDEBUG != 0
      if (ffdebug)
	fprintf(stderr, "Discarding token %d (%s).\n", ffchar, fftname[ffchar1]);
#endif

      ffchar = FFEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  fferrstatus = 3;		/* Each real token shifted decrements this */

  goto fferrhandle;

fferrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  ffn = ffdefact[ffstate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (ffn) goto ffdefault;
#endif

fferrpop:   /* pop the current state because it cannot handle the error token */

  if (ffssp == ffss) FFABORT;
  ffvsp--;
  ffstate = *--ffssp;
#ifdef FFLSP_NEEDED
  fflsp--;
#endif

#if FFDEBUG != 0
  if (ffdebug)
    {
      short *ssp1 = ffss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != ffssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

fferrhandle:

  ffn = ffpact[ffstate];
  if (ffn == FFFLAG)
    goto fferrdefault;

  ffn += FFTERROR;
  if (ffn < 0 || ffn > FFLAST || ffcheck[ffn] != FFTERROR)
    goto fferrdefault;

  ffn = fftable[ffn];
  if (ffn < 0)
    {
      if (ffn == FFFLAG)
	goto fferrpop;
      ffn = -ffn;
      goto ffreduce;
    }
  else if (ffn == 0)
    goto fferrpop;

  if (ffn == FFFINAL)
    FFACCEPT;

#if FFDEBUG != 0
  if (ffdebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++ffvsp = fflval;
#ifdef FFLSP_NEEDED
  *++fflsp = fflloc;
#endif

  ffstate = ffn;
  goto ffnewstate;
}
#line 667 "eval.y"


/*************************************************************************/
/*  Start of "New" routines which build the expression Nodal structure   */
/*************************************************************************/

static int Alloc_Node( void )
{
                      /* Use this for allocation to guarantee *Nodes */
   Node *newNodePtr;  /* survives on failure, making it still valid  */
                      /* while working our way out of this error     */

   if( gParse.nNodes == gParse.nNodesAlloc ) {
      if( gParse.Nodes ) {
	 gParse.nNodesAlloc += gParse.nNodesAlloc;
	 newNodePtr = (Node *)realloc( gParse.Nodes,
				       sizeof(Node)*gParse.nNodesAlloc );
      } else {
	 gParse.nNodesAlloc = 100;
	 newNodePtr = (Node *)malloc ( sizeof(Node)*gParse.nNodesAlloc );
      }	 

      if( newNodePtr ) {
	 gParse.Nodes = newNodePtr;
      } else {
	 gParse.status = MEMORY_ALLOCATION;
	 return( -1 );
      }
   }

   return ( gParse.nNodes++ );
}

static void Free_Last_Node( void )
{
   if( gParse.nNodes ) gParse.nNodes--;
}

static int New_Const( int returnType, void *value, long len )
{
   Node *this;
   int n;

   n = Alloc_Node();
   if( n>=0 ) {
      this             = gParse.Nodes + n;
      this->operation  = CONST_OP;             /* Flag a constant */
      this->DoOp       = NULL;
      this->nSubNodes  = 0;
      this->type       = returnType;
      memcpy( &(this->value.data), value, len );
      this->value.undef = NULL;
      this->value.nelem = 1;
      this->value.naxis = 1;
      this->value.naxes[0] = 1;
   }
   return(n);
}

static int New_Column( int ColNum )
{
   Node *this;
   int  n, i;

   n = Alloc_Node();
   if( n>=0 ) {
      this              = gParse.Nodes + n;
      this->operation   = -ColNum;
      this->DoOp        = NULL;
      this->nSubNodes   = 0;
      this->type        = gParse.colInfo[ColNum].type;
      this->value.nelem = gParse.colInfo[ColNum].nelem;
      this->value.naxis = gParse.colInfo[ColNum].naxis;
      for( i=0; i<gParse.colInfo[ColNum].naxis; i++ )
	 this->value.naxes[i] = gParse.colInfo[ColNum].naxes[i];
   }
   return(n);
}

static int New_Unary( int returnType, int Op, int Node1 )
{
   Node *this, *that;
   int  i,n;

   if( Node1<0 ) return(-1);
   that = gParse.Nodes + Node1;

   if( !Op ) Op = returnType;

   if( (Op==DOUBLE || Op==FLTCAST) && that->type==DOUBLE  ) return( Node1 );
   if( (Op==LONG   || Op==INTCAST) && that->type==LONG    ) return( Node1 );
   if( (Op==BOOLEAN              ) && that->type==BOOLEAN ) return( Node1 );
   
   n = Alloc_Node();
   if( n>=0 ) {
      this              = gParse.Nodes + n;
      this->operation   = Op;
      this->DoOp        = Do_Unary;
      this->nSubNodes   = 1;
      this->SubNodes[0] = Node1;
      this->type        = returnType;

      that              = gParse.Nodes + Node1; /* Reset in case .Nodes mv'd */
      this->value.nelem = that->value.nelem;
      this->value.naxis = that->value.naxis;
      for( i=0; i<that->value.naxis; i++ )
	 this->value.naxes[i] = that->value.naxes[i];

      if( that->operation==CONST_OP ) this->DoOp( this );
   }
   return( n );
}

static int New_BinOp( int returnType, int Node1, int Op, int Node2 )
{
   Node *this,*that1,*that2;
   int  n,i,constant;

   if( Node1<0 || Node2<0 ) return(-1);

   n = Alloc_Node();
   if( n>=0 ) {
      this             = gParse.Nodes + n;
      this->operation  = Op;
      this->nSubNodes  = 2;
      this->SubNodes[0]= Node1;
      this->SubNodes[1]= Node2;
      this->type       = returnType;

      that1            = gParse.Nodes + Node1;
      that2            = gParse.Nodes + Node2;
      constant         = (that1->operation==CONST_OP
                          && that2->operation==CONST_OP);
      if( that1->type!=STRING && that1->type!=BITSTR )
	 if( !Test_Dims( Node1, Node2 ) ) {
	    Free_Last_Node();
	    ffpmsg("Array sizes/dims do not match for binary operator");
	    return(-1);
	 }
      if( that1->value.nelem == 1 ) that1 = that2;

      this->value.nelem = that1->value.nelem;
      this->value.naxis = that1->value.naxis;
      for( i=0; i<that1->value.naxis; i++ )
	 this->value.naxes[i] = that1->value.naxes[i];

	 /*  Both subnodes should be of same time  */
      switch( that1->type ) {
      case BITSTR:  this->DoOp = Do_BinOp_bit;  break;
      case STRING:  this->DoOp = Do_BinOp_str;  break;
      case BOOLEAN: this->DoOp = Do_BinOp_log;  break;
      case LONG:    this->DoOp = Do_BinOp_lng;  break;
      case DOUBLE:  this->DoOp = Do_BinOp_dbl;  break;
      }
      if( constant ) this->DoOp( this );
   }
   return( n );
}

static int New_Func( int returnType, funcOp Op, int nNodes,
		     int Node1, int Node2, int Node3, int Node4, 
		     int Node5, int Node6, int Node7 )
/* If returnType==0 , use Node1's type and vector sizes as returnType, */
/* else return a single value of type returnType                       */
{
   Node *this, *that;
   int  i,n,constant;

   if( Node1<0 || Node2<0 || Node3<0 || Node4<0 || 
       Node5<0 || Node6<0 || Node7<0 ) return(-1);

   n = Alloc_Node();
   if( n>=0 ) {
      this              = gParse.Nodes + n;
      this->operation   = (int)Op;
      this->DoOp        = Do_Func;
      this->nSubNodes   = nNodes;
      this->SubNodes[0] = Node1;
      this->SubNodes[1] = Node2;
      this->SubNodes[2] = Node3;
      this->SubNodes[3] = Node4;
      this->SubNodes[4] = Node5;
      this->SubNodes[5] = Node6;
      this->SubNodes[6] = Node7;
      i = constant = nNodes;    /* Functions with zero params are not const */
      while( i-- )
         constant = ( constant &&
		      gParse.Nodes[ this->SubNodes[i] ].operation==CONST_OP );
      
      if( returnType ) {
	 this->type           = returnType;
	 this->value.nelem    = 1;
	 this->value.naxis    = 1;
	 this->value.naxes[0] = 1;
      } else {
	 that              = gParse.Nodes + Node1;
	 this->type        = that->type;
	 this->value.nelem = that->value.nelem;
	 this->value.naxis = that->value.naxis;
	 for( i=0; i<that->value.naxis; i++ )
	    this->value.naxes[i] = that->value.naxes[i];
      }
      if( constant ) this->DoOp( this );
   }
   return( n );
}

static int New_Deref( int Var,  int nDim,
		      int Dim1, int Dim2, int Dim3, int Dim4, int Dim5 )
{
   int n, idx, constant;
   long elem=0;
   Node *this, *theVar, *theDim[MAXDIMS];

   if( Var<0 || Dim1<0 || Dim2<0 || Dim3<0 || Dim4<0 || Dim5<0 ) return(-1);

   theVar = gParse.Nodes + Var;
   if( theVar->operation==CONST_OP || theVar->value.nelem==1 ) {
      ffpmsg("Cannot index a scalar value");
      gParse.status = PARSE_SYNTAX_ERR;
      return(-1);
   }

   n = Alloc_Node();
   if( n>=0 ) {
      this              = gParse.Nodes + n;
      this->nSubNodes   = nDim+1;
      theVar            = gParse.Nodes + (this->SubNodes[0]=Var);
      theDim[0]         = gParse.Nodes + (this->SubNodes[1]=Dim1);
      theDim[1]         = gParse.Nodes + (this->SubNodes[2]=Dim2);
      theDim[2]         = gParse.Nodes + (this->SubNodes[3]=Dim3);
      theDim[3]         = gParse.Nodes + (this->SubNodes[4]=Dim4);
      theDim[4]         = gParse.Nodes + (this->SubNodes[5]=Dim5);
      constant          = theVar->operation==CONST_OP;
      for( idx=0; idx<nDim; idx++ )
	 constant = (constant && theDim[idx]->operation==CONST_OP);

      for( idx=0; idx<nDim; idx++ )
	 if( theDim[idx]->value.nelem>1 ) {
	    Free_Last_Node();
	    ffpmsg("Cannot use an array as an index value");
	    gParse.status = PARSE_SYNTAX_ERR;
	    return(-1);
	 } else if( theDim[idx]->type!=LONG ) {
	    Free_Last_Node();
	    fferror("Index value must be an integer type");
	    return(-1);
	 }

      this->operation   = '[';
      this->DoOp        = Do_Deref;
      this->type        = theVar->type;

      if( theVar->value.naxis == nDim ) { /* All dimensions specified */
	 this->value.nelem    = 1;
	 this->value.naxis    = 1;
	 this->value.naxes[0] = 1;
      } else if( nDim==1 ) { /* Dereference only one dimension */
	 elem=1;
	 this->value.naxis = theVar->value.naxis-1;
	 for( idx=0; idx<this->value.naxis; idx++ ) {
	    elem *= ( this->value.naxes[idx] = theVar->value.naxes[idx] );
	 }
	 this->value.nelem = elem;
      } else {
	 Free_Last_Node();
	 fferror("Must specify just one or all indices for vector");
	 return(-1);
      }
      if( constant ) this->DoOp( this );
   }
   return(n);
}

static int New_GTI( char *fname, int Node1, char *start, char *stop )
{
   fitsfile *fptr;
   Node *this, *that0, *that1;
   int  type,i,n, startCol, stopCol, Node0, colnum;
   int  hdutype, hdunum, evthdu, samefile, extvers, movetotype, tstat;
   char extname[100];
   long nrows;
   double timeZeroI[2], timeZeroF[2], dt, timeSpan;

   if( Node1==-99 ) {
      type = ffbuildcolumn( "TIME", &colnum );
      if( type==LONG || type==DOUBLE ) {
	 Node1 = New_Column( colnum );
      } else {
	 fferror("Could not build TIME column for GTIFILTER");
	 return(-1);
      }
   }
   Node1 = New_Unary( DOUBLE, 0, Node1 );
   Node0 = Alloc_Node(); /* This will hold the START/STOP times */
   if( Node1<0 || Node0<0 ) return(-1);

   /*  Record current HDU number in case we need to move within this file  */

   fptr = gParse.def_fptr;
   ffghdn( fptr, &evthdu );

   /*  Look for TIMEZERO keywords in current extension  */

   tstat = 0;
   if( ffgkyd( fptr, "TIMEZERO", timeZeroI, NULL, &tstat ) ) {
      tstat = 0;
      if( ffgkyd( fptr, "TIMEZERI", timeZeroI, NULL, &tstat ) ) {
	 timeZeroI[0] = timeZeroF[0] = 0.0;
      } else if( ffgkyd( fptr, "TIMEZERF", timeZeroF, NULL, &tstat ) ) {
	 timeZeroF[0] = 0.0;
      }
   } else {
      timeZeroF[0] = 0.0;
   }

   /*  Resolve filename parameter  */

   switch( fname[0] ) {
   case '\0':
      samefile = 1;
      hdunum = 1;
      break;
   case '[':
      samefile = 1;
      i = 1;
      while( fname[i] != '\0' && fname[i] != ']' ) i++;
      if( fname[i] ) {
	 fname[i] = '\0';
	 fname++;
	 ffexts( fname, &hdunum, extname, &extvers, &movetotype,
		 &gParse.status );
         if( *extname ) {
	    ffmnhd( fptr, movetotype, extname, extvers, &gParse.status );
	    ffghdn( fptr, &hdunum );
	 } else if( hdunum ) {
	    ffmahd( fptr, ++hdunum, &hdutype, &gParse.status );
	 } else if( !gParse.status ) {
	    fferror("Cannot use primary array for GTI filter");
	    return( -1 );
	 }
      } else {
	 fferror("File extension specifier lacks closing ']'");
	 return( -1 );
      }
      break;
   case '+':
      samefile = 1;
      hdunum = atoi( fname ) + 1;
      if( hdunum>1 )
	 ffmahd( fptr, hdunum, &hdutype, &gParse.status );
      else {
	 fferror("Cannot use primary array for GTI filter");
	 return( -1 );
      }
      break;
   default:
      samefile = 0;
      if( ! ffopen( &fptr, fname, READONLY, &gParse.status ) )
	 ffghdn( fptr, &hdunum );
      break;
   }
   if( gParse.status ) return(-1);

   /*  If at primary, search for GTI extension  */

   if( hdunum==1 ) {
      while( 1 ) {
	 hdunum++;
	 if( ffmahd( fptr, hdunum, &hdutype, &gParse.status ) ) break;
	 if( hdutype==IMAGE_HDU ) continue;
	 tstat = 0;
	 if( ffgkys( fptr, "EXTNAME", extname, NULL, &tstat ) ) continue;
	 ffupch( extname );
	 if( strstr( extname, "GTI" ) ) break;
      }
      if( gParse.status ) {
	 if( gParse.status==END_OF_FILE )
	    fferror("GTI extension not found in this file");
	 return(-1);
      }
   }

   /*  Locate START/STOP Columns  */

   ffgcno( fptr, CASEINSEN, start, &startCol, &gParse.status );
   ffgcno( fptr, CASEINSEN, stop,  &stopCol,  &gParse.status );
   if( gParse.status ) return(-1);

   /*  Look for TIMEZERO keywords in GTI extension  */

   tstat = 0;
   if( ffgkyd( fptr, "TIMEZERO", timeZeroI+1, NULL, &tstat ) ) {
      tstat = 0;
      if( ffgkyd( fptr, "TIMEZERI", timeZeroI+1, NULL, &tstat ) ) {
	 timeZeroI[1] = timeZeroF[1] = 0.0;
      } else if( ffgkyd( fptr, "TIMEZERF", timeZeroF+1, NULL, &tstat ) ) {
	 timeZeroF[1] = 0.0;
      }
   } else {
      timeZeroF[1] = 0.0;
   }

   n = Alloc_Node();
   if( n >= 0 ) {
      this                 = gParse.Nodes + n;
      this->nSubNodes      = 2;
      this->SubNodes[1]    = Node1;
      this->operation      = (int)gtifilt_fct;
      this->DoOp           = Do_GTI;
      this->type           = BOOLEAN;
      that1                = gParse.Nodes + Node1;
      this->value.nelem    = that1->value.nelem;
      this->value.naxis    = that1->value.naxis;
      for( i=0; i < that1->value.naxis; i++ )
	 this->value.naxes[i] = that1->value.naxes[i];

      /* Init START/STOP node to be treated as a "constant" */

      this->SubNodes[0]    = Node0;
      that0                = gParse.Nodes + Node0;
      that0->operation     = CONST_OP;
      that0->DoOp          = NULL;
      that0->value.data.ptr= NULL;

      /*  Read in START/STOP times  */

      if( ffgkyj( fptr, "NAXIS2", &nrows, NULL, &gParse.status ) )
	 return(-1);
      that0->value.nelem = nrows;
      if( nrows ) {

	 that0->value.data.dblptr = (double*)malloc( 2*nrows*sizeof(double) );
	 if( !that0->value.data.dblptr ) {
	    gParse.status = MEMORY_ALLOCATION;
	    return(-1);
	 }
	 
	 ffgcvd( fptr, startCol, 1L, 1L, nrows, 0.0,
		 that0->value.data.dblptr, &i, &gParse.status );
	 ffgcvd( fptr, stopCol, 1L, 1L, nrows, 0.0,
		 that0->value.data.dblptr+nrows, &i, &gParse.status );
	 if( gParse.status ) {
	    free( that0->value.data.dblptr );
	    return(-1);
	 }

	 /*  Test for fully time-ordered GTI... both START && STOP  */

	 that0->type = 1; /*  Assume yes  */
	 i = nrows;
	 while( --i )
	    if(    that0->value.data.dblptr[i-1]
                   >= that0->value.data.dblptr[i]
		|| that0->value.data.dblptr[i-1+nrows]
		   >= that0->value.data.dblptr[i+nrows] ) {
	       that0->type = 0;
	       break;
	    }
	 
	 /*  Handle TIMEZERO offset, if any  */
	 
	 dt = (timeZeroI[1] - timeZeroI[0]) + (timeZeroF[1] - timeZeroF[0]);
	 timeSpan = that0->value.data.dblptr[nrows+nrows-1]
	    - that0->value.data.dblptr[0];
	 
	 if( fabs( dt / timeSpan ) > 1e-12 ) {
	    for( i=0; i<(nrows+nrows); i++ )
	       that0->value.data.dblptr[i] += dt;
	 }
      }
      if( gParse.Nodes[Node1].operation==CONST_OP )
	 this->DoOp( this );
   }

   if( samefile )
      ffmahd( fptr, evthdu, &hdutype, &gParse.status );
   else
      ffclos( fptr, &gParse.status );

   return( n );
}

static int New_REG( char *fname, int NodeX, int NodeY, char *colNames )
{
   Node *this, *that0;
   int  type, n, Node0;
   int  colnum, Xcol, Ycol, tstat;
   WCSdata wcs;
   SAORegion *Rgn;
   char *cX, *cY;

   if( NodeX==-99 ) {
      type = ffbuildcolumn( "X", &colnum );
      if( type==LONG || type==DOUBLE ) {
	 NodeX = New_Column( colnum );
      } else {
	 fferror("Could not build X column for REGFILTER");
	 return(-1);
      }
   }
   if( NodeY==-99 ) {
      type = ffbuildcolumn( "Y", &colnum );
      if( type==LONG || type==DOUBLE ) {
	 NodeY = New_Column( colnum );
      } else {
	 fferror("Could not build Y column for REGFILTER");
	 return(-1);
      }
   }
   NodeX = New_Unary( DOUBLE, 0, NodeX );
   NodeY = New_Unary( DOUBLE, 0, NodeY );
   Node0 = Alloc_Node(); /* This will hold the Region Data */
   if( NodeX<0 || NodeY<0 || Node0<0 ) return(-1);

   n = Alloc_Node();
   if( n >= 0 ) {
      this                 = gParse.Nodes + n;
      this->nSubNodes      = 3;
      this->SubNodes[0]    = Node0;
      this->SubNodes[1]    = NodeX;
      this->SubNodes[2]    = NodeY;
      this->operation      = (int)regfilt_fct;
      this->DoOp           = Do_REG;
      this->type           = BOOLEAN;
      this->value.nelem    = 1;
      this->value.naxis    = 1;
      this->value.naxes[0] = 1;

      /* Init Region node to be treated as a "constant" */

      that0                = gParse.Nodes + Node0;
      that0->operation     = CONST_OP;
      that0->DoOp          = NULL;

      /*  Identify what columns to use for WCS information  */

      Xcol = Ycol = 0;
      if( *colNames ) {
	 /*  Use the column names in this string for WCS info  */
	 while( *colNames==' ' ) colNames++;
	 cX = cY = colNames;
	 while( *cY && *cY!=' ' && *cY!=',' ) cY++;
	 if( *cY )
	    *(cY++) = '\0';
	 while( *cY==' ' ) cY++;
	 if( !*cY ) {
	    fferror("Could not extract valid pair of column names from REGFILTER");
	    Free_Last_Node();
	    return( -1 );
	 }
	 fits_get_colnum( gParse.def_fptr, CASEINSEN, cX, &Xcol,
			  &gParse.status );
	 fits_get_colnum( gParse.def_fptr, CASEINSEN, cY, &Ycol,
			  &gParse.status );
	 if( gParse.status ) {
	    fferror("Could not locate columns indicated for WCS info");
	    Free_Last_Node();
	    return( -1 );
	 }

      } else {
	 /*  Try to find columns used in X/Y expressions  */
	 Xcol = Locate_Col( gParse.Nodes + NodeX );
	 Ycol = Locate_Col( gParse.Nodes + NodeY );
	 if( Xcol<0 || Ycol<0 ) {
	    fferror("Found multiple X/Y column references in REGFILTER");
	    Free_Last_Node();
	    return( -1 );
	 }
      }

      /*  Now, get the WCS info, if it exists, from the indicated columns  */
      wcs.exists = 0;
      if( Xcol>0 && Ycol>0 ) {
	 tstat = 0;
	 ffgtcs( gParse.def_fptr, Xcol, Ycol,
		 &wcs.xrefval, &wcs.yrefval,
		 &wcs.xrefpix, &wcs.yrefpix,
		 &wcs.xinc,    &wcs.yinc,
		 &wcs.rot,      wcs.type,
		 &tstat );
	 if( tstat==NO_WCS_KEY ) {
	    wcs.exists = 0;
	 } else if( tstat ) {
	    gParse.status = tstat;
	    Free_Last_Node();
	    return( -1 );
	 } else {
	    wcs.exists = 1;
	 }
      }

      /*  Read in Region file  */

      fits_read_rgnfile( fname, &wcs, &Rgn, &gParse.status );
      if( gParse.status ) {
	 Free_Last_Node();
	 return( -1 );
      }

      that0->value.data.ptr = Rgn;

      if( gParse.Nodes[NodeX].operation==CONST_OP
	  && gParse.Nodes[NodeY].operation==CONST_OP )
	 this->DoOp( this );
   }

   return( n );
}

static int Locate_Col( Node *this )
/*  Locate the TABLE column number of any columns in "this" calculation.  */
/*  Return ZERO if none found, or negative if more than 1 found.          */
{
   Node *that;
   int  i, col=0, newCol, nfound=0;
   
   for( i=0; i<this->nSubNodes; i++ ) {
      that = gParse.Nodes + this->SubNodes[i];
      if( that->operation>0 ) {
	 newCol = Locate_Col( that );
	 if( newCol<=0 ) {
	    nfound += -newCol;
	 } else {
	    if( !nfound ) {
	       col = newCol;
	       nfound++;
	    } else if( col != newCol ) {
	       nfound++;
	    }
	 }
      } else if( that->operation!=CONST_OP ) {
	 /*  Found a Column  */
	 newCol = gParse.colData[- that->operation].colnum;
	 if( !nfound ) {
	    col = newCol;
	    nfound++;
	 } else if( col != newCol ) {
	    nfound++;
	 }
      }
   }
   if( nfound!=1 )
      return( - nfound );
   else
      return( col );
}

static int Test_Dims( int Node1, int Node2 )
{
   Node *that1, *that2;
   int valid, i;

   if( Node1<0 || Node2<0 ) return(0);

   that1 = gParse.Nodes + Node1;
   that2 = gParse.Nodes + Node2;

   if( that1->value.nelem==1 || that2->value.nelem==1 )
      valid = 1;
   else if( that1->type==that2->type
	    && that1->value.nelem==that2->value.nelem
	    && that1->value.naxis==that2->value.naxis ) {
      valid = 1;
      for( i=0; i<that1->value.naxis; i++ ) {
	 if( that1->value.naxes[i]!=that2->value.naxes[i] )
	    valid = 0;
      }
   } else
      valid = 0;
   return( valid );
}   

/********************************************************************/
/*    Routines for actually evaluating the expression start here    */
/********************************************************************/

void Evaluate_Node( int thisNode )
    /**********************************************************************/
    /*  Recursively evaluate thisNode's subNodes, then call one of the    */
    /*  Do_<Action> functions pointed to by thisNode's DoOp element.      */
    /**********************************************************************/
{
   Node *this;
   int i;
   
   if( gParse.status ) return;

   this = gParse.Nodes + thisNode;
   if( this->operation>0 ) {  /* <=0 indicate constants and columns */
      i = this->nSubNodes;
      while( i-- ) {
	 Evaluate_Node( this->SubNodes[i] );
	 if( gParse.status ) return;
      }
      this->DoOp( this );
   }
}

void Reset_Parser( long firstRow, long rowOffset, long nRows )
    /***********************************************************************/
    /*  Reset the parser for processing another batch of data...           */
    /*    firstRow:  Row number of the first element of the iterCol.array  */
    /*    rowOffset: How many rows of iterCol.array should be skipped      */
    /*    nRows:     Number of rows to be processed                        */
    /*  Then, allocate and initialize the necessary UNDEF arrays for each  */
    /*  column used by the parser.  Finally, initialize each COLUMN node   */
    /*  so that its UNDEF and DATA pointers point to the appropriate       */
    /*  column arrays.                                                     */
    /***********************************************************************/
{
   int     i, column;
   long    nelem, len, row, offset, idx;
   char  **bitStrs;
   char  **sptr;
   char   *barray;
   long   *iarray;
   double *rarray;

   gParse.nRows    = nRows;
   gParse.firstRow = firstRow + rowOffset;

   /*  Resize and fill in UNDEF arrays for each column  */

   for( i=0; i<gParse.nCols; i++ ) {
      if( gParse.colData[i].iotype == OutputCol ) continue;

      nelem  = gParse.colInfo[i].nelem;
      len    = nelem * nRows;
      offset = nelem * rowOffset + 1; /* Skip initial NULLVAL in [0] elem */

      switch ( gParse.colInfo[i].type ) {
      case BITSTR:
      /* No need for UNDEF array, but must make string DATA array */
	 len = (nelem+1)*nRows;   /* Count '\0' */
	 bitStrs = ((char***)gParse.colNulls)[i];
	 if( bitStrs ) free( bitStrs[0] );
	 free( bitStrs );
	 bitStrs = (char**)malloc( nRows*sizeof(char*) );
	 if( bitStrs==NULL ) {
	    gParse.status = MEMORY_ALLOCATION;
	    break;
	 }
	 bitStrs[0] = (char*)malloc( len*sizeof(char) );
	 if( bitStrs[0]==NULL ) {
	    free( bitStrs );
	    gParse.colNulls[i] = NULL;
	    gParse.status = MEMORY_ALLOCATION;
	    break;
	 }

	 for( row=0; row<gParse.nRows; row++ ) {
	    bitStrs[row] = bitStrs[0] + row*(nelem+1);
	    idx = (row+rowOffset)*( (nelem+7)/8 ) + 1;
	    for(len=0; len<nelem; len++) {
	       if( ((char*)gParse.colData[i].array)[idx] & (1<<(7-len%8)) )
		  bitStrs[row][len] = '1';
	       else
		  bitStrs[row][len] = '0';
	       if( len%8==7 ) idx++;
	    }
	    bitStrs[row][len] = '\0';
	 }
	 gParse.colNulls[i] = (char*)bitStrs;
	 break;
      case STRING:
	 sptr = (char**)gParse.colData[i].array;
	 free( gParse.colNulls[i] );
	 gParse.colNulls[i] = (char*)malloc( nRows*sizeof(char) );
	 if( gParse.colNulls[i]==NULL ) {
	    gParse.status = MEMORY_ALLOCATION;
	    break;
	 }
	 for( row=0; row<nRows; row++ ) {
	    if( **sptr != '\0' && FSTRCMP( sptr[0], sptr[row+rowOffset+1] )==0 )
	       gParse.colNulls[i][row] = 1;
	    else
	       gParse.colNulls[i][row] = 0;
	 }
	 break;
      case BOOLEAN:
	 barray = (char*)gParse.colData[i].array;
	 free( gParse.colNulls[i] );
	 gParse.colNulls[i] = (char*)malloc( len*sizeof(char) );
	 if( gParse.colNulls[i]==NULL ) {
	    gParse.status = MEMORY_ALLOCATION;
	    break;
	 }
	 while( len-- ) {
	    gParse.colNulls[i][len] = 
	       ( barray[0]!=0 && barray[0]==barray[len+offset] );
	 }
	 break;
      case LONG:
	 iarray = (long*)gParse.colData[i].array;
	 free( gParse.colNulls[i] );
	 gParse.colNulls[i] = (char*)malloc( len*sizeof(char) );
	 if( gParse.colNulls[i]==NULL ) {
	    gParse.status = MEMORY_ALLOCATION;
	    break;
	 }
	 while( len-- ) {
	    gParse.colNulls[i][len] = 
	       ( iarray[0]!=0L && iarray[0]==iarray[len+offset] );
	 }
	 break;
      case DOUBLE:
	 rarray = (double*)gParse.colData[i].array;
	 free( gParse.colNulls[i] );
	 gParse.colNulls[i] = (char*)malloc( len*sizeof(char) );
	 if( gParse.colNulls[i]==NULL ) {
	    gParse.status = MEMORY_ALLOCATION;
	    break;
	 }
	 while( len-- ) {
	    gParse.colNulls[i][len] = 
	       ( rarray[0]!=0.0 && rarray[0]==rarray[len+offset]);
	 }
	 break;
      }
      if( gParse.status ) {  /*  Deallocate NULL arrays of previous columns */
	 while( i-- ) {
	    if( gParse.colInfo[i].type==BITSTR )
	       free( ((char***)gParse.colNulls)[i][0] );
	    free( gParse.colNulls[i] );
	    gParse.colNulls[i] = NULL;
	 }
	 return;
      }
   }

   /*  Reset Column Nodes' pointers to point to right data and UNDEF arrays  */

   for( i=0; i<gParse.nNodes; i++ ) {
      if(    gParse.Nodes[i].operation >  0
	  || gParse.Nodes[i].operation == CONST_OP ) continue;

      column = -gParse.Nodes[i].operation;
      offset = gParse.colInfo[column].nelem * rowOffset + 1;

      gParse.Nodes[i].value.undef = gParse.colNulls[column];

      switch( gParse.Nodes[i].type ) {
      case BITSTR:
	 gParse.Nodes[i].value.data.strptr = ((char***)gParse.colNulls)[column];
	 gParse.Nodes[i].value.undef       = NULL;
	 break;
      case STRING:
	 gParse.Nodes[i].value.data.strptr = 
	    ((char**)gParse.colData[column].array)+rowOffset+1;
	 break;
      case BOOLEAN:
	 gParse.Nodes[i].value.data.logptr = 
	    ((char*)gParse.colData[column].array)+offset;
	 break;
      case LONG:
	 gParse.Nodes[i].value.data.lngptr = 
	    ((long*)gParse.colData[column].array)+offset;
	 break;
      case DOUBLE:
	 gParse.Nodes[i].value.data.dblptr = 
	    ((double*)gParse.colData[column].array)+offset;
	 break;
      }
   }
}

static void Allocate_Ptrs( Node *this )
{
   long elem, row, size;

   if( this->type==BITSTR || this->type==STRING ) {

      this->value.data.strptr = (char**)malloc( gParse.nRows
						* sizeof(char*) );
      if( this->value.data.strptr ) {
	 this->value.data.strptr[0] = (char*)malloc( gParse.nRows
						     * (this->value.nelem+2)
						     * sizeof(char) );
	 if( this->value.data.strptr[0] ) {
	    row = 0;
	    while( (++row)<gParse.nRows ) {
	       this->value.data.strptr[row] =
		  this->value.data.strptr[row-1] + this->value.nelem+1;
	    }
	    if( this->type==STRING ) {
	       this->value.undef = this->value.data.strptr[row-1]
                                   + this->value.nelem+1;
	    } else {
	       this->value.undef = NULL;  /* BITSTRs don't use undef array */
	    }
	 } else {
	    gParse.status = MEMORY_ALLOCATION;
	    free( this->value.data.strptr );
	 }
      } else {
	 gParse.status = MEMORY_ALLOCATION;
      }

   } else {

      elem = this->value.nelem * gParse.nRows;
      switch( this->type ) {
      case DOUBLE:  size = sizeof( double ); break;
      case LONG:    size = sizeof( long   ); break;
      case BOOLEAN: size = sizeof( char   ); break;
      default:      size = 1;                break;
      }

      this->value.data.ptr = malloc( elem*(size+1) );
      
      if( this->value.data.ptr==NULL ) {
	 gParse.status = MEMORY_ALLOCATION;
      } else {
	 this->value.undef = (char *)this->value.data.ptr + elem*size;
      }
   }
}

static void Do_Unary( Node *this )
{
   Node *that;
   long elem;

   that = gParse.Nodes + this->SubNodes[0];

   if( that->operation==CONST_OP ) {  /* Operating on a constant! */
      switch( this->operation ) {
      case DOUBLE:
      case FLTCAST:
	 if( that->type==LONG )
	    this->value.data.dbl = (double)that->value.data.lng;
	 else if( that->type==BOOLEAN )
	    this->value.data.dbl = ( that->value.data.log ? 1.0 : 0.0 );
	 break;
      case LONG:
      case INTCAST:
	 if( that->type==DOUBLE )
	    this->value.data.lng = (long)that->value.data.dbl;
	 else if( that->type==BOOLEAN )
	    this->value.data.lng = ( that->value.data.log ? 1L : 0L );
	 break;
      case BOOLEAN:
	 if( that->type==DOUBLE )
	    this->value.data.log = ( that->value.data.dbl != 0.0 );
	 else if( that->type==LONG )
	    this->value.data.log = ( that->value.data.lng != 0L );
	 break;
      case UMINUS:
	 if( that->type==DOUBLE )
	    this->value.data.dbl = - that->value.data.dbl;
	 else if( that->type==LONG )
	    this->value.data.lng = - that->value.data.lng;
	 break;
      case NOT:
	 if( that->type==BOOLEAN )
	    this->value.data.log = ( ! that->value.data.log );
	 else if( that->type==BITSTR )
	    bitnot( this->value.data.str, that->value.data.str );
	 break;
      }
      this->operation = CONST_OP;

   } else {

      Allocate_Ptrs( this );

      if( !gParse.status ) {

	 if( this->type!=BITSTR ) {
	    elem = gParse.nRows;
	    if( this->type!=STRING )
	       elem *= this->value.nelem;
	    while( elem-- )
	       this->value.undef[elem] = that->value.undef[elem];
	 }

	 elem = gParse.nRows * this->value.nelem;

	 switch( this->operation ) {

	 case BOOLEAN:
	    if( that->type==DOUBLE )
	       while( elem-- )
		  this->value.data.logptr[elem] =
		     ( that->value.data.dblptr[elem] != 0.0 );
	    else if( that->type==LONG )
	       while( elem-- )
		  this->value.data.logptr[elem] =
		     ( that->value.data.lngptr[elem] != 0L );
	    break;

	 case DOUBLE:
	 case FLTCAST:
	    if( that->type==LONG )
	       while( elem-- )
		  this->value.data.dblptr[elem] =
		     (double)that->value.data.lngptr[elem];
	    else if( that->type==BOOLEAN )
	       while( elem-- )
		  this->value.data.dblptr[elem] =
		     ( that->value.data.logptr[elem] ? 1.0 : 0.0 );
	    break;

	 case LONG:
	 case INTCAST:
	    if( that->type==DOUBLE )
	       while( elem-- )
		  this->value.data.lngptr[elem] =
		     (long)that->value.data.dblptr[elem];
	    else if( that->type==BOOLEAN )
	       while( elem-- )
		  this->value.data.lngptr[elem] =
		     ( that->value.data.logptr[elem] ? 1L : 0L );
	    break;

	 case UMINUS:
	    if( that->type==DOUBLE ) {
	       while( elem-- )
		  this->value.data.dblptr[elem] =
		     - that->value.data.dblptr[elem];
	    } else if( that->type==LONG ) {
	       while( elem-- )
		  this->value.data.lngptr[elem] =
		     - that->value.data.lngptr[elem];
	    }
	    break;

	 case NOT:
	    if( that->type==BOOLEAN ) {
	       while( elem-- )
		  this->value.data.logptr[elem] =
		     ( ! that->value.data.logptr[elem] );
	    } else if( that->type==BITSTR ) {
	       elem = gParse.nRows;
	       while( elem-- )
		  bitnot( this->value.data.strptr[elem],
			  that->value.data.strptr[elem] );
	    }
	    break;
	 }
      }
   }

   if( that->operation>0 ) {
      free( that->value.data.ptr );
   }
}

static void Do_BinOp_bit( Node *this )
{
   Node *that1, *that2;
   char *sptr1=NULL, *sptr2=NULL;
   int  const1, const2;
   long rows;

   that1 = gParse.Nodes + this->SubNodes[0];
   that2 = gParse.Nodes + this->SubNodes[1];

   const1 = ( that1->operation==CONST_OP );
   const2 = ( that2->operation==CONST_OP );
   sptr1  = ( const1 ? that1->value.data.str : NULL );
   sptr2  = ( const2 ? that2->value.data.str : NULL );

   if( const1 && const2 ) {
      switch( this->operation ) {
      case NE:
	 this->value.data.log = !bitcmp( sptr1, sptr2 );
	 break;
      case EQ:
	 this->value.data.log =  bitcmp( sptr1, sptr2 );
	 break;
      case GT:
      case LT:
      case LTE:
      case GTE:
	 this->value.data.log = bitlgte( sptr1, this->operation, sptr2 );
	 break;
      case '|': 
	 bitor( this->value.data.str, sptr1, sptr2 );
	 break;
      case '&': 
	 bitand( this->value.data.str, sptr1, sptr2 );
	 break;
      case '+':
	 strcpy( this->value.data.str, sptr1 );
	 strcat( this->value.data.str, sptr2 );
	 break;
      }
      this->operation = CONST_OP;

   } else {

      Allocate_Ptrs( this );

      if( !gParse.status ) {
	 rows  = gParse.nRows;
	 switch( this->operation ) {

	    /*  BITSTR comparisons  */

	 case NE:
	 case EQ:
	 case GT:
	 case LT:
	 case LTE:
	 case GTE:
	    while( rows-- ) {
	       if( !const1 )
		  sptr1 = that1->value.data.strptr[rows];
	       if( !const2 )
		  sptr2 = that2->value.data.strptr[rows];
	       switch( this->operation ) {
	       case NE:  this->value.data.logptr[rows] = 
                                                      !bitcmp( sptr1, sptr2 );
                         break;
	       case EQ:  this->value.data.logptr[rows] = 
                                                       bitcmp( sptr1, sptr2 );
                         break;
	       case GT:
	       case LT:
	       case LTE:
	       case GTE: this->value.data.logptr[rows] = 
                                     bitlgte( sptr1, this->operation, sptr2 );
	                 break;
	       }
	       this->value.undef[rows] = 0;
	    }
	    break;
	 
	    /*  BITSTR AND/ORs ...  no UNDEFS in or out */
      
	 case '|': 
	 case '&': 
	 case '+':
	    while( rows-- ) {
	       if( !const1 )
		  sptr1 = that1->value.data.strptr[rows];
	       if( !const2 )
		  sptr2 = that2->value.data.strptr[rows];
	       if( this->operation=='|' )
		  bitor(  this->value.data.strptr[rows], sptr1, sptr2 );
	       else if( this->operation=='&' )
		  bitand( this->value.data.strptr[rows], sptr1, sptr2 );
	       else {
		  strcpy( this->value.data.strptr[rows], sptr1 );
		  strcat( this->value.data.strptr[rows], sptr2 );
	       }
	    }
	    break;
	 }
      }
   }

   if( that1->operation>0 ) {
      free( that1->value.data.strptr[0] );
      free( that1->value.data.strptr    );
   }
   if( that2->operation>0 ) {
      free( that2->value.data.strptr[0] );
      free( that2->value.data.strptr    );
   }
}

static void Do_BinOp_str( Node *this )
{
   Node *that1, *that2;
   char *sptr1, *sptr2, null1=0, null2=0;
   int const1, const2, val;
   long rows;

   that1 = gParse.Nodes + this->SubNodes[0];
   that2 = gParse.Nodes + this->SubNodes[1];

   const1 = ( that1->operation==CONST_OP );
   const2 = ( that2->operation==CONST_OP );
   sptr1  = ( const1 ? that1->value.data.str : NULL );
   sptr2  = ( const2 ? that2->value.data.str : NULL );

   if( const1 && const2 ) {  /*  Result is a constant  */
      switch( this->operation ) {

	 /*  Compare Strings  */

      case NE:
      case EQ:
	 val = ( FSTRCMP( sptr1, sptr2 ) == 0 );
	 this->value.data.log = ( this->operation==EQ ? val : !val );
	 break;

	 /*  Concat Strings  */

      case '+':
	 strcpy( this->value.data.str, sptr1 );
	 strcat( this->value.data.str, sptr2 );
	 break;
      }
      this->operation = CONST_OP;

   } else {  /*  Not a constant  */

      Allocate_Ptrs( this );

      if( !gParse.status ) {

	 rows = gParse.nRows;
	 switch( this->operation ) {

	    /*  Compare Strings  */

	 case NE:
	 case EQ:
	    while( rows-- ) {
	       if( !const1 ) null1 = that1->value.undef[rows];
	       if( !const2 ) null2 = that2->value.undef[rows];
	       this->value.undef[rows] = (null1 || null2);
	       if( ! this->value.undef[rows] ) {
		  if( !const1 ) sptr1  = that1->value.data.strptr[rows];
		  if( !const2 ) sptr2  = that2->value.data.strptr[rows];
		  val = ( FSTRCMP( sptr1, sptr2 ) == 0 );
		  this->value.data.logptr[rows] =
		     ( this->operation==EQ ? val : !val );
	       }
	    }
	    break;
	    
	    /*  Concat Strings  */
	    
	 case '+':
	    while( rows-- ) {
	       if( !const1 ) null1 = that1->value.undef[rows];
	       if( !const2 ) null2 = that2->value.undef[rows];
	       this->value.undef[rows] = (null1 || null2);
	       if( ! this->value.undef[rows] ) {
		  if( !const1 ) sptr1  = that1->value.data.strptr[rows];
		  if( !const2 ) sptr2  = that2->value.data.strptr[rows];
		  strcpy( this->value.data.strptr[rows], sptr1 );
		  strcat( this->value.data.strptr[rows], sptr2 );
	       }
	    }
	    break;
	 }
      }
   }

   if( that1->operation>0 ) {
      free( that1->value.data.strptr[0] );
      free( that1->value.data.strptr );
   }
   if( that2->operation>0 ) {
      free( that2->value.data.strptr[0] );
      free( that2->value.data.strptr );
   }
}

static void Do_BinOp_log( Node *this )
{
   Node *that1, *that2;
   int vector1, vector2;
   char val1=0, val2=0, null1=0, null2=0;
   long rows, nelem, elem;

   that1 = gParse.Nodes + this->SubNodes[0];
   that2 = gParse.Nodes + this->SubNodes[1];

   vector1 = ( that1->operation!=CONST_OP );
   if( vector1 )
      vector1 = that1->value.nelem;
   else {
      val1  = that1->value.data.log;
   }

   vector2 = ( that2->operation!=CONST_OP );
   if( vector2 )
      vector2 = that2->value.nelem;
   else {
      val2  = that2->value.data.log;
   }

   if( !vector1 && !vector2 ) {  /*  Result is a constant  */
      switch( this->operation ) {
      case OR:
	 this->value.data.log = (val1 || val2);
	 break;
      case AND:
	 this->value.data.log = (val1 && val2);
	 break;
      case EQ:
	 this->value.data.log = ( (val1 && val2) || (!val1 && !val2) );
	 break;
      case NE:
	 this->value.data.log = ( (val1 && !val2) || (!val1 && val2) );
	 break;
      }
      this->operation=CONST_OP;
   } else {
      rows  = gParse.nRows;
      nelem = this->value.nelem;
      elem  = this->value.nelem * rows;

      Allocate_Ptrs( this );

      if( !gParse.status ) {
	 while( rows-- ) {
	    while( nelem-- ) {
	       elem--;

	       if( vector1>1 ) {
		  val1  = that1->value.data.logptr[elem];
		  null1 = that1->value.undef[elem];
	       } else if( vector1 ) {
		  val1  = that1->value.data.logptr[rows];
		  null1 = that1->value.undef[rows];
	       }

	       if( vector2>1 ) {
		  val2  = that2->value.data.logptr[elem];
		  null2 = that2->value.undef[elem];
	       } else if( vector2 ) {
		  val2  = that2->value.data.logptr[rows];
		  null2 = that2->value.undef[rows];
	       }

	       this->value.undef[elem] = (null1 || null2);
	       switch( this->operation ) {

	       case OR:
		  /*  This is more complicated than others to suppress UNDEFs */
		  /*  in those cases where the other argument is DEF && TRUE  */

		  if( !null1 && !null2 ) {
		     this->value.data.logptr[elem] = (val1 || val2);
		  } else if( (null1 && !null2 && val2)
			     || ( !null1 && null2 && val1 ) ) {
		     this->value.data.logptr[elem] = 1;
		     this->value.undef[elem] = 0;
		  }
		  break;

	       case AND:
		  /*  This is more complicated than others to suppress UNDEFs */
		  /*  in those cases where the other argument is DEF && FALSE */

		  if( !null1 && !null2 ) {
		     this->value.data.logptr[elem] = (val1 && val2);
		  } else if( (null1 && !null2 && !val2)
			     || ( !null1 && null2 && !val1 ) ) {
		     this->value.data.logptr[elem] = 0;
		     this->value.undef[elem] = 0;
		  }
		  break;

	       case EQ:
		  this->value.data.logptr[elem] = 
		     ( (val1 && val2) || (!val1 && !val2) );
		  break;

	       case NE:
		  this->value.data.logptr[elem] =
		     ( (val1 && !val2) || (!val1 && val2) );
		  break;
	       }
	    }
	    nelem = this->value.nelem;
	 }
      }
   }

   if( that1->operation>0 ) {
      free( that1->value.data.ptr );
   }
   if( that2->operation>0 ) {
      free( that2->value.data.ptr );
   }
}

static void Do_BinOp_lng( Node *this )
{
   Node *that1, *that2;
   int  vector1, vector2;
   long val1=0, val2=0;
   char null1=0, null2=0;
   long rows, nelem, elem;

   that1 = gParse.Nodes + this->SubNodes[0];
   that2 = gParse.Nodes + this->SubNodes[1];

   vector1 = ( that1->operation!=CONST_OP );
   if( vector1 )
      vector1 = that1->value.nelem;
   else {
      val1  = that1->value.data.lng;
   }

   vector2 = ( that2->operation!=CONST_OP );
   if( vector2 )
      vector2 = that2->value.nelem;
   else {
      val2  = that2->value.data.lng;
   }

   if( !vector1 && !vector2 ) {  /*  Result is a constant  */

      switch( this->operation ) {
      case '~':   /* Treat as == for LONGS */
      case EQ:    this->value.data.log = (val1 == val2);   break;
      case NE:    this->value.data.log = (val1 != val2);   break;
      case GT:    this->value.data.log = (val1 >  val2);   break;
      case LT:    this->value.data.log = (val1 <  val2);   break;
      case LTE:   this->value.data.log = (val1 <= val2);   break;
      case GTE:   this->value.data.log = (val1 >= val2);   break;

      case '+':   this->value.data.lng = (val1  + val2);   break;
      case '-':   this->value.data.lng = (val1  - val2);   break;
      case '*':   this->value.data.lng = (val1  * val2);   break;

      case '%':
	 if( val2 ) this->value.data.lng = (val1 % val2);
	 else       fferror("Divide by Zero");
	 break;
      case '/': 
	 if( val2 ) this->value.data.lng = (val1 / val2); 
	 else       fferror("Divide by Zero");
	 break;
      case POWER:
	 this->value.data.lng = (long)pow((double)val1,(double)val2);
	 break;
      }
      this->operation=CONST_OP;

   } else {

      rows  = gParse.nRows;
      nelem = this->value.nelem;
      elem  = this->value.nelem * rows;

      Allocate_Ptrs( this );

      while( rows-- && !gParse.status ) {
	 while( nelem-- && !gParse.status ) {
	    elem--;

	    if( vector1>1 ) {
	       val1  = that1->value.data.lngptr[elem];
	       null1 = that1->value.undef[elem];
	    } else if( vector1 ) {
	       val1  = that1->value.data.lngptr[rows];
	       null1 = that1->value.undef[rows];
	    }

	    if( vector2>1 ) {
	       val2  = that2->value.data.lngptr[elem];
	       null2 = that2->value.undef[elem];
	    } else if( vector2 ) {
	       val2  = that2->value.data.lngptr[rows];
	       null2 = that2->value.undef[rows];
	    }

	    this->value.undef[elem] = (null1 || null2);
	    switch( this->operation ) {
	    case '~':   /* Treat as == for LONGS */
	    case EQ:   this->value.data.logptr[elem] = (val1 == val2);   break;
	    case NE:   this->value.data.logptr[elem] = (val1 != val2);   break;
	    case GT:   this->value.data.logptr[elem] = (val1 >  val2);   break;
	    case LT:   this->value.data.logptr[elem] = (val1 <  val2);   break;
	    case LTE:  this->value.data.logptr[elem] = (val1 <= val2);   break;
	    case GTE:  this->value.data.logptr[elem] = (val1 >= val2);   break;
	       
	    case '+':  this->value.data.lngptr[elem] = (val1  + val2);   break;
	    case '-':  this->value.data.lngptr[elem] = (val1  - val2);   break;
	    case '*':  this->value.data.lngptr[elem] = (val1  * val2);   break;

	    case '%':   
	       if( val2 ) this->value.data.lngptr[elem] = (val1 % val2);
	       else {
		  fferror("Divide by Zero");
		  free( this->value.data.ptr );
	       }
	       break;
	    case '/': 
	       if( val2 ) this->value.data.lngptr[elem] = (val1 / val2); 
	       else {
		  fferror("Divide by Zero");
		  free( this->value.data.ptr );
	       }
	       break;
	    case POWER:
	       this->value.data.lngptr[elem] = (long)pow((double)val1,(double)val2);
	       break;
	    }
	 }
	 nelem = this->value.nelem;
      }
   }

   if( that1->operation>0 ) {
      free( that1->value.data.ptr );
   }
   if( that2->operation>0 ) {
      free( that2->value.data.ptr );
   }
}

static void Do_BinOp_dbl( Node *this )
{
   Node   *that1, *that2;
   int    vector1, vector2;
   double val1=0.0, val2=0.0;
   char   null1=0, null2=0;
   long   rows, nelem, elem;

   that1 = gParse.Nodes + this->SubNodes[0];
   that2 = gParse.Nodes + this->SubNodes[1];

   vector1 = ( that1->operation!=CONST_OP );
   if( vector1 )
      vector1 = that1->value.nelem;
   else {
      val1  = that1->value.data.dbl;
   }

   vector2 = ( that2->operation!=CONST_OP );
   if( vector2 )
      vector2 = that2->value.nelem;
   else {
      val2  = that2->value.data.dbl;
   } 

   if( !vector1 && !vector2 ) {  /*  Result is a constant  */

      switch( this->operation ) {
      case '~':   this->value.data.log = ( fabs(val1-val2) < APPROX );   break;
      case EQ:    this->value.data.log = (val1 == val2);   break;
      case NE:    this->value.data.log = (val1 != val2);   break;
      case GT:    this->value.data.log = (val1 >  val2);   break;
      case LT:    this->value.data.log = (val1 <  val2);   break;
      case LTE:   this->value.data.log = (val1 <= val2);   break;
      case GTE:   this->value.data.log = (val1 >= val2);   break;

      case '+':   this->value.data.dbl = (val1  + val2);   break;
      case '-':   this->value.data.dbl = (val1  - val2);   break;
      case '*':   this->value.data.dbl = (val1  * val2);   break;

      case '%':
	 if( val2 ) this->value.data.dbl = val1 - val2*((int)(val1/val2));
	 else       fferror("Divide by Zero");
	 break;
      case '/': 
	 if( val2 ) this->value.data.dbl = (val1 / val2); 
	 else       fferror("Divide by Zero");
	 break;
      case POWER:
	 this->value.data.dbl = (double)pow(val1,val2);
	 break;
      }
      this->operation=CONST_OP;

   } else {

      rows  = gParse.nRows;
      nelem = this->value.nelem;
      elem  = this->value.nelem * rows;

      Allocate_Ptrs( this );

      while( rows-- && !gParse.status ) {
	 while( nelem-- && !gParse.status ) {
	    elem--;

	    if( vector1>1 ) {
	       val1  = that1->value.data.dblptr[elem];
	       null1 = that1->value.undef[elem];
	    } else if( vector1 ) {
	       val1  = that1->value.data.dblptr[rows];
	       null1 = that1->value.undef[rows];
	    }

	    if( vector2>1 ) {
	       val2  = that2->value.data.dblptr[elem];
	       null2 = that2->value.undef[elem];
	    } else if( vector2 ) {
	       val2  = that2->value.data.dblptr[rows];
	       null2 = that2->value.undef[rows];
	    }

	    this->value.undef[elem] = (null1 || null2);
	    switch( this->operation ) {
	    case '~':   this->value.data.logptr[elem] =
                                          ( fabs(val1-val2) < APPROX );   break;
	    case EQ:    this->value.data.logptr[elem] = (val1 == val2);   break;
	    case NE:    this->value.data.logptr[elem] = (val1 != val2);   break;
	    case GT:    this->value.data.logptr[elem] = (val1 >  val2);   break;
	    case LT:    this->value.data.logptr[elem] = (val1 <  val2);   break;
	    case LTE:   this->value.data.logptr[elem] = (val1 <= val2);   break;
	    case GTE:   this->value.data.logptr[elem] = (val1 >= val2);   break;
	       
	    case '+':   this->value.data.dblptr[elem] = (val1  + val2);   break;
	    case '-':   this->value.data.dblptr[elem] = (val1  - val2);   break;
	    case '*':   this->value.data.dblptr[elem] = (val1  * val2);   break;

	    case '%':
	       if( val2 ) this->value.data.dblptr[elem] =
                                val1 - val2*((int)(val1/val2));
	       else {
		  fferror("Divide by Zero");
		  free( this->value.data.ptr );
	       }
	       break;
	    case '/': 
	       if( val2 ) this->value.data.dblptr[elem] = (val1 / val2); 
	       else {
		  fferror("Divide by Zero");
		  free( this->value.data.ptr );
	       }
	       break;
	    case POWER:
	       this->value.data.dblptr[elem] = (double)pow(val1,val2);
	       break;
	    }
	 }
	 nelem = this->value.nelem;
      }
   }

   if( that1->operation>0 ) {
      free( that1->value.data.ptr );
   }
   if( that2->operation>0 ) {
      free( that2->value.data.ptr );
   }
}

static void Do_Func( Node *this )
{
   Node *theParams[MAXSUBS];
   int  vector[MAXSUBS], allConst;
   lval pVals[MAXSUBS];
   char pNull[MAXSUBS];
   long   ival;
   double dval;
   int  i;
   long row, elem, nelem;

   i = this->nSubNodes;
   allConst = 1;
   while( i-- ) {
      theParams[i] = gParse.Nodes + this->SubNodes[i];
      vector[i]   = ( theParams[i]->operation!=CONST_OP );
      if( vector[i] ) {
	 allConst = 0;
	 vector[i] = theParams[i]->value.nelem;
      } else {
	 if( theParams[i]->type==DOUBLE ) {
	    pVals[i].data.dbl = theParams[i]->value.data.dbl;
	 } else if( theParams[i]->type==LONG ) {
	    pVals[i].data.lng = theParams[i]->value.data.lng;
	 } else if( theParams[i]->type==BOOLEAN ) {
	    pVals[i].data.log = theParams[i]->value.data.log;
	 } else
	    strcpy(pVals[i].data.str, theParams[i]->value.data.str);
	 pNull[i] = 0;
      }
   }

   if( this->nSubNodes==0 ) allConst = 0; /* These do produce scalars */

   if( allConst ) {

      switch( this->operation ) {

	    /* Non-Trig single-argument functions */

	 case sum_fct:
	    if( theParams[0]->type==BOOLEAN )
	       this->value.data.lng = ( pVals[0].data.log ? 1 : 0 );
	    else if( theParams[0]->type==LONG )
	       this->value.data.lng = pVals[0].data.lng;
	    else
	       this->value.data.dbl = pVals[0].data.dbl;
	    break;
	 case abs_fct:
	    if( theParams[0]->type==DOUBLE ) {
	       dval = pVals[0].data.dbl;
	       this->value.data.dbl = (dval>0.0 ? dval : -dval);
	    } else {
	       ival = pVals[0].data.lng;
	       this->value.data.lng = (ival> 0  ? ival : -ival);
	    }
	    break;

            /* Special Null-Handling Functions */

         case isnull_fct:  /* Constants are always defined */
	    this->value.data.log = 0;
	    break;
         case defnull_fct:
	    if( this->type==BOOLEAN )
	       this->value.data.log = pVals[0].data.log;
            else if( this->type==LONG )
	       this->value.data.lng = pVals[0].data.lng;
            else if( this->type==DOUBLE )
	       this->value.data.dbl = pVals[0].data.dbl;
            else if( this->type==STRING )
	       strcpy(this->value.data.str,pVals[0].data.str);
	    break;

	    /* Trig functions with 1 double argument */

	 case sin_fct:
	    this->value.data.dbl = sin( pVals[0].data.dbl );
	    break;
	 case cos_fct:
	    this->value.data.dbl = cos( pVals[0].data.dbl );
	    break;
	 case tan_fct:
	    this->value.data.dbl = tan( pVals[0].data.dbl );
	    break;
	 case asin_fct:
	    dval = pVals[0].data.dbl;
	    if( dval<-1.0 || dval>1.0 )
	       fferror("Out of range argument to arcsin");
	    else
	       this->value.data.dbl = asin( dval );
	    break;
	 case acos_fct:
	    dval = pVals[0].data.dbl;
	    if( dval<-1.0 || dval>1.0 )
	       fferror("Out of range argument to arccos");
	    else
	       this->value.data.dbl = acos( dval );
	    break;
	 case atan_fct:
	    dval = pVals[0].data.dbl;
	    this->value.data.dbl = atan( dval );
	    break;
	 case exp_fct:
	    dval = pVals[0].data.dbl;
	    this->value.data.dbl = exp( dval );
	    break;
	 case log_fct:
	    dval = pVals[0].data.dbl;
	    if( dval<=0.0 )
	       fferror("Out of range argument to log");
	    else
	       this->value.data.dbl = log( dval );
	    break;
	 case log10_fct:
	    dval = pVals[0].data.dbl;
	    if( dval<=0.0 )
	       fferror("Out of range argument to log10");
	    else
	       this->value.data.dbl = log10( dval );
	    break;
	 case sqrt_fct:
	    dval = pVals[0].data.dbl;
	    if( dval<0.0 )
	       fferror("Out of range argument to sqrt");
	    else
	       this->value.data.dbl = sqrt( dval );
	    break;

	    /* Two-argument Trig Functions */

	 case atan2_fct:
	    this->value.data.dbl =
	       atan2( pVals[0].data.dbl, pVals[1].data.dbl );
	    break;

	    /* Boolean SAO region Functions... all arguments scalar dbls */

	 case near_fct:
	    this->value.data.log = bnear( pVals[0].data.dbl, pVals[1].data.dbl,
					  pVals[2].data.dbl );
	    break;
	 case circle_fct:
	    this->value.data.log = circle( pVals[0].data.dbl, pVals[1].data.dbl,
					   pVals[2].data.dbl, pVals[3].data.dbl,
					   pVals[4].data.dbl );
	    break;
	 case box_fct:
	    this->value.data.log = saobox( pVals[0].data.dbl, pVals[1].data.dbl,
					   pVals[2].data.dbl, pVals[3].data.dbl,
					   pVals[4].data.dbl, pVals[5].data.dbl,
					   pVals[6].data.dbl );
	    break;
	 case elps_fct:
	    this->value.data.log =
                               ellipse( pVals[0].data.dbl, pVals[1].data.dbl,
					pVals[2].data.dbl, pVals[3].data.dbl,
					pVals[4].data.dbl, pVals[5].data.dbl,
					pVals[6].data.dbl );
	    break;
      }
      this->operation = CONST_OP;

   } else {

      Allocate_Ptrs( this );

      row  = gParse.nRows;
      elem = row * this->value.nelem;

      if( !gParse.status ) {
	 switch( this->operation ) {

	    /* Special functions with no arguments */

	 case row_fct:
	    while( row-- ) {
	       this->value.data.lngptr[row] = gParse.firstRow + row;
	       this->value.undef[row] = 0;
	    }
	    break;
	 case rnd_fct:
	    if( rand()<32768 && rand()<32768 )
	       dval =      32768.0;
	    else
	       dval = 2147483648.0;
	    while( row-- ) {
	       this->value.data.dblptr[row] = (double)rand() / dval;
	       this->value.undef[row] = 0;
	    }
	    break;

	    /* Non-Trig single-argument functions */
	    
	 case sum_fct:
	    elem = row * theParams[0]->value.nelem;
	    if( theParams[0]->type==BOOLEAN ) {
	       while( row-- ) {
		  this->value.data.lngptr[row] = 0;
		  this->value.undef[row] = 0;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     this->value.data.lngptr[row] +=
			( theParams[0]->value.data.logptr[elem] ? 1 : 0 );
		     this->value.undef[row] |=
			  theParams[0]->value.undef[elem];
		  }
	       }		  
	    } else if( theParams[0]->type==LONG ) {
	       while( row-- ) {
		  this->value.data.lngptr[row] = 0;
		  this->value.undef[row] = 0;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     this->value.data.lngptr[row] +=
			theParams[0]->value.data.lngptr[elem];
		     this->value.undef[row] |=
			  theParams[0]->value.undef[elem];
		  }
	       }		  
	    } else {
	       while( row-- ) {
		  this->value.data.dblptr[row] = 0.0;
		  this->value.undef[row] = 0;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     this->value.data.dblptr[row] +=
			theParams[0]->value.data.dblptr[elem];
		     this->value.undef[row] |=
			  theParams[0]->value.undef[elem];
		  }
	       }		  
	    }
	    break;
	 case abs_fct:
	    if( theParams[0]->type==DOUBLE )
	       while( elem-- ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  this->value.data.dblptr[elem] = (dval>0.0 ? dval : -dval);
		  this->value.undef[elem] = theParams[0]->value.undef[elem];
	       }
	    else
	       while( elem-- ) {
		  ival = theParams[0]->value.data.lngptr[elem];
		  this->value.data.lngptr[elem] = (ival> 0  ? ival : -ival);
		  this->value.undef[elem] = theParams[0]->value.undef[elem];
	       }
	    break;

            /* Special Null-Handling Functions */

	 case isnull_fct:
	    if( theParams[0]->type==STRING ) elem = row;
	    while( elem-- ) {
	       this->value.data.logptr[elem] = theParams[0]->value.undef[elem];
	       this->value.undef[elem] = 0;
	    }
	    break;
         case defnull_fct:
	    switch( this->type ) {
	    case BOOLEAN:
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pNull[i] = theParams[i]->value.undef[elem];
			   pVals[i].data.log =
			      theParams[i]->value.data.logptr[elem];
			} else if( vector[i] ) {
			   pNull[i] = theParams[i]->value.undef[row];
			   pVals[i].data.log =
			      theParams[i]->value.data.logptr[row];
			}
		     if( pNull[0] ) {
			this->value.undef[elem] = pNull[1];
			this->value.data.logptr[elem] = pVals[1].data.log;
		     } else {
			this->value.undef[elem] = 0;
			this->value.data.logptr[elem] = pVals[0].data.log;
		     }
		  }
	       }
	       break;
	    case LONG:
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pNull[i] = theParams[i]->value.undef[elem];
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[elem];
			} else if( vector[i] ) {
			   pNull[i] = theParams[i]->value.undef[row];
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[row];
			}
		     if( pNull[0] ) {
			this->value.undef[elem] = pNull[1];
			this->value.data.lngptr[elem] = pVals[1].data.lng;
		     } else {
			this->value.undef[elem] = 0;
			this->value.data.lngptr[elem] = pVals[0].data.lng;
		     }
		  }
	       }
	       break;
	    case DOUBLE:
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pNull[i] = theParams[i]->value.undef[elem];
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[elem];
			} else if( vector[i] ) {
			   pNull[i] = theParams[i]->value.undef[row];
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[row];
			}
		     if( pNull[0] ) {
			this->value.undef[elem] = pNull[1];
			this->value.data.dblptr[elem] = pVals[1].data.dbl;
		     } else {
			this->value.undef[elem] = 0;
			this->value.data.dblptr[elem] = pVals[0].data.dbl;
		     }
		  }
	       }
	       break;
	    case STRING:
	       while( row-- ) {
		  i=2; while( i-- )
		     if( vector[i] ) {
			pNull[i] = theParams[i]->value.undef[row];
			strcpy(pVals[i].data.str,
			       theParams[i]->value.data.strptr[row]);
		     }
		  if( pNull[0] ) {
		     this->value.undef[elem] = pNull[1];
		     strcpy(this->value.data.strptr[elem],pVals[1].data.str);
		  } else {
		     this->value.undef[elem] = 0;
		     strcpy(this->value.data.strptr[elem],pVals[0].data.str);
		  }
	       }
	    }
	    break;

	    /* Trig functions with 1 double argument */

	 case sin_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     sin( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case cos_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     cos( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case tan_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     tan( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case asin_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  if( dval<-1.0 || dval>1.0 ) {
		     fferror("Out of range argument to arcsin");
		     free( this->value.data.ptr );
		     break;
		  } else
		     this->value.data.dblptr[elem] = asin( dval );
	       }
	    break;
	 case acos_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  if( dval<-1.0 || dval>1.0 ) {
		     fferror("Out of range argument to arccos");
		     free( this->value.data.ptr );
		     break;
		  } else
		     this->value.data.dblptr[elem] = acos( dval );
	       }
	    break;
	 case atan_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  this->value.data.dblptr[elem] = atan( dval );
	       }
	    break;
	 case exp_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  this->value.data.dblptr[elem] = exp( dval );
	       }
	    break;
	 case log_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  if( dval<=0.0 ) {
		     fferror("Out of range argument to log");
		     free( this->value.data.ptr );
		     break;
		  } else
		     this->value.data.dblptr[elem] = log( dval );
	       }
	    break;
	 case log10_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  if( dval<=0.0 ) {
		     fferror("Out of range argument to log10");
		     free( this->value.data.ptr );
		     break;
		  } else
		     this->value.data.dblptr[elem] = log10( dval );
	       }
	    break;
	 case sqrt_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  if( dval<0.0 ) {
		     fferror("Out of range argument to sqrt");
		     free( this->value.data.ptr );
		     break;
		  } else
		     this->value.data.dblptr[elem] = sqrt( dval );
	       }
	    break;

	    /* Two-argument Trig Functions */
	    
	 case atan2_fct:
	    while( row-- ) {
	       nelem = this->value.nelem;
	       while( nelem-- ) {
		  elem--;
		  i=2; while( i-- )
		     if( vector[i]>1 ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[elem];
			pNull[i] = theParams[i]->value.undef[elem];
		     } else if( vector[i] ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[row];
			pNull[i] = theParams[i]->value.undef[row];
		     }
		  if( !(this->value.undef[elem] = (pNull[0] || pNull[1]) ) )
		     this->value.data.dblptr[elem] =
			atan2( pVals[0].data.dbl, pVals[1].data.dbl );
	       }
	    }
	    break;

	    /* Boolean SAO region Functions... all arguments scalar dbls */

	 case near_fct:
	    while( row-- ) {
	       this->value.undef[row] = 0;
	       i=3; while( i-- )
		  if( vector[i] ) {
		     pVals[i].data.dbl = theParams[i]->value.data.dblptr[row];
		     this->value.undef[row] |= theParams[i]->value.undef[row];
		  }
	       if( !(this->value.undef[row]) )
		  this->value.data.logptr[row] =
		     bnear( pVals[0].data.dbl, pVals[1].data.dbl,
			    pVals[2].data.dbl );
	    }
	    break;
	 case circle_fct:
	    while( row-- ) {
	       this->value.undef[row] = 0;
	       i=5; while( i-- )
		  if( vector[i] ) {
		     pVals[i].data.dbl = theParams[i]->value.data.dblptr[row];
		     this->value.undef[row] |= theParams[i]->value.undef[row];
		  }
	       if( !(this->value.undef[row]) )
		  this->value.data.logptr[row] =
		     circle( pVals[0].data.dbl, pVals[1].data.dbl,
			     pVals[2].data.dbl, pVals[3].data.dbl,
			     pVals[4].data.dbl );
	    }
	    break;
	 case box_fct:
	    while( row-- ) {
	       this->value.undef[row] = 0;
	       i=7; while( i-- )
		  if( vector[i] ) {
		     pVals[i].data.dbl = theParams[i]->value.data.dblptr[row];
		     this->value.undef[row] |= theParams[i]->value.undef[row];
		  }
	       if( !(this->value.undef[row]) )
		  this->value.data.logptr[row] =
		     saobox( pVals[0].data.dbl, pVals[1].data.dbl,
			     pVals[2].data.dbl, pVals[3].data.dbl,
			     pVals[4].data.dbl, pVals[5].data.dbl,
			     pVals[6].data.dbl );
	    }
	    break;
	 case elps_fct:
	    while( row-- ) {
	       this->value.undef[row] = 0;
	       i=7; while( i-- )
		  if( vector[i] ) {
		     pVals[i].data.dbl = theParams[i]->value.data.dblptr[row];
		     this->value.undef[row] |= theParams[i]->value.undef[row];
		  }
	       if( !(this->value.undef[row]) )
		  this->value.data.logptr[row] =
		     ellipse( pVals[0].data.dbl, pVals[1].data.dbl,
			      pVals[2].data.dbl, pVals[3].data.dbl,
			      pVals[4].data.dbl, pVals[5].data.dbl,
			      pVals[6].data.dbl );
	    }
	    break;
	 }
      }
   }

   i = this->nSubNodes;
   while( i-- ) {
      if( theParams[i]->operation>0 ) {
	 /*  Currently only numeric params allowed  */
	 free( theParams[i]->value.data.ptr );
      }
   }
}

static void Do_Deref( Node *this )
{
   Node *theVar, *theDims[MAXDIMS];
   int  isConst[MAXDIMS], allConst;
   long dimVals[MAXDIMS];
   int  i, nDims;
   long row, elem, dsize;

   theVar = gParse.Nodes + this->SubNodes[0];

   i = nDims = this->nSubNodes-1;
   allConst = 1;
   while( i-- ) {
      theDims[i] = gParse.Nodes + this->SubNodes[i+1];
      isConst[i] = ( theDims[i]->operation==CONST_OP );
      if( isConst[i] )
	 dimVals[i] = theDims[i]->value.data.lng;
      else
	 allConst = 0;
   }

   if( this->type==DOUBLE ) {
      dsize = sizeof( double );
   } else if( this->type==LONG ) {
      dsize = sizeof( long );
   } else if( this->type==BOOLEAN ) {
      dsize = sizeof( char );
   } else
      dsize = 0;

   Allocate_Ptrs( this );

   if( !gParse.status ) {

      if( allConst && theVar->value.naxis==nDims ) {

	 /* Dereference completely using constant indices */

	 elem = 0;
	 i    = nDims;
	 while( i-- ) {
	    if( dimVals[i]<1 || dimVals[i]>theVar->value.naxes[i] ) break;
	    elem = theVar->value.naxes[i]*elem + dimVals[i]-1;
	 }
	 if( i<0 ) {
	    for( row=0; row<gParse.nRows; row++ ) {
	       this->value.undef[row] = theVar->value.undef[elem];
	       if( this->type==DOUBLE )
		  this->value.data.dblptr[row] = 
		     theVar->value.data.dblptr[elem];
	       else if( this->type==LONG )
		  this->value.data.lngptr[row] = 
		     theVar->value.data.lngptr[elem];
	       else
		  this->value.data.logptr[row] = 
		     theVar->value.data.logptr[elem];
	       elem += theVar->value.nelem;
	    }
	 } else {
	    fferror("Index out of range");
	    free( this->value.data.ptr );
	 }
	 
      } else if( allConst && nDims==1 ) {
	 
	 /* Reduce dimensions by 1, using a constant index */
	 
	 if( dimVals[0] < 1 ||
	     dimVals[0] > theVar->value.naxes[ theVar->value.naxis-1 ] ) {
	    fferror("Index out of range");
	    free( this->value.data.ptr );
	 } else {
	    elem = this->value.nelem * (dimVals[0]-1);
	    for( row=0; row<gParse.nRows; row++ ) {
	       memcpy( this->value.undef + row*this->value.nelem,
		       theVar->value.undef + elem,
		       this->value.nelem * sizeof(char) );
	       memcpy( (char*)this->value.data.ptr
		       + row*dsize*this->value.nelem,
		       (char*)theVar->value.data.ptr + elem*dsize,
		       this->value.nelem * dsize );
	       elem += theVar->value.nelem;
	    }	       
	 }
      
      } else if( theVar->value.naxis==nDims ) {

	 /* Dereference completely using an expression for the indices */

	 for( row=0; row<gParse.nRows; row++ ) {

	    for( i=0; i<nDims; i++ ) {
	       if( !isConst[i] ) {
		  if( theDims[i]->value.undef[row] ) {
		     fferror("Null encountered as vector index");
		     free( this->value.data.ptr );
		     break;
		  } else
		     dimVals[i] = theDims[i]->value.data.lngptr[row];
	       }
	    }
	    if( gParse.status ) break;

	    elem = 0;
	    i    = nDims;
	    while( i-- ) {
	       if( dimVals[i]<1 || dimVals[i]>theVar->value.naxes[i] ) break;
	       elem = theVar->value.naxes[i]*elem + dimVals[i]-1;
	    }
	    if( i<0 ) {
	       elem += row*theVar->value.nelem;
	       this->value.undef[row] = theVar->value.undef[elem];
	       if( this->type==DOUBLE )
		  this->value.data.dblptr[row] = 
		     theVar->value.data.dblptr[elem];
	       else if( this->type==LONG )
		  this->value.data.lngptr[row] = 
		     theVar->value.data.lngptr[elem];
	       else
		  this->value.data.logptr[row] = 
		     theVar->value.data.logptr[elem];
	    } else {
	       fferror("Index out of range");
	       free( this->value.data.ptr );
	    }
	 }

      } else {

	 /* Reduce dimensions by 1, using a nonconstant expression */

	 for( row=0; row<gParse.nRows; row++ ) {

	    /* Index cannot be a constant */

	    if( theDims[0]->value.undef[row] ) {
	       fferror("Null encountered as vector index");
	       free( this->value.data.ptr );
	       break;
	    } else
	       dimVals[0] = theDims[0]->value.data.lngptr[row];

	    if( dimVals[0] < 1 ||
		dimVals[0] > theVar->value.naxes[ theVar->value.naxis-1 ] ) {
	       fferror("Index out of range");
	       free( this->value.data.ptr );
	    } else {
	       elem  = this->value.nelem * (dimVals[0]-1);
	       elem += row*theVar->value.nelem;
	       memcpy( this->value.undef + row*this->value.nelem,
		       theVar->value.undef + elem,
		       this->value.nelem * sizeof(char) );
	       memcpy( (char*)this->value.data.ptr
		       + row*dsize*this->value.nelem,
		       (char*)theVar->value.data.ptr + elem*dsize,
		       this->value.nelem * dsize );
	    }
	 }
      }
   }

   if( theVar->operation>0 ) {
      free( theVar->value.data.ptr );
   }
   for( i=0; i<nDims; i++ )
      if( theDims[i]->operation>0 ) {
	 free( theDims[i]->value.data.ptr );
      }
}

static void Do_GTI( Node *this )
{
   Node *theExpr, *theTimes;
   double *start, *stop, *times;
   long elem, nGTI, gti;
   int ordered;

   theTimes = gParse.Nodes + this->SubNodes[0];
   theExpr  = gParse.Nodes + this->SubNodes[1];

   nGTI    = theTimes->value.nelem;
   start   = theTimes->value.data.dblptr;
   stop    = theTimes->value.data.dblptr + nGTI;
   ordered = theTimes->type;

   if( theExpr->operation==CONST_OP ) {

      this->value.data.log = 
	 (Search_GTI( theExpr->value.data.dbl, nGTI, start, stop, ordered )>=0);
      this->operation      = CONST_OP;

   } else {

      Allocate_Ptrs( this );

      times = theExpr->value.data.dblptr;
      if( !gParse.status ) {

	 elem = gParse.nRows * this->value.nelem;
	 if( nGTI ) {
	    gti = -1;
	    while( elem-- ) {
	       if( (this->value.undef[elem] = theExpr->value.undef[elem]) )
		  continue;

            /*  Before searching entire GTI, check the GTI found last time  */
	       if( gti<0 || times[elem]<start[gti] || times[elem]>stop[gti] ) {
		  gti = Search_GTI( times[elem], nGTI, start, stop, ordered );
	       }
	       this->value.data.logptr[elem] = ( gti>=0 );
	    }
	 } else
	    while( elem-- ) {
	       this->value.data.logptr[elem] = 0;
	       this->value.undef[elem]       = 0;
	    }
      }
   }

   if( theExpr->operation>0 )
      free( theExpr->value.data.ptr );
}

static long Search_GTI( double evtTime, long nGTI, double *start,
			double *stop, int ordered )
{
   long gti, step;
                             
   if( ordered && nGTI>15 ) { /*  If time-ordered and lots of GTIs,   */
                              /*  use "FAST" Binary search algorithm  */
      if( evtTime>=start[0] && evtTime<=stop[nGTI-1] ) {
	 gti = step = (nGTI >> 1);
	 while(1) {
	    if( step>1L ) step >>= 1;
	    
	    if( evtTime>stop[gti] ) {
	       if( evtTime>=start[gti+1] )
		  gti += step;
	       else {
		  gti = -1L;
		  break;
	       }
	    } else if( evtTime<start[gti] ) {
	       if( evtTime<=stop[gti-1] )
		  gti -= step;
	       else {
		  gti = -1L;
		  break;
	       }
	    } else {
	       break;
	    }
	 }
      } else
	 gti = -1L;
      
   } else { /*  Use "SLOW" linear search  */
      gti = nGTI;
      while( gti-- )
	 if( evtTime>=start[gti] && evtTime<=stop[gti] )
	    break;
   }
   return( gti );
}

static void Do_REG( Node *this )
{
   Node *theRegion, *theX, *theY;
   double Xval=0.0, Yval=0.0;
   char   Xnull=0, Ynull=0;
   int    Xvector, Yvector;
   long   nelem, elem, rows;

   theRegion = gParse.Nodes + this->SubNodes[0];
   theX      = gParse.Nodes + this->SubNodes[1];
   theY      = gParse.Nodes + this->SubNodes[2];

   Xvector = ( theX->operation!=CONST_OP );
   if( Xvector )
      Xvector = theX->value.nelem;
   else {
      Xval  = theX->value.data.dbl;
   }

   Yvector = ( theY->operation!=CONST_OP );
   if( Yvector )
      Yvector = theY->value.nelem;
   else {
      Yval  = theY->value.data.dbl;
   } 

   if( !Xvector && !Yvector ) {

      this->value.data.log =
	 ( fits_in_region( Xval, Yval, (SAORegion *)theRegion->value.data.ptr )
	   != 0 );
      this->operation      = CONST_OP;

   } else {

      Allocate_Ptrs( this );

      if( !gParse.status ) {

	 rows  = gParse.nRows;
	 nelem = this->value.nelem;
	 elem  = rows*nelem;

	 while( rows-- ) {
	    while( nelem-- ) {
	       elem--;

	       if( Xvector>1 ) {
		  Xval  = theX->value.data.dblptr[elem];
		  Xnull = theX->value.undef[elem];
	       } else if( Xvector ) {
		  Xval  = theX->value.data.dblptr[rows];
		  Xnull = theX->value.undef[rows];
	       }

	       if( Yvector>1 ) {
		  Yval  = theY->value.data.dblptr[elem];
		  Ynull = theY->value.undef[elem];
	       } else if( Yvector ) {
		  Yval  = theY->value.data.dblptr[rows];
		  Ynull = theY->value.undef[rows];
	       }

	       this->value.undef[elem] = ( Xnull || Ynull );
	       if( this->value.undef[elem] )
		  continue;

	       this->value.data.logptr[elem] = 
		  ( fits_in_region( Xval, Yval,
				    (SAORegion *)theRegion->value.data.ptr )
		    != 0 );
	    }
	    nelem = this->value.nelem;
	 }
      }
   }

   if( theX->operation>0 )
      free( theX->value.data.ptr );
   if( theY->operation>0 )
      free( theY->value.data.ptr );
}

/*****************************************************************************/
/*  Utility routines which perform the calculations on bits and SAO regions  */
/*****************************************************************************/

static char bitlgte(char *bits1, int oper, char *bits2)
{
 int val1, val2, nextbit;
 char result;
 int i, l1, l2, length, ldiff;
 char stream[256];
 char chr1, chr2;

 l1 = strlen(bits1);
 l2 = strlen(bits2);
 if (l1 < l2)
   {
    length = l2;
    ldiff = l2 - l1;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l1--    ) stream[i++] = *(bits1++);
    stream[i] = '\0';
    bits1 = stream;
   }
 else if (l2 < l1)
   {
    length = l1;
    ldiff = l1 - l2;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l2--    ) stream[i++] = *(bits2++);
    stream[i] = '\0';
    bits2 = stream;
   }
 else
    length = l1;

 val1 = val2 = 0;
 nextbit = 1;

 while( length-- )
    {
     chr1 = bits1[length];
     chr2 = bits2[length];
     if ((chr1 != 'x')&&(chr1 != 'X')&&(chr2 != 'x')&&(chr2 != 'X'))
       {
        if (chr1 == '1') val1 += nextbit;
        if (chr2 == '1') val2 += nextbit;
        nextbit *= 2;
       }
    }
 result = 0;
 switch (oper)
       {
        case LT:
             if (val1 < val2) result = 1;
             break;
        case LTE:
             if (val1 <= val2) result = 1;
             break;
        case GT:
             if (val1 > val2) result = 1;
             break;
        case GTE:
             if (val1 >= val2) result = 1;
             break;
       }
 return (result);
}

static void bitand(char *result,char *bitstrm1,char *bitstrm2)
{
 int i, l1, l2, ldiff;
 char stream[256];
 char chr1, chr2;

 l1 = strlen(bitstrm1);
 l2 = strlen(bitstrm2);
 if (l1 < l2)
   {
    ldiff = l2 - l1;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l1--    ) stream[i++] = *(bitstrm1++);
    stream[i] = '\0';
    bitstrm1 = stream;
   }
 else if (l2 < l1)
   {
    ldiff = l1 - l2;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l2--    ) stream[i++] = *(bitstrm2++);
    stream[i] = '\0';
    bitstrm2 = stream;
   }
 while ( (chr1 = *(bitstrm1++)) ) 
    {
       chr2 = *(bitstrm2++);
       if ((chr1 == 'x') || (chr2 == 'x'))
          *result = 'x';
       else if ((chr1 == '1') && (chr2 == '1'))
          *result = '1';
       else
          *result = '0';
       result++;
    }
 *result = '\0';
}

static void bitor(char *result,char *bitstrm1,char *bitstrm2)
{
 int i, l1, l2, ldiff;
 char stream[256];
 char chr1, chr2;

 l1 = strlen(bitstrm1);
 l2 = strlen(bitstrm2);
 if (l1 < l2)
   {
    ldiff = l2 - l1;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l1--    ) stream[i++] = *(bitstrm1++);
    stream[i] = '\0';
    bitstrm1 = stream;
   }
 else if (l2 < l1)
   {
    ldiff = l1 - l2;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l2--    ) stream[i++] = *(bitstrm2++);
    stream[i] = '\0';
    bitstrm2 = stream;
   }
 while ( (chr1 = *(bitstrm1++)) ) 
    {
       chr2 = *(bitstrm2++);
       if ((chr1 == '1') || (chr2 == '1'))
          *result = '1';
       else if ((chr1 == '0') || (chr2 == '0'))
          *result = '0';
       else
          *result = 'x';
       result++;
    }
 *result = '\0';
}

static void bitnot(char *result,char *bits)
{
   int length;
   char chr;

   length = strlen(bits);
   while( length-- ) {
      chr = *(bits++);
      *(result++) = ( chr=='1' ? '0' : ( chr=='0' ? '1' : chr ) );
   }
   *result = '\0';
}

static char bitcmp(char *bitstrm1, char *bitstrm2)
{
 int i, l1, l2, ldiff;
 char stream[256];
 char chr1, chr2;

 l1 = strlen(bitstrm1);
 l2 = strlen(bitstrm2);
 if (l1 < l2)
   {
    ldiff = l2 - l1;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l1--    ) stream[i++] = *(bitstrm1++);
    stream[i] = '\0';
    bitstrm1 = stream;
   }
 else if (l2 < l1)
   {
    ldiff = l1 - l2;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l2--    ) stream[i++] = *(bitstrm2++);
    stream[i] = '\0';
    bitstrm2 = stream;
   }
 while( (chr1 = *(bitstrm1++)) )
    {
       chr2 = *(bitstrm2++);
       if ( ((chr1 == '0') && (chr2 == '1'))
	    || ((chr1 == '1') && (chr2 == '0')) )
	  return( 0 );
    }
 return( 1 );
}

static char bnear(double x, double y, double tolerance)
{
 if (fabs(x - y) < tolerance)
   return ( 1 );
 else
   return ( 0 );
}

static char saobox(double xcen, double ycen, double xwid, double ywid,
		   double rot,  double xcol, double ycol)
{
 double x,y,xprime,yprime,xmin,xmax,ymin,ymax,theta;

 theta = (rot / 180.0) * myPI;
 xprime = xcol - xcen;
 yprime = ycol - ycen;
 x =  xprime * cos(theta) + yprime * sin(theta);
 y = -xprime * sin(theta) + yprime * cos(theta);
 xmin = - 0.5 * xwid; xmax = 0.5 * xwid;
 ymin = - 0.5 * ywid; ymax = 0.5 * ywid;
 if ((x >= xmin) && (x <= xmax) && (y >= ymin) && (y <= ymax))
   return ( 1 );
 else
   return ( 0 );
}

static char circle(double xcen, double ycen, double rad,
		   double xcol, double ycol)
{
 double r2,dx,dy,dlen;

 dx = xcol - xcen;
 dy = ycol - ycen;
 dx *= dx; dy *= dy;
 dlen = dx + dy;
 r2 = rad * rad;
 if (dlen <= r2)
   return ( 1 );
 else
   return ( 0 );
}

static char ellipse(double xcen, double ycen, double xrad, double yrad,
		    double rot, double xcol, double ycol)
{
 double x,y,xprime,yprime,dx,dy,dlen,theta;

 theta = (rot / 180.0) * myPI;
 xprime = xcol - xcen;
 yprime = ycol - ycen;
 x =  xprime * cos(theta) + yprime * sin(theta);
 y = -xprime * sin(theta) + yprime * cos(theta);
 dx = x / xrad; dy = y / yrad;
 dx *= dx; dy *= dy;
 dlen = dx + dy;
 if (dlen <= 1.0)
   return ( 1 );
 else
   return ( 0 );
}

static void fferror(char *s)
{
    char msg[80];

    if( !gParse.status ) gParse.status = PARSE_SYNTAX_ERR;

    strncpy(msg, s, 80);
    msg[79] = '\0';
    ffpmsg(msg);
}

