/*
*+
*  Name:
*     tag.h
*
*  Purpose:
*     Include file for parsing routines.
*
*  Description:
*     This include file contains the shared declarations for the C code
*     which forms the lex/yacc based source code parsers.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     10-DEC-1999 (MBT):
*        Initial revision.
*     24-JAN-2000 (MBT):
*        Adapted to use for inscnf.
*-
*/

/* Functions for keeping track of whitespace-type characters to be put
   into yylval but not yytext. */
      void sappend( char *s );
      void cappend( char c );

/* Utility functions. */
      void *memok( void *ptr );

/* Configuration flags. */
      int strict;

/* Generic linked list type. */
      struct element {
         char *text;
         struct element *next;
      };
      typedef struct element ELEMENT;

/*
*  These definitions set up variables to be used for passing text to
*  the parser.  When each token is returned, as well as the matched
*  characters an extra string is prepended to them as follows:
*
*     preval
*        The text to precede the next yylval.
*     preleng  
*        The length of preval, excluding the terminating NULL.
*     prealloc
*        The space allocated for preval, excluding the terminating NULL.
*
*  The routines which modify these keep the amount of space allocated
*  and the length values up to date.
*/
   char *preval;
   int preleng;
   int prealloc;

/* Amount by which to increase the preval buffer each time it needs
   increasing.  Its value affects only time/memory efficiency. */
#define BUFINC 4096

/* Define type of yylval lex global. */
typedef char * STRING;
#define YYSTYPE STRING
   YYSTYPE yylval;


/* Define token numbers for yylex return values.  Since we don't have yacc,
   we have to do this by hand.  The values of these are not significant,
   but they must be ints out of the range of ASCII characters, and all
   different. */
#define LINE_START 257
#define LINE_END 258
#define BLANK_LINE 259
#define COMMENT_LINE 260
#define SUBROUTINE 261
#define ENTRY 262
#define BLOCKDATA 263
#define PROGRAM 264
#define FUNCTION 265
#define INTEGER 266
#define REAL 267
#define DOUBLEPRECISION 268
#define COMPLEX 269
#define LOGICAL 270
#define CHARACTER 271
#define BYTE 272
#define UBYTE 273
#define WORD 274
#define UWORD 275
#define GENERIC_TYPE 276
#define DIMENSION 277
#define INCLUDE 278
#define IF 279
#define ELSEIF 280
#define THEN 281
#define CALL 282
#define INTEGER_CONSTANT 283
#define STRING_CONSTANT 284
#define ILLEGAL_CHAR 285
#define TOKEN 286
#define VAL 287
#define END 288
#define CNF_PVAL 289


/* $Id$ */
