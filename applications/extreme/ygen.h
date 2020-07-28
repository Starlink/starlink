/*
*+
*  Name:
*     ygen.h
*
*  Purpose:
*     Include file for parsing routines.
*
*  Description:
*     This include file contains the shared declarations for the C code
*     which forms the lex based source code parsers.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     10-DEC-1999 (MBT):
*        Initial revision.
*     24-JAN-2000 (MBT):
*        Adapted to use for EXTREME.
*     28-JUL-2020 (DSB):
*        Only define the globals if requested. Otherwise just declare
*        them. Previous behaves causes errors with gcc 10.
*-
*/

/* Set the value of macro EXTERN so that the variables are either
   defined or declared, as required. */
#ifdef DEFINE_GLOBALS
#define CEXTERN
#else
#define CEXTERN  extern
#endif

/* Functions for keeping track of whitespace-type characters to be put
   into yylval but not yytext. */
      void sappend( char *s );
      void cappend( char c );

/* Utility functions. */
      int yylex();
      void *memok( void *ptr );
      char *filter( int argc, char **argv );

/* Task top-level functions. */
      void inscnf();
      void crepint();

/* Generic linked list type. */
      struct element {
         char *text;
         struct element *next;
      };
      typedef struct element ELEMENT;

/* Name of the executable. */
CEXTERN      char *name;

/* Start of part of yylval string which actually matches the token, rather
   than the preceding fluff. */
CEXTERN      char *ymatst;

/* Useful macros. */
#define MAXIMUM(x,y) ((x) > (y) ? (x) : (y))
#define MINIMUM(x,y) ((x) < (y) ? (x) : (y))

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
CEXTERN   char *preval;
CEXTERN   int preleng;
CEXTERN   int prealloc;

/* Amount by which to increase the preval buffer each time it needs
   increasing.  Its value affects only time/memory efficiency. */
#define BUFINC 4096

/* Define type of yylval lex global. */
typedef char * STRING;
#define YYSTYPE STRING
CEXTERN   YYSTYPE yylval;



/* $Id$ */
