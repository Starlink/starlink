/*
*+
*  Name:
*     tag.h
*
*  Purpose:
*     Include file for tagging routines.
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
*-
*/

/* Functions for keeping track of allocated strings in the current unit. */
      void uclear();
      void unew();
      void uadd( char *item );
      char *ucontent();

/* Functions for keeping track of whitespace-type characters to be put
   into yylval but not yytext. */
      void sappend( const char *s );
      void cappend( char c );

/* Function for doing grammar-specific tidying after the parser is finished. */
      void tagwrap();

/* Utility functions. */
      void *memok( void *ptr );

/* Configuration flags. */
      int strict;

/* Function for string concatenation. */
      char *scat( int n, ... );

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

#define BUFINC 1024


/* $Id$ */
