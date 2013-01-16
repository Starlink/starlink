/*
*+
*  Name:
*     tag.c
*
*  Type of module:
*     C source code.
*
*  Purpose:
*     Common routines used by lex and yacc files for source code tagging.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     25-NOV-1999 (MBT):
*        Intial version.
*
*-
*/


#include <stdio.h>
#include <sys/file.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include "tag.h"

int yy_flex_debug;
int yydebug;

   int main( int argc, char **argv ) {
/*
*+
*  Name:
*     main
*
*  Purpose:
*     Harness routine for calling the yacc-generated parser.
*
*  Flags:
*     -s
*        If present, this enforces strict parsing; any parse errors lead
*        to program termination.  By default, parse errors are silently
*        tolerated.
*     -d
*        Debugging.  It may be followed immediately by the letters 'l',
*        'y' or both.  This turns on the debugging messages in the lex
*        part and/or the yacc part of the processor respectively.
*        This will only work if lex and yacc have been compiled to
*        enable debugging reports.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     25-NOV-1999 (MBT):
*        Original version.
*-
*/

/* Declare external function. */
      int yyparse();

/* Declare local variables. */
      int retval;
      char *name, *usagef, *text;
      char c;

/* Get name of program etc. */
      name = *(argv++);
      argc--;
      usagef = "Usage: %s [-d[l][y]] [-s] [ in [ out ] ]\n";

/* Work through any command line flags. */
      yy_flex_debug = 0;
      yydebug = 0;
      strict = 0;
      while ( argc > 0 && **argv == '-' ) {
         switch( *(++(*argv)) ) {

/* Debugging output flag. */
            case 'd':
               while ( c = *(++(*argv)) )
                  switch( c ) {
                     case 'l':
                        yy_flex_debug = 1;
                        break;
                     case 'y':
                        yydebug = 1;
                        break;
                  };
               break;

/* Strict error handling flag. */
            case 's':
               strict = 1;
               break;

/* Reply to any other flag (including -h) with a usage message. */
            default:
               printf( usagef, name );
               exit( 1 );
         }
         argv++;
         argc--;
      }

/* Open standard input and output appropriately according to command line
   arguments, in the normal filter-type way. */
      switch( argc ) {
         case 2:
            if ( freopen( argv[ 1 ], "w", stdout ) == NULL ) {
               perror( argv[ 1 ] );
               exit( 1 );
            }
         case 1:
            if ( freopen( argv[ 0 ], "r", stdin ) == NULL ) {
               perror( argv[ 0 ] );
               exit( 1 );
            }
         case 0:
            break;
         default:
            printf( usagef, name );
            exit( 1 );
      }

/* Some initialisation. */
      unew();
      preleng = 0;
      prealloc = 0;
      preval = "";

/* Call the parser. */
      retval = yyparse();

/* Some tidying. */
      text = ucontent();
      printf( "%s", text );
      free( text );
      uclear();
      tagwrap();

/* Return. */
      return( retval );
   }


   void *memok( void *ptr ) {
/*
*+
*  Name:
*     memok
*
*  Purpose:
*     Check that memory has been allocated successfully.
*
*  Description:
*     This routine checks that a pointer does not point to NULL.  It
*     should be called on any pointer value which is got by a call to
*     malloc, realloc or calloc.
*
*     If the argument is non-NULL then the routine returns without action.
*     If it is NULL, the routine terminates the program with an error
*     status.
*
*  Arguments:
*     memok = void *
*        A pointer, presumably a return value from malloc, realloc or calloc.
*
*  Return value:
*     The same as the argument ptr.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     09-DEC-1999 (MBT):
*        Initial revision.
*-
*/
      if ( ptr == NULL ) {
         fprintf( stderr, "Memory allocation failed.\n" );
         exit( 1 );
      }
      return( ptr );
   }


   char *scat( int n, ... ) {
/*+
*  Name:
*     scat
*
*  Invocation:
*     string = scat( n, ... )
*
*  Purpose:
*     Concatenate a list of strings.
*
*  Arguments:
*     n = int
*        The number of strings to be concatenated.  n may be zero, in
*        which case a newly allocated string of length zero is returned.
*     sp1, sp2, ... = char *
*        The other arguments are all strings, and there are n of them.
*        free() is called on each of them, so they must have been malloc'd
*        at some time in the past, and must not be used subsequent to
*        passing to this function.
*
*  Return value:
*     string = char *
*        The return value is a string containing the concatenation of
*        all the strings supplied.  It is obtained using malloc, so
*        should be free'd at some time in the future.
*
*  Description:
*     This routine returns a newly malloc'd string which is the concatenation
*     of all the strings supplied to it as arguments.  Each of those arguments
*     gets free'd by this routine, so they must have been malloc'd (probably
*     by this routine) in the past, and must not be referred to again after
*     calling this routine.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     10-DEC-1999 (MBT):
*        Initial revision.
*-
*/

/* Local variables. */
      va_list ap;
      int len, i;
      char *string, *sp;

/* Work out the length of the final string. */
      len = 0;
      va_start( ap, n );
      for ( i = 0; i < n; i++ ) {
         sp = va_arg( ap, char * );
         len += strlen( sp );
      }
      va_end( ap );

/* Allocate the memory we will need, and initialise it. */
      string = (char *) memok( malloc( len + 1 ) );
      *string = '\0';

/* Copy the arguments into the allocated space.  We free the storage used
   by each argument as we go along. */
      va_start( ap, n );
      for ( i = 0; i < n; i++ ) {
         sp = va_arg( ap, char * );
         strcat( string, sp );
         free( sp );
      }
      va_end( ap );

/* Return. */
      return( string );
   }


/*
*  Define the elements of the list of as-yet unoutput strings.
*  This list is maintained so that if there is an error in the yacc
*  grammar parsing, in which case yacc throws away all the tokens
*  back to the end of the last unerroneous unit, we can output
*  the text which has been omitted.  Under normal (non-error)
*  circumstances however this list is simply discarded at the end of
*  each correctly parsed unit at the same time that the unit is output.
*
*  The routines following this are to be used to manipulate this list.
*/
   static ELEMENT ubase = { "", (ELEMENT *) NULL };
   static ELEMENT *ufirst, *ulast;


   void uclear() {
/*
*+
*  Name:
*     uclear
*
*  Purpose:
*     Clear the list of unoutput strings.
*
*  Description:
*     This routine reclaims the memory used by the list of unoutput
*     strings and initialises the values of the start and end pointers.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     10-DEC-1999 (MBT):
*        Initial revision.
*-
*/
      ELEMENT *i, *j;

/* Reclaim memory. */
      i = ufirst->next;
      while ( i != NULL ) {
         j = i->next;
         if ( i->text != NULL )
            free( i->text );
         free( i );
         i = j;
      }

/* Reset pointers. */
      unew();
   }

   void unew() {
/*
*+
*  Name:
*     unew
*
*  Purpose:
*     Initialise list of unoutput strings.
*
*  Description:
*     This resets the pointers for the list of unoutput strings.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     10-DEC-1999 (MBT):
*        Initial revision.
*-
*/
      ufirst = &ubase;
      ulast = &ubase;
      ubase.text = "";
      ubase.next = (ELEMENT *) NULL;
   }

   void uadd( char *item ) {
/*
*+
*  Name:
*     uadd
*
*  Purpose:
*     Add a string to the list of unoutput strings.
*
*  Description:
*     This routine adds a string to the list of unoutput strings.  Every
*     character encountered by the lexer should be added to this list
*     as it is encountered.
*
*  Parameters:
*     item = char *
*        The string to be added.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     10-DEC-1999 (MBT):
*        Initial revision.
*-
*/
      ulast->next = (ELEMENT *) memok( malloc( sizeof( ELEMENT ) ) );
      ulast = ulast->next;
      ulast->next = NULL;
      ulast->text = (char *) memok( malloc( strlen( item ) + 1 ) );
      strcpy( ulast->text, item );
   }

   char *ucontent() {
/*
*+
*  Name:
*     ucontent
*
*  Purpose:
*     Get unoutput string.
*
*  Description:
*     This routine returns a string which consists of all the strings
*     which have had uadd() called on them since the last call to
*     unew().
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     10-DEC-1999 (MBT):
*        Initial revision.
*-
*/
      ELEMENT *i;
      char *text;
      int j, leng;

/* Work out how long the whole string is. */
      leng = 0;
      for ( i = ufirst; i != NULL; i = i->next ) {
         leng += strlen( i->text );
      }

/* Allocate space for the string. */
      text = (char *) memok( malloc( leng + 1 ) );
      j = 0;

/* Copy the lexically encountered items into the string sequentially. */
      for ( i = ufirst; i != NULL; i = i->next ) {
         strcpy( text + j, i->text );
         j += strlen( i->text );
      }
      text[ j ] = '\0';

/* Return the concatenated string. */
      return( text );
   }


   void sappend( const char *s ) {
/*
*+
*  Name:
*     sappend
*
*  Purpose:
*     Append a string to the preval string.
*
*  Description:
*     This routine appends a string to the preval string.  If the preval
*     string is not long enough to hold the new one, then more space
*     is allocated for it.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     10-DEC-1999 (MBT):
*        Initial revision.
*-
*/

/* Local variables. */
      int leng;

/* See how many extra characters are to be added to preval. */
      leng = strlen( s );

/* Extend the allocated space if necessary. */
      while ( preleng + leng > prealloc ) {
         if ( prealloc == 0 ) {
            preval = (char *) memok( malloc( BUFINC + 1 ) );
            *preval = '\0';
         }
         else {
            preval = (char *) memok( realloc( preval, prealloc + BUFINC + 1 ) );
         }
         prealloc += BUFINC;
      }

/* Append the string to preval. */
      strcat( preval, s );
      preleng += leng;
   }


   void cappend( char c ) {
/*
*+
*  Name:
*     cappend
*
*  Purpose:
*     Append a character to the preval string.
*
*  Description:
*     This routine appends a single character to the preval string.
*     If the character is '<', '>' or '&', then it is replaced in the
*     preval string by the appropriate HTML entity reference.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     10-DEC-1999 (MBT):
*        Initial revision.
*-
*/

/* Switch on the value of the character. */
      switch( c ) {

/* If it needs to be replaced by an entity reference, do so via sappend. */
         case '<':
            sappend( "&lt;" );
            break;
         case '>':
            sappend( "&gt;" );
            break;
         case '&':
            sappend( "&amp;" );
            break;

/* Otherwise it's just a single character: extend allocation if necessary
   and add the new character. */
         default:
            if ( preleng + 1 > prealloc ) {
               if ( prealloc == 0 )
                  preval = (char *) memok( malloc( BUFINC + 1 ) );
               else
                  preval = (char *) memok( realloc( preval,
                                                    prealloc + BUFINC + 1 ) );
               prealloc++;
            }
            preval[ preleng ] = c;
            preval[ ++preleng ] = '\0';
      }
   }


/* $Id$ */
