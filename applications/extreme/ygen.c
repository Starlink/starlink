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
*     24-JAN-2000 (MBT):
*        Adapted for use by inscnf.
*-
*/


#include <stdio.h>
#include <sys/file.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include "ygen.h"


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



   void sappend( char *s ) {
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
