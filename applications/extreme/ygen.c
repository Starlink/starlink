/*
*+
*  Name:
*     ygen.c
*
*  Type of module:
*     C source code.
*
*  Purpose:
*     Common routines used for source code processing.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     25-NOV-1999 (MBT):
*        Intial version.
*     24-JAN-2000 (MBT):
*        Adapted for use by EXTREME.
*-
*/


#include <stdio.h>
#include <sys/file.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#define DEFINE_GLOBALS 1
#include "ygen.h"


   char *filter( int argc, char **argv ) {
/*
*+
*  Name:
*     filter
*
*  Purpose:
*     Process command line arguments to set up filter program.
*
*  Return value:
*     ifilter = char *
*        The name under which the executable was invoked.  Only the bare
*        name is returned, not any path information.
*-
*/

/* Declare local variables. */
      char *pc;                         /* Pointer to character              */
      char *name;                       /* Return value                      */
      char *usagef;                     /* Usage format string               */

/* Get name of program etc. */
      name = *(argv++);
      argc--;
      if ( ( pc = strrchr( name, '/' ) ) != NULL ) name = pc + 1;
      usagef = "Usage: %s [ in [ out ] ]\n";

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

/* Finish. */
      return name;
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
*     The length of the preval buffer is extended if necessary.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     10-DEC-1999 (MBT):
*        Initial revision.
*-
*/

/* Extend allocation if necessary and add the new character. */
      if ( preleng + 1 > prealloc ) {
         if ( prealloc == 0 )
            preval = (char *) memok( malloc( BUFINC + 1 ) );
         else
            preval = (char *) memok( realloc( preval, prealloc + BUFINC + 1 ) );
         prealloc++;
      }
      preval[ preleng ] = c;
      preval[ ++preleng ] = '\0';
   }


/* $Id$ */
