#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include "f77.h"
#include "sae_par.h"

#include <stdlib.h>
#include <stdio.h>
#include "merswrap.h"

static void Error( const char *, int * );
extern F77_SUBROUTINE(err_rep)( CHARACTER(param), CHARACTER(mess),
                                INTEGER(STATUS) TRAIL(param) TRAIL(mess) );


F77_SUBROUTINE(kps1_memry)( INTEGER(MEM), INTEGER(STATUS) ){
/*
*  Name:
*     kps1_memry

*  Purpose:
*     Return memory currently being used by the current process.

*  Description:
*     This subroutine returns the maximum resident set size for the
*     current proces.

*  Parameters:
*     MEM = INTEGER (Returned)
*        The memory currently being used by the current process, in
*        KB.
*     STATUS = INTEGER (Given and Returned)
*        The inherited global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-NOV-2002 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/

   GENPTR_INTEGER(MEM)
   GENPTR_INTEGER(STATUS)
   struct rusage usage;

   *MEM = 0;

/* Check the global status. */
   if( *STATUS != SAI__OK ) return;

   if( getrusage( RUSAGE_SELF, &usage ) != 0 ) {
      *STATUS = SAI__ERROR;
      Error( strerror( errno ), STATUS );
   } else {
      *MEM = (int) usage.ru_maxrss*getpagesize()/1024;
   }
}

static void Error( const char *text, int *STATUS ) {
/*
*  Name:
*     Error

*  Purpose:
*     Report an error using EMS.

*  Description:
*     The supplied text is used as the text of the error message.
*     A blank parameter name is used.

*  Parameters:
*     text
*        The error message text. Only the first 80 characters are used.
*     STATUS
*        A pointer to the global status value. This should have been set
*        to a suitable error value before calling this function.

*  Notes:
*     - If a NULL pointer is supplied for "text", no error is reported.
*/

   DECLARE_CHARACTER(param,1);
   DECLARE_CHARACTER(mess,80);
   int j;

/* Check the supplied pointer. */
   if( text ) {

/* Set the parameter name to a blank string. */
      param[0] = ' ';

/* Copy the first "mess_length" characters of the supplied message into 
      "mess". */
      strncpy( mess, text, mess_length );

/* Pad any remaining bytes with spaces (and replace the terminating null
   character with a space). */
      for( j = strlen(mess); j < mess_length; j++ ) {
         mess[ j ] = ' ';
      }

/* Report the error. */
      F77_CALL(err_rep)( CHARACTER_ARG(param), CHARACTER_ARG(mess),
                         INTEGER_ARG(STATUS) TRAIL_ARG(param) 
                         TRAIL_ARG(mess) );
   }
}

