#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "f77.h"
#include "cnf.h"
#include "sae_par.h"

F77_SUBROUTINE(lpg1_tmpnm)( CHARACTER(TMPNAM), INTEGER(STATUS) TRAIL(TMPNAM) ){
/*
*  Name:
*     LPG1_TMPNM

*  Purpose:
*     Returns a unique temporary filename.

*  Description:
*     This C function calls mkstemp to create a a unique temporary filename.

*  Parameters:
*     TMPNAM = CHARACTER * ( * ) (Returns)
*        The file name, or blank if an error occurred.
*     STATUS = INTEGER (Given and Returned)
*        The inherited global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     15-MAR-2004 (DSB):
*        Original version.
*     12-SEP-2005 (TIMJ):
*        Fix compiler warnings
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/

   GENPTR_CHARACTER(TMPNAM)
   GENPTR_INTEGER(STATUS)

   char fname[ 255 ];
   int ifd;

/* Check the global status. */
   if( *STATUS != SAI__OK ) return;

/* Get a unique temporary file name. */
   strcpy( fname, "lpgtmpXXXXXX" );
   ifd = mkstemp( fname );

/* If an error occurred,return a blank string. */
   if( ifd == -1 ){
      fname[ 0 ] = 0;
   } else {
      close( ifd );
      remove( fname );
   }

/* Copy the string to the function argument */
   cnfExprt( fname, TMPNAM, TMPNAM_length );
}

