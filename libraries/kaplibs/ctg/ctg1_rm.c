#include "f77.h"
#include "sae_par.h"
#include <stdio.h>

F77_SUBROUTINE(ctg1_rm)( CHARACTER(FILE), INTEGER(STATUS) TRAIL(FILE) ){
/*
*  Name:
*     CTG1_RM

*  Purpose:
*     Remove a file.

*  Description:
*     This C function calls the "remove" RTL function to remove a specified
*     file. No error occurs if the file cannot be removed for any reason.

*  Parameters:
*     FILE = CHARACTER * ( * ) (Given)
*        The path to the file.
*     STATUS = INTEGER (Given and Returned)
*        The inherited global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-SEP-1999 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/

   GENPTR_CHARACTER(FILE)
   GENPTR_INTEGER(STATUS)

   char *file;

/* Check the global status. */
   if( *STATUS != SAI__OK ) return;

/* Allocate memory to store a null-terminated copy of the file name. */
   file = (char *) malloc( FILE_length + 1 );
   if ( file) {

/* Copy the blank padded fortran file name to a null terminated C string. */
      cnf_imprt( FILE, FILE_length, file );

/* Remove the file. */
      (void) remove( file );

/* Free the memory. */
      free( file );
   }

}
