#include "f77.h"
#include "sae_par.h"
#include "psx_err.h"
#include <stdio.h>
#include "psx1.h"
#include "ems.h"
#include <errno.h>
#include "star/mem.h"

F77_SUBROUTINE(psx_remove)( CHARACTER(pathname), INTEGER(status) 
			    TRAIL(pathname) ){
/*
*+
*  Name:
*     PSX_REMOVE

*  Purpose:
*     Remove a file or directory

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_REMOVE( PATHNAME, STATUS )

*  Description:
*     This C function calls the "remove" RTL function to remove a specified
*     file or directory. It is equivalent to "unlink" for files and "rmdir"
*     for directories. A directory must be empty. On error, STATUS is set
*     to PSX__ERRNO and the error message will contain the system error
*     message.

*  Arguments:
*     PATHNAME = CHARACTER * ( * ) (Given)
*        The path to the file.
*     STATUS = INTEGER (Given and Returned)
*        The inherited global status.

*  Examples:
*     CALL PSX_REMOVE( 'tmp.dat', STATUS )
*        This will remove the file tmp.dat

*  External Routines Used:
*     cnf: cnfImprt
*     ems: emsSyser

*  References:
*     - POSIX standard, IEEE Std 1003.1

*  Copyright:
*     Copyright (C) 1999-2004 CLRC

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     6-SEP-1999 (DSB):
*        Original version (LPG/CTG).
*     3-OCT-2004 (TIMJ):
*        PSX-ify. Status is now set on error.
*     23-FEB-2006 (TIMJ):
*        Use starMalloc
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

   GENPTR_CHARACTER(pathname)
   GENPTR_INTEGER(status)

   char *file = NULL;
   int rstatus = 0;

/* Check the global status. */
   if( *status != SAI__OK ) return;

/* Allocate memory to store a null-terminated copy of the file name. */
   file = (char *) starMalloc( pathname_length + 1 );
   if ( file ) {

/* Copy the blank padded fortran file name to a null terminated C string. */
      cnfImprt( pathname, pathname_length, file );

/* Remove the file. */
      rstatus = remove( file );

/* Check status value */
      if (rstatus == -1) {
	*status = PSX__ERRNO;
	emsSyser("ERRNO", errno);
	emsSetc("PATH", file );
	psx1_rep_c("PSX_REMOVE_ERR",
		   "Error removing '^PATH': ^ERRNO", status);

      }

/* Free the memory. */
      cnfFree( file );
   } else {
     *status = PSX__NOMEM;
     psx1_rep_c("PSX_REMOVE_ERR2",
		"Unable to allocate memory for path removal", status);

   }
}
