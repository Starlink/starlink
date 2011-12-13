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
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1999-2004 CLRC. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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
*     16-FEB-2008 (TIMJ):
*        Use starFree not cnfFree.
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
  file = starMalloc( pathname_length + 1 );
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
    starFree( file );
  } else {
    *status = PSX__NOMEM;
    psx1_rep_c("PSX_REMOVE_ERR2",
               "Unable to allocate memory for path removal", status);

  }
}
