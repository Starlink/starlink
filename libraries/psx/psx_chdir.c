/*
*+
*  Name:
*     PSX_CHDIR

*  Purpose:
*     Change current working directory

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL PSX_CHDIR( DIR, STATUS )

*  Description:
*     Provides a Fortran interface to change working directory.

*  Arguments:
*     DIR = CHARACTER * ( * ) (Given)
*        On exit contains the name of the current directory. Status will be set to
*        PSX__ERRNO on error.
*     STATUS = INTEGER (Given & Returned)
*        The global status. No action takes place if status is bad on entry.

*  References:
*     - POSIX Standard IEEE Std 1003.1-1988

*  Copyright:
*     Copyright (C) Particle Physics and Astronomy Research Council 2006

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  History:
*     15-FEB-2006 (TIMJ):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include <unistd.h>
#include <errno.h>

#include "sae_par.h"
#include "f77.h"
#include "psx_err.h"
#include "psx1.h"
#include "ems.h"

F77_SUBROUTINE(psx_chdir)( CHARACTER(DIR), INTEGER(STATUS) TRAIL(DIR))
{

  GENPTR_CHARACTER(DIR)
  GENPTR_INTEGER(STATUS)
  int err;             /* Local errno */
  char * dir_c;        /* Local copy of the string in C format */
  int lstat;           /* Status from chdir */

  if (*STATUS != SAI__OK) return;

  if ( DIR_length == 0 ) {
    err = EINVAL;
    goto ERROR;
  }

  dir_c = cnfCreim( DIR, DIR_length );
  if (dir_c == NULL) {
    err = ENOMEM;
    goto ERROR;
  }

  lstat = chdir( dir_c );
  err = errno;
  cnfFree( dir_c );

  if (lstat != 0) goto ERROR;
  return;

 ERROR:
  /* Report the error */
  emsSyser( "REASON", err );
  emsSetnc( "DIR", DIR, DIR_length);
  *STATUS = PSX__ERRNO;
  psx1_rep_c( "PSX_CHDIR_ERR", "Error changing current working directory to '^DIR' - ^REASON",
	      STATUS);
  return;

}
