/*
*+
*  Name:
*     PSX_ACCESS

*  Purpose:
*     Check file accessibility

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_ACCESS( NAME, MODE, ACCESSIBLE, REASON, STATUS )

*  Description:
*     Provides a FORTRAN interface to the C library function access() to
*     determine the existence of a file for a specified type of access.

*  Arguments:
*     NAME = CHARACTER*(*) (Given)
*        The name of the file to test.
*     MODE = CHARACTER*(*) (Given)
*        The access mode - either a ' ' (space) to merely check
*        for file existence or one or more of the letters R,W,X.
*     ACCESSIBLE = LOGICAL (Returned)
*        .TRUE. if the access mode is allowed. .FALSE. otherwise.
*     REASON = INTEGER (Returned)
*        Error code (errno) describing the reason. Can be passed to EMS_SYSER
*        for translation if required. Will be zero if ACCESSIBLE is true or
*        if status was set.
*     STATUS = INTEGER (Given & Returned)
*        Inherited status.

*  References:
*     - POSIX Standard ISO/IEC 9945-1:1990

*  Copyright:
*     Copyright (C) 1995 CCLRC
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council

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

*  Notes:
*     Some FORTRAN compilers have an ACCESS intrinsic, but not all.
*     This PSX routine is provided for portability.

*  Authors:
*     BKM: Brian McIlwrath (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     15-DEC-1995 (BKM):
*        Original version.
*     11-AUG-2005 (TIMJ):
*        Move from pcs/misc to the PSX library because the g95
*        gets confused if you declare an INTRINSIC as EXTERNAL
*        and gfortran does not (yet) include the ACCESS intrinsic.
*     15-FEB-2006 (TIMJ):
*        Make more PSX-like in its calling style.
*        Use CNF.
*     {enter_changes_her}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#if HAVE_CONFIG_H
# include <config.h>
#endif

#include "f77.h"
#include "ems.h"
#include "psx1.h"

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#include <errno.h>

#if HAVE_STRING_H
# include <string.h>
#endif

/* Starlink status */
#include "sae_par.h"

F77_SUBROUTINE(psx_access) ( CHARACTER(fname), CHARACTER(mode),
			     LOGICAL(accessible), INTEGER(reason), INTEGER(status)
			     TRAIL(fname) TRAIL(mode) )
{
    GENPTR_CHARACTER(fname)
    GENPTR_CHARACTER(mode)
    GENPTR_INTEGER(status)
    char *tmpstr;
    int imode, mpos, rval;

    /* Status check */
    *accessible = F77_FALSE;
    *reason = 0;
    if ( *status != SAI__OK ) return;

/* Check arguments */

    if( fname_length == 0 || mode_length == 0) {
      *status = SAI__ERROR;
      psx1_rep_c("PSX_ACCESS_ERR1", "PSX_ACCESS: Zero length string supplied",
		 status);
      return;
    }

    /* Import strings */
    tmpstr = cnfCreim( fname, fname_length );

    /* Process mode argument */

    if(*(mode) == ' ') {
      imode = F_OK;
    } else {
        imode = 0;
	for(mpos=0; mpos<mode_length; mpos++)
	    if(*(mode+mpos) == ' ')
		break;
	    else
		switch (*(mode+mpos)) {
		  case 'r':
		  case 'R':
		    imode |= R_OK;
		    break;
		  case 'w':
		  case 'W':
		    imode |= W_OK;
		    break;
		  case 'x':
		  case 'X':
		    imode |= X_OK;
		    break;
		  default:
		    cnfFree(tmpstr);
		    *status = SAI__ERROR;
		    emsSetnc("MODE",mode, mode_length);
		    psx1_rep_c("PSX_ACCESS_ERR2",
			       "Invalid access mode string ^MODE. Can only contain R, W or X",
			       status);
		    return;
		}
    }

# if HAVE_ACCESS
    rval = access(tmpstr, imode);
# else
    *status = SAI__ERROR;
    psx1_rep_c("PSX_ACCESS_ERR",
	       "Your system library does not have the ACCESS system call",
	       status);
# endif

    if ( rval == 0 ) {
      *accessible = F77_TRUE;
    } else {
      *reason = errno;
      *accessible = F77_FALSE;
    }

    cnfFree(tmpstr);

}

