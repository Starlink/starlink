/* Function: psx_access( name, mode, status )
*+
*  Name:
*     PSX_ACCESS

*  Purpose:
*     Check file accessibility

*  Language:
*     ANSI C

*  Invocation:
*     I = ACCESS( NAME, MODE, STATUS )

*  Description:
*     Provides a FORTRAN interface to the C library function access() to
*     determine the existence of a file for a specified type of access.

*  Arguments:
*     NAME = CHARACTER*(*) (Given)
*        The name of the file to test.
*     MODE = CHARACTER*(*) (Given)
*        The access mode - either a ' ' (space) to merely check
*        for file existence or one or more of the letters R,W,X.
*     STATUS = INTEGER (Given & Returned)
*        Inherited status.

*  Function value:
*     PSX_ACCESS = INTEGER
*        Zero if the routine has succeeded. Otherwise the value of errno.

*  References:
*     - POSIX Standard ISO/IEC 9945-1:1990

*  Copyright:
*     Copyright (C) 1995 CCLRC
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council

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
*     {enter_changes_her}

*  Implementation Status:
*     Has not been converted to use CNF.

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#if HAVE_CONFIG_H
# include <config.h>
#endif

#include "f77.h"

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#include <errno.h>

#if HAVE_STRING_H
# include <string.h>
#endif

/* Starlink status */
#include "sae_par.h"

F77_INTEGER_FUNCTION(psx_access) ( CHARACTER(fname), CHARACTER(mode),
                                   INTEGER(status)
                                   TRAIL(fname) TRAIL(mode) )
{
    GENPTR_CHARACTER(fname)
    GENPTR_CHARACTER(mode)
    GENPTR_INTEGER(status)
    char *tmpstr;
    int imode, npos, mpos, rval;

    /* Status check */
    if ( *status != SAI__OK ) return -1;

/* Check arguments */

    if( fname_length == 0 || mode_length == 0)
	return EINVAL;

/* Find the end of the file name and turn this into a zero terminated C
 * string
 */
    if((tmpstr = (char *) malloc(fname_length+1)) == NULL)
	return errno;
    strncpy(tmpstr, fname, fname_length);
    for(npos=0; npos<fname_length; npos++)
	if(*(tmpstr+npos) == ' ')
	    break;
    *(tmpstr+npos) = '\0';

/* Process mode argument */

    if(*(mode) == ' ')
	imode = F_OK;
    else {
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
		    free(tmpstr);
		    return EINVAL;
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

    free(tmpstr);

    return rval;

}

