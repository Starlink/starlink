/*+  ACCESS -  Check file access
*    INTEGER FUNCTION ACCESS 
*    Description :
*     Provides a FORTRAN interface to the C library function access() to
*     determine the existence of a file for a specified type of access.
*    Language:
*     C
*    Invocation :
*     I = ACCESS(NAME, MODE)
*    Parameters :
*     NAME (CHARACTER*(*))  The file name.
*     MODE (CHARACTER*(*))  The access mode - either a ' ' (space) to merely
*     check for file existence or one or more of the letters R,W,X
*    Return value:
*     Zero if the routine has succeeded. Otherwise the value of errno.
*    Method :
*     Uses standard Unix C system service routines
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     BKM: B.K. McIlwrath (STARLINK, RAL)
*    History :
*     date:  changes (institution::username)
*     15-DEC-1995 (BKM):
*        Original version.
*    endhistory
*/
#include "f77.h"
#include <unistd.h>
#include <errno.h>

F77_INTEGER_FUNCTION(access) ( CHARACTER(fname), CHARACTER(mode)
                               TRAIL(fname) TRAIL(mode) )
{
    GENPTR_CHARACTER(fname)
    GENPTR_CHARACTER(mode)
    char *tmpstr;
    int imode, npos, mpos, rval;

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
    rval = access(tmpstr, imode);
    free(tmpstr);

    return rval;

}

