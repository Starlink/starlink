/*+  CHDIR -  Change process current directory
     INTEGER FUNCTION CHDIR 
*    Description :
*     Provides a FORTRAN interface to the C library function chdir() to
*     change the process current directory.
*    Language:
*     C
*    Invocation :
*     I = CHDIR(DIRNAME)
*    Parameters :
*     DIRNAME (CHARACTER*(*))  The desired new directory name.
*    Return value:
*     Zero if C level chdir() was successful. Else errno of the failure.
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

F77_INTEGER_FUNCTION(chdir) ( CHARACTER(dirname) TRAIL(dirname) )
{
    GENPTR_CHARACTER(dirname)
    char *tmpstr;
    int npos, rval;

/* Check argument */

    if( dirname_length == 0 )
	return EINVAL;

/* Find the end of the directory name and turn this into a zero terminated C
 * string
 */
    if((tmpstr = (char *) malloc(dirname_length+1)) == NULL)
	return errno;
    strncpy(tmpstr, dirname, dirname_length);
    for(npos=0; npos<dirname_length; npos++)
	if(*(tmpstr+npos) == ' ')
	    break;
    *(tmpstr+npos) = '\0';

    rval = chdir(tmpstr);
    free(tmpstr);

    return rval;

}
