/*+  GETCWD -  Get process current directory
*    INTEGER FUNCTION GETCWD
*    Description :
*     Provides a FORTRAN interface to the C library function getcwd() to
*     obtain the current process directory.
*    Language:
*     C
*    Invocation :
*     I = GETCWD(DIRNAME)
*    Parameters :
*     DIRNAME (CHARACTER*(*)) (given)  A FORTRAN CHARACTER variable to hold
*                                      the returned directory name.
*    Return value:
*     Zero if C level getcwd() was successful. Else errno of the failure.
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

F77_INTEGER_FUNCTION(getcwd) ( CHARACTER(dirname) TRAIL(dirname) )
{
    GENPTR_CHARACTER(dirname)

    int npos;

/* Check argument */

    if( dirname_length == 0 )
	return EINVAL;

/* Get the directory name directly into the FORTRAN variable */

    if( getcwd(dirname, dirname_length) == NULL)
	return errno;

/* Space fill the string as expected by FORTRAN */

    for(npos = strlen(dirname); npos < dirname_length; npos++)
	*(dirname+npos) = ' ';

    return 0;
}
