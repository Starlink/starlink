/*
*+
*  Name:
*     PUTENV

*  Purpose:
*     Set environment variable

*  Language:
*     C

*  Invocation:
*     I = PUTENV(VAR=VAL)

*  Description:
*     Provides a FORTRAN interface to the C library function putenv() to
*     set an environment variable.

*  Arguments:
*     VAR=VAL (CHARACTER*(*))  variable_name=Value

*  Algorithm:
*     Uses standard Unix C system service routines

*  Authors:
*     AJC: A.J.Chipperfield (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1997 (AJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Return Value:
*     Zero if C level putenv() was successful. Else errno of the failure.

*-
*/
#include "f77.h"
#include <string.h>
#include <unistd.h>
#include <errno.h>

F77_INTEGER_FUNCTION(putenv) ( CHARACTER(value) TRAIL(value) )
{
    GENPTR_CHARACTER(value)
    char *tmpstr;
    int npos, rval;

/* Check argument */

    if( value_length == 0 )
	return EINVAL;

/* Find the end of the value string and turn this into a zero terminated C
 * string
 */
    if((tmpstr = (char *) malloc(value_length+1)) == NULL)
	return errno;
    strncpy(tmpstr, value, value_length);
    for(npos=value_length; npos>0; npos--)
	if(*(tmpstr+npos) != ' ')
	    break;
    *(tmpstr+npos) = '\0';

    rval = putenv(tmpstr);
/*    free(tmpstr);*/

    return rval;

}
