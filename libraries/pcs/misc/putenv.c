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

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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
