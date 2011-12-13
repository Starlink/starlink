/*
*+
*  Name:
*     GWM_Error

*  Purpose:
*     Handles error message writing.

*  Language:
*     C

*  Invocation:
*     GWM_Error( status );

*  Description:
*     Prints an error message on stderr. The message includes the
*     error number so that an error reported by a child process can be piped
*     to the parent process and re-reported in the normal way.

*  Arguments:
*     status = int (Given)
*        Error number.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     DLT: D L Terrett (Starlink)

*  History:
*     14-MAR-1991 (DLT):
*         Original version.

*-
*/

/*
**
**  INCLUDE FILES
**
*/

#include <stdio.h>
#include "gwm_err.h"
#include "gwm_sys.h"
/*
**
**  MACRO DEFINITIONS
**
*/
#define BUFLEN 64

void GWM_Error( int status )
{
    char msgbuf[BUFLEN];

    if (status!=GWM_SUCCESS)
    {
	    GWM_ErrorMessage(status, msgbuf, BUFLEN);
	    fprintf(stderr, "GWM %d %% %s\n",status,msgbuf);
    }
    return;
}
