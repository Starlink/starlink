/*
*+
*  Name:
*     GWM_ErrorMessage

*  Purpose:
*     Converts a GWM error code into a message.

*  Language:
*     C

*  Invocation:
*     GWM_ErrorMessage( status, msgbuf, msgbuflen );

*  Description:
*     Converts a GWM error code into a message

*  Arguments:
*     status = int (Given)
*        Error number
*     msgbuf = char * (Given & Returned)
*        Buffer to receive the message.
*     msgbuflen = int (Given)
*        The legnth of the buffer.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     19-APR-1994 (DLT):
*         Original version.

*-
*/

/*
**
**  INCLUDE FILES
**
*/

#include <stdio.h>
#include <string.h>
#include "gwm_err.h"
/*
**
**  MACRO DEFINITIONS
**
*/
#define MAXMSGLEN 64

void GWM_ErrorMessage( int status, char *msgbuf, int msgbuflen )
{
    char locbuf[MAXMSGLEN];

    switch( status )
    {
	case GWM_WINDOW_EXISTS:
	    sprintf(locbuf, "Window already exists");
	    break;

	case GWM_MEM_ALLOC:
	    sprintf(locbuf, "Unable to allocate memory");
	    break;

	case GWM_COL_ALLOC:
	    sprintf(locbuf,"Unable to allocate colour cells");
	    break;

	case GWM_WIN_CREATE:
	    sprintf(locbuf,"Failed to create window");
	    break;

	case GWM_PIX_CREATE:
	    sprintf(locbuf,"Failed to create pixmap");
	    break;

	case GWM_WIN_NAME:
	    sprintf(locbuf,"Invalid window name");
	    break;

	case GWM_WIN_NOEXIST:
	    sprintf(locbuf,"Window does not exist");
	    break;

	case GWM_NOT_GWMWIN:
	    sprintf(locbuf,"Not a GWM window");
	    break;

	case GWM_WRONG_NAME:
	    sprintf(locbuf,"Window has the wrong name");
	    break;

	case GWM_NO_PIXMAP:
	    sprintf(locbuf,"Window has no associated pixmap");
	    break;

	case GWM_NO_OFFSET:
	    sprintf(locbuf,"Window has no offsets");
	    break;

	case GWM_INV_WINID:
	    sprintf(locbuf,"Invalid window id");
	    break;

	case GWM_INV_PIXID:
	    sprintf(locbuf,"Invalid pixmap id");
	    break;

	case GWM_NO_WIN_NAME:
	    sprintf(locbuf,"No window name");
	    break;

	case GWM_CHILD_DEAD:
	    sprintf(locbuf,"Child process died");
	    break;

	case GWM_SS_ERR:
	    sprintf(locbuf,"System service error");
	    break;

	case GWM_BADOPT:
	    sprintf(locbuf,"Unrecognised command option");
	    break;

	case GWM_DUP_NAME:
	    sprintf(locbuf, "Duplicate window name specified");
	    break;
	case GWM_NO_COLTAB:
	    sprintf(locbuf,"Window has no colour table");
	    break;

	case GWM_NO_DISPLAY:
	    sprintf(locbuf,"Unable to open display");
	    break;

	case GWM_BAD_COLOUR:
	    sprintf(locbuf,"Bad colour specification");
	    break;

	case GWM_OVNOTSUP:
	    sprintf(locbuf,
		"Overlays not supported on this display");
	    break;

	case GWM_INVOV:
	    sprintf(locbuf, "Invalid number of overlays specified");
	    break;

	case GWM_NO_OVMASK:
	    sprintf(locbuf, "Window has no overlay mask");
	    break;

	case GWM_NO_FOREGROUND:
	    sprintf(locbuf, "Window has no foreground property");
	    break;

	case GWM_NO_BACKGROUND:
	    sprintf(locbuf, "Window has no background property");
	    break;

	default:
	    sprintf(locbuf,"Unknown GWM error %d",status);
	    break;
    }
    /*
    **  Copy the message to the output buffer
    */
    strncpy( msgbuf, locbuf, msgbuflen-1);
    msgbuf[msgbuflen-1] = '\0';

    return;
}
