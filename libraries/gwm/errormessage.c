/*
**++
**  FACILITY:  GWM
**
**  MODULE DESCRIPTION:
**
**      GWM_ErrorMessage
**
**  AUTHORS:
**
**      D L Terrett
**
**  CREATION DATE:  19_APR-1994
**--
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


/*
**++
**  FUNCTIONAL DESCRIPTION:
**
**	Converts a GWM error code into a message
**
**  FORMAL PARAMETERS:
**
**      status:
**          Error number
**      msgbuf:
**	    buffer to receive the message
**      msgbuflen:
**          the length of the buffer
**
**  RETURN VALUE:
**
**      None
**
**  DESIGN:
**
**
**--
*/
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
