/* The returnbuf routine sends a message back to the client program.  It */
/* allows only one message to be sent to the client at a time - the client */
/* must ensure that it gets a value before asking for the next one. */

/* Sam Southard, Jr. */
/* Created: 19-Nov-1990 */
/* Modification History: */
/* 15-Aug-1991	SNS/CIT	No longer includes hooks for Xvideo */
/*  8-Oct-1991	SNS/CIT	Globals now in globals.h */

#include "figdisp.h"
#include "globals.h"

/* The X Window include files */
#include <X11/Xlib.h>
#include <X11/Xatom.h>

void returnbuf(msg,len,destwin)
short *msg;	/* the message to send to the client. */
int len;	/* The length of the message. */
Window destwin;	/* The window who's atom should be changed. */
{
	/* If the window is still around, then send the reply */
	if (selset) XChangeProperty(display,destwin,selatom,XA_STRING,8,
		PropModeReplace,(unsigned char *)msg,len*2);
	return;
}
