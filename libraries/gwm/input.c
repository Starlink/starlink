/*
**
**  INCLUDE FILES
**
*/
#include <stdio.h>
#include <X11/Xlib.h>
#include "gwm.h"

/*
*+
*  Name:
*     Input
*
*  Purpose:
*     Demonstrate how to get input from a GWM window
*
*  Language:
*     C
*
*  Invocation:
*     intest <window name>
*
*  Description:
*     The pointer position is printed whenever a key or a mouse button is
*     pressed with the pointer in the GWM window. If mouse button 3 was
*     pressed the program then exits.
*
*  Arguments:
*
*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     DLT: David Terrett (Starlink RAL)
*     {enter_new_authors_here}
*
*  History:
*      25-SEP-1992 (DLT):
*        Original version
*     {enter_changes_here}
*
*  Bugs:
*     The error handling is somewhat brutal!
*     {note_any_bugs_here}
*-
*/
int main(int argc, char *argv[])
{
    Display *display;                           /* display id           */
    int status;                                 /* status               */
    XEvent event;				/* X event structure	*/
    Window win, root, inwin, child;		/* Window id's		*/
    Pixmap pix;					/* Pixmap id		*/
    int x, y, xroot, yroot;			/* window coordinates	*/
    unsigned int width, height, border, depth;  /* window geometry	*/
    unsigned int mask;				/* key state mask	*/
    XSetWindowAttributes winatt;	/* window attribute structure	*/
    int xscroll, yscroll;			/* scroll offsets	*/

/*
**  Open the default X display
*/
    display = XOpenDisplay( NULL );
    XSynchronize( display, True);

/*
**  Get the id of the specified window
*/
    status = GWM_FindWindow( display, argv[1], &win);
    if (status) GWM_Error(status);

/*
**  Get the id of the assocated pixmap
*/
    status = GWM_GetPixmap( display, win, &pix);
    if (status) GWM_Error(status);

/*
 *  Get the scroll offsets
 */
    GWM_GetScroll( display, win, &xscroll, &yscroll);

/*
**  Create and map an input only window on top of the drawing area window
*/
    XGetGeometry( display, pix, &root, &x, &y, &width, &height, &border,
	&depth);
    winatt.event_mask = ButtonPressMask | KeyPressMask | StructureNotifyMask;
    inwin = XCreateWindow( display, win, xscroll, yscroll, width, height, 0, 0,
	InputOnly, CopyFromParent, CWEventMask, &winatt );
    XMapWindow( display, inwin);
    XWindowEvent( display, inwin, StructureNotifyMask, &event );

/*
**  wait for input events
*/
    for (;;)
    {
	XWindowEvent( display, inwin,  ButtonPressMask | KeyPressMask, &event);

/*
 *  	Read and print the cursor position
 */
    	XQueryPointer( display, win, &root, &child, &xroot, &yroot, &x, &y,
	    &mask);
        printf( "%d %d\n", x - xscroll, y -yscroll);

/*
** 	Exit if the key pressed was mouse button 3.
*/
        if ( event.type == ButtonPress && event.xbutton.button == Button3 )
	    break;
    }

/*
**  Close the display
*/
    XCloseDisplay( display);
}
