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
*  Name :
*     Ovclear
*
*  Purpose :
*     Demonstrate how to write to a GWM window's overlay plane.
*
*  Language :
*     C
*
*  Invocation :
*     ovclear <window name>
*
*  Description :
*     The overlay plane is cleared; this illustrates how to draw in a GWM 
*     window's overlay plane.
*
*  Arguments :
*
*  Authors :
*     DLT: David Terrett (Starlink RAL)
*     {enter_new_authors_here}
*
*  History :
*      9-MAR-1992 (DLT):
*        Orignal version
*     {enter_changes_here}
*
*  Bugs:
*     The error handling is somewhat brutal!
*     {note_any_bugs_here}
*-
*/
int main(int argc, char *argv[])
{
    Display *display;                             /* display id              */
    Window win;                                   /* window id               */
    Pixmap pix;                                   /* pixmap id               */
    int status;                                   /* status                  */
    unsigned long mask;                            /* overlay plane mask      */
    int xoff, yoff;                               /* window offsets          */
    GC gc;                                        /* graphics context        */
    XGCValues gcval;                              /* graphics context values */
    Window root;                                  /* parent window id        */
    int x, y;                                     /* pixmap dimensions       */
    unsigned int width, height, border, depth;    /*    "        "           */

/*
**  Open the default X display
*/
    display = XOpenDisplay( NULL );

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
**  Get the overlay mask
*/
    status = GWM_GetOvMask( display, win, &mask);
    if (status) GWM_Error(status);

/*
**  Get the overlay scroll values
*/
    status = GWM_GetOvScroll( display, win, &xoff, &yoff);

/*
**  Get the dimensions of the pixmap
*/
    XGetGeometry( display, pix, &root, &x, &y, &width, &height, &border,
	&depth);

/*
**  Create a graphics context with the foreground set to zero and the plane
**  mask set to protect all planes except the overlay plane.
*/
    gcval.foreground = 0;
    gcval.plane_mask = ~mask;
    gc = XCreateGC( display, win, GCForeground | GCPlaneMask, &gcval);

/*
**  Erase the contents of the pixmap
*/
    XFillRectangle( display, pix, gc, 0, 0, width, height);

/*
**  Erase the area of the window occupied by the pixmap
*/
    XFillRectangle( display, win, gc, xoff, yoff, width, height);

/*
**  Close the display
*/
    XCloseDisplay( display);
}
