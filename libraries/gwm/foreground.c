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
*     Foreground
*
*  Purpose :
*     Demonstrate how to change the foreground of a GWM window.
*
*  Language :
*     C
*
*  Invocation :
*     foreground <window name> <colour>
*
*  Description :
*     The foreground window property is updated and the colour table
*     updated either buy changing the colour representation of entry
*     1 (if the colour table is writable) or changing the value of
*     entry 1 to point to the closest available colour.
*
*  Arguments :
*
*  Authors :
*     DLT: David Terrett (Starlink RAL)
*     {enter_new_authors_here}
*
*  History :
*      3-APR-1992 (DLT):
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
    unsigned int width, height, border, depth;    /*    "        "           */
    unsigned long *table, size;                   /* colour table array      */
    int visual_class;				  /* visual class of display */
    XColor fg_color;				  /* X colour structure */

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
**  Get the associated colour table array
*/
    status = GWM_GetColTable( display, win, &table, &size);
    if (status) GWM_Error(status);

/*
**  Set the new foreground window property
*/
    status = GWM_SetFgCol( display, win, argv[2]);
    if (status) GWM_Error(status);

/*
**  Convert the colour specification to an XColor structure (the
**  specification must be valid because SetFgCol succeeded).
*/
    XParseColor( display, DefaultColormap( display, DefaultScreen( display )),
	argv[2], &fg_color);

/*
**  Get the visual class of the display
*/
    visual_class = DefaultVisual( display, DefaultScreen( display ) )-> class;

    if (visual_class == StaticGray || visual_class == StaticColor ||
            visual_class == TrueColor )
    {

/*
**     Colour table is static so find nearest available colour
*/
        XAllocColor( display, DefaultColormap( display,
            DefaultScreen( display )), &fg_color);
        table[1] = fg_color.pixel;

/*
**      Write the revised colour table back to the window
*/
	status = GWM_SetColTable( display, win, table, size);
    }
    else
    {

/*
**      Colour table is dynamic so store the new colour in the server's
**      colourmap.
*/
	fg_color.pixel = table[1];
	XStoreColor( display, DefaultColormap( display,
            DefaultScreen( display )), &fg_color);
     }

/*
**  Close the display
*/
    XCloseDisplay( display);
}
