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
*     Background
*
*  Purpose:
*     Demonstrate how to change the background of a GWM window.
*
*  Language:
*     C
*
*  Invocation:
*     background <window name> <colour>
*
*  Description:
*     The background window property is updated and the colour table
*     updated either buy changing the colour representation of entry
*     0 (if the colour table is writable) or changing the value of
*     entry 0 to point to the closest available colour.
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
    XColor bg_color;				  /* X colour structure */

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
    status = GWM_SetBgCol( display, win, argv[2]);
    if (status) GWM_Error(status);

/*
**  Convert the colour specification to an XColor structure (the
**  specification must be valid because SetBgCol succeeded).
*/
    XParseColor( display, DefaultColormap( display, DefaultScreen( display )),
	argv[2], &bg_color);

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
            DefaultScreen( display )), &bg_color);
        table[0] = bg_color.pixel;

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
	bg_color.pixel = table[0];
	XStoreColor( display, DefaultColormap( display,
            DefaultScreen( display )), &bg_color);
     }

/*
**  Close the display
*/
    XCloseDisplay( display);
}
