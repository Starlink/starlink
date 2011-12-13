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
*     ovcolour
*
*  Purpose:
*     Demonstrate how to change the colour of GWM window's overlay plane
*
*  Language:
*     C
*
*  Invocation:
*     ovcolour <window name> <colour>
*
*  Description:
*     The colour of the overlay plane is changed to the specified colour.
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
    unsigned long *table, size;                   /* colour table array      */
    XColor color;                                 /* colour specification    */
    unsigned long mask;                           /* overlay plane mask      */
    int i;                                        /* loop index              */
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
**  Check that the colour table entry is writable
*/
    if ( DefaultVisual( display, DefaultScreen( display ) )->class
	== StaticColor ||
         DefaultVisual( display, DefaultScreen( display ) )->class
	== StaticGray ||
         DefaultVisual( display, DefaultScreen( display ) )->class
        == TrueColor )
    {
	fprintf( stderr, "Sorry - display has a fixed colour table\n");
        return;
    }

/*
**  Convert the colour specification
*/
    status = XParseColor( display, DefaultColormapOfScreen(
	DefaultScreenOfDisplay( display ) ), argv[2], &color);
    if (!status)
    {
	fprintf( stderr,
	    "Sorry - can't understand colour specification \"%s\"\n", argv[2]);
	return;
    }

/*
**  Get the overlay plane mask
*/
    status = GWM_GetOvMask( display, win, &mask);
    if (status) GWM_Error(status);

/*
**  Set the colour of every entry in the colourtable with the overlay plane
**  bit set.
*/
    for ( i = 0; i < size; i++ )
    {
    	color.pixel = table[i] | ~mask;
    	XStoreColor( display, DefaultColormapOfScreen( DefaultScreenOfDisplay(
	    display ) ), &color);
    }

/*
**  Close the display
*/
    XCloseDisplay( display);
}
