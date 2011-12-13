/*
**
**  INCLUDE FILES
**
*/

#include <X11/Xlib.h>
#include "gwm_err.h"
#include "gwm.h"

int GWM_SetColour( Display *display, Window win_id, unsigned long entry,
	unsigned long r, unsigned long g, unsigned long b)
/*
*+
*  Name:
*     GWM_SetColour
*
*  Purpose:
*     Set colour
*
*  Language:
*     C
*
*  Invocation:
*     status = GWM_SetColour( display, win_id, entry, r, g, b);
*
*  Description:
*    The specified colour table entry is set to the requested colour.
*
*  Arguments:
*     display = *Display (Given)
*        Display id
*     win_id = Window (Given)
*        Window id
*     entry = unsigned long (given)
*        Colour table entry to be changed
*     r = unsigned long (given)
*        New red value
*     g = unsigned long (given)
*        New green value
*     b = unsigned long (given)
*        New blue value
*
*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*      9-JUL-1991 (DLT):
*        Orignal version
*     21-APR-1995 (DLT):
*        Set entry directly instead of via the gwm server.
*     {enter_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*        This routine is obsolete and inefficient (because it has to inquire
*        the window attribute to get the colormap for every call) and
*        should not be used.
*-
*/
{
    XColor color;
    XWindowAttributes winatt;

/*
**  Find the colourmap for this window.
*/
    XGetWindowAttributes( display, win_id, &winatt);

/*
**  Check that the colour table is writable
*/
    if ( winatt.visual->class == PseudoColor ||
	winatt.visual->class == DirectColor ||
	winatt.visual->class == GrayScale)
    {
	color.pixel = entry;
	color.red = r;
	color.green = g;
	color.blue = b;
	color.flags = DoRed | DoGreen | DoBlue;
	XStoreColor( display, winatt.colormap, &color);
    }
    return GWM_SUCCESS;
}
