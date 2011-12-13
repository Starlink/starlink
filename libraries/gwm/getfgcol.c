/*
**
**  INCLUDE FILES
**
*/

#include <string.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include "gwm_err.h"
#include "gwm.h"

int GWM_GetFgCol( Display *display, Window win_id, char **fg)
/*
*+
*  Name:
*     GWM_GetFgCol
*
*  Purpose:
*     Get foreground colour
*
*  Language:
*     C
*
*  Invocation:
*     status = GWM_GetFgCol(display, win_id, &fg);
*
*  Description:
*     The value of the GWM_foreground property is fetched from the window.
*
*  Arguments:
*     display = *Display (given)
*        Display id
*     win_id = Window (given)
*        Window id
*     fg = char* (returned)
*        Pointer to foreground colour specification
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
*     {note_any_bugs_here}
*-
*/
{
    int status;
    Atom atom, actual_type;
    int actual_format;
    char *local_fg;
    unsigned long nitems, bytes_after;

/*
**  Foreground Colour
*/
    atom = XInternAtom(display, "GWM_foreground", False );
    if (!atom) return GWM_NO_FOREGROUND;

    status = XGetWindowProperty( display, win_id , atom, 0, 255, False,
	XA_STRING, &actual_type, &actual_format, &nitems, &bytes_after,
	(unsigned char**)(&local_fg));
    if ( status != Success || nitems == 0) return GWM_NO_FOREGROUND;

    *fg = local_fg;
    return GWM_SUCCESS;
}
