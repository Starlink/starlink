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

int GWM_GetPixmap( Display *display, Window win_id, Pixmap *pix_id)
/*
*+
*  Name:
*     GWM_GetPixmap
*
*  Purpose:
*     Get pixmap id
*
*  Language:
*     C
*
*  Invocation:
*     status = GWM_GetPixmap( display, win_id, &pix_id);
*
*  Description:
*     The value of the GWM_pixmap property is fetched from the window.
*
*  Arguments:
*     display = *Display (given)
*        Display id
*     win_di = Window (given)
*        Window id
*     pix_id = Pixmap (returned)
*        Pixmap id
*
*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
    unsigned long nitems, bytes_after;
    Pixmap *local_pix_id;

/*
**  Get the value of the GWM_Pixmap property from the window.
*/
    atom = XInternAtom(display, "GWM_pixmap", False );
    if (!atom) return GWM_NO_PIXMAP;

    status = XGetWindowProperty( display, win_id , atom, 0, 1, False,
	XA_PIXMAP, &actual_type, &actual_format, &nitems, &bytes_after,
	(unsigned char**)(&local_pix_id));
    if ( status != Success || nitems == 0) return GWM_NO_PIXMAP;

/*
**  Copy the pixmap id to the output argument
*/
    *pix_id = *local_pix_id;

/*
**  Free the storage allocated by XGetWindowPropery
*/
    XFree( (char*)local_pix_id );

    return GWM_SUCCESS;
}
