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

int GWM_SetPixmap( Display *display, Window win_id, Pixmap pix_id)
/*
*+
*  Name:
*     GWM_SetPixmap
*
*  Purpose:
*     Set pixmap id
*
*  Language:
*     C
*
*  Invocation:
*     status = GWM_SetPixmap( display, win_id, pix_id);
*
*  Description:
*     The value of the GWM_pixmap window property is replaced with the
*     new value.
*
*  Arguments:
*     display = *Display (given)
*        Display id
*     win_id = Window (given)
*        Window id
*     pix_id = Pixmap (given)
*        New pixmap id
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
*      10-JUL-1991 (DLT):
*        Orignal version
*     {enter_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/
{
    Atom atom;

/*
**  Pixmap id.
*/
    atom = XInternAtom(display, "GWM_pixmap", False );

    XChangeProperty( display, win_id, atom, XA_PIXMAP, 32, PropModeReplace,
        (unsigned char*)&pix_id, 1 );

    return GWM_SUCCESS;
}
