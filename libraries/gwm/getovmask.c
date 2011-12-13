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

int GWM_GetOvMask( Display *display, Window win_id, unsigned long *mask)
/*
*+
*  Name:
*     GWM_GetOvMask
*
*  Purpose:
*     Get overlay mask
*
*  Language:
*     C
*
*  Invocation:
*     status = GWM_GetOvMask( display, win_id, &mask);
*
*  Description:
*     The GMW_ov_mask property is fetched from the window.
*
*  Arguments:
*     display = *Display (given)
*        Display id
*     win_id = Window (given)
*        Window id
*     mask = unsigned long
*        Overlay plane mask
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
*      21-AUG-1991 (DLT):
*        Orignal version
*      17-DEC-1991 (DLT):
*        Change data type of mask to match type in GC structure
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
    unsigned long *local_mask;

/*
**  Get the value of the GWM_ov_mask property from the window.
*/
    atom = XInternAtom(display, "GWM_ov_mask", False );
    if (!atom) return GWM_NO_OVMASK;

    status = XGetWindowProperty( display, win_id , atom, 0, 1, False,
	XA_INTEGER, &actual_type, &actual_format, &nitems, &bytes_after,
	(unsigned char**)(&local_mask));
    if ( status != Success || nitems == 0) return GWM_NO_OVMASK;

/*
**  Copy the mask to the output argument
*/
    *mask = *local_mask;

/*
**  Free the storage allocated by XGetWindowPropery
*/
    XFree( (char*)local_mask );

    return GWM_SUCCESS;
}
