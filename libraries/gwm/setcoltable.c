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

int GWM_SetColTable( Display *display, Window win_id, unsigned long *table,
	unsigned long size)
/*
*+
*  Name:
*     GWM_SetColTable
*
*  Purpose:
*     Set colour table
*
*  Language:
*     C
*
*  Invocation:
*     status = GWM_SetColTable( display, win_id, table, size);
*
*  Description:
*     The GWM_colour_table window property is replaced with the new
*     array of values.
*
*  Arguments:
*     display = *Display (given)
*        Display id
*     win_id = Window (given)
*        Window id
*     table = *unsigned long (given)
*        Pointer to new colour table array
*     size = unsigned long (given)
*        Number of entries in colour table
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

    atom = XInternAtom(display, "GWM_colour_table", False );

    XChangeProperty( display, win_id, atom, XA_INTEGER, 32, PropModeReplace,
        (unsigned char*)(table), size );

    return GWM_SUCCESS;
}
