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

int GWM_GetColTable( Display *display, Window win_id,
	unsigned long **table, unsigned long *size)
/*
*+
*  Name:
*     GWM_GetColTable
*
*  Purpose:
*     Get window's colour table
*
*  Language:
*     C
*
*  Invocation:
*     status = GWM_GetColTable( display, win_id, &table, &size);
*
*  Description:
*     The GWM_colour_table property is fetched from the window and
*     pointer to it and the number of values it contains returned.
*
*  Arguments:
*     display = *Display (given)
*        Display id
*     win_id = Window (given)
*        Window id
*     table = *unsigned long (returned)
*        Pointer to Colour table
*     size = unsigned long (returned)
*        Number of colour table entries
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
    unsigned int i;
    Atom atom, actual_type;
    int actual_format;
    unsigned long bytes_after;
    unsigned long *local_table;

    atom = XInternAtom(display, "GWM_colour_table", False );
    if (!atom) return GWM_NO_COLTAB;
    status = XGetWindowProperty( display, win_id , atom, 0, 1024, False,
	XA_INTEGER, &actual_type, &actual_format, size, &bytes_after,
	(unsigned char**)(&local_table));
    if ( status != Success || *size == 0) return GWM_NO_COLTAB;

    *table = (unsigned long*)malloc( sizeof(unsigned long) * (*size));
    if (!table) return( GWM_MEM_ALLOC);

    for ( i = 0; i < *size; i++ ) (*table)[i] = local_table[i];

    XFree( (char*)local_table );
    return GWM_SUCCESS;
}
