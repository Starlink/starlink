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
*  Name :
*     GWM_SetColTable
*
*  Purpose :
*     Set colour table
*
*  Language :
*     C
*
*  Invocation :
*     status = GWM_SetColTable( display, win_id, table, size);
*
*  Description :
*     The GWM_colour_table window property is replaced with the new
*     array of values.
*
*  Arguments :
*     display = *Display (given)
*        Display id
*     win_id = Window (given)
*        Window id
*     table = *unsigned long (given)
*        Pointer to new colour table array
*     size = unsigned long (given)
*        Number of entries in colour table
*
*  Authors :
*     DLT: David Terrett (Starlink RAL)
*     {enter_new_authors_here}
*
*  History :
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
