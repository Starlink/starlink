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

int GWM_SetOvScroll( Display *display, Window win_id, int xoffset, int yoffset)
{
/*
*+
*  Name :
*     GWM_SetOvScroll
*
*  Purpose :
*     Set overlay scroll offset
*
*  Language :
*     C
*
*  Invocation :
*     status = GWM_SetOvScroll( display, win_id, xoffset, yoffset);
*
*  Description :
*     The values of the GWM_x_ov_offset and GWM_y_ov_offset window
*     properties are replaced by the new values.
*
*  Arguments :
*     display = *Display (given)
*        Display id
*     win_id = Window (given)
*        Window id
*     xoffset = int (given)
*        New overlay scroll value in x
*     yoffset = int (given)
*        New overlay scroll value in y
*
*  Authors :
*     DLT: David Terrett (Starlink RAL)
*     {enter_new_authors_here}
*
*  History :
*      19-NOV-1991 (DLT):
*        Orignal version
*     {enter_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/
    Atom atom;
    long local_offset;
        
    atom = XInternAtom(display, "GWM_x_ov_offset", False );
    if (!atom) return GWM_NO_OVOFFSET;

    local_offset = (long)xoffset;
    XChangeProperty( display, win_id, atom, XA_INTEGER, 32, PropModeReplace,
        (unsigned char*)(&local_offset), 1 );

    atom = XInternAtom(display, "GWM_y_ov_offset", False );
    if (!atom) return GWM_NO_OVOFFSET;
    
    local_offset = (long)yoffset;
    XChangeProperty( display, win_id, atom, XA_INTEGER, 32, PropModeReplace,
        (unsigned char*)(&local_offset), 1 );

    return GWM_SUCCESS;
}
