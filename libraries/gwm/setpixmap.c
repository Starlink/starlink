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
*  Name :
*     GWM_SetPixmap
*
*  Purpose :
*     Set pixmap id
*
*  Language :
*     C
*
*  Invocation :
*     status = GWM_SetPixmap( display, win_id, pix_id);
*
*  Description :
*     The value of the GWM_pixmap window property is replaced with the
*     new value.
*
*  Arguments :
*     display = *Display (given)
*        Display id
*     win_id = Window (given)
*        Window id
*     pix_id = Pixmap (given)
*        New pixmap id
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
        
/*	  
**  Pixmap id.
*/	  
    atom = XInternAtom(display, "GWM_pixmap", False );

    XChangeProperty( display, win_id, atom, XA_PIXMAP, 32, PropModeReplace,
        (unsigned char*)&pix_id, 1 );

    return GWM_SUCCESS;
}
