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
*  Name :
*     GWM_GetFgCol
*
*  Purpose :
*     Get foreground colour
*
*  Language :
*     C
*
*  Invocation :
*     status = GWM_GetFgCol(display, win_id, &fg);
*
*  Description :
*     The value of the GWM_foreground property is fetched from the window.
*
*  Arguments :
*     display = *Display (given)
*        Display id
*     win_id = Window (given)
*        Window id
*     fg = char* (returned)
*        Pointer to foreground colour specification
*
*  Authors :
*     DLT: David Terrett (Starlink RAL)
*     {enter_new_authors_here}
*
*  History :
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
