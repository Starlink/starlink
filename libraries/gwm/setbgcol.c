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

int GWM_SetBgCol( Display *display, Window win_id, char *bg)
/*
*+
*  Name :
*     GWM_SetBgCol
*
*  Purpose :
*     Set background colour property
*
*  Language :
*     C
*
*  Invocation :
*     status = GWM_SetBgCol(display, win_id, bg);
*
*  Description :
*     The value of the GWM_background property is set on the window.
*
*  Arguments :
*     display = *Display (given)
*        Display id
*     win_id = Window (given)
*        Window id
*     bg = char (given)
*        Background colour specification
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
    Atom atom;
    XColor color;
        
/*
**  Check that the colour specification is understood by the server
*/
    status = XParseColor( display, DefaultColormapOfScreen(
        DefaultScreenOfDisplay( display ) ), bg, &color);
    if (!status) return GWM_BAD_COLOUR;
 
/*	  
**  Background Colour
*/	  
    atom = XInternAtom(display, "GWM_background", False );
    if (!atom) return GWM_NO_BACKGROUND;

    XChangeProperty( display, win_id, atom, XA_STRING, 8, PropModeReplace,
        (unsigned char*)bg, strlen(bg) );
 
    return GWM_SUCCESS;
}
