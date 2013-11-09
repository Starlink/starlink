/*
**
**  INCLUDE FILES
**
*/
#include <stdio.h>
#include <X11/Xlib.h>
#include "gwm.h"

/*
*+
*  Name :
*     Raise
*
*  Purpose :
*     Raise a GWM window
*
*  Language :
*     C
*
*  Invocation :
*    Raise <window name>
*
*  Description :
*    Maps an unmapped GWM window and raises to the top of the stack.
*    
*
*  Arguments :
*
*  Authors :
*     DLT: David Terrett (Starlink RAL)
*     PDRAPER: Peter Draper (Starlink - Durham University)
*     {enter_new_authors_here}
*
*  History :
*     19-MAR-1993 (DLT):
*        Original version
*     16-MAR-1993 (PDRAPER):
*        Modification of hide.
*     {enter_changes_here}
*
*  Bugs:
*     The error handling is somewhat brutal!
*     {note_any_bugs_here}
*-
*/
int main(int argc, char *argv[])
{
    Display *display;                             /* display id              */
    Window win;                                   /* window id               */
    int status;                                   /* status                  */

/*
**  Open the default X display
*/
    display = XOpenDisplay( NULL );

/*
**  Get the id of the specified window
*/
    status = GWM_FindWindow( display, argv[1], &win);
    if (status) GWM_Error(status);

/*
**  Map and raise the window
*/
    XMapRaised( display, win);

/*
**  Close the display
*/
    XCloseDisplay( display);
}
