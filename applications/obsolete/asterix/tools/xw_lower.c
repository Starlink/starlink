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
*     Lower
*
*  Purpose :
*     Lower a GWM window to the back of the screen
*
*  Language :
*     C
*
*  Invocation :
*    lower <window name>
*
*  Description :
*    
*
*  Arguments :
*
*  Authors :
*     DLT: David Terrett (Starlink RAL)
*     {enter_new_authors_here}
*
*  History :
*     19-MAR-1993 (DLT):
*        Original version
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
**  Lower the window
*/
    XLowerWindow( display, win);

/*
**  Close the display
*/
    XCloseDisplay( display);
}
