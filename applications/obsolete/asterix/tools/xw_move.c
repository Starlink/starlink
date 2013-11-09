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
*     Move
*
*  Purpose :
*     Moves a GWM window to a new position on the screen
*
*  Language :
*     C
*
*  Invocation :
*    move <window name> <x_position> <y_position>
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
    int x, y;					  /* window position	     */

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
**  Decode the position arguments
*/
    sscanf( argv[2], "%d", &x);
    sscanf( argv[3], "%d", &y);

/*
**  Move the window
*/
    XMoveWindow( display, win, x, y);

/*
**  Close the display
*/
    XCloseDisplay( display);
}
