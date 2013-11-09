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
*     Slide
*
*  Purpose :
*     Slide a GWM window between 2 positions on the screen
*
*  Language :
*     C
*
*  Invocation :
*    slide <window name> <old_x_position> <old_y_position> <new_y_position>
*          <step_size>
*
*  Description :
*    
*
*  Arguments :
*
*  Authors :
*     DLT: David Terrett (Starlink RAL)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}
*
*  History :
*     19-MAR-1993 (DLT):
*        Original version
*     26-MAR-1993 (RFWS):
*        Changed to perform a slide operation.
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
    int x, y1, y2, step, i;			  /* window position	     */

/*
**  Open the default X display
*/
    display = XOpenDisplay( NULL );
    XSynchronize( display, True);
/*
**  Get the id of the specified window
*/
    status = GWM_FindWindow( display, argv[1], &win);
    if (status) GWM_Error(status);

/*
**  Decode the position arguments
*/
    sscanf( argv[2], "%d", &x);
    sscanf( argv[3], "%d", &y1);
    sscanf( argv[4], "%d", &y2);
    sscanf( argv[5], "%d", &step);

/*
**  Move the window
*/
    if ( y2 >= y1 )
    {
       for( i = y1; i <= y2; i += step )
       {
          if ( i > y2 ) i = y2;
          XMoveWindow( display, win, x, i);
       }
    } else {
       for( i = y1; i >= y2; i -= step )
       {
          if ( i < y2 ) i = y2;
          XMoveWindow( display, win, x, i);
       }
    }

/*
**  Close the display
*/
    XCloseDisplay( display);
}
