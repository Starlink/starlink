/*
**
**  INCLUDE FILES
**
*/

#include <stdio.h>
#include <X11/Xlib.h>
#include "gwm.h"
#include "gwm_sys.h"

int main(int argc, char *argv[])
/*
*+
*  Name :
*     xmake
*
*  Purpose :
*     Creates a GWM X window.
*     
*  Language :
*     C
*
*  Invocation :
*     xmake <window_name>
*
*  Description :
*     A refresh process is created and the argument list passed to it.:w
*     
*  Arguments :
*     <window name>
*     -display <display name>
*     -colours <number of colours>
*     -geometry <window geometry>
*     -foreground <foreground colour>
*     -fg <foreground colour>
*     -background <background colour>
*     -bg <background colour>
*     -title <window title>
*     -ovcolour <overlay colour>
*     -boderwidth <border width>
*     -overlay
*     -interactive
*
*  Authors :
*     DLT: David Terrett (Starlink RAL)
*     {enter_new_authors_here}
*
*  History :
*      4-JUL-1991 (DLT):
*        Orignal version
*     {enter_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/
{
    Display *display;
    int status; 
    char name[64];

/*
**  Create the window passing the program arguments
*/
    status = GWM_CreateWindow( argc, argv, &display, name);
    if (status) 
    {
	GWM_Error(status);
	goto error;
    }

    XCloseDisplay(display);

#if defined(VMS)
error:
    return 1;
#else
    return 0;
error:
    return 1;
#endif
}
