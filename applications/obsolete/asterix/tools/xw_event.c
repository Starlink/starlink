/*
**
**  INCLUDE FILES
**
*/
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/cursorfont.h>

/*
*+
*  Name :
*     Event
*
*  Purpose :
*     Waits for an input event from the X server.
*
*  Language :
*     C
*
*  Invocation :
*    event
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
*     12-JUN-1995 (DLT):
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
    Display *display;                   /* display id              */
    Window win;                         /* window id               */
    int status;                         /* status                  */
    XSetWindowAttributes winatt;        /* window attributes set structure */
    XEvent event;                       /* X event structure */

/*
**  Open the default X display.
*/
    display = XOpenDisplay( NULL );

/*
**  Create a transparent window that covers the whole of the display.
*/
    winatt.event_mask = ButtonPressMask | KeyPressMask | StructureNotifyMask;
    winatt.override_redirect = True;
    winatt.cursor = XCreateFontCursor( display, XC_mouse);

    win =  XCreateWindow( display, DefaultRootWindow(display), 0, 0, 
        WidthOfScreen(DefaultScreenOfDisplay(display)),
        HeightOfScreen(DefaultScreenOfDisplay(display)), 0, 0, InputOnly,
        CopyFromParent, CWEventMask | CWOverrideRedirect | CWCursor, &winatt);

/*
**  Map the window and get the input focus (grabbing the server first to 
**  avoid a race condition (I hope!).
*/
    XGrabServer( display);
    XMapWindow( display, win);
    XWindowEvent( display, win, StructureNotifyMask, &event);
    XSetInputFocus( display, win, RevertToPointerRoot, CurrentTime);
    XUngrabServer( display);

/*
**  Wait for a keyboard or button press event.
*/
     XWindowEvent( display, win,  ButtonPressMask | KeyPressMask, &event);

/*
**  Close the display.
*/
    XCloseDisplay( display);
}
