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
*     colour
*
*  Purpose :
*     Demonstrate how to change a colour in a GWM window.
*
*  Language :
*     C
*
*  Invocation :
*     colour <window name> <entry> <colour>
*
*  Description :
*     The colour of the colour table entry is changed to the specified colour.
*
*  Arguments :
*
*  Authors :
*     DLT: David Terrett (Starlink RAL)
*     {enter_new_authors_here}
*
*  History :
*      9-MAR-1992 (DLT):
*        Orignal version
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
    Pixmap pix;                                   /* pixmap id               */
    int status;                                   /* status                  */
    unsigned long *table, size;                   /* colour table array      */
    unsigned long entry;                          /* colour table entry      */
    XColor color;                                 /* colour specification    */
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
**  Get the associated colour table array
*/
    status = GWM_GetColTable( display, win, &table, &size);
    if (status) GWM_Error(status);

/*
**  Check that the colour table entry is writable
*/
    if ( DefaultVisual( display, DefaultScreen( display ) )->class 
	== StaticColor ||
         DefaultVisual( display, DefaultScreen( display ) )->class 
	== StaticGray ||
         DefaultVisual( display, DefaultScreen( display ) )->class 
        == TrueColor )
    {
	fprintf( stderr, "Sorry - display has a fixed colour table\n");
        return;
    }

/*
**  Decode the colour table entry to change and check that it is in range
*/
    sscanf( argv[2], "%d", &entry);
    if ( entry > size ) 
    {
	fprintf( stderr, "Sorry - the colour table has only %u entries\n",
	    size);
	return;
    }

/*
**  Convert the colour specification
*/
    status = XParseColor( display, DefaultColormapOfScreen( 
	DefaultScreenOfDisplay( display ) ), argv[3], &color);
    if (!status)
    {
	fprintf( stderr, 
	    "Sorry - can't understand colour specification \"%s\"\n", argv[3]);
	return;
    }

/*
**  Set the colour of the selected colour table entry
*/
    color.pixel = table[ entry];
    XStoreColor( display, DefaultColormapOfScreen( DefaultScreenOfDisplay( 
	display ) ), &color);

/*
**  Close the display
*/
    XCloseDisplay( display);
}
