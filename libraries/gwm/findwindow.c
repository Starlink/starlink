/*
**
**  INCLUDE FILES
**
*/

#include <string.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include "gwm.h"
#include "gwm_err.h"
#include "gwm_sys.h"

static int dummy_handler(Display* display, XErrorEvent* errorevent)
{
    return 0;
}

int GWM_FindWindow( Display *display, char name[], Window *win_id)
{
/*
*+
*  Name:
*     GWM_FindWindow
*
*  Purpose:
*     Find a window
*
*  Language:
*     C
*
*  Invocation:
*     status = GWM_FindWindow( display, name, &win);
*
*  Description:
*     The X server is seached for a GWM window with the specified name
*     and the id of the window returned.
*
*  Arguments:
*     display = *Display (given)
*        Display id
*     name = char[] (given)
*        Window name
*     win = Window (returned)
*        Window id
*
*  Algorithm:
*     The root window of the display is searched for a property with
*     the name GWM_<name>. The value of this property is taken to be
*     the window id and the window itself is checked for a property
*     called GWM_name whose value matches the window name.
*
*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DLT: David Terrett (Starlink RAL)
*     {enter_new_authors_here}
*
*  History:
*      7-MAR-1991 (DLT):
*        Orignal version
*      8-MAR-1994 (DLT):
*        Fix memory leak
*     {enter_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/
    int status;
    char *prop_name, *real_name;
    Atom atom, actual_type;
    Window *win_ptr = NULL;
    int actual_format;
    unsigned long nitems, bytes_after;
    int (*prev_handler)();

/*
**  Convert the window name to a property atom and get its value from the
**  root window
*/
    prop_name = malloc( strlen((char*)name) + 5 );
    if ( !prop_name ) return GWM_MEM_ALLOC;

    (void)strcpy( prop_name, "GWM_");
    (void)strcat( prop_name, (char*)name );
    atom = XInternAtom( display, prop_name, True );
    free( prop_name );
    if ( !atom ) return GWM_WIN_NOEXIST;
    status = XGetWindowProperty( display, DefaultRootWindow( display ) , atom,
	0, 32, False, XA_WINDOW, &actual_type, &actual_format, &nitems,
	&bytes_after, (unsigned char**)(&win_ptr));
    if ( status || (nitems == 0) )
    {
	if (win_ptr != NULL) XFree( win_ptr );
	return GWM_WIN_NOEXIST;
    }

/*
**  Now check that the window is an GWM window
**
**  If the window no longer exists this will generate an X lib error
**  so an error handler that does nothing has to be installed and then
**  removed after the call to XGetWindowProperty.
*/
    atom = XInternAtom(display, "GWM_name", False );

    prev_handler = XSetErrorHandler( dummy_handler );
    status = XGetWindowProperty( display, *win_ptr , atom,0, 32, False,
	XA_STRING, &actual_type, &actual_format, &nitems, &bytes_after,
	(unsigned char**)(&real_name));
    (void)XSetErrorHandler( prev_handler );
    if ( status || (nitems == 0)) return GWM_NOT_GWMWIN;
    status = strcmp( real_name, (char*)name );
    XFree( real_name );
    if ( status ) return GWM_WRONG_NAME;

/*
**  Return the window id
*/
    *win_id = *win_ptr;
    XFree( win_ptr );
    return GWM_SUCCESS;
}

