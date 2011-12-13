/*
**
**  INCLUDE FILES
**
*/
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include "gwm_err.h"
#include "gwm.h"

int GWM_DestroyWindow( Display *display, char name[])
/*
*+
*  Name:
*     GWM_DestroyWindow
*
*  Purpose:
*     Destroy a window.
*
*  Language:
*     C
*
*  Invocation:
*     status = GWM_DestroyWindow( display, name);
*
*  Description:
*     The X display is searched for the named window and if found, the
*     window name property is removed from the root window and the window
*     and its associated pixmap destroyed.
*
*  Arguments:
*     display = *Display (given)
*        Display id
*     name = char[] (given)
*        Window name
*
*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*      26-MAR-1991 (DLT):
*        Orignal version
*     {enter_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/
{
    int status;
    Window *win_id;
    char *prop_name, *real_name;
    Atom atom, actual_type;
    int actual_format;
    unsigned long nitems, bytes_after;


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
	0, 1, True, XA_WINDOW, &actual_type, &actual_format, &nitems,
	&bytes_after, (unsigned char**)(&win_id));
    if ( status ) return GWM_WIN_NOEXIST;
    if ( nitems == 0 ) return GWM_WIN_NOEXIST;

/*
**  Check that the window is an GWM window
*/
    atom = XInternAtom(display, "GWM_name", False );
    status = XGetWindowProperty( display, *win_id , atom, 0, 32, False,
	XA_STRING, &actual_type, &actual_format, &nitems, &bytes_after,
	(unsigned char**)(&real_name));
    if ( status ) return GWM_NOT_GWMWIN;
    status = strcmp( real_name, (char*)name );
    XFree( real_name );
    if ( status )
    {
        XFree(win_id);
	return GWM_WRONG_NAME;
    }

 /*
 ** Delete the window
 */
    XDestroyWindow( display, *win_id);
    XFlush( display );

 /*
 ** Free the property buffers.
 */
    XFree(win_id);

    return GWM_SUCCESS;
}
