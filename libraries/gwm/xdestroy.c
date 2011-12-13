/*
**
**  INCLUDE FILES
**
*/

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include "gwm.h"
#include "gwm_sys.h"

int main( int argc, char *argv[])
/*
*+
*  Name:
*     xdestroy
*
*  Purpose:
*     A program that destroys a given window
*
*  Language:
*     C
*
*  Invocation:
*     xdestroy <window_name>
*
*  Description:
*     GWM_DestroyWindow is called to destroy the window.
*
*  Arguments:
*     Window name
*     -display <display_name>
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
    Widget top;
    Display *display;
    int i, status;
/*
**  Use XtInitialize to parse the command arguments using the standard Xt
**  options. The one we are interested in is -display.
*/
    top = XtInitialize("main", "Xrefresh", NULL, 0, &argc, argv);

/*
**  Open the display
*/
    display = XtDisplay( top );

/*
**  Search the argument list for an argument that doesn't begin with "-"; this
**  must be the window name.
*/
    for ( i = 1; argv[i]; i++ ) if (*argv[i] != '-') break;
    if ( !argv[i] )
    {
	fprintf( stderr, "%%%% No window name supplied\n");
	goto error;
    }

/*
**  Destroy the window
*/
    status = GWM_DestroyWindow( display, argv[i]);
    if ( status ) GWM_Error( status );

    XCloseDisplay( display);

#if defined(VMS)
error:
    return 1;
#else
    return 0;
error:
    return 1;
#endif
}
