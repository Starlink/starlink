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
*  Name:
*     xmake
*
*  Purpose:
*     Creates a GWM X window.
*
*  Language:
*     C
*
*  Invocation:
*     xmake <window_name>
*
*  Description:
*     A refresh process is created and the argument list passed to it.:w
*
*  Arguments:
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
