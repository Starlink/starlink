/*
**
**  INCLUDE FILES
**
*/

#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xresource.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include "gwm.h"
#include "gwm_err.h"
#include "gwm_sys.h"
#if defined(VMS)
#include descrip
#include ssdef
#include starlet
#include iodef
#include libdef
#endif

#if defined(VMS)
#define MAIN VMS_main

/*
 *  On VMS we need a condition handler that allows the process to exit
 *  without trying to write to its output channel
 */
int handler( int *sigargs, int *mechargs)
{
    exit(1);
}
#else
#define MAIN main
#endif
int MAIN (int argc, char *argv[])
/*
*+
*  Name:
*     xrefresh
*
*  Purpose:
*     Refresh an X window in response to expose events.
*
*  Language:
*     C
*
*  Invocation:
*     status = GWM_
*
*  Description:
*     On VMS the normal C command line conventions cannot be used directly
*     because the program is maybe run as a subprocess without a CLI present
*     so the main program on other systems has to be a function that is called
*     by the real main program. This real main program constructs the argument
*     list by reading from a mailbox.
*
*     Either an error message or the name of the display is written to
*     to the standard error channel. This provides the parent process
*     with enough information to connect to the window.
*
*  Arguments:
*     See xmake
*
*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
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
*      13-MAR-1991 (DLT):
*        Orignal version
*      21-AUG-1991 (DLT):
*        Added overlay plane support
*      13_NOV-1991 (DLT):
*        Pass event structure by reference
*      06-APR-1992 (DLT):
*        Add -iconic
*     {enter_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/
{
    Widget top;
    Display *display;
    Window win_id;
    int size_x, size_y;
    int pos_x, pos_y;
    int gravity;
    XSizeHints* hints;
    int status;
    struct wininfo *info;
    XEvent event;
    XGCValues gcvalues;
    XrmOptionDescRec optable[] = {
	{"-colours", "*colours", XrmoptionSepArg, (caddr_t)NULL},
	{"-geometry", "*geometry", XrmoptionSepArg, (caddr_t)NULL},
	{"-foreground", "*foreground", XrmoptionSepArg, (caddr_t)NULL},
	{"-fg", "*foreground", XrmoptionSepArg, (caddr_t)NULL},
	{"-background", "*background", XrmoptionSepArg, (caddr_t)NULL},
	{"-bg", "*background", XrmoptionSepArg, (caddr_t)NULL},
	{"-title", "*title", XrmoptionSepArg, (caddr_t)NULL},
	{"-ovcolour", "*overlayColour", XrmoptionSepArg, (caddr_t)NULL},
	{"-borderwidth", "*borderWidth", XrmoptionSepArg, (caddr_t)NULL},
        {"-overlay", "*overlay", XrmoptionNoArg, "True"},
        {"-nooverlay", "*overlay", XrmoptionNoArg, "False"},
	{"-interactive", "*interactive", XrmoptionNoArg, "True"},
	{"-nointeractive", "*interactive", XrmoptionNoArg, "False"},
	{"-iconic", "*iconic", XrmoptionNoArg, "True"}
    };
    typedef struct {
	char *geom, *fg, *bg, *title, *ovcol;
	int ncols;
	unsigned int border;
	Boolean inter, overlay, iconic;
	} ApData, *ApDataPtr;
    static ApData data;

/*
**  Additional command arguments
*/
    static XtResource resources[] = {
	{"geometry", "Geometry", XtRString, sizeof(String),
		XtOffset(ApDataPtr,geom), XtRString, "780x512"},
	{XtNforeground, XtCForeground, XtRString, sizeof(String),
		XtOffset(ApDataPtr,fg), XtRString, "White"},
	{XtNbackground, XtCBackground, XtRString, sizeof(String),
		XtOffset(ApDataPtr,bg), XtRString, "Black"},
	{"title", "Title", XtRString, sizeof(String),
		XtOffset(ApDataPtr,title), XtRString, NULL},
	{"overlayColour", "OverlayColour", XtRString, sizeof(String),
		XtOffset(ApDataPtr,ovcol), XtRString, ""},
	{"colours", "Colours", XtRInt, sizeof(int),
		XtOffset(ApDataPtr,ncols), XtRImmediate, (caddr_t)0},
	{XtNborderWidth, XtCBorderWidth, XtRInt, sizeof(int),
		XtOffset(ApDataPtr,border), XtRImmediate, (caddr_t)10},
	{"overlay", "Overlay", XtRBoolean, sizeof(Boolean),
		XtOffset(ApDataPtr,overlay), XtRString, "False"},
	{"interactive", "Interactive", XtRBoolean, sizeof(Boolean),
		XtOffset(ApDataPtr,inter), XtRString, "False"},
	{"iconic", "Iconic", XtRBoolean, sizeof(Boolean),
		XtOffset(ApDataPtr,iconic), XtRString, "False"},
	};

/*
**  Use XtInitialize to parse the command arguments using the standard Xt
**  options.
*/
    top = XtInitialize("gwm", "Gwm", optable, XtNumber(optable),
	&argc, argv);

/*
**  Open the display
*/
    display = XtDisplay( top );
    if (!display)
    {
	GWM_Error( GWM_NO_DISPLAY );
	goto error_exit;
    }

/*
**  There should be only one argument left (in addition to the program
**  name in argv[0]) which is the window name
*/
    if ( argc <= 1 )
    {
	GWM_Error( GWM_NO_WIN_NAME );
	goto error_exit;
    }
    else
    {
	if ( argc > 2 )
        {
	    if (*argv[1] == '-' || *argv[2] == '-')
		GWM_Error( GWM_BADOPT );
	    else
		GWM_Error( GWM_DUP_NAME);
	    goto error_exit;
	}
	else
	{
	    if (*argv[1] == '-')
	    {
		GWM_Error( GWM_BADOPT );
	    	goto error_exit;
	    }
	}

    }

/*
**  Fetch the values of the window size parameters from the resource
**  database
*/
    XtGetSubresources(top, &data, argv[1], argv[1], resources,
	XtNumber(resources), NULL, 0);

/*
**  Decode the geometry specification
*/
#if 0
    XGeometry( display, DefaultScreen( display), data.geom, "800x600+0+0",
	data.border, 1, 1, 0, 0, &pos_x, &pos_y, &size_x, &size_y);
#endif
    hints = XAllocSizeHints();
    XWMGeometry( display, DefaultScreen( display), data.geom, "800x600+0+0",
	data.border, hints, &pos_x, &pos_y, &size_x, &size_y, &gravity);
    XFree( hints);

/*
**  Use foreground colour if no overlay colour is specified
*/
    if (data.overlay && (data.ovcol[0] == '\0')) data.ovcol = data.fg;

/*
**  Make the window
*/
    status = GWM_MakeWindow (display, argv[1], data.title, data.ncols,
	data.inter, data.iconic, data.border, data.fg, data.bg, data.overlay,
	data.ovcol, size_x, size_y, pos_x, pos_y, &win_id );

    if ( status != GWM_SUCCESS)
    {
	GWM_Error( status );
	goto error_exit;
    }

/*
**  Set up the structure that GWM_ProcessEvent uses to process the events
*/
    status = GWM_GetWinInfo( display, win_id, &info);

    if ( status != GWM_SUCCESS)
    {
	GWM_Error( status );
	goto error_exit;
    }

/*
**  The window has been successfully created so a message containing the
**  the display name is written to the error channel.
*/
    fprintf(stderr, "GWM %d %s %s\n", GWM_SUCCESS, DisplayString(display),
	argv[1]);
    fflush(stderr);

/*
 *  On VMS we get an access violation instead of a Destroy Notify event
 *  if the X server is reset so we have to esablish a condition handler
 *  that allows the process to exit cleanly
 */
#if defined(VMS)
    (void)lib$establish( handler );
#endif
/*
**  Create a graphics context for copying the pixmap to the window.
*/
   info->gc = XCreateGC( display, win_id, 0, &gcvalues);

/*
**  Select the events to process
*/
    XSelectInput( display, win_id, ExposureMask | PropertyChangeMask |
	    StructureNotifyMask | StructureNotifyMask);

/*
**  Loop waiting for events
*/
    for (;;)
    {
	XNextEvent( display, &event);

	if ( event.type == Expose || event.type == ConfigureNotify ||
	    event.type == DestroyNotify || event.type == ClientMessage
	    || event.type == PropertyNotify)
	{
	    status = GWM_ProcessEvent( info, &event );
    	    if ( status != GWM_SUCCESS)
	    {
		GWM_Error( status );
		XDestroyWindow(display, win_id);
	    }
	}

/*
**  If it was a window destroy event then exit
*/
	if ( event.type == DestroyNotify) break;
    }

#if defined(VMS)
error_exit:
    return 1;
#else
    return 0;
error_exit:
    return 1;
#endif
}

#if defined(VMS)
int main()
{
    char *argv[255], *newline;
    int argc;

/*
**  Read arguments the standard input until EOF is seen
*/
    for ( argc = 0 ; argc < 255; argc++)
    {
	argv[argc] = malloc(255);
        if ( fgets( argv[argc], 255, stdin) == NULL ) break;
        newline = strchr( argv[argc], (int)'\n');
        if (newline) *newline = '\0';
	argv[argc] = realloc(argv[argc], strlen(argv[argc])+1);
    }
    argv[argc] = '\0';
/*
**  Call the "real" main program
*/
    return VMS_main(argc, argv);
}
#endif
