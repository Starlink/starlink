/*
**
**  INCLUDE FILES
**
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include "gwm_err.h"
#include "gwm.h"
#if defined(VMS) | defined(ultrix)
#include <X11/DECWmHints.h>
#endif

int GWM_MakeWindow ( Display *display, char name[], char *title, int ncols,
			Boolean inter, Boolean iconic, unsigned int border,
			char *fg, char *bg, Boolean ovl, char *ovcol,
			int size_x, int size_y, int pos_x, int pos_y,
			Window *win_id )
/*
*+
*  Name:
*     GWM_Make_Window
*
*  Purpose:
*     Creates a GWM window and pixmap
*
*  Language:
*     C
*
*  Invocation:
*     status = GWM_MakeWindow( display, name title, ncols, inter,
*                   border, fg, bg, olv, ovcol, size_x, size_y,
*                   pos_x, pos_y, &win_id);
*
*  Description:
*      The following operations are carried out:
*         Colour cells allocated - these are read/write if the server supports
*         it, otherwise they are read only.
*         Window created and mapped.
*         Pixmap created.
*         Icon created and set on the window
*         GWM properties set on the window
*     If any part of the operation fails, all resources allocated are released
*     and the window id set to NULL.
*
*
*  Function return value:
*     status = int
*        The error status
*
*  Arguments:
*     display = *Display (given)
*        Display id
*     name = char[] (given)
*        Window name
*     title = *char (given)
*        Window title
*     ncols = int (given)
*        Number of colours to be allocated to the window
*     inter = Boolean (given)
*        Wait for window to be resized interactively if True
*     iconic = Boolean (given)
*        Initial window state
*     border = unsigned int (given)
*        Border width in pixels
*     fg = *char (given)
*        Foreground colour
*     bg = *char (given)
*        Background colour
*     olv = Boolean (given)
*        Allocate overlay plane if True
*     ovcol = *char (given)
*        Overlay plane colour
*     size_x = int (given)
*        Size of window in x
*     size_y = int (given)
*        Size of window in y
*     pos_x = int (given)
*        Position of window in x
*     pos_y = int (given)
*        Position of window in y
*     win_id = Window (returned)
*        Id of new window
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
*      4-MAR-1991 (DLT):
*        Orignal version
*      13-NOV_1991 (DLT):
*        Change type of inter and ovl to Boolean
*     {enter_changes_here}
*      19-NOV-1991 (DLT):
*        Fix setting of overlay colour
*      03-APR-1992 (DLT):
*        Add back/foreground window properties
*      06-APR-1992 (DLT):
*        Add initial window state
*       1-DEC-1994 (DLT):
*        Correct calls to AllocColorPlanes
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/
{
    int status, lncols ,i;
    unsigned long *pix_array_l, mask = 0, mask_i, mask_return;
    Pixmap pix_id, icon_pixmap;
    Window win_temp;
    int visual_class;
    char* ltitle;
    XEvent event;
    XWindowAttributes win_attrib;
    GC gc;
    XGCValues gcvals;
    Atom atom;
    XColor bg_color, fg_color, ov_color;
    long zero = 0;
    Colormap colormap;

#include "starlink.icon"		/* icon pixmap definition */

    XWMHints wmhints;

#if defined(VMS) | defined(ultrix)
#include "cshead.icon"
    DECWmHintsRec dwmhints;      /* decwindows hints structure */
    Pixmap cs_icon_pixmap;
#endif
    Atom wmatom;                 /* atom identifier */
    Atom dwatom;                 /* atom identifier */


/*
**  Preset window id to zero
*/
    *win_id = (Window)0;

/*
**  Attempt to find a window with this name in case it already exists; if
**  it does then just exit.
*/
    status = GWM_FindWindow( display, name, &win_temp);
    if (status == GWM_SUCCESS) return GWM_WINDOW_EXISTS;

/*
**  Get the visual class of the display
*/
    visual_class = DefaultVisual( display, DefaultScreen( display ) )-> class;

/*
**  Select a suitable number of colours to allocate - if the number
**  requested is < 0 then use that number, otherwise no number has been
**  specified yet and we have to chose a value based on the properties
**  of the X server
*/
    if (ncols > 0)
    {
	lncols = ncols;
    }
    else
    {
/*
**	If the colour table is static then colour table entries are
**	shared by all windows and we can use the maximum available
*/
	if (visual_class == StaticGray || visual_class == StaticColor ||
	    visual_class == TrueColor )
	{
	    lncols = CellsOfScreen( DefaultScreenOfDisplay( display ) );
	}
	else
	{

/*
**	    Otherwise use a quarter of the total
*/
	    lncols = CellsOfScreen( DefaultScreenOfDisplay( display ) ) / 4;

/*
**	    But if that is silly, use half
*/
	    if (lncols < 4) lncols = CellsOfScreen( DefaultScreenOfDisplay(
		display ) ) / 2;
	}
    }

/*
**  Allocate arrays to hold the colour table
*/
    pix_array_l = (unsigned long*)malloc( sizeof(unsigned long) * lncols );
    if (!(pix_array_l)) return GWM_MEM_ALLOC;

  /*  From now on there are resources to be deallocated if an error occurs */

/*
**  Allocate the number of colour cells requested - read/write if the server
**  supports it. If the colour map is read only then we just have to check that
**  it is at least as big as the number of requested colours.
*/

    colormap = DefaultColormap( display, DefaultScreen( display ) );

    if ( visual_class == GrayScale || visual_class == PseudoColor )
    {
	status = XAllocColorCells( display, colormap, False, &mask,
	    ovl ? 1 : 0 ,pix_array_l, lncols);

/*
**	If the allocation failed, create new colourmap
*/
	if (!status)
	{
	    colormap = XCopyColormapAndFree( display, colormap);
	    status = XAllocColorCells( display, colormap, False, &mask,
		ovl ? 1 : 0 ,pix_array_l, lncols);
	}
    }
    else
    {

/*
**	Static colour tables cannot support overlays
*/
	if ( ovl )
	{
	    status = GWM_OVNOTSUP;
	    goto cleanup;
	}
	if ( visual_class == DirectColor )
	{
	    status = XAllocColorPlanes( display, colormap, False, pix_array_l,
		lncols, 0, 0, 0, &mask_return, &mask_return, &mask_return );
/*
**	    If the allocation failed, create new colourmap
*/
	    if (!status)
	    {
		colormap = XCopyColormapAndFree( display, colormap);
	    	status = XAllocColorPlanes( display, colormap, False,
		    pix_array_l, lncols, 0, 0, 0, &mask_return,
		    &mask_return, &mask_return );
	    }
	}
	else
	{
	    if ( CellsOfScreen( DefaultScreenOfDisplay( display ) ) >= lncols )
		status = True;
	    else
		status = False;
	}
    }
    if ( !status )
    {
	status = GWM_COL_ALLOC;
	goto cleanup;
    }

/*
**  Load the colour table with the appropriate values for the foreground
**  and background colours
*/
    status = XParseColor( display, colormap, bg, &bg_color);
    if ( !status )
    {
	status = GWM_BAD_COLOUR;
	goto cleanup;
    }

    status = XParseColor( display, colormap, fg, &fg_color);
    if ( !status )
    {
	status = GWM_BAD_COLOUR;
	goto cleanup;
    }

    if ( visual_class == GrayScale || visual_class == PseudoColor ||
	visual_class == DirectColor)
    {
	bg_color.pixel = pix_array_l[0];
	XStoreColor( display, colormap, &bg_color);
	fg_color.pixel = pix_array_l[1];
	XStoreColor( display, colormap, &fg_color);
    }
    else
    {
	XAllocColor( display, colormap, &bg_color);
	pix_array_l[0] = bg_color.pixel;
	XAllocColor( display, colormap, &fg_color);
	pix_array_l[1] = fg_color.pixel;
    }

/*
**  Load the colour table with the overlay colour
*/
    if (ovl)
    {
    	status = XParseColor( display, colormap, ovcol, &ov_color);
    	if ( !status )
    	{
	    status = GWM_BAD_COLOUR;
	    goto cleanup;
    	}
	for ( i = 0; i < lncols; i++)
	{
	    ov_color.pixel = pix_array_l[i] | mask;
	    XStoreColor( display, colormap, &ov_color);
	}
    }


/*
**  Create the window.
*/
    *win_id = XCreateSimpleWindow( display,
	RootWindow( display, DefaultScreen( display ) ),
	pos_x, pos_y, size_x, size_y, border, pix_array_l[1], pix_array_l[0]);

    if ( !(*win_id) )
    {
	status = GWM_WIN_CREATE;
	goto cleanup;
    }

/*
**  Set the colour map if not the default
*/
    if (colormap != DefaultColormap( display, DefaultScreen( display ) ) )
    {
	XSetWindowColormap( display, *win_id, colormap);
    }

/*
 *  Create the icon pixmap
 */
    icon_pixmap = XCreateBitmapFromData( display,
	RootWindow( display, DefaultScreen( display ) ),
	(char*)captain_starlink_bits, captain_starlink_width,
	captain_starlink_height);


/*
 *  Set the Icon (this doesn't work with DECWindows)
 */
    wmhints.flags = IconPixmapHint | IconMaskHint;
    wmhints.icon_pixmap = icon_pixmap;
    wmhints.icon_mask = icon_pixmap;

#if defined(_DECWmHints_h_) /* do the equivalent for DECwindows */
/*
 *  Create the icon pixmap
 */
    cs_icon_pixmap = XCreateBitmapFromData( display,
	RootWindow( display, DefaultScreen( display ) ),
	(char*)cshead_bits, cshead_width, cshead_height);
/*
 * 	Set the "iconify pixmap" into title bar and small icon
 */
    wmatom = XInternAtom(display, "DEC_WM_HINTS", 0);
    if (wmatom != None)
    {
        dwmhints.value_mask = DECWmIconifyPixmapMask;
        dwmhints.iconify_pixmap = cs_icon_pixmap;

/*
 * Replace the window properties
 */
	XChangeProperty(display,                 /* display */
                   *win_id,                      /* window */
                   wmatom,                       /* property name */
                   wmatom,                       /* type */

                   32,                           /* format 32 bit*/
                   PropModeReplace,              /* mode */
                   (unsigned char *) &dwmhints,  /* property data */
                   9);                           /* number of elements */
   }
#endif

/*
 *  Declare an interest in the WM_DELETE_WINDOW window manager protocol
 */
    wmatom = XInternAtom(display, "WM_PROTOCOLS", 0);
    if (wmatom != None)
    {
    	dwatom = XInternAtom(display, "WM_DELETE_WINDOW", 0);
	XChangeProperty(display,                 /* display */
                   *win_id,                      /* window */
                   wmatom,                       /* property name */
                   XA_ATOM,                      /* type */

                   32,                           /* format 32 bit*/
                   PropModeReplace,              /* mode */
                   (unsigned char*)&dwatom,      /* property data */
                   1);                           /* number of elements */
    }
/*
 *  Set the initial window state to iconic if requested
 */
    if ( iconic )
    {
	wmhints.initial_state = IconicState;
	wmhints.flags |= StateHint;
    }
    XSetWMHints( display, *win_id, &wmhints);

/*
**  Generate a window title if one hasn't been provided
*/
    if (!title)
    {
    	ltitle = malloc(strlen(name) + 15 );
    	if ( !ltitle )
    	{
	    status = GWM_MEM_ALLOC;
	    goto cleanup;
   	}
    	(void)strcpy( ltitle, "GWM window - ");
    	(void)strcat( ltitle, name );
    }
    else
    {
	ltitle = title;
    }
    XStoreName( display, *win_id, ltitle);
    if (!title) free( ltitle );

/*
 *  Map the window and wait for it to be created
 */
    XSelectInput( display, *win_id, StructureNotifyMask );
    XMapWindow( display, *win_id );
    XWindowEvent( display, *win_id, StructureNotifyMask, &event );

/*
**  The window should now have appeared on the screen (unless its
**  initial state is iconic) and if requested, we wait for a button
**  press event to signify that the user has finished resizing the
**  new window. Any configure notify events that appear before the
**  button press are discarded
*/
    if ( inter && (!iconic) )
    {
	for (;;)
	{
	    XSelectInput( display, *win_id,
		StructureNotifyMask | ButtonPressMask );
	    XWindowEvent( display, *win_id,
		StructureNotifyMask  |ButtonPressMask, &event );
	    if (event.type == ButtonPress ) break;
	}
    }

/*
**  We can now create the off-screen pixmap
*/
    XGetWindowAttributes( display, *win_id, &win_attrib );
    pix_id = XCreatePixmap( display, *win_id, win_attrib.width,
	win_attrib.height,
	DefaultDepthOfScreen(ScreenOfDisplay(display,DefaultScreen(display))));

    if ( !(pix_id) )
    {
	status = GWM_PIX_CREATE;
	goto cleanup;
    }

/*
**  Fill the pixmap with the background colour
*/
    gcvals.foreground = pix_array_l[0];
    gc = XCreateGC( display, pix_id, GCForeground, &gcvals);
    XFillRectangle( display, pix_id, gc, 0, 0, win_attrib.width,
	 win_attrib.height);
    XFreeGC( display, gc);


/*
**  All the necessary resources have now been allocated and various window
**  properties can be created to tie them all together.
*/
    title = malloc( strlen(name) + 5 );
    if ( !title )
    {
	status = GWM_MEM_ALLOC;
	goto cleanup;
    }
    (void)strcpy( title, "GWM_");
    (void)strcat( title, name );
    atom = XInternAtom( display, title, False );
    free( title );
    XChangeProperty( display, DefaultRootWindow( display ), atom, XA_WINDOW,
	32, PropModeReplace, (unsigned char*)win_id, 1);

    atom = XInternAtom( display, "GWM_name", False );
    XChangeProperty( display, *win_id, atom, XA_STRING, 8,PropModeReplace,
	(unsigned char*)name, strlen(name) );

    atom = XInternAtom( display, "GWM_pixmap", False );
    XChangeProperty( display, *win_id, atom, XA_PIXMAP, 32, PropModeReplace,
	(unsigned char*)&pix_id, 1 );

    atom = XInternAtom( display, "GWM_colour_table", False );
    XChangeProperty( display, *win_id, atom, XA_INTEGER, 32, PropModeReplace,
	(unsigned char*)(pix_array_l), lncols );

    atom = XInternAtom( display, "GWM_x_offset", False );
    XChangeProperty( display, *win_id, atom, XA_INTEGER, 32, PropModeReplace,
	(unsigned char*)(&zero), 1 );

    atom = XInternAtom( display, "GWM_y_offset", False );
    XChangeProperty( display, *win_id, atom, XA_INTEGER, 32, PropModeReplace,
	(unsigned char*)(&zero), 1 );

    atom = XInternAtom( display, "GWM_ov_mask", False );
    mask_i = ~mask;
    XChangeProperty( display, *win_id, atom, XA_INTEGER, 32, PropModeReplace,
	(unsigned char*)&mask_i, 1 );

    atom = XInternAtom( display, "GWM_x_ov_offset", False );
    XChangeProperty( display, *win_id, atom, XA_INTEGER, 32, PropModeReplace,
	(unsigned char*)(&zero), 1 );

    atom = XInternAtom( display, "GWM_y_ov_offset", False );
    XChangeProperty( display, *win_id, atom, XA_INTEGER, 32, PropModeReplace,
	(unsigned char*)(&zero), 1 );

    atom = XInternAtom( display, "GWM_foreground", False );
    XChangeProperty( display, *win_id, atom, XA_STRING, 8, PropModeReplace,
	(unsigned char*)fg, strlen(fg) );

    atom = XInternAtom( display, "GWM_background", False );
    XChangeProperty( display, *win_id, atom, XA_STRING, 8, PropModeReplace,
	(unsigned char*)bg, strlen(bg) );

    return GWM_SUCCESS;

cleanup:
    if ( pix_id ) XFreePixmap( display, pix_id );
    if ( *win_id ) XDestroyWindow( display, *win_id );
    if ( status != GWM_COL_ALLOC )
    {
	if ( visual_class == GrayScale || visual_class == PseudoColor ||
	     visual_class == DirectColor )
	{
	    XFreeColors( display, colormap, pix_array_l, lncols, 0);
	}
    }
    if ( pix_array_l ) free( pix_array_l );

    return status;
}
