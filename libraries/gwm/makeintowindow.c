/*
**
**  INCLUDE FILES
**
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include "gwm_err.h"
#include "gwm.h"

int GWM_MakeIntoWindow ( Display *display, Window win_id, char *name,
			 unsigned int width, unsigned int height, int ncols,
			 int mincols, int fg, int bg, Boolean ovl, int ovcol)
/*
*+
*  Name:
*     GWM_MakeIntoWindow
*
*  Purpose:
*     Converts a window in to a GWM window
*
*  Language:
*     C
*
*  Invocation:
*     status = GWM_MakeIntoWindow( display, win_id, name, ncols, fg, bg,
*                   olv, ovcol);
*
*  Description:
*      The following operations are carried out:
*         Colour cells allocated - these are read/write if the server supports
*         it, otherwise they are read only.
*         Pixmap created.
*         GWM properties set on the window
*     If any part of the operation fails, all resources allocated are released.
*
*
*  Function return value:
*     status = int
*        The error status
*
*  Arguments:
*     display = *Display (given)
*        Display id
*     win_id = Window (given)
*        Id of window
*     name *char (given)
*        Window name
*     ncols = int (given)
*        Number of colours to be allocated to the window
*     mincols = int (given)
*        Minimum number of colours acceptable
*     fg = int (given)
*        Foreground colour
*     bg = int (given)
*        Background colour
*     olv = Boolean (given)
*        Allocate overlay plane if True
*     ovcol = int (given)
*        Overlay plane colour
*
*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1999, 2004 Central Laboratory of the Research Councils.
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
*     TIMJ: Tim Jenness (JAC)
*     {enter_new_authors_here}
*
*  History:
*      23-SEP-1992 (DLT):
*        Orignal version
*      05-MAR-1999 (DLT):
*        Use attributes of window instead of default attributes of X server
*      13-MAR-2004 (TIMJ):
*        pix_id is not a pointer and so can not be initialised with a NULL
*     {enter_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/
{
    int status, lncols ,i, ctsize, vclass, nitems;
    unsigned long *pix_array, mask = 0, mask_return;
    Pixmap pix_id;
    GC gc;
    XGCValues gcvals;
    Atom atom;
    XColor bg_color, fg_color, ov_color;
    char colname[14];
    unsigned long zero = 0;
    Window win_temp;
    char *title;
    int cols;
    XWindowAttributes winatt;
    XVisualInfo *vinfo, vinfo_template;

/*
**  Attempt to find a window with this name in case it already exists; if
**  it does then just exit.
*/
    status = GWM_FindWindow( display, name, &win_temp);
    if (status == GWM_SUCCESS) return GWM_WINDOW_EXISTS;

/*
**  Get info on the window
*/
    XGetWindowAttributes( display, win_id, &winatt);

/*
**  Determine how many entries the color map has.
*/
    vinfo_template.visualid = XVisualIDFromVisual( winatt.visual );
    vinfo = XGetVisualInfo( display, VisualIDMask, &vinfo_template, &nitems);
    vclass = vinfo->class;
    if ( vclass == TrueColor )
       ctsize = vinfo->depth * vinfo->depth * vinfo->depth;
    else
       ctsize = vinfo->colormap_size;
    XFree( vinfo );

/*
**  Select a suitable number of colours to allocate - if the number
**  requested is > 0 then use that number, otherwise no number has been
**  specified yet and we have to chose a value based on the properties
**  of the window
*/
    if (ncols > 0)
    {
	lncols = ncols;
    }
    else
    {
/*
**	If the colour table is static then colour table entries are
**	shared by all windows and we can use the maximum available but
**      don't use more than 256 as some applications can't cope with more.
*/
	if (vclass == StaticGray ||
	    vclass == StaticColor ||
	    vclass == TrueColor )
	{
	    lncols = ctsize < 256 ? ctsize : 256;
	}
	else
	{

/*
**	    Otherwise use a quarter of the total
*/
	    lncols = ctsize / 4;

/*
**	    But if that is silly, use half
*/
	    if (lncols < 4) lncols = ctsize / 2;
	}
    }

/*
**  Now make sure that the number we are going to try and get is at least
**  as big as the minimum
*/
    if ( lncols < mincols ) lncols = mincols;

/*
**  Allocate an array to hold the colour table
*/
    pix_array = (unsigned long*)malloc( sizeof(unsigned long) * lncols );
    if (!(pix_array)) return GWM_MEM_ALLOC;

  /*  From now on there are resources to be deallocated if an error occurs */
    pix_id = 0;

/*
**  Allocate the number of colour cells requested - read/write if the server
**  supports it. If the colour map is read only then we just have to check that
**  it is at least as big as the minimum number of requested colours.
*/

    if ( vclass == GrayScale ||
         vclass == PseudoColor )
    {
	for (cols = lncols; cols >= mincols; cols-- )
	{
	    status = XAllocColorCells( display, winatt.colormap, False,
		&mask, ovl ? 1 : 0 ,pix_array, cols);
	    if (status) break;
	}
	lncols = cols;
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
	if ( vclass == DirectColor )
	{
	    for (; lncols >= mincols; lncols-- )
	    {
		status = XAllocColorPlanes( display,
		    winatt.colormap, False, pix_array, lncols, 0, 0, 0,
                    &mask_return, &mask_return, &mask_return);
		if (status) break;
	    }
	}
	else
	{
	    if ( ctsize >= mincols )
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
    fg_color.pixel = fg;
    XQueryColor( display, winatt.colormap, &fg_color);
    bg_color.pixel = bg;
    XQueryColor( display, winatt.colormap, &bg_color);

    if ( winatt.visual->class == GrayScale ||
         winatt.visual->class == PseudoColor ||
	 winatt.visual->class == DirectColor)
    {
	bg_color.pixel = pix_array[0];
	XStoreColor( display, winatt.colormap, &bg_color);
	fg_color.pixel = pix_array[1];
	XStoreColor( display, winatt.colormap, &fg_color);
    }
    else
    {
	XAllocColor( display, winatt.colormap, &bg_color);
	pix_array[0] = bg_color.pixel;
	XAllocColor( display, winatt.colormap, &fg_color);
	pix_array[1] = fg_color.pixel;
    }

/*
**  Load the colour table with the overlay colour
*/
    if (ovl)
    {
	ov_color.pixel = ovcol;
	XQueryColor( display, winatt.colormap, &ov_color);
	for ( i = 0; i < lncols; i++)
	{
	    ov_color.pixel = pix_array[i] | mask;
	    XStoreColor( display, winatt.colormap, &ov_color);
	}
    }

/*
**  Create the off-screen pixmap
*/
    pix_id = XCreatePixmap( display, win_id, width, height, winatt.depth);

    if ( !(pix_id) )
    {
	status = GWM_PIX_CREATE;
	goto cleanup;
    }

/*
**  Fill the pixmap with the background colour
*/
    gcvals.foreground = pix_array[0];
    gc = XCreateGC( display, pix_id, GCForeground, &gcvals);
    XFillRectangle( display, pix_id, gc, 0, 0, width, height);
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
	32, PropModeReplace, (unsigned char*)&win_id, 1);

    atom = XInternAtom( display, "GWM_name", False );
    XChangeProperty( display, win_id, atom, XA_STRING, 8,PropModeReplace,
	(unsigned char*)name, strlen(name) );

    atom = XInternAtom( display, "GWM_pixmap", False );
    XChangeProperty( display, win_id, atom, XA_PIXMAP, 32, PropModeReplace,
	(unsigned char*)&pix_id, 1 );

    atom = XInternAtom( display, "GWM_colour_table", False );
    XChangeProperty( display, win_id, atom, XA_INTEGER, 32, PropModeReplace,
	(unsigned char*)(pix_array), lncols );
    free( pix_array );

    atom = XInternAtom( display, "GWM_x_offset", False );
    XChangeProperty( display, win_id, atom, XA_INTEGER, 32, PropModeReplace,
	(unsigned char*)(&zero), 1 );

    atom = XInternAtom( display, "GWM_y_offset", False );
    XChangeProperty( display, win_id, atom, XA_INTEGER, 32, PropModeReplace,
	(unsigned char*)(&zero), 1 );

    atom = XInternAtom( display, "GWM_ov_mask", False );
    mask = ~mask;
    XChangeProperty( display, win_id, atom, XA_INTEGER, 32, PropModeReplace,
	(unsigned char*)&mask, 1 );

    atom = XInternAtom( display, "GWM_x_ov_offset", False );
    XChangeProperty( display, win_id, atom, XA_INTEGER, 32, PropModeReplace,
	(unsigned char*)(&zero), 1 );

    atom = XInternAtom( display, "GWM_y_ov_offset", False );
    XChangeProperty( display, win_id, atom, XA_INTEGER, 32, PropModeReplace,
	(unsigned char*)(&zero), 1 );

    sprintf( colname, "#%04.4x%04.4x%04.4x", fg_color.red,
	fg_color.green, fg_color.blue );
    atom = XInternAtom( display, "GWM_foreground", False );
    XChangeProperty( display, win_id, atom, XA_STRING, 8, PropModeReplace,
	(unsigned char*)colname, strlen(colname) );

    sprintf( colname, "#%04.4x%04.4x%04.4x", bg_color.red,
	bg_color.green, bg_color.blue );
    atom = XInternAtom( display, "GWM_background", False );
    XChangeProperty( display, win_id, atom, XA_STRING, 8, PropModeReplace,
	(unsigned char*)colname, strlen(colname) );

    return GWM_SUCCESS;

cleanup:
    if ( pix_id ) XFreePixmap( display, pix_id );
    if ( status != GWM_COL_ALLOC )
    {
	if ( vclass == GrayScale ||
             vclass == PseudoColor ||
	     vclass == DirectColor )
	{
	    XFreeColors( display, winatt.colormap, pix_array, lncols, 0);
	}
    }
    if ( pix_array ) free( pix_array );

    return status;
}
