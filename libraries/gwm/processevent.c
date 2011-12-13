/*
**
**  INCLUDE FILES
**
*/

#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include "gwm_err.h"
#include "gwm_sys.h"

int GWM_ProcessEvent( struct wininfo *info, XEvent *event)
/*
*+
*  Name:
*     GWM_ProcessEvent
*
*  Purpose:
*     Handles X events for the refresh process
*
*  Language:
*     C
*
*  Invocation:
*     status = GWM_ProcessEvent( &info, &event);
*
*  Description:
*     This routine is called whenever there is an event in the event queue.
*     and handled as follows:
*     Expose:
*     	The exposed area of the window is refreshed by copying the
*     	relevant area of the pixmap.
*     ConfigureNotify:
*     	The pixmap offset is computed, and the new values of the
*     	offset properties set.
*     PropertyNotify:
*     	The new value of the changed property is stored in the window
*     	information structure.
*     DestroyNotify:
*     	Delete the pixmap and remove the GWM_<name> property from the
*     	root window.
*     ClientEvent:
*     	Colour cells are loaded in response to requests from
*     	applications. (this mechanism is obsolete and will be removed
*       in a future release).
*
*  Function return value:
*     status = int
*        The error status
*
*  Arguments:
*     info = *struct wininfo (given)
*        Window information structure
*     event = XEvent (given)
*        X event structure
*
*  Copyright:
*     Copyright (C) 1991, 1992, 1994 Science & Engineering Research Council.
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
*      6-MAR-1991 (DLT):
*        Orignal version
*      13-NOV-1991 (DLT):
*        Pass event structure by reference
*      19-NOV-1991 (DLT):
*        Allow settting of overlay plane colour
*      5-MAR-1992 (DLT):
*        Update scroll offsets
*      9-MAR-1992 (DLT):
*        Copy only overlap of expose area and pixmap to avoid clearing
*        areas outside pixmap
*      2-MAR-1994 (DLT):
*        Use colormap from win info structure
*     19-APR-1994 (DLT):
*        Refresh area outside pixmap with background
*     {enter_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/
{
    Bool status;
    Atom actual_type;
    int actual_format, off_diff, i;
    int x0, x1, y0, y1;
    int expx1, expy1;
    unsigned long nitems, bytes_after;
    long *plocal_offset;
    long local_offset;
    Pixmap *local_pix_id;
    Window *win_id;
    XColor color;
    XRectangle clip[8];
    int nrect;

	switch( event->type )
	{
	    case Expose:
	    /*
	    **  Calculate the intersection of the exposed region and the
            **  pixmap
            */
		x0 = event->xexpose.x > info->x_offset ? event->xexpose.x :
		    info->x_offset;
		x1 = (event->xexpose.x + event->xexpose.width) <
		    (info->x_offset + info->pix_width) ?
		    (event->xexpose.x + event->xexpose.width) :
		    ( info->x_offset + info->pix_width);
		y0 = event->xexpose.y > info->y_offset ? event->xexpose.y :
		    info->y_offset;
		y1 = (event->xexpose.y + event->xexpose.height) <
		    (info->y_offset + info->pix_height) ?
		    (event->xexpose.y + event->xexpose.height) :
		    ( info->y_offset + info->pix_height);

	    /*
	    **  Compute the rectangles that define the exposed area that
	    **  Lies outside the pixmap.
	    */
		expx1 = event->xexpose.width + event->xexpose.x;
		expy1 = event->xexpose.height + event->xexpose.y;
		nrect = 0;
		if (x0 > event->xexpose.x && y0 > event->xexpose.y )
		{
		    clip[nrect].x = event->xexpose.x;
		    clip[nrect].y = event->xexpose.y;
		    clip[nrect].width = x0 - event->xexpose.x;
		    clip[nrect].height = y0 - event->xexpose.y;
		    nrect++;
		}
		if ( x1 > x0 && y0 > event->xexpose.y)
		{
		    clip[nrect].x = x0;
		    clip[nrect].y = event->xexpose.y;
		    clip[nrect].width = x1 - x0;
		    clip[nrect].height = y0 - event->xexpose.y;
		    nrect++;
		}
		if ( expx1 > x1 && y0 > event->xexpose.y)
		{
		    clip[nrect].x = x1;
		    clip[nrect].y = event->xexpose.y;
		    clip[nrect].width = expx1 - x1;
		    clip[nrect].height = y0 - event->xexpose.y;
		    nrect++;
		}
		if ( x0 > event->xexpose.x && y1 > y0)
		{
		    clip[nrect].x = event->xexpose.x;
		    clip[nrect].y = y0;
		    clip[nrect].width = x0 - event->xexpose.x;
		    clip[nrect].height = y1 - y0;
		    nrect++;
		}
		if ( expx1 > x1 && y1 > y0)
		{
		    clip[nrect].x = x1;
		    clip[nrect].y = y0;
		    clip[nrect].width = expx1 - x1;
		    clip[nrect].height = y1 - y0;
		    nrect++;
		}
		if ( x0 > event->xexpose.x && expy1 > y1)
		{
		    clip[nrect].x = event->xexpose.x;
		    clip[nrect].y = y1;
		    clip[nrect].width = x0 - event->xexpose.x;
		    clip[nrect].height = expy1 - y1;
		    nrect++;
		}
		if ( x1 > x0 && expy1 > y1)
		{
		    clip[nrect].x = x0;
		    clip[nrect].y = y1;
		    clip[nrect].width = x1 - x0;
		    clip[nrect].height = expy1 - y1;
		    nrect++;
		}
		if ( expx1 > x1 && expy1 > y1)
		{
		    clip[nrect].x = x1;
		    clip[nrect].y = y1;
		    clip[nrect].width = expx1 - x1;
		    clip[nrect].height = expy1 - y1;
		    nrect++;
		}

	    /*
	    **  Set the exposed area to the background
	    */
		if (nrect > 0)
		{
		    XSetPlaneMask( info->display, info->gc, info->mask);
		    XSetForeground( info->display, info->gc, info->ctable[0]);
		    XSetClipRectangles( info->display, info->gc, 0, 0, clip,
		        nrect, YXSorted);
		    XFillRectangle(info->display, info->win_id, info->gc,
		        event->xexpose.x, event->xexpose.y,
			event->xexpose.width, event->xexpose.height);
		}
	    /*
            **  If the intersection is not empty then copy the relevent
            **  area of the pixmap
            */
		XSetClipMask(info->display, info->gc, None);
		if ( (x0 < x1) && (y0 < y1) )
		{
		    XSetPlaneMask( info->display, info->gc, info->mask);
		    XCopyArea( info->display, info->pix_id, info->win_id,
			info->gc, x0 - info->x_offset, y0 - info->y_offset,
			x1 - x0, y1 - y0, x0, y0);
		}

	    /*
	    **  Repeat for the overlay plane (if there is one)
            */
		if (~info->mask)
		{
		    x0 = event->xexpose.x > info->x_ov_offset ?
			event->xexpose.x : info->x_ov_offset;
		    x1 = (event->xexpose.x + event->xexpose.width) <
		    	(info->x_ov_offset + info->pix_width) ?
		    	(event->xexpose.x + event->xexpose.width) :
		    	( info->x_ov_offset + info->pix_width);
		    y0 = event->xexpose.y > info->y_ov_offset ?
			event->xexpose.y : info->y_ov_offset;
		    y1 = (event->xexpose.y + event->xexpose.height) <
		    	(info->y_ov_offset + info->pix_height) ?
		    	(event->xexpose.y + event->xexpose.height) :
		    	( info->y_ov_offset + info->pix_height);
		    expx1 = event->xexpose.width + event->xexpose.x;
		    expy1 = event->xexpose.height + event->xexpose.y;
		    nrect = 0;
		    if (x0 > event->xexpose.x && y0 > event->xexpose.y )
		    {
			clip[nrect].x = event->xexpose.x;
			clip[nrect].y = event->xexpose.y;
			clip[nrect].width = x0 - event->xexpose.x;
			clip[nrect].height = y0 - event->xexpose.y;
			nrect++;
		    }
		    if ( x1 > x0 && y0 > event->xexpose.y)
		    {
			clip[nrect].x = x0;
			clip[nrect].y = event->xexpose.y;
			clip[nrect].width = x1 - x0;
			clip[nrect].height = y0 - event->xexpose.y;
			nrect++;
		    }
		    if ( expx1 > x1 && y0 > event->xexpose.y)
		    {
			clip[nrect].x = x1;
			clip[nrect].y = event->xexpose.y;
			clip[nrect].width = expx1 - x1;
			clip[nrect].height = y0 - event->xexpose.y;
			nrect++;
		    }
		    if ( x0 > event->xexpose.x && y1 > y0)
		    {
			clip[nrect].x = event->xexpose.x;
			clip[nrect].y = y0;
			clip[nrect].width = x0 - event->xexpose.x;
			clip[nrect].height = y1 - y0;
			nrect++;
		    }
		    if ( expx1 > x1 && y1 > y0)
		    {
			clip[nrect].x = x1;
			clip[nrect].y = y0;
			clip[nrect].width = expx1 - x1;
			clip[nrect].height = y1 - y0;
			nrect++;
		    }
		    if ( x0 > event->xexpose.x && expy1 > y1)
		    {
			clip[nrect].x = event->xexpose.x;
			clip[nrect].y = y1;
			clip[nrect].width = x0 - event->xexpose.x;
			clip[nrect].height = expy1 - y1;
			nrect++;
		    }
		    if ( x1 > x0 && expy1 > y1)
		    {
			clip[nrect].x = x0;
			clip[nrect].y = y1;
			clip[nrect].width = x1 - x0;
			clip[nrect].height = expy1 - y1;
			nrect++;
		    }
		    if ( expx1 > x1 && expy1 > y1)
		    {
			clip[nrect].x = x1;
			clip[nrect].y = y1;
			clip[nrect].width = expx1 - x1;
			clip[nrect].height = expy1 - y1;
			nrect++;
		    }
		    if (nrect > 0)
		    {
			XSetPlaneMask( info->display, info->gc, ~info->mask);
			XSetForeground( info->display, info->gc, 0);
			XSetClipRectangles( info->display, info->gc, 0, 0, clip,
	 		    nrect, YXSorted);
			XFillRectangle(info->display, info->win_id, info->gc,
	 		    event->xexpose.x, event->xexpose.y,
			    event->xexpose.width, event->xexpose.height);
		    }
		    if ( (x0 < x1) && (y0 < y1) )
		    {
		    	XSetPlaneMask( info->display, info->gc, ~info->mask);
			XSetClipMask(info->display, info->gc, None);
		    	XCopyArea( info->display, info->pix_id, info->win_id,
			    info->gc, x0 - info->x_ov_offset,
			    y0 - info->y_ov_offset, x1 - x0, y1 - y0, x0, y0);
		    }
		}
		break;

	    case ConfigureNotify:
	    /*
	    **  Deal with x first - only do something if the width has
	    **  changed.
	    */
		if ( event->xconfigure.width != info->pix_width )
		{
		/*
		**  if the window is wider than the pixmap then just
		**  centre the pixmap in the window.
		*/
		    if ( event->xconfigure.width >= info->pix_width )
		    {
			off_diff = info->x_offset - info->x_ov_offset;
			info->x_offset = (int)( event->xconfigure.width -
			    info->pix_width + off_diff) / 2;
			info->x_ov_offset = (int)( event->xconfigure.width -
			    info->pix_width - off_diff) / 2;
		    }
		    else
		    {
		    	/*
		    	**  Keep the centre of the window in the same place
		    	**	relative to the pixmap
		    	*/
			info->x_offset += ((int)event->xconfigure.width -
			    (int)info->win_width) / 2;
			info->x_ov_offset += ((int)event->xconfigure.width -
			    (int)info->win_width) / 2;
		    }
		/*
		**  Update the info structure with the new size and
		**  position
		*/
		    info->x_pos = event->xconfigure.x;
		    info->win_width = event->xconfigure.width;
		/*
		**  Update the scroll offset window propertys
		*/
		    local_offset = (long)info->x_offset;
		    XChangeProperty( info->display, info->win_id,
			info->xoff_atom, XA_INTEGER, 32, PropModeReplace,
        		(unsigned char*)(&local_offset), 1 );
		    local_offset = (long)info->x_ov_offset;
		    XChangeProperty( info->display, info->win_id,
			info->xov_off_atom, XA_INTEGER, 32, PropModeReplace,
        		(unsigned char*)(&local_offset), 1 );
		}
	    /*
	    **  Now deal with y in the same way
	    */
		if ( event->xconfigure.height != info->pix_height )
		{
		    if ( event->xconfigure.height >= info->pix_height )
		    {
			off_diff = info->y_offset - info->y_ov_offset;
			info->y_offset = (int)( event->xconfigure.height -
			    info->pix_height + off_diff) / 2;
			info->y_ov_offset = (int)( event->xconfigure.height -
			    info->pix_height - off_diff) / 2;
		    }
		    else
		    {
			info->y_offset += ((int)event->xconfigure.height -
			    (int)info->win_height) / 2;
			info->y_ov_offset += ((int)event->xconfigure.height -
			    (int)info->win_height) / 2;
		    }
		    info->y_pos = event->xconfigure.y;
		    info->win_height = event->xconfigure.height;

		    local_offset = (long)info->y_offset;
		    XChangeProperty( info->display, info->win_id,
			info->yoff_atom, XA_INTEGER, 32, PropModeReplace,
        		(unsigned char*)(&local_offset), 1 );
		    local_offset = (long)info->y_ov_offset;
		    XChangeProperty( info->display, info->win_id,
			info->yov_off_atom, XA_INTEGER, 32, PropModeReplace,
        		(unsigned char*)(&local_offset), 1 );
		}
                break;

	    case PropertyNotify:
            /*
            **  If the property has been deleted then ignore the event
            */
		if ( event->xproperty.state == PropertyDelete) break;
	    /*
	    **	Update the window info structure with the new value of the
	    **	property that has change.
	    */
		if ( event->xproperty.atom == info->pix_atom )
		{
		    status = XGetWindowProperty( info->display,
			info->win_id, event->xproperty.atom, 0, 1, False,
			XA_PIXMAP, &actual_type, &actual_format, &nitems,
			&bytes_after, (unsigned char**)(&local_pix_id));
		    if (!status)
		    {
			info->pix_id = *local_pix_id;
			XFree( (char*)local_pix_id );
		    }
		}
		else if ( event->xproperty.atom == info->xoff_atom)
		{
		    status = XGetWindowProperty( info->display,
			info->win_id, event->xproperty.atom, 0, 1, False,
		 	XA_INTEGER, &actual_type, &actual_format,
			&nitems, &bytes_after,
			(unsigned char**)(&plocal_offset));
		    if (!status)
		    {
			info->x_offset = (int)*plocal_offset;
			XFree( (char*)plocal_offset);
		    }
		}
		else if ( event->xproperty.atom == info->yoff_atom)
		{
		    status = XGetWindowProperty( info->display,
			info->win_id, event->xproperty.atom, 0, 1, False,
			XA_INTEGER, &actual_type, &actual_format,
			&nitems, &bytes_after,
			(unsigned char**)(&plocal_offset));
		    if (!status)
		    {
			info->y_offset = (int)*plocal_offset;
			XFree( (char*)plocal_offset);
		    }
		}
		else if ( event->xproperty.atom == info->xov_off_atom)
		{
		    status = XGetWindowProperty( info->display,
			info->win_id, event->xproperty.atom, 0, 1, False,
		 	XA_INTEGER, &actual_type, &actual_format,
			&nitems, &bytes_after,
			(unsigned char**)(&plocal_offset));
		    if (!status)
		    {
			info->x_ov_offset = (int)*plocal_offset;
			XFree( (char*)plocal_offset);
		    }
		}
		else if ( event->xproperty.atom == info->yov_off_atom)
		{
		    status = XGetWindowProperty( info->display,
			info->win_id, event->xproperty.atom, 0, 1, False,
			XA_INTEGER, &actual_type, &actual_format,
			&nitems, &bytes_after,
			(unsigned char**)(&plocal_offset));
		    if (!status)
		    {
			info->y_ov_offset = (int)*plocal_offset;
			XFree( (char*)plocal_offset);
		    }
		}
		break;

	    case ClientMessage:
	    /*
	    **  Check that it is a colour table change request
	    */
		if (event->xclient.message_type == info->col_atom)
		{
		/*
		**  Check that the colour table is writable
		*/
		    if ( info->visclass == PseudoColor ||
			info->visclass == DirectColor ||
			info->visclass == GrayScale)
		    {
		    /*
		    **   Check that the colour map entry is allocated to
		    **   this window
		    */
			for( i = 0; i < info->ctsize; i++)
			{
			    if (info->ctable[i] == event->xclient.data.l[0]
				|| (info->ctable[i] | ~info->mask)
				== event->xclient.data.l[0])
			    {
			    	color.pixel = event->xclient.data.l[0];
			    	color.red = event->xclient.data.l[1];
			    	color.green = event->xclient.data.l[2];
			    	color.blue = event->xclient.data.l[3];
			    	color.flags = DoRed | DoGreen | DoBlue;
			    	XStoreColor( info->display, info->cmap,
				    &color);
				break;
			    }
			}
		    }
		}
	    /*
	    **  or a WM_PROTOCOLS message
	    */
		if (event->xclient.message_type ==
		    XInternAtom(info->display, "WM_PROTOCOLS", True) )
		{
			event->type = DestroyNotify;
		}
		break;

	    case DestroyNotify:
	    /*
	    **  Destroy the pixmap
	    */
		XFreePixmap( info->display, info->pix_id);
	    /*
	    **  Remove the window name from the root window
	    */
		status = XGetWindowProperty( info->display,
		    DefaultRootWindow( info->display ),
		    info->winname_atom, 0, 1, True, XA_WINDOW,
		    &actual_type, &actual_format, &nitems, &bytes_after,
		    (unsigned char**)(&win_id));
		XFree( win_id);
		break;
	    }
/*
**  Flush the display queue
*/
    XFlush( info->display );

    return GWM_SUCCESS;
}
