/*
**
**  INCLUDE FILES
**
*/

#include <string.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include "gwm_err.h"
#include "gwm_sys.h"

int GWM_GetWinInfo( Display *display, Window win_id, struct wininfo **info)
/*
*+
*  Name:
*     GWM_GetWinInfo
*
*  Purpose:
*     Returns a structure containing information about the specified
*     window.
*
*  Language:
*     C
*
*  Invocation:
*     status = GWM_GetWinInfo( display, win_id, &info);
*
*  Description:
*     A wininfo structure is allocated and filled with the specification of
*     the specified window. This structure is used by GWM_ProcessEvent
*     in order to minimize the amount of work that has to be done in
*     response to events.
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
*        Window id
*     info = *struct wininfo
*        Window information structure
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
*      2-OCT-1991 (DLT):
*        Original version
*      2-MAR-1994 (DLT):
*        Get visual class
*      9-MAR-1994 (DLT):
*        Change name of statement label
*     {enter_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/
{
    struct wininfo *local_info;
    int status;
    Atom atom, actual_type;
    int actual_format;
    unsigned long nitems, bytes_after;
    Pixmap *local_pix_id;
    Drawable root;
    unsigned int border, depth;
    unsigned int i;
    int i1, i2;
    unsigned long *ctable_i, *local_mask;
    long *local_offset;
    char *name, *prop_name;
    XWindowAttributes winatt;

    local_info = (struct wininfo*)malloc( sizeof( struct wininfo ));
    if (!(local_info)) return GWM_MEM_ALLOC;

/*
**  Display id.
*/
    local_info->display = display;

/*
**  Window id.
*/
    local_info->win_id = win_id;

/*
**  Visual class
*/
    status = XGetWindowAttributes( display, win_id, &winatt);
    if ( !status )
    {
	status = GWM_WIN_NOEXIST;
	goto free_resources;
    }
    local_info->visclass = (winatt.visual)->class;

/*
**  Colour table and size
*/
    atom = XInternAtom(display, "GWM_colour_table", True );
    status = XGetWindowProperty( display, win_id , atom, 0, 1024, False,
       	XA_INTEGER, &actual_type, &actual_format, &(local_info->ctsize),
	&bytes_after, (unsigned char**)(&ctable_i));

    local_info->ctable = (unsigned long*)malloc(
	sizeof(unsigned long) * local_info->ctsize );
    if (!(local_info->ctable)) return GWM_MEM_ALLOC;
    for ( i = 0; i < local_info->ctsize; i++ )
	local_info->ctable[i] = ctable_i[i];
    XFree( (char*)ctable_i );

/*
**  Window name atom
*/
    atom = XInternAtom(display, "GWM_name", True );
    status = XGetWindowProperty( display, win_id, atom,0, 32, False, XA_STRING,
	&actual_type, &actual_format, &nitems, &bytes_after,
	(unsigned char**)(&name));
    prop_name = malloc( strlen((char*)name) + 5 );
    if ( !prop_name ) return GWM_MEM_ALLOC;

    (void)strcpy( prop_name, "GWM_");
    (void)strcat( prop_name, (char*)name );
    local_info->winname_atom = XInternAtom( display, prop_name, True );
    free( prop_name );

/*
**  Pixmap id.
*/
    atom = XInternAtom(display, "GWM_pixmap", False );
    status = XGetWindowProperty( display, win_id , atom, 0, 1, False,
	XA_PIXMAP, &actual_type, &actual_format, &nitems, &bytes_after,
	(unsigned char**)(&local_pix_id));
    if ( status != Success )
    {
	status = GWM_NO_PIXMAP;
	goto free_resources;
    }
    local_info->pix_id  = *local_pix_id;
    local_info->pix_atom = atom;
    XFree( (char*)local_pix_id );

/*
**  Window size and position
*/
    local_info->x_pos = winatt.x;
    local_info->y_pos = winatt.y;
    local_info->win_width = winatt.width;
    local_info->win_height = winatt.height;

/*
**  Pixmap size
*/
    status = XGetGeometry( display, local_info->pix_id, &root, &i1, &i2,
	    &local_info->pix_width, &local_info->pix_height, &border, &depth);
    if ( !status )
    {
	status = GWM_INV_PIXID;
	goto free_resources;
    }

/*
**  Offsets
*/
    atom = XInternAtom(display, "GWM_x_offset", False );
    status = XGetWindowProperty( display, win_id , atom, 0, 1, False,
	XA_INTEGER, &actual_type, &actual_format, &nitems, &bytes_after,
	(unsigned char**)(&local_offset));
    if ( status != Success )
    {
	status = GWM_NO_OFFSET;
	goto free_resources;
    }
    local_info->x_offset  = (int)*local_offset;
    local_info->xoff_atom = atom;
    XFree( (char*)local_offset );

    atom = XInternAtom(display, "GWM_y_offset", False );
    status = XGetWindowProperty( display, win_id , atom, 0, 1, False,
	XA_INTEGER, &actual_type, &actual_format, &nitems, &bytes_after,
	(unsigned char**)(&local_offset));
    if ( status != Success )
    {
	status = GWM_NO_OFFSET;
	goto free_resources;
    }
    local_info->y_offset  = (int)*local_offset;
    local_info->yoff_atom = atom;
    XFree( (char*)local_offset );

/*
**  overlay mask
*/
    atom = XInternAtom(display, "GWM_ov_mask", False );
    status = XGetWindowProperty( display, win_id , atom, 0, 1, False,
	XA_INTEGER, &actual_type, &actual_format, &nitems, &bytes_after,
	(unsigned char**)(&local_mask));
    if ( status != Success )
    {
	status = GWM_NO_OVMASK;
	goto free_resources;
    }
    local_info->mask  = *local_mask;
    XFree( (char*)local_mask );

/*
**  Overlay offsets
*/
    atom = XInternAtom(display, "GWM_x_ov_offset", False );
    status = XGetWindowProperty( display, win_id , atom, 0, 1, False,
	XA_INTEGER, &actual_type, &actual_format, &nitems, &bytes_after,
	(unsigned char**)(&local_offset));
    if ( status != Success )
    {
	status = GWM_NO_OFFSET;
	goto free_resources;
    }
    local_info->x_ov_offset  = (int)*local_offset;
    local_info->xov_off_atom = atom;
    XFree( (char*)local_offset );

    atom = XInternAtom(display, "GWM_y_ov_offset", False );
    status = XGetWindowProperty( display, win_id , atom, 0, 1, False,
	XA_INTEGER, &actual_type, &actual_format, &nitems, &bytes_after,
	(unsigned char**)(&local_offset));
    if ( status != Success )
    {
	status = GWM_NO_OFFSET;
	goto free_resources;
    }
    local_info->y_ov_offset  = (int)*local_offset;
    local_info->yov_off_atom = atom;
    XFree( (char*)local_offset );

/*
**  Colour change message atom
*/
    local_info->col_atom =  XInternAtom(display, "GWM_colour_change", False );

/*
**  Colour map
*/
    local_info->cmap = winatt.colormap;

/*
**  Copy pointer to wininfo structure to output argument
*/
    *info = local_info;
    return GWM_SUCCESS;

free_resources:
    free( local_info );
    return status;
}
