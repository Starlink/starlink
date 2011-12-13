/*
*+
*  Name:
*     gwm.h

*  Purpose:
*     Public function and structure definitions for GWM.

*  Language:
*     C Header file

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     DLT: D L Terrett (Starlink)

*-
*/

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>

/*
**  Functions
*/
int GWM_FindWindow( Display *display, char name[], Window *win_id);

int GWM_CreateWindow( int argc, char *argv[], Display **display, char[]);

int GWM_DestroyWindow( Display *display, char name[]);

int GWM_GetPixmap( Display *display, Window win, Pixmap *pix_id);

int GWM_GetOvMask( Display *display, Window win, unsigned long *mask);

int GWM_SetPixmap( Display *display, Window win, Pixmap pix_id);

int GWM_GetColTable( Display *display, Window win, unsigned long
	**table, unsigned long *size);

int GWM_SetColTable( Display *display, Window win, unsigned long
	*table, unsigned long size);

int GWM_GetScroll( Display *display, Window win, int *xscroll, int *yscroll);

int GWM_GetOvScroll( Display *display, Window win, int *xscroll, int *yscroll);

int GWM_SetScroll( Display *display, Window win, int xscroll, int yscroll);

int GWM_SetOvScroll( Display *display, Window win, int xscroll, int yscroll);

int GWM_GetBgCol( Display *display, Window win_id, char **bg);

int GWM_SetBgCol( Display *display, Window win_id, char *bg);

int GWM_GetFgCol( Display *display, Window win_id, char **bg);

int GWM_SetFgCol( Display *display, Window win_id, char *bg);

int GWM_SetColour( Display *display, Window win, unsigned long entry,
	unsigned long r, unsigned long g, unsigned long b);
