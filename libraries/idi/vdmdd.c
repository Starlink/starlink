/*
*+
*  Name:
*     VDMDD.C
*
*  Purpose:
*     Virtual Display Management
*
*  Description:
*     Virtual Display Management
*
*  Contents:
*     vdm_cr_a
*        Device dependent A display window creation
*     vdm_cr_g
*        Device dependent G display window creation
*     vdm_cr_i
*        Device dependent I display window creation
*     vdm_del_x
*        Device dependent display window deletion
*     vdm_inq_x
*        Device dependent inquire display window characteristics
*     vdm_mod_x
*        Device dependent display window characteristics modification
*     vdm_sav_x
*        Device dependent display window save as a bitmap
*
*  Copyright:
*     Copyright (C) 1988, 1991, 1992, 1993, 1994 Science & Engineering Research Council.
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
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     RS: R.Smareglia
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     26-OCT-1988 (PASIAN,SANTIN):
*        Version for XWindow X10.4
*     26-OCT-1988 (SANTIN,RS):
*        Version for XWindow X11
*     21-FEB-1991 (NE):
*        Added include files for window manager hints
*     16-MAY-1991 (NE):
*        Added conditional compilation
*     26-MAR-1992 (NE):
*        Added idi_err.h
*/

/* System definitions */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>

#if HAVE_X11_DEVWMHINTS_H
#include <X11/decwmhints.h>
#endif

/* Package definitions */

#include "gwm.h"
#include "device.dep"
#include "vdm.h"
#include "idi_err.h"
#include "kwm.h"
#include "idi.h"
#include "idifuncs.h"

/* Package definitions */

static  int             arg[ARGSIZE];
static  char            filwnd[256];
static  int             virgin = 1;

/******************************************************************************/

void vdm_cr_a ( char device[], int xoff, int yoff, int xdim, int ydim,
                char wind[], int* status )

/*
*+
*  Name:
*     vdm_cr_a
*
*  Purpose:
*     Device dependent A display window creation
*
*  Invocation:
*     vdm_cr_a( device, xoff, yoff, xdim, ydim, wind, status )
*
*  Description:
*     Device dependent A display window creation called by : vdm_cre
*
*  Arguments:
*     device = char[]
*        Device type
*     xoff = int
*        X offset
*     yoff = int
*        Y offset
*     xdim = int
*        X display dimension
*     ydim = int
*        Y display dimension
*     wind = char[]
*        Window name
*     status = int
*        Status code
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/

{

return;
}

/******************************************************************************/

void vdm_cr_g ( char device[], int xoff, int yoff, int xdim, int ydim,
                char wind[], int* status )

/*
*+
*  Name:
*     vdm_cr_g
*
*  Purpose:
*     Device dependent G display window creation
*
*  Invocation:
*     vdm_cr_g( device, xoff, yoff, xdim, ydim, wind, status )
*
*  Description:
*     Device dependent G display window creation called by : vdm_cre
*
*  Arguments:
*     device = char[]
*        Device type
*     xoff = int
*        X offset
*     yoff = int
*        Y offset
*     xdim = int
*        X display dimension
*     ydim = int
*        Y display dimension
*     wind = char[]
*        Window name
*     status = int
*        Status code
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/


{
return;
}

/******************************************************************************/

void vdm_cr_i ( char device[], int xoff, int yoff, int xdim, int ydim,
                char wind[], int* status )

/*
*+
*  Name:
*     vdm_cr_i
*
*  Purpose:
*     Device dependent I display window creation
*
*  Invocation:
*     vdm_cr_i( device, xoff, yoff, xdim, ydim, wind, status )
*
*  Description:
*     Device dependent I display window creation called by : vdm_cre
*
*  Arguments:
*     device = char[]
*        Device type
*     xoff = int
*        X offset
*     yoff = int
*        Y offset
*     xdim = int
*        X display dimension
*     ydim = int
*        Y display dimension
*     wind = char[]
*        Window name
*     status = int
*        Status code
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Get window file records from memory file and removed redundant code
*      8-MAY-1991 (NE):
*        Strict X library adherence
*     23-MAY-1991 (NE):
*        Use character array for wind0
*     20-AUG-1991 (NE):
*        Added GWM
*     18-NOV-1991 (NE):
*        Corrected arguments to GWM_CreateWindow
*     19-MAY-1992 (NE):
*        Decrement virgin counter if an error occurs
*     15-APR-1993 (DLT):
*        Use long for pixels
*     11-MAR-1994 (DLT):
*        Free pixels array
*     19-MAY-1992 (TIMJ):
*        Decrement virgin counter if an unsupported visual error occurs
*/

{

/* Local Variables */
Display            *display_id = NULL;
Window             w_id;
Pixmap             pix_id;
XWindowAttributes  info;
GC		   gcima;
XGCValues	   values;
Status             istat;
Window             root;
int                x, y;
unsigned int       width, height, border, depth;
XWindowAttributes  winatt;
XVisualInfo vinfo_template, *vinfo;
int                nitems;


int            screen;

char   wind0[4], lname[32], geom[32], cols[32], *argv[5];
char   *getdev();
int    col, i, j, jstat, ncolors, newwin, nit, wfree;
unsigned long *pixels, *pix;

* status = VD_SUCCESS;

/* - - - - - - - - - - - - - - - - -  */
/* System dependent Init              */
/* - - - - - - - - - - - - - - - - -  */

/* Connect to X server */

strcpy( wind0, "WI " );
if (virgin == 1)
   {
   if (!(display_id = XOpenDisplay(NULL)))
      {
      *status = VD_XCONNERR;
      return;
      }
   virgin++;
   }
else
   {
   virgin++;
   newwin = 0 ;
   wind0[2]= '0';
   while (newwin == 0 )
      {

/* Get window information from memory file */
      nit = 1;
      kwm_xtr (filwnd , wind0 , &nit , arg);
      if (nit == 0)
         {
         *status = VD_FILKWERR;
         return;
         }
      if (arg[0] != 0)
         {
         display_id =  (Display*) arg[0] ;
         wind0[2]++;
         }
      else
	 newwin = 1;
      }

   }

/* Window creation */
/* Look for an existing window with this name */
if ( GWM_FindWindow( display_id, device, &w_id ) != VD_SUCCESS )
   {

/* If a window doesn't exist then create one */
   argv[0] = "IDI";
   argv[1] = device;

/* Create a string containing the geometry */
   argv[2] = "-geometry";
   sprintf( geom, "%dx%d+%d+%d", xdim, ydim, xoff, yoff );
   argv[3] = geom;

/* Terminate the argument list with a NULL */
   argv[4] = NULL;
   if ( GWM_CreateWindow( 4, argv, &display_id, lname ) != VD_SUCCESS )
      {
      if ( virgin > 1 ) virgin--;
      *status = VD_WINOTOPN;
      return;
      }
   if ( GWM_FindWindow( display_id, lname, &w_id ) != VD_SUCCESS )
      {
      if ( virgin > 1 ) virgin--;
      *status = VD_WINOTOPN;
      return;
      }

/* Check that the visual type is supported */
      XGetWindowAttributes( display_id, w_id, &winatt);
      vinfo_template.visualid = XVisualIDFromVisual( winatt.visual );
      vinfo = XGetVisualInfo( display_id, VisualIDMask, &vinfo_template,
                &nitems);
      if ( vinfo->class != PseudoColor && vinfo->class != GrayScale )
         {
         XFree( vinfo );
         if ( virgin > 1 ) virgin--;
         *status = VD_UNSUPVT;
         return;
      }
      XFree( vinfo );

/* Install a greyscale LUT */
   if ( GWM_GetColTable( display_id, w_id, &pixels, &ncolors ) != VD_SUCCESS )
      {
      if ( virgin > 1 ) virgin--;
      free (pixels);
      *status = VD_WINOTOPN;
      return;
      }
   pix = pixels;
   for ( j = 0; j < ncolors; j++ )
      {
      col = ( 65535 * j ) / ( ncolors - 1 );
      jstat = GWM_SetColour( display_id, w_id, *pix++, col, col, col );
      }
   free (pixels);

/* End of window creation */
   }
else
   {
/* Check that the visual type is supported */
   XGetWindowAttributes( display_id, w_id, &winatt);
   vinfo_template.visualid = XVisualIDFromVisual( winatt.visual );
   vinfo = XGetVisualInfo( display_id, VisualIDMask, &vinfo_template,
             &nitems);
   if ( vinfo->class != PseudoColor && vinfo->class != GrayScale )
      {
      XFree( vinfo );
      if ( virgin > 1 ) virgin--;
      *status = VD_UNSUPVT;
      return;
      }
   XFree( vinfo );
   }

if (w_id == VD_NULL)
   {
   if ( virgin > 1 ) virgin--;
   *status = VD_WINOTOPN;
   return;
   }

XMapRaised (display_id, w_id);
XFlush (display_id);

/* ---------------------------------------------- */

screen = XDefaultScreen (display_id);
istat = GWM_GetPixmap(display_id, w_id,&pix_id);
istat = XGetGeometry (display_id, pix_id, &root, &x, &y, &width, &height,
                      &border, &depth);

xdim = width;
ydim = height;
istat = XGetWindowAttributes (display_id, w_id, &info);
xoff = info.x ;
yoff = XDisplayHeight (display_id, screen) -
       info.height - info.y - info.border_width * 2;

/* - - - - - - - - - - - - - - -   */
/* Update window file              */
/* - - - - - - - - - - - - - - -   */

wind0[2] = '0';
wfree = 0;
while (wfree == 0 )
   {

/* Get window information from memory file */
   nit = 1;
   kwm_xtr (filwnd , wind0 , &nit , arg);
   if (nit == 0)
      {
      *status = VD_FILKWERR;
      return;
      }
   if (arg[0] != 0)
      wind0[2]++;
   else
      {
      arg[0] = (int) display_id;
      arg[1] = (int) w_id;
      arg[2] = xoff;
      arg[3] = yoff;
      arg[4] = xdim;
      arg[5] = ydim;

/* Update window information in memory file */
      nit = 6;
      kwm_upd  (filwnd , wind0 , &nit , arg);
      if (nit == 0)
         {
         *status = VD_FILKWERR;
         return;
         }

/* Get number of windows of I type from memory file */
      nit = 1;
      kwm_xtr (filwnd , "NWNDI" , &nit , arg);
      if (nit == 0)
         {
         *status = VD_FILKWERR;
         return;
         }
      arg[0]++;

/* Update number of windows of I type in memory file */
      kwm_upd  (filwnd , "NWNDI" , &nit , arg);
      if (nit == 0)
         {
         *status = VD_FILKWERR;
         return;
         }
      wfree = 1;
      strcpy( wind, getdev() );
      strcat( wind, "." );
      strcat( wind, wind0 );
      }
   }

return;
}

/******************************************************************************/

void vdm_del_x ( char device[], char wind[], int* status )

/*
*+
*  Name:
*     vdm_del_x
*
*  Purpose:
*     Device dependent display window deletion
*
*  Invocation:
*     vdm_del_x( device, wind, status )
*
*  Description:
*     Device dependent display window deletion A/G/I called by : vdm_del
*
*  Arguments:
*     device = char[]
*        Device type
*     wind = char[]
*        Window name
*     status = int
*        Status code
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Get window file records from memory file.
*/

{

/* Local Variables */
Display        *display_id;
Window         w_id;

int            nit;

*status = VD_SUCCESS;

/* - - - - - - - - - - - - - - - - -   */
/* System dependent Close              */
/* - - - - - - - - - - - - - - - - -   */

/* Get window information from memory file */
nit = ARGSIZE;
kwm_xtr (filwnd , wind , &nit , arg);
if (nit == 0)
   {
   *status = VD_FILKWERR;
   return;
   }
virgin-- ;

display_id = (Display *) arg[0];
w_id = (Window) arg[1];

XDestroyWindow( display_id, w_id );
XFlush( display_id );

/* Update window information in memory file */
nit = 1;
arg[0] = 0;
kwm_upd (filwnd , wind , &nit , arg);
if (nit == 0)
   {
   *status = VD_FILKWERR;
   return;
   }

return;
}

/******************************************************************************/

void vdm_inq_x ( char device[], char wind[], int* xoff, int* yoff, int* xdim,
                 int* ydim, int* status )

/*
*+
*  Name:
*     vdm_inq_x
*
*  Purpose:
*     Device dependent inquire display window characteristics
*
*  Invocation:
*     vdm_inq_x( device, wind, xoff, yoff, xdim, ydim, status )
*
*  Description:
*     Device dependent inquire display window characteristics A / G / I
*     called by : vdm_inq
*
*  Arguments:
*     device = char[]
*        Device type
*     wind = char[]
*        Window name
*     xoff = int
*        X offset
*     yoff = int
*        Y offset
*     xdim = int
*        X display dimension
*     ydim = int
*        Y display dimension
*     status = int
*        Status code
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Get window file records from memory file.
*        Removed redundant code.
*      8-MAY-1991 (NE):
*        Strict X library adherence.
*/

{

/* Local Variables */
Display            *display_id;
Window             w_id;
Pixmap             pix_id;
XWindowAttributes  info;
Status             istat;
Window             root;
int                x, y;
unsigned int       width, height, border, depth;

int                screen;
int    nit;

*status = VD_SUCCESS;

if (strlen(wind) == 0)
   {
/* get physical device characteristics */

#if 0
   screen = XDefaultScreen (display_id);
   *xdim = XDisplayWidth  (display_id, screen);
   *ydim = XDisplayHeight (display_id, screen);

   *xoff = 0;
   *yoff = 0;
#endif
   }

else
   {
/* get virtual display characteristics */

/* - - - - - - - - - - - - - - -   */
/* Inquire & update window file    */
/* - - - - - - - - - - - - - - -   */

/* Get window information from memory file */
   nit = ARGSIZE;
   kwm_xtr (filwnd , wind , &nit , arg);
   if (nit == 0)
         {
      *status = VD_FILKWERR;
      return;
      }
   display_id = (Display *) arg[0];
   w_id = (Window) arg[1];
   screen = XDefaultScreen (display_id);

   istat = GWM_GetPixmap( display_id, w_id, &pix_id);
   istat = XGetGeometry (display_id, pix_id, &root, &x, &y, &width, &height,
                      &border, &depth);
   *xdim = width;
   *ydim = height;
   *xoff = 0;
   *yoff = 0;

   /* - - - - - - - - - - - - - - -   */
   /* Update window file              */
   /* - - - - - - - - - - - - - - -   */

   arg[2] = *xoff;
   arg[3] = *yoff;
   arg[4] = *xdim;
   arg[5] = *ydim;

/* Update window information in memory file */
   nit = 6;
   kwm_upd  (filwnd , wind , &nit , arg);
   if (nit == 0)
      {
      *status = VD_FILKWERR;
      return;
      }

   }

return;
}

/******************************************************************************/

void vdm_mod_x ( char device[], char wind[], int vis, int dxoff, int dyoff,
                 int dxdim, int dydim, int* status )

/*
*+
*  Name:
*     vdm_mod_x
*
*  Purpose:
*     Device dependent display window characteristics modification
*
*  Invocation:
*     vdm_mod_x( device, wind, vis, dxoff, dyoff, dxdim, dydim, status )
*
*  Description:
*     Device dependent display window characteristics modification A / G / I
*     called by : vdm_mod
*
*  Arguments:
*     device = char[]
*        Device type
*     wind = char[]
*        Window name
*     vis = int
*        Window visibility
*     dxoff = int
*        Delta X offset
*     dyoff = int
*        Delta Y offset
*     dxdim = int
*        Delta X display dimension
*     dydim = int
*        Delta Y display dimension
*     status = int
*        Status code
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Get window file records from memory file.
*        Removed redundant code.
*/

{

/* Local variables */
Display            *display_id;
Window             w_id;
Pixmap             pix_id;
XWindowAttributes  info;
Status             istat;
Window             root;
int                x, y;
unsigned int       width, height, border, depth;

int            nit, rne;
int            xoff, yoff;
unsigned int   xdim, ydim;

*status = VD_SUCCESS;

/* Get window information from memory file */
nit = 2;
kwm_xtr (filwnd , wind , &nit , arg);

/* - - - - - - - - - - - - - - - - -  */
/* System dependent modification      */
/* - - - - - - - - - - - - - - - - -  */

/* Get window information from memory file */
nit = ARGSIZE;
kwm_xtr (filwnd , wind , &nit , arg);
if (nit == 0)
   {
   *status = VD_FILKWERR;
   return;
   }

display_id = (Display *) arg[0];
w_id = (Window) arg[1];

istat = GWM_GetPixmap(display_id, w_id,&pix_id);
istat = XGetGeometry (display_id, pix_id, &root, &x, &y, &width, &height,
                      &border, &depth);

xdim = width + dxdim;
ydim = height + dydim;
xoff = dxoff;
yoff = dyoff;

if (vis == 0)
   {
   XUnmapWindow (display_id, w_id);
   XMoveResizeWindow (display_id, w_id, xoff, yoff, xdim, ydim);
   }
else
   {
   XUnmapWindow (display_id, w_id);
   XMoveResizeWindow (display_id, w_id, xoff, yoff, xdim, ydim);
   XMapRaised (display_id, w_id);
   }

/* - - - - - - - - - - - - - - -   */
/* Update window file              */
/* - - - - - - - - - - - - - - -   */

arg[2] += 0;
arg[3] += 0;
arg[4] += dxdim;
arg[5] += dydim;

/* Update window information in memory file */
kwm_upd  (filwnd , wind , &nit , arg);
if (nit == 0)
   {
   *status = VD_FILKWERR;
   return;
   }

return;
}

/******************************************************************************/

void vdm_sav_x ( char device[], char wind[], char bmfile[], int* status )

/*
*+
*  Name:
*     vdm_sav_x
*
*  Purpose:
*     Device dependent display window save as a bitmap
*
*  Invocation:
*     vdm_sav_x ( device, wind, bmfile, status )
*
*  Description:
*     Device dependent display window save as a bitmap G / I
*     called by : vdm_sav
*
*  Arguments:
*     device = char[]
*        Device type
*     wind = char[]
*        Window name
*     bmfile = char[]
*        Save bitmap file name
*     status = int
*        Status code
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Get window file records from memory file.
*        Removed redundant code.
*/

{
/* - - - - - - - - - - - - - - - - -   */
/* System dependent Close              */
/* - - - - - - - - - - - - - - - - -   */
return;
}

