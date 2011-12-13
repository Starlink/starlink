/*
*+
*  Name:
*     IDIEXTRA.C
*
*  Purpose:
*     Pixmap Routines
*
*  Description:
*     These routines manipulate an X-windows pixmap that is used
*     to store the contents of the display memory.
*
*  Contents:
*     attach_pixmap
*        Attach to the GWM pixmap
*     create_pixmap
*        Create a pixmap for the display or memory
*     clear_pixmap
*        Clear the pixmap
*     free_pixmap
*        Release the pixmap resources
*     int_mem_scroll
*        Interactive memory scroll
*     int_dis_scroll
*        Interactive display scroll
*     imagrefr_p
*        Image : Refresh image into pixmap
*     imagrefr_z_p
*        Image : Refresh image into pixmap ( zoom > 1 )
*     imagrefr_uz_p
*        Image : Refresh image into pixmap ( zoom < 0 )
*     polyrefr_p
*        Polyline : Refresh polylines into pixmap
*     textrefr_p
*        Text : Refresh text into pixmap
*     polyline_p
*        Polyline : Refresh polylines into pixmap
*     text_p
*        Text : Refresh text into pixmap
*     refr_p
*        Refresh the pixmap into the display
*     snap_pix
*        Get snapshot from pixmap
*     pack
*        Pack the input bytes into the output bytes.
*     update_memory
*        Update memory description from pixmap
*     update_current_pixmap
*        Update the current memory identifiers in the display structure
*     define_memory
*        Dynamic memory definition
*     update_keys
*        Update key descriptions from X
*
*  Copyright:
*     Copyright (C) 1991, 1992, 1993, 1994 Science & Engineering Research Council.
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*      8-MAY-1991 (NE):
*        Put fontnames in device.dep
*     23-MAY-1991 (NE):
*        Added stdlib.h for free and exit
*     21-JUN-1991 (NE):
*        Moved pixmap_id and gcpix to x11defs.h.
*        Added unistd.h for fork and execl.
*     29-NOV-1991 (NE):
*        Remove exposed and init_refr routines
*     16-APR-1992 (NE):
*        Added update_current_pixmap and define_memory routines.
*        Remove draw_pixmap routine. Added static arg[] array.
*      8-JAN-1993 (NE):
*        Added update_keys
*/

/* System definitions */

#include    <stdlib.h>
#include    <stdio.h>
#include    <string.h>
#include    <X11/Xlib.h>
#include    <X11/Xatom.h>
#include    <X11/keysym.h>
#include    <math.h>
#if HAVE_UNISTD_H
#include    <unistd.h>
#endif

#include    "gwm.h"

/* Package definitions */

#include    "device.dep"
#include    "idi.h"
#include    "idi_err.h"
#include    "iii.h"
#include    "idistruct_e.h"
#include    "kwm.h"
#include    "x11defs.h"
#include    "idifuncs.h"

/* Local definitions */

static int           old_x_pos, old_y_pos, arg[ARGSIZE];

struct bm_window
   {
   int x_coord;
   int y_coord;
   int x_size;
   int y_size;
   };
typedef struct bm_window BM_WINDOW;

/******************************************************************************/

void attach_pixmap ( int display, int memid )

/*
*+
*  Name:
*     attach_pixmap
*
*  Purpose:
*     Attach to the GWM pixmap
*
*  Invocation:
*     attach_pixmap ( display, memid );
*
*  Description:
*     Attach to the pixmap in the current GWM window
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*
*  Algorithm:
*     Attach to the pixmap in the current GWM window
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     23-AUG-1991 (NE):
*        Orignal version
*     27-NOV-1991 (NE):
*        Added plane mask
*     16-APR-1992 (NE):
*        Initialise overlay memory identifier
*/

{

/* Local variables */
int screen;
XGCValues xgcv;
CONF_DATA *conf;
MEM_DATA *mem;

/* Allocate the memory for the pixmap */
display_id = (Display*) device[display].vd_id;
w_id = (Window) device[display].wd_id;
screen = XDefaultScreen( display_id );

/* Get the pixmap from the current GWM window */
GWM_GetPixmap( display_id, w_id, &pixmap_id );

/* Create a graphics context */
xgcv.foreground = XBlackPixel( display_id, screen );
xgcv.background = XBlackPixel( display_id, screen );
gcpix = XCreateGC( display_id, w_id, GCForeground | GCBackground, &xgcv );
XSetGraphicsExposures( display_id, gcpix, 0 );
XSetPlaneMask( display_id, gcpix, device[display].pm_mask );

/* Remember the pixmap details */
conf = device[display].config[device[display].confid];
mem = conf->memory[memid];
mem->pm_id = (int) pixmap_id;
mem->pm_mask = device[display].pm_mask;
device[display].pm_mem = memid;
device[display].pm_memov = -99;

return;
}

/******************************************************************************/

void create_pixmap ( int display, int memid, int confn )

/*
*+
*  Name:
*     create_pixmap
*
*  Purpose:
*     Create pixmap
*
*  Invocation:
*     create_pixmap ( display, memid, confn );
*
*  Description:
*     Create pixmap for the display or memory for use by interactive scroll
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     confn = int
*        Configuration number
*
*  Algorithm:
*     Create pixmap for the display or memory for use by interactive scroll
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*      8-MAY-1991 (NE):
*        Strict X library adherence
*      1-JUL-1991 (NE):
*        Call XFlush to force pixmap creation
*     12-JUL-1991 (NE):
*        Store pixmap ids in device structure
*     28-AUG-1991 (NE):
*        Pass pixmap_id to GWM and alter argument list in clear_pixmap
*     10-SEP-1991 (NE):
*        Add configuration number to argument list
*     11-OCT-1991 (NE):
*        Set Close Down Mode to retain new pixmap after Close Display
*     27-NOV-1991 (NE):
*        Added plane mask
*     16-APR-1992 (NE):
*        Initialise overlay memory identifier
*     22-MAY-1992 (NE):
*        Set conf number before calling clear_pixmap and remove GWM_SetPixmap
*/

{

/* Local variables */
int i;
int lmemid, lmemov;
int status;
unsigned int xdim;
unsigned int ydim;
unsigned int dpth;
unsigned long mask;
int screen;
int tempcn;
CONF_DATA *conf;
MEM_DATA *mem;
XGCValues xgcv;
Window root;
int x, y;
unsigned int width, height, bw;

/* Use a local memory identifier as it may get updated */
lmemid = memid;
conf = device[display].config[confn];

/* If memid is undefined use the last visible memory */
if ( lmemid == -1 )
   {
   for ( i = 0; i < conf->n_mem; i++ )
      {
      mem = conf->memory[i];
      if ( mem->visibility == 1 )
         lmemid = i;
      }
   }

/* Inquire current memory size */
mem = conf->memory[lmemid];
xdim = mem->x_size;
ydim = mem->y_size;

/* Have to set the Close Down Mode to stop the pixmap disappearing */
/* when the IDI image exits, so that GWM can hold onto it */
XSetCloseDownMode( display_id, RetainTemporary );

/* Allocate the memory for the pixmap */
display_id = (Display*) device[display].vd_id;
w_id = (Window) device[display].wd_id;
screen = XDefaultScreen( display_id );
XGetGeometry( display_id, w_id, &root, &x, &y, &width, &height, &bw, &dpth);
pixmap_id = XCreatePixmap( display_id, w_id, xdim, ydim, dpth );

/* Create a graphics context */
xgcv.foreground = XBlackPixel( display_id, screen );
xgcv.background = XBlackPixel( display_id, screen );
gcpix = XCreateGC( display_id, w_id, GCForeground | GCBackground, &xgcv );
XSetGraphicsExposures( display_id, gcpix, 0 );
XSetPlaneMask( display_id, gcpix, device[display].pm_mask );

/* Remember the pixmap details */
mem->pm_id = (int) pixmap_id;

/* See if there is an overlay plane */
GWM_GetOvMask( display_id, w_id, &mask );
if ( ~mask > 0 )
   {

/* Define the overlay memory to be depth = 1 and type = 7 */
   status = define_memory( display, xdim, ydim, 1, 7, &lmemov );
   mmbm_all( display, confn, lmemov, &status );
   mem = conf->memory[lmemov];
   mem->pm_id = (int) pixmap_id;

/* Remember the overlay memory id */
   conf->memid = lmemov;
   }

/* Fill the pixmap with the background colour */
/* Fix the configuration number before the call to clear_pixmap */
tempcn = device[display].confid;
device[display].confid = confn;
clear_pixmap( display, lmemid, 0, AllPlanes );

/* Reset the current configuration number */
device[display].confid = tempcn;

return;
}

/******************************************************************************/

void clear_pixmap ( int display, int memid, int bck,
                    unsigned long plane_mask )

/*
*+
*  Name:
*     clear_pixmap
*
*  Purpose:
*     Clear_pixmap
*
*  Invocation:
*     clear_pixmap ( display, memid, bck, plane_mask );
*
*  Description:
*     Clear the pixmap
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     bck = int
*        Background fill colour index
*     plane_mask = unsigned long
*        Plane mask
*
*  Algorithm:
*     Clear the pixmap
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*      8-MAY-1991 (NE):
*        Strict X library adherence
*     12-JUL-1991 (NE):
*        Get pixmap ids from device structure
*     28-AUG-1991 (NE):
*        Added bck to argument list
*     16-APR-1992 (NE):
*        Added plane_mask to argument list
*        Get pixmap id from memory structure
*     11-NOV-1992 (NE):
*        Dereference colour index through lut as well as curlut
*/

{

/* Local variables */
int curconf;
int i;
int lmemid;
unsigned int xdim;
unsigned int ydim;
CONF_DATA *conf;
MEM_DATA *mem;
LUT_DATA *lut;

/* Use a local memory identifier as it may get updated */
lmemid = memid;
curconf = device[display].confid;
conf = device[display].config[curconf];

/* If memid is undefined use the last visible memory */
if ( lmemid == -1 )
   {
   for ( i = 0; i < conf->n_mem; i++ )
      {
      mem = conf->memory[i];
      if ( mem->visibility == 1 )
         lmemid = i;
      }
   }

/* Inquire current memory size */
mem = conf->memory[lmemid];
xdim = mem->x_size;
ydim = mem->y_size;
lut = device[display].lookup[mem->lut_id];

/* Inquire the foreground and background colours */
display_id = (Display*) device[display].vd_id;
pixmap_id = (Pixmap) mem->pm_id;

/* Fill the pixmap with the background colour */
XSetPlaneMask( display_id, gcpix, plane_mask );
XSetForeground( display_id, gcpix, curlut.lutpix[lut->lutpix[bck]] );
XFillRectangle( display_id, pixmap_id, gcpix, 0, 0, xdim, ydim );
XFlush( display_id );

return;
}

/******************************************************************************/

void free_pixmap ( int display, int confid, int memid )

/*
*+
*  Name:
*     free_pixmap
*
*  Purpose:
*     Free pixmap
*
*  Invocation:
*     free_pixmap ( display, confid, memid );
*
*  Description:
*     Release the resources of the pixmap
*
*  Arguments:
*     display = int
*        Display identifier
*     confid = int
*        Configuration identifier
*     memid = int
*        Memory identifier
*
*  Algorithm:
*     Release the resources of the pixmap
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink, RAL)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*      8-MAY-1991 (NE):
*        Strict X library adherence
*     12-JUL-1991 (NE):
*        Get pixmap ids from device structure
*     27-MAY-1992 (NE):
*        Add confid and memid to argument list
*     11-MAR-1994 (DLT):
*        Free GC as well as pixmap
*/

{
CONF_DATA *conf;
MEM_DATA *mem;

/* Check the pixmap is valid */
if ( memid >= 0 )
   {

/* Release the memory for the pixmap */
   display_id = (Display*) device[display].vd_id;

/* Get the pixmap_id from the memory structure */
   conf = device[display].config[confid];
   mem = conf->memory[memid];
   pixmap_id  = (Pixmap) mem->pm_id;

/* Free the cg */
   XFreeGC( display_id, gcpix );

/* Free the resources */
   XFreePixmap( display_id, pixmap_id );
   }

return;
}

/******************************************************************************/

void int_mem_scroll ( int display, int nint, short ev_type, short ev_data,
                      short pos[], int ew, int* err )

/*
*+
*  Name:
*     int_mem_scroll
*
*  Purpose:
*     Interactive memory scroll
*
*  Invocation:
*     int_mem_scroll( display, nint, ev_type, ev_data, pos, ew, err );
*
*  Description:
*     Interactive memory scroll
*
*  Arguments:
*     display = int
*        Display identifier
*     nint = int
*        Interaction number
*     ev_type = short
*        Event type
*     ev_data = short
*        Event data
*     pos = short[]
*        Position of locator
*     ew = int
*        Enter window flag
*     err = int
*        Pointer to status
*
*  Algorithm:
*     The scrolling is achieved by first copying as much of the pixmap
*     that is still visible into its new position and then filling in the
*     exposed areas with data from the internal memory. The exposed area
*     comprises two rectangles which are defined from the following four
*     possible arrangements:-
*         000000    000000    1.....    .....1
*         1.....    .....1    1.....    .....1
*         1.....    .....1    000000    000000
*     Here 0's represent the first rectangular area which stretches the
*     full width of the window, 1's represent the remaining area and .'s
*     represent the part of the old pixmap copied to the new position.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Original version
*     18-DEC-1992 (NE):
*        Rewrite to allow scrolling while zoomed
*      5-JAN-1993 (NE):
*        Set device[].curs_flag and .roi_flag before grefr
*     17-FEB-1993 (NE):
*        Correct the y-inversion
*/

{

/* Local variables */
unsigned char  *curbm, *curbm0;
unsigned char  *filbm, *filbm0;
unsigned char  *pacbm, *pacbm0 = NULL;
XImage         *ima;
Visual         *visual;
int curconf;
int loc0;
int interactor_id;
int loc_id;
unsigned int dpth;
int pfact;
int memid;
int f0;
int fsize, psize;
unsigned char * maxmem;
int bck;
int i, j, k, n;
int x_dst, x_src, y_dst, y_src;
int x_off, y_off;
int xd, ix0, jx0;
int zoom;
float mem_x_old;
float mem_y_old;
float x0 = 0.0, y0 = 0.0, yy;
CONF_DATA *conf;
MEM_DATA *mem;
INTER_DATA *intdata;
INT_DEV_DATA *intdev;
LOC_DATA *loc;
LUT_DATA *lut;
BM_WINDOW bm[2];
Window root;
int x, y;
unsigned int width, height, bw;

/* Reset status on entry */
*err = II_SUCCESS;

/* Get the window identifiers from the global variables */
display_id = (Display*) device[display].vd_id;
w_id = (Window) device[display].wd_id;

/* Get the window descriptors from X */
screen = XDefaultScreen( display_id );
visual = VisualOfWindow( display );
XGetGeometry( display_id, w_id, &root, &x, &y, &width, &height, &bw, &dpth);
pfact = 8 / dpth;

/* Get the interaction descriptors from the global variables */
intdata = device[display].inter[nint];
memid = intdata->obj_id;
curconf = device[display].confid;
conf = device[display].config[curconf];
mem = conf->memory[memid];

/* Get the locator descriptor for this interaction */
loc0 = intdata->int_id;
get_loc( loc0, &interactor_id, &loc_id );
intdev = int_struct.int_dev[interactor_id];
loc = intdev->loc[loc_id];

/* Reset the locator coordinates if the pointer has entered the window */
if ( ( ew == 1 ) || ( int_scroll_reset[display] == 1 ) )
   {
   loc->x_pos = pos[0];
   loc->y_pos = device[display].dev_ysiz - 1 - pos[1];
   old_x_pos = loc->x_pos;
   old_y_pos = loc->y_pos;
   int_scroll_reset[display] = 0;
   }

/* Check that this locator has an event pending */
zoom = mem->zoom;
test_loc( display, zoom, ev_type, ev_data, pos, interactor_id, loc_id, &f0 );

if ( f0 == 1 )
   {

/* return if there is a pending motion event */
   if ( is_motion_pending( display, loc->x_pos, loc->y_pos ) ) return;

/* undisplay any cursors or ROIs, except for those in the window */
   device[display].curs_flag = 99;
   device[display].roi_flag = 99;
   grefr( display, err );

/* Get some more values from the global variables */
   pixmap_id = (Pixmap) mem->pm_id;
   lut = device[display].lookup[mem->lut_id];
   bck = curlut.lutpix[0];

/* update scroll factors */
   x_off = loc->x_pos - old_x_pos;
   y_off = loc->y_pos - old_y_pos;
   mem_x_old = mem->zoom_xsc;
   mem_y_old = mem->zoom_ysc;
   mem->x_scroll += (float)x_off / (float)mem->zoom;
   mem->y_scroll += (float)y_off / (float)mem->zoom;
   mem->zoom_xsc_new += x_off;
   mem->zoom_ysc_new += y_off;
   mem->zoom_xsc += x_off;
   mem->zoom_ysc += y_off;
   old_x_pos = loc->x_pos;
   old_y_pos = loc->y_pos;

/* calculate new position of pixmap */
   x_src = ( mem->zoom_xsc > mem_x_old ) ? 0 : (int)(mem_x_old - mem->zoom_xsc);
   y_src = ( mem->zoom_ysc < mem_y_old ) ? 0 : (int)(mem->zoom_ysc - mem_y_old);
   x_dst = ( mem->zoom_xsc < mem_x_old ) ? 0 : (int)(mem->zoom_xsc - mem_x_old);
   y_dst = ( mem->zoom_ysc > mem_y_old ) ? 0 : (int)(mem_y_old - mem->zoom_ysc);
   width = mem->x_size - ( x_src + x_dst );
   height = mem->y_size - ( y_src + y_dst );

/* copy the pixmap to the new position */
   XSetPlaneMask( display_id, gcpix, mem->pm_mask );
   XCopyArea( display_id, pixmap_id, pixmap_id, gcpix, x_src, y_src,
              width, height, x_dst, y_dst );

/* calculate the parts of the old pixmap that need to be refreshed */
   bm[0].x_coord = 0;
   bm[0].y_coord = ( y_src == 0 ) ? 0 : height;
   bm[0].x_size = mem->x_size;
   bm[0].y_size = y_src + y_dst;
   bm[1].x_coord = ( x_src == 0 ) ? 0 : width;
   bm[1].y_coord = ( y_src == 0 ) ? y_dst : 0;
   bm[1].x_size = x_src + x_dst;
   bm[1].y_size = height;

/* update the visual memory offsets */
   zoom = mem->zoom * device[display].zoom;
   if ( bm[1].x_coord == 0 )
      mem->x_v_off -= (float)bm[1].x_size / (float)zoom;
   else
      mem->x_v_off += (float)bm[1].x_size / (float)zoom;
   if ( bm[0].y_coord == 0 )
      mem->y_v_off += (float)bm[0].y_size / (float)zoom;
   else
      mem->y_v_off -= (float)bm[0].y_size / (float)zoom;

/* loop through the two exposed areas */
   for ( n = 0; n < 2; n++ )
      {

/* fill in the areas revealed by the scroll */
      fsize = bm[n].x_size * bm[n].y_size;
      if ( fsize == 0 ) continue;
      filbm0 = (unsigned char *) malloc( fsize );
      filbm = filbm0;

/* Calculate the origin of the fill rectangle */
      switch( n )
         {
         case 0:
         x0 = mem->x_v_off;
         y0 = mem->y_v_off + (float)( height - bm[0].y_coord ) / (float)zoom;
         break;

         case 1:
         x0 = mem->x_v_off + (float)bm[1].x_coord / (float)zoom;
         y0 = mem->y_v_off + (float)(bm[0].y_size-bm[1].y_coord) / (float)zoom;
         break;
         }

/* Calculate the (zoomed) fraction of a pixel starting a line */
      ix0 = (int)x0;
      xd = (int)( ( x0 - ix0 ) * (float)zoom );
      maxmem = mem->mmbm + mem->x_size * mem->y_size;

/* Resample the image */
      for (i = bm[n].y_size; i > 0; i--)
         {
         yy = (float)i / (float)zoom;
         curbm0 = (unsigned char *)( mem->mmbm + ( mem->y_size - 1 -
                  (int)( y0 + yy ) ) * mem->x_size + ix0 );
         curbm = curbm0;
         k = xd;
         for (j = 0; j < bm[n].x_size; j++)
            {
            jx0 = ix0 + j / zoom;
            *filbm++ = ( ( mem->mmbm <= curbm ) &&
                       (  curbm < maxmem ) &&
                       ( jx0 >= 0 ) && ( jx0 < mem->x_size ) ) ? *curbm : bck;
            if ( ++k >= zoom )
               {
               k = 0;
               curbm++;
               }
            }
         }

/* If the packing factor is one then use the data as supplied */
      if ( pfact == 1 )
         {
         pacbm = filbm0;
         psize = 0;
         }

/* Otherwise pack the data into bytes */
      else
         {
         psize = ( bm[n].x_size / pfact + 1 ) * bm[n].y_size;
         pacbm0 = (unsigned char *) malloc( psize );
         pacbm = pacbm0;
         pack( filbm0, pacbm, pfact, bm[n].x_size, bm[n].y_size );
         }

/* Draw the image data into the global pixmap */
      ima = XCreateImage( display_id, visual, dpth, ZPixmap,
                          0, (char*)pacbm, bm[n].x_size, bm[n].y_size, 8, 0 );
      XSetPlaneMask( display_id, gcpix, mem->pm_mask );
      XPutImage( display_id, pixmap_id, gcpix, ima, 0, 0,
                 bm[n].x_coord, bm[n].y_coord, ima->width, ima->height) ;

/* Release the bitmap memory */
      XFree( (char *)ima );
      free (filbm0);
      if ( psize > 0 ) free( pacbm0 );
      }

/* get the scroll offsets as properties of the window */
   if ( memid == device[display].pm_memov )
      GWM_GetOvScroll( display_id, w_id, &x_off, &y_off );
   else
      GWM_GetScroll( display_id, w_id, &x_off, &y_off );

/* copy the pixmap to the new position */
   XCopyArea( display_id, pixmap_id, w_id, gcpix, 0, 0,
              mem->x_size, mem->y_size, x_off, y_off );
   XFlush( display_id );

/* refresh any cursors or ROIs */
   device[display].curs_flag = memid;
   device[display].roi_flag = memid;
   grefr( display, err );
   device[display].curs_flag = -1;
   device[display].roi_flag = -1;
   }

/* reset the locator position if the scroll limit was exceeded */
else
   {
   if ( intdata->int_type == II_LOC && intdev->descr == II_MOUSE )
      {
      loc->x_pos = pos[0];
      loc->y_pos = device[display].dev_ysiz - 1 - pos[1];
      old_x_pos = loc->x_pos;
      old_y_pos = loc->y_pos;
      }
   }

return;
}

/******************************************************************************/

void int_dis_scroll ( int display, int nint, short ev_type, short ev_data,
                      short pos[], int ew, int* err )

/*
*+
*  Name:
*     int_dis_scroll
*
*  Purpose:
*     Interactive display scroll
*
*  Invocation:
*     int_dis_scroll( display, nint, ev_type, ev_data, pos, ew, err );
*
*  Description:
*     Interactive display scroll
*
*  Arguments:
*     display = int
*        Display identifier
*     nint = int
*        Interaction number
*     ev_type = short
*        Event type
*     ev_data = short
*        Event data
*     pos = short[]
*        Position of locator
*     ew = int
*        Enter window flag
*     err = int
*        Pointer to status
*
*  Algorithm:
*     The scrolling is achieved by first copying as much of the pixmap
*     that is still visible into its new position and then filling in the
*     exposed areas with data from the internal memory. The exposed area
*     comprises two rectangles which are defined from the following four
*     possible arrangements:-
*         000000    000000    1.....    .....1
*         1.....    .....1    1.....    .....1
*         1.....    .....1    000000    000000
*     Here 0's represent the first rectangular area which stretches the
*     full width of the window, 1's represent the remaining area and .'s
*     represent the part of the old pixmap copied to the new position.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*      7-MAR-1991 (NE):
*        Original version
*     18-DEC-1992 (NE):
*        Rewrite to allow scrolling while zoomed
*      5-JAN-1993 (NE):
*        Set device[].curs_flag and .roi_flag before grefr
*     17-FEB-1993 (NE):
*        Correct the y-inversion
*/

{

/* Local variables */
unsigned char  *curbm, *curbm0;
unsigned char  *filbm, *filbm0;
unsigned char  *pacbm, *pacbm0 = NULL;
XImage         *ima;
Visual         *visual;
int curconf;
int loc0;
int interactor_id;
int loc_id;
unsigned int dpth;
int pfact;
int memid = 0;
int f0;
int fsize, psize;
unsigned char * maxmem;
int bck;
int i, j, k, n;
int x_dst, x_src, y_dst, y_src;
int x_off, y_off;
int ydiff;
int xd, ix0, jx0;
int zoom;
float dis_x_old;
float dis_y_old;
float dis_x_new;
float dis_y_new;
float x0 = 0.0, y0 = 0.0, yy;
CONF_DATA *conf;
MEM_DATA *mem;
INTER_DATA *intdata;
INT_DEV_DATA *intdev;
LOC_DATA *loc;
LUT_DATA *lut;
BM_WINDOW bm[2];
Window root;
int x, y;
unsigned int width, height, bw;

/* Reset status on entry */
*err = II_SUCCESS;

/* Get the window identifiers from the global variables */
display_id = (Display*) device[display].vd_id;
w_id = (Window) device[display].wd_id;

/* Get the window descriptors from X */
screen = XDefaultScreen( display_id );
visual = VisualOfWindow( display );
XGetGeometry( display_id, w_id, &root, &x, &y, &width, &height, &bw, &dpth);
pfact = 8 / dpth;

/* Get the interaction descriptors from the global variables */
intdata = device[display].inter[nint];
curconf = device[display].confid;
conf = device[display].config[curconf];

/* To draw the display use the last visible memory */
for ( i = 0; i < conf->n_mem; i++ )
   {
   mem = conf->memory[i];
   if ( mem->visibility == 1 )
      memid = i;
   }

/* Update the current memory if the identifier does not match the pixmap */
if ( ( memid != device[display].pm_mem ) &&
     ( memid != device[display].pm_memov ) )
   {
   update_current_pixmap( display, memid );
   refr_p( display );
   }

/* Get the locator descriptor for this interaction */
loc0 = intdata->int_id;
get_loc( loc0, &interactor_id, &loc_id );
intdev = int_struct.int_dev[interactor_id];
loc = intdev->loc[loc_id];

/* Reset the locator coordinates if the pointer has entered the window */
if ( ( ew == 1 ) || ( int_scroll_reset[display] == 1 ) )
   {
   loc->x_pos = pos[0];
   loc->y_pos = device[display].dev_ysiz - 1 - pos[1];
   old_x_pos = loc->x_pos;
   old_y_pos = loc->y_pos;
   int_scroll_reset[display] = 0;
   }

/* Check that this locator has an event pending */
zoom = device[display].zoom;
test_loc( display, zoom, ev_type, ev_data, pos, interactor_id, loc_id, &f0 );

if ( f0 == 1 )
   {

/* return if there is a pending motion event */
   if ( is_motion_pending( display, loc->x_pos, loc->y_pos ) ) return;

/* undisplay any cursors or ROIs, except for those in the window */
   device[display].curs_flag = 99;
   device[display].roi_flag = 99;
   grefr( display, err );

/* update scroll factors */
   x_off = loc->x_pos - old_x_pos;
   y_off = loc->y_pos - old_y_pos;
   dis_x_old = device[display].x_scroll;
   dis_y_old = device[display].y_scroll;
   dis_x_new = dis_x_old + (float)x_off;
   dis_y_new = dis_y_old + (float)y_off;
   device[display].x_scroll += (float)x_off / (float)device[display].zoom;
   device[display].y_scroll += (float)y_off / (float)device[display].zoom;
   old_x_pos = loc->x_pos;
   old_y_pos = loc->y_pos;

/* calculate new position of pixmap */
   x_src = ( dis_x_new > dis_x_old ) ? 0 : (int)(dis_x_old - dis_x_new);
   y_src = ( dis_y_new < dis_y_old ) ? 0 : (int)(dis_y_new - dis_y_old);
   x_dst = ( dis_x_new < dis_x_old ) ? 0 : (int)(dis_x_new - dis_x_old);
   y_dst = ( dis_y_new > dis_y_old ) ? 0 : (int)(dis_y_old - dis_y_new);

/* deal with the base memory first */
   memid = device[display].pm_mem;
   mem = conf->memory[memid];
   pixmap_id = (Pixmap) mem->pm_id;
   lut = device[display].lookup[mem->lut_id];
   bck = curlut.lutpix[0];
   width = mem->x_size - ( x_src + x_dst );
   height = mem->y_size - ( y_src + y_dst );

/* copy the pixmap to the new position */
/* Locate the memory structure */
   XSetPlaneMask( display_id, gcpix, mem->pm_mask );
   XCopyArea( display_id, pixmap_id, pixmap_id, gcpix, x_src, y_src,
              width, height, x_dst, y_dst );

/* calculate the parts of the old pixmap that need to be refreshed */
   bm[0].x_coord = 0;
   bm[0].y_coord = ( y_src == 0 ) ? 0 : height;
   bm[0].x_size = mem->x_size;
   bm[0].y_size = y_src + y_dst;
   bm[1].x_coord = ( x_src == 0 ) ? 0 : width;
   bm[1].y_coord = ( y_src == 0 ) ? y_dst : 0;
   bm[1].x_size = x_src + x_dst;
   bm[1].y_size = height;

/* update the visual memory offsets */
   zoom = mem->zoom * device[display].zoom;
   if ( bm[1].x_coord == 0 )
      mem->x_v_off -= (float)bm[1].x_size / (float)zoom;
   else
      mem->x_v_off += (float)bm[1].x_size / (float)zoom;
   if ( bm[0].y_coord == 0 )
      mem->y_v_off += (float)bm[0].y_size / (float)zoom;
   else
      mem->y_v_off -= (float)bm[0].y_size / (float)zoom;

/* loop through the two exposed areas */
   for ( n = 0; n < 2; n++ )
      {

/* fill in the areas revealed by the scroll */
      fsize = bm[n].x_size * bm[n].y_size;
      if ( fsize == 0 ) continue;
      filbm0 = (unsigned char *) malloc( fsize );
      filbm = filbm0;

/* Calculate the origin of the fill rectangle */
      switch( n )
         {
         case 0:
         x0 = mem->x_v_off;
         y0 = mem->y_v_off + (float)( height - bm[0].y_coord ) / (float)zoom;
         break;

         case 1:
         x0 = mem->x_v_off + (float)bm[1].x_coord / (float)zoom;
         y0 = mem->y_v_off + (float)(bm[0].y_size-bm[1].y_coord) / (float)zoom;
         break;
         }

/* Calculate the (zoomed) fraction of a pixel starting a line */
      ix0 = (int)x0;
      xd = (int)( ( x0 - ix0 ) * (float)zoom );
      maxmem = mem->mmbm + mem->x_size * mem->y_size;

/* Resample the image */
      for (i = bm[n].y_size; i > 0; i--)
         {
         yy = (float)i / (float)zoom;
         curbm0 = (unsigned char *)( mem->mmbm + ( mem->y_size - 1 -
                  (int)( y0 + yy ) ) * mem->x_size + ix0 );
         curbm = curbm0;
         k = xd;
         for (j = 0; j < bm[n].x_size; j++)
            {
            jx0 = ix0 + j / zoom;
            *filbm++ = ( ( mem->mmbm <= curbm ) &&
                       ( curbm < maxmem ) &&
                       ( jx0 >= 0 ) && ( jx0 < mem->x_size ) ) ? *curbm : bck;
            if ( ++k >= zoom )
               {
               k = 0;
               curbm++;
               }
            }
         }

/* If the packing factor is one then use the data as supplied */
      if ( pfact == 1 )
         {
         pacbm = filbm0;
         psize = 0;
         }

/* Otherwise pack the data into bytes */
      else
         {
         psize = ( bm[n].x_size / pfact + 1 ) * bm[n].y_size;
         pacbm0 = (unsigned char *) malloc( psize );
         pacbm = pacbm0;
         pack( filbm0, pacbm, pfact, bm[n].x_size, bm[n].y_size );
         }

/* Draw the image data into the global pixmap */
      ima = XCreateImage( display_id, visual, dpth, ZPixmap,
                          0, (char*)pacbm, bm[n].x_size, bm[n].y_size, 8, 0 );
      XSetPlaneMask( display_id, gcpix, mem->pm_mask );
      XPutImage( display_id, pixmap_id, gcpix, ima, 0, 0,
                 bm[n].x_coord, bm[n].y_coord, ima->width, ima->height) ;

/* Release the bitmap memory */
      XFree( (char *)ima );
      free (filbm0);
      if ( psize > 0 ) free( pacbm0 );
      }

/* get the scroll offsets as properties of the window */
   GWM_GetScroll( display_id, w_id, &x_off, &y_off );

/* copy the pixmap to the new position */
   XCopyArea( display_id, pixmap_id, w_id, gcpix, 0, 0,
              mem->x_size, mem->y_size, x_off, y_off );

/* deal with the overlay memory */
   if ( device[display].overlay )
      {
      memid = device[display].pm_memov;
      mem = conf->memory[memid];
      pixmap_id = (Pixmap) mem->pm_id;
      lut = device[display].lookup[mem->lut_id];
      bck = curlut.lutpix[0];
      width = mem->x_size - ( x_src + x_dst );
      height = mem->y_size - ( y_src + y_dst );

/* copy the pixmap to the new position */
/* Locate the memory structure */
      XSetPlaneMask( display_id, gcpix, mem->pm_mask );
      XCopyArea( display_id, pixmap_id, pixmap_id, gcpix, x_src, y_src,
                 width, height, x_dst, y_dst );

/* calculate the parts of the old pixmap that need to be refreshed */
      bm[0].x_coord = 0;
      bm[0].y_coord = ( y_src == 0 ) ? 0 : height;
      bm[0].x_size = mem->x_size;
      bm[0].y_size = y_src + y_dst;
      bm[1].x_coord = ( x_src == 0 ) ? 0 : width;
      bm[1].y_coord = ( y_src == 0 ) ? y_dst : 0;
      bm[1].x_size = x_src + x_dst;
      bm[1].y_size = height;

/* update the visual memory offsets */
      zoom = mem->zoom * device[display].zoom;
      if ( bm[1].x_coord == 0 )
         mem->x_v_off -= (float)bm[1].x_size / (float)zoom;
      else
         mem->x_v_off += (float)bm[1].x_size / (float)zoom;
      if ( bm[0].y_coord == 0 )
         mem->y_v_off += (float)bm[0].y_size / (float)zoom;
      else
         mem->y_v_off -= (float)bm[0].y_size / (float)zoom;

/* loop through the two exposed areas */
      for ( n = 0; n < 2; n++ )
         {

/* fill in the areas revealed by the scroll */
         fsize = bm[n].x_size * bm[n].y_size;
         if ( fsize == 0 ) continue;
         filbm0 = (unsigned char *) malloc( fsize );
         filbm = filbm0;

/* Calculate the origin of the fill rectangle */
         switch( n )
            {
            case 0:
            x0 = mem->x_v_off;
            y0 = mem->y_v_off + (float)( height - bm[0].y_coord ) / (float)zoom;
            break;

            case 1:
            x0 = mem->x_v_off + (float)bm[1].x_coord / (float)zoom;
            y0 = mem->y_v_off + (float)(bm[0].y_size-bm[1].y_coord)/(float)zoom;
            break;
            }

/* Calculate the (zoomed) fraction of a pixel starting a line */
         ix0 = (int)x0;
         xd = (int)( ( x0 - ix0 ) * (float)zoom );
         maxmem = mem->mmbm + mem->x_size * mem->y_size;

/* Resample the image */
         for (i = bm[n].y_size; i > 0; i--)
            {
            yy = (float)i / (float)zoom;
            curbm0 = (unsigned char *)( mem->mmbm + ( mem->y_size - 1 -
                     (int)( y0 + yy ) ) * mem->x_size + ix0 );
            curbm = curbm0;
            k = xd;
            for (j = 0; j < bm[n].x_size; j++)
               {
               jx0 = ix0 + j / zoom;
               *filbm++ = ( ( mem->mmbm <= curbm ) &&
                            ( curbm < maxmem ) &&
                         ( jx0 >= 0 ) && ( jx0 < mem->x_size ) ) ? *curbm : bck;
               if ( ++k >= zoom )
                  {
                  k = 0;
                  curbm++;
                  }
               }
            }

/* If the packing factor is one then use the data as supplied */
         if ( pfact == 1 )
            {
            pacbm = filbm0;
            psize = 0;
            }

/* Otherwise pack the data into bytes */
         else
            {
            psize = ( bm[n].x_size / pfact + 1 ) * bm[n].y_size;
            pacbm0 = (unsigned char *) malloc( psize );
            pacbm = pacbm0;
            pack( filbm0, pacbm, pfact, bm[n].x_size, bm[n].y_size );
            }

/* Draw the image data into the global pixmap */
         ima = XCreateImage( display_id, visual, dpth, ZPixmap,
                            0, (char*)pacbm, bm[n].x_size, bm[n].y_size, 8, 0 );
         XSetPlaneMask( display_id, gcpix, mem->pm_mask );
         XPutImage( display_id, pixmap_id, gcpix, ima, 0, 0,
                    bm[n].x_coord, bm[n].y_coord, ima->width, ima->height) ;

/* Release the bitmap memory */
         XFree( (char *)ima );
         free (filbm0);
         if ( psize > 0 ) free( pacbm0 );
         }

/* get the scroll offsets as properties of the window */
      GWM_GetOvScroll( display_id, w_id, &x_off, &y_off );

/* copy the pixmap to the new position */
      XCopyArea( display_id, pixmap_id, w_id, gcpix, 0, 0,
                 mem->x_size, mem->y_size, x_off, y_off );
      XFlush( display_id );
      }

/* refresh any cursors or ROIs */
   device[display].curs_flag = -1;
   device[display].roi_flag = -1;
   grefr( display, err );
   }

/* reset the locator position if the scroll limit was exceeded */
else
   {
   if ( intdata->int_type == II_LOC && intdev->descr == II_MOUSE )
      {
      loc->x_pos = pos[0];
      loc->y_pos = device[display].dev_ysiz - 1 - pos[1];
      old_x_pos = loc->x_pos;
      old_y_pos = loc->y_pos;
      }
   }

return;
}

/******************************************************************************/

void imagrefr_p ( int display, int memid )

/*
*+
*  Name:
*     imagrefr_p
*
*  Purpose:
*     Refresh image into pixmap
*
*  Invocation:
*     imagrefr_p ( display, memid );
*
*  Description:
*     Draw images into the pixmap.
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*
*  Algorithm:
*     Create an image from the data.
*     Draw that image into the pixmap.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*     24-APR-1991 (NE):
*        Pack data if display less than 8 bits deep
*      8-MAY-1991 (NE):
*        Strict X library adherence
*     12-JUL-1991 (NE):
*        Get pixmap ids from device structure
*     16-APR-1992 (NE):
*        Get pixmap id from memory structure
*     18-DEC-1992 (NE):
*        Position pixmap according to scrolls and free image resources
*      3-MAR-1998 (DLT):
*        Remove scrolls from writing to pixmap
*/

{

/* Local variables */
unsigned char  *curbm;
unsigned char  *pacbm;
unsigned char  *pacbm0 = NULL;
CONF_DATA      *conf;
MEM_DATA       *mem;
XImage         *image;
Visual         *visual;
int            curconf;
int            i;
int            lmemid;
int            pfact;
int            psize;
int            screen;
int            x_dst, y_dst;
Window         root;
int            x,y;
unsigned int   width, height, bw, dpth;

/* Use a local memory identifier as it may get updated */
lmemid = memid;
curconf = device[display].confid;
conf = device[display].config[curconf];

/* If memid is undefined use the last visible memory */
if ( lmemid == -1 )
   {
   for ( i = 0; i < conf->n_mem; i++ )
      {
      mem = conf->memory[i];
      if ( mem->visibility == 1 )
         lmemid = i;
      }
   }

/* Get the window identifiers from the global variables */
display_id = (Display*) device[display].vd_id;

/* Get the window descriptors from X */
screen = XDefaultScreen( display_id );
visual = VisualOfWindow( display );

/* Get the location of the data from the global variables */
mem = conf->memory[lmemid];
pixmap_id = (Pixmap) mem->pm_id;
curbm = (unsigned char*)( mem->mmbm );

/* Get the depth of the pixmap */
XGetGeometry( display_id, pixmap_id, &root, &x, &y, &width, &height, &bw,
    &dpth);
pfact = 8 /dpth;

/* Draw the image data into the global pixmap */
if ((mem->mem_free == 0) && ((mem->type & II_IMAGE) > 0))
   {

/* If the packing factor is one then use the data as supplied */
   if ( pfact == 1 )
      {
      pacbm = curbm;
      psize = 0;
      }

/* Otherwise pack the data into bytes */
   else
      {
      psize = ( mem->x_size / pfact + 1 ) * mem->y_size;
      pacbm0 = (unsigned char *) malloc( psize );
      pacbm = pacbm0;
      pack( curbm, pacbm, pfact, mem->x_size, mem->y_size );
      }

/* Display the image */
   image = XCreateImage( display_id, visual, dpth, ZPixmap, 0, (char*)pacbm,
                         mem->x_size, mem->y_size, 8, 0 );
   XSetPlaneMask( display_id, gcpix, mem->pm_mask );
   x_dst = (int)(device[display].x_scroll);
   y_dst = -(int)(device[display].y_scroll);
   XPutImage( display_id, pixmap_id, gcpix, image, 0, 0, x_dst, y_dst,
              image->width, image->height );
   XFlush(display_id);

/* Free the temporary workspace */
   XFree( (char *)image );
   if ( psize > 0 )
      free( pacbm0 );
   }

return;
}

/******************************************************************************/

void imagrefr_z_p ( int display, int memid, int zoom, float x0, float y0,
                    float xs0, float ys0, float x1, float y1, float xs1,
                    float ys1 )

/*
*+
*  Name:
*     imagrefr_z_p
*
*  Purpose:
*     Refresh zoomed image into pixmap ( zoom > 1 )
*
*  Invocation:
*     imagrefr_z_p( display, memid, zoom, x0, y0, xs0, ys0, x1, y1, xs1, ys1 )
*
*  Description:
*     Resample images and draw into the pixmap.
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     zoom = int
*        Zoom factor
*     x0 = float
*        Origin coordinates for main memory bitmap (source)
*     y0 = float
*        Origin coordinates for main memory bitmap (source)
*     xs0 = float
*        Sub-image main memory bitmap dimensions   (source)
*     ys0 = float
*        Sub-image main memory bitmap dimensions   (source)
*     x1 = float
*        Origin coordinates for display bitmap     (target)
*     y1 = float
*        Origin coordinates for display bitmap     (target)
*     xs1 = float
*        Sub-image display bitmap dimensions       (target)
*     ys1 = float
*        Sub-image display bitmap dimensions       (target)
*
*  Algorithm:
*     Create an image from the data.
*     Draw that image into the pixmap.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*     24-APR-1991 (NE):
*        Pack data if display less than 8 bits deep
*      8-MAY-1991 (NE):
*        Strict X library adherence
*     27-JUN-1991 (NE):
*        Allow memory and display to be different sizes
*     12-JUL-1991 (NE):
*        Get pixmap ids from device structure
*     16-APR-1992 (NE):
*        Get pixmap id from memory structure
*     16-OCT-1992 (NE):
*        Change pixel arguments to real to allow for partial pixels
*/

{

/* Local variables */
unsigned char  *curbm;
unsigned char  *curbm0;
unsigned char  *pacbm;
unsigned char  *pacbm0 = NULL;
unsigned char  *tmpbm;
unsigned char  *tmpbm0;
CONF_DATA      *conf;
MEM_DATA       *mem;
BM_WINDOW      bm0;
BM_WINDOW      bm1;
Visual         *visual;
int            bmsize;
int            curconf;
unsigned int   dpth;
int            lmemid;
int            pfact;
int            psize;
int            screen;
int            i;
int            j;
int            k;
int            l;
int            xd;
float          yy;
Window root;
int x, y;
unsigned int width, height, bw;

/* Use a local memory identifier as it may get updated */
lmemid = memid;
curconf = device[display].confid;
conf = device[display].config[curconf];

/* If memid is undefined use the last visible memory */
if ( lmemid == -1 )
   {
   for ( i = 0; i < conf->n_mem; i++ )
      {
      mem = conf->memory[i];
      if ( mem->visibility == 1 )
         lmemid = i;
      }
   }

/* Get the window identifiers from the global variables */
display_id = (Display*) device[display].vd_id;
gcima = (GC) device[display].gcima_id;

/* Get the window descriptors from X */
screen = XDefaultScreen( display_id );
visual = VisualOfWindow( display );
XGetGeometry( display_id, w_id, &root, &x, &y, &width, &height, &bw, &dpth);
pfact = 8 /dpth;

/* Get the location of the data from the global variables */
mem = conf->memory[lmemid];
curbm = (unsigned char*)( mem->mmbm );
pixmap_id = (Pixmap) mem->pm_id;

/* Set source bitmap window (with Y inversion) */
bm0.x_coord = x0;
bm0.y_coord = y0;
bm0.x_size  = xs0;
bm0.y_size  = ys0;

/* Set target (display) bitmap window (with Y inversion) */
bm1.x_coord = x1;
bm1.y_coord = mem->y_size -
              ((device[display].dev_ysiz < (y1 + ys1)) ?
               device[display].dev_ysiz : (y1 + ys1));

/* Ensure the target sizes are not rounded down */
bm1.x_size = (int)( xs0 * (float)zoom + 0.5 );
bm1.y_size = (int)( ys0 * (float)zoom + 0.5 );

/* Allocate some memory for the bitmap */
bmsize =  bm1.x_size * bm1.y_size ;
tmpbm0 = (unsigned char *) malloc (bmsize);
tmpbm  = tmpbm0;

/* Check the memory allocation */
if ( tmpbm0 == 0 )
   {
   perror( "malloc" );
   exit( MEMALLERR );
   }

/* Calculate the (zoomed) fraction of a pixel starting a line */
xd = (int)( ( x0 - (int)x0 ) * (float)zoom );

/* Resample the image */
for (i = ys1; i > 0; i--)
   {
   yy = (float)i / (float)zoom;
   curbm0 = (unsigned char *)( mem->mmbm + ( mem->y_size - 1 -
                               (int)( y0 + yy ) ) * mem->x_size + (int)x0 );
   curbm = curbm0;
   k = xd;
   for (j = 0; j < xs1; j++)
      {
      *tmpbm++ = *curbm;
      if ( ++k >= zoom )
         {
         k = 0;
         curbm++;
         }
      }
   }

/* If the packing factor is one then use the data as supplied */
if ( pfact == 1 )
   {
   pacbm = tmpbm0;
   psize = 0;
   }

/* Otherwise pack the data into bytes */
else
   {
   psize = ( bm1.x_size / pfact + 1 ) * bm1.y_size;
   pacbm0 = (unsigned char *) malloc( psize );
   pacbm = pacbm0;
   pack( tmpbm0, pacbm, pfact, bm1.x_size, bm1.y_size );
   }

/* Draw the image data into the global pixmap */
ima = XCreateImage( display_id, visual, dpth, ZPixmap,
                    0, (char*)pacbm, bm1.x_size, bm1.y_size, 8, 0 );
XSetPlaneMask( display_id, gcpix, mem->pm_mask );
XPutImage(display_id, pixmap_id, gcpix, ima, 0, 0,
          bm1.x_coord, bm1.y_coord, ima->width, ima->height) ;
XFlush(display_id);

/* Release the bitmap memory */
XFree( (char *)ima );
free (tmpbm0);
if ( psize > 0 )
   free( pacbm0 );

return;
}

/******************************************************************************/

void imagrefr_uz_p ( int display, int memid, int zoom, float x0, float y0,
                     float xs0, float ys0, float x1, float y1, float xs1,
                     float ys1 )

/*
*+
*  Name:
*     imagrefr_uz_p
*
*  Purpose:
*     Refresh zoomed image into pixmap ( zoom < 0 )
*
*  Invocation:
*     imagrefr_uz_p( display, memid, zoom, x0, y0, xs0, ys0, x1, y1, xs1, ys1 )
*
*  Description:
*     Resample images and draw into the pixmap.
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     zoom = int
*        Zoom factor
*     x0 = float
*        Origin coordinates for main memory bitmap (source)
*     y0 = float
*        Origin coordinates for main memory bitmap (source)
*     xs0 = float
*        Sub-image main memory bitmap dimensions   (source)
*     ys0 = float
*        Sub-image main memory bitmap dimensions   (source)
*     x1 = float
*        Origin coordinates for display bitmap     (target)
*     y1 = float
*        Origin coordinates for display bitmap     (target)
*     xs1 = float
*        Sub-image display bitmap dimensions       (target)
*     ys1 = float
*        Sub-image display bitmap dimensions       (target)
*
*  Algorithm:
*     Create an image from the data.
*     Draw that image into the pixmap.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*     24-APR-1991 (NE):
*        Pack data if display less than 8 bits deep
*      8-MAY-1991 (NE):
*        Strict X library adherence
*     27-JUN-1991 (NE):
*        Allow memory and display to be different sizes
*     12-JUL-1991 (NE):
*        Get pixmap ids from device structure
*     16-APR-1992 (NE):
*        Get pixmap id from memory structure
*     16-OCT-1992 (NE):
*        Change pixel arguments to real to allow for partial pixels
*/

{

/* Local variables */
unsigned char  *curbm;
unsigned char  *pacbm;
unsigned char  *pacbm0 = NULL;
unsigned char  *tmpbm;
unsigned char  *tmpbm0;
CONF_DATA      *conf;
MEM_DATA       *mem;
BM_WINDOW      bm0;
BM_WINDOW      bm1;
Visual         *visual;
int            bmsize;
int            curconf;
unsigned int   dpth;
int            i;
int            j;
int            lmemid;
int            pfact;
int            psize;
int            screen;
Window root;
int x, y;
unsigned int width, height, bw;

/* Use a local memory identifier as it may get updated */
lmemid = memid;
curconf = device[display].confid;
conf = device[display].config[curconf];

/* If memid is undefined use the last visible memory */
if ( lmemid == -1 )
   {
   for ( i = 0; i < conf->n_mem; i++ )
      {
      mem = conf->memory[i];
      if ( mem->visibility == 1 )
         lmemid = i;
      }
   }

/* Get the window identifiers from the global variables */
display_id = (Display*) device[display].vd_id;
gcima = (GC) device[display].gcima_id;

/* Get the window descriptors from X */
screen = XDefaultScreen( display_id );
visual = VisualOfWindow( display );
XGetGeometry( display_id, w_id, &root, &x, &y, &width, &height, &bw, &dpth);
pfact = 8 / dpth;

/* Get the location of the data from the global variables */
mem = conf->memory[lmemid];
pixmap_id = (Pixmap) mem->pm_id;

/* Set source bitmap window (with Y inversion) */
bm0.x_coord = x0;
bm0.y_coord = y0;
bm0.x_size = xs0;
bm0.y_size = ys0;

/* Set target (display) bitmap window (with Y inversion) */
bm1.x_coord = x1;
bm1.y_coord = mem->y_size -
              ((device[display].dev_ysiz < (y1 + ys1)) ?
               device[display].dev_ysiz : (y1 + ys1));
bm1.x_size = xs0 / zoom;
bm1.y_size = ys0 / zoom;

/* Allocate some memory for the bitmap */
bmsize = bm1.x_size * bm1.y_size;
tmpbm0 = (unsigned char *) malloc (bmsize);
tmpbm  = tmpbm0;

/* Check the memory allocation */
if ( tmpbm0 == 0 )
   {
   perror( "malloc" );
   exit( MEMALLERR );
   }

/* Copy main memory bitmap to display bitmap */
for (i = 0; i < ys0; i+= zoom)
   {
   curbm = (unsigned char *) (mem->mmbm + (bm0.y_coord + i) *
                               mem->x_size + bm0.x_coord);
   for (j = 0; j < xs0; j += zoom)
      *tmpbm++ = *(curbm + j);
   }

/* Ensure the width and height are rounded correctly */
width = ( xs0 + zoom - 1 ) / zoom;
height = ( ys0 + zoom - 1 ) / zoom;

/* If the packing factor is one then use the data as supplied */
if ( pfact == 1 )
   {
   pacbm = tmpbm0;
   psize = 0;
   }

/* Otherwise pack the data into bytes */
else
   {
   psize = ( width / pfact + 1 ) * height;
   pacbm0 = (unsigned char *) malloc( psize );
   pacbm = pacbm0;
   pack( tmpbm0, pacbm, pfact, width, height );
   }

/* Draw the image data into the global pixmap */
ima = XCreateImage( display_id, visual, dpth, ZPixmap,
                    0, (char*)pacbm, width, height, 8, 0 );
XSetPlaneMask( display_id, gcpix, mem->pm_mask );
XPutImage( display_id, pixmap_id, gcpix, ima, 0, 0,
           bm1.x_coord, bm1.y_coord, ima->width, ima->height );
XFlush( display_id );

/* Release the bitmap memory */
XFree( (char *)ima );
free (tmpbm0);
if ( psize > 0 )
   free( pacbm0 );

return;
}

/******************************************************************************/

void polyrefr_p ( int display, int confn, int memid )

/*
*+
*  Name:
*     polyrefr_p
*
*  Purpose:
*     Refresh polylines into pixmap
*
*  Invocation:
*     polyrefr_p ( display, confn, memid );
*
*  Description:
*     Draw polylines into the pixmap.
*
*  Arguments:
*     display = int
*        Display identifier
*     confn = int
*        Configuration number
*     memid = int
*        Memory identifier
*
*  Algorithm:
*     Draw each polyline in turn into the pixmap.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*     12-JUL-1991 (NE):
*        Remove test against pixmap memory
*      1-OCT-1992 (NE):
*        Allow for display zoom and scrolls
*      6-JAN-1993 (NE):
*        Remove special treatment for unzoomed case
*/

{

/* Local variables */
int j;
int k;
int col;
int style;
int np;
int xm;
int ym;
int xs[1024];
int ys[1024];
int *x0;
int *y0;
int zoom;
CONF_DATA  *conf;
MEM_DATA   *mem;
G_LIST     *curr_gel = NULL;

/* Get the location of the data from the global variables */
conf = device[display].config[confn];
mem = conf->memory[memid];
zoom = mem->zoom * device[display].zoom;

/* Draw the polyline data into the global pixmap */
if ((mem->visibility == 1) && ((mem->type & II_GRAPHIC) > 0))
   {
   for (j = 0; j < mem->n_gel; j++)
      {
      curr_gel = (j == 0) ? mem->el_glist : curr_gel->next_gel;
      col = curr_gel->color;
      style = curr_gel->style;
      np = curr_gel->np;
      x0 = curr_gel->xl;
      y0 = curr_gel->yl;
      for ( k = 0; k < np; k++ )
         {
         xm = (int) *x0++;
         ym = (int) *y0++;
         mem_screen_conv( display, memid, xm, ym, &xs[k], &ys[k] );
         }
      polyline_p( display, memid, col, style, xs, ys, np, 0 );
      }
   }

return;
}

/******************************************************************************/

void textrefr_p ( int display, int confn, int memid )

/*
*+
*  Name:
*     textrefr_p
*
*  Purpose:
*     Refresh text into pixmap
*
*  Invocation:
*     textrefr_p ( display, confn, memid );
*
*  Description:
*     Draw text into the pixmap.
*
*  Arguments:
*     display = int
*        Display identifier
*     confn = int
*        Configuration number
*     memid = int
*        Memory identifier
*
*  Algorithm:
*     Draw each text string in turn into the pixmap.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*     27-JUN-1991 (NE):
*        Change argument list in text_p
*     12-JUL-1991 (NE):
*        Remove test against pixmap memory
*      1-OCT-1992 (NE):
*        Allow for display zoom and scrolls
*      6-JAN-1993 (NE):
*        Remove special treatment for unzoomed case
*/

{

/* Local variables */
int j;
int x0;
int y0;
int xd;
int yd;
int path;
int orient;
int color;
int size;
int idierr;
int zoom;
char text[80];
CONF_DATA *conf;
MEM_DATA *mem;
T_LIST *curr_tel = NULL;

/* Get the location of the data from the global variables */
conf = device[display].config[confn];
mem = conf->memory[memid];
zoom = mem->zoom * device[display].zoom;

/* Draw the text data into the global pixmap */
if ((mem->visibility == 1) && ((mem->type & II_TEXT) > 0))
   {
   for (j = 0; j < mem->n_tel; j++)
      {
      curr_tel = (j == 0) ? mem->el_tlist : curr_tel->next_tel;

      x0 = curr_tel->x0;
      y0 = curr_tel->y0;
      path = curr_tel->path;
      orient = curr_tel->orient;
      color = curr_tel->color;
      size = curr_tel->size;
      strcpy (text , curr_tel->text);
      mem_screen_conv( display, memid, x0, y0, &xd, &yd );
      text_p( display, memid, xd, yd, path, orient, color, size, text, 0 );
      }
   }

return;
}

/******************************************************************************/

void polyline_p ( int display, int memid, int col, int style, int xs[],
                  int ys[], int np, int inwin )

/*
*+
*  Name:
*     polyline_p
*
*  Purpose:
*     Polyline on memory pixmap
*
*  Invocation:
*     polyline_p ( display, memid, col, style, xs, ys, np, inwin );
*
*  Description:
*     Draw polyline into the pixmap.
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     col = int
*        Colour index
*     style = int
*        Line style
*     np = int
*        Number of points
*     xs = int[]
*        Array of x-coordinates
*     ys = int[]
*        Array of y-coordinates
*     inwin = int
*        Draw polyline into window
*
*  Algorithm:
*     Draw polyline into the pixmap.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*      8-MAY-1991 (NE):
*        Strict X library adherence
*     27-JUN-1991 (NE):
*        Use memory rather than display size for y offset
*     12-JUL-1991 (NE):
*        Get pixmap ids from device structure
*     24-JUL-1991 (NE):
*        Differentiate between image and graphics memories for colour
*     16-APR-1992 (NE):
*        Get pixmap id from memory structure
*      1-MAY-1992 (NE):
*        Use overlay colour if there is an overlay plane
*     10-AUG-1992 (NE):
*        Fix bug selecting pen colour for IMAGE memories
*     24-SEP-1992 (NE):
*        Draw polyline into window as well as pixmap if inwin is set
*      4-NOV-1992 (NE):
*        Set line width according to zoom
*     11-NOV-1992 (NE):
*        Dereference colour index through lut as well as curlut
*      6-JAN-1993 (NE):
*        Remove offsets for unzoomed case
*     17-FEB-1993 (NE):
*        Correct the y-inversion
*      3-MAR-1998 (DLT):
*        Correct positions in window for GWM scroll
*/

{

/* Local variables */
int i;
int lmemid;
int locol;
int xor;
int screen;
unsigned int loff, width;
int curconf;
int x_off, y_off, zoom;
int gwm_x_off, gwm_y_off;
CONF_DATA *conf;
MEM_DATA *mem;
LUT_DATA *lut;
Colormap cmap;
XPoint vlist[8192];
XColor xcol;
Status xstat;

/* Use a local memory identifier as it may get updated */
lmemid = memid;
curconf = device[display].confid;
conf = device[display].config[curconf];

/* If memid is undefined use the last visible memory */
if ( lmemid == -1 )
   {
   for ( i = 0; i < conf->n_mem; i++ )
      {
      mem = conf->memory[i];
      if ( mem->visibility == 1 )
         lmemid = i;
      }
   }

/* Get the window identifiers from the global variables */
display_id = (Display*) device[display].vd_id ;
gcdraw     = (GC) device[display].gcdraw_id ;
mem = conf->memory[lmemid];
pixmap_id = (Pixmap) mem->pm_id;
w_id = (Window) device[display].wd_id;
lut = device[display].lookup[mem->lut_id];

/* Get the window descriptors from X */
screen     = XDefaultScreen( display_id );
cmap	   = ColormapOfWindow( display );

/* Establish the colour and line style */
xor = 0;
if ( col < 0 )
   {
   col = ( -col );
   xor = 1;
   }

/* Set line width according to the zoom */
zoom = mem->zoom * device[display].zoom;
width = zoom > 0 ? zoom : 1;

/* Offset the line width so that it is drawn through the pixel centre */
loff = width / 2;

/* Copy the data points into the X Point Data Structure */
for ( i = 0; i < np; i++ )
   {
   vlist[i].x = (short) ( xs[i] + loff );
   vlist[i].y = (short) ( mem->y_size - 1 - ys[i] - loff );
   }

/* If there is an overlay plane then use one of the overlay colours */
if ( ( lmemid == device[display].pm_memov ) &&
     ( device[display].overlay ) )
   locol = curlut.lutpix[ curlut.len / 2 ] | ~device[display].pm_mask;

/* Select colour according to the memory type */
else
   {
   if ( ( mem->type & II_IMAGE ) > 0 )
      locol = curlut.lutpix[lut->lutpix[col]];

/* Define the line colour */
   else
      {
      switch ( col )
         {
            case II_BLACK:
            XParseColor( display_id, cmap, "#000000", &xcol );
            break;
         case II_WHITE:
            XParseColor( display_id, cmap, "#ffffff", &xcol );
            break;
         case II_RED:
            XParseColor( display_id, cmap, "#ff0000", &xcol );
            break;
         case II_GREEN:
            XParseColor( display_id, cmap, "#00ff00", &xcol );
            break;
         case II_BLUE:
            XParseColor( display_id, cmap, "#0000ff", &xcol );
            break;
         case II_YELLOW:
            XParseColor( display_id, cmap, "#ffff00", &xcol );
            break;
         case II_MAGENTA:
            XParseColor( display_id, cmap, "#ff00ff", &xcol );
            break;
         case II_CYAN:
            XParseColor( display_id, cmap, "#00ffff", &xcol );
            break;
         default:
            XParseColor( display_id, cmap, "#ffffff", &xcol );
            break;
         }

      xstat = XAllocColor( display_id, cmap, &xcol );
      locol = xcol.pixel;
      }
   }

/* Calculate the offset of the pixmap from the window */
x_off = 0;
y_off = device[display].dev_ysiz - mem->y_size;

/* Select the graphics context */
if ( xor == 1 )
   {

/* Draw the lines into the pixmap */
   XSetLineAttributes( display_id, gcdraw, width, LineSolid,
                       CapButt, JoinMiter );
   XSetForeground( display_id, gcdraw, locol );
   XSetPlaneMask( display_id, gcdraw, mem->pm_mask );
   XDrawLines( display_id, pixmap_id, gcdraw, vlist, np, CoordModeOrigin );

/* Draw the lines into the window allowing for the offset and GWM scroll */
   if ( inwin )
      GWM_GetScroll( display_id, w_id, &gwm_x_off, &gwm_y_off );
      {
      for ( i = 0; i < np; i++ )
         {
         vlist[i].x = vlist[i].x + (short) x_off + gwm_x_off;
         vlist[i].y = vlist[i].y + (short) y_off + gwm_y_off;
         }
      XDrawLines( display_id, w_id, gcdraw, vlist, np, CoordModeOrigin );
      }
   }

/* Select the graphics context */
else
   {

/* Draw the lines into the pixmap */
   XSetLineAttributes( display_id, gcpix, width, LineSolid,
                       CapButt, JoinMiter );
   XSetForeground( display_id, gcpix, locol );
   XSetPlaneMask( display_id, gcpix, mem->pm_mask );
   XDrawLines( display_id, pixmap_id, gcpix, vlist, np, CoordModeOrigin );

/* Draw the lines into the window allowing for the offset */
   if ( inwin )
      {
      GWM_GetScroll( display_id, w_id, &gwm_x_off, &gwm_y_off );
      for ( i = 0; i < np; i++ )
         {
         vlist[i].x = vlist[i].x + (short) x_off + gwm_x_off;
         vlist[i].y = vlist[i].y + (short) y_off + gwm_y_off;
         }
      XDrawLines( display_id, w_id, gcpix, vlist, np, CoordModeOrigin );
      }
   }

XFlush( display_id );

return;
}

/******************************************************************************/

void text_p ( int display, int memid, int x0, int y0, int path, int orient,
              int col, int size, char txt[], int inwin )

/*
*+
*  Name:
*     text_p
*
*  Purpose:
*     Text on memory pixmap
*
*  Invocation:
*     text_p ( display, memid, x0, y0, path, orient, col, size, txt, inwin );
*
*  Description:
*     Draw text into the pixmap.
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     x0 = int
*        X position of text
*     y0 = int
*        Y position of text
*     path = int
*        Text path
*     orient = int
*        Text orientation
*     col = int
*        Pixel colour value
*     size = int
*        Text size
*     txt = char[]
*        Text string
*     inwin = int
*        Draw text into window
*
*  Algorithm:
*     Draw text into the pixmap.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*      3-MAY-1991 (NE):
*        Allocate colours as in polyline_p
*      8-MAY-1991 (NE):
*        Strict X library adherence
*     24-JUN-1991 (NE):
*        Select from four font sizes.
*        Added memid to argument list and removed idierr.
*        Use memory rather than display size for y offset.
*     12-JUL-1991 (NE):
*        Get pixmap ids from device structure
*     24-JUL-1991 (NE):
*        Differentiate between image and graphics memories for colour
*     16-APR-1992 (NE):
*        Get pixmap id from memory structure
*      1-MAY-1992 (NE):
*        Use overlay colour if there is an overlay plane
*     24-SEP-1992 (NE):
*        Draw text into window as well as pixmap if inwin is set
*     11-NOV-1992 (NE):
*        Dereference colour index through lut as well as curlut
*     19-NOV-1992 (NE):
*        Offset text by the font descent, not the ascent
*      6-JAN-1993 (NE):
*        Remove offsets for unzoomed case
*     17-FEB-1993 (NE):
*        Correct the y-inversion
*      9-MAR-1994 (DLT):
*        Free font and info structure after use
*      3-MAR-1998 (DLT):
*        Correct positions in window for GWM scroll
*/

{

/* Local variables */
char fontname[256];
int i;
int lmemid;
int locol;
int txtlen;
int screen;
int white;
int curconf;
int x_off, y_off, zoom;
int gwm_x_off, gwm_y_off;
CONF_DATA *conf;
MEM_DATA *mem;
LUT_DATA *lut;
Colormap cmap;
XFontStruct *font_info;
XColor xcol;
Status xstat;

/* Use a local memory identifier as it may get updated */
lmemid = memid;
curconf = device[display].confid;
conf = device[display].config[curconf];

/* If memid is undefined use the last visible memory */
if ( lmemid == -1 )
   {
   for ( i = 0; i < conf->n_mem; i++ )
      {
      mem = conf->memory[i];
      if ( mem->visibility == 1 )
         lmemid = i;
      }
   }

/* Get the window identifiers from the global variables */
display_id = (Display*) device[display].vd_id;
mem = conf->memory[lmemid];
pixmap_id = (Pixmap) mem->pm_id;
w_id = (Window) device[display].wd_id;
lut = device[display].lookup[mem->lut_id];

/* Get the window descriptors from X */
screen	   = XDefaultScreen( display_id );
white	   = XWhitePixel( display_id, screen );
cmap	   = ColormapOfWindow( display );

/* Select the font according to size */
switch ( size )
   {
   default : strcpy( fontname, normalname );
             break;
   case 1  : strcpy( fontname, largename );
             break;
   case 2  : strcpy( fontname, vlargename );
             break;
   case 3  : strcpy( fontname, smallname );
             break;
   }

/* Inquire the length of the text string */
txtlen = strlen( txt );

/* Transform the y-coordinate allowing for the character size */
font_info = XLoadQueryFont( display_id,fontname );
y0 = mem->y_size - 1 - ( y0 +font_info->descent );

/* If there is an overlay plane then use one of the overlay colours */
if ( ( lmemid == device[display].pm_memov ) &&
     ( device[display].overlay ) )
   locol = curlut.lutpix[ curlut.len / 2 ] | ~device[display].pm_mask;

/* Select colour according to the memory type */
else
   {
   if ( ( mem->type & II_IMAGE ) > 0 )
      locol = curlut.lutpix[lut->lutpix[col]];

/* Define the text colour */
   else
      {
      switch ( col )
         {
         case II_BLACK:
            XParseColor( display_id, cmap, "#000000", &xcol );
            break;
         case II_WHITE:
            XParseColor( display_id, cmap, "#ffffff", &xcol );
            break;
         case II_RED:
            XParseColor( display_id, cmap, "#ff0000", &xcol );
            break;
         case II_GREEN:
            XParseColor( display_id, cmap, "#00ff00", &xcol );
            break;
         case II_BLUE:
            XParseColor( display_id, cmap, "#0000ff", &xcol );
            break;
         case II_YELLOW:
            XParseColor( display_id, cmap, "#ffff00", &xcol );
            break;
         case II_MAGENTA:
            XParseColor( display_id, cmap, "#ff00ff", &xcol );
            break;
         case II_CYAN:
            XParseColor( display_id, cmap, "#00ffff", &xcol );
            break;
         default:
            XParseColor( display_id, cmap, "#ffffff", &xcol );
            break;
         }

      xstat = XAllocColor( display_id, cmap, &xcol );
      locol = xcol.pixel;
      }
   }

/* Calculate the offset of the pixmap from the window */
x_off = 0;
y_off = device[display].dev_ysiz - mem->y_size;

/* Draw the text into the pixmap and window */
XSetFont( display_id, gcpix, font_info->fid );
XSetForeground( display_id, gcpix, locol );
XSetPlaneMask( display_id, gcpix, mem->pm_mask );
XDrawString( display_id, pixmap_id, gcpix, x0, y0, txt, txtlen );
if ( inwin )
{
   GWM_GetScroll( display_id, w_id, &gwm_x_off, &gwm_y_off );
   XDrawString( display_id, w_id, gcpix, x0 + x_off + gwm_x_off,
                y0 + y_off + gwm_y_off, txt, txtlen );
}
XFreeFont( display_id, font_info );
XFlush( display_id );

XSetForeground( display_id, gcpix, white );
XFlush( display_id );

return;
}

/******************************************************************************/

void refr_p ( int display )

/*
*+
*  Name:
*     refr_p
*
*  Purpose:
*     Refresh the pixmap into the display
*
*  Invocation:
*     refr_p ( display );
*
*  Description:
*     Refresh the pixmap into the display
*
*  Arguments:
*     display = int
*        Display identifier
*
*  Algorithm:
*     Refresh the pixmap into the display
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*      8-MAY-1991 (NE):
*        Strict X library adherence
*     27-JUN-1991 (NE):
*        Allow memory and display to be different sizes
*     12-JUL-1991 (NE):
*        Get pixmap ids from device structure
*     23-AUG-1991 (NE):
*        Send scroll offsets to GWM
*     16-APR-1992 (NE):
*        Get pixmap id from memory structure
*      7-MAY-1992 (NE):
*        Allow for overlay plane
*     18-DEC-1992 (NE):
*        Use the GWM scrolls to offset the pixmap
*/

{

/* Local variables */
CONF_DATA      *conf;
MEM_DATA       *mem;
int curconf;
int x_off;
int y_off;

/* Get the window identifiers from the global variables */
display_id = (Display*) device[display].vd_id;
w_id = (Window) device[display].wd_id;

/* Get details of the current configuration */
curconf = device[display].confid;
conf = device[display].config[curconf];

/* Inquire current base memory */
mem = conf->memory[device[display].pm_mem];
pixmap_id = (Pixmap) mem->pm_id;

/* Get the scroll offsets from GWM */
GWM_GetScroll( display_id, w_id, &x_off, &y_off );

/* Copy the pixmap into the window */
XClearWindow( display_id, w_id );
XSetPlaneMask( display_id, gcpix, mem->pm_mask );
XCopyArea( display_id, pixmap_id, w_id, gcpix, 0, 0,
           mem->x_size, mem->y_size, x_off, y_off );
XFlush( display_id );

/* Inquire current overlay memory */
if ( device[display].overlay )
   {
   mem = conf->memory[device[display].pm_memov];
   pixmap_id = (Pixmap) mem->pm_id;

/* Get the scroll offsets from GWM */
   GWM_GetOvScroll( display_id, w_id, &x_off, &y_off );

/* Copy the pixmap into the window */
   XSetPlaneMask( display_id, gcpix, mem->pm_mask );
   XCopyArea( display_id, pixmap_id, w_id, gcpix, 0, 0,
              mem->x_size, mem->y_size, x_off, y_off );
   XFlush( display_id );
   }

return;
}

/******************************************************************************/

void snap_pix ( int display, int colmode, int npixel, int xoff, int yoff,
                int nlines, int dd, int depth, int packf, int data[] )

/*
*+
*  Name:
*     snap_pix
*
*  Purpose:
*     Get snapshot from pixmap
*
*  Invocation:
*     snap_pix( display, colmode, npixel, xoff, yoff, nlines, dd, depth,
*               packf, data )
*
*  Description:
*     Get snapshot from pixmap. This implementation assumes the display
*     pseudocolour, and therefore ignores the colour mode argument.
*
*  Arguments:
*     display = int
*        Display identifier
*     colmode = int
*        Colour mode
*     npixel = int
*        Data array length
*     xoff = int
*        X data origin
*     yoff = int
*        Y data origin
*     nlines = int
*        Number of complete lines to read
*     dd = int
*        Data depth truncation
*     depth = int
*        Data depth ( bits / pixel )
*     packf = int
*        Packing factor ( pixel / integer )
*     data = int
*        Image data
*
*  Algorithm:
*     Get snapshot from pixmap
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlik, RAL)
*
*  History:
*     18-MAR-1991 (NE):
*        Orignal version
*      8-MAY-1991 (NE):
*        Strict X library adherence
*     14-MAY-1991 (NE):
*        Reverse the line order from top to bottom
*     12-JUL-1991 (NE):
*        Get pixmap ids from device structure
*     27-NOV-1991 (NE):
*        Subtract curlut.off from data value and correct image sampling
*     19-MAR-1992 (NE):
*        Declare rint function for Ultrix as well as VAX machines
*     26-MAY-1992 (NE):
*        Allow for overlay plane.
*        On Suns XGetImage does not return masked values.
*     14-OCT-1992 (NE):
*        Check memory visibilities
*     16-MAR-1994 (DLT):
*        Correct rounding
*/

{

/* Local variables */
#if !HAVE_DECL_RINT
double rint( double x );
#endif
unsigned char *curbm, *curbm1;
int           base;
double        blue;
int           curconf;
int           first;
double        green;
int           height, height1 = 0;
int           i;
int           j;
int           k;
int           last;
int           left;
int           lend;
int           lines, lines1;
int           middle;
int           nb;
int           overlay;
int           pcount, pcount1;
int           pen;
int           pix;
double        red;
int           status;
int           val;
int           width, width1 = 0;
int           xorigin, xorigin1 = 0;
int           yorigin, yorigin1 = 0;
int           xscroll, xscroll1;
int           yscroll, yscroll1;
int           xend1 = 0;
int           yend1 = 0;
int           xstart1 = 0;
int           ystart1 = 0;
CONF_DATA     *conf;
MEM_DATA      *mem, *mem1 = NULL;
LUT_DATA      *lut, *lut1;
XImage        *image, *image1 = NULL;

/* Conversion factors for 128 intensity levels */
/* red = 127 * 0.30, green = 127 * 0.59, blue = 127 * 0.11 */
red = (pow( (double) 2., (double) depth ) -1. ) * 0.30;
green = (pow( (double) 2., (double) depth ) - 1. ) * 0.59;
blue = (pow( (double) 2., (double) depth ) - 1. ) * 0.11;

/* Get the display identifier */
display_id = (Display*) device[display].vd_id;
w_id = (Window) device[display].wd_id;
curconf = device[display].confid;
conf = device[display].config[curconf];
mem = conf->memory[device[display].pm_mem];
lut = device[display].lookup[mem->lut_id];
pixmap_id = (Pixmap) mem->pm_id;

/* Read off enough of the pixmap to contain all the requested pixels */
xorigin = mem->x_woff;
yorigin = mem->y_size - ( mem->y_woff + yoff + nlines );
width = mem->x_wdim;
height = nlines;

/* Calculate how much of the overlay plane occupies this transfer window */
overlay = device[display].overlay;
if ( overlay )
   {
   mem1 = conf->memory[device[display].pm_memov];
   lut1 = device[display].lookup[mem1->lut_id];

/* Calculate the origin of the transfer window w.r.t. the overlay */
   GWM_GetScroll( display_id, w_id, &xscroll, &yscroll );
   GWM_GetOvScroll( display_id, w_id, &xscroll1, &yscroll1 );
   xorigin1 = xorigin + xscroll - xscroll1;
   yorigin1 = yorigin + yscroll - yscroll1;
   height1 = height;
   width1 = width;

/* xstart1 and ystart1 give the position of the start of the */
/* image data w.r.t. the transfer window */
   xstart1 = 0;
   ystart1 = 0;

/* If the transfer window lies partly outside the overlay memory then */
/* adjust the variables. */
   if ( xorigin1 < 0 )
      {
      width1 = width + xorigin1;
      xstart1 = -xorigin1;
      xorigin1 = 0;
      }
   if ( yorigin1 < 0 )
      {
      height1 = height + yorigin1;
      ystart1 = -yorigin1;
      yorigin1 = 0;
      }
   if ( ( xorigin1 + width1 ) > mem->x_size )
      width1 = mem->x_size - xorigin1;
   if ( ( yorigin1 + height1 ) > mem->y_size )
      height1 = mem->y_size - yorigin1;

/* xend1 and yend1 give the position of the end of the */
/* image data w.r.t. the transfer window */
   xend1 = xstart1 + width1;
   yend1 = ystart1 + height1;

/* If the width or height are negative then can forget the overlay */
   if ( ( width1 < 0 ) || ( height1 < 0 ) ) overlay = 0;
   }

/* Read the pixmap into an image structure ( top to bottom ) */
base = mem->visibility;
image = XGetImage( display_id, pixmap_id, xorigin, yorigin, width, height,
                   mem->pm_mask, ZPixmap );
overlay = overlay && mem1->visibility;
if ( overlay )
   image1 = XGetImage( display_id, pixmap_id, xorigin1, yorigin1, width1,
                       height1, mem1->pm_mask, ZPixmap );

/* Number of bits per pixel in a longword */
nb = 32 / packf;

/* Calculate the number of pixels in the bottom row, the number of
   subsequent complete lines and the number of pixels in the top row */
first = npixel < mem->x_wdim - xoff ? npixel : mem->x_wdim - xoff;
middle = ( npixel - first ) / mem->x_wdim;
last = npixel - first - middle * mem->x_wdim;
lines = middle + ( last > 0 ? 1 : 0 );

/* Read the image data in reverse line order, allowing for any offset, by
   positioning the pointer at the start of the last ( top to bottom ) line */
curbm = (unsigned char*)image->data + xoff + lines * image->bytes_per_line;

/* Initialise various loop variables */
i = 0;
j = 0;
k = 0;
pix = 0;
pcount = 0;
left = npixel;
lend = npixel - last - middle * mem->x_wdim;

/* Keep looping for the required number of pixels */
while ( j < npixel )
   {

/* Convert the pen number into a colour and pack in the output longword */
/* On Suns XGetImage does not return masked values so do it here */
   if ( base )
      {
      pen = ( dd > 0 ) ? ((( (int)*curbm & mem->pm_mask ) - curlut.off ) << dd )
                    : ((( (int)*curbm & mem->pm_mask ) - curlut.off ) >> dd );
      val = (int) ( (double)curlut.lutr[pen] / 65535.0 * red ) +
                  ( (double)curlut.lutg[pen] / 65535.0 * green ) +
                  ( (double)curlut.lutb[pen] / 65535.0 * blue ) + 0.5;
      }
   else
      val = 0;

/* See if there is an overlay pixel at this point */
   if ( overlay )
      {
      if ( ( pcount >= xstart1 ) && ( pcount < xend1 ) &&
           ( lines >= ystart1 ) && ( lines < yend1 ) )
         {
         pcount1 = pcount - xstart1;
         lines1 = lines - ystart1;

/* Read the overlay pixel value at this point */
/* On Suns XGetImage does not return masked values so do it here */
         curbm1 = (unsigned char*)image1->data + pcount1 +
                  lines1 * image1->bytes_per_line;
         pen = ( dd > 0 ) ? ( ( (int)*curbm1 & mem1->pm_mask ) << dd )
                           : ( ( (int)*curbm1 & mem1->pm_mask ) >> dd );

/* If the value is non-zero then use the overlay pixel */
         if ( pen != 0 )
            val = (int) ( (double)curlut.lutr[pen] / 65535.0 * red ) +
                        ( (double)curlut.lutg[pen] / 65535.0 * green ) +
                        ( (double)curlut.lutb[pen] / 65535.0 * blue ) + 0.5;
         }
      }

/* Pack the pixel value into a longword */
   pix |= val << ( k * nb );

/* Copy the image data into the output array if the longword is full */
   k = ++k % packf;
   if ( k == 0 )
      {
      data[i++] = pix;
      pix = 0;
      }

/* Increment the data pointer */
   curbm++;

/* If a line has ended reposition the pointer at the start of a new line */
   if ( ++pcount >= lend )
      {
      left -= pcount;
      if ( left > mem->x_wdim )
         lend = mem->x_wdim;
      else
         lend = left;
      lines -= 1;
      curbm = (unsigned char*)image->data + lines * image->bytes_per_line;
      pcount = 0;
      }
   j++;
   }

/* Copy an incomplete longword to the output array */
if ( k |= 0 )
   data[i++] = pix;

/* Free the image resources */
status = XDestroyImage( image );
if ( overlay ) status = XDestroyImage( image1 );

return;
}

/******************************************************************************/

void pack ( unsigned char *inp, unsigned char *outp, int pfact, int width,
            int height )

/*
*+
*  Name:
*     pack
*
*  Purpose:
*     Pack the input bytes into the output bytes.
*
*  Invocation:
*     pack( inp, outp, pfact, width, height )
*
*  Description:
*     Pack the input bytes into the output bytes, with a data
*     compression given by the packing factor.
*
*  Arguments:
*     inp = char
*        Pointer to input data
*     outp = char
*        Pointer to output data
*     pfact = int
*        Packing factor.
*     width = int
*        Number of pixels in x-direction
*     height = int
*        Number of pixels in y-direction
*
*  Algorithm:
*     Pack the data, given as one pixel per byte, into pfact
*     pixels per byte. The packing is done by shifting pixels
*     to the left until the output byte is full. This algorithm
*     assumes that the input data has no bits set outside the
*     range of the packing factor, e.g. data packed into 4 bits
*     must have no data values bigger than 15.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     25-APR-1991 (NE):
*        Orignal version
*     29-AUG-1991 (NE):
*        Correct packing when there is no remainder
*      4-NOV-1992 (NE):
*        Use mask to blank out more significant bits when shifted
*/

{

/* Local variables */
int i;
int j;
int k;
int mask;
int nout;
int rem;
int step;

/* Calculate the number of complete output bytes and the remainder */
nout = width / pfact;
rem = width - nout * pfact;
step = 8 / pfact;
mask = step * 2 - 1;

/* Step through one line at a time */
for (j = 0; j < height; j++ )
   {

/* Resample the data */
   for (i = 0; i < nout; i++)
      {
      *outp = 0;
      for (k = 0; k < pfact; k++)
          *outp |= ( *inp++ & mask ) << ( k * step );
      outp++;
      }

/* Resample the remainder */
   if ( rem > 0 )
      {
      *outp = 0;
      for (k = 0; k < rem; k++)
         *outp |= ( *inp++ & mask ) << ( k * step );
      outp++;
      }
   }

return;
}

/******************************************************************************/

void update_memory ( int display )

/*
*+
*  Name:
*     update_memory
*
*  Purpose:
*     Update memory description from pixmap dimensions
*
*  Invocation:
*     update_memory ( display );
*
*  Description:
*     Update memory description from pixmap
*
*  Arguments:
*     display = int
*        Display identifier
*
*  Algorithm:
*     Update memory description from pixmap
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink, RAL)
*
*  History:
*     29-AUG-1991 (NE):
*        Original version
*     21-OCT-1991 (NE):
*        Set memory visibility
*     16-APR-1992 (NE):
*        Allow for an overlay plane
*     15-MAY-1992 (NE):
*        Release memory before reducing number of memories.
*        Fix overlay pen numbers and overlay LUT.
*     14-OCT-1993 (DLT):
*        Add explicit cast to suppress compiler warnings on Solaris
*      4-MAR-1998 (DLT):
*        Ignore GWM scrolls when updating memory scroll information
*/

{

/* Local variables */
CONF_DATA *conf;
MEM_DATA *mem;
LUT_DATA *lut;
Window root_id;
int curconf, i, ii, j, k, l, lov, pfact, status, x_dev, x_off, y_dev, y_off;
unsigned int border, depth, x_size, y_size;
unsigned char *curbm, *tmpbm;
unsigned long mask;
XImage *image;
Window root;
int x, y;
unsigned int width, height, bw;

/* Inquire current base memory */
curconf = device[display].confid;
conf = device[display].config[curconf];
mem = conf->memory[device[display].pm_mem];

/* Inquire the pixmap size */
display_id = (Display*) device[display].vd_id;
w_id = (Window) device[display].wd_id;
pixmap_id = (Pixmap) mem->pm_id;
XGetGeometry( display_id, pixmap_id, &root_id, &x_off, &y_off,
              &x_size, &y_size, &border, &depth );

/* Update the memory description */
mem->x_size = x_size;
mem->y_size = y_size;
mem->x_wdim = x_size;
mem->y_wdim = y_size;
mem->x_v_size = ( device[display].dev_xsiz < x_size )
                ? device[display].dev_xsiz : x_size;
mem->y_v_size = ( device[display].dev_ysiz < y_size )
                ? device[display].dev_ysiz : y_size;

/* Update the memory scrolls */
status = VDM_INQ( device[display].devtyp, &x_off, &y_off, &x_dev, &y_dev );
#if 0
status = GWM_GetScroll( display_id, w_id, &x_off, &y_off );
mem->x_scroll = x_off;
mem->y_scroll = y_dev - y_size - y_off;
mem->zoom_xsc_new = mem->x_scroll;
mem->zoom_ysc_new = mem->y_scroll;
mem->zoom_xsc = mem->x_scroll;
mem->zoom_ysc = mem->y_scroll;
#endif

/* Read the contents of the pixmap */
mmbm_all( display, curconf, device[display].pm_mem, &status );
curbm = (unsigned char*)( mem->mmbm );
image = XGetImage( display_id, pixmap_id, 0, 0, x_size, y_size,
                   mem->pm_mask, ZPixmap );
tmpbm = (unsigned char*)( image->data );

/* Copy the picture into the internal memory */
/* Note. For devices less than 8 bits deep the pixel values are packed */
pfact = 8 / depth;
k = 0;
for ( j = 0; j < y_size; j++ )
   {
   ii = 0;
   for ( i = 0; i < x_size; i++ )
      {
      *curbm++ = *tmpbm >> k;

/* Move onto the next image byte if all the pixels have been extracted */
      if ( ( k = ++k % pfact ) == 0 )
         {
         tmpbm++;
         ii += 1;
         }
      }

/* At the end of a line account for any incomplete bytes */
   if ( k > 0 )
      {
      tmpbm++;
      ii += 1;
      k = 0;
      }

/* At the end of a line account for any excess bytes in the image */
   for ( i = ii; i < image->bytes_per_line; i++ ) tmpbm++;
   }

/* Indicate that the memory has been written to and that it is visible*/
mem->mem_free = 0;
mem->visibility = 1;

/* Free the image resources */
status = XDestroyImage( image );

/* Inquire current overlay memory */
lov = 0;
if ( device[display].pm_memov >= 0 )
   {
   mem = conf->memory[device[display].pm_memov];
   lov = 1;
   }

/* Otherwise create a new memory if there is an overlay */
else
   {
   GWM_GetOvMask( display_id, w_id, &mask );
   if ( ~mask > 0 )
      {
      lov = 1;

/* Set the dynamic configuration flag and decrement the number of */
/* configuration memories before calling define_memory */
      device[display].dynconfid = curconf;
      if ( conf->n_mem > 1 )
         {
         mem = conf->memory[conf->n_mem - 1];
         for ( l =0; l < mem->n_itt; l++ ) free( mem->itt[l] );
         free( mem );
         conf->n_mem -= 1;
         }

/* Define the overlay memory to be depth = 1 and type = 7 */
      status = define_memory( display, x_size, y_size, 1, 7,
                              &device[display].pm_memov );
      device[display].dynconfid = -1;
      mmbm_all( display, curconf, device[display].pm_memov, &status );

/* Store the mask and the pixmap identifer in the memory structure */
      mem = conf->memory[device[display].pm_memov];
      mem->pm_mask = ~mask;
      mem->pm_id = pixmap_id;

/* Remember the overlay memory id */
      conf->memid = device[display].pm_memov;
      }
   }

/* Update the overlay memory description */
if ( lov )
   {
#if 0
   status = GWM_GetOvScroll( display_id, w_id, &x_off, &y_off );
   mem->x_scroll = x_off;
   mem->y_scroll = y_dev - y_size - y_off;
   mem->zoom_xsc_new = mem->x_scroll;
   mem->zoom_ysc_new = mem->y_scroll;
   mem->zoom_xsc = mem->x_scroll;
   mem->zoom_ysc = mem->y_scroll;
#endif

/* Read the contents of the pixmap */
   curbm = (unsigned char*)( mem->mmbm );
   image = XGetImage( display_id, pixmap_id, 0, 0, x_size, y_size,
                      mem->pm_mask, ZPixmap );
   tmpbm = (unsigned char*)( image->data );

/* Copy the picture into the internal memory */
/* Note. For devices less than 8 bits deep the pixel values are packed */
   pfact = 8 / depth;
   k = 0;
   for ( j = 0; j < y_size; j++ )
      {
      ii = 0;
      for ( i = 0; i < x_size; i++ )
         {

/* XGetImage returns 0 or 1's for the overlay so fix this by */
/* adding the current LUT offset onto the overlay pen number */
         *curbm++ = ( (int)*tmpbm + curlut.off ) >> k;

/* Move onto the next image byte if all the pixels have been extracted */
         if ( ( k = ++k % pfact ) == 0 )
            {
            tmpbm++;
            ii += 1;
            }
         }

/* At the end of a line account for any incomplete bytes */
      if ( k > 0 )
         {
         tmpbm++;
         ii += 1;
         k = 0;
         }

/* At the end of a line account for any excess bytes in the image */
      for ( i = ii; i < image->bytes_per_line; i++ ) tmpbm++;
      }

/* Indicate that the memory has been written to and that it is visible*/
   mem->mem_free = 0;
   mem->visibility = 1;

/* Assign the top LUT to this memory */
   mem->lut_id = device[display].n_lut - 1;

/* Fix the overlay LUT so that pen zero is the background colour */
   lut = device[display].lookup[mem->lut_id];
   lut->lutr[0] = 0;
   lut->lutg[0] = 0;
   lut->lutb[0] = 0;

/* Free the image resources */
   status = XDestroyImage( image );
   }

/* If there is no overlay then decrement the number of memories */
else
   if ( conf->n_mem > 1 )
      {
      mem = conf->memory[conf->n_mem - 1];
      for ( l =0; l < mem->n_itt; l++ ) free( mem->itt[l] );
      free( mem );
      conf->n_mem -= 1;
      }

return;
}

/******************************************************************************/

void update_current_pixmap ( int display, int memid )

/*
*+
*  Name:
*     update_current_pixmap
*
*  Purpose:
*     Update the current memory identifiers in the display structure
*
*  Invocation:
*     update_current_pixmap ( display );
*
*  Description:
*     Update the current memory identifiers in the display structure.
*     The given memory identifier indicates one of the memories currently
*     displayed in the window. This routine searches the memory structures
*     to see if any other memory is associated with the currently displayed
*     pixmap.
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*
*  Algorithm:
*     Search the memory structures to see if any other memory is associated
*     with the currently displayed pixmap. If it is then use the plane mask
*     to decide which is the base plane and which is the overlay plane.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     16-APR-1992 (NE):
*        Original version
*/

{

/* Local variables */
CONF_DATA *conf;
MEM_DATA *mem;
int i, lpm_id;
unsigned long lpm_mask;

/* Assume for now the given memory is the base memory */
device[display].pm_mem = memid;

/* Get the memory structure for the given memory */
conf = device[display].config[device[display].confid];
mem = conf->memory[memid];

/* Remember the pixmap id and mask for this memory */
lpm_id = mem->pm_id;
lpm_mask = mem->pm_mask;

/* Search for another memory that uses the same pixmap */
for ( i = 0; i < conf->n_mem; i++ )
   {
   mem = conf->memory[i];

/* Ignore the memory if it is the same as the given one */
   if ( ( i != memid ) && ( mem->pm_id == lpm_id ) )
      {

/* Assume the memory is the overlay if the plane mask has a lower value */
      if ( mem->pm_mask < lpm_mask )
         device[display].pm_memov = i;
      else
         {
         device[display].pm_memov = memid;
         device[display].pm_mem = i;
         }
      }
   }

return;
}

/******************************************************************************/

int define_memory ( int display, int xdim, int ydim, int mdepth, int mtype,
                    int* memid )

/*
*+
*  Name:
*     define_memory
*
*  Purpose:
*     Dynamic memory definition
*
*  Invocation:
*     status = define_memory( display, xdim, ydim, mdepth, mtype, memid )
*
*  Description:
*     Dynamic memory definition
*
*  Arguments:
*     display = int
*        Display identifier
*     xdim = int
*        Memory X size
*     ydim = int
*        Memory Y size
*     mdepth = int
*        Memory depth
*     mtype = int
*        Memory type
*     memid = int
*        Memory identifier
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     16-APR-1992 (NE):
*        Orignal version taken from IIDAMY_C
*/

{

/* Local Variables */
int   l , m , curconf , nit;
int   iiderr;
float r;
char  fildct[256];
#if !HAVE_DECL_RINT
double rint( double x );
#endif

CONF_DATA  *conf;
MEM_DATA   *mem;
ITT_DATA   *itt;

iiderr = II_SUCCESS;

/* dynamic memory configuration not enabled */
if (device[display].dynconfid == -1)
   {
   iiderr = DYNCONFNOTEN;
   return(iiderr);
   }

getdctfile  (fildct);

nit = ARGSIZE;
kwi_xtr (fildct , "N_MEM" , &nit , arg);
if (nit == 0)
   {
   iiderr = DCTFILWARN;
   return(iiderr);
   }

curconf = device[display].dynconfid;
conf = device[display].config[curconf];

/* check for max number of memories */

if (conf->n_mem == arg[0])
   {
   iiderr = MAXMEM;
   return(iiderr);
   }

*memid = conf->n_mem;

/* allocate dynamic memory structure */

mem = (struct mem_data *) malloc (sizeof(struct mem_data));
if (mem == II_NULL)
   {
   iiderr = MEMALLERR;
   return(iiderr);
   }
conf->memory[*memid] = mem;

mem->mem_free = -1;
mem->mmbm = NULL;
mem->attbm = 0;
mem->ebdepth = 0;
mem->ebpackf = 0;
mem->visibility = 1;
mem->x_size = xdim;
mem->y_size = ydim;
mem->x_v_size = (device[display].dev_xsiz < mem->x_size) ?
                 device[display].dev_xsiz : mem->x_size;
mem->y_v_size = (device[display].dev_ysiz < mem->y_size) ?
                 device[display].dev_ysiz : mem->y_size;
mem->x_v_off = 0;
mem->y_v_off = 0;
mem->depth  = mdepth;
mem->type   = mtype;
mem->x_woff   = 0;                        /* default I/O Transfer Window */
mem->y_woff   = 0;
mem->x_wdim   = mem->x_size;
mem->y_wdim   = mem->y_size;
mem->lut_id = 0;
mem->itt_id = 0;
mem->n_gel = 0;
mem->n_tel = 0;
mem->x_scroll = 0;
mem->y_scroll = 0;
mem->zoom_xsc  = 0;
mem->zoom_ysc  = 0;
mem->zoom_xsc_new = 0;
mem->zoom_ysc_new = 0;
mem->zoom     = 1;
mem->zoom_new    = 1;
mem->bck      = 0;
mem->pm_id = 0;

nit = ARGSIZE;
kwi_xtr (fildct , "N_ITT" , &nit , arg);
if (nit == 0)
   {
   iiderr = DCTFILWARN;
   return(iiderr);
   }

mem->n_itt  = arg[0];
mem->itt_id = 0;
for (l = 0; l < mem->n_itt; l++)
   {
   itt = (struct itt_data *) malloc (sizeof(struct itt_data));
   if (itt == II_NULL)
      {
      iiderr = MEMALLERR;
      return(iiderr);
      }
   mem->itt[l] = itt;
   itt = mem->itt[l];
   itt->itt_def = 1;
   itt->itt_len = (int)rint (pow ( (double) 2. , (double) mem->depth ));
   for (m = 0; m < itt->itt_len; m++)
      {
/* default ITT : linear up to a maximum value of itt->itt_len */
      itt->ittlev[m] = m;
      itt->ittinv[m] = m;
      }
   }

conf->n_mem += 1;

return(iiderr);
}

/******************************************************************************/

void update_keys ( int display )

/*
*+
*  Name:
*     update_keys
*
*  Purpose:
*     Update key descriptions from X
*
*  Invocation:
*     update_keys ( display );
*
*  Description:
*     The keys that are described by key symbols (e.g. XK_Left) in the
*     description file are updated so that the key codes are obtained from
*     the display.
*
*  Arguments:
*     display = int
*        Display identifier
*
*  Algorithm:
*     To prevent kws_xtr opening the description file multiple times
*     the key symbols for the locator movements and evaluator are
*     hardwired into this routine using the appropriate X symbols
*     (e.g. XK_Left, XK_Right).
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*      7-JAN-1993 (NE):
*        Original version
*/

{

/* Local variables */
KeyCode kcode;
KeySym ksym;
char fildct[256];
char space[ARGSIZE][SKWSIZE];
char *sarg[ARGSIZE];
char *p;
int i, j, k, l, m, n, nit, nval;
INT_DEV_DATA *intdev;
LOC_DATA *loc;
EVL_DATA *evl;
TRG_DATA *trg;

/* Get the X display identifier from the global storage */
display_id = (Display*) device[display].vd_id;

/* Allocate space for the character strings */
for( i = 0; i < ARGSIZE; i++ )
   sarg[i] = &space[i][0];

/* Get the name of the DCT file */
getdctfile( fildct );

/* Define the key to move the locator left */
for ( i = 0; i < int_struct.n_int_dev; i++ )
   {
   intdev = int_struct.int_dev[i];
   for ( k = 0; k < intdev->n_loc; k++ )
      {
      loc = intdev->loc[k];

/* Check the key definition for left arrow key */
      if ( loc->left_ls < 0 )
         {

/* Convert the keysym to a keycode and replace the existing value */
         kcode = XKeysymToKeycode( display_id, XK_Left );
         loc->left_ls = (int)kcode;
         }
      }
   }

/* Define the key to move the locator right */
for ( i = 0; i < int_struct.n_int_dev; i++ )
   {
   intdev = int_struct.int_dev[i];
   for ( k = 0; k < intdev->n_loc; k++ )
      {
      loc = intdev->loc[k];

/* Check the key definition for right arrow key */
      if ( loc->right_ls < 0 )
         {

/* Convert the keysym to a keycode and replace the existing value */
         kcode = XKeysymToKeycode( display_id, XK_Right );
         loc->right_ls = (int)kcode;
         }
      }
   }

/* Define the key to move the locator up */
for ( i = 0; i < int_struct.n_int_dev; i++ )
   {
   intdev = int_struct.int_dev[i];
   for ( k = 0; k < intdev->n_loc; k++ )
      {
      loc = intdev->loc[k];

/* Check the key definition for up arrow key */
      if ( loc->up_ls < 0 )
         {

/* Convert the keysym to a keycode and replace the existing value */
         kcode = XKeysymToKeycode( display_id, XK_Up );
         loc->up_ls = (int)kcode;
         }
      }
   }

/* Define the key to move the locator down */
for ( i = 0; i < int_struct.n_int_dev; i++ )
   {
   intdev = int_struct.int_dev[i];
   for ( k = 0; k < intdev->n_loc; k++ )
      {
      loc = intdev->loc[k];

/* Check the key definition for down arrow key */
      if ( loc->down_ls < 0 )
         {

/* Convert the keysym to a keycode and replace the existing value */
         kcode = XKeysymToKeycode( display_id, XK_Down );
         loc->down_ls = (int)kcode;
         }
      }
   }

/* Get the key definitions for the evaluators */
for ( k = 0; k < int_struct.n_int_dev; k++ )
   {
   intdev = int_struct.int_dev[k];
   for ( i = 0; i < intdev->n_evl; i++ )
      {
      evl = intdev->evl[i];

/* Define key to reduce evaluator value */
      if ( evl->def[0] < 0 )
         {

/* Convert the keysym to a keycode and replace the existing value */
         kcode = XKeysymToKeycode( display_id, XK_Left );
         evl->def[0] = (int)kcode;
         }

/* Define key to increase evaluator value */
      if ( evl->def[1] < 0 )
         {

/* Convert the keysym to a keycode and replace the existing value */
         kcode = XKeysymToKeycode( display_id, XK_Right );
         evl->def[1] = (int)kcode;
         }
      }
   }

/* Sum the number of expected triggers */
nval = 0;
for ( k = 0; k < int_struct.n_int_dev; k++ )
   {
   intdev = int_struct.int_dev[k];
   nval += intdev->n_trg;
   }

/* Get the key definitions for the triggers from the description file */
n = 0;
nit = ARGSIZE;
kws_xtr( fildct, "TRG_TYPE", &nit, sarg );
if ( nit == nval )
   {
   for ( k = 0; k < int_struct.n_int_dev; k++ )
      {
      intdev = int_struct.int_dev[k];
      for ( i = 0; i < intdev->n_trg; i++ )
         {
         m = i + n;

/* Check if the trg_type begins with XK_ */
         if ( strncmp( sarg[m], "XK_", 3 ) == 0 )
            {

/* Skip over the first three characters of the string */
            p = sarg[m];
            for ( j = 0; j < 3; j++ ) p++;

/* Convert the string to a keycode and replace the existing trg->def value */
            ksym = XStringToKeysym( p );
            kcode = XKeysymToKeycode( display_id, ksym );
            trg = intdev->trg[i];
            trg->def = (int)kcode;
            }
         }
      n += intdev->n_trg;
      }
   }

return;
}

