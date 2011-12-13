/*
*+
*  Name:
*     IDIOTHER.C
*
*  Purpose:
*     Miscellaneous routines
*
*  Description:
*     Miscellaneous routines
*
*  Contents:
*     roi_rectangle
*        Draw a rectangular ROI with active corner;
*     roi_refresh
*        Refresh the ROIs
*     roi_switch
*        Switch the ROI active corner
*     lut_loc_rotate
*        Rotate the LUT using a locator
*     dis_zoom
*        Interactive zoom of display
*     dis_unzoom
*        Interactive unzoom of display
*     dis_clzoom
*        Interactive clear zoom of display
*     remove_pending
*        Remove multiple events of the same type from the event queue
*     is_motion_pending
*        Inquire if next event is a motion event
*     mem_screen_conv
*        Memory to screen coordinate conversion
*     screen_mem_conv
*        Screen to memory coordinate conversion
*     hard_cursor
*        Hardware cursor
*
*  Copyright:
*     Copyright (C) 1991, 1992, 1993 Science & Engineering Research Council.
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
*     23-MAY-1991 (NE):
*        Added stdlib.h for abs
*     29-NOV-1991 (NE):
*        Removed save_property routine
*     19-MAR-1992 (NE):
*        Added remove_pending routine
*     24-SEP-1992 (NE):
*        Added is_motion_pending routine
*     30-SEP-1992 (NE):
*        Added mem_screen_conv and screen_mem_conv routines
*      2-NOV-1992 (NE):
*        Added hard_cursor routine
*/

/* System definitions */

#include    <X11/Xlib.h>
#include    <stdlib.h>
#include    <stdio.h>

#include    "gwm.h"

/* Package definitions */

#include    "device.dep"
#include    "idi.h"
#include    "idi_err.h"
#include    "iii.h"
#include    "idistruct_e.h"
#include    "x11defs.h"
#include    "idifuncs.h"

/* Local definitions */

static int old_x_pos;

/******************************************************************************/

void roi_rectangle ( int display, int corner, int x0, int y0, int x1, int y1,
                   int roicol )

/*
*+
*  Name:
*     roi_rectangle
*
*  Purpose:
*     Draw a region of interest rectangle
*
*  Invocation:
*     roi_rectangle ( display, corner, x0, y0, x1, y1, roicol )
*
*  Description:
*     Draw a region of interest rectangle
*
*  Arguments:
*     display = int
*        Display identifier
*     corner = int
*        Active corner
*     x0 = int
*        X coordinate of first corner
*     y0 = int
*        Y coordinate of first corner
*     x1 = int
*        X coordinate of opposite corner
*     y1 = int
*        Y coordinate of opposite corner
*     roicol = int
*        Region of interest colour
*
*  Algorithm:
*     Draw a region of interest rectangle
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*     29-APR-1991 (NE):
*        Define ROI colour as table mid-point
*      8-MAY-1991 (NE):
*        Strict X library adherence
*     16-JUL-1991 (NE):
*        Pass active corner as argument rather than ROI id.
*      5-SEP-1991 (NE):
*        Draw with white pen on one bit displays
*     29-NOV-1991 (NE):
*        Draw ROI into pixmap as well
*     14-APR-1992 (NE):
*        Use cursor graphics context and plane mask
*      5-JAN-1993 (NE):
*        Only draw in window if flag is set
*     17-FEB-1993 (NE):
*        Correct the y-inversion
*/

{

/* Local variables */
int nsegments;
int screen;
int x_off, y_off, zoom;
unsigned long colpix;
Colormap cmap;
CONF_DATA *conf;
MEM_DATA *mem, *memf;
ROI_DATA *roi;

/* Local data */
/* These offsets are added onto the coordinates of the ROI vertices */
static int delta [4][16] = {
   { -20, 0, -1, 0,  0, 0, 0, -1,    0, 0, 1, 0,    0, 0, 0, -20 },
   { 0, 0, -1, 0,    0, 0, 0, 20,    20, 0, 1, 0,   0, 0, 0, 1 },
   { 0, 0, -1, 0,    0, 0, 0, -1,    0, 0, -20, 0,  0, 20, 0, 1 },
   { 0, 0, 20, 0,    0, -20, 0, -1,  0, 0, 1, 0,    0, 0, 0, 1 } };

/* Get the window identifiers from the global variables */
display_id = (Display*) device[display].vd_id ;
w_id       = (Window) device[display].wd_id ;
gccurs     = (GC) device[display].gccurs_id ;

/* Use the currently displayed pixmap */
conf = device[display].config[device[display].confid];
mem = conf->memory[device[display].pm_mem];
pixmap_id  = (Pixmap) mem->pm_id;

/* Get the window descriptors from X */
screen     = XDefaultScreen(display_id);
cmap       = ColormapOfWindow( display );

/* X defines the y coordinates from the top of the screen */
y0 = device[display].dev_ysiz - 1 - y0;
y1 = device[display].dev_ysiz - 1 - y1;

/* Subtract the delta's from the y's because of coordinate inversion */
curso0[0].x1 = x0 + delta[corner][0];
curso0[0].y1 = y0 - delta[corner][1];
curso0[0].x2 = x1 + delta[corner][2];
curso0[0].y2 = y0 - delta[corner][3];

curso0[1].x1 = x1 + delta[corner][4];
curso0[1].y1 = y0 - delta[corner][5];
curso0[1].x2 = x1 + delta[corner][6];
curso0[1].y2 = y1 - delta[corner][7];

curso0[2].x1 = x1 + delta[corner][8];
curso0[2].y1 = y1 - delta[corner][9];
curso0[2].x2 = x0 + delta[corner][10];
curso0[2].y2 = y1 - delta[corner][11];

curso0[3].x1 = x0 + delta[corner][12];
curso0[3].y1 = y1 - delta[corner][13];
curso0[3].x2 = x0 + delta[corner][14];
curso0[3].y2 = y0 - delta[corner][15];

/* Draw with a white pen on a one bit display for XOR to work properly */
colpix = ( curlut.lutpix[curlut.len/2] > 0 )
         ? curlut.lutpix[curlut.len/2] | ~device[display].pm_mask : 1;
XSetForeground( display_id, gccurs, colpix);

/* Output the rectangle */
/* Only draw in window if flag is set */
nsegments = 4 ;
if ( device[display].roi_flag < 0 )
   {
   XSetPlaneMask( display_id, gccurs, AllPlanes );
   XDrawSegments(display_id, w_id, gccurs, curso0, nsegments);
   }
else
   {

/* Refresh individual memories */
   if ( device[display].roi_flag < conf->n_mem )
      {
      memf = conf->memory[device[display].roi_flag];
      XSetPlaneMask( display_id, gccurs, memf->pm_mask );
      XDrawSegments(display_id, w_id, gccurs, curso0, nsegments);
      }
   }

/* Plot cursor into pixmap */
x_off = 0;
y_off = device[display].dev_ysiz - mem->y_size;
curso0[0].x1 = x0 + delta[corner][0] - x_off;
curso0[0].y1 = y0 - delta[corner][1] - y_off;
curso0[0].x2 = x1 + delta[corner][2] - x_off;
curso0[0].y2 = y0 - delta[corner][3] - y_off;
curso0[1].x1 = x1 + delta[corner][4] - x_off;
curso0[1].y1 = y0 - delta[corner][5] - y_off;
curso0[1].x2 = x1 + delta[corner][6] - x_off;
curso0[1].y2 = y1 - delta[corner][7] - y_off;
curso0[2].x1 = x1 + delta[corner][8] - x_off;
curso0[2].y1 = y1 - delta[corner][9] - y_off;
curso0[2].x2 = x0 + delta[corner][10] - x_off;
curso0[2].y2 = y1 - delta[corner][11] - y_off;
curso0[3].x1 = x0 + delta[corner][12] - x_off;
curso0[3].y1 = y1 - delta[corner][13] - y_off;
curso0[3].x2 = x0 + delta[corner][14] - x_off;
curso0[3].y2 = y0 - delta[corner][15] - y_off;
XSetPlaneMask( display_id, gccurs, mem->pm_mask );
XDrawSegments(display_id, pixmap_id, gccurs, curso0, nsegments);

/* Plot cursor into pixmap overlay */
if ( ~device[display].pm_mask > 0 )
   {
   mem = conf->memory[device[display].pm_memov];
   x_off = 0;
   y_off = device[display].dev_ysiz - mem->y_size;
   curso0[0].x1 = x0 + delta[corner][0] - x_off;
   curso0[0].y1 = y0 - delta[corner][1] - y_off;
   curso0[0].x2 = x1 + delta[corner][2] - x_off;
   curso0[0].y2 = y0 - delta[corner][3] - y_off;
   curso0[1].x1 = x1 + delta[corner][4] - x_off;
   curso0[1].y1 = y0 - delta[corner][5] - y_off;
   curso0[1].x2 = x1 + delta[corner][6] - x_off;
   curso0[1].y2 = y1 - delta[corner][7] - y_off;
   curso0[2].x1 = x1 + delta[corner][8] - x_off;
   curso0[2].y1 = y1 - delta[corner][9] - y_off;
   curso0[2].x2 = x0 + delta[corner][10] - x_off;
   curso0[2].y2 = y1 - delta[corner][11] - y_off;
   curso0[3].x1 = x0 + delta[corner][12] - x_off;
   curso0[3].y1 = y1 - delta[corner][13] - y_off;
   curso0[3].x2 = x0 + delta[corner][14] - x_off;
   curso0[3].y2 = y0 - delta[corner][15] - y_off;
   XSetPlaneMask( display_id, gccurs, mem->pm_mask );
   XDrawSegments(display_id, pixmap_id, gccurs, curso0, nsegments);
   }

/* Reset cursor graphics context */
XSetPlaneMask( display_id, gccurs, AllPlanes );
XFlush(display_id);

return;
}

/******************************************************************************/

void roi_refresh ( int display )

/*
*+
*  Name:
*     roi_refresh
*
*  Purpose:
*     Refresh the region of interests
*
*  Invocation:
*     roi_refresh ( display )
*
*  Description:
*     Refresh the region of interests
*
*  Arguments:
*     display = int
*        Display identifier
*
*  Algorithm:
*     Refresh the region of interests
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*/

{

/* Local variables */
int i;
int iierr;
int xmax;
int xmin;
int ymax;
int ymin;
ROI_DATA *roi;

/* Redraw those ROI's that are visible */
for ( i = 0; i < device[display].n_roi; i++ )
   {
   roi = device[display].roi[i];

/* Set the active corner to the lower left */
   roi->corner = 0;

   if ( roi->vis == 1 )
      {
      xmin = roi->x_min;
      xmax = roi->x_max;
      ymin = roi->y_min;
      ymax = roi->y_max;
      iierr = IIRWRI_C( display, roi->memid, i, xmin, ymin, xmax, ymax );
      }
   }

return;
}

/******************************************************************************/

void roi_switch ( int display, int nint, short ev_type, short ev_data,
                  int* err )

/*
*+
*  Name:
*     roi_switch
*
*  Purpose:
*     Switch the active corner of the region of interest
*
*  Invocation:
*     roi_switch ( display, nint, ev_type, ev_data, err )
*
*  Description:
*     Refresh the region of interests
*
*  Arguments:
*     display = int
*        Display identifier
*     nint = int
*        Interactor number
*     ev_type = short
*        Event type
*     ev_data = short
*        Event data
*     err = int
*        Pointer to status
*
*  Algorithm:
*     Switch the active corner of the region of interest
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*/

{

/* Local variables */
int f0;
int i;
int iierr;
int interactor_id;
int j;
int loc0;
int loc_id;
int roin;
int trg_id;
int trg0;
int xmax;
int xmin;
int ymax;
int ymin;
INT_DEV_DATA *intdev;
INTER_DATA *intdata;
LOC_DATA *loc;
ROI_DATA *roi;

/* Get the trigger descriptor for this interaction */
intdata = device[display].inter[nint];
trg0 = intdata->int_id;
get_trg( trg0, &interactor_id, &trg_id );

/* Check that this trigger has an event pending */
test_trg( ev_type, ev_data, interactor_id, trg_id, &f0 );

if ( f0 == 1 )
   {

/* Switch the roi active corner */
   roin = intdata->obj_id;
   roi = device[display].roi[roin];
   switch( roi->corner )
      {
      case 0:
         roi->corner = 1;
         break;
      case 1:
         roi->corner = 0;
         break;
      case 2:
         roi->corner = 3;
         break;
      case 3:
         roi->corner = 2;
         break;
      }

/* Draw the roi with the new corner */
   if ( roi->vis == 1 )
      {
      xmin = roi->x_min;
      xmax = roi->x_max;
      ymin = roi->y_min;
      ymax = roi->y_max;
      iierr = IIRWRI_C( display, roi->memid, roin, xmin, ymin, xmax, ymax );
      }

/* Update the locator position to reflect the new active corner */
   for (i = 0; i < device[display].n_inter; i++)
      {
      intdata = device[display].inter[i];
      if ( ( intdata->oper == II_MODIFY ) &&
           ( intdata->obj_type == II_ROI ) &&
           ( intdata->int_type == II_LOC ) )
         {
         loc0 = intdata->int_id;
         get_loc( loc0, &interactor_id, &loc_id );
         intdev = int_struct.int_dev[interactor_id];
         loc = intdev->loc[loc_id];
         switch ( roi->corner )
            {
            case 0:
               loc->x_pos = roi->x_min;
               loc->y_pos = roi->y_min;
               break;
            case 1:
               loc->x_pos = roi->x_max;
               loc->y_pos = roi->y_max;
               break;
            case 2:
               loc->x_pos = roi->x_min;
               loc->y_pos = roi->y_max;
               break;
            case 3:
               loc->x_pos = roi->x_max;
               loc->y_pos = roi->y_min;
               break;
            }
         }
      }
   }
return;
}

/******************************************************************************/

void lut_loc_rotate ( int display, int nint, short ev_type, short ev_data,
                      short pos[], int ew, int* err )

/*
*+
*  Name:
*     lut_loc_rotate
*
*  Purpose:
*     Rotate the LUT using the x-motion of the locator
*
*  Invocation:
*     lut_loc_rotate( display, nint, ev_type, ev_data, pos, ew, err )
*
*  Description:
*     Rotate the LUT using the x-motion of the locator
*
*  Arguments:
*     display = int
*        Display identifier
*     nint = int
*        Interactor number
*     ev_type = short
*        Event type
*     ev_data = short
*        Event data
*     pos = short[]
*        Locator position
*     ew = int
*        Enter window flag
*     err = int
*        Pointer to status
*
*  Algorithm:
*     Rotate the LUT using the x-motion of the locator
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*      5-MAR-1991 (NE):
*        Orignal version
*     23-MAY-1991 (NE):
*        Removed definition of abs
*     19-MAY-1992 (NE):
*        Use lut->lutpix to reference curlut entries.
*     17-FEB-1993 (NE):
*        Correct the y-inversion
*/

{

/* Local variables */
INTER_DATA *intdata;
INT_DEV_DATA *intdev;
LOC_DATA *loc;
LUT_DATA *lut;
int i;
int j;
int interactor_id;
int loc0;
int loc_id;
int f0;
int tmpr;
int tmpg;
int tmpb;
int drot;
int rotdir;
int zf;

/* Get the locator descriptor for this interaction */
intdata = device[display].inter[nint];
loc0 = intdata->int_id;
get_loc( loc0, &interactor_id, &loc_id );
intdev = int_struct.int_dev[interactor_id];
loc = intdev->loc[loc_id];
lut = device[display].lookup[intdata->obj_id];

/* Reset the rotation offset */
if ( ew == 1 )
   {
   loc->x_pos = pos[0];
   loc->y_pos = device[display].dev_ysiz - 1 - pos[1];
   old_x_pos = loc->x_pos;
   return;
   }

/* Check that this locator has an event pending */
zf = device[display].zoom;
test_loc( display, zf, ev_type, ev_data, pos, interactor_id, loc_id, &f0 );

if ( f0 == 1 )
   {

/* Establish the amount and direction of the rotation */
   drot = loc->x_pos - old_x_pos;
   old_x_pos = loc->x_pos;
   if (drot != 0)
      rotdir = drot / abs (drot);
   else
      rotdir = 1;

/* Rotate the LUT forwards */
   if (rotdir == 1)
      {
      for (i = 0; i < drot; i++)
         {
         tmpr = curlut.lutr[lut->lutpix[curlut.len - 1]];
         tmpg = curlut.lutg[lut->lutpix[curlut.len - 1]];
         tmpb = curlut.lutb[lut->lutpix[curlut.len - 1]];
         for (j = curlut.len - 1; j > 0 ; j--)
            {
            curlut.lutr[lut->lutpix[j]] = curlut.lutr[lut->lutpix[j - 1]];
            curlut.lutg[lut->lutpix[j]] = curlut.lutg[lut->lutpix[j - 1]];
            curlut.lutb[lut->lutpix[j]] = curlut.lutb[lut->lutpix[j - 1]];
            }
         curlut.lutr[lut->lutpix[0]] = tmpr;
         curlut.lutg[lut->lutpix[0]] = tmpg;
         curlut.lutb[lut->lutpix[0]] = tmpb;
         }
      }

/* Rotate the LUT backwards */
   else
      {
      for (i = 0; i < abs (drot); i++)
         {
         tmpr = curlut.lutr[lut->lutpix[0]];
         tmpg = curlut.lutg[lut->lutpix[0]];
         tmpb = curlut.lutb[lut->lutpix[0]];
         for (j = 0; j < curlut.len - 1; j++)
            {
            curlut.lutr[lut->lutpix[j]] = curlut.lutr[lut->lutpix[j + 1]];
            curlut.lutg[lut->lutpix[j]] = curlut.lutg[lut->lutpix[j + 1]];
            curlut.lutb[lut->lutpix[j]] = curlut.lutb[lut->lutpix[j + 1]];
            }
         curlut.lutr[lut->lutpix[curlut.len - 1]] = tmpr;
         curlut.lutg[lut->lutpix[curlut.len - 1]] = tmpg;
         curlut.lutb[lut->lutpix[curlut.len - 1]] = tmpb;
         }
      }

/* Send the new LUT to the display */
   wr_lut(display);
   }

/* Reset the locator position if the scroll limit was exceeded */
else
   {
   if ( intdata->int_type == II_LOC && intdev->descr == II_MOUSE )
      {
      loc->x_pos = pos[0];
      loc->y_pos = device[display].dev_ysiz - 1 - pos[1];
      }
   }

*err = II_SUCCESS;

return;
}

/******************************************************************************/

void dis_zoom ( int display, int nint, short ev_type, short ev_data, int* err )

/*
*+
*  Name:
*     dis_zoom
*
*  Purpose:
*     Interactive zoom of display
*
*  Invocation:
*     dis_zoom( display, nint, ev_type, ev_data, err )
*
*  Description:
*     Interactive zoom of display
*
*  Arguments:
*     display = int
*        Display identifier
*     nint = int
*        Interactor number
*     ev_type = short
*        Event type
*     ev_data = short
*        Event data
*     err = int
*        Pointer to status
*
*  Algorithm:
*     Interactive zoom of display
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*      8-MAR-1991 (NE):
*        Orignal version
*/

{

/* Local variables */
int f0;
int interactor_id;
int trg_id;
int trg0;
int xoff;
int yoff;
int zoomf;
INTER_DATA *intdata;

/* Get the tirgger descriptor for this interaction */
intdata = device[display].inter[nint];
trg0 = intdata->int_id;
get_trg( trg0, &interactor_id, &trg_id );

/* Check that this trigger has an event pending */
test_trg( ev_type, ev_data, interactor_id, trg_id, &f0 );

if (f0 == 1)
   {
   switch (intdata->obj_type)
      {
      case II_DISPLAY:
         xoff = (int)device[display].x_scroll;
         yoff = (int)device[display].y_scroll;
         zoomf = device[display].zoom + 1;
         *err = IIZWZP_C( display, xoff, yoff, zoomf );
         break;
      }

/* The ROI locator position has to be reset after a zoom */
   roi_reset = 1;
   }
return;
}

/******************************************************************************/

void dis_unzoom ( int display, int nint, short ev_type, short ev_data,
                  int* err )

/*
*+
*  Name:
*     dis_unzoom
*
*  Purpose:
*     Interactive unzoom of display
*
*  Invocation:
*     dis_unzoom( display, nint, ev_type, ev_data, err )
*
*  Description:
*     Interactive unzoom of display
*
*  Arguments:
*     display = int
*        Display identifier
*     nint = int
*        Interactor number
*     ev_type = short
*        Event type
*     ev_data = short
*        Event data
*     err = int
*        Pointer to status
*
*  Algorithm:
*     Interactive unzoom of display
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*      8-MAR-1991 (NE):
*        Orignal version
*/

{

/* Local variables */
int f0;
int interactor_id;
int trg_id;
int trg0;
int xoff;
int yoff;
int zoomf;
INTER_DATA *intdata;

/* Get the tirgger descriptor for this interaction */
intdata = device[display].inter[nint];
trg0 = intdata->int_id;
get_trg( trg0, &interactor_id, &trg_id );

/* Check that this trigger has an event pending */
test_trg( ev_type, ev_data, interactor_id, trg_id, &f0 );

if (f0 == 1)
   {
   switch (intdata->obj_type)
      {
      case II_DISPLAY:
         xoff = (int)device[display].x_scroll;
         yoff = (int)device[display].y_scroll;
         zoomf = device[display].zoom - 1;
         if ( zoomf == 0 )
            zoomf = 1;
         *err = IIZWZP_C( display, xoff, yoff, zoomf );
         break;
      }

/* The ROI locator position has to be reset after a zoom */
   roi_reset = 1;
   }
return;
}

/******************************************************************************/

void dis_clzoom ( int display, int nint, short ev_type, short ev_data,
                  int* err )

/*
*+
*  Name:
*     dis_clzoom
*
*  Purpose:
*     Interactive clear zoom of display
*
*  Invocation:
*     dis_clzoom( display, nint, ev_type, ev_data, err )
*
*  Description:
*     Interactive clear zoom of display
*
*  Arguments:
*     display = int
*        Display identifier
*     nint = int
*        Interactor number
*     ev_type = short
*        Event type
*     ev_data = short
*        Event data
*     err = int
*        Pointer to status
*
*  Algorithm:
*     Interactive clear zoom of display
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*      8-MAR-1991 (NE):
*        Orignal version
*/

{

/* Local variables */
int f0;
int interactor_id;
int trg_id;
int trg0;
int xoff;
int yoff;
int zoomf;
INTER_DATA *intdata;

/* Get the tirgger descriptor for this interaction */
intdata = device[display].inter[nint];
trg0 = intdata->int_id;
get_trg( trg0, &interactor_id, &trg_id );

/* Check that this trigger has an event pending */
test_trg( ev_type, ev_data, interactor_id, trg_id, &f0 );

if (f0 == 1)
   {
   switch (intdata->obj_type)
      {
      case II_DISPLAY:
         xoff = (int)device[display].x_scroll;
         yoff = (int)device[display].y_scroll;
         zoomf = 1;
         *err = IIZWZP_C( display, xoff, yoff, zoomf );
         break;
      }

/* The ROI locator position has to be reset after a zoom */
   roi_reset = 1;
   }
return;
}

/******************************************************************************/

void remove_pending( int display, short ev_type )

/*
*+
*  Name:
*     remove_pending
*
*  Purpose:
*     Remove multiple events of the same type from the event queue
*
*  Invocation:
*     remove_pending( display, ev_type )
*
*  Description:
*     Remove multiple events of the same type from the event queue.
*     Events of the given type are removed from the front of the queue
*     until an event of a different type is met.
*
*  Arguments:
*     display = int
*        Display identifier
*     ev_type = short
*        Type of event to remove
*
*  Algorithm:
*     Remove multiple events of the same type from the event queue.
*     Events of the given type are removed from the front of the queue
*     until an event of a different type is met.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     19-MAR-1992 (NE):
*        Orignal version
*/

{

/* Local variables */
int count;
int j;

display_id = (Display*) device[display].vd_id;

/* Inquire the number of events on the event queue */
count = XEventsQueued( display_id, QueuedAlready );

/* Leave at least one event on the queue */
if ( count > 1 )
   {

/* Peek at the first event on the queue to determine it's type */
   XPeekEvent( display_id, &evw_data );

/* Loop until either there is only one event left or */
/* until the event type is not the same as the given one */
   j = 1;
   while ( ( j < count ) && ( (short)evw_data.type == ev_type ) )
      {

/* Remove the current event from the queue */
      XNextEvent( display_id, &evw_data );

/* Peek at the next event on the queue to determine it's type */
      XPeekEvent( display_id, &evw_data );
      j++;
      }
   }

return;
}

/******************************************************************************/

int is_motion_pending( int display, int x_pos, int y_pos )

/*
*+
*  Name:
*     is_motion_pending
*
*  Purpose:
*     Inquire if next event is a motion event
*
*  Invocation:
*     is_motion_pending( display, x_pos, y_pos )
*
*  Description:
*     If the next event is a motion event and it the position does not
*     exceed the maximum allowable then return true otherwise return false.
*
*  Arguments:
*     display = int
*        Display identifier
*     x_pos = int
*        Current locator position
*     y_pos = int
*        Current locator position
*
*  Algorithm:
*     If there is a pending event on the queue then peek at it.
*     If the next event is a motion event and it the position does not
*     exceed the maximum allowable then return true otherwise return false.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     24-SEP-1992 (NE):
*        Orignal version
*     17-FEB-1993 (NE):
*        Correct the y-inversion
*/

{

/* Local variables */
int count;

display_id = (Display*) device[display].vd_id;

/* Inquire the number of events on the event queue */
count = XEventsQueued( display_id, QueuedAlready );

/* If there are no pending events then return false */
if ( count < 1 )
   {
   return( 0 );
   }

/* Otherwise peek at the first event to determine it's type */
else
   {
   XPeekEvent( display_id, &evw_data );

/* Test to see if the event is a motion event and the */
/* position does not exceed the maximum allowable */
   return( ( evw_data.type == MotionNotify ) &&
           ( abs( x_pos - evw_data.xmotion.x ) < LOCX_SLIP ) &&
           ( abs( y_pos + evw_data.xmotion.y + 1 -
                  device[display].dev_ysiz ) < LOCY_SLIP ) );
   }
}

/******************************************************************************/

void mem_screen_conv ( int display, int memid, int xmem, int ymem, int* xscr,
                       int* yscr )

/*
*+
*  Name:
*     mem_screen_conv
*
*  Purpose:
*     Memory to screen coordinate conversion
*
*  Invocation:
*     mem_screen_conv( display, memid, xmem, ymem, xscr, yscr )
*
*  Description:
*     Memory to screen coordinate conversion
*
*  Arguments:
*     display = int
*     memid = int
*     xmem = int
*     ymem = int
*     xscr = int
*     yscr = int
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     30-SEP-1992 (NE):
*        Orignal version
*/

{

/* Local Variables */
int curconf;
int xoff, yoff, zoom;
CONF_DATA *conf;
MEM_DATA *mem;

/* display zooms around centre of screen */
zoom = device[display].zoom;
xoff = (int)( device[display].x_scroll * zoom ) -
       ( zoom - 1 ) * ( device[display].dev_xsiz / 2 );
yoff = (int)( device[display].y_scroll * zoom ) -
       ( zoom - 1 ) * ( device[display].dev_ysiz / 2 );

/* coordinates are with respect to screen */
if (memid == -1)
   {
   *xscr = xmem * zoom + xoff;
   *yscr = ymem * zoom + yoff;
   }

/* coordinates are with respect to specified memory */
else
   {
   curconf = device[display].confid;
   conf = device[display].config[curconf];
   mem = conf->memory[memid];
   *xscr = ( xmem * mem->zoom + (int)mem->zoom_xsc ) * zoom + xoff;
   *yscr = ( ymem * mem->zoom + (int)mem->zoom_ysc ) * zoom + yoff;
   }

return;
}

/******************************************************************************/

void screen_mem_conv ( int display, int memid, int xscr, int yscr, int* xmem,
                       int* ymem )

/*
*+
*  Name:
*     screen_mem_conv
*
*  Purpose:
*     Screen to memory coordinate conversion
*
*  Invocation:
*     screen_mem_conv( display, memid, xscr, yscr, xmem, ymem )
*
*  Description:
*     Screen to memory coordinate conversion
*
*  Arguments:
*     display = int
*     memid = int
*     xscr = int
*     yscr = int
*     xmem = int
*     ymem = int
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     30-SEP-1992 (NE):
*        Orignal version
*/

{

/* Local Variables */
int curconf;
int xoff, yoff, zoom;
CONF_DATA *conf;
MEM_DATA *mem;

/* display zooms around centre of screen */
zoom = device[display].zoom;
xoff = (int)( device[display].x_scroll * zoom ) -
       ( zoom - 1 ) * ( device[display].dev_xsiz / 2 );
yoff = (int)( device[display].y_scroll * zoom ) -
       ( zoom - 1 ) * ( device[display].dev_ysiz / 2 );

/* coordinates are with respect to screen */
if (memid == -1)
   {
   *xmem = ( xscr - xoff ) / zoom;
   *ymem = ( yscr - yoff ) / zoom;
   }

/* coordinates are with respect to specified memory */
else
   {
   curconf = device[display].confid;
   conf = device[display].config[curconf];
   mem = conf->memory[memid];
   *xmem = ( ( xscr - xoff ) / zoom - (int)mem->zoom_xsc ) / mem->zoom;
   *ymem = ( ( yscr - yoff ) / zoom - (int)mem->zoom_ysc ) / mem->zoom;
   }

return;
}

/******************************************************************************/

void hard_cursor ( int display, int func, int xin, int yin, int* xout,
                   int* yout )

/*
*+
*  Name:
*     hard_cursor
*
*  Purpose:
*     Hardware cursor
*
*  Invocation:
*     hard_cursor( display, func, xin, yin, xout, yout )
*
*  Description:
*     Hardware cursor
*
*  Arguments:
*     display = int
*     func = int
*     xin = int
*     yin = int
*     xout = int
*     yout = int
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink, RAL)
*
*  History:
*     26-OCT-1992 (NE):
*        Orignal version
*     17-FEB-1993 (NE):
*        Correct the y-inversion
*     13-OCT-1993 (DLT):
*        Change type of bitmaps to unsigned
*      3-MAR-1998 (DLT):
*        Add correction for scrolled GWM window
*/

{

/* Local Variables */
int xroot, xx, yroot, yy, x_off, y_off;
unsigned int state_mask;
Window child, root;
XColor fore_color, back_color;
Pixmap cursor_mask, cursor_pixmap;

#define cursor_width 16
#define cursor_height 16
#define cursor_x_hot 7
#define cursor_y_hot 7
static unsigned char cursor_bits[] = {
   0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
   0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0xff, 0x7f,
   0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
   0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x00, 0x00};
static unsigned char mask_bits[] = {
   0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01,
   0xc0, 0x01, 0xc0, 0x01, 0xff, 0x7f, 0xff, 0x7f,
   0xff, 0x7f, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01,
   0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0x00, 0x00};

display_id = (Display*) device[display].vd_id ;
w_id       = (Window) device[display].wd_id ;
screen	   = XDefaultScreen(display_id);
cmap       = ColormapOfWindow( display );

switch ( func )
   {

/* Undefine cursor */
   case HC_UNDO:
      {
      XDefineCursor( display_id, w_id, cur_def );
      break;
      }

/* Define cursor */
   case HC_DO:
      {
      back_color.pixel = XWhitePixel( display_id, screen );
      XQueryColor( display_id, DefaultColormap( display_id, screen),
          &back_color );
      XAllocColor( display_id, cmap, &back_color);
      fore_color.pixel = XBlackPixel( display_id, screen );
      XQueryColor( display_id, DefaultColormap( display_id, screen),
          &fore_color );
      XAllocColor( display_id, cmap, &fore_color);
      cursor_pixmap = XCreateBitmapFromData( display_id, w_id,
                        (char*)cursor_bits, cursor_width, cursor_height );
      cursor_mask = XCreateBitmapFromData( display_id, w_id, (char*)mask_bits,
                                           cursor_width, cursor_height );
      cur_def_int = XCreatePixmapCursor( display_id, cursor_pixmap, cursor_mask,
               &fore_color, &back_color, cursor_x_hot, cursor_y_hot );
      XFreePixmap( display_id, cursor_pixmap );
      XFreePixmap( display_id, cursor_mask );
      XDefineCursor( display_id, w_id, cur_def_int );
      break;
      }

/* Write cursor position */
   case HC_WRITE:
      {
      yin = device[display].dev_ysiz - 1 - yin;
      GWM_GetScroll( display_id, w_id, &x_off, &y_off );
      XWarpPointer( display_id, None, w_id, 0, 0, 0, 0, xin + x_off,
                    yin + y_off);
      break;
      }

/* Read cursor position */
   case HC_READ:
      {
      XQueryPointer( display_id, w_id, &root, &child, &xroot, &yroot,
                     &xx, &yy, &state_mask );
      GWM_GetScroll( display_id, w_id, &x_off, &y_off );
      *xout = xx - x_off;
      *yout = device[display].dev_ysiz - 1 - yy + y_off;
      break;
      }

  }

XFlush(display_id);

return;
}

