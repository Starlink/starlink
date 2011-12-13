/*
*+
*  Name:
*     IDILOCAL.C
*
*  Purpose:
*     Device Dependent Routines
*
*  Description:
*     Device Dependent Routines
*
*  Contents:
*     local_init
*        Device Dependent Init;
*     disp_init
*        Local Display Init;
*     acq_disp
*        Acquire Display;
*     rel_disp
*        Release Display;
*     acq_lut
*        Acquire XWindows Color Cells;
*     icol
*        Code color;
*     icol1
*        Decode color;
*     cl_display
*        Clear Display;
*     wr_lut
*        Write XWindows LUT;
*     wr_lut_gwm
*        Write XWindows LUT;
*     get_slicol
*        Get Default Color for LUT Slice Operation;
*     smv
*        Set Memory Visibility : zoom = 1;
*     smv_z
*        Set Memory Visibility : zoom > 1;
*     smv_uz
*        Set Memory Visibility : zoom < 0;
*     int_enable
*        Enable Interaction;
*     int_disable
*        Disable Intaraction;
*     exit_trg_enable
*        Enable Exit Trigger;
*     exit_trg_disable
*        Disable Exit Trigger;
*     wait_int
*        Wait for Interaction;
*     test_loc
*        Test Locator Status;
*     test_evl
*        Test Evaluator Status;
*     test_trg
*        Test Trigger Status;
*     idi__bar
*        Intensity Bar;
*     cross_hair
*        Draw Cross-Hair Cursor;
*     cross
*        Draw Cross Cursor;
*     rectangle
*        Draw Rectangular ROI;
*     polyline_d
*        Draw Polyline on Display Bitmap;
*     text_d
*        Write Text on Display Bitmap;
*     ColormapOfWindow
*        Return Colormap associated with a window
*
*  Copyright:
*     Copyright (C) 1988, 1991, 1992, 1993, 1994 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink RAL)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Added font names, iii.h and idi_err.h
*      8-MAY-1991 (NE):
*        Put font names in device.dep
*     23-MAY-1991 (NE):
*        Added stdlib.h for free
*     20-JUN-1991 (NE):
*        Put inter_mask in iii.h
*     02-MAR-1994 (DLT):
*        Add ColormapOfWindow
*/

/* System definitions */

# include    <stdlib.h>
# include    <stdio.h>
# include    <string.h>
# include    <math.h>

# include    "gwm.h"

/* Package definitions */

# include    "device.dep"
# include    "kwm.h"
# include    "idi.h"
# include    "idi_err.h"
# include    "idistruct_e.h"
# include    "x11defs.h"
# include    "iii.h"
# include    "idifuncs.h"

/* Local definitions */

static  int   shift_pr ;
static  int   key_shift_l ;
static  int   key_shift_r ;
static  int   up_ls, left_ls, right_ls, down_ls;

static  int   arg[ARGSIZE];

struct bm_window
   {
   int x_coord;
   int y_coord;
   int x_size;
   int y_size;
   };
typedef struct bm_window BM_WINDOW;

struct bm_origin
   {
   int x_coord;
   int y_coord;
   };
typedef struct bm_origin BM_ORIGIN;

/******************************************************************************/

void local_init ( int display )

/*
*+
*  Name:
*     local_init
*
*  Purpose:
*     Device Dependent Init
*
*  Invocation:
*     local_init( display )
*
*  Description:
*     Device Dependent Init
*
*  Arguments:
*     display = int
*        Display identifier
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink, RAL)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Removed redundant code
*     22-AUG-1991 (NE):
*        Removed inquiry of LUT length
*     14-OCT-1993 (DLT):
*        Remove redundent lutlen argument to acq_lut
*/

{

display_id = (Display*) device[display].vd_id;
w_id       = (Window) device[display].wd_id;

cur_id  = 34 ;
cur_def = XCreateFontCursor (display_id, cur_id);
cur_id  = 46 ;
cur_def_int = XCreateFontCursor (display_id, cur_id);
XDefineCursor (display_id, w_id, cur_def);

XFlush(display_id);

acq_lut (display);

return;
}

/******************************************************************************/

void disp_init ( int display, char wind[], int xdisp, int xwind, int* depth,
                 int* lutlen, int* idierr )

/*
*+
*  Name:
*     disp_init
*
*  Purpose:
*     Local Display Init
*
*  Invocation:
*     disp_init( display, wind, xdisp, xwind, idierr )
*
*  Description:
*     Local Display Init
*
*  Arguments:
*     display = int
*     wind = char[]
*     xdisp = int
*     xwind = int
*     depth = int
*     lutlen = int
*     idierr = int
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink RAL)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Set up asynchronous event to refresh on display exposure.
*        Call kwm_ routines instead of kwi_ routines.
*      8-MAY-1991 (NE):
*        Strict X library adherence.
*     19-JUN-1991 (NE):
*        Move call to set up asynchronous event to init_refr
*     25-NOV-1991 (NE):
*        Added depth and lutlen arguments
*     27-NOV-1991 (NE):
*        Added plane mask
*     14-APR-1992 (NE):
*        Use all planes for cursor graphics context
*     14-MAY-1992 (NE):
*        Set overlay flag
*     15-APR-1993 (DLT):
*        Use unsigned long for pixels
*     11-MAR-1994 (DLT):
*        Free lut array after getting length
*/

{

/* Local Variables */
char   filwnd[256];
int    nit;
unsigned long *pixels;
XWindowAttributes winatt;

/* get virtual display parameters from memory file */
nit = ARGSIZE;
kwm_xtr (filwnd, wind, &nit, arg);
if (nit == 0)
   {
   *idierr = DISWNDERR;
   return;
   }

device [display].bitmap   = arg[0] ;

display_id = (Display*) xdisp;
w_id       = (Window) xwind;
screen     = XDefaultScreen (display_id);
white      = XWhitePixel (display_id, screen);
black      = XBlackPixel (display_id, screen);

/* Inquire the plane mask from GWM, otherwise set it to its default */
if ( GWM_GetOvMask( display_id, w_id, &device[display].pm_mask ) != 0 )
   device[display].pm_mask = AllPlanes;

if ( ~device[display].pm_mask > 0 )
   device[display].overlay = 1;
else
   device[display].overlay = 0;

gcima = XCreateGC(display_id, w_id, 0, &values_ima);
XSetFunction   (display_id, gcima, GXcopy);
XSetBackground (display_id, gcima, white);
XSetForeground (display_id, gcima, black);
XSetGraphicsExposures( display_id, gcima, 0 );
XSetPlaneMask( display_id, gcima, device[display].pm_mask );

gcdraw = XCreateGC(display_id, w_id, 0, &values_draw);
XSetFunction   (display_id, gcdraw, GXxor);
XSetBackground (display_id, gcdraw, black);
XSetForeground (display_id, gcdraw, white);
XSetGraphicsExposures( display_id, gcdraw, 0 );
XSetPlaneMask( display_id, gcdraw, device[display].pm_mask );

gccurs = XCreateGC(display_id, w_id, 0, &values_curs);
XSetFunction   (display_id, gccurs, GXxor);
XSetBackground (display_id, gccurs, black);
XSetForeground (display_id, gccurs, white);
XSetGraphicsExposures( display_id, gccurs, 0 );
XSetPlaneMask( display_id, gccurs, AllPlanes );

XFlush(display_id);

device[display].gcima_id  = (int) gcima ;
device[display].gcdraw_id = (int) gcdraw ;
device[display].gccurs_id = (int) gccurs ;

/* Inquire the memory depth from X */
XGetWindowAttributes( display_id, w_id, &winatt);
*depth = winatt.depth;

/* Inquire the LUT length from GWM */
*idierr = GWM_GetColTable( display_id, w_id, &pixels, lutlen );
if (*idierr==0) free( pixels );

return;
}

/******************************************************************************/

void acq_disp ( int display )

/*
*+
*  Name:
*     acq_disp
*
*  Purpose:
*     Acquire Display
*
*  Invocation:
*     acq_disp( display )
*
*  Description:
*     Acquire Display
*
*  Arguments:
*     display = int
*        Display identifier
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*/

{
return;
}

/******************************************************************************/

void rel_disp ( int display )

/*
*+
*  Name:
*     rel_disp
*
*  Purpose:
*     Release Display
*
*  Invocation:
*     rel_disp( display )
*
*  Description:
*     Release Display
*
*  Arguments:
*     display = int
*        Display identifier
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*/

{
return;
}

/******************************************************************************/

void acq_lut ( int display )

/*
*+
*  Name:
*     acq_lut
*
*  Purpose:
*     Acquire Color Cells
*
*  Invocation:
*     acq_lut( display )
*
*  Description:
*     Acquire Color Cells
*
*  Arguments:
*     display = int
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink RAL)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     23-APR-1991 (NE):
*        Remove redundant call to AllocColorCells and
*        use the predefined colours if the allocation fails
*      8-MAY-1991 (NE):
*        Strict X library adherence.
*     13-MAY-1991 (NE):
*        Inquire colours if LUT is fixed
*     22-AUG-1991 (NE):
*        Inquire colour entries from GWM
*     14-MAY-1992 (NE):
*        Use curlut.nalloc to indicate number of allocated pens.
*        Inquire the overlay colours and intialise curlut.id.
*     15-APR-1993 (DLT):
*        Use unsigned long for pixels
*     14-OCT-1993 (DLT):
*        Remove redundent lutlen argument
*     11-MAR-1994 (DLT):
*        Free pixel array
*/

{

/* Local Variables */
unsigned long *pixels;
int i, j, ncolors, value;

display_id = (Display*) device[display].vd_id;
w_id = (Window) device[display].wd_id;

/* Inquire the allocated colour entries from GWM */
curlut.id = 0;
GWM_GetColTable( display_id, w_id, &pixels, &ncolors );

/* Remember the number of allocated colours allowing for the overlay plane */
if ( device[display].overlay )
   curlut.nalloc = ncolors * 2;
else
   curlut.nalloc = ncolors;

/* Assume that if there are two or less colours the LUT is fixed */
if ( ncolors <= 2 )
   curlut.nalloc = 0;

/* Store the lowest colour entry as the LUT offset */
curlut.len = ncolors;
curlut.off = *pixels;
j = 0;

/* Extract the pen colours from the list returned from GWM */
for( i = 0; i < curlut.len; i++ )
   {
   value = pixels[i];
   if ( value < curlut.off ) curlut.off = value;
   defs[j++].pixel = value;

/* If there is an overlay plane the extract the extra pen numbers */
   if ( device[display].overlay )
      defs[j++].pixel = value | ~device[display].pm_mask;
   }

/* Inquire the predefined colours */
cmap = ColormapOfWindow( display );
XQueryColors( display_id, cmap, defs, j );

/* Put the values in the current LUT */
for ( i = 0; i < j; i++ )
   {
   curlut.lutpix[i] = defs[i].pixel;
   curlut.lutr[i] = (int) defs[i].red;
   curlut.lutg[i] = (int) defs[i].green;
   curlut.lutb[i] = (int) defs[i].blue;
   }

free (pixels);
return;
}

/******************************************************************************/

int icol ( double lutval )

/*
*+
*  Name:
*     icol
*
*  Purpose:
*     Code Color
*
*  Invocation:
*     icol( lutval )
*
*  Description:
*     Code Color
*
*  Arguments:
*     lutval = float
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*/

{
return ( (int)(65535. * lutval) );
}

/******************************************************************************/

float icol1 ( int color )

/*
*+
*  Name:
*     icol1
*
*  Purpose:
*     Decode Color
*
*  Invocation:
*     icol1( color )
*
*  Description:
*     Decode Color
*
*  Arguments:
*     color = int
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*/

{

/* Local Variables */
float col;

col = color;
return (col / 65535.);
}

/******************************************************************************/

void cl_display ( int display, int bck )

/*
*+
*  Name:
*     cl_display
*
*  Purpose:
*     Clear Display
*
*  Invocation:
*     cl_display( display, bck )
*
*  Description:
*     Clear Display
*
*  Arguments:
*     display = int
*     bck = int
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     28-AUG-1991 (NE):
*        Added call to clear_pixmap
*/

{

/* Local Variables */
int val;

Display   *disp;
Window    wnd;

display_id = (Display*) device[display].vd_id;
w_id = (Window) device[display].wd_id;

/* Clear the window */
val = curlut.lutpix[bck];
XSetWindowBackground (display_id, w_id, val);
XClearWindow (display_id, w_id);

XFlush (display_id);

/* Clear the pixmap */
clear_pixmap( display, device[display].pm_mem, bck, AllPlanes );

return;
}

/******************************************************************************/

void wr_lut ( int display )

/*
*+
*  Name:
*     wr_lut
*
*  Purpose:
*     Write XWindows LUT
*
*  Invocation:
*     wr_lut( display )
*
*  Description:
*     Load screen look up table
*
*  Arguments:
*     display = int
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     14-MAY-1992 (NE):
*        Define the number of colours with curlut.nalloc
*/

{

/* Local Variables */
int ncolors;
int i;

display_id = (Display*) device[display].vd_id;

ncolors = curlut.nalloc;

for ( i = 0; i < ncolors; i++ )
   {
   defs[i].pixel = curlut.lutpix[i];
   defs[i].red   = (unsigned short) curlut.lutr[i];
   defs[i].green = (unsigned short) curlut.lutg[i];
   defs[i].blue  = (unsigned short) curlut.lutb[i];
   defs[i].flags = DoRed | DoGreen | DoBlue;
   }

XStoreColors( display_id, cmap, defs, ncolors );
XFlush( display_id );

return;
}

/******************************************************************************/

void wr_lut_gwm ( int display )

/*
*+
*  Name:
*     wr_lut_gwm
*
*  Purpose:
*     Write XWindows LUT
*
*  Invocation:
*     wr_lut_gwm( display )
*
*  Description:
*     Inform GWM of a change to the screen look up table
*
*  Arguments:
*     display = int
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     28-AUG-1991 (NE):
*        Orignal version
*     14-MAY-1992 (NE):
*        Define the number of colours with curlut.nalloc
*/

{

/* Local Variables */
int ncolors;
int i;

display_id = (Display*) device[display].vd_id;
w_id = (Window) device[display].wd_id;

ncolors = curlut.nalloc;

for (i=0; i<ncolors; i++)
   {
   GWM_SetColour( display_id, w_id, curlut.lutpix[i], curlut.lutr[i],
                  curlut.lutg[i], curlut.lutb[i] );
   }

XFlush (display_id);

return;
}

/******************************************************************************/

void get_slicol ( int* r, int* g, int* b )

/*
*+
*  Name:
*     get_slicol
*
*  Purpose:
*     Get default color for LUT slice
*
*  Invocation:
*     get_slicol( r, g, b )
*
*  Description:
*     Get default color for LUT slice
*
*  Arguments:
*     r = int
*     g = int
*     b = int
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*/

{

/* Default : GREEN */

*r = 0;
*g = 65535;
*b = 0;

return;
}

/******************************************************************************/

void smv ( int display, int memid )

/*
*+
*  Name:
*     smv
*
*  Purpose:
*     Set memory visibility : zoom = 1
*
*  Invocation:
*     smv( display, memid )
*
*  Description:
*     Set memory visibility : zoom = 1
*
*  Arguments:
*     display = int
*     memid = int
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Removed redundant code
*      8-MAY-1991 (NE):
*        Strict X library adherence.
*     16-APR-1992 (NE):
*        Set graphics context plane mask
*/

{

/* Local Variables */
int    i, j, curconf, bmsize;
unsigned int    dpth;
unsigned char   *curbm;

int pippo, pippo1;

Pixmap  PicturePixmap ;

BM_ORIGIN  bm1;
BM_WINDOW  bm0;
CONF_DATA  *conf;
MEM_DATA    *mem;

display_id = (Display*) device[display].vd_id;
w_id       = (Window) device[display].wd_id;
gcima      = (GC) device[display].gcima_id;
screen     = XDefaultScreen(display_id);
visual     = VisualOfWindow( display );
dpth       = XDisplayPlanes(display_id, screen);

curconf = device[display].confid;
conf = device [display].config[curconf];
mem = conf->memory[memid];


bm0.x_coord = mem->x_v_off;
bm0.y_coord = ((mem->y_scroll + mem->y_size) < device[display].dev_ysiz) ? 0 :
               mem->y_scroll + mem->y_size - device[display].dev_ysiz;
bm0.x_size  = mem->x_v_size;
bm0.y_size  = mem->y_v_size;
                            /* Set target (display) bitmap window
                             (with Y inversion)                */

bm1.x_coord = ((int)(mem->x_scroll + device[display].x_scroll) > 0) ?
              (int)(mem->x_scroll + device[display].x_scroll) : 0;
bm1.y_coord = device[display].dev_ysiz - mem->y_v_size -
              (((int)(mem->y_scroll + device[display].y_scroll) > 0) ?
               (int)(mem->y_scroll + device[display].y_scroll) : 0);



curbm = mem->mmbm;

ima = XCreateImage( display_id, visual, dpth, ZPixmap,
               0, (char*)curbm, mem->x_size, mem->y_size, 8, mem->x_size);

XFlush(display_id);

XSetPlaneMask( display_id, gcima, mem->pm_mask );
XPutImage(display_id, w_id, gcima, ima, bm0.x_coord, bm0.y_coord,
          bm1.x_coord, bm1.y_coord, ima->width, ima->height) ;

return;
}

/******************************************************************************/

void smv_z ( int display, int memid, int zoom, int x0, int y0, int xs0,
             int ys0, int x1, int y1, int xs1, int ys1 )

/*
*+
*  Name:
*     smv_z
*
*  Purpose:
*     Set memory visibility : zoom > 1
*
*  Invocation:
*     smv_z( display, memid, zoom, x0, y0, xs0, ys0, x1, y1, xs1, ys1 )
*
*  Description:
*     Set memory visibility : zoom > 1
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     x0 = int
*        Origin coordinates for main memory bitmap (source)
*     y0 = int
*        Origin coordinates for main memory bitmap (source)
*     xs0 = int
*        Sub-image main memory bitmap dimensions (source)
*     ys0 = int
*        Sub-image main memory bitmap dimensions (source)
*     x1 = int
*        Origin coordinates for display bitmap (target)
*     y1 = int
*        Origin coordinates for display bitmap (target)
*     xs1 = int
*        Sub-image display bitmap dimensions (target)
*     ys1 = int
*        Sub-image display bitmap dimensions (target)
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Fixed bug with curbm0 and removed redundant code
*      8-MAY-1991 (NE):
*        Strict X library adherence.
*     16-APR-1992 (NE):
*        Set graphics context plane mask
*/

{

/* Local Variables */
int    i, j, l, k;
int    curconf, bmsize;

unsigned char       *tmpbm0 , *tmpbm;
unsigned char       *curbm0 , *curbm;
unsigned int        dpth;

Pixmap  PicturePixmap ;


BM_WINDOW  bm0, bm1;
CONF_DATA  *conf;
MEM_DATA   *mem;

display_id = (Display*) device[display].vd_id;
w_id       = (Window) device[display].wd_id;
gcima      = (GC) device[display].gcima_id;
screen     = XDefaultScreen(display_id);
visual     = VisualOfWindow( display );
dpth       = XDisplayPlanes(display_id, screen);

curconf = device[display].confid;
conf = device [display].config[curconf];
mem = conf->memory[memid];

                            /* Set source bitmap window
                              (with Y inversion)	      */

bm0.x_coord = x0;
bm0.y_coord = y0;
bm0.x_size  = xs0;
bm0.y_size  = ys0;
                            /* Set target (display) bitmap window
                             (with Y inversion)                */

bm1.x_coord = x1;
bm1.y_coord = device[display].dev_ysiz -
              ((device[display].dev_ysiz < (y1 + ys1)) ?
               device[display].dev_ysiz : (y1 + ys1));
bm1.x_size = xs0 * zoom;
bm1.y_size = ys0 * zoom;

bmsize =  bm1.x_size * bm1.y_size ;
tmpbm0 = (unsigned char *) malloc (bmsize);
tmpbm  = tmpbm0;

/*for (i = 0; i < ys0; i++)*/
for (i = ys0-1; i >= 0; i--)
   {
   curbm0 = (mem->mmbm + (mem->y_size - bm0.y_coord - i - 1) *
                               mem->x_size + bm0.x_coord);
   for (l = 0; l < zoom; l++)
      {
      curbm = curbm0;
      for (j = 0; j < xs0; j++)
         {
         for (k = 0; k < zoom; k++)
            *tmpbm++ = *curbm;

         curbm++;
         }
      }
   }



ima = XCreateImage( display_id, visual, dpth, ZPixmap,
               0, (char*)tmpbm0 , bm1.x_size, bm1.y_size, 8, bm1.x_size);

XFlush(display_id);

XSetPlaneMask( display_id, gcima, mem->pm_mask );
XPutImage(display_id, w_id, gcima, ima, 0, 0,
          bm1.x_coord, bm1.y_coord, ima->width, ima->height) ;

XFlush(display_id);


free (tmpbm0);

return;
}

/******************************************************************************/

void smv_uz ( int display, int memid, int zoom, int x0, int y0, int xs0,
              int ys0, int x1, int y1, int xs1, int ys1 )

/*
*+
*  Name:
*     smv_uz
*
*  Purpose:
*     Set memory visibility : zoom < 0
*
*  Invocation:
*     smv_uz ( display, memid, zoom, x0, y0, xs0, ys0, x1, y1, xs1, ys1 )
*
*  Description:
*     Set memory visibility : zoom < 0
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     x0 = int
*        Origin coordinates for main memory bitmap (source)
*     y0 = int
*        Origin coordinates for main memory bitmap (source)
*     xs0 = int
*        Sub-image main memory bitmap dimensions (source)
*     ys0 = int
*        Sub-image main memory bitmap dimensions (source)
*     x1 = int
*        Origin coordinates for display bitmap (target)
*     y1 = int
*        Origin coordinates for display bitmap (target)
*     xs1 = int
*        Sub-image display bitmap dimensions (target)
*     ys1 = int
*        Sub-image display bitmap dimensions (target)
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Removed redundant code
*      8-MAY-1991 (NE):
*        Strict X library adherence.
*     16-APR-1992 (NE):
*        Set graphics context plane mask
*/

{

/* Local Variables */
int        i , j , curconf ,bmsize;

unsigned char       *tmpbm0 , *tmpbm;
unsigned char       *curbm;
unsigned int        dpth;

BM_WINDOW  bm0 , bm1;
CONF_DATA  *conf;
MEM_DATA   *mem;


display_id = (Display*) device[display].vd_id;
w_id       = (Window) device[display].wd_id;
gcima      = (GC) device[display].gcima_id;
screen     = XDefaultScreen(display_id);
visual     = VisualOfWindow( display );
dpth       = XDisplayPlanes(display_id, screen);

curconf = device[display].confid;
conf = device [display].config[curconf];
mem = conf->memory[memid];

                       /*       Set source bitmap window
                                 (with Y inversion)      */

bm0.x_coord = x0;
bm0.y_coord = y0;
bm0.x_size = xs0;
bm0.y_size = ys0;

                       /* Set target (display) bitmap window
                             (with Y inversion)		 */

bm1.x_coord = x1;
bm1.y_coord = device[display].dev_ysiz -
              ((device[display].dev_ysiz < (y1 + ys1)) ?
               device[display].dev_ysiz : (y1 + ys1));
bm1.x_size = xs0 / zoom;
bm1.y_size = ys0 / zoom;

                       /* Copy main memory bitmap to display bitmap */


bmsize = bm1.x_size * bm1.y_size;

tmpbm0 = (unsigned char *) malloc (bmsize);
tmpbm  = tmpbm0;

for (i = 0; i < ys0; i+= zoom)
   {
   curbm = (mem->mmbm + (bm0.y_coord + i) *
                               mem->x_size + bm0.x_coord);
   for (j = 0; j < xs0; j += zoom)
      *tmpbm++ = *(curbm + j);
   }


ima = XCreateImage( display_id, visual, dpth, ZPixmap,
               0, (char*)tmpbm0 , bm1.x_size, bm1.y_size, 8, bm1.x_size);
XFlush(display_id);

XSetPlaneMask( display_id, gcima, mem->pm_mask );
XPutImage(display_id, w_id, gcima, ima, 0, 0,
          bm1.x_coord, bm1.y_coord, ima->width, ima->height) ;
XFlush(display_id);

free (tmpbm0);


return;
}

/******************************************************************************/

void int_enable ( int display )

/*
*+
*  Name:
*     int_enable
*
*  Purpose:
*     Interaction enable
*
*  Invocation:
*     int_enable( display )
*
*  Description:
*     Interaction enable
*
*  Arguments:
*     display = int
*        Display identifier
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink RAL)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Update locator position to enable switching of ROI corners.
*        Add focus change as an event and removed redundant code.
*      3-JAN-1992 (NE):
*        Update locator position when cursor movement is enabled.
*     15-APR-1993 (DLT):
*        Use input only window for input events
*/

{

/* Local Variables */
int            i , interactor_id , loc_id , evl_id , trg_id;
int            loc0 , evl0 , trg0;
int            discard = 1;

Window inwin;
XSetWindowAttributes wind_attrib;
XEvent event;

INTER_DATA     *intdata;
INT_DEV_DATA   *intdev;
TRG_DATA       *trg;
LOC_DATA       *loc;
CURS_DATA      *curs;
ROI_DATA       *roi;

shift_pr  =  0 ;

intdev = int_struct.int_dev[1];
trg         = intdev->trg[3-3];
up_ls       = trg->def;
trg         = intdev->trg[5-3];
left_ls     = trg->def;
trg         = intdev->trg[7-3];
right_ls    = trg->def;
trg         = intdev->trg[9-3];
down_ls     = trg->def;
trg         = intdev->trg[53-3];
key_shift_l = trg->def;
trg         = intdev->trg[54-3];
key_shift_r = trg->def;

display_id = (Display*) device[display].vd_id;
w_id       = (Window) device[display].wd_id;

wind_attrib.event_mask = StructureNotifyMask;
inwin = XCreateWindow( display_id, w_id, 0, 0,
    device[display].dev_xsiz, device[display].dev_ysiz, 0, 0,
    InputOnly, CopyFromParent, CWEventMask, &wind_attrib );
XMapWindow( display_id, inwin);
XWindowEvent( display_id, inwin, StructureNotifyMask, &event );
device[display].inwin = inwin;

XDefineCursor (display_id, w_id, cur_def_int);

for (i = 0; i < device[display].n_inter; i++)
   {
   intdata = device[display].inter[i];
   switch (intdata->int_type)
      {
      case II_LOC:
         loc0 = intdata->int_id;
         get_loc (loc0 , &interactor_id , &loc_id);
         intdev = int_struct.int_dev[interactor_id];
         switch (intdev->descr)
            {
            case II_MOUSE:
               inter_mask[display] |= PointerMotionMask;
               inter_mask[display] |= EnterWindowMask;
               XSelectInput (display_id ,inwin ,inter_mask[display]);
               XFlush (display_id);
               break;

            case II_KEYB:
               inter_mask[display] |= KeyReleaseMask;
               inter_mask[display] |= KeyPressMask;
               inter_mask[display] |= EnterWindowMask;
               inter_mask[display] |= FocusChangeMask;
               XSelectInput (display_id ,inwin ,inter_mask[display]);
               XFlush (display_id);
               break;

            }
/* Update the locator position if a cursor is in use */
         if ( intdata->obj_type == II_CURSOR )
            {
            loc = intdev->loc[loc_id];
            curs = device[display].cursor[intdata->obj_id];
            loc->x_pos = curs->x_pos;
            loc->y_pos = curs->y_pos;
            }
/* Update the locator position if an ROI is in use */
         if ( intdata->obj_type == II_ROI )
            {
            loc = intdev->loc[loc_id];
            roi = device[display].roi[intdata->obj_id];
            loc->x_pos = roi->x_min;
            loc->y_pos = roi->y_min;
            }
         break;

      case II_EVLR:
      case II_EVLI:
      case II_EVLS:
      case II_EVLT:

         evl0 = intdata->int_id;
         get_evl (evl0 , &interactor_id , &evl_id);
         intdev = int_struct.int_dev[interactor_id];
         switch (intdev->descr)
            {
            case II_MOUSE:
               inter_mask[display] |= ButtonPressMask;
               XSelectInput (display_id ,inwin ,inter_mask[display]);
               XFlush (display_id);
               break;

            case II_KEYB:
               inter_mask[display] |= KeyPressMask;
               inter_mask[display] |= KeyReleaseMask;
               XSelectInput (display_id ,inwin ,inter_mask[display]);
               XFlush (display_id);
               break;

            }
         break;

      case II_TRG:

         trg0 = intdata->int_id;
         get_trg (trg0 , &interactor_id , &trg_id);
         intdev = int_struct.int_dev[interactor_id];
         switch (intdev->descr)
            {
            case II_MOUSE:
               inter_mask[display] |= ButtonPressMask;
               XSelectInput (display_id ,inwin ,inter_mask[display]);
               XFlush (display_id);
               break;

            case II_KEYB:
               inter_mask[display] |= KeyReleaseMask;
               inter_mask[display] |= KeyPressMask;
               XSelectInput (display_id ,inwin ,inter_mask[display]);
               XFlush (display_id);
               break;

            }
         break;
      }
   }


return;
}

/******************************************************************************/

void int_disable ( int display )

/*
*+
*  Name:
*     int_disable
*
*  Purpose:
*     Interaction disable
*
*  Invocation:
*     int_disable( display )
*
*  Description:
*     Interaction disable
*
*  Arguments:
*     display = int
*        Display identifier
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink RAL)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Remove pending events from the queue.
*        Retain an exposure event and removed redundant code.
*     18-JUN-1991 (NE):
*        Make the exposure event a conditional compilation
*     15-APR-1993 (DLT):
*        Delete input only window
*/

{

/* Local Variables */
int    status;
XEvent event;

display_id = (Display*) device[display].vd_id;
w_id       = (Window) device[display].wd_id;

XDefineCursor (display_id, w_id, cur_def);

/* Remove pending events from the queue */
inter_mask[display] = EnterWindowMask;
inter_mask[display] |= FocusChangeMask;
inter_mask[display] |= ButtonPressMask;
inter_mask[display] |= ButtonReleaseMask;
inter_mask[display] |= PointerMotionMask;
inter_mask[display] |= KeyReleaseMask;
inter_mask[display] |= KeyPressMask;
do
   {
   status = XCheckWindowEvent( display_id, device[display].inwin,
        inter_mask[display], &event );
   }
while( status );

/* Retain the ability to refresh the screen on exposure */
#ifdef VAXC
inter_mask[display] = ExposureMask;
#else
inter_mask[display] = 0;
#endif

XSelectInput( display_id, device[display].inwin, inter_mask[display] );
XDestroyWindow( display_id, device[display].inwin );

XFlush(display_id);

return;
}

/******************************************************************************/

void exit_trg_enable ( int display )

/*
*+
*  Name:
*     exit_trg_enable
*
*  Purpose:
*     Exit trigger enable
*
*  Invocation:
*     exit_trg_enable( display )
*
*  Description:
*     Exit trigger enable
*
*  Arguments:
*     display = int
*        Display identifier
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink RAL)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Removed redundant code.
*     30-JUL-1991 (NE):
*        Get trigger numbers from inter_data.
*     15-APR-1993 (DLT):
*        Use input only window for input events
*/

{

/* Local Variables */
int            interactor_id , trg_id;
int            i, trg0;
Window inwin;
XSetWindowAttributes wind_attrib;
XEvent event;

INTER_DATA     *intdata;
INT_DEV_DATA   *intdev;

display_id = (Display*) device[display].vd_id;
w_id       = (Window) device[display].wd_id;

wind_attrib.event_mask = StructureNotifyMask;
inwin = XCreateWindow( display_id, w_id, 0, 0,
    device[display].dev_xsiz, device[display].dev_ysiz, 0, 0,
    InputOnly, CopyFromParent, CWEventMask, &wind_attrib );
XMapWindow( display_id, inwin);
XWindowEvent( display_id, inwin, StructureNotifyMask, &event );
device[display].inwin = inwin;

for ( i = 0; i < device[display].n_inter; i++ )
   {
   intdata = device[display].inter[i];
   trg0 = intdata->trigger;
   get_trg( trg0, &interactor_id, &trg_id );
   intdev = int_struct.int_dev[interactor_id];
   switch ( intdev->descr )
      {
      case II_MOUSE:
         inter_mask[display] |= ButtonPressMask;
         XSelectInput( display_id, inwin, inter_mask[display] );
         XFlush( display_id );
         break;

      case II_KEYB:
         inter_mask[display] |= KeyReleaseMask;
         inter_mask[display] |= KeyPressMask;
         XSelectInput( display_id, inwin, inter_mask[display] );
         XFlush( display_id );
         break;
      }
   }

return;
}

/******************************************************************************/

void exit_trg_disable ( int display )

/*
*+
*  Name:
*     exit_trg_disable
*
*  Purpose:
*     Exit trigger disable
*
*  Invocation:
*     exit_trg_disable( display )
*
*  Description:
*     Exit trigger disable
*
*  Arguments:
*     display = int
*        Display identifier
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*/


{

  display_id = (Display*) device[display].vd_id;
  w_id       = (Window) device[display].wd_id;

  XDefineCursor (display_id, w_id, cur_def);
  XFlush(display_id);

  return;
}

/******************************************************************************/

void wait_int ( int display, int* ew, short* type, short* data,
                short position[2] )

/*
*+
*  Name:
*     wait_int
*
*  Purpose:
*     Wait for interaction
*
*  Invocation:
*     wait_int( display, ew, type, data, position )
*
*  Description:
*     Synchronous interaction routine
*
*  Arguments:
*     display = int
*        Display identifier
*     ew  = int
*        Enter window flag ( discard this interaction )
*     type = short
*        Internally defined interactor ( locator, locator-buttons, keyboard )
*     data = short
*        Interactor dependent ( mouse-button, key )
*     position = short[]
*        Cursor position
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink RAL)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Added focus as a window event and removed redundant code.
*      7-JAN-1993 (NE):
*        Fix to allow either shift key to speed up motion
*     15-APR-1993 (DLT):
*        Use input only window for input events
*/

{

display_id = (Display*) device[display].vd_id;
w_id       = (Window) device[display].wd_id;


XWindowEvent (display_id, device[display].inwin, inter_mask[display],
    &evw_data);

position[0] = (short)evw_data.xcrossing.x;
position[1] = (short)evw_data.xcrossing.y;
*type       = (short)evw_data.type;
*data       = evw_data.xcrossing.detail;

if ( *type == (short) KeyPress )
   {

/* If arrow key pressed with shift key then change to high speed */
   if ( shift_pr > 0)
      {
      if (*data == left_ls)
         *data = 301;
      else if (*data == right_ls)
         *data = 302;
      else if (*data == up_ls)
         *data = 300;
      else if (*data == down_ls)
         *data = 303;
      }

/* Remember if shift key pressed */
   if ( *data == key_shift_l )
      shift_pr = 1;
   else if ( *data == key_shift_r )
      shift_pr = 2;
   }

/* Forget if shift key released */
if (shift_pr == 1)
   if ( (*type == (short) KeyRelease) && (*data == key_shift_l) )
      shift_pr = 0;
if (shift_pr == 2)
   if ( (*type == (short) KeyRelease) && (*data == key_shift_r) )
      shift_pr = 0;

/* Set enter window flag */
if ((*type == (short)EnterNotify)||
    (*type == (short)FocusIn))
   *ew = 1;
else
   *ew = 0;

XFlush(display_id);

return;
}

/******************************************************************************/

void test_loc ( int display, int zf, short ev_type, short ev_data,
                short pos[2], int interactor_id, int loc_id, int* f0 )

/*
*+
*  Name:
*     test_loc
*
*  Purpose:
*     Test locator status
*
*  Invocation:
*     test_loc( display, zf, ev_type, ev_data, pos, interactor_id, loc_id, f0 )
*
*  Description:
*     Update locator status after interaction
*
*  Arguments:
*     display = int
*        Display identifier
*     zf = int
*        Zoom factor for keyboard cursor move
*     ev_type = short
*        Interaction type (mouse , keyboard ...
*     ev_data = short
*        Interaction output (mouse-button , key ...
*     pos = short[]
*        Cursor position
*     interactor_id = int
*        Interactor device id
*     loc_id = int
*        Locator id relative to the interactor device
*     f0 = int
*        Interaction flag
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Removed redundant code.
*     24-MAR-1992 (NE):
*        Added call to remove_pending for motion events
*     21-SEP-1992 (NE):
*        Added LOCX_SLIP and LOCY_SLIP
*     17-FEB-1993 (NE):
*        Correct the y-inversion
*/

{

/* Local Variables */
int                     dx , dy;

INT_DEV_DATA   *intdev;
LOC_DATA       *loc;

intdev  = int_struct.int_dev[interactor_id];
loc = intdev->loc[loc_id];

*f0 = 0;
switch (intdev->descr)
   {
   case II_MOUSE:

      if (ev_type == (short)MotionNotify)
         {
         if (user == 1)
            {
/*                  static values to be used by Get_Locator_Displacement */
            LOC_X[loc_id] = pos[0];
            LOC_Y[loc_id] = device[display].dev_ysiz - 1 - pos[1];
            *f0 = 1;
/* Remove any multiple pending events of this type */
            remove_pending( display, ev_type );
            }
         else
            {
            if ((abs(loc->x_pos - pos[0]) < LOCX_SLIP) &&
                (abs(loc->y_pos - (device[display].dev_ysiz - 1 - pos[1])) <
                 LOCY_SLIP))
               {
               loc->x_pos = pos[0];
               loc->y_pos = device[display].dev_ysiz - 1 - pos[1];
               *f0 = 1;
               }
            }
         }
      break;

   case II_KEYB:

      if (ev_type == (short)KeyPress)
         {
         dx = 0;
         dy = 0;
         if (ev_data == (short)loc->left_ls)
            dx = -1;
         else if (ev_data == (short)loc->left_hs)
            dx = -loc->hs_fact;
         else if (ev_data == (short)loc->right_ls)
            dx = 1;
         else if (ev_data == (short)loc->right_hs)
            dx = loc->hs_fact;
         else if (ev_data == (short)loc->up_ls)
            dy = 1;
         else if (ev_data == (short)loc->up_hs)
            dy = loc->hs_fact;
         else if (ev_data == (short)loc->down_ls)
            dy = -1;
         else if (ev_data == (short)loc->down_hs)
            dy = -loc->hs_fact;

         else
            return;

         loc->x_pos += dx * zf;
         loc->y_pos += dy * zf;
         *f0 = 1;
/*                 static values to be used by Get_Locator_Displacement */
         LOC_X[loc_id] = loc->x_pos;
         LOC_Y[loc_id] = loc->y_pos;
         }
      break;
   }


return;
}

/******************************************************************************/

void test_evl ( int display, short ev_type, short ev_data, int interactor_id,
                int evl_id, int* f0 )

/*
*+
*  Name:
*     test_evl
*
*  Purpose:
*     Test evaluator status
*
*  Invocation:
*     test_evl( display, ev_type, ev_data, interactor_id, evl_id, f0 )
*
*  Description:
*     Update evaluator status after interaction
*
*  Arguments:
*     display = int
*        Display identifier
*     ev_type = short
*        Interaction type (mouse , keyboard ...
*     ev_data = short
*        Interaction output (mouse-button , key ...
*     interactor_id = int
*        Interactor device id
*     evl_id = int
*        Evaluator id relative to the interactor device
*     f0 = int
*        Interaction flag
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Removed redundant code.
*/

{

/* Local Variables */
short          pos[2];

char           sval[32];
int            i , ew;

INT_DEV_DATA   *intdev;
EVL_DATA       *evl;

intdev  = int_struct.int_dev[interactor_id];
evl = intdev->evl[evl_id];

i = 0;
*f0 = 0;
switch (intdev->descr)
   {
   case II_MOUSE:
      break;

   case II_KEYB:

      switch (evl->type)
         {
         case II_EVLT:   /*  T evaluator */

            if ((ev_type == KeyPress) &&
                (ev_data == (short) evl->def[0]))

               {
               evl->ival += evl->min;
               *f0 = 1;
               }

            if ((ev_type == KeyPress) &&
                (ev_data == (short) evl->def[1]))

               {
               evl->ival += evl->max;
               *f0 = 1;
               }

            break;

         case II_EVLR: /*  R evaluator */
         case II_EVLI: /*  I evaluator */

            if ((ev_type == KeyPress) &&
                (ev_data >= (short) NUM_MIN) &&
                (ev_data <= (short) NUM_MAX))
               {
               sval[i++] = (char) ev_data;
               while (ev_data != (short) KEYB_CR)
                  {
                  wait_int (display , &ew , &ev_type , &ev_data , pos);
                  if ((ev_type == KeyPress) &&
                      (ev_data >= (short) NUM_MIN) &&
                      (ev_data <= (short) NUM_MAX))
                     sval[i++] = (char) ev_data;
                  }
               sval[i] = '\0';
               if (evl->type == II_EVLI)
                  sscanf (sval , "%d", &evl->ival);
               else
                  sscanf (sval , "%f", &evl->rval);

               *f0 = 1;
               }
            break;

         case II_EVLS: /*   S evaluator */

            if ((ev_type == KeyPress) &&
                (ev_data >= (short) ALNUM_MIN) &&
                (ev_data <= (short) ALNUM_MAX))
               {
               sval[i++] = (char) ev_data;
               while (ev_data != (char) KEYB_CR)
                  {
                  if ((ev_type == KeyPress) &&
                        (ev_data >= (short) ALNUM_MIN) &&
                        (ev_data <= (short) ALNUM_MAX))
                     {
                     wait_int (display , &ew , &ev_type , &ev_data , pos);
                     sval[i++] = (char) ev_data;
                     }
                  }
               sval[i] = '\0';
               strcpy (evl->sval , sval);

               *f0 = 1;
               }
            break;
         }
   }


return;
}

/******************************************************************************/

void test_trg ( short ev_type, short ev_data, int interactor_id, int trg_id,
                int* f0 )

/*
*+
*  Name:
*     test_trg
*
*  Purpose:
*     Test trigger status
*
*  Invocation:
*     test_trg( ev_type, ev_data, interactor_id, trg_id, f0 )
*
*  Description:
*     Update trigger status after interaction
*
*  Arguments:
*     ev_type = short
*        Interaction type (mouse , keyboard ...
*     ev_data = short
*        Interaction output (mouse-button , key ...
*     interactor_id = int
*        Interactor device id
*     trg_id = int
*        Trigger id relative to the interactor device
*     f0 = int
*        Interaction flag
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Removed redundant code.
*/

{

/* Local Variables */
INT_DEV_DATA   *intdev;
TRG_DATA       *trigger;

intdev  = int_struct.int_dev[interactor_id];
trigger = intdev->trg[trg_id];

*f0 = 0;
switch (intdev->descr)
   {
   case II_MOUSE:
      switch (trigger->def)
         {
         case MOUSE_LEFT:
            if ((ev_type == ButtonPress) &&
                ((ev_data & 0xFF) == MOUSE_LEFT))
               *f0 = 1;
            break;
         case MOUSE_MIDDLE:
            if ((ev_type == ButtonPress) &&
                ((ev_data & 0xFF) == MOUSE_MIDDLE))
               *f0 = 1;
            break;
         case MOUSE_RIGHT:
            if ((ev_type == ButtonPress) &&
                ((ev_data & 0xFF) == MOUSE_RIGHT))
               *f0 = 1;
            break;
         }

   break;

   case II_KEYB:
      if ((ev_type == KeyPress) &&
          (ev_data == (short)trigger->def))
         *f0 = 1;
   break;
   }


return;
}

/******************************************************************************/

void idi__bar ( int display, int memid, int vis, int* idierr )

/*
*+
*  Name:
*     idi__bar
*
*  Purpose:
*     Intensity bar
*
*  Invocation:
*     idi__bar( display, memid, vis, idierr )
*
*  Description:
*     Intensity bar
*
*  Arguments:
*     display = int
*     memid = int
*     vis = int
*     idierr = int
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Set no graphics exposures for window
*      8-MAY-1991 (NE):
*        Strict X library adherence.
*     27-NOV-1991 (NE):
*        Added plane mask.
*/

{

/* Local Variables */
GC      gc;
Window 	wnd;

int      black, white, screen;

int  i , k, curconf , ihdim , icol;
int  xoff , yoff , xdim , ydim;
int  bardim = 15;
int  barxdim, barydim;

CONF_DATA *conf;
MEM_DATA  *mem;
ITT_DATA  *itt;

*idierr = II_SUCCESS;

display_id = (Display*) device[display].vd_id;
w_id       = (Window) device[display].wd_id;

                 /* inquire current display size to determine bar length */
*idierr = VDM_INQ (device[display].devtyp, &xoff, &yoff, &xdim, &ydim);
if (*idierr != II_SUCCESS)
   return;

curconf = device[display].confid;
conf = device [display].config[curconf];
mem = conf->memory[memid];
itt = mem->itt[mem->itt_id];

ihdim = xdim / curlut.len;
barxdim = ihdim * curlut.len;
barydim = bardim;

if (vis == 1)                        /*  Visibility = 1 */
   {
   if (device[display].bar.created == 0)
      {                                         /* create window for bar */
      screen = XDefaultScreen (display_id);
      white = XWhitePixel (display_id, screen);
      black = XBlackPixel (display_id, screen);

      wnd = XCreateSimpleWindow (display_id, w_id, 0, 0, barxdim, barydim, 1,
                                 black, white);
      gc = XCreateGC (display_id, wnd, 0, 0);
      XSetFunction (display_id, gc, GXcopy);
      XSetGraphicsExposures( display_id, gc, 0 );
      XSetPlaneMask( display_id, gc, device[display].pm_mask );
      XMapRaised (display_id, wnd);
      XSetWindowColormap (display_id, wnd, cmap);
      XFlush (display_id);

      for (i = 0; i < curlut.len; i++)
         {
         icol = curlut.lutpix[itt->ittlev[i]];
         XSetWindowBackground (display_id, wnd, icol);
         XClearArea (display_id, wnd, i*ihdim, 0, ihdim, barydim, 0);
         }
      XFlush(display_id);

      device[display].bar.created = 1;
      device[display].bar.wnd = wnd;
      }
   else
      {                           /* window fo bar already created */
      wnd = device[display].bar.wnd;
      XMapRaised (display_id, wnd);
      XSetWindowColormap (display_id, wnd, cmap);
      XFlush (display_id);

      for (i = 0; i < curlut.len; i++)
         {
         icol = curlut.lutpix[itt->ittlev[i]];
         XSetWindowBackground (display_id, wnd, icol);
         XClearArea (display_id, wnd, i*ihdim, 0, ihdim, barydim, 0);
         }
      XFlush(display_id);
      }
   }
else                              /* Visibility = 0 */
   {
   if (device[display].bar.created == 1)
      {
      wnd = device[display].bar.wnd;
      XUnmapWindow (display_id, wnd);
      XFlush (display_id);
      }
   }

return;
}

/******************************************************************************/

void cross_hair ( int display, int xcur, int ycur, int curcol )

/*
*+
*  Name:
*     cross_hair
*
*  Purpose:
*     Cross-hair cursor
*
*  Invocation:
*     cross_hair( display, xcur, ycur, curcol )
*
*  Description:
*     Cross-hair cursor
*
*  Arguments:
*     display = int
*     xcur = int
*     ycur = int
*     curcol = int
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     24-APR-1991 (NE):
*        Define pixel colour as table mid-point
*      8-MAY-1991 (NE):
*        Strict X library adherence.
*      5-SEP-1991 (NE):
*        Draw with white pen on one bit displays
*     29-NOV-1991 (NE):
*        Draw cursor into pixmap as well
*     14-APR-1992 (NE):
*        Use cursor graphics context and plane mask
*      5-JAN-1993 (NE):
*        Only draw in window if flag is set
*     17-FEB-1993 (NE):
*        Correct the y-inversion
*/

{

/* Local Variables */
int    nsegments;
int    colpix;
int    x_off, y_off, zoom;
CONF_DATA *conf;
MEM_DATA *mem, *memf;

display_id = (Display*) device[display].vd_id ;
w_id       = (Window) device[display].wd_id ;
gccurs     = (GC) device[display].gccurs_id ;
screen	   = XDefaultScreen(display_id);
cmap       = ColormapOfWindow( display );

/* Use the currently displayed pixmap */
conf = device[display].config[device[display].confid];
mem = conf->memory[device[display].pm_mem];
pixmap_id  = (Pixmap) mem->pm_id;

ycur = device[display].dev_ysiz - 1 - ycur;

curso0[0].x1 = 0;
curso0[0].y1 = ycur;
curso0[0].x2 = curso0[0].x1 + device[display].dev_xsiz - 1;
curso0[0].y2 = curso0[0].y1;

curso0[1].x1 = xcur;
curso0[1].y1 = 0;
curso0[1].x2 = curso0[1].x1;
curso0[1].y2 = curso0[1].y1 + device[display].dev_ysiz - 1;

nsegments = 2 ;

/* Draw with a white pen on a one bit display for XOR to work properly */
colpix = ( curlut.lutpix[curlut.len/2] > 0 )
         ? curlut.lutpix[curlut.len/2] | ~device[display].pm_mask : 1;
XSetForeground( display_id, gccurs, colpix );

/* Only draw in window if flag is set */
if ( device[display].curs_flag < 0 )
   {
   XSetPlaneMask( display_id, gccurs, AllPlanes );
   XDrawSegments(display_id, w_id, gccurs, curso0, nsegments);
   }
else
   {

/* Refresh individual memories */
   if ( device[display].curs_flag < conf->n_mem )
      {
      memf = conf->memory[device[display].curs_flag];
      XSetPlaneMask( display_id, gccurs, memf->pm_mask );
      XDrawSegments(display_id, w_id, gccurs, curso0, nsegments);
      }
   }

/* Plot cursor into pixmap */
x_off = 0;
y_off = device[display].dev_ysiz - mem->y_size;
curso0[0].y1 = ycur - y_off;
curso0[0].y2 = curso0[0].y1;
curso0[1].x1 = xcur - x_off;
curso0[1].x2 = curso0[1].x1;
XSetPlaneMask( display_id, gccurs, mem->pm_mask );
XDrawSegments(display_id, pixmap_id, gccurs, curso0, nsegments);

/* Plot cursor into pixmap overlay */
if ( ~device[display].pm_mask > 0 )
   {
   mem = conf->memory[device[display].pm_memov];
   x_off = 0;
   y_off = device[display].dev_ysiz - mem->y_size;
   curso0[0].y1 = ycur - y_off;
   curso0[0].y2 = curso0[0].y1;
   curso0[1].x1 = xcur - x_off;
   curso0[1].x2 = curso0[1].x1;
   XSetPlaneMask( display_id, gccurs, mem->pm_mask );
   XDrawSegments(display_id, pixmap_id, gccurs, curso0, nsegments);
   }

/* Reset cursor graphics context */
XSetPlaneMask( display_id, gccurs, AllPlanes );
XFlush(display_id);

return;
}

/******************************************************************************/

void cross ( int display, int xcur, int ycur, int curcol )

/*
*+
*  Name:
*     cross
*
*  Purpose:
*     Cross cursor
*
*  Invocation:
*     cross( display, xcur, ycur, curcol )
*
*  Description:
*     Cross cursor
*
*  Arguments:
*     display = int
*     xcur = int
*     ycur = int
*     curcol = int
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     24-APR-1991 (NE):
*        Define pixel colour as table mid-point
*      8-MAY-1991 (NE):
*        Strict X library adherence.
*      5-SEP-1991 (NE):
*        Draw with white pen on one bit displays
*     29-NOV-1991 (NE):
*        Draw cursor into pixmap as well
*     14-APR-1992 (NE):
*        Use cursor graphics context and plane mask
*      5-JAN-1993 (NE):
*        Only draw in window if flag is set
*     17-FEB-1993 (NE):
*        Correct the y-inversion
*/

{

/* Local Variables */
int    nsegments;
int    colpix;
int    x_off, y_off, zoom;
CONF_DATA *conf;
MEM_DATA *mem, *memf;

display_id = (Display*) device[display].vd_id ;
w_id       = (Window) device[display].wd_id ;
gccurs     = (GC) device[display].gccurs_id ;
screen	   = XDefaultScreen(display_id);
cmap       = ColormapOfWindow( display );

/* Use the currently displayed pixmap */
conf = device[display].config[device[display].confid];
mem = conf->memory[device[display].pm_mem];
pixmap_id  = (Pixmap) mem->pm_id;

ycur = device[display].dev_ysiz - 1 - ycur;

curso0[0].x1 = xcur - 16;
curso0[0].y1 = ycur;
curso0[0].x2 = curso0[0].x1 + 32;
curso0[0].y2 = curso0[0].y1;

curso0[1].x1 = xcur;
curso0[1].y1 = ycur - 16;
curso0[1].x2 = curso0[1].x1;
curso0[1].y2 = curso0[1].y1 + 32;
nsegments = 2 ;

/* Draw with a white pen on a one bit display for XOR to work properly */
colpix = ( curlut.lutpix[curlut.len/2] > 0 )
         ? curlut.lutpix[curlut.len/2] | ~device[display].pm_mask : 1;
XSetForeground( display_id, gccurs, colpix );

/* Only draw in window if flag is set */
if ( device[display].curs_flag < 0 )
   {
   XSetPlaneMask( display_id, gccurs, AllPlanes );
   XDrawSegments(display_id, w_id, gccurs, curso0, nsegments);
   }
else
   {

/* Refresh individual memories */
   if ( device[display].curs_flag < conf->n_mem )
      {
      memf = conf->memory[device[display].curs_flag];
      XSetPlaneMask( display_id, gccurs, memf->pm_mask );
      XDrawSegments(display_id, w_id, gccurs, curso0, nsegments);
      }
   }

/* Plot cursor into pixmap */
x_off = 0;
y_off = device[display].dev_ysiz - mem->y_size;
curso0[0].x1 = xcur - 16 - x_off;
curso0[0].y1 = ycur - y_off;
curso0[0].x2 = curso0[0].x1 + 32;
curso0[0].y2 = curso0[0].y1;
curso0[1].x1 = xcur - x_off;
curso0[1].y1 = ycur - 16 - y_off;
curso0[1].x2 = curso0[1].x1;
curso0[1].y2 = curso0[1].y1 + 32;
XSetPlaneMask( display_id, gccurs, mem->pm_mask );
XDrawSegments(display_id, pixmap_id, gccurs, curso0, nsegments);

/* Plot cursor into pixmap overlay */
if ( ~device[display].pm_mask > 0 )
   {
   mem = conf->memory[device[display].pm_memov];
   x_off = 0;
   y_off = device[display].dev_ysiz - mem->y_size;
   curso0[0].x1 = xcur - 16 - x_off;
   curso0[0].y1 = ycur - y_off;
   curso0[0].x2 = curso0[0].x1 + 32;
   curso0[0].y2 = curso0[0].y1;
   curso0[1].x1 = xcur - x_off;
   curso0[1].y1 = ycur - 16 - y_off;
   curso0[1].x2 = curso0[1].x1;
   curso0[1].y2 = curso0[1].y1 + 32;
   XSetPlaneMask( display_id, gccurs, mem->pm_mask );
   XDrawSegments(display_id, pixmap_id, gccurs, curso0, nsegments);
   }

/* Reset cursor graphics context */
XSetPlaneMask( display_id, gccurs, AllPlanes );
XFlush(display_id);

return;
}

/******************************************************************************/

void rectangle ( int display, int x0, int y0, int x1, int y1, int roicol )

/*
*+
*  Name:
*     rectangle
*
*  Purpose:
*     Rectangular ROI
*
*  Invocation:
*     rectangle( display, x0, y0, x1, y1, roicol )
*
*  Description:
*     Rectangular ROI
*
*  Arguments:
*     display = int
*     x0 = int
*     y0 = int
*     x1 = int
*     y1 = int
*     roicol = int
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     24-APR-1991 (NE):
*        Define pixel colour as table mid-point
*      8-MAY-1991 (NE):
*        Strict X library adherence.
*      5-SEP-1991 (NE):
*        Draw with white pen on one bit displays
*     17-FEB-1993 (NE):
*        Correct the y-inversion
*/

{

/* Local Variables */
int    nsegments;
int    colpix;

display_id = (Display*) device[display].vd_id ;
w_id       = (Window) device[display].wd_id ;
gccurs     = (GC) device[display].gccurs_id ;
screen     = XDefaultScreen(display_id);
cmap       = ColormapOfWindow(display);


y0 = device[display].dev_ysiz - 1 - y0;
y1 = device[display].dev_ysiz - 1 - y1;

curso0[0].x1 = x0;
curso0[0].y1 = y0;
curso0[0].x2 = x1 - 1;
curso0[0].y2 = y0;

curso0[1].x1 = x1;
curso0[1].y1 = y0;
curso0[1].x2 = x1;
curso0[1].y2 = y1 - 1;

curso0[2].x1 = x1;
curso0[2].y1 = y1;
curso0[2].x2 = x0 - 1;
curso0[2].y2 = y1;

curso0[3].x1 = x0;
curso0[3].y1 = y1 - 1;
curso0[3].x2 = x0;
curso0[3].y2 = y0 + 1;

nsegments = 4 ;

/* Draw with a white pen on a one bit display for XOR to work properly */
colpix = ( curlut.lutpix[curlut.len/2] > 0 )
         ? curlut.lutpix[curlut.len/2] | ~device[display].pm_mask : 1;
XSetForeground( display_id, gccurs, colpix );

XDrawSegments(display_id, w_id, gccurs, curso0, nsegments);
XFlush(display_id);

return;
}

/******************************************************************************/

void polyline_d ( int display, int memid, int col, int style, int xs[],
                  int ys[], int np )

/*
*+
*  Name:
*     polyline_d
*
*  Purpose:
*     Polyline on display bitmap
*
*  Invocation:
*     polyline_d( display, memid, col, style, xs, ys, np )
*
*  Description:
*     Polyline on display bitmap
*
*  Arguments:
*     display = int
*     memid = int
*     col = int
*     style = int
*     np = int
*     xs = int[]
*     ys = int[]
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Removed redundant code
*      8-MAY-1991 (NE):
*        Strict X library adherence.
*     16-APR-1992 (NE):
*        Set graphics context plane mask
*     17-FEB-1993 (NE):
*        Correct the y-inversion
*/

{

/* Local Variables */
int                  i, xor;
unsigned int	     width;
CONF_DATA  *conf;
MEM_DATA    *mem;

display_id = (Display*) device[display].vd_id ;
w_id       = (Window) device[display].wd_id ;
gcima      = (GC) device[display].gcima_id ;
gcdraw     = (GC) device[display].gcdraw_id ;
screen     = XDefaultScreen (display_id);
cmap	   = ColormapOfWindow (display);

conf = device [display].config[device[display].confid];
mem = conf->memory[memid];

xor = 0;
if (col < 0)
   {
   col = (-col);
   xor = 1;
   }

if (style >= 10)
   {
   width = (unsigned int) ((style/10) + 1);
   style = style - (style/10);
   }
else
   width = 1;

for (i = 0; i < np; i++)
   {
   vlist[i].x = (short)xs[i];
   vlist[i].y = (short) (device[display].dev_ysiz - 1 - ys[i]);
   }

switch (col)
   {
   case II_BLACK:
      XParseColor( display_id, cmap, "#000000", &xcol);
      break;
   case II_WHITE:
      XParseColor( display_id, cmap, "#ffffff", &xcol);
      break;
   case II_RED:
      XParseColor( display_id, cmap, "#ff0000", &xcol);
      break;
   case II_GREEN:
      XParseColor( display_id, cmap, "#00ff00", &xcol);
      break;
   case II_BLUE:
      XParseColor( display_id, cmap, "#0000ff", &xcol);
      break;
   case II_YELLOW:
      XParseColor( display_id, cmap, "#ffff00", &xcol);
      break;
   case II_MAGENTA:
      XParseColor( display_id, cmap, "#ff00ff", &xcol);
      break;
   case II_CYAN:
      XParseColor( display_id, cmap, "#00ffff", &xcol);
      break;
   default:
      XParseColor( display_id, cmap, "#ffffff", &xcol);
      break;
   }

XAllocColor (display_id, cmap, &xcol);

if (xor == 1)
   {
   XSetLineAttributes (display_id, gcdraw, width, LineSolid,
			CapButt, JoinMiter );
   XSetForeground( display_id, gcdraw, xcol.pixel);
   XSetPlaneMask( display_id, gcdraw, mem->pm_mask );
   XDrawLines (display_id, w_id, gcdraw, vlist, np, CoordModeOrigin);
   }
else
   {
   XSetLineAttributes (display_id, gcima, width, LineSolid,
			CapButt, JoinMiter );
   XSetForeground( display_id, gcima, xcol.pixel);
   XSetPlaneMask( display_id, gcima, mem->pm_mask );
   XDrawLines (display_id, w_id, gcima, vlist, np, CoordModeOrigin);
   }

XFlush(display_id);


return;
}

/******************************************************************************/

void text_d ( int display, int x0, int y0, int path, int orient, int col,
              int size, char txt[], int* idierr )

/*
*+
*  Name:
*     text_d( display, x0, y0, path, orient, col, size, txt, idierr )
*
*  Purpose:
*     Text on display bitmap
*
*  Invocation:
*     text_d( display, x0, y0, path, orient, col, size, txt, idierr )
*
*  Description:
*     Text on display bitmap
*
*  Arguments:
*     display = int
*     x0 = int
*     y0 = int
*     path = int
*     orient = int
*     col = int
*     size = int
*     txt = char[]
*     idierr = int
*
*  Authors:
*     PASIAN: F. Pasian (Trieste Astronomical Observatory)
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (PASIAN,SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Changed font names, called XSetFont and removed redundant code
*      8-MAY-1991 (NE):
*        Strict X library adherence.
*     24-JUN-1991 (NE):
*        Select from four font sizes
*     16-APR-1992 (NE):
*        Set graphics context plane mask
*     17-FEB-1993 (NE):
*        Correct the y-inversion
*/

{

/* Local Variables */
char        fontname[256];
int         forcol , bckcol;
int         txtlen;


display_id = (Display*) device[display].vd_id;
w_id       = (Window) device[display].wd_id ;
gcima      = (GC) device[display].gcima_id ;

screen	   = XDefaultScreen(display_id);
white	   = XWhitePixel( display_id, screen);

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

txtlen = strlen (txt);

font_info = XLoadQueryFont(display_id,fontname);
y0 = device[display].dev_ysiz - 1 - (y0 +font_info->ascent);

XSetFont( display_id, gcima, font_info->fid );
XSetForeground (display_id, gcima, col);
XSetPlaneMask( display_id, gcima, AllPlanes );
XDrawString (display_id, w_id, gcima, x0, y0, txt, txtlen);
XFlush(display_id);


XSetForeground (display_id, gcima, white);

XFlush(display_id);


return;
}

/******************************************************************************/

Colormap ColormapOfWindow (display)

/*
*+
*  Name:
*     colormap ColormapOfWindow (int display)
*
*  Purpose:
*     Return the colormap associated with a window
*
*  Invocation:
*     cmap = ColormapOfWindow ( display);
*
*  Description:
*
*
*  Arguments:
*     display = int
*
*  Authors:
*     David Terrett (DLT)
*
*  History:
*     02-MAR-1994 (DLT):
*        Orignal version
*/

/* Arguments Given */
int  display;
{

/* Local Variables */
XWindowAttributes        winatt;


display_id = (Display*) device[display].vd_id;
w_id       = (Window) device[display].wd_id ;

XGetWindowAttributes( display_id, w_id, &winatt);

return winatt.colormap;

}


/******************************************************************************/

Visual *VisualOfWindow (display)

/*
*+
*  Name:
*     visual VisualOfWindow (int display)
*
*  Purpose:
*     Return the visual associated with a window
*
*  Invocation:
*     visual = VisualOfWindow ( display);
*
*  Description:
*
*
*  Arguments:
*     display = int
*
*  Authors:
*     David Terrett (DLT)
*
*  History:
*     21-APR-1999 (DLT):
*        Orignal version
*/

/* Arguments Given */
int  display;
{

/* Local Variables */
XWindowAttributes        winatt;


display_id = (Display*) device[display].vd_id;
w_id       = (Window) device[display].wd_id ;

XGetWindowAttributes( display_id, w_id, &winatt);

return winatt.visual;

}

