/*
*+
*  Name:
*     IDIUTIL.C
*
*  Purpose:
*     Internal Service Routines
*
*  Description:
*     Internal Service Routines
*
*  Contents:
*     idi__min
*        Minimum;
*     idi__max
*        Maximum;
*     refr
*        Display Refresh of memories , cursors and roi;
*     grefr
*        Display Refresh of cursors and roi;
*     polyrefr
*        Display Refresh of Graphics;
*     txtrefr
*        Display Refresh of Text;
*     mem_dis_conv
*        Memory to Display coordinate conversion;
*     dis_mem_conv
*        Display to Memory coordinate conversion;
*     cl_bitmap
*        Clear Main Memory Bitmap;
*     mmbm_all
*        main memory Bitmap Allocate;
*     mmbm_deall
*        Main memory Bitmap Deallocate;
*     wr_mem
*        Write Main Memory Bitmap;
*     rd_mem
*        Read Main Memory Bitmap;
*     ebwmy
*        External Bitmap Write Memory;
*     eb_openi
*        External Bitmap Open (input);
*     loc_zero
*        Zero Locator Data;
*     test_user
*        Test USER status;
*     sync_loc
*        Locator Interaction;
*     sync_evl
*        Evaluator Interaction;
*     sync_trg
*        Trigger Interaction;
*     get_loc
*        Transform Absolute to Relative Locator id;
*     get_evl
*        Transform Absolute to Relative Evaluator id;
*     get_trg
*        Transform Absolute to Relative Trigger id;
*     cursor_move
*        Interactive Cursor Movement;
*     roi_move
*        Interactive Region of Interest Move;
*     roi_modify
*        Interactive Region of Interest Modify;
*     lut_rotate
*        Interactive LUT Rotate;
*     lut_slice
*        Interactive LUT Slice;
*     mem_scroll
*        Interactive Memory Scroll;
*     mem_zoom
*        Interactive Memory Zoom;
*     mem_unzoom
*        Interactive Memory Unzoom;
*     mem_clzoom
*        Interactive Memory Clear Zoom;
*     polyline_dl
*        Polyline : Display List Management;
*     text_dl
*        Text : Display List Management;
*
*  Copyright:
*     Copyright (C) 1988, 1990, 1991, 1992, 1994 Science & Engineering Research Council.
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
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Added iii.h
*     23-MAY-1991 (NE):
*        Added stdlib.h for malloc and abs
*      7-MAY-1992 (NE):
*        Added x11defs.h for AllPlanes
*/

/* System definitions */

#include    <stdlib.h>
#include    <stdio.h>
#include    <string.h>
#include    <math.h>

/* Package definitions */

#include    "device.dep"

#include    "kwm.h"
#include    "idi.h"
#include    "idi_err.h"
#include    "idistruct_e.h"
#include    "iii.h"
#include    "x11defs.h"
#include    "idifuncs.h"

/* Local definitions */

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

int idi__min ( int ival[4], int n )

/*
*+
*  Name:
*     idi__min
*
*  Purpose:
*     Min
*
*  Invocation:
*     idi__min( ival, n )
*
*  Description:
*     Min
*
*  Arguments:
*     ival = int[4]
*     n = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/

{

/* Local Variables */
int i , minval;

minval = ival[0];
for (i = 0; i < n; i++)
   {
   if (ival[i] < minval)
      minval = ival[i];
   }
return (minval);
}

/******************************************************************************/

int idi__max ( int ival[4], int n )

/*
*+
*  Name:
*     idi__max
*
*  Purpose:
*     Max
*
*  Invocation:
*     idi__max( ival, n )
*
*  Description:
*     Max
*
*  Arguments:
*     ival = int[4]
*     n = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/

{
int i , maxval;

maxval = ival[0];
for (i = 0; i < n; i++)
   {
   if (ival[i] > maxval)
      maxval = ival[i];
   }
return (maxval);
}

/******************************************************************************/

void refr ( int display, int* iierr )

/*
*+
*  Name:
*     refr
*
*  Purpose:
*     Refresh
*
*  Invocation:
*     refr( display, iierr )
*
*  Description:
*     General refresh of image memories
*
*  Arguments:
*     display = int
*     iierr = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Refresh pixmap rather than display window
*     25-JUL-1991 (NE):
*        Added array for int_scroll_reset
*     28-AUG-1991 (NE):
*        Alter argument list in clear_pixmap
*     16-APR-1992 (NE):
*        Alter argument list in clear_pixmap
*      7-MAY-1992 (NE):
*        Correct operation with overlay planes
*        Clear the pixmap in all circumstances
*/

{

/* Local Variables */
int    i , curconf , xoff , yoff , xdim , ydim , status;
int    memlist[MAX_MEM], nmem;
int    reflag;

CONF_DATA   *conf;
MEM_DATA    *mem;

*iierr = II_SUCCESS;

/* inquire current display size */

status = VDM_INQ (device[display].devtyp, &xoff, &yoff, &xdim, &ydim);

if (status != II_SUCCESS)
   {
   *iierr = status;
   return;
   }

/* Use a flag to stop the display being refreshed twice */
reflag = 1;

device[display].dev_xsiz = xdim;
device[display].dev_ysiz = ydim;

curconf = device[display].confid;
conf = device[display].config[curconf];

/* Clear the pixmap for the current configuration */
clear_pixmap( display, 0, 0, AllPlanes );

/* Refresh the images */
nmem = 0;
for (i = 0; i < conf->n_mem; i++)
   {                             /* Image memories */
   mem = conf->memory[i];
   if ((mem->visibility == 1) && ((mem->type & II_IMAGE) > 0))
      {
      reflag = 0;
      mem->visibility = 0;
      memlist[nmem] = i;
      nmem += 1;
      }
   }
if ( nmem > 0 ) *iierr = IIMSMV_C (display, memlist, nmem, 1);

                                 /* Graphic memories */
for (i = 0; i < conf->n_mem; i++)
   {
   mem = conf->memory[i];
   if ((mem->visibility == 1) && ((mem->type & II_IMAGE) == 0))
      {
      polyrefr_p( display, curconf, i );
      textrefr_p( display, curconf, i );
      reflag = 1;
      }
   }

if (device[display].bar.vis == 1)
   *iierr = IILSBV_C (display , device[display].bar.mem , 1);

/* Set the interactive scrolling flag */
int_scroll_reset[display] = 1;

/* Refresh the pixmap onto the display if not already done so */
if ( reflag )
   refr_p( display );

return;
}

/******************************************************************************/

void grefr ( int display, int* iierr )

/*
*+
*  Name:
*     grefr
*
*  Purpose:
*     Cursors & Roi Refresh
*
*  Invocation:
*     grefr( display, iierr )
*
*  Description:
*     General refresh of cursors and ROIs
*
*  Arguments:
*     display = int
*     iierr = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/

{

/* Local Variables */
int           i , curconf;

CONF_DATA   *conf;
MEM_DATA    *mem;
CURS_DATA   *curs;
ROI_DATA    *roi;

*iierr = II_SUCCESS;

curconf = device[display].confid;
conf = device[display].config[curconf];

for (i = 0; i < device[display].n_roi; i++)
   {
   roi = device[display].roi[i];
   if (roi->memid != -1)
      {
      mem = conf->memory[roi->memid];
      if ((mem->visibility == 0) && (roi->vis == 1))
         roi->vis = -1;
      else if ((mem->visibility == 1) && (roi->vis == -1))
         roi->vis = 1;
      }
   if (roi->vis == 1)
      {
      roi->vis = 0;
      *iierr = IIRSRV_C (display , i , 1);
      }
   }

for (i = 0; i < device[display].n_curs; i++)
   {
   curs = device[display].cursor[i];
   if (curs->cur_memid != -1)
      {
      mem = conf->memory[curs->cur_memid];
      if ((mem->visibility == 0) && (curs->vis == 1))
         curs->vis = -1;
      else if ((mem->visibility == 1) && (curs->vis == -1))
         curs->vis = 1;
      }

   if (curs->vis == 1)
      {
      curs->vis = 0;
      *iierr = IICSCV_C (display , i , 1);
      }
   }

return;
}

/******************************************************************************/

void polyrefr ( int display, int confn, int memid )

/*
*+
*  Name:
*     polyrefr
*
*  Purpose:
*     Graphics refresh from display list
*
*  Invocation:
*     polyrefr( display, confn, memid )
*
*  Description:
*     Graphics refresh from display list
*
*  Arguments:
*     display = int
*     confn = int
*     memid = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/

{

/* Local Variables */
int    j, k;
int    col, style, np;
int    xs[8192], ys[8192];
int    *x0, *y0, xm, ym;


CONF_DATA  *conf;
MEM_DATA   *mem;
G_LIST     *curr_gel = NULL;

conf = device[display].config[confn];
mem = conf->memory[memid];

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
      for (k = 0; k < np; k++)
         {
         xm = (int) *x0++;
         ym = (int) *y0++;
         mem_dis_conv (display, memid, xm, ym, &xs[k], &ys[k]);
         }
      polyline_d (display, memid, col, style, xs, ys, np);
      }
   }

return;
}

/******************************************************************************/

void txtrefr ( int display, int confn, int memid )

/*
*+
*  Name:
*     txtrefr
*
*  Purpose:
*     Text refresh from display list
*
*  Invocation:
*     txtrefr( display, confn, memid )
*
*  Description:
*     Text refresh from display list
*
*  Arguments:
*     display = int
*     confn = int
*     memid = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Position text relative to memory rather than display
*/

{

/* Local Variables */
int    j;
int    x0, y0, xd, yd, path, orient, color, size;
int    idierr;
char   text[80];

CONF_DATA  *conf;
MEM_DATA   *mem;
T_LIST     *curr_tel = NULL;

conf = device[display].config[confn];
mem = conf->memory[memid];

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
      mem_dis_conv (display, memid, x0, y0, &xd, &yd);

      text_d (display, xd, yd, path, orient, color, size, text, &idierr);
      }
   }

return;
}

/******************************************************************************/

void mem_dis_conv ( int display, int memid, int xmem, int ymem, int* xdis,
                    int* ydis )

/*
*+
*  Name:
*     mem_dis_conv
*
*  Purpose:
*     Memory to display coordinate conversion
*
*  Invocation:
*     mem_dis_conv( display, memid, xmem, ymem, xdis, ydis )
*
*  Description:
*     Memory to display coordinate conversion
*
*  Arguments:
*     display = int
*     memid = int
*     xmem = int
*     ymem = int
*     xdis = int
*     ydis = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     RS:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*      1-FEB_1990 (RS):
*        Remove scroll factors from conversion
*     21-FEB-1991 (NE):
*        Removed redundant code
*/

{

/* Local Variables */
int curconf;

CONF_DATA   *conf;
MEM_DATA    *mem;

if (memid == -1)
   {
   *xdis = xmem;
   *ydis = ymem;
   }
else
   {
   curconf = device[display].confid;
   conf = device[display].config[curconf];
   mem = conf->memory[memid];


   *xdis = xmem * mem->zoom + (int)mem->zoom_xsc ;
   *ydis = ymem * mem->zoom + (int)mem->zoom_ysc ;

   }

return;
}

/******************************************************************************/

void dis_mem_conv ( int display, int memid, int xdis, int ydis, int* xmem,
                    int* ymem )

/*
*+
*  Name:
*     dis_mem_conv
*
*  Purpose:
*     Display to memory coordinate conversion
*
*  Invocation:
*     dis_mem_conv( display, memid, xdis, ydis, xmem, ymem )
*
*  Description:
*     Display to memory coordinate conversion
*
*  Arguments:
*     display = int
*     memid = int
*     xdis = int
*     ydis = int
*     xmem = int
*     ymem = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     RS:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*      1-FEB_1990 (RS):
*        Include scroll factors in conversion
*     21-FEB-1991 (NE):
*        Removed redundant code
*/

{

/* Local Variables */
int curconf;

CONF_DATA   *conf;
MEM_DATA    *mem;

if (memid == -1)
   {
   *xmem = xdis;
   *ymem = ydis;
   }
else
   {
   curconf = device[display].confid;
   conf = device[display].config[curconf];
   mem = conf->memory[memid];

   *xmem = (xdis - (int)mem->zoom_xsc ) / mem->zoom;
   *ymem = (ydis - (int)mem->zoom_ysc ) / mem->zoom;

   }

return;
}

/******************************************************************************/

void cl_bitmap ( int display, int confn, int memid, int bck )

/*
*+
*  Name:
*     cl_bitmap
*
*  Purpose:
*     Clear Main Memory Bitmap
*
*  Invocation:
*     cl_bitmap( display, confn, memid, bck )
*
*  Description:
*     Clear Main Memory Bitmap
*
*  Arguments:
*     display = int
*     confn = int
*     memid = int
*     bck = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     11-NOV-1992 (NE):
*        Dereference colour index through lut as well as curlut
*/

{

/* Local Variables */
unsigned char *bitmap;
int i , ilen;

CONF_DATA *conf;
MEM_DATA  *mem;
LUT_DATA  *lut;

conf = device[display].config[confn];
mem = conf->memory[memid];
lut = device[display].lookup[mem->lut_id];

ilen = mem->x_size * mem->y_size;

bitmap = mem->mmbm;

for (i=0; i<ilen; i++)
    *bitmap++ = (unsigned char) curlut.lutpix[lut->lutpix[bck]];

mem->bck = bck;

return;
}

/******************************************************************************/

void cl_dl ( int display, int confn, int memid )

/*
*+
*  Name:
*     cl_dl
*
*  Purpose:
*     Clear Display List
*
*  Invocation:
*     cl_dl( display, confn, memid )
*
*  Description:
*     Clear Display List
*
*  Arguments:
*     display = int
*     confn = int
*     memid = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/

{

/* Local Variables */
CONF_DATA *conf;
MEM_DATA  *mem;

conf = device[display].config[confn];
mem = conf->memory[memid];

mem->n_gel = 0;
mem->n_tel = 0;

return;
}

/******************************************************************************/

void mmbm_all ( int display, int confn, int memid, int* iierr )

/*
*+
*  Name:
*     mmbm_all
*
*  Purpose:
*     Main memory bitmap allocate
*
*  Invocation:
*     mmbm_all( display, confn, memid, iierr )
*
*  Description:
*     Main memory bitmap allocate
*
*  Arguments:
*     display = int
*     confn = int
*     memid = int
*     iierr = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     23-MAY-1991 (NE):
*        Removed definition of malloc
*/

{

/* Local Variables */
int  bmsize;
unsigned char *curbm;

CONF_DATA *conf;
MEM_DATA  *mem;

*iierr = II_SUCCESS;

conf = device[display].config[confn];
mem = conf->memory[memid];

bmsize = mem->x_size * mem->y_size;
curbm = malloc (bmsize);
if (curbm == NULL)
   {
   *iierr = MEMALLERR;
   return;
   }


mem->mmbm = curbm;
mem->mem_free = 1;

cl_bitmap (display , confn , memid , 0);

return;
}

/******************************************************************************/

void mmbm_deall ( int display, int confn, int memid )

/*
*+
*  Name:
*     mmbm_deall
*
*  Purpose:
*     Main memory bitmap deallocate
*
*  Invocation:
*     mmbm_deall( display, confn, memid )
*
*  Description:
*     Main memory bitmap deallocate
*
*  Arguments:
*     display = int
*     confn = int
*     memid = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/

{

/* Local Variables */
CONF_DATA *conf;
MEM_DATA  *mem;

conf = device[display].config[confn];
mem = conf->memory[memid];

free ( mem->mmbm );

mem->mmbm = NULL;
mem->attbm = 0;
mem->mem_free = 1;

return;
}

/******************************************************************************/

void wr_mem ( int display, int memid, int x0, int y0, int packf, int iy,
              int dd, int depth, int npixel, int data[] )

/*
*+
*  Name:
*     wr_mem
*
*  Purpose:
*     Write main memory bitmap
*
*  Invocation:
*     wr_mem( display, memid, x0, y0, packf, iy, dd, depth, npixel, data )
*
*  Description:
*     Write main memory bitmap
*
*  Arguments:
*     display = int
*     memid = int
*     x0 = int
*     y0 = int
*     packf = int
*     iy = int
*     dd = int
*     depth = int
*     npixel = int
*     data = int[]
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink, RAL)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Define prototype for rint function and removed redundant code
*     19-MAR-1992 (NE):
*        Declare rint function for Ultrix as well as VAX machines
*     24-SEP-1992 (NE):
*        Reverse word/byte order on SUNs
*     15-MAR-1994 (DLT):
*        Implement transfer window load direction
*/

{

/* Local Variables */
int    i, j, jj, k, n, curconf, pix, nb, bl ,yinc;

CONF_DATA  *conf;
MEM_DATA   *mem;
ITT_DATA   *itt;

unsigned char *curbm, *curbm0;

#if !HAVE_DECL_RINT
double rint( double x );
#endif

nb = 32 / packf;              /* no. of pixels in a longword */
                              /* input data mask             */
 bl = (int)rint( pow ((double) 2 , (double)depth) - 1);

curconf = device[display].confid;
conf = device [display].config[curconf];
mem = conf->memory[memid];
itt = mem->itt[mem->itt_id];
yinc = mem->load_dir ? -1 : 1;

n = 0;
j = 0;
jj = 0;
i = 0;

curbm0 = (unsigned char*)(mem->mmbm + (mem->y_size-y0-1)*mem->x_size-
	   ( mem->y_woff * mem->x_size - (mem->x_woff + x0)));
curbm = curbm0;

while (i < iy)
   {
   for (k = 0; k < packf; k++)
      {
/* Reverse word/byte order on SUNs */
#if WORDS_BIGENDIAN
      pix = ((data[jj] >> ((packf - 1 - k) * nb)) & bl) >> dd;
#else
      pix = ((data[jj] >> (k * nb)) & bl) >> dd;
#endif
      *curbm++ = (unsigned char) (curlut.off + itt->ittlev[pix] );
      n++;
      if (n >= npixel) return;
      j++;
      if (j >= mem->x_wdim)
         {
         j = 0;
         i += yinc;
         curbm = curbm0 - (i * mem->x_size);
         }
      }
   jj++;
   }

return;
}

/******************************************************************************/

void rd_mem ( int display, int memid, int x0, int y0, int packf, int npixel,
              int iy, int dd, int depth, int ittf, int data[] )

/*
*+
*  Name:
*     rd_mem
*
*  Purpose:
*     Read main memory bitmap
*
*  Invocation:
*     rd_mem( display, memid, x0, y0, packf, npixel, iy, dd, depth, ittf, data )
*
*  Description:
*     Read main memory bitmap
*
*  Arguments:
*     display = int
*     memid = int
*     x0 = int
*     y0 = int
*     packf = int
*     npixel = int
*     iy = int
*     dd = int
*     depth = int
*     ittf = int
*     data = int[]
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     19-OCT-1991 (NE):
*        Correct order of read and correct padding of data.
*     25-NOV-1991 (NE):
*        Subtract curlut.off from each element of packed output word
*     13-FEB-1992 (NE):
*        Added ITT flag to argument list, and act on it
*     19-MAY-1992 (NE):
*        Fix pen numbers when overlay plane is present.
*     24-SEP-1992 (NE):
*        Reverse word/byte order on SUNs
*/

{

/* Local Variables */
int    i, im, j , jj , k , curconf , pix , nb , bl, shift;

CONF_DATA  *conf;
MEM_DATA   *mem;
ITT_DATA   *itt;

unsigned char *curbm;


nb = 32 / packf;              /* no. of pixels in a longword */

curconf = device[display].confid;
conf = device [display].config[curconf];
mem = conf->memory[memid];
itt = mem->itt[mem->itt_id];

jj = 0;
i = 0;
k = 0;
pix = 0;

/* When an overlay plane is present and the ITT flag is false the */
/* pen numbers need to be fixed so that they lie in the range 0 to */
/* curlut.len - 1 */
if ( (int)~mem->pm_mask > 0 )
   shift = ~mem->pm_mask;
else
   shift = 0;

/* Step through the transfer window line by line */
while (i < iy)
   {
   curbm = (unsigned char*)( mem->mmbm + mem->x_woff + x0 +
           ( mem->y_size - mem->y_woff - y0 - i - 1 ) * mem->x_size );
   j = 0;
   while (j < ((npixel < mem->x_wdim) ? npixel : mem->x_wdim))
      {

/* Apply the inverse intensity transformation if required */
      im = (int)*curbm - curlut.off;
      if ( ittf != 0 )
         im = itt->ittinv[im];

/* Otherwise fix the pen number if an overlay plane is present */
      else
         im = im >> shift;

/* Pack the data into the output word */
/* Reverse word/byte order on SUNs */
#if WORDS_BIGENDIAN
      pix |= (dd>0)? ((im << dd) << ((packf - 1 - k) * nb)):
                     ((im >> dd) << ((packf - 1 - k) * nb));
#else
      pix |= (dd>0)? ((im << dd) << (k * nb)):
                     ((im >> dd) << (k * nb));
#endif
      curbm++;
      j++;
      k = ++k % packf;
      if ( k == 0 )
         {
         data[jj++] = pix;
         pix = 0;
         }
      }
   i++;
   }

/* Output any excess pixels */
if ( k != 0 ) data[jj++] = pix - curlut.off;

return;
}

/******************************************************************************/

void ebwmy ( int display, int confn, int memid, int x0, int y0, int* iierr )

/*
*+
*  Name:
*     ebwmy
*
*  Purpose:
*     External bitmap write memory
*
*  Invocation:
*     ebwmy( display, confn, memid, x0, y0, iierr )
*
*  Description:
*     Write image memory from external bitmap
*
*  Arguments:
*     display = int
*     confn = int
*     memid = int
*     x0 = int
*     y0 = int
*     iierr = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Define prototype for rint function
*     23-MAY-1991 (NE):
*        Removed definition of malloc
*     19-MAR-1992 (NE):
*        Declare rint function for Ultrix as well as VAX machines
*/

{

/* Local Variables */
int            i , j , k , nb , bl , dd , pix , nr;
unsigned char  *tmpbm;
int            *curbm , *curbm0;
#if !HAVE_DECL_RINT
double rint( double x );
#endif

BM_WINDOW      bm0;
BM_ORIGIN      bm1;

CONF_DATA      *conf;
MEM_DATA       *mem;
ITT_DATA       *itt;

FILE           *feb;

if (device[display].feb == 0)
   {
   *iierr = EXTBMNOTFND;
   return;
   }

conf = device [display].config[confn];
mem = conf->memory[memid];
                               /* retrieve external bitmap definitions */
mem->feb  = device[display].feb;
mem->x_wdim = device[display].extbm_xsize;
mem->y_wdim = (device[display].extbm_ysize < mem->y_size) ?
               device[display].extbm_ysize : mem->y_size;
mem->ebdepth = device[display].ebdepth;
mem->ebpackf = device[display].ebpackf;

itt = mem->itt[mem->itt_id];

nb = 32 / mem->ebpackf;       /* no. of pixels in a longword */
                              /* input data mask             */
bl = (int) rint(pow ((double) 2. , (double) mem->ebdepth) - 1);
                              /* no. of bits to truncate     */
dd = mem->ebdepth - mem->depth;

bm0.x_coord = 0;
bm0.y_coord = 0;
bm0.x_size  = mem->x_wdim;
bm0.y_size  = mem->y_wdim;

bm1.x_coord = mem->x_woff + x0;
bm1.y_coord = mem->y_woff + y0;

feb = mem->feb;

curbm0 = (int *) malloc (bm0.x_size * 4);

i = 0;
while (i < bm0.y_size)
   {
   tmpbm = (unsigned char *) (mem->mmbm + (bm1.y_coord + i) *
                              mem->x_size + bm1.x_coord);

   curbm = curbm0;
   nr = fread (curbm , 4 , bm0.x_size/mem->ebpackf , feb);

   j = 0;
   while (j < bm0.x_size)
      {
      for (k = 0; k < mem->ebpackf; k++)
         {
         pix = ((*curbm >> (k * nb)) & bl) >> dd;
         *tmpbm++ = (unsigned char) (curlut.off + itt->ittlev[pix]);
         j++;
         }
      curbm++;
      }
   i++;
   }

mem->mem_free = 0;
*iierr = II_SUCCESS;

return;
}

/******************************************************************************/

void eb_openi ( int display, char bmdscr[], int* xsize, int* ysize,
                int* packf, int* ebdepth, int* idierr )

/*
*+
*  Name:
*     eb_openi
*
*  Purpose:
*     External Bitmap Open (Read)
*
*  Invocation:
*     eb_openi( display, bmdscr, xsize, ysize, packf, ebdepth, idierr )
*
*  Description:
*     ad interim format of external bitmap file
*     eb_label           : 80 char
*     eb_date            : 6 char
*     depth, packf       : 2 int :  bitmap depth (bits/pixel)
*                                packing factor (pixels/int)
*     xsize, ysize       : 2 int :  X , Y bitmap size (pixels)
*     data               : Xsize*Ysize/packf int
*     lut                : 3 * (2**depth) int
*
*  Arguments:
*     display = int
*     bmdscr = char[]
*     xsize = int
*     ysize = int
*     packf = int
*     ebdepth = int
*     idierr = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     23-MAY-1991 (NE):
*        Removed definition of malloc and fopen
*/

{

/* Local Variables */
char           eb_label[80] , eb_date[6];
int            nb;
FILE           *feb;


if ((feb = fopen(bmdscr , "r")) == 0)
   {
   *idierr = EXTBMNOTFND;
   return;
   }

/* save external bitmap file descriptor */

device[display].feb = feb;

nb = fread (eb_label , 1 , 80 , feb);
nb = fread (eb_date , 1 , 6 , feb);
nb = fread (ebdepth , 4 , 1 , feb);
nb = fread (packf , 4 , 1 , feb);
nb = fread (xsize , 4 , 1 , feb);
nb = fread (ysize , 4 , 1 , feb);

*idierr = II_SUCCESS;
return;
}

/******************************************************************************/

void loc_zero ( int display )

/*
*+
*  Name:
*     loc_zero
*
*  Purpose:
*     Locator positions zero
*
*  Invocation:
*     loc_zero( display )
*
*  Description:
*     Locator positions zero
*
*  Arguments:
*     display = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/

{

/* Local Variables */
int   i , j;

INT_DEV_DATA   *intdev;
LOC_DATA       *loc;

for (i = 0; i < int_struct.n_int_dev; i++)
   {
   intdev = int_struct.int_dev[i];
   for (j = 0; j < intdev->n_loc; j++)
      {
      loc = intdev->loc[j];
      loc->x_pos = 0;
      loc->y_pos = 0;
      }
   }

return;
}

/******************************************************************************/

void test_user ( int display, int nint, short ev_type, short ev_data,
                 short pos[], int ew, int* user_flag, int* trg_flag )

/*
*+
*  Name:
*     test_user
*
*  Purpose:
*     Test user
*
*  Invocation:
*     test_user( display, nint, ev_type, ev_data, pos, ew,
*                user_flag, trg_flag )
*
*  Description:
*     Check USER enabled interaction
*
*  Arguments:
*     display = int
*     nint = int
*     ev_type = short
*     ev_data = short
*     pos = short[]
*     ew = int
*     user_flag = int
*     trg_flag = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/

{

/* Local Variables */
int   loc_flag , evl_flag;

INTER_DATA     *intdata;

intdata = device[display].inter[nint];

*user_flag = 0;

if (intdata->int_type == II_LOC)
   {                          /* interactor type : II_LOCATOR ? */
   sync_loc (display, nint, ev_type, ev_data, pos, &loc_flag);
   if (loc_flag == 1)
      *user_flag = 1;
   }
else if ((intdata->int_type == II_EVLR) ||
         (intdata->int_type == II_EVLI) ||
         (intdata->int_type == II_EVLS) ||
         (intdata->int_type == II_EVLT))
   {                          /* interactor type : EVALUATOR ? */
   sync_evl (display, nint, ev_type, ev_data, &evl_flag);
   if (evl_flag != 0)
      {
      *trg_flag = *trg_flag | evl_flag;
      *user_flag = 1;
      }
   }
else if (intdata->int_type == II_TRG)
   {                          /* interactor type : TRIGGER ? */
   sync_trg (display, nint, ev_type, ev_data, trg_flag);
   if (*trg_flag != -1)
      *user_flag = 1;
   }
return;
}

/******************************************************************************/

void sync_loc ( int display, int nint, short ev_type, short ev_data,
                short pos[], int* loc_flag )

/*
*+
*  Name:
*     sync_loc
*
*  Purpose:
*     Sync loc
*
*  Invocation:
*     sync_loc( display, nint, ev_type, ev_data, pos, loc_flag )
*
*  Description:
*     Test LOCATOR interaction
*
*  Arguments:
*     display = int
*     nint = int
*     ev_type = short
*     ev_data = short
*     pos = short[]
*     loc_flag = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/

{

/* Local Variables */
int   interactor_id, loc_id, loc0, f0, zf;

INTER_DATA     *intdata;

intdata = device[display].inter[nint];
loc0 = intdata->int_id;

get_loc (loc0, &interactor_id, &loc_id);

zf = 1;
test_loc (display, zf, ev_type, ev_data, pos, interactor_id,
          loc_id, &f0);

*loc_flag = f0;
return;
}

/******************************************************************************/

void sync_evl ( int display, int nint, short ev_type, short ev_data,
                int* evl_flag )

/*
*+
*  Name:
*     sync_evl
*
*  Purpose:
*     Sync evl
*
*  Invocation:
*     sync_evl( display, nint, ev_type, ev_data, evl_flag )
*
*  Description:
*     Test EVALUATOR interaction
*
*  Arguments:
*     display = int
*     nint = int
*     ev_type = short
*     ev_data = short
*     evl_flag = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/

{

/* Local Variables */
int   interactor_id, evl_id, evl0, f0;

INTER_DATA     *intdata;

intdata = device[display].inter[nint];
evl0 = intdata->int_id;

get_evl (evl0, &interactor_id, &evl_id);

test_evl (display, ev_type, ev_data, interactor_id, evl_id, &f0);

*evl_flag = - f0 * evl0;
return;
}

/******************************************************************************/

void sync_trg ( int display, int nint, short ev_type, short ev_data,
                int* trg_flag )

/*
*+
*  Name:
*     sync_trg
*
*  Purpose:
*     Sync trigger
*
*  Invocation:
*     sync_trg( display, nint, ev_type, ev_data, trg_flag )
*
*  Description:
*     Test TRIGGER interaction
*
*  Arguments:
*     display = int
*     nint = int
*     ev_type = short
*     ev_data = short
*     trg_flag = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     30-JUL-1991 (NE):
*        Allow for more than one exit trigger
*/

{

/* Local Variables */
int   i, interactor_id, trg_id, trg0, f0;

INTER_DATA     *intdata;

/* nint = -1 for Exit Trigger */
if (nint == -1)
   {
   *trg_flag = -1;
   for ( i = 0; i < device[display].n_inter; i++ )
      {
      intdata = device[display].inter[i];
      trg0 = intdata->trigger;

      get_trg( trg0, &interactor_id, &trg_id );
      test_trg( ev_type, ev_data, interactor_id, trg_id, &f0 );
      if ( f0 != 0 )
         *trg_flag = trg0;
      }
   }

/*  nint >= 0 for interaction[nint] */
else
   {
   intdata = device[display].inter[nint];
   trg0 = intdata->int_id;

   get_trg( trg0, &interactor_id, &trg_id );
   test_trg( ev_type, ev_data, interactor_id, trg_id, &f0 );
   if ( f0 == 0 )
      *trg_flag = -1;
   else
      *trg_flag = trg0;
   }

return;
}

/******************************************************************************/

void get_loc ( int loc0, int* interactor_id, int* loc_id )

/*
*+
*  Name:
*     get_loc
*
*  Purpose:
*     Get locator id
*
*  Invocation:
*     get_loc( loc0, interactor_id, loc_id )
*
*  Description:
*     From absolute locator id get relative (to interactor device) id
*
*  Arguments:
*     loc0 = int
*     interactor_id = int
*     loc_id = short
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/

{

/* Local Variables */
int  n;

n = 0;
*interactor_id = -1;

while ((n + int_struct.int_dev[*interactor_id + 1]->n_loc) <= loc0)
   {
   *interactor_id += 1;
   n += int_struct.int_dev[*interactor_id]->n_loc;
   }

*interactor_id += 1;                        /* Interactor id       */
*loc_id = loc0 - n;                         /* Relative locator id */

return;
}

/******************************************************************************/

void get_evl ( int evl0, int* interactor_id, int* evl_id )

/*
*+
*  Name:
*     get_evl
*
*  Purpose:
*     Get evaluator id
*
*  Invocation:
*     get_evl( evl0, interactor_id, evl_id )
*
*  Description:
*     From absolute evaluator id get relative (to interactor device) id
*
*  Arguments:
*     evl0 = int
*     interactor_id = int
*     evl_id = short
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/

{

/* Local Variables */
int  n;

n = 0;
*interactor_id = -1;

while ((n + int_struct.int_dev[*interactor_id + 1]->n_evl) <= evl0)
   {
   *interactor_id += 1;
   n += int_struct.int_dev[*interactor_id]->n_evl;
   }

*interactor_id += 1;                        /* Interactor id         */
*evl_id = evl0 - n;                         /* Relative evaluator id */

return;
}

/******************************************************************************/

void get_trg ( int trg0, int* interactor_id, int* trg_id )

/*
*+
*  Name:
*     get_trg
*
*  Purpose:
*     Get trigger id
*
*  Invocation:
*     get_trg( trg0, interactor_id, trg_id )
*
*  Description:
*     From absolute trigger id get relative (to interactor device) id
*
*  Arguments:
*     trg0 = int
*     interactor_id = int
*     trg_id = short
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/

{

/* Local Variables */
int  n;

n = 0;
*interactor_id = -1;

while ((n + int_struct.int_dev[*interactor_id + 1]->n_trg) <= trg0)
   {
   *interactor_id += 1;
   n += int_struct.int_dev[*interactor_id]->n_trg;
   }

*interactor_id += 1;                        /* Interactor id       */
*trg_id = trg0 - n;                         /* Relative trigger id */

return;
}

/******************************************************************************/

void cursor_move ( int display, int nint, short ev_type, short ev_data,
                   short pos[], int ew, int* err )

/*
*+
*  Name:
*     cursor_move
*
*  Purpose:
*     Cursor move
*
*  Invocation:
*     cursor_move( display, nint, ev_type, ev_data, pos, ew, err )
*
*  Description:
*     Cursor move
*
*  Arguments:
*     display = int
*        Display identifier
*     nint = int
*        Interaction number
*     ev_type = short
*        Interaction type (mouse , keyboard ...
*     ev_data = short
*        Interaction output (mouse-button , key ....
*     pos = short[]
*        Cursor position
*     ew = int
*        Enter window flag ( discard this interaction )
*     err = int
*        Status
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/

{

/* Local Variables */
int     loc0, interactor_id, loc_id, curs_id, f0;
int     curconf, memid, x_cur, y_cur, zf;

CONF_DATA      *conf;
MEM_DATA       *mem;
INTER_DATA     *intdata;
INT_DEV_DATA   *intdev;
CURS_DATA      *curs;
LOC_DATA       *loc;

*err = II_SUCCESS;

intdata = device[display].inter[nint];
curs_id = intdata->obj_id;
curs = device[display].cursor[curs_id];

if (curs->cur_sh == -1)
   {                                     /* cursor not defined */
   *err = CURNOTDEF;
   return;
   }
if (curs->vis == 0)
   {                                     /* cursor not visible */
   *err = CURNOTVIS;
   return;
   }

loc0 = intdata->int_id;
get_loc ( loc0, &interactor_id, &loc_id);

intdev = int_struct.int_dev[interactor_id];
loc = intdev->loc[loc_id];

if (ew == 1)
   {
   loc->x_pos = curs->x_pos;
   loc->y_pos = curs->y_pos;
   return;
   }

if (curs->cur_memid != -1)
   {
   memid = curs->cur_memid;

   curconf = device[display].confid;
   conf = device[display].config[curconf];
   mem = conf->memory[memid];
   zf = mem->zoom * device[display].zoom;
   }
else
   zf = device[display].zoom;


test_loc (display, zf, ev_type, ev_data, pos, interactor_id,
          loc_id , &f0);

if (f0 == 1)
   {
   memid = curs->cur_memid;

   dis_mem_conv (display, curs->cur_memid, loc->x_pos, loc->y_pos ,
                 &x_cur , &y_cur);

   *err = IICWCP_C (display, memid, curs_id, x_cur, y_cur);
   }

return;
}

/******************************************************************************/

void roi_move ( int display, int nint, short ev_type, short ev_data,
                short pos[], int ew, int* err )

/*
*+
*  Name:
*     roi_move
*
*  Purpose:
*     Roi move
*
*  Invocation:
*     roi_move( display, nint, ev_type, ev_data, pos, ew, err )
*
*  Description:
*     Roi move
*
*  Arguments:
*     display = int
*        Display identifier
*     nint = int
*        Interaction number
*     ev_type = short
*        Interaction type (mouse , keyboard ...
*     ev_data = short
*        Interaction output (mouse-button , key ....
*     pos = short[]
*        Cursor position
*     ew = int
*        Enter window flag ( discard this interaction )
*     err = int
*        Status
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Move relative to active corner rather than centre of roi
*     30-APR-1991 (NE):
*        For enter window event convert memory to display coordinates
*     26-NOV-1991 (NE):
*        Allow for memid = -1
*/

{

/* Local Variables */
int    loc0, interactor_id, loc_id, f0;
int    curconf, memid, zf, roin;
int    xcur, ycur;
int    dimx, dimy;
int    xmin, ymin, xmax, ymax;

CONF_DATA      *conf;
MEM_DATA       *mem;
INTER_DATA     *intdata;
INT_DEV_DATA   *intdev;
LOC_DATA       *loc;
ROI_DATA       *roi;

*err = II_SUCCESS;

intdata = device[display].inter[nint];
roin = intdata->obj_id;
roi = device[display].roi[roin];

if (roi->sh == -1)
   {                                     /* roi not defined */
   *err = ROINOTDEF;
   return;
   }
if (roi->vis == 0)
   {                                     /* roi not visible */
   *err = ROINOTVIS;
   return;
   }

loc0 = intdata->int_id;
get_loc ( loc0 , &interactor_id , &loc_id);

intdev = int_struct.int_dev[interactor_id];
loc = intdev->loc[loc_id];

dimx = roi->x_max - roi->x_min;
dimy = roi->y_max - roi->y_min;

if (ew == 1)
   {

/* Move relative to active corner ( lower left ) */
   mem_dis_conv( display, roi->memid, roi->x_min, roi->y_min,
                 &xcur, &ycur );
   loc->x_pos = xcur;
   loc->y_pos = ycur;
   roi->corner = 0;
   return;
   }

memid = roi->memid;

curconf = device[display].confid;
conf = device[display].config[curconf];
if ( memid == -1 )
   zf = device[display].zoom;
else
   {
   mem = conf->memory[memid];
   zf = mem->zoom;
   }

test_loc (display, zf, ev_type, ev_data, pos, interactor_id,
          loc_id , &f0);

if (f0 == 1)
   {
   dis_mem_conv (display, roi->memid, loc->x_pos, loc->y_pos,
                 &xcur , &ycur);

/* Define lower left of roi as active corner */
   xmin = xcur;
   ymin = ycur;
   xmax = xmin + dimx;
   ymax = ymin + dimy;

   *err = IIRWRI_C (display, roi->memid, roin, xmin, ymin, xmax, ymax);
   }

return;
}

/******************************************************************************/

void roi_modify ( int display, int nint, short ev_type, short ev_data,
                  short pos[], int ew, int* err )

/*
*+
*  Name:
*     roi_modify
*
*  Purpose:
*     Roi modify
*
*  Invocation:
*     roi_modify( display, nint, ev_type, ev_data, pos, ew, err )
*
*  Description:
*     Roi modify
*
*  Arguments:
*     display = int
*        Display identifier
*     nint = int
*        Interaction number
*     ev_type = short
*        Interaction type (mouse , keyboard ...
*     ev_data = short
*        Interaction output (mouse-button , key ....
*     pos = short[]
*        Cursor position
*     ew = int
*        Enter window flag ( discard this interaction )
*     err = int
*        Status
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Modify roi by moving active corner
*     30-APR-1991 (NE):
*        For enter window event convert memory to display coordinates
*/

{

/* Local Variables */
int   loc0, interactor_id, loc_id, f0;
int   curconf, memid, zf, roin;
int   x0, x1, y0, y1, dimx, dimy, dx, dy;
int   temp, xmin, ymin, xmax, ymax;
int   xmin0, ymin0, xmax0, ymax0;
int   xcur, ycur;

CONF_DATA      *conf;
MEM_DATA       *mem = NULL;
INTER_DATA     *intdata;
INT_DEV_DATA   *intdev;
LOC_DATA       *loc;
ROI_DATA       *roi;

*err = II_SUCCESS;

xmin = 0; xmax = 0; ymin = 0; ymax = 0;

curconf = device[display].confid;
conf = device[display].config[curconf];

intdata = device[display].inter[nint];
roin = intdata->obj_id;
roi = device[display].roi[roin];
if (roi->memid > 0)
   mem = conf->memory[roi->memid];

if (roi->sh == -1)
   {                                     /* roi not defined */
   *err = ROINOTDEF;
   return;
   }
if (roi->vis == 0)
   {                                     /* roi not visible */
   *err = ROINOTVIS;
   return;
   }

loc0 = intdata->int_id;
get_loc ( loc0 , &interactor_id , &loc_id);

intdev = int_struct.int_dev[interactor_id];
loc = intdev->loc[loc_id];

if (ew == 1)
/* Update locator position according to which corner is active */
   {
   switch ( roi->corner )
      {
      case 0:
         mem_dis_conv( display, roi->memid, roi->x_min, roi->y_min,
                       &xcur, &ycur );
         loc->x_pos = xcur;
         loc->y_pos = ycur;
         break;
      case 1:
         mem_dis_conv( display, roi->memid, roi->x_max, roi->y_max,
                       &xcur, &ycur );
         loc->x_pos = xcur;
         loc->y_pos = ycur;
         break;
      case 2:
         mem_dis_conv( display, roi->memid, roi->x_min, roi->y_max,
                       &xcur, &ycur );
         loc->x_pos = xcur;
         loc->y_pos = ycur;
         break;
      case 3:
         mem_dis_conv( display, roi->memid, roi->x_max, roi->y_min,
                       &xcur, &ycur );
         loc->x_pos = xcur;
         loc->y_pos = ycur;
         break;
      }
   return;
   }

x0 = loc->x_pos;
y0 = loc->y_pos;
dimx = roi->x_max - roi->x_min;
dimy = roi->y_max - roi->y_min;

memid = roi->memid;

if (memid >= 0)
   {
   curconf = device[display].confid;
   conf = device[display].config[curconf];
   mem = conf->memory[memid];
   zf = mem->zoom;
   }
else
   zf = 1;

test_loc (display, zf, ev_type, ev_data, pos, interactor_id,
          loc_id, &f0);

if (f0 == 1)
   {
   x1 = loc->x_pos;
   y1 = loc->y_pos;

/* Calculate size of roi */
   dx = x1 - x0;
   dy = y1 - y0;

   if (roi->sh == II_CIRCLE)
      {
      dx = ((dx > 0) || (dy > 0)) ? ((dx > dy) ? dx : dy) :
                                    ((dx < dy) ? dx : dy);
      dy = dx;
      }


   if (roi->memid >= 0)
      {
      dx *= mem->zoom;
      dy *= mem->zoom;
      }

/* Calculate new lower and upper limits of roi */
   switch ( roi->corner )
      {
      case 0:
         xmin = roi->x_min + dx;
         xmax = roi->x_max;
         ymin = roi->y_min + dy;
         ymax = roi->y_max;
         break;
      case 1:
         xmin = roi->x_min;
         xmax = roi->x_max + dx;
         ymin = roi->y_min;
         ymax = roi->y_max + dy;
         break;
      case 2:
         xmin = roi->x_min + dx;
         xmax = roi->x_max;
         ymin = roi->y_min;
         ymax = roi->y_max + dy;
         break;
      case 3:
         xmin = roi->x_min;
         xmax = roi->x_max + dx;
         ymin = roi->y_min + dy;
         ymax = roi->y_max;
         break;
      }

/* Switch active corners if necessary */
   if ( ( xmin > xmax ) && ( ymin > ymax ) )
      {
      switch ( roi->corner )
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
      }
   if ( ( xmin <= xmax ) && ( ymin > ymax ) )
      {
      switch ( roi->corner )
         {
         case 0:
            roi->corner = 2;
            break;
         case 1:
            roi->corner = 3;
            break;
         case 2:
            roi->corner = 0;
            break;
         case 3:
            roi->corner = 1;
            break;
         }
      }
   if ( ( xmin > xmax ) && ( ymin <= ymax ) )
      {
      switch ( roi->corner )
         {
         case 0:
            roi->corner = 3;
            break;
         case 1:
            roi->corner = 2;
            break;
         case 2:
            roi->corner = 1;
            break;
         case 3:
            roi->corner = 0;
            break;
         }
      }
   if ( xmin > xmax )
      {
      temp = xmin;
      xmin = xmax;
      xmax = temp;
      }
   if ( ymin > ymax )
      {
      temp = ymin;
      ymin = ymax;
      ymax = temp;
      }

   if (roi->memid != -1)
      {
      dis_mem_conv (display, roi->memid, xmin, ymin,
                    &xmin0, &ymin0);
      dis_mem_conv (display, roi->memid, xmax, ymax,
                    &xmax0, &ymax0);
      }
   else
      {
      xmin0 = xmin;
      ymin0 = ymin;
      xmax0 = xmax;
      ymax0 = ymax;
      }
   *err = IIRWRI_C (display, roi->memid, roin, xmin0, ymin0,
                    xmax0, ymax0);
   }

return;
}

/******************************************************************************/

void lut_rotate ( int display, int nint, short ev_type, short ev_data,
                  int* err )

/*
*+
*  Name:
*     lut_rotate
*
*  Purpose:
*     Lut rotate
*
*  Invocation:
*     lut_rotate( display, nint, ev_type, ev_data, err )
*
*  Description:
*     Lut rotate
*
*  Arguments:
*     display = int
*        Display identifier
*     nint = int
*        Interaction number
*     ev_type = short
*        Interaction type (mouse , keyboard ...
*     ev_data = short
*        Interaction output (mouse-button , key ....
*     err = int
*        Status
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     23-MAY-1991 (NE):
*        Removed definition of abs
*/

{

/* Local Variables */
INTER_DATA       *intdata;
INT_DEV_DATA     *intdev;
EVL_DATA         *evl;

int              i, j, interactor_id, evl0, evl_id, f0;
int              tmpr, tmpg, tmpb, drot, rotdir;

intdata = device[display].inter[nint];
evl0 = intdata->int_id;

get_evl (evl0, &interactor_id, &evl_id);

intdev = int_struct.int_dev[interactor_id];
evl = intdev->evl[evl_id];
evl->ival = 0;

test_evl (display, ev_type, ev_data, interactor_id, evl_id, &f0);

if (f0 == 1)
   {
   intdev = int_struct.int_dev[interactor_id];
   evl = intdev->evl[evl_id];
   drot = evl->ival;
   if (drot != 0)
      rotdir = drot / abs (drot);
   else
      rotdir = 1;

   if (rotdir == 1)
      {
      for (i = 0; i < drot; i++)
         {
         tmpr = curlut.lutr[curlut.len - 1];
         tmpg = curlut.lutg[curlut.len - 1];
         tmpb = curlut.lutb[curlut.len - 1];
         for (j = curlut.len - 1; j > 0 ; j--)
            {
            curlut.lutr[j] = curlut.lutr[j - 1];
            curlut.lutg[j] = curlut.lutg[j - 1];
            curlut.lutb[j] = curlut.lutb[j - 1];
            }
         curlut.lutr[0] = tmpr;
         curlut.lutg[0] = tmpg;
         curlut.lutb[0] = tmpb;
         }
      }
   else
      {
      for (i = 0; i < abs (drot); i++)
         {
         tmpr = curlut.lutr[0];
         tmpg = curlut.lutg[0];
         tmpb = curlut.lutb[0];
         for (j = 0; j < curlut.len - 1; j++)
            {
            curlut.lutr[j] = curlut.lutr[j + 1];
            curlut.lutg[j] = curlut.lutg[j + 1];
            curlut.lutb[j] = curlut.lutb[j + 1];
            }
         curlut.lutr[curlut.len - 1] = tmpr;
         curlut.lutg[curlut.len - 1] = tmpg;
         curlut.lutb[curlut.len - 1] = tmpb;
         }
      }

   evl->ival = 0;

   wr_lut(display);
   }

*err = II_SUCCESS;

return;
}

/******************************************************************************/

void lut_slice ( int display, int nint, short ev_type, short ev_data,
                 int* err )

/*
*+
*  Name:
*     lut_slice
*
*  Purpose:
*     Lut slice
*
*  Invocation:
*     lut_slice( display, nint, ev_type, ev_data, err )
*
*  Description:
*     Lut slice
*
*  Arguments:
*     display = int
*        Display identifier
*     nint = int
*        Interaction number
*     ev_type = short
*        Interaction type (mouse , keyboard ...
*     ev_data = short
*        Interaction output (mouse-button , key ....
*     err = int
*        Status
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     23-MAY-1991 (NE):
*        Added stdlib.h for abs
*/

{

/* Local Variables */
INTER_DATA       *intdata;
INT_DEV_DATA     *intdev;
EVL_DATA         *evl;

int              i, interactor_id, evl0, evl_id, f0;
int              tmpr, tmpg, tmpb, drot, rotdir;

intdata = device[display].inter[nint];
evl0 = intdata->int_id;

get_evl (evl0, &interactor_id, &evl_id);

test_evl (display, ev_type, ev_data, interactor_id, evl_id, &f0);

if (f0 == 1)
   {
   intdev = int_struct.int_dev[interactor_id];
   evl = intdev->evl[evl_id];
   drot = evl->ival;
   if (drot != 0)
      rotdir = drot / abs (drot);
   else
      rotdir = 1;

   for (i = 0; i < abs (drot); i++)
      {
      tmpr = curlut.lutr[sli_ind];
      tmpg = curlut.lutg[sli_ind];
      tmpb = curlut.lutb[sli_ind];
      curlut.lutr[sli_ind] = sli_r;
      curlut.lutg[sli_ind] = sli_g;
      curlut.lutb[sli_ind] = sli_b;
      sli_ind += rotdir;

      if (sli_ind > curlut.len )
         sli_ind = 0;
      if (sli_ind < 0)
         sli_ind = curlut.len;

      sli_r = curlut.lutr[sli_ind];
      sli_g = curlut.lutg[sli_ind];
      sli_b = curlut.lutb[sli_ind];
      curlut.lutr[sli_ind] = tmpr;
      curlut.lutg[sli_ind] = tmpg;
      curlut.lutb[sli_ind] = tmpb;
      }

   evl->ival = 0;

   wr_lut ( display );
   }

*err = II_SUCCESS;
return;
}

/******************************************************************************/

void mem_scroll ( int display, int nint, short ev_type, short ev_data,
                  short pos[], int ew, int* err )

/*
*+
*  Name:
*     mem_scroll
*
*  Purpose:
*     Memory scroll
*
*  Invocation:
*     mem_scroll( display, nint, ev_type, ev_data, pos, ew, err )
*
*  Description:
*     Memory scroll
*
*  Arguments:
*     display = int
*        Display identifier
*     nint = int
*        Interaction number
*     ev_type = short
*        Interaction type (mouse , keyboard ...
*     ev_data = short
*        Interaction output (mouse-button , key ....
*     pos = short[]
*        Cursor position
*     ew = int
*        Enter window flag ( discard this interaction )
*     err = int
*        Status
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/

{

/* Local Variables */
short   pos0[2];

int     curconf, loc0, interactor_id, loc_id, memid, zf, f0;
int     nmem, memlist[MAX_MEM], x_scr, y_scr;

CONF_DATA      *conf;
MEM_DATA       *mem;
INTER_DATA     *intdata;
INT_DEV_DATA   *intdev;
LOC_DATA       *loc;

*err = II_SUCCESS;

pos0[0] =0;
pos0[1] =0;

intdata = device[display].inter[nint];
memid = intdata->obj_id;

curconf = device[display].confid;
conf = device[display].config[curconf];
mem = conf->memory[memid];

if (mem->zoom > 1)
   {
   *err = INTNOTALL;              /* scroll not implemented if zoom > 1 */
   return;
   }

loc0 = intdata->int_id;
get_loc (loc0, &interactor_id, &loc_id);

intdev = int_struct.int_dev[interactor_id];
loc = intdev->loc[loc_id];

if (ew == 1)
   return;


zf = mem->zoom;

test_loc (display, zf, ev_type, ev_data, pos, interactor_id,
          loc_id , &f0);

if (f0 == 1)
   {
   x_scr = loc->x_pos;
   y_scr = loc->y_pos;

   nmem = 1;
   memlist[0] = memid;

   *err = IIZWSC_C (display, memlist, nmem, x_scr, y_scr);
   }

return;
}

/******************************************************************************/

void mem_zoom ( int display, int nint, short ev_type, short ev_data, int* err )

/*
*+
*  Name:
*     mem_zoom
*
*  Purpose:
*     Memory zoom
*
*  Invocation:
*     mem_zoom( display, nint, ev_type, ev_data, err )
*
*  Description:
*     Interactive zoom execution, connected to a trigger
*
*  Arguments:
*     display = int
*        Display identifier
*     nint = int
*        Interaction number
*     ev_type = short
*        Interaction type (mouse , keyboard ...
*     ev_data = short
*        Interaction output (mouse-button , key ....
*     err = int
*        Status
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     30-APR-1991 (NE):
*        Set roi_reset after zoom
*/

{

/* Local Variables */
int   interactor_id, trg_id, trg0, f0, nmem, zoomfact;
int   memlist[MAX_MEM], curconf;

CONF_DATA      *conf;
MEM_DATA       *mem;
INTER_DATA     *intdata;

intdata = device[display].inter[nint];
trg0 = intdata->int_id;

get_trg (trg0, &interactor_id, &trg_id);

test_trg (ev_type, ev_data, interactor_id, trg_id, &f0);

if (f0 == 1)
   {
   switch (intdata->obj_type)
      {
      case II_MEMORY:
         nmem = 1;
         memlist[0] = intdata->obj_id;
         curconf = device[display].confid;
         conf = device[display].config[curconf];
         mem = conf->memory[intdata->obj_id];
         zoomfact = mem->zoom + 1;
         *err = IIZWZM_C (display, memlist, nmem, zoomfact);
         break;
      }

/* The ROI locator position has to be reset after a zoom */
   roi_reset = 1;
   }
return;
}

/******************************************************************************/

void mem_unzoom ( int display, int nint, short ev_type, short ev_data,
                  int* err )

/*
*+
*  Name:
*     mem_unzoom
*
*  Purpose:
*     Memory unzoom
*
*  Invocation:
*     mem_unzoom( display, nint, ev_type, ev_data, err )
*
*  Description:
*     Interactive unzoom execution, connected to a trigger
*
*  Arguments:
*     display = int
*        Display identifier
*     nint = int
*        Interaction number
*     ev_type = short
*        Interaction type (mouse , keyboard ...
*     ev_data = short
*        Interaction output (mouse-button , key ....
*     err = int
*        Status
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     30-APR-1991 (NE):
*        Set roi_reset after zoom
*/

{

/* Local Variables */
int   interactor_id, trg_id, trg0, f0, nmem, zoomfact;
int    memlist[MAX_MEM], curconf;

CONF_DATA      *conf;
MEM_DATA       *mem;
INTER_DATA     *intdata;

intdata = device[display].inter[nint];
trg0 = intdata->int_id;

get_trg (trg0, &interactor_id, &trg_id);

test_trg (ev_type, ev_data, interactor_id, trg_id, &f0);

if (f0 == 1)
   {
   switch (intdata->obj_type)
      {
      case II_MEMORY:
         nmem = 1;
         memlist[0] = intdata->obj_id;
         curconf = device[display].confid;
         conf = device[display].config[curconf];
         mem = conf->memory[intdata->obj_id];
         zoomfact = mem->zoom - 1;
         if (zoomfact == 0)
            zoomfact = 1;
         *err = IIZWZM_C (display, memlist, nmem, zoomfact);
         break;
      }

/* The ROI locator position has to be reset after a zoom */
   roi_reset = 1;
   }
return;
}

/******************************************************************************/

void mem_clzoom ( int display, int nint, short ev_type, short ev_data,
                  int* err )

/*
*+
*  Name:
*     mem_clzoom
*
*  Purpose:
*     Memory clzoom
*
*  Invocation:
*     mem_clzoom( display, nint, ev_type, ev_data, err )
*
*  Description:
*     Interactive clear zoom execution, connected to a trigger
*
*  Arguments:
*     display = int
*        Display identifier
*     nint = int
*        Interaction number
*     ev_type = short
*        Interaction type (mouse , keyboard ...
*     ev_data = short
*        Interaction output (mouse-button , key ....
*     err = int
*        Status
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     30-APR-1991 (NE):
*        Set roi_reset after zoom
*/

{

/* Local Variables */
int   interactor_id, trg_id, trg0, f0, nmem, zoomfact;
int   memlist[MAX_MEM], curconf;

CONF_DATA      *conf;
MEM_DATA       *mem;
INTER_DATA     *intdata;

intdata = device[display].inter[nint];
trg0 = intdata->int_id;

get_trg (trg0, &interactor_id, &trg_id);

test_trg (ev_type, ev_data, interactor_id, trg_id, &f0);

if (f0 == 1)
   {
   switch (intdata->obj_type)
      {
      case II_MEMORY:
         nmem = 1;
         memlist[0] = intdata->obj_id;
         curconf = device[display].confid;
         conf = device[display].config[curconf];
         mem = conf->memory[intdata->obj_id];
         zoomfact = 1;
         *err = IIZWZM_C (display, memlist, nmem, zoomfact);
         break;
      }

/* The ROI locator position has to be reset after a zoom */
   roi_reset = 1;
   }
return;
}

/******************************************************************************/

void polyline_dl ( int display, int memid, int color, int style, int xs[],
                   int ys[], int np )

/*
*+
*  Name:
*     polyline_dl
*
*  Purpose:
*     Polyline : display list management
*
*  Invocation:
*     polyline_dl( display, memid, color, style, xs, ys, np )
*
*  Description:
*     Polyline : display list management
*
*  Arguments:
*     display = int
*     memid = int
*     color = int
*     style = int
*     xs = int[]
*     ys = int[]
*     np = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     23-MAY-1991 (NE):
*        Removed definition of malloc
*/

{

/* Local Variables */
int                  i , curconf;

CONF_DATA            *conf;
MEM_DATA             *mem;

int      *xl0 , *yl0;
G_LIST   *curr_g_el , *g_last_el;


curconf = device[display].confid;
conf = device[display].config[curconf];
mem = conf->memory[memid];

curr_g_el = (struct g_list *) malloc (sizeof(struct g_list));

mem->n_gel += 1;

curr_g_el->geln = mem->n_gel - 1;
curr_g_el->color = color;
curr_g_el->style = style;
curr_g_el->np = np;

xl0 = (int *) malloc (np * 4);
yl0 = (int *) malloc (np * 4);

curr_g_el->xl = xl0;
curr_g_el->yl = yl0;
for (i = 0; i < np; i++)
   {
   *xl0++ = xs[i];
   *yl0++ = ys[i];
   }

if (mem->n_gel > 1)
   {
   g_last_el = mem->g_last_el;
   g_last_el->next_gel = curr_g_el;
   }
else
   mem->el_glist = curr_g_el;

mem->g_last_el = curr_g_el;

return;
}

/******************************************************************************/

void text_dl ( int display, int memid, char text[], int x0, int y0, int path,
               int orient, int color, int size )

/*
*+
*  Name:
*     text_dl
*
*  Purpose:
*     Text : display list management
*
*  Invocation:
*     text_dl( display, memid, text, x0, y0, path, orient, color, size )
*
*  Description:
*     Text : display list management
*
*  Arguments:
*     display = int
*     memid = int
*     text = char[]
*     x0 = int
*     y0 = int
*     path = int
*     orient = int
*     color = int
*     size = int
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     23-MAY-1991 (NE):
*        Removed definition of malloc
*/

{

/* Local Variables */
int                  curconf;

CONF_DATA            *conf;
MEM_DATA             *mem;

T_LIST   *curr_t_el , *t_last_el;


curconf = device[display].confid;
conf = device[display].config[curconf];
mem = conf->memory[memid];

curr_t_el = (struct t_list *) malloc (sizeof(struct t_list));

mem->n_tel += 1;

curr_t_el->teln = mem->n_tel - 1;
curr_t_el->x0 = x0;
curr_t_el->y0 = y0;
curr_t_el->path = path;
curr_t_el->orient = orient;
curr_t_el->color = color;
curr_t_el->size = size;

strcpy (curr_t_el->text , text);

if (mem->n_tel > 1)
   {
   t_last_el = mem->t_last_el;
   t_last_el->next_tel = curr_t_el;
   }
else
   mem->el_tlist = curr_t_el;

mem->t_last_el = curr_t_el;

return;
}

