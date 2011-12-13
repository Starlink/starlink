/*
*+
*  Name:
*     IIM.C
*
*  Purpose:
*     Memory Routines
*
*  Description:
*     Memory Routines
*
*  Contents:
*     IIMSTW_C
*        Set Transfer Window;
*     IIMWMY_C
*        Image Memory Write;
*     IIMRMY_C
*        Image Memory Read;
*     IIMSMV_C
*        Set Memory Visibility;
*     IIMCMY_C
*        Clear memory;
*     IIMSLT_C
*        Select Video Lookup Table;
*     IIMBLM_C
*        Blink Memories;
*     IIMEBM_C
*        External Bitmap;
*
*  Copyright:
*     Copyright (C) 1988, 1990, 1991, 1992 Science & Engineering Research Council.
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
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*/

/* System definitions */

# include    <stdio.h>
# include    <math.h>

/* Package definitions */

# include    "device.dep"
# include    "kwm.h"
# include    "idi.h"
# include    "idi_err.h"
# include    "idistruct_e.h"
# include    "idifuncs.h"

/* Local definitions */
static  int      ival[4] , arg[ARGSIZE];

/******************************************************************************/

int IIMSTW_C ( int display, int memid, int loaddir, int xwdim, int ywdim,
               int depth, int xwoff, int ywoff )

/*
*+
*  Name:
*     IIMSTW_C
*
*  Purpose:
*     Set image memory transfer window
*
*  Invocation:
*     status = IIMSTW_C( display, memid, loaddir, xwdim, ywdim, depth,
*                        xwoff, ywoff )
*
*  Description:
*     Set image memory transfer window
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     loaddir = int
*        Load direction
*     xwdim = int
*        X memory size
*     ywdim = int
*        Y memory size
*     depth = int
*        Data depth (bits/pixel)
*     xwoff = int
*        X memory offset
*     ywoff = int
*        Y memory offset
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     27-MAY-1992 (NE):
*        Return error if transfer window limits exceed memory limits
*/

{

/* Local Variables */
int   curconf;
int   iimerr;

CONF_DATA  *conf;
MEM_DATA   *mem;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iimerr = DEVNOTOP;
   return(iimerr);
   }

iimerr = II_SUCCESS;

curconf = device[display].confid;
conf = device [display].config[curconf];

/* check memory id */

if ((memid < 0) || (memid >= conf->n_mem))
   {
   iimerr = ILLMEMID;
   return(iimerr);
   }

mem = conf->memory[memid];

/* check for transfer window size against memory size */
/* lower bounds too small */
if ((xwoff < 0) || (ywoff < 0))
   {
   iimerr = TWTOOBIG;
   return(iimerr);
   }

/* or upper bounds too big */
if ((xwoff + xwdim > mem->x_size) || (ywoff + ywdim > mem->y_size))
   {
   iimerr = TWTOOBIG;
   return(iimerr);
   }

/* check data depth against memory depth */

if (depth > mem->depth)
   iimerr = DEPTHERR;

/* store transfer window parameters */

mem->x_wdim = xwdim;
mem->y_wdim = ywdim;
mem->x_woff = xwoff;
mem->y_woff = ywoff;
mem->load_dir = loaddir;

return(iimerr);
}

/******************************************************************************/

int IIMWMY_C ( int display, int memid, int data[], int npixel, int depth,
               int packf, int x0, int y0 )

/*
*+
*  Name:
*     IIMWMY_C
*
*  Purpose:
*     Writes image memory
*
*  Invocation:
*     status = IIMWMY_C ( display, memid, data, npixel, depth, packf, x0, y0 )
*
*  Description:
*     Writes image memory
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     data = int[]
*        Image or graphic data, single array -npixel- long
*     npixel = int
*        Number of data points
*     depth = int
*        Data depth (bits/pixel)
*     packf = int
*        Packing factor ( pixel/int >> 1..32 )
*     x0 = int
*        X data origin
*     y0 = int
*        Y data origin
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Removed redundant code
*/

{

/* Local Variables */
int    iy , nb , bl , dd , curconf;
int    nmem , vis , memlist[1];
int    iimerr;

CONF_DATA  *conf;
MEM_DATA   *mem;
ITT_DATA   *itt;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iimerr = DEVNOTOP;
   return(iimerr);
   }

curconf = device[display].confid;
conf = device [display].config[curconf];

/* check memory id */

if ((memid < 0) || (memid >= conf->n_mem))
   {
   iimerr = ILLMEMID;
   return(iimerr);
   }

iimerr = II_SUCCESS;

mem = conf->memory[memid];

/* --- Allocate main memory bitmap ----- */

if (mem->mmbm == 0)
   mmbm_all (display , curconf , memid , &iimerr);

/* --- Write main memory bitmap ----- */


/* Test if External Bitmap is defined */
/* ---------------------------------- */

/* IDI extension : if the npixel argument is set to zero */
/*                 the data for iimwmy interface will be */
/*                 taken from an external bitmap         */

if (npixel == 0)
   {
/* external bitmap write memory routine */

   ebwmy (display , curconf , memid , x0 , y0 , &iimerr);
   return(iimerr);
   }

/* else continue with standard format data */
/* --------------------------------------- */

iy = npixel / mem->x_wdim;         /* no. of rows to write */

/* check input image dimension */

if (y0 +iy > mem->y_size)
   {
   iimerr = IMGTOOBIG;
   return(iimerr);
   }

/* check data depth against memory depth */

if (depth > mem->depth)
   {
   iimerr = DEPTHERR;
   dd = depth - mem->depth;        /* no. of bits to truncate */
   }
else
   dd = 0;

wr_mem (display , memid , x0 , y0 , packf , iy , dd , depth , npixel , data);

mem->mem_free = 0;


return(iimerr);
}

/******************************************************************************/

int IIMRMY_C ( int display, int memid, int npixel, int x0, int y0, int depth,
               int packf, int ittf, int data[] )

/*
*+
*  Name:
*     IIMRMY_C
*
*  Purpose:
*     Read image memory
*
*  Invocation:
*     status = IIMRMY_C( display, memid, npixel, x0, y0, depth, packf,
*                        ittf, data )
*
*  Description:
*     Read image memory
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     npixel = int
*        Number of data points
*     x0 = int
*        X data origin
*     y0 = int
*        Y data origin
*     depth = int
*        Data depth (bits/pixel)
*     packf = int
*        Packing factor ( pixel/int >> 1..32 )
*     ittf = int
*        ITT flag
*     data = int[]
*        Image or graphic data, single array -npixel- long
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     25-NOV-1991 (NE):
*        Only test if depth is greater than memory depth
*     13-FEB-1992 (NE):
*        Added ittf to argument list of rd_mem
*/

{

/* Local Variables */
int    i , iy , nb , bl , dd , curconf;
int    iimerr;

CONF_DATA  *conf;
MEM_DATA   *mem;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iimerr = DEVNOTOP;
   return(iimerr);
   }

curconf = device[display].confid;
conf = device [display].config[curconf];

/* check memory id */

if ((memid < 0) || (memid >= conf->n_mem))
   {
   iimerr = ILLMEMID;
   return(iimerr);
   }

mem = conf->memory[memid];
iy = npixel / mem->x_wdim;             /* no. of rows to read */
/* if les han 1 row than */
if (iy == 0)
   iy = 1;

/* check image dimension */

if (y0 + iy > mem->y_size)
   {
   iimerr = IMGTOOBIG;
   return(iimerr);
   }

/* check data depth against memory depth */

if (depth > mem->depth)
   {
   iimerr = DEPTHERR;
   dd = depth - mem->depth;          /* no. of bits to truncate */
   }
else
   dd = 0;


rd_mem (display, memid, x0, y0, packf, npixel, iy, dd, depth, ittf, data);

iimerr = II_SUCCESS;

return(iimerr);
}

/******************************************************************************/

int IIMSMV_C ( int display, int memlist[], int nmem, int vis )

/*
*+
*  Name:
*     IIMSMV_C
*
*  Purpose:
*     Set memory visibility
*
*  Invocation:
*     status = IIMSMV_C( display, memlist, nmem, vis )
*
*  Description:
*     Set memory visibility
*
*  Arguments:
*     display = int
*        Display identifier
*     memlist = int
*        Memory list
*     nmem = int
*        Number of memories
*     vis = int
*        Visibility (1/0)
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Ensure that the logical flag has values 0 or 1.
*        Allow for display as well as memory zooms.
*        Refresh image into pixmap rather than window.
*     25-JUL-1991 (NE):
*        Change sign of device scroll when calculating ival[3]
*      7-MAY-1992 (NE):
*        Update graphics and text memories from the input list only.
*     15-OCT-1992 (NE):
*        Use real variables for pixel bounds.
*     15-DEC-1992 (NE):
*        Correct visible memory offsets, x_v_off y_v_off
*/

{

/* Local Variables */
int    i , j , k , jj , kk , curconf , curmem , bck , status;
int    xc0 , xc1 = 0, yc0 , yc1 = 0;
int    dxc , dyc , x_dev , y_dev , x_mem , y_mem;
int    lowx , lowy , upx , upy , x_min , y_min , x_max , y_max;
int    zx , zy , xoff , yoff , xdim , ydim;
int    outmem;
int    iimerr;
int    lvis;
int    zoomf;

float  x0 , x1 , y0 , y1 , xs0 , xs1 , ys0 , ys1;
float  zoom = 0.0;

CONF_DATA  *conf;
MEM_DATA   *mem;
CURS_DATA  *curs;
ROI_DATA   *roi;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iimerr = DEVNOTOP;
   return(iimerr);
   }

/* Ensure that the logical flag has values 0 or 1 */
if ( vis == 0 )
   lvis = 0;
else
   lvis = 1;

curconf = device[display].confid;
conf = device [display].config[curconf];

for (i = 0; i < nmem; i++)
   {
   curmem = memlist[i];
   mem = conf->memory[curmem];

/* check memory id and test if free */

   if ((curmem >= 0) && (curmem < conf->n_mem) && (mem->mem_free == 0))
      {
      mem->visibility = lvis;
      if (lvis == 1)                                 /* Visibility  ON */
         {                                          /* -------------- */

/* Allow for display as well as memory zooms */
         zoomf = mem->zoom_new * device[display].zoom;
         if ((zoomf == 1) || (zoomf == -1))
                                                    /* Zoom = +- 1    */
                                                    /* --------       */

            {                       /* Compute size of visible memory */
            ival[0] = device[display].dev_xsiz;
            ival[1] = mem->x_size;
            ival[2] = ival[1] + mem->x_scroll + device[display].x_scroll;
            ival[3] = ival[0] - mem->x_scroll - device[display].x_scroll;
            mem->x_v_size = idi__min (ival , 4);

            ival[0] = device[display].dev_ysiz;
            ival[1] = mem->y_size;
            ival[2] = ival[1] + mem->y_scroll + device[display].y_scroll;
            ival[3] = ival[0] - mem->y_scroll - device[display].y_scroll;
            mem->y_v_size = idi__min (ival , 4);

/* to next memory if visible size == 0 */

            if ((mem->x_v_size <= 0) || (mem->y_v_size <= 0))
               continue;

                                /* Compute offsets of visible memory  */
                                /* (relative to memory origin)        */

            mem->x_v_off = -(mem->x_scroll + device[display].x_scroll);
            mem->y_v_off = -(mem->y_scroll + device[display].y_scroll);

/* Refresh image into pixmap */
            if ((mem->type & II_IMAGE) > 0)
               imagrefr_p( display, curmem );

            mem->zoom_xsc = mem->zoom_xsc_new;
            mem->zoom_ysc = mem->zoom_ysc_new;
            mem->zoom = mem->zoom_new;
            }
         else
            {                                    /* Zoomed image */
                                                 /* ------------ */
                                /* xc1 , yc1 : centre of zoom    */
                                /*             display coords    */

                                /* xc0 , yc0 : centre of zoom    */
                                /*             memory  coords    */
/* Zoom about centre of display */
            if (zoomf > 0)
               {
               zoom = zoomf;
               xc1 = device[display].dev_xsiz / 2 -
                     ( (int)device[display].x_scroll * zoomf ) / ( zoomf - 1 );
               yc1 = device[display].dev_ysiz / 2 -
                     ( (int)device[display].y_scroll * zoomf ) / ( zoomf - 1 );
               }
            else if (zoomf < 0)
               {
               zoom = - 1. / zoomf;
               xc1 = device[display].dev_xsiz / 2 -
                     (int)device[display].x_scroll / ( zoomf + 1 );
               yc1 = device[display].dev_ysiz / 2 -
                     (int)device[display].y_scroll / ( zoomf + 1 );
               }

            /* convert to memory coordinates */

            dis_mem_conv (display , curmem , xc1 , yc1 , &xc0 , &yc0);

            xc0 *= zoom;
            yc0 *= zoom;

                                /* check for ROI visibility      */
                                /* assume ROI centre as          */
                                /* zoom centre                   */

                        /* ROI has priority over memory position */

            for (k = 0; k < device[display].n_roi; k++)
               {
               if (device[display].roi[k]->vis == 1)
                  {
                  roi = device[display].roi[k];
                  if (device[display].roi[k]->memid == -1)
                     {
                     /* ROI associated to display */

                     dis_mem_conv (display , curmem , roi->x_min ,
                                   roi->y_min , &x_min , &y_min);
                     dis_mem_conv (display , curmem , roi->x_max ,
                                   roi->y_max , &x_max , &y_max);

                     xc0 = (x_min + (x_max - x_min) / 2) * zoom;
                     yc0 = (y_min + (y_max - y_min) / 2) * zoom;

                     xc1 = (roi->x_min + (roi->x_max - roi->x_min) / 2);
                     yc1 = (roi->y_min + (roi->y_max - roi->y_min) / 2);
                     break;
                     }
                  else
                     {
                     /* ROI associated to memory */

                     mem_dis_conv (display , curmem , roi->x_min , roi->y_min ,
                                   &x_min , &y_min);
                     mem_dis_conv (display , curmem , roi->x_max , roi->y_max ,
                                   &x_max , &y_max);

                     xc0 = (roi->x_min + (roi->x_max - roi->x_min) / 2) * zoom;
                     yc0 = (roi->y_min + (roi->y_max - roi->y_min) / 2) * zoom;
                     xc1 = x_min + (x_max - x_min) / 2;
                     yc1 = y_min + (y_max - y_min) / 2;
                     break;
                     }
                  }
               }

                                /* check for cursor visibility   */
                                /* assume cursor position as     */
                                /* zoom centre                   */

              /* cursor has priority over ROI or memory position */

            for (k = 0; k < device[display].n_curs; k++)
               {
               if (device[display].cursor[k]->vis == 1)
                  {
                  curs = device[display].cursor[k];
                  if (device[display].cursor[k]->cur_memid == -1)
                     {
                     /* cursor associated to display */

                     dis_mem_conv (display , curmem , curs->x_pos ,
                                   curs->y_pos , &x_mem , &y_mem);

                     xc0 = x_mem * zoom;
                     yc0 = y_mem * zoom;
                     xc1 = curs->x_pos;
                     yc1 = curs->y_pos;

                     break;
                     }
                  else
                     {
                     /* cursor associated to memory */

                     mem_dis_conv (display, curmem, curs->x_pos,
                                   curs->y_pos , &x_dev , &y_dev);
                     xc0 = curs->x_pos * zoom;
                     yc0 = curs->y_pos * zoom;
                     xc1 = x_dev;
                     yc1 = y_dev;
                     break;
                     }
                  }
               }

            dxc = xc1 - xc0;
            dyc = yc1 - yc0;

                              /* x1 , y1   : image origin     */
                              /* xs1 , ys1 : image dimensions */
                              /*   display coordinates        */

                              /* x0 , y0   : image origin     */
                              /* xs0 , ys0 : image dimensions */
                              /*   memory coordinates         */

            x1 = (dxc > 0) ? dxc : 0;
            y1 = (dyc > 0) ? dyc : 0;


            xs1 = ((mem->x_size * zoom + dxc) < device[display].dev_xsiz) ?
                   (mem->x_size * zoom + dxc - x1) :
                   (device[display].dev_xsiz - x1);
            ys1 = ((mem->y_size * zoom + dyc) < device[display].dev_ysiz) ?
                   (mem->y_size * zoom + dyc - y1) :
                   (device[display].dev_ysiz - y1);

            x0 = (dxc > 0) ? 0 : -dxc;
            y0 = (dyc > 0) ? 0 : -dyc;

                                   /* zoomed image dimensions         */
            xs0 = xs1;
            ys0 = ys1;

                                  /* reset values to original image   */
            xc0 /= zoom;
            yc0 /= zoom;

            x0 /= zoom;
            y0 /= zoom;

            xs0 /= zoom;
            ys0 /= zoom;

            /* store visible memory parameters */

            mem->x_v_off  = -(float)dxc / (float)zoom;
            mem->y_v_off  = -(float)dyc / (float)zoom;
            mem->x_v_size = xs0;
            mem->y_v_size = ys0;

/* set zoomed memory visible */
            if ((mem->type & II_IMAGE) > 0)
               {
               if (zoomf > 0)
                  imagrefr_z_p( display, curmem, zoomf, x0, y0,
                                xs0, ys0, x1, y1, xs1, ys1 );
               else if (zoomf < 0)
                  imagrefr_uz_p( display, curmem, -zoomf, x0, y0,
                                 xs0, ys0, x1, y1, xs1, ys1);

               }
            mem->zoom_xsc = mem->zoom_xsc_new;
            mem->zoom_ysc = mem->zoom_ysc_new;
            mem->zoom = mem->zoom_new;

            }

         }
      }
   }


/* ------------------------------------- */
if (lvis == 0)
/* Refresh Memories , Cursors & Roi */
   refr (display , &iimerr);

else
   {
/* Refresh of Graphic & Text Display List */
   for (i = 0; i < nmem; i++)
      {
      curmem = memlist[i];
      mem = conf->memory[curmem];
      if ( mem->visibility ==1 )
         {
         polyrefr_p( display, curconf, curmem );
         textrefr_p( display, curconf, curmem );
         }
      }

/* Refresh pixmap onto display */
   refr_p( display );

/* Refresh Cursors & Roi */
   grefr (display , &iimerr);
   }

/* ------------------------------------- */

iimerr = II_SUCCESS;
return(iimerr);
}

/******************************************************************************/

int IIMCMY_C ( int display, int memlist[], int nmem, int bck )

/*
*+
*  Name:
*     IIMCMY_C
*
*  Purpose:
*     Clear memory
*
*  Invocation:
*     status = IIMCMY_C( display, memlist, nmem, bck )
*
*  Description:
*     Clear memory
*
*  Arguments:
*     display = int
*        Display identifier
*     memlist = int
*        Memory list
*     nmem = int
*        Number of memories
*     bck = int
*        Background value
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     RS:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     19-MAR-1990 (RS):
*        Added call to cl_bitmap
*     24-SEP-1991 (NE):
*        Added call to clear_pixmap
*     16-APR-1992 (NE):
*        Altered arguments in clear_pixmap
*     11-NOV-1992 (NE):
*        Remove call to clear_pixmap (it is called in refr)
*/

{

/* Local Variables */
int    i , curconf , curmem;
int    iimerr;

CONF_DATA  *conf;
MEM_DATA   *mem;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iimerr = DEVNOTOP;
   return(iimerr);
   }

curconf = device[display].confid;

conf = device [display].config[curconf];
for (i = 0; i < nmem; i++)
   {
   curmem = memlist[i];

   /* check memory id */

   if ((curmem < 0) || (curmem >= conf->n_mem))
      {
      iimerr = ILLMEMID;
      return(iimerr);
      }
   mem = conf->memory[curmem];

   /* check if memory has been allocated */

   /* clear main memory bitmap */

   cl_dl (display, curconf, curmem);
   if ((mem->type & II_IMAGE) > 0)
      {
      if (mem->mmbm > 0)
         cl_bitmap( display, curconf, curmem, bck );
      }
   }

refr (display , &iimerr);          /* Refresh Memories , Cursors & Roi */

iimerr = II_SUCCESS;
return(iimerr);
}

/******************************************************************************/

int IIMSLT_C ( int display, int memid, int lutn, int ittn )

/*
*+
*  Name:
*     IIMSLT_C
*
*  Purpose:
*     Select video lookup table
*
*  Invocation:
*     status = IIMSLT_C( display, memid, lutn, ittn )
*
*  Description:
*     Select video lookup table
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     lutn = int
*        LUT number
*     ittn = int
*        ITT number
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     24-APR-1991 (NE):
*        Return error if current LUT is fixed
*     14-MAY-1992 (NE):
*        Obtain curlut indices from lut->lutpix array
*/

{

/* Local Variables */
int   i , ii , curconf , lutoff , lutlen;
int   iimerr;

CONF_DATA  *conf;
MEM_DATA   *mem;
LUT_DATA   *lut;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iimerr = DEVNOTOP;
   return(iimerr);
   }

iimerr = II_SUCCESS;

curconf = device[display].confid;
conf = device [display].config[curconf];


/* check memory id */

if ((memid < -1) || (memid >= conf->n_mem))
   {
   iimerr = ILLMEMID;
   return(iimerr);
   }

mem = conf->memory[memid];

if ((ittn >= 0) && (ittn < mem->n_itt))
   {
   curconf = device[display].confid;
   conf = device[display].config[curconf];
   mem = conf->memory[memid];
   mem->itt_id = ittn;
   }
/* check ITT number */

else if (ittn != -1)
   iimerr = ITTIDERR;

if (lutn == -1)                    /* current LUT */
   return(iimerr);

if ((lutn < 0) || (lutn >= device[display].n_lut))
   {
   iimerr = LUTIDERR;
   return(iimerr);
   }

lut = device[display].lookup[lutn];

/* check if LUT has been defined */

if (lut->lut_free == 1)
   {
   iimerr = LUTNOTDEF;
   return(iimerr);
   }

/* check that current LUT can be modified */

if (curlut.nalloc == 0)
   {
   iimerr = LUTNOTFREE;
   return(iimerr);
   }


lutoff = 0;
lutlen = lut->lut_len;
                                      /* Current LUT saved */
curlut.len = lutlen;
curlut.id  = lutn;
for (i = 0; i < lutlen; i++)
   {
   curlut.lutr[lut->lutpix[i]] = lut->lutr[i];
   curlut.lutg[lut->lutpix[i]] = lut->lutg[i];
   curlut.lutb[lut->lutpix[i]] = lut->lutb[i];
   }

wr_lut (display);

iimerr = II_SUCCESS;
return(iimerr);
}

/******************************************************************************/

int IIMBLM_C ( int display, int memlst[], int nmem, float period[] )

/*
*+
*  Name:
*     IIMBLM_C
*
*  Purpose:
*     Blink memories
*
*  Invocation:
*     status = IIMBLM_C( display, memlst, nmem, period )
*
*  Description:
*     Blink memories
*
*  Arguments:
*     display = int
*        Display identifier
*     memlist = int[]
*        Memory list
*     nmem = int
*        Number of memories
*     period = float[]
*        Blink periods
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
int iimerr;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iimerr = DEVNOTOP;
   return(iimerr);
   }

iimerr = II_SUCCESS;
return(iimerr);
}

/******************************************************************************/

int IIMEBM_C ( int display, char bmdscr[], char bmtype, int* xdim, int* ydim )

/*
*+
*  Name:
*     IIMEBM_C
*
*  Purpose:
*     Define external bitmap
*
*  Invocation:
*     status = IIMEBM_C( display, bmdscr, bmtype, xdim, ydim )
*
*  Description:
*     Define external bitmap
*
*  Arguments:
*     display = int
*        Display identifier
*     bmdscr = char[]
*        External bitmap descriptor
*     bmtype = char
*        External bitmap type [ I / O ]
*     xdim = int
*        memory X size
*     ydim = int
*        memory Y size
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
int  extbm, xsize = 0, ysize = 0;
int  iimerr;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iimerr = DEVNOTOP;
   return(iimerr);
   }

/* external bitmap defined for input */

if ((bmtype == 'I') || (bmtype == 'i'))

#if 0
   eb_openi (display, bmdscr , &extbm, &xsize , &ysize);
#endif
                            /* set external bitmap parameters */
                            /* to be used by IIMWMY routine   */
/*device[display].extbm = extbm;*/
device[display].extbm_xsize = xsize;
device[display].extbm_ysize = ysize;

*xdim = xsize;
*ydim = ysize;

iimerr = II_SUCCESS;
return(iimerr);
}

