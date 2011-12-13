/*
*+
*  Name:
*     IIZ.C
*
*  Purpose:
*     Environment Service Routines
*
*  Description:
*     Environment Service Routines
*
*  Contents:
*     IIZWSC_C
*        Write Memory Scroll;
*     IIZWZM_C
*        Write Memory Zoom;
*     IIZRSZ_C
*        Read  Memory Scroll and Zoom;
*     IIZWZP_C
*        Write Display Zoom and Pan;
*     IIZRZP_C
*        Read  Display Zoom and Pan;
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

/* Package definitions */

# include    "device.dep"
# include    "kwm.h"
# include    "idi.h"
# include    "idi_err.h"
# include    "idistruct_e.h"
# include    "idifuncs.h"


/******************************************************************************/

int IIZWSC_C ( int display, int memlist[], int nmem, int xscr, int yscr )

/*
*+
*  Name:
*     IIZWSC_C
*
*  Purpose:
*     Write memory scroll
*
*  Invocation:
*     status = IIZWSC_C( display, memlist, nmem, xscr, yscr )
*
*  Description:
*     Write memory scroll
*
*  Arguments:
*     display = int
*        Display identifier
*     memlist = int
*        Memory list
*     nmem = int
*        Number of memories
*     xscr = int
*        X scroll
*     yscr = int
*        Y scroll
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     RS:
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*      1-FEB-1990 (RS):
*        Store scroll factors for zoom update
*/

{

/* Local Variables */
int    i , ir , curconf , curmem;
int    iizerr;

CONF_DATA   *conf;
MEM_DATA    *mem = NULL;

iizerr = II_SUCCESS;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iizerr = DEVNOTOP;
   return(iizerr);
   }

curconf = device[display].confid;
conf = device [display].config[curconf];
ir = 0;
for (i = 0; i < nmem; i++)
   {
   curmem = memlist[i];
/* check memory id */

   if ((curmem >= 0) && (curmem < conf->n_mem))
      {
/* store scroll factors */
      mem = conf->memory[curmem];
      if (mem->visibility == 1)
         ir = 1;
      mem->x_scroll = xscr;
      mem->y_scroll = yscr;
      }
   }

/* Store scroll factors for zoom update */
   mem->zoom_xsc_new = xscr;
   mem->zoom_ysc_new = yscr;


/* refresh display if visibility == on */
if (ir == 1)
   refr  (display , &iizerr);

return(iizerr);
}

/******************************************************************************/

int IIZWZM_C ( int display, int memlist[] , int nmem, int zoomf )

/*
*+
*  Name:
*     IIZWZM_C
*
*  Purpose:
*     Write memory zoom
*
*  Invocation:
*     status = IIZWZM_C( display, memlist, nmem, zoomf )
*
*  Description:
*     Write memory zoom
*
*  Arguments:
*     display = int
*        Display identifier
*     memlist = int
*        Memory list
*     nmem = int
*        Number of memories
*     zoomf = int
*        Zoom factor
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     RS:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*      2-FEB-1990 (RS):
*        Zoom about centre of ROI
*      7-FEB-1990 (RS):
*        Update memory scroll
*     21-FEB-1991 (NE):
*        Use local variable for zoom factor and zoom about centre of display.
*        Removed redundant code.
*     20-MAY-1992 (NE):
*        Correct memory id passed to dis_mem_conv
*/

{

/* Local Variables */
int    i , k , curconf , curmem, outmem;
int    xc , yc , xc_mem , yc_mem , xc_dev , yc_dev;
int    xc_mem_min , yc_mem_min, xc_mem_max, yc_mem_max;
int    xc_dev_min , yc_dev_min, xc_dev_max, yc_dev_max;
int    zoom;
int    iizerr;

CONF_DATA   *conf;
MEM_DATA    *mem;
CURS_DATA   *curs;
ROI_DATA    *roi;

xc = 0; yc = 0;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iizerr = DEVNOTOP;
   return(iizerr);
   }

iizerr = II_SUCCESS;

/* check zoom factor */
/* Use local variable for zoom factor */
zoom = zoomf;
if (zoom < device[display].zoom_min)
   zoom = device[display].zoom_min;
if (zoom > device[display].zoom_max)
   zoom = device[display].zoom_max;

curconf = device[display].confid;
conf = device [display].config[curconf];
for (i = 0; i < nmem; i++)
   {
   curmem = memlist[i];

/* check memory id */
   if ((curmem >= 0) && (curmem < conf->n_mem))
      {
      mem = conf->memory[curmem];

/* compute zooming centre for memory */
/* Zoom about centre of display */
      xc_dev = device[display].dev_xsiz / 2;
      yc_dev = device[display].dev_ysiz / 2;

      dis_mem_conv( display, curmem, xc_dev, yc_dev, &xc_mem, &yc_mem );

      xc = xc_dev - xc_mem * zoom;
      yc = yc_dev - yc_mem * zoom;

      if (zoom == 1)
         {
         xc = (int)mem->x_scroll;
         yc = (int)mem->y_scroll;
         }


/* compute zooming centre for ROI (if visible) */
      for (k = 0; k < device[display].n_roi; k++)
         {
         roi = device[display].roi[k];
         if (roi->vis == 1)
            {
            iizerr = IIRRRI_C (display, -1, k, &xc_dev_min, &yc_dev_min,
                               &xc_dev_max, &yc_dev_max, &outmem);
            iizerr = IIRRRI_C (display, curmem, k,
                               &xc_mem_min, &yc_mem_min,
                               &xc_mem_max, &yc_mem_max, &outmem);

/* Zoom about centre of ROI */
              xc = (xc_dev_max - xc_dev_min)/2 + xc_dev_min -
                    ( (xc_mem_max - xc_mem_min)/2 + xc_mem_min) * zoom;
              yc = (yc_dev_max - yc_dev_min)/2 + yc_dev_min -
                    ( (yc_mem_max - yc_mem_min)/2 + yc_mem_min) * zoom;

/* Update memory scroll */
              mem->x_scroll = (xc_dev_max - xc_dev_min)/2 + xc_dev_min -
                    ( (xc_mem_max - xc_mem_min)/2 + xc_mem_min) ;
              mem->y_scroll = (yc_dev_max - yc_dev_min)/2 + yc_dev_min -
                    ( (yc_mem_max - yc_mem_min)/2 + yc_mem_min) ;


            break;
            }
         }

/* compute zooming centre for cursor (if visible) */

      for (k = 0; k < device[display].n_curs; k++)
         {
         curs = device[display].cursor[k];
         if (curs->vis == 1)
            {
            iizerr = IICRCP_C (display, -1, k, &xc_dev, &yc_dev, &outmem);
            iizerr = IICRCP_C (display, curmem, k, &xc_mem, &yc_mem, &outmem);


            xc = xc_dev - xc_mem * zoom;
            yc = yc_dev - yc_mem * zoom;

/* Update memory scroll */
            mem->x_scroll = xc_dev - xc_mem ;
            mem->y_scroll = yc_dev - yc_mem ;

            break;
            }
         }
      }
   }


for (i = 0; i < nmem; i++)
   {
   curmem = memlist[i];
   mem = conf->memory[curmem];
   mem->zoom_xsc_new = xc;
   mem->zoom_ysc_new = yc;
   mem->zoom_new = zoom;
   }

refr  (display , &iizerr);

return(iizerr);
}

/******************************************************************************/

int IIZRSZ_C ( int display, int memid, int* xscr, int* yscr, int* zoom )

/*
*+
*  Name:
*     IIZRSZ_C
*
*  Purpose:
*     Read memory scroll and zoom
*
*  Invocation:
*     status = IIZRSZ_C( display, memid, xscr, yscr, zoom )
*
*  Description:
*     Read memory scroll and zoom
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     xscr = int
*        X scroll
*     yscr = int
*        Y scroll
*     zoom = int
*        Zoom factor
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     28-NOV-1991 (NE):
*        Correct scroll factors when zoomed
*/

{

/* Local Variables */
int         curconf;
int         iizerr;

CONF_DATA   *conf;
MEM_DATA    *mem;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iizerr = DEVNOTOP;
   return(iizerr);
   }

curconf = device[display].confid;
conf = device [display].config[curconf];

/* check memory id */

if ((memid < 0) || (memid >= conf->n_mem))
      {
      iizerr = ILLMEMID;
      return(iizerr);
      }
mem = conf->memory[memid];

/* read scroll & zoom parameters */

if ( ( mem->zoom == 1 ) || ( mem->zoom == -1 ) )
   {
   *xscr = (int)mem->x_scroll;
   *yscr = (int)mem->y_scroll;
   }
else
   {
   *xscr = (int)mem->zoom_xsc / mem->zoom;
   *yscr = (int)mem->zoom_ysc / mem->zoom;
   }
*zoom = mem->zoom;

iizerr = II_SUCCESS;

return(iizerr);
}

/******************************************************************************/

int IIZWZP_C ( int display, int xscr, int yscr, int zoomf )

/*
*+
*  Name:
*     IIZWZP_C
*
*  Purpose:
*     Write display zoom and pan
*
*  Invocation:
*     status = IIZWZP_C( display, xscr, yscr, zoomf )
*
*  Description:
*     Write display zoom and pan
*
*  Arguments:
*     display = int
*        Display identifier
*     xscr = int
*        X scroll
*     yscr = int
*        Y scroll
*     zoomf = int
*        Zoom factor
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Use local variable for zoom factor
*/

{

/* Local Variables */
int zoom;
int iizerr;


/* check if display has been opened */

if (device[display].opened == 0)
   {
   iizerr = DEVNOTOP;
   return(iizerr);
   }

iizerr = II_SUCCESS;

/* Check zoom factor using local vaiable */
zoom = zoomf;
if (zoom < device[display].zoom_min)
   zoom = device[display].zoom_min;
if (zoom > device[display].zoom_max)
   zoom = device[display].zoom_max;

/* store display scroll & zoom factors */

device[display].x_scroll = xscr;
device[display].y_scroll = yscr;
device[display].zoom = zoom;

refr  (display , &iizerr);

return(iizerr);
}

/******************************************************************************/

int IIZRZP_C ( int display, int* xscr, int* yscr, int* zoom )

/*
*+
*  Name:
*     IIZRZP_C
*
*  Purpose:
*     Read display zoom and pan
*
*  Invocation:
*     status = IIZRZP_C( display, xscr, yscr, zoom )
*
*  Description:
*     Read display zoom and pan
*
*  Arguments:
*     display = int
*        Display identifier
*     xscr = int
*        X scroll
*     yscr = int
*        Y scroll
*     zoomf = int
*        Zoom factor
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
int iizerr;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iizerr = DEVNOTOP;
   return(iizerr);
   }

/* read display scroll & zoom factors */

*xscr = (int)device[display].x_scroll;
*yscr = (int)device[display].y_scroll;
*zoom = device[display].zoom;

iizerr = II_SUCCESS;

return(iizerr);
}

