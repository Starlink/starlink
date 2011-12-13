/*
*+
*  Name:
*     IIR.C
*
*  Purpose:
*     Region of Interest Routines
*
*  Description:
*     Region of Interest Routines
*     All roi coords are relative to the associated memory
*     (or to the device in case of memory = -1)
*     (0,0 = bottom left)
*
*  Contents:
*     IIRINR_C
*        Initialize Region of Interest;
*     IIRSRV_C
*        Set Region of Interest Visibility;
*     IIRRRI_C
*        Read Region of Interest Position;
*     IIRWRI_C
*        Write Region of Interest Position;
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
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     16-JUL-1991 (NE):
*        Added iii.h for roi_last_active
*/

/* System definitions */

# include    <stdio.h>

/* Package definitions */

# include    "device.dep"
# include    "kwm.h"
# include    "idi.h"
# include    "iii.h"
# include    "idi_err.h"
# include    "idistruct_e.h"
# include    "idifuncs.h"

/******************************************************************************/

int IIRINR_C ( int display, int memid, int roicol, int roixmin, int roiymin,
               int roixmax, int roiymax, int *roiid )

/*
*+
*  Name:
*     IIRINR_C
*
*  Purpose:
*     Initialize region of interest
*
*  Invocation:
*     status = IIRINR_C ( display, memid, roicol, roixmin, roiymin, roixmax,
*                         roiymax, roiid )
*
*  Description:
*     Initialize region of interest
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     roicol = int
*        Region of interest colour
*     roixmin = int
*        X coordinate of first corner
*     roiymin = int
*        Y coordinate of first corner
*     roixmax = int
*        X coordinate of opposite corner
*     roiymax = int
*        Y coordinate of opposite corner
*     roiid = int
*        Region of interest identifier
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     26-NOV-1991 (NE):
*        Initialise roi->corner
*     27-MAY-1992 (NE):
*        Do not update roi->memid as it causes problems otherwise
*/

{

/* Local Variables */
int curconf, xmax, ymax;
int iirerr;

CONF_DATA   *conf;
MEM_DATA    *mem;
ROI_DATA    *roi;

if (device[display].opened == 0)
   {
   iirerr = DEVNOTOP;
   return(iirerr);
   }

iirerr = II_SUCCESS;

curconf = device[display].confid;
conf = device[display].config[curconf];

/* check memory id */

if ((memid < -1) || (memid >= conf->n_mem))
   {
   iirerr = ILLMEMID;
   return(iirerr);
   }

if (memid > 0)
   {
   mem = conf->memory[memid];

/* check if memory has been allocated */

   if (mem->mmbm == 0)
      {
      iirerr = ILLMEMID;
      return(iirerr);
      }
   }

*roiid = 0;
roi = device[display].roi[*roiid];
while (roi->sh != -1)
   {
   *roiid += 1;

/* check ROI id range */

   if (*roiid >= device[display].n_roi)
      {
      iirerr = MAXROI;
      return(iirerr);
      }
/* OK */

   roi = device[display].roi[*roiid];
   }

roi = device[display].roi[*roiid];

/* store ROI parameters */

/*roi->memid   = memid;*/
roi->col     = roicol;
roi->sh      = II_RECTANGLE;
roi->corner  = 0;

/* write ROI */

if (memid == -1)
   {
   xmax = device[display].dev_xsiz;
   ymax = device[display].dev_ysiz;
   }
else
   {
   mem = conf->memory[memid];
   xmax = mem->x_size;
   ymax = mem->y_size;
   }

if ((roixmin >= 0) && (roixmin < xmax) &&
    (roiymin >= 0) && (roiymin < ymax))
   iirerr = IIRWRI_C (display, memid, *roiid, roixmin, roiymin, roixmax,
             roiymax);


return(iirerr);
}

/******************************************************************************/

int IIRSRV_C ( int display, int roiid, int vis )

/*
*+
*  Name:
*     IIRSRV_C ( display, roiid, vis )
*
*  Purpose:
*     Set region of interest visibility
*
*  Invocation:
*     status = IIRSRV_C ( display, roiid, vis )
*
*  Description:
*     Set region of interest visibility
*
*  Arguments:
*     display = int
*        Display identifier
*     roiid = int
*        Region of interest identifier
*     vis = int
*        Visibility [1 / 0]
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
*        Draw rectangle with active corner.
*     16-JUL-1991 (NE):
*        Pass active corner to roi_rectangle rather than ROI id.
*/

{

/* Local Variables */
int    corner;
int    x0 , y0 , x1 , y1;
int    lvis;
int    iirerr;

ROI_DATA  *roi;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iirerr = DEVNOTOP;
   return(iirerr);
   }

/* Ensure that the logical flag has values 0 or 1 */
if ( vis == 0 )
   lvis = 0;
else
   lvis = 1;

roi = device[display].roi[roiid];

/* visibility already set : return */

if (roi->vis == lvis)
   {
   iirerr = II_SUCCESS;
   return(iirerr);
   }

/* convert coordinates from associated memory to display */

mem_dis_conv (display , roi->memid , roi->x_min , roi->y_min ,
              &x0 , &y0);
mem_dis_conv (display , roi->memid , roi->x_max , roi->y_max ,
              &x1 , &y1);

/* Draw ROI with active corner, but use the old active corner if erasing */
if ( roi->vis == 1 )
   corner = roi_last_active;
else
   corner = roi->corner;
roi_last_active = roi->corner;
roi_rectangle (display, corner, x0, y0, x1, y1, roi->col);

roi->vis = lvis;

iirerr = II_SUCCESS;
return(iirerr);
}

/******************************************************************************/

int IIRRRI_C ( int display, int inmemid, int roiid, int* roixmin,
               int* roiymin,int*  roixmax, int* roiymax, int* outmemid )

/*
*+
*  Name:
*     IIRRRI_C
*
*  Purpose:
*     Read region of interest position
*
*  Invocation:
*     status = IIRRRI_C( display, inmemid, roiid, roixmin, roiymin, roixmax,
*                        roiymax, outmemid )
*
*  Description:
*     Read region of interest position
*
*  Arguments:
*     display = int
*        Display identifier
*     inmemid = int
*        Input memory identifier
*     roiid = int
*        Region of interest identifier
*     roixmin = int
*        X coordinate of first corner
*     roiymin = int
*        Y coordinate of first corner
*     roixmax = int
*        X coordinate of opposite corner
*     roiymax = int
*        Y coordinate of opposite corner
*     outmemid = int
*        Output memory identifier
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
*        Remove check if input memory has been allocated
*     21-FEB-1991 (NE):
*        Removed redundant code
*      2-AUG-1991 (NE):
*        Removed display zoom factors
*/

{

/* Local Variables */
int   i, curconf, roixc, roiyc, xmin, xmax, ymin, ymax;
int   iirerr;

CONF_DATA   *conf;
MEM_DATA    *mem;
ROI_DATA    *roi;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iirerr = DEVNOTOP;
   return(iirerr);
   }

curconf = device[display].confid;
conf = device[display].config[curconf];

/* check memory id */

if ((inmemid < -1) || (inmemid >= conf->n_mem))
   {
   iirerr = ILLMEMID;
   return(iirerr);
   }

roi = device[display].roi[roiid];

if (inmemid == -1)
   {
/* ROI is related to display */

   mem_dis_conv (display, roi->memid, roi->x_min, roi->y_min,
                 &xmin, &ymin);
   mem_dis_conv (display, roi->memid, roi->x_max, roi->y_max,
                 &xmax, &ymax);

   *roixmin = xmin;
   *roiymin = ymin;
   *roixmax = xmax;
   *roiymax = ymax;

   roixc = (*roixmax - *roixmin) / 2;
   roiyc = (*roiymax - *roiymin) / 2;

/* search for associated memory */

   for (i = 0; i < conf->n_mem; i++)
      {
      mem = conf->memory[i];
      if ((mem->mem_free != -1) && (mem->visibility == 1) &&
          (roixc >= (int)mem->x_scroll / mem->zoom) &&
          (roiyc >= (int)mem->y_scroll / mem->zoom) &&
          (roixc <= ((int)mem->x_scroll + mem->x_v_size) / mem->zoom) &&
          (roiyc <= ((int)mem->y_scroll + mem->y_v_size) / mem->zoom))
             {
             *outmemid = i;
             break;
             }
      }
   }
else
   {
/* ROI is related to a memory */
   mem= conf->memory[inmemid];
/* check if memory has been allocated */
   if (mem->mem_free == -1)
      {
      iirerr = ILLMEMID;
      return (iirerr);
      }
   mem_dis_conv (display, roi->memid, roi->x_min, roi->y_min,
                 &xmin, &ymin);
   mem_dis_conv (display, roi->memid, roi->x_max, roi->y_max,
                 &xmax, &ymax);
   dis_mem_conv (display, inmemid, xmin, ymin, roixmin, roiymin);
   dis_mem_conv (display, inmemid, xmax, ymax, roixmax, roiymax);
   *outmemid = inmemid;
   }

iirerr = II_SUCCESS;
return(iirerr);
}

/******************************************************************************/

int IIRWRI_C ( int display, int memid, int roiid, int roixmin, int roiymin,
               int roixmax, int roiymax )

/*
*+
*  Name:
*     IIRWRI_C
*
*  Purpose:
*     Write region of interest position
*
*  Invocation:
*     status = IIRWRI_C( display, memid, roiid, roixmin, roiymin, roixmax,
*                        roiymax )
*
*  Description:
*     Write region of interest position
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     roiid = int
*        Region of interest identifier
*     roixmin = int
*        X coordinate of first corner
*     roiymin = int
*        Y coordinate of first corner
*     roixmax = int
*        X coordinate of opposite corner
*     roiymax = int
*        Y coordinate of opposite corner
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*      2-AUG-1991 (NE):
*        Removed display zoom factors
*/

{

/* Local Variables */
int curconf , vis , vis0;
int roixmind , roiymind , roixmaxd , roiymaxd;
int iirerr;

CONF_DATA   *conf;
MEM_DATA    *mem;
ROI_DATA    *roi;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iirerr = DEVNOTOP;
   return(iirerr);
   }

curconf = device[display].confid;
conf = device [display].config[curconf];

/* check memory id */

if ((memid < -1) || (memid >= conf->n_mem))
   {
   iirerr = ILLMEMID;
   return(iirerr);
   }

roi = device[display].roi[roiid];

/* switch off ROI */

vis = roi->vis ;
if (vis == 1)
   {
   vis0 = 0;
   iirerr = IIRSRV_C (display , roiid , vis0);
   }

/* get display coordinates */

if (memid == -1)
   {
   roixmind = roixmin;
   roiymind = roiymin;
   roixmaxd = roixmax;
   roiymaxd = roiymax;
   }
else
   {
   mem_dis_conv (display , memid , roixmin , roiymin ,
                 &roixmind , &roiymind);
   mem_dis_conv (display , memid , roixmax , roiymax ,
                 &roixmaxd , &roiymaxd);
   }

/* store ROI coordinates (with conversion to associated memory) */

if (roi->memid == -1)
   {
   roi->x_min = roixmind;
   roi->y_min = roiymind;
   roi->x_max = roixmaxd;
   roi->y_max = roiymaxd;
   }
else
   {
   mem = conf->memory[roi->memid];

/* check if memory has been allocated */

   if (mem->mmbm == 0)
      {
      iirerr = ILLMEMID;
      return(iirerr);
      }
   dis_mem_conv (display , roi->memid , roixmind , roiymind ,
                 &roi->x_min , &roi->y_min);
   dis_mem_conv (display , roi->memid , roixmaxd , roiymaxd ,
                 &roi->x_max , &roi->y_max);
   }

/* switch on ROI */

if (vis == 1)
   {
   vis0 = 1;
   iirerr = IIRSRV_C (display , roiid , vis0);
   }

iirerr = II_SUCCESS;
return(iirerr);
}

