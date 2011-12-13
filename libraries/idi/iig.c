/*
*+
*  Name:
*     IIG.C
*
*  Purpose:
*     Graphics Routines
*
*  Description:
*     Graphics Routines
*
*  Contents:
*     IIGPLY_C
*        Polyline;
*     IIGTXT_C
*        Plot Text;
*
*  Copyright:
*     Copyright (C) 1988, 1991, 1992, 1993 Science & Engineering Research Council.
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

int IIGPLY_C ( int display, int memid, int x[], int y[], int np, int color,
               int style )

/*
*+
*  Name:
*     IIGPLY_C
*
*  Purpose:
*     Polyline
*
*  Invocation:
*     status = IIGPLY_C( display, memid, x, y, np, color, style )
*
*  Description:
*     Polyline
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     x = int[]
*        X position list
*     y = int[]
*        Y position list
*     np = int
*        Number of points
*     color = int
*        Pixel colour value
*     style = int
*        Line style
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Plot polyline in pixmap rather than window
*      1-OCT-1991 (NE):
*        Use memory coordinates for the pixmap
*     24-SEP-1992 (NE):
*        Do not refresh pixmap.
*        Allow for display zoom and scrolls.
*     19-NOV-1992 (NE):
*        Set mem->mem_free before plotting on pixmap
*      6-JAN-1993 (NE):
*        Remove special treatment for unzoomed case
*/

{

/* Local Variables */
int i, curconf, xs0, ys0;
int xs[8192], ys[8192];
int iigerr;
CONF_DATA *conf;
MEM_DATA *mem = NULL;

/* check if dispaly has been opened */

if (device[display].opened == 0)
   {
   iigerr = DEVNOTOP;
   return(iigerr);
   }

curconf = device[display].confid;
conf = device[display].config[curconf];

/* check memory id */

if ((memid < -1) || (memid >= conf->n_mem))
   {
   iigerr = ILLMEMID;
   return(iigerr);
   }

/* select memory */
if ( memid >= 0 ) mem = conf->memory[memid];

if ((memid >=0) && ((mem->type & II_IMAGE) > 0) &&
    (mem->mmbm == 0))
   mmbm_all (display, curconf, memid, &iigerr);

if (memid != -1)
   {
/* associated memory : convert coordinates and */
/*                     update graphic display list */

   for (i = 0; i < np; i++)
      {
      mem_screen_conv( display, memid, x[i], y[i], &xs0, &ys0 );
      xs[i] = xs0;
      ys[i] = ys0;
      }

   mem->mem_free = 0;
   if (mem->visibility == 1)

/* Plot polyline in pixmap */
      {
      polyline_p( display, memid, color, style, xs, ys, np, 1 );
      }

   polyline_dl (display, memid, color, style, x, y, np);
   }
else
   {
/* draw polyline only to display */
/* convert the coordinates into memory coords */
   for (i = 0; i < np; i++)
      {
      dis_mem_conv( display, device[display].pm_mem, x[i], y[i], &xs0, &ys0 );
      xs[i] = xs0;
      ys[i] = ys0;
      }

/* Plot polyline in pixmap */
   polyline_p( display, memid, color, style, xs, ys, np, 1 );
   }

iigerr = II_SUCCESS;
return(iigerr);
}

/******************************************************************************/

int IIGTXT_C ( int display, int memid, char txt[], int x0, int y0, int path,
               int orient, int color, int txtsize )

/*
*+
*  Name:
*     IIGTXT_C
*
*  Purpose:
*     Plot text
*
*  Invocation:
*     status = IIGTXT_C ( display, memid, txt, x0, y0, path, orient,
*                         color, txtsize )
*
*  Description:
*     Plot text
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     txt = char[]
*        Text string
*     x0 = int[]
*        X position
*     y0 = int[]
*        Y position
*     path = int
*        Text path
*     orient = int
*        Text orientation
*     color = int
*        Pixel colour value
*     txtsize = int
*        Text size
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Plot text in pixmap rather than window
*     27-JUN-1991 (NE):
*        Change argument list in text_p
*      1-OCT-1991 (NE):
*        Use memory coordinates for the pixmap
*     24-SEP-1992 (NE):
*        Do not refresh pixmap.
*        Allow for display zoom and scrolls.
*     19-NOV-1992 (NE):
*        Set mem->mem_free before plotting on pixmap
*      6-JAN-1993 (NE):
*        Remove special treatment for unzoomed case
*/

{

/* Local Variables */
int curconf, xs0, ys0;
int iigerr;
CONF_DATA *conf;
MEM_DATA *mem = NULL;

/* check if dispaly has been opened */

if (device[display].opened == 0)
   {
   iigerr = DEVNOTOP;
   return(iigerr);
   }

curconf = device[display].confid;
conf = device[display].config[curconf];

/* check memory id */

if ((memid < -1) || (memid >= conf->n_mem))
   {
   iigerr = ILLMEMID;
   return(iigerr);
   }

/* select memory */
if ( memid >= 0 ) mem = conf->memory[memid];

if ((memid >=0) && ((mem->type & II_IMAGE) > 0) &&
    (mem->mmbm == 0))
   mmbm_all (display, curconf, memid, &iigerr);

if (memid != -1)
   {
/* associated memory : convert coordinates and */
/*                     update text display list */
   mem_screen_conv( display, memid, x0, y0, &xs0, &ys0 );

   mem->mem_free = 0;
   if (mem->visibility == 1)

/* Plot text in pixmap rather than window */
      {
      text_p( display, memid, xs0, ys0, path, orient, color,
              txtsize, txt, 1 );
      }

   text_dl (display, memid, txt, xs0, ys0, path, orient, color,
           txtsize);
   }
else
   {
/* draw text only on display */
/* convert the coordinates into memory coords */
   dis_mem_conv( display, device[display].pm_mem, x0, y0, &xs0, &ys0 );

/* Plot text in pixmap rather than window */
   text_p( display, device[display].pm_mem, xs0, ys0, path, orient,
           color, txtsize, txt, 1 );
   }

iigerr = II_SUCCESS;
return(iigerr);
}

