/*
*+
*  Name:
*     IIC.C
*
*  Purpose:
*     Cursor Routines
*
*  Description:
*     Cursor Routines
*     All cursor coords are relative to the associated memory
*     (or to the device in case of memory = -1)
*     (0,0 = bottom left)
*
*  Contents:
*     IICINC_C
*        Initialize Cursor;
*     IICSCV_C
*        Set Cursor Visibility;
*     IICRCP_C
*        Read Cursor Position;
*     IICWCP_C
*        Write Cursor Position;
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

int IICINC_C ( int display, int memid, int curn, int cursh, int curcol,
               int xcur, int ycur )

/*
*+
*  Name:
*     IICINC_C
*
*  Purpose:
*     Initialize cursor
*
*  Invocation:
*     status = IICINC_C( display, memid, curn, cursh, curcol, xcur, ycur )
*
*  Description:
*     Initialize cursor
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     curn = int
*        Cursor number
*     cursh = int
*        Cursor shape
*     curcol = int
*        Cursor colour
*     xcur = int
*        X cursor position
*     ycur = int
*        Y cursor position
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     27-MAY-1992 (NE):
*        Do not update curs->cur_memid as it causes problems otherwise
*      2-NOV-1992 (NE):
*        Changes for hardware cursor
*/

{

/* Local Variables */
CONF_DATA  *conf;
MEM_DATA   *mem;
CURS_DATA  *curs;

int         curconf , vis0 , col, xmax, ymax;
int         iicerr = IDI__OK;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iicerr = DEVNOTOP;
   return(iicerr);
   }


/* check cursor number */

if ((curn < 0) || (curn >= device[display].n_curs))
   {
   iicerr = ILLCURID;
   return(iicerr);
   }

curconf = device[display].confid;
conf = device [display].config[curconf];

/* check memory id */

if ((memid < -1) || (memid >= conf->n_mem))
   {
   iicerr = ILLMEMID;
   return(iicerr);
   }

curs = device[display].cursor[curn];
if (curs->cur_sh > 0)
   {                                 /* switch off cursor if soft */
   vis0 = 0;
   iicerr = IICSCV_C (display , curn , vis0);
   }

/* set cursor parameters */

curs->cur_sh = cursh;
curs->cur_col = curcol;
curs->vis = 0;
/*curs->cur_memid = memid;*/

/* write cursor */

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

curs->init = 1;

if ((xcur >= 0) && (xcur < xmax) &&
    (ycur >= 0) && (ycur < ymax))
   iicerr = IICWCP_C (display , memid , curn , xcur , ycur);

return(iicerr);
}

/******************************************************************************/

int IICSCV_C ( int display, int curn, int vis )

/*
*+
*  Name:
*     IICSCV_C
*
*  Purpose:
*     Set cursor visibility
*
*  Invocation:
*     status = IICSCV_C( display, curn, vis )
*
*  Description:
*     Set cursor visibility
*
*  Arguments:
*     display = int
*        Display identifier
*     curn = int
*        Cursor number
*     vis = int
*        visibility [1 / 0]
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Ensure the visibility flag has values 0 or 1
*      2-NOV-1992 (NE):
*        Changes for hardware cursor
*/

{

/* Local Variables */
int         xcur, xout, ycur, yout;
int         lvis;
int         iicerr;

CURS_DATA   *curs;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iicerr = DEVNOTOP;
   return(iicerr);
   }

/* Ensure that the logical flag has values 0 or 1 */
if ( vis == 0 )
   lvis = 0;
else
   lvis = 1;

/* check cursor number */

if ((curn < 0) || (curn >= device[display].n_curs))
   {
   iicerr = ILLCURID;
   return(iicerr);
   }

curs = device[display].cursor[curn];
if (curs->cur_sh == -1)             /* shape = -1 : not initialized */
   {
   iicerr = CURNOTDEF;
   return(iicerr);
   }

/* return if already set */

if (curs->vis == lvis)
   {
   iicerr = II_SUCCESS;
   return(iicerr);
   }

/* convert coordinates from associated memory to display */

mem_dis_conv (display , curs->cur_memid , curs->x_pos , curs->y_pos ,
                 &xcur , &ycur);

/* undefine hardware cursor if visibility = 0 */
if ( ( curs->cur_sh == 0 ) && ( lvis == 0 ) )
   hard_cursor( display, HC_UNDO, xcur, ycur, &xout, &yout );
else
{

/* draw cursor on display */

switch (curs->cur_sh)
   {
   case II_CROSSHAIR:

      cross_hair (display , xcur , ycur , curs->cur_col);
      break;

   case II_CROSS:

      cross (display , xcur , ycur , curs->cur_col);
      break;

   default:                       /* default : hardware              */

/* cursor 0 is the hardware cursor */
      hard_cursor( display, HC_WRITE, xcur, ycur, &xout, &yout );
      break;
   }
}

curs->vis = lvis;

iicerr = II_SUCCESS;
return(iicerr);
}

/******************************************************************************/

int IICRCP_C ( int display, int inmemid, int curn, int* xcur, int* ycur,
               int* outmemid )

/*
*+
*  Name:
*     IICRCP_C
*
*  Purpose:
*     Read cursor position
*
*  Invocation:
*     status = IICRCP_C( display, inmemid, curn, xcur, ycur, outmemid )
*
*  Description:
*     Read cursor position
*
*  Arguments:
*     display = int
*        Display identifier
*     inmemid = int
*        Input memory identifier
*     curn = int
*        Cursor number
*     xcur = int
*        X cursor position
*     ycur = int
*        Y cursor position
*     outmemid = int
*        Output memory identifier
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
*        Removed redundant code
*      2-AUG-1991 (NE):
*        Removed display zoom factors
*      2-NOV-1992 (NE):
*        Changes for hardware cursor
*     16-JUN-1993 (DLT):
*	 Correct cursor position when display is zoomed
*/

{

/* Local Variables */
int i , curconf , xc, xcurd, yc, ycurd;
int iicerr;
int xoff, yoff, zoom;

CONF_DATA   *conf;
MEM_DATA    *mem;
CURS_DATA   *curs;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iicerr = DEVNOTOP;
   return(iicerr);
   }

curconf = device[display].confid;
conf = device [display].config[curconf];

/* check memory id */

if ((inmemid < -1) || (inmemid >= conf->n_mem))
   {
   iicerr = ILLMEMID;
   return(iicerr);
   }

/* check cursor number */

if ((curn < 0) || (curn >= device[display].n_curs))
   {
   iicerr = ILLCURID;
   return(iicerr);
   }

curs = device[display].cursor[curn];
if (curs->cur_sh == -1)              /* shape = -1 : not initialized */
   {
   iicerr = CURNOTDEF;
   return(iicerr);
   }

/* read the current hardware cursor position */
if ( curs->cur_sh == 0 )
   {
   hard_cursor( display, HC_READ, 0, 0, &xc, &yc );

/* store new cursor position - according to zoom factor */
   if ( curs->cur_memid == -1 )
      {

/* coordinates are with respect to screen */
/* display zooms around centre of screen */
      zoom = device[display].zoom;
      xoff = (int)( device[display].x_scroll * zoom ) -
       ( zoom - 1 ) * ( device[display].dev_xsiz / 2 );
      yoff = (int)( device[display].y_scroll * zoom ) -
       ( zoom - 1 ) * ( device[display].dev_ysiz / 2 );

      curs->x_pos = (xc - xoff) / zoom;
      curs->y_pos = (yc - yoff) / zoom;
      }
   else
      dis_mem_conv( display, curs->cur_memid, xc, yc,
                    &curs->x_pos, &curs->y_pos );
   }

if (inmemid == -1)
   {
/* cursor is related to display */

   mem_dis_conv (display, curs->cur_memid, curs->x_pos, curs->y_pos,
                 &xc, &yc);
   *xcur = xc;
   *ycur = yc;

/* search current memory for cursor */

   for (i = 0; i < conf->n_mem; i++)
      {
      mem = conf->memory[i];
      if ((mem->mem_free != -1) && (mem->visibility == 1) &&
          (*xcur >= (int)mem->x_scroll / mem->zoom) &&
          (*ycur >= (int)mem->y_scroll / mem->zoom) &&
          (*xcur <= ((int)mem->x_scroll + mem->x_v_size) / mem->zoom) &&
          (*ycur <= ((int)mem->y_scroll + mem->y_v_size) / mem->zoom))

         {
         *outmemid = i;
         break;
         }
      }
   }
else
   {
/* cursor is related to a memory */

   mem = conf->memory[inmemid];
/* check if memory has been allocated */

   if (mem->mem_free == -1)
      {
      iicerr = ILLMEMID;
      return(iicerr);
      }

   mem_dis_conv (display, curs->cur_memid, curs->x_pos, curs->y_pos,
                 &xc, &yc);
   dis_mem_conv (display, inmemid, xc, yc, xcur, ycur);
   *outmemid = inmemid;
   }

iicerr = II_SUCCESS;
return(iicerr);
}

/******************************************************************************/

int IICWCP_C ( int display, int memid, int curn, int xcur, int ycur )

/*
*+
*  Name:
*     IICWCP_C
*
*  Purpose:
*     Write cursor position
*
*  Invocation:
*     status = IICWCP_C( display, memid, curn, xcur, ycur )
*
*  Description:
*     Write cursor position
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     curn = int
*        Cursor number
*     xcur = int
*        X cursor position
*     ycur = int
*        Y cursor position
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
*      2-AUG-1991 (NE):
*        Removed display zoom factors
*      2-NOV-1992 (NE):
*        Changes for hardware cursor
*/

{

/* Local Variables */
int curconf , vis , vis0;
int xcurd, xout, ycurd, yout;
int iicerr;

CONF_DATA   *conf;
MEM_DATA    *mem;
CURS_DATA   *curs;

/* check if display has been opened */

iicerr = II_SUCCESS;

if (device[display].opened == 0)
   {
   iicerr = DEVNOTOP;
   return(iicerr);
   }

curconf = device[display].confid;
conf = device [display].config[curconf];

/* check memory id */

if ((memid < -1) || (memid >= conf->n_mem))
   {
   iicerr = ILLMEMID;
   return(iicerr);
   }

/* check cursor number */

if ((curn < 0) || (curn >= device[display].n_curs))
   {
   iicerr = ILLCURID;
   return(iicerr);
   }

curs = device[display].cursor[curn];
if (curs->cur_sh == -1)            /* shape = -1 : not initialized */
   {
   iicerr = CURNOTDEF;
   return(iicerr);
   }

/* switch off soft cursor if visible */

vis = curs->vis;
if ( (vis == 1) && (curs->cur_sh > 0) )
   {
   vis0 = 0;
   iicerr = IICSCV_C (display , curn , vis0);
   }

/* convert coordinates from associated memory to display */
/* if necessary */

if (memid == -1)
   {
   xcurd = xcur;
   ycurd = ycur;
   }
else
   mem_dis_conv (display , memid , xcur , ycur , &xcurd , &ycurd);

/* store new cursor position - according to zoom factor */

if (curs->cur_memid == -1)
   {
   curs->x_pos = xcurd;
   curs->y_pos = ycurd;
   }
else
   {
   mem = conf->memory[memid];

/* check if memory has been allocated */

   dis_mem_conv (display , memid , xcurd , ycurd ,
                 &curs->x_pos , &curs->y_pos);
   }

/* initialise hard cursor if necessary */
if ( ( curs->init ) && ( curs->cur_sh == 0 ) )
   {
   hard_cursor( display, HC_DO, xcurd, ycurd, &xout, &yout );
   curs->init = 0;
   }

/* switch on curson - if visibility == 1 */

if (vis == 1)
   {
   vis0 = 1;
   iicerr = IICSCV_C (display , curn , vis0);
   }

return(iicerr);
}

