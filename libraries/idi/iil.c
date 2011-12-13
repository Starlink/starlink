/*
*+
*  Name:
*     IIL.C
*
*  Purpose:
*     LUT Routines
*
*  Description:
*     LUT Routines
*
*  Contents:
*     IILWIT_C
*        Write Intensity Transformation Table;
*     IILRIT_C
*        Read Intensity Transformation Table;
*     IILWLT_C
*        Write Video Look-up Table;
*     IILRLT_C
*        Read  Video Look-up Table;
*     IILSBV_C
*        Set Bar Visibility;
*
*  Copyright:
*     Copyright (C) 1988, 1991, 1993 Science & Engineering Research Council.
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

/******************************************************************************/

int IILWIT_C ( int display, int memid, int ittn, int ittstart, int ittlen,
               float ittdata[] )

/*
*+
*  Name:
*     IILWIT_C
*
*  Purpose:
*     Write intensity transformation table
*
*  Invocation:
*     status = IILWIT_C( display, memid, ittn, ittstart, ittlen, ittdata )
*
*  Description:
*     Write intensity transformation table
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     ittn = int
*        ITT identifier
*     ittstart = int
*        ITT offset
*     ittlen = int
*        ITT length
*     ittdata = float[]
*        ITT data
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Added inverse intensity transformation itt->ittinv
*/

{

/* Local Variables */
int        i, im, curconf;
int        iilerr;

CONF_DATA  *conf;
MEM_DATA   *mem;
ITT_DATA   *itt;


/* check if display has been opened */

if (device[display].opened == 0)
   {
   iilerr = DEVNOTOP;
   return(iilerr);
   }

iilerr = II_SUCCESS;

curconf = device[display].confid;
conf = device[display].config[curconf];
mem = conf->memory[memid];

/* check ITT number */

if ((ittn < -1) || (ittn >= mem->n_itt))
   {
   iilerr = ITTIDERR;
   return(iilerr);
   }

if (ittn == -1)                   /* ITT id = -1 --> Current ITT */
   ittn = mem->itt_id;

itt = mem->itt[ittn];

/* check ITT length */

if (ittstart + ittlen > itt->itt_len)
   {
   iilerr = ITTLENERR;
   return(iilerr);
   }

/* store ITT values */

for (i = 0; i < ittlen; i++)
   {
   im = (int) ((float) (itt->itt_len - 1) * ittdata[i]);
   itt->ittlev[ittstart + i] = im;
   itt->ittinv[im] = ittstart + i;
   }

itt->itt_def = 1;

return(iilerr);
}

/******************************************************************************/

int IILRIT_C ( int display, int memid, int ittn, int ittstart, int ittlen,
               float ittdata[] )

/*
*+
*  Name:
*     IILRIT_C
*
*  Purpose:
*     Read intensity transformation table
*
*  Invocation:
*     status = IILRIT_C( display, memid, ittn, ittstart, ittlen, ittdata )
*
*  Description:
*     Read intensity transformation table
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     ittn = int
*        ITT identifier
*     ittstart = int
*        ITT offset
*     ittlen = int
*        ITT length
*     ittdata = float[]
*        ITT data
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
int      i , curconf;
int      iilerr;

CONF_DATA  *conf;
MEM_DATA   *mem;
ITT_DATA   *itt;


/* check if display has been opened */

if (device[display].opened == 0)
   {
   iilerr = DEVNOTOP;
   return(iilerr);
   }

iilerr = II_SUCCESS;

curconf = device[display].confid;
conf = device[display].config[curconf];
mem = conf->memory[memid];

/* check ITT number */

if ((ittn < -1) || (ittn >= mem->n_itt))
   {
   iilerr = ITTIDERR;
   return(iilerr);
   }

if (ittn == -1)                   /* ITT id = -1 --> Current ITT */
   ittn = mem->itt_id;

itt = mem->itt[ittn];

/* check ITT length */

if (ittstart + ittlen > itt->itt_len)
   {
   iilerr = ITTLENERR;
   return(iilerr);
   }

/* get ITT values */

for (i = 0; i < ittlen; i++)
   ittdata[i] = (float) itt->ittlev[ittstart + i] /
                (float) (itt->itt_len - 1);

return(iilerr);
}

/******************************************************************************/

int IILWLT_C ( int display, int lutn, int lutstart, int lutlen,
               float lutdata[] )

/*
*+
*  Name:
*     IILWLT_C
*
*  Purpose:
*     Write video look-up table
*
*  Invocation:
*     status = IILWLT_C( display, lutn, lutstart, lutlen, lutdata )
*
*  Description:
*     Write video look-up table
*
*  Arguments:
*     display = int
*        Display identifier
*     lutn = int
*        Look-up identifier
*     lutstart = int
*        Look-up offset
*     lutlen = int
*        Look-up length
*     lutdata = float[]
*        Look-up data
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     15-JAN-1993 (NE):
*        Do not test lut->lut_free to be false
*        Call IIMSLT if there is an overlay (curlut.nalloc > curlut.len)
*/

{

/* Local Variables */
int      i , icolr , icolg , icolb;
int      iilerr;

LUT_DATA   *lut;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iilerr = DEVNOTOP;
   return(iilerr);
   }

iilerr = II_SUCCESS;

/* check LUT number */

if ((lutn < -1) || (lutn >= device[display].n_lut))
   {
   iilerr = LUTIDERR;
   return(iilerr);
   }

if (lutn == -1)                   /* Lut id = -1 --> Current LUT */
   lutn = curlut.id;

lut = device[display].lookup[lutn];

/* check LUT length */

if (lutstart + lutlen > lut->lut_len)
   {
   iilerr = LUTLENERR;
   return(iilerr);
   }

/* store LUT values for R G B */

for (i = 0; i < lutlen; i++)
   {
   icolr = icol (lutdata [i]);
   icolg = icol (lutdata [lutlen + i]);
   icolb = icol (lutdata [2 * lutlen + i]);
   lut->lutr[lutstart + i] = icolr;
   lut->lutg[lutstart + i] = icolg;
   lut->lutb[lutstart + i] = icolb;
   }

lut->lut_free = 0;
                                 /* Current LUT : Update */
if ( (lutn == curlut.id) | (curlut.nalloc > curlut.len) )
   iilerr = IIMSLT_C (display , -1 , lutn , -1);

return(iilerr);
}

/******************************************************************************/

int IILRLT_C ( int display, int lutn, int lutstart, int lutlen,
               float lutdata[] )

/*
*+
*  Name:
*     IILRLT_C
*
*  Purpose:
*     Read video look-up table
*
*  Invocation:
*     status = IILRLT_C( display, lutn, lutstart, lutlen, lutdata )
*
*  Description:
*     Read video look-up table
*
*  Arguments:
*     display = int
*        Display identifier
*     lutn = int
*        Look-up identifier
*     lutstart = int
*        Look-up offset
*     lutlen = int
*        Look-up length
*     lutdata = float[]
*        Look-up data
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
int      i;
int      iilerr;

float    icol1();

LUT_DATA   *lut;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iilerr = DEVNOTOP;
   return(iilerr);
   }

iilerr = II_SUCCESS;

/* check LUT number */

if ((lutn < -1) || (lutn >= device[display].n_lut))
   {
   iilerr = LUTIDERR;
   return(iilerr);
   }

if (lutn == -1)                   /* Lut id = -1 --> Current LUT */
   lutn = curlut.id;

lut = device[display].lookup[lutn];

/* check if LUT has already been written */

if (lut->lut_free == 1)
   {
   iilerr = LUTNOTDEF;
   return(iilerr);
   }

/* check LUT length */

if (lutstart + lutlen > lut->lut_len)
   {
   iilerr = LUTLENERR;
   return(iilerr);
   }

/* read LUT values for R G B */

for (i = 0; i < lutlen; i++)
   {
   lutdata [i] = icol1 (lut->lutr[lutstart + i]);
   lutdata [lutlen + i] = icol1 (lut->lutg[lutstart + i]);
   lutdata [2 * lutlen + i] = icol1 (lut->lutb[lutstart + i]);
   }

return(iilerr);
}

/******************************************************************************/

int IILSBV_C ( int display, int memid, int vis )

/*
*+
*  Name:
*     IILSBV_C
*
*  Purpose:
*     Set bar visibility
*
*  Invocation:
*     status = IILSBV_C( display, memid, vis )
*
*  Description:
*     Set bar visibility
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     vis = int
*        Bar visibility [0/1]
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Ensure that the logical flag has values 0 or 1
*/

{

/* Local Variables */
int  curconf;
int  lvis;
int  iilerr;

CONF_DATA *conf;
MEM_DATA  *mem;

/* check if display has been opened */

iilerr = II_SUCCESS;

if (device[display].opened == 0)
   {
   iilerr = DEVNOTOP;
   return(iilerr);
   }

/* Ensure that the logical flag has values 0 or 1 */
if ( vis == 0 )
   lvis = 0;
else
   lvis = 1;

/* refresh display if vis == 0 */

/*
if (lvis == 0)
   {
   device[display].bar.vis = 0;
   refr (display , &iilerr);
   return(iilerr);
   }
*/

curconf = device[display].confid;
conf = device [display].config[curconf];
mem = conf->memory[memid];

/* store bar parameters */

device[display].bar.conf = curconf;
device[display].bar.mem = memid;
device[display].bar.itt = mem->itt_id;
device[display].bar.vis = lvis;

/* display intensity bar */

idi__bar (display , memid, lvis, &iilerr);

return(iilerr);
}

