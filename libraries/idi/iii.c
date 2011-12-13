/*
*+
*  Name:
*     III.C
*
*  Purpose:
*     Interaction Routines
*
*  Description:
*     Interaction Routines
*
*  Contents:
*     IIIENI_C
*        Enable Interaction;
*     IIISTI_C
*        Stop Interaction;
*     IIIEIW_C
*        Execute Interaction & Wait;
*     IIIGIE_C
*        Get Integer Evaluator;
*     IIIGRE_C
*        Get Real Evaluator;
*     IIIGSE_C
*        Get String Evaluator;
*     IIIGLE_C
*        Get Logical Evaluator;
*     IIIQID_C
*        Query interactor Description;
*     IIIGLD_C
*        Get Locator Displacement;
*
*  Copyright:
*     Copyright (C) 1988, 1991, 1992, 1994 Science & Engineering Research Council.
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
*        Added <iii.h>
*     23-MAY-1991 (NE):
*        Added stdlib.h for malloc
*/

/* System definitions */

# include    <stdlib.h>
# include    <stdio.h>
# include    <string.h>

/* Package definitions */

# include    "device.dep"
# include    "kwm.h"
# include    "idi.h"
# include    "idi_err.h"
# include    "idistruct_e.h"
# include    "iii.h"
# include    "idifuncs.h"

/******************************************************************************/

int IIIENI_C ( int display, int intype, int intid, int objtype, int objid,
               int oper, int trigger )

/*
*+
*  Name:
*     IIIENI_C
*
*  Purpose:
*     Enable interaction
*
*  Invocation:
*     status = IIIENI_C( display, intype, intid, objtype, objid, oper,
*                        trigger )
*
*  Description:
*     Enable interaction
*
*  Arguments:
*     display = int
*        Display identifier
*     intype = int
*        Interactor type
*     intid = int
*        Interactor id
*     objtype = int
*        Object type
*     objid = int
*        Object id
*     oper = int
*        Interactive operation
*     trigger = int
*        Exit trigger
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     25-JUL-1991 (NE):
*        Added user_reset
*     30-JUL-1991 (NE):
*        Moved exit trigger to inter_data and removed trigger number test
*/

{

/* Local Variables */
int              i , j , ii , jj , intid0 = 0;
int              slir , slig , slib;
int              iiierr;

INTER_DATA        *intdata;
INT_DEV_DATA      *intdev;
EVL_DATA          *evl;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iiierr = DEVNOTOP;
   return(iiierr);
   }

/* check max number of enabled interactions */

if (device[display].n_inter == device[display].n_max_inter)
   {
   iiierr = MAXNOINT;
   return(iiierr);
   }

i = device[display].n_inter;
intdata = device[display].inter[i];

switch (intype)
   {
   case II_LOC:
   case II_TRG:
      intdata->int_type = intype;
      intdata->int_id = intid;
      break;

   case II_EVLR:
   case II_EVLI:
   case II_EVLS:
   case II_EVLT:
      intdata->int_type = intype;

/* determine absolute number of evaluator from   */
/* its relative number (integer , real , string) */

      ii = 0;
      jj = 0;
      for (i = 0; i < int_struct.n_int_dev; i++)
         {
         intdev = int_struct.int_dev[i];
         for (j = 0; j < intdev->n_evl; j++)
            {
            evl = intdev->evl[j];
            if (evl->type == intype)
               {
               if (intid == ii) intid0 = jj;
               ii++;
               }
            jj++;
            }
         }

      intdata->int_id = intid0;
      break;

   default:
      iiierr = ILLINTTYPE;
      return(iiierr);
   }

switch (objtype)
   {
   case II_NULL:
   case II_CURSOR:
   case II_ITT:
   case II_LUT:
   case II_ROI:
   case II_MEMORY:
   case II_DISPLAY:
   case II_COLOR:
      intdata->obj_type =  objtype;
      intdata->obj_id = objid;
      break;
   default:
      iiierr = ILLINTOBJ;
      return(iiierr);
   }

switch (oper)
   {
   case II_USER:
      user_reset[display] = 1;
   case II_MOVE:
   case II_ROTATE:
   case II_ZOOM:
   case II_UNZOOM:
   case II_CLZOOM:
   case II_BLINK:
   case II_MODIFY:
      intdata->oper = oper;
      break;
   case II_SLICE:
      intdata->oper = oper;
      sli_ind = 0;
      sli_r = curlut.lutr[0];
      sli_g = curlut.lutg[0];
      sli_b = curlut.lutb[0];

/* fixed value for sliced color */

      get_slicol (&slir , &slig , &slib);
      curlut.lutr[0] = slir;
      curlut.lutg[0] = slig;
      curlut.lutb[0] = slib;
      break;
   default:
      iiierr = ILLINTOPER;
      return(iiierr);
   }

intdata->trigger = trigger;

device[display].n_inter += 1;

iiierr = II_SUCCESS;

return(iiierr);
}

/******************************************************************************/

int IIISTI_C ( int display )

/*
*+
*  Name:
*     IIISTI_C
*
*  Purpose:
*     Stop interaction
*
*  Invocation:
*     status = IIISTI_C( display )
*
*  Description:
*     Stop interaction
*
*  Arguments:
*     display = int
*        Display identifier
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Refresh any visible ROIs
*     25-JUL-1991 (NE):
*        Added user_reset
*/

{

/* Local Variables */
int   iiierr;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iiierr = DEVNOTOP;
   return(iiierr);
   }

device[display].n_inter = 0;
user_reset[display] = 0;
                                       /* disable interactive devices */
int_disable (display);
                                       /* Exit trigger disable */
exit_trg_disable (display);

/* Refresh any visible ROIs */
roi_refresh( display );

loc_zero (display);                    /* Lcators pos zero */

iiierr = II_SUCCESS;
return(iiierr);
}

/******************************************************************************/

int IIIEIW_C ( int display, int trgstatus[MAX_TRG] )

/*
*+
*  Name:
*     IIIEIW_C
*
*  Purpose:
*     Execute interaction & wait
*
*  Invocation:
*     status = IIIEIW_C( display, trgstatus )
*
*  Description:
*     Execute interaction & wait
*
*  Arguments:
*     display = int
*        Display identifier
*     trgstatus = int[]
*        Output trigger status
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
*        Allow for interactive scrolling of the pixmap.
*        Added call to roi_switch to switch active corners.
*        Added call to lut_loc_rotate to rotate LUT using a locator.
*        Added calls to dis_zoom, dis_unzoom and dis_clzoom.
*     30-APR-1991 (NE):
*        Reset ROI locator after a zoom
*     25-JUL-1991 (NE):
*        Added user_reset to prevent updates
*     30-JUL-1991 (NE):
*        Move exit trigger to inter_data and replace call to IIISTI_C
*        with calls to int_disable and exit_trg_disable
*     28-AUG-1991 (NE):
*        Alter argument list in clear_pixmap
*     16-APR-1992 (NE):
*        Call update_current_pixmap instead of clear_pixmap
*     09-MAR-1994 (DLT):
*        Correct arguments to grefr
*/

{

/* Local Variables */
short  ev_data;
short  ev_type;
short  pos[2];

int  i , trg_flag , user_flag , exit_trg;
int  userstatus , ew;
int  iiierr;
int  int_scroll;
int  int_scroll_mem = 0;

INTER_DATA     *intdata;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iiierr = DEVNOTOP;
   return(iiierr);
   }

/* check if any interaction has been enabled */

if (device[display].n_inter == 0)
   {
   iiierr = INTNOEN;
   return(iiierr);
   }

/* acquire display - if necessary */
acq_disp (display);

/* enable interactive devices */
int_enable (display);

/* Exit trigger enable */
exit_trg_enable (display);

/* Set the interactive scroll flag */
int_scroll_reset[display] = 1;

/* See if there is an interaction for a memory or display scroll */
/* or for a user interaction */
int_scroll = 0;
user_flag = 0;
for (i = 0; i < device[display].n_inter; i++)
   {
   intdata = device[display].inter[i];
   switch ( intdata->oper )
      {
      case II_USER:
         user_flag = 1;
         break;
      case II_MOVE:
         switch ( intdata->obj_type )
            {
            case II_MEMORY:
               int_scroll = 1;
               int_scroll_mem = intdata->obj_id;
               break;
            case II_DISPLAY:
               int_scroll = 1;
               int_scroll_mem = device[display].pm_mem;
               break;
            }
         break;
      }
   }

/* If there is a user interaction in progress do not update pixmap */
if ( ( user_flag == 1 ) && ( user_reset[display] == 0 ) )
   {
   int_scroll = 0;
   int_scroll_reset[display] = 0;
   }
user_reset[display] = 0;

/* If there is an interactive memory scroll then check the memory */
/* displayed in the pixmap matches the one to be scrolled */
if ( int_scroll == 1 )
   {
   if ( ( int_scroll_mem != device[display].pm_mem ) &&
        ( int_scroll_mem != device[display].pm_memov ) )
      {
      update_current_pixmap( display, int_scroll_mem );
      refr_p( display );
      grefr( display, &iiierr );
      }
   }

/* ---------------------------------------------------------------- */

iiierr = II_SUCCESS;

roi_reset = 0;
user_flag = 0;
userstatus = 0;
trg_flag = -1;
exit_trg  = -1;

/* zero trigger status array */

for (i = 0; i < MAX_TRG; i++)
   trgstatus[i] = 0;

inter_active = 1;

/* loop on enabled interactions */
/* wait until exit trigger is fired  or */
/* some USER interaction is verified    */

while ((userstatus == 0) && (exit_trg == -1))
   {
   wait_int (display , &ew , &ev_type , &ev_data , pos);

/* external event : */
/* loop on multiple enabled interactions */

   for (i = 0; i < device[display].n_inter; i++)
      {
      intdata = device[display].inter[i];
      user = 0;
      switch (intdata->oper)
         {
         case II_USER:

            user = 1;

            test_user (display, i, ev_type, ev_data, pos, ew, &user_flag,
                       &trg_flag);
            userstatus = userstatus | user_flag;
            break;

         case II_MOVE:

            switch (intdata->obj_type)
               {
               case II_MEMORY:

/* Scroll the pixmap to achieve a smooth scroll */
                  int_mem_scroll( display, i, ev_type, ev_data, pos, ew, &iiierr );
                  break;

               case II_DISPLAY:

/* Scroll the pixmap to achieve a smooth scroll */
                  int_dis_scroll( display, i, ev_type, ev_data, pos, ew, &iiierr );
                  break;

               case II_CURSOR:
                  cursor_move (display , i , ev_type , ev_data , pos , ew , &iiierr);
                  break;

               case II_ROI:

/* If the ROI is to be reset pretend that an enter window event has occured */
                  if ( roi_reset == 1 )
                     {
                     ew = 1;
                     roi_reset = 0;
                     }
                  roi_move (display , i , ev_type , ev_data , pos , ew , &iiierr);
                  break;
               }
            break;

         case II_MODIFY:

            switch (intdata->obj_type)
               {
               case II_ROI:

/* If the interaction type is a trigger then switch active corners */
                  if ( intdata->int_type == II_TRG )
                     roi_switch( display, i, ev_type, ev_data, &iiierr );

/* If the ROI is to be reset pretend that an enter window event has occured */
                  else
                     {
                     if ( roi_reset == 1 )
                        {
                        ew = 1;
                        roi_reset = 0;
                        }
                     roi_modify( display, i, ev_type, ev_data, pos, ew, &iiierr );
                     }
                  break;
               }
            break;

         case II_ROTATE:

            switch (intdata->obj_type)
               {
               case II_LUT:

/* Rotate the LUT with a locator */
                  lut_loc_rotate( display, i, ev_type, ev_data, pos, ew, &iiierr );
                  break;
               }
            break;

         case II_SLICE:

            switch (intdata->obj_type)
               {
               case II_LUT:
                  lut_slice (display , i , ev_type , ev_data , &iiierr);
                  break;
               }
            break;

         case II_ZOOM:
            switch (intdata->obj_type)
               {
               case  II_MEMORY:
                  mem_zoom (display , i , ev_type , ev_data , &iiierr);
                  break;

/* Zoom whole display */
               case II_DISPLAY:
                  dis_zoom( display, i, ev_type, ev_data, &iiierr );
                  break;
               }
            break;

         case II_UNZOOM:
            switch (intdata->obj_type)
               {
               case  II_MEMORY:
                  mem_unzoom (display , i , ev_type , ev_data , &iiierr);
                  break;

/* Unzoom whole display */
               case II_DISPLAY:
                  dis_unzoom( display, i, ev_type, ev_data, &iiierr );
                  break;
               }
            break;

         case II_CLZOOM:
            switch (intdata->obj_type)
               {
               case  II_MEMORY:
                  mem_clzoom (display , i , ev_type , ev_data , &iiierr);
                  break;

/* Clear zoom for whole display */
               case II_DISPLAY:
                  dis_clzoom( display, i, ev_type, ev_data, &iiierr );
                  break;
               }
            break;

         }

/* set flag in trigger array status */

      if (trg_flag != -1)
         trgstatus[trg_flag] = 1;
      }

/* check for exit trigger */

   sync_trg (display , -1 , ev_type , ev_data , &exit_trg);
   }

if (exit_trg != -1)                /* Exit Trigger on : Stop Interactions */
   {
/* set flag for exit trigger in array status */

   trgstatus[exit_trg] = 1;
   int_disable( display );
   exit_trg_disable( display );
   }

inter_active = 0;
                               /* release display - if necessary */
rel_disp (display);

return(iiierr);
}

/******************************************************************************/

int IIIGIE_C ( int display, int evlid, int* evlival )

/*
*+
*  Name:
*     IIIGIE_C
*
*  Purpose:
*     Get integer evaluator
*
*  Invocation:
*     status = IIIGIE_C( display, evlid, evlival )
*
*  Description:
*     Get integer evaluator
*
*  Arguments:
*     display = int
*        Display identifier
*     evlid = int
*        Evaluator identifier
*     evlival = int
*        Evaluator integer value
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
INT_DEV_DATA    *intdev;
EVL_DATA        *evl;

int  i , j , ii;
int  iiierr;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iiierr = DEVNOTOP;
   return(iiierr);
   }

/* determine absolute number of evaluator from   */
/* its relative number (integer , real , string) */

ii = 0;
for (i = 0; i < int_struct.n_int_dev; i++)
   {
   intdev = int_struct.int_dev[i];
   for (j = 0; j < intdev->n_evl; j++)
      {
      evl = intdev->evl[j];
      if (evl->type == II_EVLI)
         {
         if (evlid == ii)
/* get evaluator value */

            *evlival = evl->ival;
         ii++;
         }
      }
   }
iiierr = II_SUCCESS;
return(iiierr);
}

/******************************************************************************/

int IIIGRE_C ( int display, int evlid, float* evlrval )

/*
*+
*  Name:
*     IIIGRE_C
*
*  Purpose:
*     Get real evaluator
*
*  Invocation:
*     status = IIIGRE_C( display, evlid, evlrval )
*
*  Description:
*     Get real evaluator
*
*  Arguments:
*     display = int
*        Display identifier
*     evlid = int
*        Evaluator identifier
*     evlrval = int
*        Evaluator real value
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
int   i , j , ii;
int   iiierr;

INT_DEV_DATA    *intdev;
EVL_DATA        *evl;


/* check if display has been opened */

if (device[display].opened == 0)
   {
   iiierr = DEVNOTOP;
   return(iiierr);
   }

/* determine absolute number of evaluator from   */
/* its relative number (integer , real , string) */

ii = 0;
for (i = 0; i < int_struct.n_int_dev; i++)
   {
   intdev = int_struct.int_dev[i];
   for (j = 0; j < intdev->n_evl; j++)
      {
      evl = intdev->evl[j];
      if (evl->type == II_EVLR)
         {
         if (evlid == ii)
/* get evaluator value */

            *evlrval = evl->rval;
         ii++;
         }
      }
   }
iiierr = II_SUCCESS;
return(iiierr);
}

/******************************************************************************/

int IIIGSE_C ( int display, int evlid, char evlsval[], int* svallen )

/*
*+
*  Name:
*     IIIGSE_C
*
*  Purpose:
*     Get string evaluator
*
*  Invocation:
*     status = IIIGSE_C( display, evlid, evlsval, svallen )
*
*  Description:
*     Get string evaluator
*
*  Arguments:
*     display = int
*        Display identifier
*     evlid = int
*        Evaluator identifier
*     evlsval = int
*        Evaluator string value
*     svallen = int
*        Returned string length
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
int   i , j , ii;
int   iiierr;

INT_DEV_DATA    *intdev;
EVL_DATA        *evl;


/* check if display has been opened */

if (device[display].opened == 0)
   {
   iiierr = DEVNOTOP;
   return(iiierr);
   }

/* determine absolute number of evaluator from   */
/* its relative number (integer , real , string) */

ii = 0;
for (i = 0; i < int_struct.n_int_dev; i++)
   {
   intdev = int_struct.int_dev[i];
   for (j = 0; j < intdev->n_evl; j++)
      {
      evl = intdev->evl[j];
      if (evl->type == II_EVLS)
         {
         if (evlid == ii)
/* get evaluator value */

            strcpy (evlsval , evl->sval);
            *svallen = strlen (evlsval);
         ii++;
         }
      }
   }
iiierr = II_SUCCESS;
return(iiierr);
}

/******************************************************************************/

int IIIGLE_C ( int display, int evlid, int* evllval )

/*
*+
*  Name:
*     IIIGLE_C
*
*  Purpose:
*     Get string evaluator
*
*  Invocation:
*     status = IIIGLE_C( display, evlid, evllval )
*
*  Description:
*     Get string evaluator
*
*  Arguments:
*     display = int
*        Display identifier
*     evlid = int
*        Evaluator identifier
*     evllval = int
*        Evaluator logical value
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
INT_DEV_DATA    *intdev;
EVL_DATA        *evl;

int  i , j , ii;
int  iiierr;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iiierr = DEVNOTOP;
   return(iiierr);
   }

/* determine absolute number of evaluator from   */
/* its relative number (integer , real , string) */
/*
ii = 0;
for (i = 0; i < int_struct.n_int_dev; i++)
   {
   intdev = int_struct.int_dev[i];
   for (j = 0; j < intdev->n_evl; j++)
      {
      evl = intdev->evl[j];
      if (evl->type == II_EVLI)
         {
         if (evlid == ii)
 get evaluator value

            *evlival = evl->ival;
         ii++;
         }
      }
   }
*/
iiierr = II_SUCCESS;
return(iiierr);
}

/******************************************************************************/

int IIIQID_C ( int display, int intype, int intn, char intdscr[],
               int* dscrlen )

/*
*+
*  Name:
*     IIIQID_C
*
*  Purpose:
*     Query Interactor Description
*
*  Invocation:
*     status = IIIQID_C( display, intype, intn, intdscr, dscrlen )
*
*  Description:
*     Query Interactor Description
*
*  Arguments:
*     display = int
*        Display identifier
*     intype = int
*        Interactor type
*     intn = int
*        Interactor number
*     intdscr = int
*        Interactor description
*     dscrlen = int
*        Description length
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink, RAL)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*     23-MAY-1991 (NE):
*        Removed definition of malloc
*     09-MAR-1994 (DLT):
*        Free strings after use
*/

{

/* Local Variables */
int   iiierr;
char fildct[256];
char *sarg[ARGSIZE];
int i;
int nit;

/* check if display has been opened */

if (device[display].opened == 0)
   {
   iiierr = DEVNOTOP;
   return(iiierr);
   }

iiierr = II_SUCCESS;

/* Allocate space for the strings */
for ( i = 0; i < ARGSIZE; i++ )
   sarg[i] = (char *)malloc( SKWSIZE );

/* Get DCT description file name */
getdctfile( fildct );

/* Select the interactor type */
switch( intype )
   {
   case II_LOC:
      nit = ARGSIZE;
      kws_xtr( fildct, "LOC_DESCR", &nit, sarg );
      if ( nit == 0 )
         {
         iiierr = DCTFILERR;
         return(iiierr);
         }

/* Check the interactor number */
      if ( intn >= nit )
         {
         iiierr = ILLINTTYPE;
         return(iiierr);
         }

/* Copy the appropriate string */
      strcpy( intdscr, sarg[intn] );
      *dscrlen = strlen( intdscr );
      break;

   case II_EVLR:
   case II_EVLI:
   case II_EVLT:
   case II_EVLS:
      nit = ARGSIZE;
      kws_xtr( fildct, "EVL_DESCR", &nit, sarg );
      if ( nit == 0 )
         {
         iiierr = DCTFILERR;
         return(iiierr);
         }

/* Check the interactor number */
      if ( intn >= nit )
         {
         iiierr = ILLINTTYPE;
         return(iiierr);
         }

/* Copy the appropriate string */
      strcpy( intdscr, sarg[intn] );
      *dscrlen = strlen( intdscr );
      break;

   case II_TRG:
      nit = ARGSIZE;
      kws_xtr( fildct, "TRG_DESCR", &nit, sarg );
      if ( nit == 0 )
         {
         iiierr = DCTFILERR;
         return(iiierr);
         }

/* Check the interactor number */
      if ( intn >= nit )
         {
         iiierr = ILLINTTYPE;
         return(iiierr);
         }

/* Copy the appropriate string */
      strcpy( intdscr, sarg[intn] );
      *dscrlen = strlen( intdscr );
      break;
   }

/* Free the strings */
for ( i = 0; i < ARGSIZE; i++ )
    free( sarg[i] );

return(iiierr);
}

/******************************************************************************/

int IIIGLD_C ( int display, int locn, int* xdis, int* ydis )

/*
*+
*  Name:
*     IIIGLD_C
*
*  Purpose:
*     Get Locator Displacement
*
*  Invocation:
*     status = IIIGLD_C( display, locn, xdis, ydis )
*
*  Description:
*     Get Locator Displacement
*
*  Arguments:
*     display = int
*        Display identifier
*     locn = int
*        Locator number
*     xdis = int
*        X displacement
*     ydis = int
*        Y displacement
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
int   interactor_id, loc_id, iiierr;
static int xdis_old[MAX_LOC], ydis_old[MAX_LOC];


/* check if display has been opened */

if (device[display].opened == 0)
   {
   iiierr = DEVNOTOP;
   return(iiierr);
   }

                                 /* static values set by test_loc  */
*xdis = LOC_X[locn] - xdis_old[locn];
*ydis = LOC_Y[locn] - ydis_old[locn];

xdis_old[locn] = LOC_X[locn];
ydis_old[locn] = LOC_Y[locn];

iiierr = II_SUCCESS;
return(iiierr);
}

