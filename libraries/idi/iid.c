/*
*+
*  Name:
*     IID.C
*
*  Purpose:
*     Device Routines
*
*  Description:
*     Device Routines
*
*  Contents:
*     IIDOPN_C
*        Open Display;
*     IIDCLO_C
*        Close Display;
*     IIDRST_C
*        Reset Display;
*     IIDQDV_C
*        Inquire Device Characteristics;
*     IIDQCI_C
*        Inquire Capability Integer;
*     IIDQCR_C
*        Inquire Capability Real;
*     IIDQDC_C
*        Inquire Defined Configuration;
*     IIDSEL_C
*        Select Defined Configuration;
*     IIDENC_C
*        Enable Dynamic Configuration;
*     IIDSTC_C
*        Stop Dynamic Configuration;
*     IIDAMY_C
*        Allocate Dynamic Memory;
*     IIDRLC_C
*        Release Configuration;
*     IIDUPD_C
*        Update Display;
*     IIDERR_C
*        Get Error;
*     IIDIAG_C
*        Get Diagnostic;
*     IIDSDP_C
*        Select Display Path;
*     IIDSNP_C
*        Get Snapshot;
*     IIDSSS_C
*        Set Split Screen;
*
*  Copyright:
*     Copyright (C) 1988, 1991, 1992, 1993, 1994 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     DLT: David Terrett (Starlink, RAL)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     23-MAY-1991 (NE):
*        Added stdlib.h for malloc, unixlib.h for getenv, ctype.h for toupper
*     22-MAY-1992 (NE):
*        Added x11defs.h for X types
*/

/* System definitions */

#include    <stdlib.h>
#include    <stdio.h>
#include    <string.h>
#include    <math.h>
#include    <ctype.h>
#if HAVE_UNIXLIB_H
#include    <unixlib.h>
#endif

#include    "gwm.h"

/* Package definitions */

#include    "device.dep"
#include    "kwm.h"
#include    "idi.h"
#include    "idi_err.h"
#include    "idistruct_e.h"
#include    "x11defs.h"
#include    "idifuncs.h"

/* Local definitions */

static  int   arg[ARGSIZE];

/******************************************************************************/

int IIDOPN_C ( char display[], int* displayid )

/*
*+
*  Name:
*     IIDOPN_C
*
*  Purpose:
*     Opens a new display for subsequent operations
*
*  Invocation:
*     status = IIDOPN_C( display, displayid )
*
*  Description:
*     Opens a new display for subsequent operations
*
*  Arguments:
*     display = char[]
*        Device type  <device>[.<window>]
*     displayid = int
*        Display identifier
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
*        Define prototype for rint function.
*        Get window records from memory file.
*        Zero locators and create pixmap.
*     23-MAY-1991 (NE):
*        Removed definition of malloc, fopen, toupper.
*        Move upper case section.
*     19-JUN-1991 (NE):
*        Call init_refr
*     11-JUL-1991 (NE):
*        Store display name in device structure
*     20-AUG-1991 (NE):
*        Pass device name direct to VDM_CRE.
*     23-AUG-1991 (NE):
*        Remove cl_display and update the memory with the pixmap dimensions
*     25-SEP-1991 (NE):
*        Set the default LUT to be that obtained from GWM
*     25-NOV-1991 (NE):
*        Added depth and lutlen to disp_init argument list
*     27-NOV-1991 (NE):
*        Make memories visible by default
*     29-NOV-1991 (NE):
*        Removed call to init_refr
*     13-FEB-1992 (NE):
*        Initialise the inverse intensity transformation in itt->ittinv
*     14-MAY-1992 (NE):
*        Define ITT to reflect LUT length.
*        Initialize overlay LUT and lut->lutpix arrays.
*        Set default configuration without calling IIDSEL.
*      2-NOV-1992 (NE):
*        Select hard cursor as default
*      4-JAN-1993 (NE):
*        Initialise device[].curs_flag and .roi_flag.
*        Initialise loc->x_max and loc->y_max with display size.
*        Add call to update_keys.
*     13-OCT-1993 (DLT):
*        Cast strlen to int
*/

{

/* Local Variables */
char    dev[DEVLEN+1] , wind[WINDLEN+1] , dispwind[DEVLEN+WINDLEN+2];
char    wtype , filwnd[256];
char    fildat[256];
int     i, j, k , l , m , nit , n_devices , nitt , x0 , y0;
int     devstream , devxoff , devyoff , devxdim , devydim , dstatus;
int     xdisp, xwind;
int     uniflag;
int     iiderr;
int     depth, lutlen;
int     index;
float   lut0 [768], r;
float   icol1();
float   factor;
#if !HAVE_DECL_RINT
double rint( double x );
#endif
FILE    *fdat;

char *devtyp = "IDI_WS";

LUT_DATA       *lut;
ITT_DATA       *itt;
CURS_DATA      *curs;
ROI_DATA       *roi;
INTER_DATA     *inter;
CONF_DATA      *conf;
MEM_DATA       *mem;
LOC_DATA       *loc;
EVL_DATA       *evl;
TRG_DATA       *trg;
INT_DEV_DATA   *intdev;

iiderr = II_SUCCESS;

/* get DCT and DAT file names */

getdatfile (fildat);

fdat = fopen (fildat , "r");          /* compressed DCT file          */
if (fdat == II_NULL)
   {
   iiderr = DCTFILERR;
   return(iiderr);
   }
                                      /* default display dimensions   */
fscanf (fdat , "%d" , &devxdim);      /* keyword = X_DIM              */
fscanf (fdat , "%d" , &devydim);      /* keyword = Y_DIM              */
devxoff = 0;
devyoff = 0;

fscanf (fdat , "%d" , &n_devices);    /* keyword = N_DISPLAY           */

if ((strpbrk (display, ".")) != 0)
                                   /* Virtual Device created by VDM  ? */
   {
   strcpy (dispwind , display);
   for (i = 0; i < (int)strlen(display); i++)
      dispwind[i] = (char) toupper ((int)dispwind[i]);

   uniflag = 0;
   for (l = 0; l < n_devices; l++)
      {
      if (strncmp(dispwind, device[l].devtyp, DEVLEN+WINDLEN+1) == 0)
         {
         *displayid = l;
         device [l].opened = 1;
         return(iiderr);
         }
      }
   }
else
   {                              /* NO, create here virtual display   */
   wtype = 'I';

/* See if device is already open */
   for (l = 0; l < n_devices; l++)
      {
      if ((device[l].unique == 1) &&
          (strncmp( display, device[l].devnam, DEVLEN ) == 0))
         {
         *displayid = l;
         device [l].opened = 1;
         return(iiderr);
         }
      }
   dstatus = VDM_CRE( display, devxoff, devyoff, devxdim, devydim,
                      wtype, dispwind );
   if (dstatus != II_SUCCESS)
      {
      iiderr = dstatus;
      return(iiderr);
      }
   uniflag = 1;
   }

                                            /* extract device & wind */
strncpy (dev , &dispwind[0] , DEVLEN);
dev[DEVLEN] = '\0';

strncpy (wind , &dispwind[DEVLEN + 1] , WINDLEN);
wind[WINDLEN] = '\0';

/* get virtual display parameters from WND file */

nit = ARGSIZE;

/* Get window information from memory file */
kwm_xtr (filwnd , wind , &nit , arg);
if (nit == 0)
   {
   iiderr = DISWNDERR;
   return(iiderr);
   }

xdisp = arg[0];
xwind = arg[1];
devxdim = arg[4];
devydim = arg[5];

/* end display window section  */
/* --------------------------  */

/* GET DEVICE PARAMETERS FROM COMPRESSED DCT FILE */
/* ---------------------------------------------- */

i = 0;
while ((device [i].opened != 0) && (i < n_devices))
   i++;
if (i > n_devices)
   {
   iiderr = NOAVAILDEV;
   return(iiderr);
   }

*displayid = i;                       /* virtual display identifier    */

/* local display init  */

disp_init (i, wind, xdisp, xwind, &depth, &lutlen, &iiderr);
if (iiderr != II_SUCCESS)
   return (iiderr);

device [i].unique    = uniflag;
device [i].opened    = 1;
device [i].vd_id    = xdisp;
device [i].wd_id    = xwind;
device [i].dynconfid = -1;
for (l = 0; l < (int)strlen(dispwind); l++)
   dispwind[l] = (char) toupper ((int)dispwind[l]);
strcpy( device[i].devtyp, dispwind );
strncpy( device[i].devnam, display, DEVLEN );
device [i].dev_xsiz  = devxdim;      /* set by display window section */
device [i].dev_ysiz  = devydim;      /* set by display window section */

device [i].x_scroll = 0;
device [i].y_scroll = 0;

device [i].zoom = 1;

device [i].curs_flag = -1;
device [i].roi_flag = -1;

fscanf( fdat, "%d", &device[i].depth );    /* keyword = DEPTH        */
/* Replace with value obtained from X */
device[i].depth = depth;

fscanf( fdat, "%d", &device[i].zoom_min ); /* keyword = ZOOM_RANGE   */
fscanf( fdat, "%d", &device[i].zoom_max );
fscanf( fdat, "%d", &device[i].n_lut );    /* keyword = N_LUT        */

for (j = 0; j < device [i].n_lut; j++)
   {
   lut = (struct lut_data *) malloc (sizeof(struct lut_data));
   if (lut == II_NULL)
      {
      iiderr = MEMALLERR;
      return(iiderr);
      }
   device [i].lookup [j] = lut;
   lut->lut_free = 1;
   fscanf( fdat, "%d", &lut->lut_len ); /* keyword = LUT_DEPTH   */
/* Replace with value obtained from GWM */
   lut->lut_len = lutlen;
   }

fscanf (fdat , "%d" , &device [i].n_curs);  /* keyword = N_CURSORS   */

for (j = 0; j < device [i].n_curs; j++)
   {
   curs = (struct curs_data *) malloc (sizeof(struct curs_data));
   if (curs == II_NULL)
      {
      iiderr = MEMALLERR;
      return(iiderr);
      }
   device[i].cursor[j] = curs;
   curs->cur_memid = -1;
   curs->cur_sh    = -1;         /* cur_sh = -1 : cursor not defined */
   curs->cur_col   = 0;
   curs->vis   = 0;
   curs->x_pos = 0;
   curs->y_pos = 0;
   curs->init = 1;
   }

fscanf (fdat , "%d" , &device [i].n_roi);  /* keyword = N_ROI       */

for (j = 0; j < device [i].n_roi; j++)
   {
   roi = (struct roi_data *) malloc (sizeof(struct roi_data));
   if (roi == II_NULL)
      {
      iiderr = MEMALLERR;
      return(iiderr);
      }
   device[i].roi[j] = roi;
   roi->memid = -1;
   roi->sh    = -1;            /* roi_sh = -1 : ROI not defined */
   roi->col   = 0;
   roi->vis   = 0;
   roi->x_min = 0;
   roi->y_min = 0;
   roi->x_max = 0;
   roi->y_max = 0;
   }
                                      /* keyword = N_INTERACTIONS */
fscanf (fdat , "%d" , &device [i].n_max_inter);

device [i].n_inter = 0;

for (j = 0; j < device [i].n_max_inter; j++)
   {
   inter = (struct inter_data *) malloc (sizeof(struct inter_data));
   if (inter == II_NULL)
      {
      iiderr = MEMALLERR;
      return(iiderr);
      }
   device[i].inter[j] = inter;
   inter->int_type = 0;
   inter->int_id   = 0;
   inter->obj_type = 0;
   inter->obj_id   = 0;
   inter->oper     = 0;
   }
                                      /* keyword = N_CONF         */
fscanf (fdat , "%d" , &device [i].n_conf);
device [i].confid    = -1;           /* configuration not selected */

for (j = 0; j < device [i].n_conf; j++)
   {
   conf = (struct conf_data *) malloc (sizeof(struct conf_data));
   if (conf == II_NULL)
      {
      iiderr = MEMALLERR;
      return(iiderr);
      }
   device [i].config [j] = conf;
   }

for (j = 0; j < device [i].n_conf; j++)
   {
   conf = device [i].config [j];
                                   /* keyword = N_MEM          */
   fscanf (fdat , "%d" , &conf->n_mem);
   }

for (j = 0; j < device [i].n_conf; j++)
   {
   conf = device [i].config [j];
   conf->memid = -1;
   for (k = 0; k < conf->n_mem; k++)
      {
      mem = (struct mem_data *) malloc (sizeof(struct mem_data));
      if (mem == II_NULL)
         {
         iiderr = MEMALLERR;
         return(iiderr);
         }
      conf->memory[k] = mem;
      }
   }

fscanf (fdat , "%d" , &nitt);    /* keyword = N_ITT           */

for (j = 0; j < device [i].n_conf; j++)
   {
   conf = device [i].config [j];
   for (k = 0; k < conf->n_mem; k++)
      {
      mem = conf->memory[k];
                                  /* keyword = MEM_X_SIZE     */
      fscanf (fdat , "%d" , &mem->x_size);
      }
   }

for (j = 0; j < device [i].n_conf; j++)
   {
   conf = device [i].config [j];
   for (k = 0; k < conf->n_mem; k++)
      {
      mem = conf->memory[k];
                                 /* keyword = MEM_Y_SIZE     */
      fscanf (fdat , "%d" , &mem->y_size);
      }
   }

for (j = 0; j < device [i].n_conf; j++)
   {
   conf = device [i].config [j];
   for (k = 0; k < conf->n_mem; k++)
      {
      mem = conf->memory[k];
                                 /* keyword = MEM_DEPTH     */
      fscanf (fdat , "%d" , &mem->depth);
/* Replace with value obtained from X */
      mem->depth = depth;
      }
   }

for (j = 0; j < device [i].n_conf; j++)
   {
   conf = device [i].config [j];
   for (k = 0; k < conf->n_mem; k++)
      {
      mem = conf->memory[k];
                                /* keyword = MEM_TYPE      */
      fscanf (fdat , "%d" , &mem->type);
      }
   }

for (j = 0; j < device [i].n_conf; j++)
   {
   conf = device [i].config [j];
   for (k = 0; k < conf->n_mem; k++)
      {
      mem = conf->memory[k];
      mem->mem_free = -1;
      mem->mmbm = 0;
      mem->attbm = 0;
      mem->ebdepth = 0;
      mem->ebpackf = 0;
      mem->visibility = 1;
      mem->x_v_size = (devxdim < mem->x_size) ? devxdim : mem->x_size;
      mem->y_v_size = (devydim < mem->y_size) ? devydim : mem->y_size;
      mem->x_v_off = 0;
      mem->y_v_off = 0;
      mem->x_woff   = 0;                        /* default I/O Transfer Window */
      mem->y_woff   = 0;
      mem->x_wdim   = mem->x_size;
      mem->y_wdim   = mem->y_size;
      mem->lut_id = 0;
      mem->itt_id = 0;
      mem->n_gel = 0;
      mem->n_tel = 0;
      mem->x_scroll = 0;
      mem->y_scroll = 0;
      mem->zoom_xsc  = 0;
      mem->zoom_ysc  = 0;
      mem->zoom_xsc_new = 0;
      mem->zoom_ysc_new = 0;
      mem->zoom         = 1;
      mem->zoom_new     = 1;
      mem->bck          = 0;
      mem->pm_id = 0;
      }
   }

for (j = 0; j < device [i].n_conf; j++)     /* ITT memory structure allocation */
   {
   conf = device [i].config [j];
   for (k = 0; k < conf->n_mem; k++)
      {
      mem = conf->memory[k];
      mem->itt_id = 0;
      mem->n_itt = nitt;
      for (l = 0; l < mem->n_itt; l++)
         {
         itt = (struct itt_data *) malloc (sizeof(struct itt_data));
         if (itt == II_NULL)
            {
            iiderr = MEMALLERR;
            return(iiderr);
            }
         mem->itt[l] = itt;
         itt = mem->itt[l];
         itt->itt_def = 1;
         itt->itt_len = (int)rint (pow ( (double) 2. , (double) mem->depth ));

         for (m = 0; m < itt->itt_len; m++)
            {
/* default ITT : linear up to a maximum value of itt->itt_len */
            itt->ittlev[m] = m;
            itt->ittinv[m] = m;
            }
         }
      }
   }

/* -------------------------------------------------------------------------- */
/* To work properly with GWM windows the LUTs and ITTs have to be */
/* initialised to reflect the allocation of pens in the window */
for ( j = 0; j < device[i].n_lut; j++ )
   {
   lut = device[i].lookup[j];

/* Initialize the lutpix array to point to entries in the curlut array */
/* using some nasty tricks. */
/* If there is an overlay then the pen numbers are interleaved */
/* Assume the base uses lut_id = 0 and the overlay uses lut_id = 1 */
/* With no overlay both LUT entries go 0,1,2,3... */
/* With an overlay the first LUT goes 0,2,4,6... and the second 1,3,5,7... */
   for ( k = 0; k < lutlen; k++ )
      lut->lutpix[k] = ( k << ~device[i].pm_mask ) +
                       ( j & ~device[i].pm_mask );
   }

/* Initialise the ITT */
for ( j = 0; j < device[i].n_conf; j++ )
   {
   conf = device[i].config[j];
   for ( k = 0; k < conf->n_mem; k++ )
      {
      mem = conf->memory[k];
      for ( l = 0; l < mem->n_itt; l++ )
         {
         itt = mem->itt[l];
         factor = (float) itt->itt_len / (float) lutlen;
         if ( factor < 1.0 ) factor = 1.0;
         for ( m = 0; m < itt->itt_len; m++ )
            {
/* default ITT : linear up to a maximum value of itt->itt_len */
            index = ( (int) ( (float) m / factor ) )
                    << ~device[i].pm_mask;
            itt->ittlev[m] = index;
            itt->ittinv[index] = m;
            }
         }
      }
   }

/* -------------------------------------------------------------------------- */
                                               /* default initialization */

device[i].bar.created = 0;

if (int_struct.opened == 0)
   local_init (i);

/* Set the default configuration = 0 */
device[i].confid = 0;

x0 = device[i].dev_xsiz / 2;           /* cursor = 0 */
y0 = device[i].dev_ysiz / 2;
iiderr = IICINC_C (i , -1 , 0 , 0 , 2 , x0 , y0);

/* Set the default LUT to be that obtained from GWM */
for ( j = 0; j < device[i].n_lut; j++ )
   {
   lut = device[i].lookup[j];
   for ( k = 0; k < curlut.len; k++ )
      {
      lut0[k] = icol1( curlut.lutr[lut->lutpix[k]] );
      lut0[curlut.len + k] = icol1( curlut.lutg[lut->lutpix[k]] );
      lut0[2 * curlut.len + k] = icol1( curlut.lutb[lut->lutpix[k]] );
      }
   iiderr = IILWLT_C( i, j, 0, curlut.len, lut0 );
   }
iiderr = IIMSLT_C( i, 0, 0, 0 );

/* Attach to the GWM pixmap */
attach_pixmap( i, 0 );

/* Update the memory to reflect the pixmap dimensions */
update_memory( i );

iiderr = II_SUCCESS;

if (int_struct.opened == 1)
   return(iiderr);

/* Interaction structure */
/* --------------------- */

fscanf (fdat , "%d" , &int_struct.n_int_dev);     /* keyword = N_INTERACTORS     */

for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = (struct int_dev_data *) malloc (sizeof(struct int_dev_data));
   int_struct.int_dev[j] = intdev;
   }

for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   fscanf (fdat , "%d" , &intdev->descr);          /* keyword = INT_DEV           */
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   fscanf (fdat , "%d" , &intdev->n_loc);          /* keyword = N_LOC             */
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   fscanf (fdat , "%d" , &intdev->n_evl);          /* keyword = N_EVL             */
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   fscanf (fdat , "%d" , &intdev->n_trg);          /* keyword = N_TRG             */
   }

for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_loc; k++)
      {
      loc = (struct loc_data *) malloc( sizeof(struct loc_data));
      intdev->loc[k] = loc;
      }
   for (k = 0; k < intdev->n_evl; k++)
      {
      evl = (struct evl_data *) malloc( sizeof(struct evl_data));
      intdev->evl[k] = evl;
      }
   for (k = 0; k < intdev->n_trg; k++)
      {
      trg = (struct trg_data *) malloc( sizeof(struct trg_data));
      intdev->trg[k] = trg;
      }
   }

for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_loc; k++)
      {
      loc = intdev->loc[k];
      fscanf (fdat , "%d" , &loc->x_min);          /* keyword = LOC_X_MIN        */
/* Replace value with default */
      loc->x_min = 0;
      }
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_loc; k++)
      {
      loc = intdev->loc[k];
      fscanf (fdat , "%d" , &loc->x_max);          /* keyword = LOC_X_MAX        */
/* Replace value with display size */
      loc->x_max = device[i].dev_xsiz;
      }
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_loc; k++)
      {
      loc = intdev->loc[k];
      fscanf (fdat , "%d" , &loc->y_min);          /* keyword = LOC_Y_MIN        */
/* Replace value with default */
      loc->y_min = 0;
      }
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_loc; k++)
      {
      loc = intdev->loc[k];
      fscanf (fdat , "%d" , &loc->y_max);          /* keyword = LOC_Y_MAX        */
/* Replace value with display size */
      loc->y_max = device[i].dev_ysiz;
      }
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_loc; k++)
      {
      loc = intdev->loc[k];
      fscanf (fdat , "%d" , &loc->left_ls);        /* keyword = LOC_LEFT_LS      */
      }
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_loc; k++)
      {
      loc = intdev->loc[k];
      fscanf (fdat , "%d" , &loc->left_hs);        /* keyword = LOC_LEFT_HS      */
      }
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_loc; k++)
      {
      loc = intdev->loc[k];
      fscanf (fdat , "%d" , &loc->right_ls);        /* keyword = LOC_RIGHT_LS    */
      }
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_loc; k++)
      {
      loc = intdev->loc[k];
      fscanf (fdat , "%d" , &loc->right_hs);        /* keyword = LOC_RIGHT_HLS    */
      }
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_loc; k++)
      {
      loc = intdev->loc[k];
      fscanf (fdat , "%d" , &loc->up_ls);           /* keyword = LOC_UP_LS        */
      }
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_loc; k++)
      {
      loc = intdev->loc[k];
      fscanf (fdat , "%d" , &loc->up_hs);           /* keyword = LOC_UP_HS        */
      }
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_loc; k++)
      {
      loc = intdev->loc[k];
      fscanf (fdat , "%d" , &loc->down_ls);         /* keyword = LOC_DOWN_LS      */
      }
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_loc; k++)
      {
      loc = intdev->loc[k];
      fscanf (fdat , "%d" , &loc->down_hs);         /* keyword = LOC_DOWN_HS      */
      }
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_loc; k++)
      {
      loc = intdev->loc[k];
      fscanf (fdat , "%d" , &loc->hs_fact);         /* keyword = LOC_HS_FACT      */
      }
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_evl; k++)
      {
      evl = intdev->evl[k];
      fscanf (fdat , "%d" , &evl->type);               /* keyword = EVL_TYPE          */
      }
   }

for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_evl; k++)
      {
      evl = intdev->evl[k];
                                                  /* keyword = EVL_DEF           */
      fscanf (fdat , "%d %d" , &evl->def[0] , &evl->def[1]);
      }
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_evl; k++)
      {
      evl = intdev->evl[k];
      fscanf (fdat , "%d" , &evl->min);            /* keyword = EVL_MIN           */
      }
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_evl; k++)
      {
      evl = intdev->evl[k];
      fscanf (fdat , "%d" , &evl->max);            /* keyword = EVL_MAX           */
      }
   }

for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_trg; k++)
      {
      trg = intdev->trg[k];
      fscanf (fdat , "%d" , &trg->type);           /* keyword = TRG_TYPE          */
      }
   }
for (j = 0; j < int_struct.n_int_dev; j++)
   {
   intdev = int_struct.int_dev[j];
   for (k = 0; k < intdev->n_trg; k++)
      {
      trg = intdev->trg[k];
      fscanf (fdat , "%d" , &trg->def);            /* keyword = TRG_DEF           */
      }
   }

int_struct.opened = 1;

/* Zero the locators */
loc_zero( i );

/* Obtain the key definitions from X */
update_keys( i );

return(iiderr);
}

/******************************************************************************/

int IIDCLO_C ( int display )

/*
*+
*  Name:
*     IIDCLO_C
*
*  Purpose:
*     Closes an opened display
*
*  Invocation:
*     status = IIDCLO_C ( display )
*
*  Description:
*     Closes an opened display
*
*  Arguments:
*     display = int
*        Display identifier
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
*        Remove redundant code
*      3-MAY-1991 (NE):
*        Reset int_struct.opened
*     20-NOV-1991 (NE):
*        Added call to wr_lut_gwm
*     27-MAY-1992 (NE):
*        Free any pixmaps not held by GWM
*     09-MAR-1994 (DLT):
*        Delete releasing ROI's after memory has been freed
*     14-MAR-1994 (DLT):
*        Free itt's before freeing memories
*        Free CGs
*        Free display list structures
*        Free interactor structures
*/

{

/* Local Variables */
int     i , j , k;
int     iiderr;
int     reset;
T_LIST  *this_el, *next_el;
INT_DEV_DATA   *intdev;

CONF_DATA  *conf;
MEM_DATA   *mem;
ROI_DATA   *roi;

/* check if device has been opened */
if (device[display].opened == 0)
   {
   iiderr = DEVNOTOP;
   return(iiderr);
   }

/* inform GWM of the current LUT */
wr_lut_gwm( display );

/* inquire the current pixmap_id from GWM */
display_id = (Display*) device[display].vd_id;
w_id = (Window) device[display].wd_id;
GWM_GetPixmap( display_id, w_id, &pixmap_id );

/* release any pixmaps that do not match this one */
/* only do this once per configuration */
for (i = 0; i < device[display].n_conf; i++)
   {
   conf = device[display].config[i];
   reset = 0;
   for (j = 0; j < conf->n_mem; j++)
      {
      mem = conf->memory[j];
      if ( reset ) mem->pm_id = 0;
      if ( ( mem->pm_id != 0 ) && ( mem->pm_id != (int)pixmap_id ) )
         {
         free_pixmap( display, i, j );
         reset = 1;
         }
      }
   }

/* free the GC used for the pixmap */
   XFreeGC (display_id, gcpix);

/* deallocate memory structure */
for (i = 0; i < device[display].n_lut; i++)
   {
   free (device[display].lookup[i]);
   device[display].lookup[i] = II_NULL;
   }

for (i = 0; i < device[display].n_curs; i++)
   {
   free (device[display].cursor[i]);
   device[display].cursor[i] = II_NULL;
   }

for (i = 0; i < device[display].n_roi; i++)
   {
   free (device[display].roi[i]);
   device[display].roi[i] = II_NULL;
   }

for (i = 0; i < device[display].n_inter; i++)
   {
   free (device[display].inter[i]);
   device[display].inter[i] = II_NULL;
   }

for (i = 0; i < device[display].n_conf; i++)
   {
   conf = device[display].config[i];
   for (j = 0; j < conf->n_mem; j++)
      {
      mem = conf->memory[j];
      if (mem->mmbm != 0)
         mmbm_deall (display , i , j);

      for (k = 0; k < mem->n_itt; k++)
      {
         free (mem->itt[k]);
      }
      this_el = mem->el_tlist;
      for (k = 0; k < mem->n_tel; k++)
      {
         next_el = this_el->next_tel;
         free (this_el);
         this_el = next_el;
      }
      free (mem);
      mem = II_NULL;
      }
   free (conf);
   conf = II_NULL;
   }

XFreeGC (display_id, (GC)device[display].gcima_id);
XFreeGC (display_id, (GC)device[display].gcdraw_id);
XFreeGC (display_id, (GC)device[display].gccurs_id);

device[display].opened = 0;
device[display].unique = 0;

for ( i = 0; i < int_struct.n_int_dev; i++)
   {
   intdev = int_struct.int_dev[i];
   for (j = 0; j < intdev->n_loc; j++)
      {
      free (intdev->loc[j]);
      }
   for (j = 0; j < intdev->n_evl; j++)
      {
      free (intdev->evl[j]);
      }
   for (j = 0; j < intdev->n_trg; j++)
      {
      free (intdev->trg[j]);
      }
   free (intdev);
   }

int_struct.opened = 0;

iiderr = II_SUCCESS;
return(iiderr);
}

/******************************************************************************/

int IIDRST_C ( int display )

/*
*+
*  Name:
*     IIDRST_C
*
*  Purpose:
*     Resets an opened display
*
*  Invocation:
*     status = IIDRST_C( display )
*
*  Description:
*     Resets an opened display
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
*        Define prototype for rint function.
*     28-AUG-1991 (NE):
*        Put cl_display after IIDSEL
*        Update the memory to reflect the pixmap dimensions
*     22-NOV-1991 (NE):
*        Fixed bug in defining default (grey) look-up table.
*     25-NOV-1991 (NE):
*        Define ITT values to match LUT length
*     27-NOV-1991 (NE):
*        Make memories visible by default
*     13-FEB-1992 (NE):
*        Initialise the inverse intensity transformation in itt->ittinv
*     27-APR-1992 (NE):
*        Remove call to update_memory and do not update memory sizes
*     14-MAY-1992 (NE):
*        Define ITT to reflect LUT length.
*        Set default configuration without calling IIDSEL.
*      2-NOV-1992 (NE):
*        Select hard cursor as default
*     19-NOV-1992 (NE):
*        Do not deallocate memories of the default configuration
*      4-JAN-1993 (NE):
*        Initialise device[].curs_flag and .roi_flag
*/

{

/* Local Variables */
char    fildct[256];
int     defconf, j , k , l , m , x0 , y0;
int     iiderr;
int     index;
float   lut0 [768], r;
float   factor;
#if !HAVE_DECL_RINT
double rint( double x );
#endif

LUT_DATA   *lut;
CURS_DATA  *curs;
ROI_DATA   *roi;
ITT_DATA   *itt;
CONF_DATA  *conf;
MEM_DATA   *mem;

/* check if device has been opened */

if (device[display].opened == 0)
   {
   iiderr = DEVNOTOP;
   return(iiderr);
   }

getdctfile (fildct);

/* Select configuration 0 as the default */
defconf = 0;
device [display].confid = -1;

device [display].n_inter = 0;                     /* reset pending interactions */

device [display].curs_flag = -1;
device [display].roi_flag = -1;

for (j = 0; j < device [display].n_lut; j++)      /* free LUTs */
   {
   lut = device [display].lookup [j];
   lut->lut_free = 1;
   }

for (j = 0; j < device [display].n_curs; j++)     /* disable cursors */
   {
   curs = device [display].cursor[j];
   curs->cur_memid = -1;
   curs->cur_sh    = -1;
   curs->cur_col   = 0;
   curs->vis   = 0;
   curs->x_pos = 0;
   curs->y_pos = 0;
   curs->init = 1;
   }

for (j = 0; j < device [display].n_roi; j++)     /* disable ROIs */
   {
   roi = device[display].roi[j];
   roi->memid = -1;
   roi->sh    = -1;
   roi->col   = 0;
   roi->vis   = 0;
   roi->x_min = 0;
   roi->y_min = 0;
   roi->x_max = 0;
   roi->y_max = 0;
   }

for (j = 0; j < device [display].n_conf; j++)
   {
   conf = device [display].config [j];        /* reset config & memory structure */

   for (k = 0; k < conf->n_mem; k++)
      {
      mem = conf->memory[k];
      mem->ebdepth = 0;
      mem->ebpackf = 0;
      mem->visibility = 1;
      mem->x_v_size = (device[display].dev_xsiz < mem->x_size) ?
                       device[display].dev_xsiz : mem->x_size;
      mem->y_v_size = (device[display].dev_ysiz < mem->y_size) ?
                       device[display].dev_ysiz : mem->y_size;
      mem->x_v_off = 0;
      mem->x_v_off = 0;
      mem->x_woff   = 0;                   /* default I/O Transfer Window */
      mem->y_woff   = 0;
      mem->x_wdim   = mem->x_size;
      mem->y_wdim   = mem->y_size;
      mem->lut_id = 0;
      mem->itt_id = 0;
      mem->x_scroll = 0;
      mem->y_scroll = 0;
      mem->zoom_xsc = 0;
      mem->zoom_ysc = 0;
      mem->zoom_xsc_new = 0;
      mem->zoom_ysc_new = 0;
      mem->zoom_new = 1;
      mem->zoom     = 1;

      for (l = 0; l < mem->n_itt; l++)           /* reset ITT structure */
         {
         itt = mem->itt[l];
         itt->itt_def = 1;
         itt->itt_len = (int)rint (pow ( (double) 2. , (double) mem->depth ));

         factor = (float) itt->itt_len / (float) curlut.len;
         if ( factor < 1.0 ) factor = 1.0;
         for ( m = 0; m < itt->itt_len; m++ )
            {
/* default ITT : linear up to a maximum value of itt->itt_len */
            index = ( (int) ( (float) m / factor ) )
                    << ~device[display].pm_mask;
            itt->ittlev[m] = index;
            itt->ittinv[index] = m;
            }
         }

      if (mem->mmbm != 0)
         {

/* Clear any memories in the default configuration */
         if (j == defconf)
            cl_bitmap(display, j, k, 0);

/* Deallocate any memories not in the default configuration */
         else
            mmbm_deall(display, j, k);
         }
      }
   }

/* -------------------------------------------------------------------------- */
                                               /* default initialization */


/* Set the default configuration = 0 */
device[display].confid = defconf;

/* Clear the window and the pixmap */
cl_display( display, 0 );

x0 = device[display].dev_xsiz / 2;           /* cursor = 0 */
y0 = device[display].dev_ysiz / 2;
iiderr = IICINC_C (display , -1 , 0 , 0 , 2 , x0 , y0);

lut = device[display].lookup[0];             /* look-up table : grey levels*/
for (j = 0; j < curlut.len; j++)
   {
   lut0[j] = (float) j / ((float) curlut.len - 1);
   lut0[curlut.len + j] = (float) j / ((float) curlut.len - 1);
   lut0[2 * curlut.len + j] = (float) j / ((float) curlut.len - 1);
   }

/* Install grey LUT */
iiderr = IILWLT_C( display, 0, 0, curlut.len, lut0 );
iiderr = IIMSLT_C( display, 0, 0, 0 );

/* -------------------------------------------------------------------------- */

iiderr = II_SUCCESS;
return(iiderr);
}

/******************************************************************************/

int IIDQDV_C ( int display, int* nconf, int* xdev, int* ydev, int* depthdev,
               int* maxlutn, int* maxittn, int* maxcurn )

/*
*+
*  Name:
*     IIDQDV_C
*
*  Purpose:
*     Inquire device characteristics
*
*  Invocation:
*     status = IIDQDV_C( display, nconf, xdev, ydev, depthdev, maxlutn,
*                        maxittn, maxcurn )
*
*  Description:
*     Inquire device characteristics
*
*  Arguments:
*     display = int
*        Display identifier
*     nconf = int
*        Number of configurations
*     xdev = int
*        Device X dimensions
*     ydev = int
*        Device X dimensions
*     depthdev = int
*        Device depth
*     maxlutn = int
*        Maximum number of LUTs
*     maxittn = int
*        Maximum number of ITTs
*     maxcurn = int
*        Maximum number of cursors
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
char fildct[256];
int  nit , xoff , yoff , status;
int  iiderr;

iiderr = II_SUCCESS;

/* check if device has been opened */

if (device[display].opened == 0)
   {
   iiderr = DEVNOTOP;
   return(iiderr);
   }

getdctfile  (fildct);

*nconf = device[display].n_conf;

status = VDM_INQ (device[display].devtyp, &xoff, &yoff, xdev, ydev);
if (status != II_SUCCESS)
   {
   iiderr = status;
   return(iiderr);
   }

nit = ARGSIZE;
kwi_xtr (fildct , "DEPTH" , &nit , arg);
if (nit == 0)
   {
   iiderr = DCTFILWARN;
   *depthdev = 0;
   }
else
   *depthdev = arg[0];

nit = ARGSIZE;
kwi_xtr (fildct , "N_LUT" , &nit , arg);
if (nit == 0)
   {
   iiderr = DCTFILWARN;
   *maxlutn = 0;
   }
else
   *maxlutn = arg[0];
nit = ARGSIZE;
kwi_xtr (fildct , "N_ITT" , &nit , arg);
if (nit == 0)
   {
   iiderr = DCTFILWARN;
   *maxittn = 0;
   }
else
   *maxittn = arg[0];

nit = ARGSIZE;
kwi_xtr (fildct , "N_CURSORS" , &nit , arg);
if (nit == 0)
   {
   iiderr = DCTFILWARN;
   *maxcurn = 0;
   }
else
   *maxcurn = arg[0];

return(iiderr);
}

/******************************************************************************/

int IIDQCI_C ( int display, int devcap, int size, int capdata[], int* ncap )

/*
*+
*  Name:
*     IIDQCI_C
*
*  Purpose:
*     Inquire device capabilities integer
*
*  Invocation:
*     status = IIDQCI_C( display, devcap, size, capdata, ncap )
*
*  Description:
*     Inquire device capabilities integer
*
*  Arguments:
*     display = int
*        Display identifier
*     devcap = int
*        Device capability integer code
*     size = int
*        Capability array size
*     capdata = int[]
*        Device capabilities array
*     ncap = int
*        Device capabilities number
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Correct number of triggers return
*     29-AUG-1991 (NE):
*        Get device size ( capability 12 ) from VDM_INQ
*      2-OCT-1991 (NE):
*        Added device type ( capability 7 )
*     25-NOV-1991 (NE):
*        Return maximum depth of VLUTs based on LUT length
*     29-JUN-1992 (NE):
*        Added number of VLUT colours ( capability 18 )
*      7-AUG-1992 (NE):
*        Return error if capability not defined
*/

{

/* Local Variables */
char fildct[256];
int  i , curconf , nit , nc , cap[16], xdev, xoff, ydev, yoff;
int  iiderr;
#if !HAVE_DECL_RINT
double rint( double x );
#endif

CURS_DATA      *curs;
ROI_DATA       *roi;
CONF_DATA      *conf;
MEM_DATA       *mem;


/* check if device has been opened */

if (device[display].opened == 0)
   {
   *ncap = 0;
   iiderr = DEVNOTOP;
   return(iiderr);
   }

iiderr = II_SUCCESS;

getdctfile  (fildct);

curconf = device[display].confid;
conf = device[display].config[curconf];

switch (devcap)
   {
   case 1:

      cap[0] = 1;               /* STANDARD Implementation */
      nc = 1;
      break;

   case 7:

      cap[0] = 3800;            /* Device type, XWindows = 3800 */
      nc = 1;
      break;

   case 10:

      nit = ARGSIZE;
      kwi_xtr (fildct , "N_CONF" , &nit , arg);
      cap[0] = arg[0];
      nc = nit;
      break;

   case 11:

      cap[0] = device[display].confid;
      nc = 1;
      break;

   case 12:

      iiderr = VDM_INQ( device[display].devtyp, &xoff, &yoff, &xdev, &ydev );
      cap[0] = xdev;
      cap[1] = ydev;
      nc = 2;
      break;

   case 13:
   case 14:
/* calculate max VLUT depth from LUT length allowing for rounding errors*/
      cap[0] = (int) ( log10( (double) curlut.len ) / log10( 2.0 ) + 0.0001 );
      nc = 1;
      break;

   case 15:

      nit = ARGSIZE;
      kwi_xtr (fildct , "N_LUT" , &nit , arg);
      cap[0] = arg[0];
      nc = nit;
      break;

   case 16:

      nit = ARGSIZE;
      kwi_xtr (fildct , "N_ITT" , &nit , arg);
      cap[0] = arg[0];
      nc = nit;
      break;

   case 17:

      nit = ARGSIZE;
      kwi_xtr (fildct , "ZOOM_RANGE" , &nit , arg);
      cap[0] = arg[0];
      cap[1] = arg[1];
      nc = nit;
      break;

   case 18:

      cap[0] = curlut.len;
      nc = 1;
      break;

   case 20:

      nc = 0;
      for (i = 0; i < conf->n_mem; i++)
         {
         mem = conf->memory[i];
         if (mem->visibility == 1)
            {
            cap[nc] = i;
            nc += 1;
            }
         }
      cap[0] = nc;
      nc = 1;
      break;

   case 21:

      if (conf->dyn == 0)
         {
         nc = conf->n_mem;
         for (i = 0; i < conf->n_mem; i++)
            cap[i] = i;
         }
      else
         {
         nc = 0;
         for (i = 0; i < conf->n_mem; i++)
            {
            mem = conf->memory[i];
            if (mem->mmbm != 0)
               {
               nc += 1;
               cap[i] = i;
               }
            }
      }

      break;

   case 22:

      for (i = 0; i < conf->n_mem; i++)
         {
         mem = conf->memory[i];
         cap[i] = mem->depth;
         }
      nc = conf->n_mem;
      break;

   case 23:

      for (i = 0; i < conf->n_mem; i++)
         {
         mem = conf->memory[i];
         cap[i] = mem->lut_id;
         }
      nc = conf->n_mem;

      break;

   case 25:

      for (i = 0; i < conf->n_mem; i++)
         {
         mem = conf->memory[i];
         cap[i] = mem->itt_id;
         }
      nc = conf->n_mem;

      break;

   case 30:

      nit = ARGSIZE;
      kwi_xtr (fildct , "TW_X_MAX" , &nit , arg);
      for (i = 0; i < conf->n_mem; i++)
         cap[i] = arg[0];
      nc = conf->n_mem;

      break;

   case 31:

      nit = ARGSIZE;
      kwi_xtr (fildct , "TW_Y_MAX" , &nit , arg);
      for (i = 0; i < conf->n_mem; i++)
         cap[i] = arg[0];
      nc = conf->n_mem;

      break;

   case 32:

      curconf = device[display].confid;
      conf = device[display].config[curconf];
      for (i = 0; i < conf->n_mem; i++)
         {
         mem = conf->memory[i];
         cap[i] = mem->x_wdim;
         }
      nc = conf->n_mem;
      break;

   case 33:

      for (i = 0; i < conf->n_mem; i++)
         {
         mem = conf->memory[i];
         cap[i] = mem->y_wdim;
         }
      nc = conf->n_mem;
      break;

   case 34:

      for (i = 0; i < conf->n_mem; i++)
         {
         mem = conf->memory[i];
         cap[i] = mem->x_woff;
         }
      nc = conf->n_mem;
      break;

   case 35:

      curconf = device[display].confid;
      conf = device[display].config[curconf];
      for (i = 0; i < conf->n_mem; i++)
         {
         mem = conf->memory[i];
         cap[i] = mem->y_woff;
         }
      nc = conf->n_mem;
      break;

   case 36:

      for (i = 0; i < conf->n_mem; i++)
         {
         mem = conf->memory[i];
         cap[i] = mem->depth;
         }
      nc = conf->n_mem;
      break;


   case 40:

      nit = ARGSIZE;
      kwi_xtr (fildct , "N_CURSORS" , &nit , arg);
      cap[0] = arg[0];
      nc = nit;
      break;

   case 41:

      nit = ARGSIZE;
      kwi_xtr (fildct , "CURS_SHAPES" , &nit , arg);
      for (i = 0; i < nit; i++)
         cap[i] = arg[i];
      nc = nit;
      break;

   case 42:

      nit = ARGSIZE;
      kwi_xtr (fildct , "CURS_SHAPES" , &nit , arg);
      cap[0] = nit;
      nc = 1;
      break;

   case 43:

      for (i = 0; i < device[display].n_curs; i++)
         {
         curs = device[display].cursor[i];
         cap[i] = curs->cur_memid;
         }
      nc = device[display].n_curs;
      break;

   case 44:

      for (i = 0; i < device[display].n_curs; i++)
         {
         curs = device[display].cursor[i];
         cap[i] = curs->cur_sh;
         }
      nc = device[display].n_curs;
      break;

   case 45:

      for (i = 0; i < device[display].n_curs; i++)
         {
         curs = device[display].cursor[i];
         cap[i] = curs->cur_col;
         }
      nc = device[display].n_curs;
      break;

   case 46:

      for (i = 0; i < device[display].n_curs; i++)
         {
         curs = device[display].cursor[i];
         cap[i] = curs->vis;
         }
      nc = device[display].n_curs;
      break;

   case 50:

      nit = ARGSIZE;
      kwi_xtr (fildct , "N_LOC" , &nit , arg);
      cap[0] = 0;
      for (i = 0; i < nit; i++)
         cap[0] += arg[i];
      nc = 1;
      break;

   case 51:

      nit = ARGSIZE;
      kwi_xtr (fildct , "N_EVL" , &nit , arg);
      nit = ARGSIZE;
      kwi_xtr (fildct , "EVL_TYPE" , &nit , arg);
      cap[0] = 0;
      for (i = 0; i < nit; i++)
         {
         if (arg[i] == II_EVLR)
            cap[0] += 1;
         }
      nc = 1;
      break;

   case 52:

      nit = ARGSIZE;
      kwi_xtr (fildct , "N_EVL" , &nit , arg);
      nit = ARGSIZE;
      kwi_xtr (fildct , "EVL_TYPE" , &nit , arg);
      cap[0] = 0;
      for (i = 0; i < nit; i++)
         {
         if (arg[i] == II_EVLI)
            cap[0] += 1;
         }
      nc = 1;
      break;

   case 53:

      nit = ARGSIZE;
      kwi_xtr (fildct , "N_EVL" , &nit , arg);
      nit = ARGSIZE;
      kwi_xtr (fildct , "EVL_TYPE" , &nit , arg);
      cap[0] = 0;
      for (i = 0; i < nit; i++)
         {
         if (arg[i] == II_EVLT)
            cap[0] += 1;
         }
      nc = 1;
      break;

   case 54:

      nit = ARGSIZE;
      kwi_xtr (fildct , "N_EVL" , &nit , arg);
      nit = ARGSIZE;
      kwi_xtr (fildct , "EVL_TYPE" , &nit , arg);
      cap[0] = 0;
      for (i = 0; i < nit; i++)
         {
         if (arg[i] == II_EVLS)
            cap[0] += 1;
         }
      nc = 1;
      break;

   case 55:

      nit = ARGSIZE;
      kwi_xtr (fildct , "N_TRG" , &nit , arg);
      cap[0] = 0;
      for (i = 0; i < nit; i++)
         cap[0] += arg[i];
      nc = 1;
      break;

   case 60:

      nit = ARGSIZE;
      kwi_xtr (fildct , "N_ROI" , &nit , arg);
      if (nit > 0)
         cap[0] = 1;
      else
         cap[0] = 0;
      nc = 1;

      break;

   case 61:

      nit = ARGSIZE;
      kwi_xtr (fildct , "N_ROI" , &nit , arg);
      cap[0] = arg[0];
      nc = nit;

      break;

   case 62:

      for (i = 0; i < device[display].n_roi; i++)
         {
         roi = device[display].roi[i];
         cap[i] = roi->memid;
         }
      nc = device[display].n_roi;
      break;

   case 63:

      for (i = 0; i < device[display].n_roi; i++)
         {
         roi = device[display].roi[i];
         cap[i] = roi->col;
         }
      nc = device[display].n_roi;
      break;

   case 64:

      for (i = 0; i < device[display].n_roi; i++)
         {
         roi = device[display].roi[i];
         cap[i] = roi->vis;
         }
      nc = device[display].n_roi;
      break;

   case 70:

      nit = ARGSIZE;
      kwi_xtr (fildct , "BLINK" , &nit , arg);
      cap[0] = arg[0];
      nc = 1;

      break;

   case 80:

      nit = ARGSIZE;
      kwi_xtr (fildct , "SPLIT_SCREEN" , &nit , arg);
      cap[0] = arg[0];
      nc = 1;

      break;

   case 90:

      nit = ARGSIZE;
      kwi_xtr (fildct , "INT_BAR" , &nit , arg);
      cap[0] = arg[0];
      nc = 1;
      break;

   case 91:

      for (i = 0; i < conf->n_mem; i++)
         {
         if (i == device[display].bar.mem)
            cap[i] = 1;
         else
            cap[i] = 0;
         }
      nc = conf->n_mem;
      break;

   case 100:

      nit = ARGSIZE;
      kwi_xtr (fildct , "SNAP_SHOT" , &nit , arg);
      cap[0] = arg[0];
      nc = 1;
      break;

   case 101:

      nit = ARGSIZE;
      kwi_xtr (fildct , "ESCAPE" , &nit , arg);
      cap[0] = arg[0];
      nc = 1;
      break;

   case 102:

      nit = ARGSIZE;
      kwi_xtr (fildct , "DIAGNOSTIC" , &nit , arg);
      cap[0] = arg[0];
      nc = 1;
      break;

   case 103:

      nit = ARGSIZE;
      kwi_xtr (fildct , "DYNAMIC" , &nit , arg);
      cap[0] = arg[0];
      nc = 1;
      break;

   default:

      nc = 0;
      iiderr = NOCAP;
      break;
   }

if (nc == 0)
   {
   *ncap = 0;
   }
else
   {
   if (nc <= size)
      *ncap = nc;
   else
      {
      *ncap = size;
      iiderr = DEVCAPTRUNC;
      }
   for (i = 0; i < *ncap; i++)
      capdata[i] = cap[i];
   }

return(iiderr);
}

/******************************************************************************/

int IIDQCR_C ( int display, int devcap, int size, float capdata[], int* ncap )

/*
*+
*  Name:
*     IIDQCR_C
*
*  Purpose:
*     Inquire device capabilities real
*
*  Invocation:
*     status = IIDQCR_C( display, devcap, size, capdata, ncap )
*
*  Description:
*     Inquire device capabilities real
*
*  Arguments:
*     display = int
*        Display identifier
*     devcap = int
*        Device capability integer code
*     size = int
*        Capability array size
*     capdata = float[]
*        Device capabilities array
*     ncap = int
*        Device capabilities number
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*      7-AUG-1992 (NE):
*        Return error if capability not defined
*/

{

/* Local Variables */
char fildct[256];
int  i , nc , cap[16];
int  iiderr;

/* check if device has been opened */

if (device[display].opened == 0)
   {
   iiderr = DEVNOTOP;
   return(iiderr);
   }

iiderr = II_SUCCESS;

getdctfile  (fildct);

switch (devcap)
   {
   case 71:

      cap[0] = 0;
      nc = 0;
      break;

   default:

      nc = 0;
      iiderr = NOCAP;
      break;
   }

if (nc == 0)
   {
   *ncap = 0;
   }
else
   {
   if (nc <= size)
      *ncap = nc;
   else
      {
      *ncap = size;
      iiderr = DEVCAPTRUNC;
      }
   for (i = 0; i < *ncap; i++)
      capdata[i] = cap[i];
   }

return(iiderr);
}

/******************************************************************************/

int IIDQDC_C ( int display, int confn, int memtyp, int maxmem, int* confmode,
               int mlist[], int mxsize[], int mysize[], int mdepth[],
               int ittlen[], int*  nmem )

/*
*+
*  Name:
*     IIDQDC_C
*
*  Purpose:
*     Inquire defined configuration
*
*  Invocation:
*     status = IIDQDC_C( display, confn, memtyp, maxmem, confmode,
*                        mlist, mxsize, mysize, mdepth, ittlen, nmem )
*
*  Description:
*     Inquire defined configuration
*
*  Arguments:
*     display = int
*        Display identifier
*     confn = int
*        Configuration number
*     memtyp = int
*        Memory type  available types IMAGE = 1, TEXT  = 2, GRAPH = 4
*     maxmem = int
*        Max memory number
*     confmode = int
*        Configuration mode  [1=mono - 2=pseudocolor - 3=RGB]
*     mlist = int[]
*        Memories id list
*     mxsize = int[]
*        Memories x size
*     mysize = int[]
*        Memories y size
*     mdepth = int[]
*        Memories depth
*     ittlen = int[]
*        Associated ITTs length
*     nmem = int
*        No. of memories in configuration
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     30-APR-1992 (NE):
*        Only return error if arrays will overflow
*/

{

/* Local variables */
int  i , memtyp0;
int  iiderr;

CONF_DATA  *conf;
MEM_DATA   *mem;

/* check if device has been opened */

if (device[display].opened == 0)
   {
   iiderr = DEVNOTOP;
   return(iiderr);
   }

if ((confn < 0) || (confn >= device [display].n_conf ))
   {
   iiderr = ILLCONFID;
   return(iiderr);
   }

if (confn == 0)                         /* current configuration */
   confn = device[display].confid;

switch (memtyp)
   {
   case II_IMAGE:
   case II_TEXT:
   case II_GRAPHIC:
   case II_IMAGE + II_GRAPHIC:
   case II_IMAGE + II_TEXT:
   case II_GRAPHIC + II_TEXT:
   case II_IMAGE + II_GRAPHIC + II_TEXT:
   case II_NULL:

      memtyp0 = memtyp;
      break;

   default:

      iiderr = MEMTYPERR;
      return(iiderr);
   }

*confmode = 1;                            /* default : pseudo-color */

*nmem = 0;

conf = device[display].config[confn];
if (conf->dyn == 0)
   {
   for (i = 0; i < conf->n_mem; i++)
      {
      mem = conf->memory[i];
      if ((mem->type & memtyp0) != 0)
         {
         if (*nmem >= maxmem)
            {
            iiderr = MAXMEMLST;
            return(iiderr);
            }
         mlist[*nmem] = i;
         mxsize[*nmem] = mem->x_size;
         mysize[*nmem] = mem->y_size;
         mdepth[*nmem] = mem->depth;
         ittlen[*nmem] = mdepth[*nmem];         /* current ITT length */
         *nmem += 1;
         }
      }
   }
else
   {
   for (i = 0; i < conf->n_mem; i++)
      {
      mem = conf->memory[i];
      if (mem->mem_free != -1)
         {
         if ((mem->type & memtyp0) != 0)
            {
            if (*nmem >= maxmem)
               {
               iiderr = MAXMEMLST;
               return(iiderr);
               }
            mlist[*nmem] = i;
            mxsize[*nmem] = mem->x_size;
            mysize[*nmem] = mem->y_size;
            mdepth[*nmem] = mem->depth;
            ittlen[*nmem] = mdepth[*nmem];        /* current ITT length */
            *nmem += 1;
            }
         }
      }
   }


iiderr = II_SUCCESS;
return(iiderr);
}

/******************************************************************************/

int IIDSEL_C ( int display, int confn )

/*
*+
*  Name:
*     IIDSEL_C
*
*  Purpose:
*     Selects a defined configuration
*
*  Invocation:
*     status = IIDSEL_C( display, confn )
*
*  Description:
*     Selects a defined configuration
*
*  Arguments:
*     display = int
*        Display identifier
*     confn = int
*        Configuration number
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     22-MAY-1992 (NE):
*        Select the pixmap that is used by this configuration
*/

{

/* Local Variables */
int  iiderr;
CONF_DATA *conf;
MEM_DATA *mem;

/* check if device has been opened */

if (device[display].opened == 0)
   {
   iiderr = DEVNOTOP;
   return(iiderr);
   }

/* check configuration number */

if ((confn < 0) || (confn >= device [display].n_conf ))
   {
   iiderr = ILLCONFID;
   return(iiderr);
   }

/* Remember the new configuration identifier */
device [display].confid = confn;

/* Update the display memories */
conf = device[display].config[confn];
if ( device[display].overlay && ( conf->n_mem > 1 ) )
   {
   device[display].pm_mem = 0;
   device[display].pm_memov = conf->memid;
   }
else
   {
   device[display].pm_mem = 0;
   device[display].pm_memov = -99;
   }

/* Pass the pixmap id to GWM */
mem = conf->memory[0];
GWM_SetPixmap( (Display*) device[display].vd_id,
               (Window) device[display].wd_id, (Pixmap) mem->pm_id );

/* Refresh the window */
refr( display, &iiderr );

iiderr = II_SUCCESS;
return(iiderr);
}

/******************************************************************************/

int IIDENC_C ( int display )

/*
*+
*  Name:
*     IIDENC_C
*
*  Purpose:
*     Enable dynamic memory configuration
*
*  Invocation:
*     status = IIDENC_C( display )
*
*  Description:
*     Enable dynamic memory configuration
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
*     23-MAY-1991 (NE):
*        Removed definition of malloc
*/

{

/* Local Variables */
int    i , nit , nmem;
int    iiderr;
char   fildct[256];

CONF_DATA  *conf;


/* check if device has been opened */

if (device[display].opened == 0)
   {
   iiderr = DEVNOTOP;
   return(iiderr);
   }

/* check if dynamic memory configuration is allowed */

getdctfile  (fildct);

nit = ARGSIZE;
kwi_xtr (fildct , "DYNAMIC" , &nit , arg);
if (nit == 0)
   {
   iiderr = DCTFILWARN;
   return(iiderr);
   }

/* Dynamic memory configuration not allowed on this device */

if (arg[0] == 0)
   {
   iiderr = NODYNCONF;
   return(iiderr);
   }

/* dynamic memory configuration already enabled ? */

if (device[display].dynconfid != -1)
   {
   iiderr = DYNCONFEN;
   return(iiderr);
   }

/* check max number of configurations */

nit = ARGSIZE;
kwi_xtr (fildct , "MAX_CONF" , &nit , arg);
if (nit == 0)
   {
   iiderr = DCTFILWARN;
   return(iiderr);
   }

if (device[display].n_conf == arg[0])
   {
   iiderr = MAXCONFN;
   return(iiderr);
   }
                     /* search for first avaliable configuration */

i = 0;
while (device[display].config[i] != II_NULL)
   i++;
device[display].dynconfid = i;

/* allocate configuration structure */

conf = (struct conf_data *) malloc (sizeof(struct conf_data));
if (conf == II_NULL)
   {
   iiderr = MEMALLERR;
   return(iiderr);
   }

nit = ARGSIZE;
kwi_xtr (fildct , "N_MEM" , &nit , arg);
if (nit == 0)
   {
   iiderr = DCTFILWARN;
   return(iiderr);
   }

conf->dyn = 0;
conf->n_mem = 0;
nmem = arg[0];
for (i = 0; i < nmem; i++)
   conf->memory[i] = II_NULL;

device [display].config [device[display].dynconfid] = conf;

iiderr = II_SUCCESS;
return(iiderr);
}

/******************************************************************************/

int IIDSTC_C ( int display, int* confid )

/*
*+
*  Name:
*     IIDSTC_C
*
*  Purpose:
*     Stop dynamic memory configuration
*
*  Invocation:
*     status = IIDSTC_C( display, confid )
*
*  Description:
*     Stop dynamic memory configuration
*
*  Arguments:
*     display = int
*        Display identifier
*     confid = int
*        Configuration number
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
int    iiderr;

/* check if device has been opened */

if (device[display].opened == 0)
   {
   iiderr = DEVNOTOP;
   return(iiderr);
   }

/* dynamic memory configuration not enabled */

if (device[display].dynconfid == -1)
   {
   iiderr = DYNCONFNOTEN;
   return(iiderr);
   }

*confid = device[display].dynconfid;
device[display].n_conf += 1;

device[display].dynconfid = -1;
iiderr = II_SUCCESS;
return(iiderr);
}

/******************************************************************************/

int IIDAMY_C ( int display, int xdim, int ydim, int mdepth, int mtype,
               int* memid )

/*
*+
*  Name:
*     IIDAMY_C
*
*  Purpose:
*     Dynamic memory definition
*
*  Invocation:
*     status = IIDAMY_C( display, xdim, ydim, mdepth, mtype, memid )
*
*  Description:
*     Dynamic memory definition
*
*  Arguments:
*     display = int
*        Display identifier
*     xdim = int
*        Memory X size
*     ydim = int
*        Memory Y size
*     mdepth = int
*        Memory depth
*     mtype = int
*        Memory type
*     memid = int
*        Memory identifier
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Define prototype for rint function.
*     23-MAY-1991 (NE):
*        Removed definition of malloc
*     11-SEP-1991 (NE):
*        Create a new pixmap
*     25-NOV-1991 (NE):
*        Define ITT values to match LUT length
*     27-NOV-1991 (NE):
*        Make memories visible by default
*     13-FEB-1992 (NE):
*        Initialise the inverse intensity transformation in itt->ittinv
*     16-APR-1992 (NE):
*        Move body of code to define_memory routine
*/

{

/* Local Variables */
int   curconf;
int   iiderr;

iiderr = II_SUCCESS;

/* check if device has been opened */

if (device[display].opened == 0)
   {
   iiderr = DEVNOTOP;
   return(iiderr);
   }

iiderr = define_memory( display, xdim, ydim, mdepth, mtype, memid );
if ( iiderr != 0 ) return(iiderr);

/* allocate memory */
curconf = device[display].dynconfid;
mmbm_all( display, curconf, *memid, &iiderr );

/* create a new pixmap */
create_pixmap( display, *memid, curconf );

return(iiderr);
}

/******************************************************************************/

int IIDRLC_C ( int display, int confid )

/*
*+
*  Name:
*     IIDRLC_C
*
*  Purpose:
*     Release dynamic configuration
*
*  Invocation:
*     status = IIDRLC_C( display, confid )
*
*  Description:
*     Release dynamic configuration
*
*  Arguments:
*     display = int
*        Display identifier
*     confid = int
*        Configuration number
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink, RAL)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     27-MAY-1992 (NE):
*        Release any pixmap not used by GWM
*     11-MAR-1994 (DLT):
*        Free itt's before freeing memories
*/

{

/* Local Variables */
int  j, k;
int  iiderr;
int  reset;

CONF_DATA  *conf;
MEM_DATA   *mem;

/* check if device has been opened */

if (device[display].opened == 0)
   {
   iiderr = DEVNOTOP;
   return(iiderr);
   }

conf = device[display].config[confid];

/* inquire the current pixmap_id from GWM */
display_id = (Display*) device[display].vd_id;
w_id = (Window) device[display].wd_id;
GWM_GetPixmap( display_id, w_id, &pixmap_id );

/* release any pixmaps that do not match this one */
/* only do this once per configuration */
reset = 0;
for (j = 0; j < conf->n_mem; j++)
   {
   mem = conf->memory[j];
   if ( reset ) mem->pm_id = 0;
   if ( ( mem->pm_id != 0 ) && ( mem->pm_id != (int)pixmap_id ) )
      {
      free_pixmap( display, confid, j );
      reset = 1;
      }
   }

/* release the memory */
for (j = 0; j < conf->n_mem; j++)
   {
   mem = conf->memory[j];
   if (mem->mmbm != 0)
      mmbm_deall (display , confid , j);

   for (k = 0; k < mem->n_itt; k++)
   {
      free (mem->itt[k]);
   }
   free (mem);
   mem = II_NULL;
   }

free (conf);
conf = II_NULL;
device[display].config[confid] = II_NULL;

device[display].n_conf -= 1;

iiderr = II_SUCCESS;
return(iiderr);
}

/******************************************************************************/

int IIDUPD_C ( int display )

/*
*+
*  Name:
*     IIDUPD_C
*
*  Purpose:
*     Update an opened display
*
*  Invocation:
*     status = IIDUPD_C( display )
*
*  Description:
*     Update an opened display
*
*  Arguments:
*     display = int
*        Display identifier
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
int  iiderr;

/* check if device has been opened */

if (device[display].opened == 0)
   {
   iiderr = DEVNOTOP;
   return(iiderr);
   }

refr  (display , &iiderr);

return(iiderr);
}

/******************************************************************************/

#include "ems.h"
#include "ems_par.h"

void IIDERR_C ( int errn, char errtxt[], int* txtlen )

/*
*+
*  Name:
*     IIDERR_C
*
*  Purpose:
*     Get error
*
*  Invocation:
*     IIDERR_C( errn, errtxt, txtlen )
*
*  Description:
*     Get error
*
*  Arguments:
*     errn = int
*        Error number
*     errtxt = char[]
*        Error text
*     txtlen = int
*        Error text length
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Relocate error text in file "idi_err.htxt"
*     23-MAY-1991 (NE):
*        Removed definition of malloc, fopen, getenv
*     22-JAN-1992 (DLT):
*        Add default value for IDI_DIR
*     26-MAR-1992 (DLT):
*        Return error message if idi_err.htxt not found
*     25-MAR-2004 (TIMJ):
*        Give up on the .htxt idea and simply use the standard
*        fac error message tables installed in any starlink system
*        and generated from the .msg file during the build. This is
*        fairly safe since IDI already ends up with EMS during its
*        build anyway. Even if EMS is dumped in the future, it will
*        be easier to copy the code that parses the fac file.
*/

{
  /* The old implemntation implied a maximum length of error text of 80
     characters */
  int maxlen = 80;

  /* Somewhere to receive the result */
  char errstr[EMS__SZMSG];

  /* Decode the error code */
  ems1Fcerr( errstr, &errn );

  /* Copy the result into the output buffer up to maxlen characters. */
  strncpy(errtxt, errstr, maxlen);

  /* Set the length */
  *txtlen = strlen(errtxt);
  return;
}

/******************************************************************************/

int IIDIAG_C ( int display, int outf )

/*
*+
*  Name:
*     IIDIAG_C
*
*  Purpose:
*     Diagnostic output
*
*  Invocation:
*     status = IIDIAG_C( display, outf )
*
*  Description:
*     Diagnostic output
*
*  Arguments:
*     display = int
*        Display identifier
*     outf = int
*        Output flag
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
int  iiderr;

/* check if device has been opened */

if (device[display].opened == 0)
   {
   iiderr = DEVNOTOP;
   return(iiderr);
   }

if (outf == 0)
   idi_diag = 0;
else
   idi_diag = -1;

iiderr = II_SUCCESS;
return(iiderr);
}

/******************************************************************************/

int IIDSDP_C ( int display, int memlist[], int nmem, int lutflag[],
               int ittflag[] )

/*
*+
*  Name:
*     IIDSDP_C
*
*  Purpose:
*     Select display path
*
*  Invocation:
*     status = IIDSDP_C( display, memlist, nmem, lutflag, ittflag )
*
*  Description:
*     Select display path
*
*  Arguments:
*     display = int
*        Display identifier
*     memlist = int[]
*        Memory list
*     nmem = int
*        Number of memories
*     lutflag = int[]
*        LUT flags
*     ittflag = int[]
*        ITT flags
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
int  iiderr;

/* check if device has been opened */

if (device[display].opened == 0)
   {
   iiderr = DEVNOTOP;
   return(iiderr);
   }

iiderr = II_SUCCESS;
return(iiderr);
}

/******************************************************************************/

int IIDSNP_C ( int display, int colmode, int npixel, int xoff, int yoff,
               int depth, int packf, int data[] )

/*
*+
*  Name:
*     IIDSNP_C
*
*  Purpose:
*     Create snapshot
*
*  Invocation:
*     status = IIDSNP_C ( display, colmode, npixel, xoff, yoff, depth,
*                         packf, data )
*
*  Description:
*     Create snapshot
*
*  Arguments:
*     display = int
*        Display identifier
*     colmode = int
*        Colour mode
*     npixel = int
*        Number of data pixels
*     xoff = int
*        X offset
*     yoff = int
*        Y offset
*     depth = int
*        Data depth
*     packf = int
*        Packing factor
*     data = int[]
*        Image data
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*     14-MAY-1991 (NE):
*        Return an error if device depth is 4 bits or less
*     26-NOV-1991 (NE):
*        Truncate data only if depth greater than memory depth
*/

{

/* Local Variables */
int    iy, memid, dd, curconf, nlines, npix;
int    iiderr;

CONF_DATA  *conf;
MEM_DATA   *mem;

/* reset status */
iiderr = II_SUCCESS;

/* check if device has been opened */
if (device[display].opened == 0)
   {
   iiderr = DEVNOTOP;
   return(iiderr);
   }

/* Use memory 0 to check the transfer window size */
memid = 0;
curconf = device[display].confid;
conf = device [display].config[curconf];
mem = conf->memory[memid];

/* Check that the offsets lie within the transfer window */
if ( ( xoff < 0 ) || ( xoff > mem->x_wdim ) ||
     ( yoff < 0 ) || ( yoff > mem->y_wdim ) )
   {
   iiderr = IMGTOOBIG;
   return(iiderr);
   }

/* Calculate how many lines have to be read from the pixmap */
npix = npixel - ( mem->x_wdim - xoff );
nlines = npix / mem->x_wdim;
nlines += ( npix - nlines * mem->x_wdim == 0 ? 1 : 2 );

/* check image dimension */
if ( yoff + nlines > mem->y_size )
   {
   iiderr = IMGTOOBIG;
   return(iiderr);
   }

/* check memory depth is greater than 4 */
if ( mem->depth <= 4 )
   {
   iiderr = DEPTHERR;
   return(iiderr);
   }

/* check data depth against memory depth */
if ( depth > mem->depth )
   {
   iiderr = DEPTHERR;
   dd = depth - mem->depth;          /* no. of bits to truncate */
   }
else
   dd = 0;

snap_pix ( display, colmode, npixel, xoff, yoff, nlines, dd, depth,
           packf, data );

return(iiderr);
}

/******************************************************************************/

int IIDSSS_C ( int display, int memid[], int xoff[], int yoff[], int splitf,
               int splitx, int splity )

/*
*+
*  Name:
*     IIDSSS_C( display, memid, xoff, yoff, splitf, splitx, splity )
*
*  Purpose:
*     Set split screen
*
*  Invocation:
*     status = IIDSSS_C( display, memid, xoff, yoff, splitf, splitx, splity )
*
*  Description:
*     Set split screen
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int[]
*        Memory list
*     xoff = int[]
*        X offsets
*     yoff = int[]
*        Y offsets
*     splitf = int
*        Split flag
*     splitf = int
*        X split location
*     splitf = int
*        Y split location
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
int  iiderr;

/* check if device has been opened */

if (device[display].opened == 0)
   {
   iiderr = DEVNOTOP;
   return(iiderr);
   }

iiderr = II_SUCCESS;
return(iiderr);
}

