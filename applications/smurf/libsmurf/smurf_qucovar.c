/*
*+
*  Name:
*     QUCOVAR

*  Purpose:
*     Find co-variance of Q and U from a POL2 observation.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_qucovar( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Calculates the co-variance of Q and U in each map pixel for a POL2
*     observation. For a single pixel, the returned co-variance is:
*
*     sum( W_i*RQ_i*RU_i )/(N*sum( W_i ))
*
*     where:

*     - RQ_I is the ith residual from the Q map, and RU_I is the ith
*     residual from the U map. A residual is the bolometer Q or U sample
*     minus the Q or U map pixel value (the pixel value is the weighted
*     mean of the Q or U sample values that fall in the pixel). These
*     residuals can be dumped by makemap when the Q and U maps are created.
*     - W_i is the weight for the ith sample. It is the geometric mean of
*     the weights associated with RQ_i and RU_i. These can also be dumped
*     by makemap, but are created initially by calcuq as the residual of
*     the fitting process for each sample.
*
*     The input NDFs should be created by running makemap twice on the
*     CALCQU output for a single POL2 observation - once to create a Q
*     map and once to create a U map. On each invocation of makemap, the
*     config should include "exportndf=(res,qua,noi,lut)". This will
*     causes two extra NDFs to be created by each invocation of makemap
*     with suffixes "QT_con_lut", "QT_con_res", "UT_con_lut" and
*     "UT_con_res". These should be supplied as input to this command.

*  ADAM Parameters:
*     QLUT = NDF (Read)
*        3-D NDF holding Q LUT model (suffix "QT_con_lut").
*     QRES = NDF (Read)
*        3-D NDF holding Q residuals time-series (suffix "QT_con_res").
*     ULUT = NDF (Read)
*        3-D NDF holding U LUT model (suffix "UT_con_lut").
*     URES = NDF (Read)
*        3-D NDF holding U residuals time-series (suffix "UT_con_res").
*     MSG_FILTER = _CHAR (Read)
*        Control the verbosity of the application. Values can be
*        NONE (no messages), QUIET (minimal messages), NORMAL,
*        VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = NDF (Write)
*        The output NDF - a 2-dimensional map holding the QU
*        co-variance at each pixel.
*     REF = NDF (Read)
*        2-D NDF holding a Q or U map made from the same POL2
*        observation. This acts as a template to define the shape, size
*        and WCS of the output NDF.

*  Authors:
*     DSB: David S Berry (EAO):
*     {enter_new_authors_here}

*  History:
*     17-OCT-2016 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2016 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

#include "star/ndg.h"
#include "star/grp.h"
#include "ndf.h"
#include "mers.h"
#include "par.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"

#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "libsmf/smf_typ.h"
#include "sc2da/sc2store.h"

/* Prototypes for local static functions. */
static void smf1_qucovar( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfQUCovar {
   double *map;
   double *prod;
   double *res_q;
   double *res_u;
   double *var_q;
   double *var_u;
   double *vprod;
   int *hits;
   int *lut_q;
   int *lut_u;
   int operation;
   size_t d1;
   size_t d2;
   smf_qual_t *qua_q;
   smf_qual_t *qua_u;
} SmfQUCovar;


void smurf_qucovar( int *status ) {

/* Local Variables: */
   AstFrameSet *iwcs;
   Grp *igrp0 = NULL;
   Grp *igrp1 = NULL;
   Grp *igrp2 = NULL;
   Grp *igrp3 = NULL;
   Grp *igrp4 = NULL;
   Grp *ogrp = NULL;
   SmfQUCovar *job_data;
   SmfQUCovar *pdata;
   ThrWorkForce *wf;
   char obsid[100];
   char obsidt[100];
   dim_t nbolo;
   dim_t nbolot;
   dim_t ntslice;
   dim_t ntslicet;
   double *map;
   double *mapweight;
   double *mapweightsq;
   double scalevar;
   int *hits;
   int indf0;
   int iw;
   int lbnd[3];
   int ndim;
   int nw;
   int ubnd[3];
   size_t bstride;
   size_t msize;
   size_t ndata;
   size_t size;
   size_t step;
   smfData *odata;
   smfData *prod_data;
   smfData *qlut_data;
   smfData *qres_data;
   smfData *ulut_data;
   smfData *ures_data;
   smfData *vprod_data;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Start AST and NDF contexts. */
   astBegin;
   ndfBegin();

/* Find the number of cores/processors available and create a pool of
   threads of the same size. */
   wf = thrGetWorkforce( thrGetNThread( SMF__THREADS, status ), status );

/* Get input NDFs. */
   kpg1Rgndf( "REF", 1, 1, "", &igrp0, &size, status );
   kpg1Rgndf( "QRES", 1, 1, "", &igrp1, &size, status );
   kpg1Rgndf( "URES", 1, 1, "", &igrp2, &size, status );
   kpg1Rgndf( "QLUT", 1, 1, "", &igrp3, &size, status );
   kpg1Rgndf( "ULUT", 1, 1, "", &igrp4, &size, status );

/* Check the Q and U residuals models have the same dimensions and are
   for the same observation. */
   smf_open_file( wf, igrp1, 1, "READ", 0, &qres_data, status );
   smf_open_file( wf, igrp2, 1, "READ", 0, &ures_data, status );

   if( *status == SAI__OK ) {
      smf_getfitss( qres_data->hdr, "OBSID", obsid, sizeof(obsid), status );
      smf_getfitss( ures_data->hdr, "OBSID", obsidt, sizeof(obsidt), status );
   }
   if( strcmp( obsid, obsidt ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "","NDFs supplied for parameters QRES and URES are for different observations.",
              status );
   }

   smf_get_dims( qres_data, NULL, NULL, &nbolo, &ntslice, &ndata,
                 &bstride, NULL, status );
   if( bstride != 1 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "","NDF supplied for parameter QRES has incorrect axis order.",
              status );
   }

   smf_get_dims( ures_data, NULL, NULL, &nbolot, &ntslicet, NULL,
                 &bstride, NULL, status );
   if( bstride != 1 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "","NDF supplied for parameter URES has incorrect axis order.",
              status );
   }

   if( ( nbolo != nbolot || ntslice != ntslicet ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "","NDFs supplied for parameters QRES and URES have different shapes.",
              status );
   }

/* Take a deep copy of the Q residuals smfData. We will use this to hold the
   product of the Q and U residuals. */
   prod_data = smf_deepcopy_smfData( wf, qres_data, 0,
          SMF__NOCREATE_DATA | SMF__NOCREATE_VARIANCE | SMF__NOCREATE_QUALITY,
          0, 0, status );
   prod_data->pntr[0] = astMalloc( ndata*sizeof(double) );

/* Take another deep copy of the Q residuals smfData. We will use this to hold the
   "variances" to associate with the Q and U products. */
   vprod_data = smf_deepcopy_smfData( wf, qres_data, 0,
          SMF__NOCREATE_DATA | SMF__NOCREATE_VARIANCE | SMF__NOCREATE_QUALITY,
          0, 0, status );
   vprod_data->pntr[0] = astMalloc( ndata*sizeof(double) );

/* How many threads do we get to play with */
   nw = wf ? wf->nworker : 1;

/* Allocate job data for threads. */
   job_data = astCalloc( nw, sizeof(*job_data) );

/* First find how many samples to process in each worker thread. */
   step = ndata/nw;
   if( step == 0 ) step = 1;

/* Store the range of samples to be processed by each thread. Ensure that
   the last thread picks up any left-over samples. */
   if( *status == SAI__OK ) {
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->d1 = iw*step;
         if( iw < nw - 1 ) {
            pdata->d2 = pdata->d1 + step - 1;
         } else {
            pdata->d2 = ndata - 1 ;
         }

/* Store other values common to all jobs. */
         pdata->qua_u = smf_select_qualpntr( ures_data, NULL, status );
         pdata->qua_q = smf_select_qualpntr( qres_data, NULL, status );
         pdata->res_u = ures_data->pntr[0];
         pdata->res_q = qres_data->pntr[0];
         pdata->var_u = ures_data->pntr[1];
         pdata->var_q = qres_data->pntr[1];
         pdata->prod = prod_data->pntr[0];
         pdata->vprod = vprod_data->pntr[0];
         pdata->operation = 1;

/* Submit the job to multiply the Q and U residuals, and the Q and U
   weights. The results go in "prod" and "wprod". */
         thrAddJob( wf, 0, pdata, smf1_qucovar, 0, NULL, status );
      }

/* Wait for all jobs to complete. */
      thrWait( wf, status );
   }

/* We can now close the input residual files. */
   smf_close_file( wf, &qres_data, status );
   smf_close_file( wf, &ures_data, status );

/* Check the Q and U LUT models have the same dimensions and are
   for the same observation. */
   smf_open_file( wf, igrp3, 1, "READ", SMF__ISFLAT, &qlut_data, status );

   if( *status == SAI__OK ) {
      smf_getfitss( qlut_data->hdr, "OBSID", obsidt, sizeof(obsidt), status );
   }
   if( strcmp( obsid, obsidt ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "","NDF supplied for parameter QLUT is for a different observation.",
              status );
   }

   smf_get_dims( qlut_data, NULL, NULL, &nbolot, &ntslicet, NULL,
                 &bstride, NULL, status );
   if( bstride != 1 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "","NDF supplied for parameter QLUT has incorrect axis order.",
              status );
   }

   if( ( nbolo != nbolot || ntslice != ntslicet ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "","NDF supplied for parameter QLUT has different shape.",
              status );
   }

   smf_open_file( wf, igrp4, 1, "READ", SMF__ISFLAT, &ulut_data, status );

   if( *status == SAI__OK ) {
      smf_getfitss( ulut_data->hdr, "OBSID", obsidt, sizeof(obsidt), status );
   }
   if( strcmp( obsid, obsidt ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "","NDF supplied for parameter ULUT is for a different observation.",
              status );
   }

   smf_get_dims( ulut_data, NULL, NULL, &nbolot, &ntslicet, NULL,
                 &bstride, NULL, status );
   if( bstride != 1 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "","NDF supplied for parameter ULUT has incorrect axis order.",
              status );
   }

   if( ( nbolo != nbolot || ntslice != ntslicet ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "","NDF supplied for parameter ULUT has different shape.",
              status );
   }

/* Flag any samples for which QLUT and ULUT have different values (some
   pixels may shift from one pixel to another because of rounding error). */
   if( *status == SAI__OK ) {
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->lut_u = ulut_data->pntr[0];
         pdata->lut_q = qlut_data->pntr[0];
         pdata->operation = 2;
         thrAddJob( wf, 0, pdata, smf1_qucovar, 0, NULL, status );
      }
      thrWait( wf, status );
   }

/* We can now close one of the LUT NDFs (since they are both the same). */
   smf_close_file( wf, &ulut_data, status );

/* Get the pixel bounds and WCS from the reference map. */
   ndgNdfas( igrp0, 1, "READ", &indf0, status );
   ndfBound( indf0, 3, lbnd, ubnd, &ndim, status );
   ndfGtwcs( indf0, &iwcs, status );

/* Create the output map with the same bounds. */
   kpg1Wgndf( "OUT", NULL, 1, 1, NULL, &ogrp, &size, status );
   smf_open_newfile( wf, ogrp, 1, SMF__DOUBLE, 3, lbnd, ubnd, 0,
                     &odata, status );

/* Store WCS. */
   if( *status == SAI__OK ) {
      ndfPtwcs( iwcs, odata->file->ndfid, status );
   }

/* Create workspace arrays. */
   msize = ( ubnd[0] - lbnd[0] + 1 )*( ubnd[1] - lbnd[1] + 1 );
   map = astMalloc( nw*msize*sizeof( *map ) );
   mapweight = astMalloc( nw*msize*sizeof( *mapweight ) );
   mapweightsq = astMalloc( nw*msize*sizeof( *mapweightsq ) );
   hits = astMalloc( nw*msize*sizeof( *hits ) );

/* Bin the QU product into a map. */
   smf_rebinmap1( wf, prod_data, vprod_data, qlut_data->pntr[0], 0, 0, 0, NULL,
                  0, SMF__Q_GOOD, 1, AST__REBININIT | AST__REBINEND, map,
                  mapweight, mapweightsq, hits, NULL, msize, 1.0, &scalevar, status );

/* Copy the map into the output NDF. */
   if( *status == SAI__OK ) {
      memcpy( odata->pntr[0], map, msize*sizeof( *map ) );

/* The variance associated with a pixel created by makemap is the standard
   error of the values that fall into the pixel. These are the values
   used by kappa:polvec to estimate the vector errors. So the co-variance
   we calculate here needs to be normalised in the same way as a standard
   error, so that it can be compared to the makemap variances directly.
   So we divide the co-variance values found above by the number of samples
   that fall in the pixel. */
      step = msize/nw;
      if( step == 0 ) step = 1;

      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->d1 = iw*step;
         if( iw < nw - 1 ) {
            pdata->d2 = pdata->d1 + step - 1;
         } else {
            pdata->d2 = msize - 1 ;
         }
         pdata->map = odata->pntr[0];
         pdata->hits = hits;
         pdata->operation = 3;
         thrAddJob( wf, 0, pdata, smf1_qucovar, 0, NULL, status );
      }
      thrWait( wf, status );
   }

/* Close everything. */
   job_data = astFree( job_data );
   map = astFree( map );
   mapweight = astFree( mapweight );
   mapweightsq = astFree( mapweightsq );
   hits = astFree( hits );

   smf_close_file( wf, &odata, status );
   smf_close_file( wf, &prod_data, status );
   smf_close_file( wf, &vprod_data, status );
   smf_close_file( wf, &qlut_data, status );

   grpDelet( &igrp0, status);
   grpDelet( &igrp1, status);
   grpDelet( &igrp2, status);
   grpDelet( &igrp3, status);
   grpDelet( &igrp4, status);
   grpDelet( &ogrp, status);

   ndfEnd( status );
   astEnd;

}



static void smf1_qucovar( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_qucovar

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     qucovar.

*  Invocation:
*     smf1_qucovar( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfQUCovar * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfQUCovar *pdata;
   double *pm;
   double *pp;
   double *prq;
   double *pru;
   double *pv;
   double *pvq;
   double *pvu;
   int *ph;
   int *plq;
   int *plu;
   size_t idata;
   smf_qual_t *pqq;
   smf_qual_t *pqu;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfQUCovar *) job_data_ptr;

/* Multiply Q and U residuals and variances, for a range of samples. */
   if( pdata->operation == 1 ) {

      pqu = pdata->qua_u + pdata->d1;
      pqq = pdata->qua_q + pdata->d1;
      pru = pdata->res_u + pdata->d1;
      prq = pdata->res_q + pdata->d1;
      pvu = pdata->var_u + pdata->d1;
      pvq = pdata->var_q + pdata->d1;
      pp = pdata->prod + pdata->d1;
      pv = pdata->vprod + pdata->d1;

      for( idata = pdata->d1; idata <= pdata->d2; idata++,
           pqu++,pqq++,pru++,prq++,pvu++,pvq++,pp++,pv++ ) {
         if( !( *pqq & SMF__Q_GOOD ) && !( *pqu & SMF__Q_GOOD ) &&
             *prq != VAL__BADD && *pru != VAL__BADD &&
             *pvq != VAL__BADD && *pvu != VAL__BADD ) {
            *pp = (*prq) * (*pru);
            *pv = sqrt( (*pvq) * (*pvu) );
         } else {
            *pp = VAL__BADD;
            *pv = VAL__BADD;
         }
      }

/* Set bad any LUT values that are different between Q and U. */
   } else if( pdata->operation == 2 ) {
      plu = pdata->lut_u + pdata->d1;
      plq = pdata->lut_q + pdata->d1;

      for( idata = pdata->d1; idata <= pdata->d2; idata++,plq++,plu++ ) {
         if( *plu != *plq ) {
            *plu = VAL__BADI;
            *plq = VAL__BADI;
         }
      }

/* Divide the co-variuance values by the hits map. */
   } else if( pdata->operation == 3 ) {
      pm = pdata->map + pdata->d1;
      ph = pdata->hits + pdata->d1;

      for( idata = pdata->d1; idata <= pdata->d2; idata++,pm++,ph++ ) {
         if( *pm != VAL__BADD && *ph > 0 ) {
            *pm /= *ph;
         } else {
            *pm = VAL__BADD;
         }
      }

   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( "", "smf1_qucovar: Illegal operation %d requested.",
               status, pdata->operation );
   }

}







