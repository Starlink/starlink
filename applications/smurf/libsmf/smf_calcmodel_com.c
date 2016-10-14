/*
*+
*  Name:
*     smf_calcmodel_com

*  Purpose:
*     Calculate the COMmon-mode model signal component

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_com( ThrWorkForce *wf, smfDIMMData *dat, int
*			 chunk, AstKeyMap *keymap, smfArray
*			 **allmodel, int flags, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     dat = smfDIMMData * (Given)
*        Struct of pointers to information required by model calculation
*     chunk = int (Given)
*        Index of time chunk in allmodel to be calculated
*     keymap = AstKeyMap * (Given)
*        Parameters that control the iterative map-maker
*     allmodel = smfArray ** (Returned)
*        Array of smfArrays (each time chunk) to hold result of model calc
*     flags = int (Given )
*        Control flags.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Calculate the common-mode signal measured at every time-slice.
*
*     For each time slice, find a sigma-clipped mean of all bolometers -
*     including those flagged as "unusual" by previous COM estimations.
*     Then find the gains, offsets and correlation coefficients by
*     comparing each bolometer block against this new COM signal. Flag
*     unusual bolo-blocks with the SMF__Q_COM flag.

*  Notes;
*     - At the moment, the old COM algorithm is still available and can
*     be used by setting COM.OLDALG non-zero. When the new algorithm
*     implemented in this function has been verified sufficiently,
*     support for the old algorithm should probably be removed in order
*     to avoid accumulation of historical clutter.
*     - In order to avoid accumulation of historical clutter, this version
*     does not include facilities that are not likely to be used any
*     more. The following keymap items are no longer used: com.boxcar,
*     com.boxfact, com.boxcard, con.boxmin, com .notfirst, com.noremove,
*     gai.flatfield. Uses of these parameters in other places should be
*     removed (e.g. gai.flatfield is used in smf_calcmodel_gai).

*  Authors:
*     David Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     7-MAY-2012 (DSB):
*        Complete re-write of the original smf_calcmodel_com to avoid using
*        different sets of bolometers to estimate COM within each block.
*        This was the approach used by the previous version of
*        smf_calcmodel_com, and led to discontinuities at the edges of
*        blocks, which in turn caused ringing in the FLT model.
*     3-SEP-2012 (DSB):
*        Tidy up the code to clarify the fact that the common mode
*        esimate is the unweighted mean of the unnormalised bolometer
*        residuals.
*     27-FEB-2013 (DSB):
*        Do not modify the com mask to exclude unused map pixels on the
*        first iteration as the map has not yet been determined.
*     19-MAR-2013 (DSB):
*        Allow a different value to be used for COM.PERARRAY on the final
*        iteration.
*     2-DEC-2013 (DSB):
*        Add a filter to flag time slices with inconsistent common mode
*        values, controlled by parameter com.sig_limit.
*     18-DEC-2013 (DSB):
*        Undo the old COM model as a separate step (performed at the
*        start of each new iteration - like FLT and EXT).
*     6-JUN-2014 (DSB):
*        Allow COM flagging to be frozen after a specified number of
*        iterations. This can help convergence.
*     28-AUG-2015 (DSB):
*        Fix bug that could cause more data than necessary to be rejected by
*        the GAI model when using COM.PERARRAY=1. A hang-over from the long
*        past days when the COM model used to iterate round forming new GAI
*        models until the rejection of samples converged. Any bolo-blocks
*        rejected when forming the GAI model for the first subarray would
*        also be rejected for subsequent subarrays regardless of whether
*        those bolo-blocks were bad or not. The effect was cumulative -
*        any additional genuinely bad bolo-block rejected from the second
*        subarray would be passed on to the third and fourth subarrays, etc.
*     13-OCT-2016 (DSB):
*        Provide facility (used by the PCA model) to fill gaps in the
*        residuals using the COM model, rather than subtract the COM model
*        off the residuals. This option is selected by setting the
*        SMF__DIMM_PCACOM bit in "flags".

*  Copyright:
*     Copyright (C) 2016 East Asian Observatory.
*     Copyright (C) 2012-2014 Science and Technology Facilities Council.
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

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "star/thr.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Prototypes for local static functions. */
static void smf1_calcmodel_com( void *job_data_ptr, int *status );

#define FILLVAL -1.23456E20

/* Local data types */
typedef struct smfCalcModelComData {
   dim_t b1;
   dim_t b2;
   dim_t idx_hi;
   dim_t idx_lo;
   dim_t nb;
   dim_t nbolo;
   dim_t ncol;
   dim_t nointslice;
   dim_t nrow;
   dim_t ntslice;
   dim_t nvar;
   dim_t t1;
   dim_t t2;
   dim_t wing;
   double **com_datas;
   double *com_data;
   double limit;
   double mean;
   double nsigma;
   double stddev;
   double svar;
   int fill;
   int flag;
   int freeze_flags;
   int gain_box;
   int icom;
   int nblock;
   int niter;
   int operation;
   smfArray *gai;
   smfArray *lut;
   smfArray *model;
   smfArray *noi;
   smfArray *res;
   smf_qual_t badqual;
   unsigned char *mask;
} SmfCalcModelComData;


void smf_calcmodel_com( ThrWorkForce *wf, smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap, smfArray **allmodel, int flags,
                        int *status ){

/* Local Variables: */
   AstKeyMap *kamap;
   AstKeyMap *kfmap;
   AstKeyMap *kmap;
   AstObject *obj;
   SmfCalcModelComData *pdata;
   SmfCalcModelComData *job_data = NULL;
   const char *modelname;
   dim_t bolostep;
   dim_t gain_box;
   dim_t i;
   dim_t idx;
   dim_t idx_hi;
   dim_t idx_lo;
   dim_t itime;
   dim_t nb;
   dim_t nbolo;
   dim_t ncol;
   dim_t nointslice;
   dim_t nrow;
   dim_t ntslice;
   dim_t nvar;
   dim_t timestep;
   double *com_datas[ 4 ];
   double *lof;
   double *p1;
   double *p2;
   double nsigma;
   double period;
   double sig_limit;
   double svar;
   int dofft;
   int fill;
   int freeze_flags;
   int icom;
   int iw;
   int nblock;
   int ncom;
   int niter;
   int nw;
   int oldalg;
   int perarray;
   int sig_wing;
   int weight;
   int whiten;
   smfArray *gai;
   smfArray *lut;
   smfArray *model;
   smfArray *noi;
   smfArray *res;
   smfFilter *filt=NULL;
   smf_modeltype modeltype;
   smf_qual_t badqual;
   unsigned char *mask;
   void *old_model;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Get a pointer to the KeyMap holding parameters controlling the
   common-mode model. If this function is being called as part of the PCA
   model, then use "PCA.xxx" keymap values instead of "COM.xxx" values. */
   modelname = ( flags & SMF__DIMM_PCACOM ) ? "PCA" : "COM";
   astMapGet0A( keymap, modelname, &obj );
   kmap = (AstKeyMap *) obj;

/* If the old algorithm is to be used, invoked it and return. */
   astMapGet0I( kmap, "OLDALG", &oldalg );
   if( oldalg ) {
      kmap = astAnnul( kmap );
      smf_calcmodel_com_old( wf, dat, chunk, keymap, allmodel, flags,
                             status );
      return;

/* If using the new algorithm, check no options have been requested
   that are not supported by the new algorithm. */
   } else {
      int i, temp;
      const char *dead[] = { "BOXCAR", "NOTFIRST", "NOREMOVE" };
      for( i = 0; i < 3; i++ ) {
         astMapGet0I( kmap, dead[ i ], &temp );
         if( temp && *status == SAI__OK ) {
            errRepf( "", "Cannot use COM.%s with new COM algorithm.",
                     status, dead[ i ] );
         }
      }
   }

/* Start an AST context to record details of AST Objects created in
   this function. */
   astBegin;

/* Get some initial configuration parameters. */
   astMapGet0I( kmap, "NITER", &niter );
   astMapGet0I( kmap, "FREEZE_FLAGS", &freeze_flags );
   astMapGet0D( kmap, "NSIGMA", &nsigma );
   astMapGet0D( kmap, "SIG_LIMIT", &sig_limit );
   astMapGet0I( kmap, "SIG_WING", &sig_wing );
   astMapGet0I( kmap, "FILL", &fill );
   if( fill ) msgOutif( MSG__VERB, "", "  Flagged values will be filled "
                        "before finding COM.", status );
   astMapGet0I( kmap, "WEIGHT", &weight );

/* We do not need the NOI model if we are just adding on an old COM model. */
   if( flags & SMF__DIMM_INVERT ) weight = 0;

/* Ensure the data is bolo-ordered (i.e. adjacent values in memory are
   adjacent time slices from the same bolometer). */
   smf_model_dataOrder( wf, dat, NULL, chunk, SMF__RES|SMF__QUA|SMF__GAI|
                        SMF__NOI|SMF__LUT, 0, status );

/* Obtain pointers to relevant smfArrays for this chunk */
   res = dat->res[ chunk ];
   lut = dat->lut ? dat->lut[ chunk ] : NULL;
   gai = dat->gai ? dat->gai[ chunk ] : NULL;
   noi = ( weight && dat->noi ) ? dat->noi[ chunk ] : NULL;
   model = allmodel[ chunk ];

/* See if a mask should be used to exclude bright source areas from
   the COM model. Cannot mask if no LUT is available. If this function
   is being called as part of the PCA model, then use "PCA.xxx" keymap
   values to define the mask instead of "COM.xxx" values. */
   modeltype = ( flags & SMF__DIMM_PCACOM ) ? SMF__PCA : SMF__COM;
   mask = lut ? smf_get_mask( wf, modeltype, keymap, dat, flags, status ) : NULL;

/* If we have a mask, copy it into the quality array of the map.
   Also set map pixels that are not used (e.g. corner pixels, etc)
   to be background pixels in the mask. */
   if( mask ) {
      double *map = dat->map;
      smf_qual_t *mapqual = dat->mapqual;
      double *mapvar = dat->mapvar;
      smf_qual_t qval = ( flags & SMF__DIMM_PCACOM ) ? SMF__MAPQ_PCA : SMF__MAPQ_COM;

      for( i=0; i<dat->msize; i++ ) {
         if( mask[i] ) {
            mapqual[i] |= qval;

         } else if( dat->iter > 0 && ( map[i] == VAL__BADD ||
                    mapvar[i] == VAL__BADD || mapvar[i] <= 0.0 ) ) {
            mask[i] = 1;
            mapqual[i] |= qval;

         } else {
            mapqual[i] &= ~qval;
         }
      }
   }

/* Select the quality value that this function should use to flag strange
   bolometer blocks. */
   badqual =  ( flags & SMF__DIMM_PCACOM ) ? SMF__Q_PCA : SMF__Q_COM;

/* Get the number of adjacent time slices for which the same gain should
   be used. */
   smf_get_nsamp( kmap, "GAIN_BOX", res->sdata[ 0 ], &gain_box, status );

/* If the COM flags are to be frozen at any point, we need to find out
   how many initial AST-skipped iterations are being used (the
   COM.FREEZE_FLAGS value does not include any iterations specified by
   AST.SKIP). */
   if( freeze_flags ) {
      int skip;
      astMapGet0A( keymap, "AST", &kamap );
      astMapGet0I( kamap, "SKIP", &skip );
      kamap = astAnnul( kamap );

/* Convert "freeze_flags" from an iteration count into a boolean flag
   indicating if the COM flags are now frozen. We always switch freezing
   on if freeze_flags is less than zero, saince this i show skylopp
   indicates that freezing is required. */
      if( dat->iter == freeze_flags + skip || freeze_flags < 0 ) {
         msgOutiff( MSG__VERB, "", "  %s flagging is now frozen due to "
                   "%s.FREEZE_FLAGS setting.", status, modelname, modelname );
         freeze_flags = 1;
      } else if( dat->iter > freeze_flags + skip ) {
         freeze_flags = 1;
      }
   }

/* Store the length of the second and third pixel axes of the smfData so
   that we can treat the array as a 2D image (note, do not use the
   smf_get_dims ncols and nrows values as these may be swapped). */
   ncol = res->sdata[ 0 ]->dims[ 1 ];
   nrow = res->sdata[ 0 ]->dims[ 2 ];

/* Get the number of time slices in the first residuals smfData. We
   report an error if any subsequent smfData has a different number of
   time slices (except for GAI). */
   smf_get_dims( res->sdata[ 0 ], NULL, NULL, &nbolo, &ntslice, NULL, NULL,
                 NULL, status );
   if( noi ) smf_get_dims( noi->sdata[ 0 ], NULL, NULL, NULL, &nointslice,
                           NULL, NULL, NULL, status );
   if( *status != SAI__OK ) goto L999;

/* If com.gain_box is zero, use a vleu of ntslice, so that a single box
   will be used covering the whoel time stream. */
   if( gain_box == 0 ) gain_box = ntslice;

/* Find the number of blocks of time slices per bolometer. Each block
   contains "gain_box" time slices (except possibly for the last time slice
   which may contain more than gain_box but will be less than 2*gain_box).
   Each block of time slices from a single bolometer has its own gain, offset
   and correlation values. */
   nblock = ntslice/gain_box;
   if( nblock == 0 ) nblock = 1;

/* How many threads do we get to play with */
   nw = wf ? wf->nworker : 1;

/* Find how many time slices to process in each worker thread. */
   timestep = ntslice/nw;
   if( timestep == 0 ) timestep = 1;

/* Find how many bolometers to process in each worker thread. */
   bolostep = nbolo/nw;
   if( bolostep == 0 ) bolostep = 1;

/* Allocate job data for threads, and store the range of times slices,
   bolos, etc, to be processed by each one. Ensure that the last thread
   picks up any left-over time slices, bolos, etc.  */
   job_data = astCalloc( nw, sizeof(*job_data) );
   if( *status == SAI__OK ) {
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->t1 = iw*timestep;
         pdata->b1 = iw*bolostep;
         if( iw < nw - 1 ) {
            pdata->t2 = pdata->t1 + timestep - 1;
            pdata->b2 = pdata->b1 + bolostep - 1;
         } else {
            pdata->t2 = ntslice - 1 ;
            pdata->b2 = nbolo - 1 ;
         }

/* Store other values common to all jobs. */
         pdata->ntslice = ntslice;
         pdata->nointslice = nointslice;
         pdata->nbolo = nbolo;
         pdata->nrow = nrow;
         pdata->ncol = ncol;
         pdata->res = res;
         pdata->noi = noi;
         pdata->gai = gai;
         pdata->lut = lut;
         pdata->model = model;
         pdata->nblock = nblock;
         pdata->gain_box = gain_box;
         pdata->mask = mask;
         pdata->niter = niter;
         pdata->nsigma = nsigma;
         pdata->fill = fill;
         pdata->freeze_flags = freeze_flags;
         pdata->badqual = badqual;
      }
   }

/* See if a single common-mode signal was used for all sub-arrays on the
   previous iteration. */
   astMapGet0I( kmap, "PERARRAY", &perarray );

/* If there is only one sub-array, use com.perarray=1. */
   if( res->ndat == 1 ) perarray = 1;

/* If "perarray" is non-zero, a separate common mode signal was calculated
   for each available subarrays on the previous iteration and is stored as
   a separate 1d vector. If "perarray" is zero, a single common mode signal
   was calculated from all available subarrays and is stored as a 1d vector.
   The corresponding smfData is at position 0 in the model sdata. Store the
   number of COM models to undo. */
   if( perarray ) {
      if( flags & SMF__DIMM_INVERT ) {
         msgOutif( MSG__VERB, "", "  Undoing separate COM models for each array.",
                   status );
      }
      ncom = model->ndat;
      if( (int) res->ndat != ncom && *status == SAI__OK  ) {
         *status = SAI__ERROR;
         errRepf( "", "smf_calcmodel_com: COM model and residuals contain "
                 "different number of data arrays (%d and %zu)!", status,
                 ncom, res->ndat );
      }
   } else {
      if( flags & SMF__DIMM_INVERT ) {
         msgOutif( MSG__VERB, "", "  Undoing a single COM model for all arrays.",
                   status );
      }
      ncom = 1;
   }

/* If we are inverting the the model, loop round undoing each old COM model. */
   if( flags & SMF__DIMM_INVERT ) {

      for( icom = 0; icom < ncom && *status == SAI__OK; icom++ ) {

/* Set the index of the first and last subarray that contributes to the
   current COM model. */
         if( perarray ) {
            idx_lo = icom;
            idx_hi = icom;
         } else {
            idx_lo = 0;
            idx_hi = res->ndat - 1;
         }

/* Set up jobs to add the previous estimate of COM back on to the
   residuals, and then wait for the jobs to complete. These jobs also
   clear any SMF__Q_COM flags set by previous iterations, so long as we
   have not passed the iteration at which these flags are to be frozen
   (which can help convergence). */
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->operation = 1;
            pdata->icom = icom;
            pdata->idx_lo = idx_lo;
            pdata->idx_hi = idx_hi;
            thrAddJob( wf, 0, pdata, smf1_calcmodel_com, 0, NULL, status );
         }
         thrWait( wf, status );

/* If we are using a GAI model, reset gains and correlation coefficients to
   unity and offsets to zero. */
         if( gai ) {
            for( iw = 0; iw < nw; iw++ ) {
               pdata = job_data + iw;
               pdata->operation = 2;
               thrAddJob( wf, 0, pdata, smf1_calcmodel_com, 0, NULL, status );
            }
            thrWait( wf, status );
         }
      }

/* Otherwise, form a new COM model and subtract from the residuals. */
   } else {
      if( weight ) msgOutif( MSG__VERB, "", "  Bolometer data will be "
                             "weighted when forming COM.", status );

/* If this is the last iteration, the user may request a different value
   for COM.PERARRAY by assigning a value ot COM.PERARRAY_LAST. For
   instance, having used com.perarray=0 together with AST masking for all
   earlier iterations in order to retain extended structure, they may want
   to use COM.PERARRAY=1 on the last iteration only, in order to suppress
   background "mottling" in the regions outside the mask. Note, the current
   value of the "perarray" variable will be left unchanged by astMapGet0I
   if PERARRAY_LAST is "<undef>" (the default). */
      if( flags & SMF__DIMM_LASTITER ) astMapGet0I( kmap, "PERARRAY_LAST",
                                                    &perarray );

/* If "perarray" is now non-zero, a separate common mode signal will be
   calculated for each available subarrays on this iteration and will be
   stored as a separate 1d vector. If "perarray" is zero, a single common
   mode signal will be calculated from all available subarrays and will be
   stored as a 1d vector. The corresponding smfData is at position 0 in the
   model sdata. Store the number of COM models to create. */
      if( perarray ) {
         msgOutif( MSG__VERB, "", "  Calculating separate COM models for each array.",
                   status );
         ncom = model->ndat;
         if( (int) res->ndat != ncom && *status == SAI__OK  ) {
            *status = SAI__ERROR;
            errRep( "", "smf_calcmodel_com: COM model and residuals contain "
                    "different number of data arrays!", status);
         }
      } else {
         msgOutif( MSG__VERB, "", "  Calculating a single COM model for all arrays.",
                   status );
         ncom = 1;
      }

/* Loop round creating a new COM model for each sub-array. */
      for( idx = 0; idx < res->ndat && *status == SAI__OK; idx++ ) {
         msgSeti( "I", idx + 1 );
         msgSeti( "N", res->ndat );
         msgOutif( MSG__VERB, "", "  Calculating common-mode signal for array ^I of ^N",
                   status );

/* Choose where to put the comon-mode signal for this sub-array. If
   com.perarray is 1 we use the COM models, otherwise we allocate temporary
   memory for the individual common-mode signals. */
         if( perarray ) {
            com_datas[ idx ] =  model->sdata[ idx ]->pntr[ 0 ];
         } else {
            com_datas[ idx ] =  astMalloc( ntslice*sizeof( **com_datas ) );
         }

/* Form the new common-mode signal. This is just the sigma-clipped mean of the
   residuals at every time slice. */
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->icom = -1;
            pdata->com_data = com_datas[ idx ];
            pdata->idx_lo = idx;
            pdata->idx_hi = idx;
            pdata->operation = 3;
            thrAddJob( wf, 0, pdata, smf1_calcmodel_com, 0, NULL, status );
         }
         thrWait( wf, status );

      }

/* If com.perarray is zero (i.e. one shared COM model for all
   sub-arrays), we now combined the individual common-mode signals into a
   single mean COM model. */
      if( ! perarray ) {

/* Form the mean common-mode signal, placing it in model->sdata[0]. */
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->com_datas = com_datas;
            pdata->idx_lo = 0;
            pdata->idx_hi = res->ndat - 1;
            pdata->operation = 4;
            thrAddJob( wf, 0, pdata, smf1_calcmodel_com, 0, NULL, status );
         }
         thrWait( wf, status );

/* Test if the following filtering has been requested. We do not do this
   filtering if the COM flagging is frozen. */
         if( sig_limit > 0.0 && !freeze_flags ) {

/* We now attempt to flag times slices in the above COM model that
   correspond to times when the high frequency component of the common-mode
   signal is very different in each sub-arrays. If the high-frequency
   structure of the individual common-modes are different, then forcing
   a single mean common-mode to be subtracted from all subarrays will
   introduce strong high frequency structure into the residuals, which
   will get through the FLT filter (maybe causing ringing) and end up in
   the map as blobs. We first smooth each individual common-mode signal
   using a highpass filter. First create the filter. */
            astMapGet0A( keymap, "FLT", &kfmap );
            filt = smf_create_smfFilter( res->sdata[0], status );
            smf_filter_fromkeymap( filt, kfmap,
                                   (flags & SMF__DIMM_LASTITER) ? "_LAST" : NULL,
                                   res->sdata[0]->hdr, &dofft, &whiten, status );

/* Get the filter wisthd, in samples. */
            smf_filter_getlowf( filt, res->sdata[ 0 ]->hdr, &period, status );

/* Allocate an array to hold the low frequency component of a single
   common-mode signal. */
            lof = astMalloc( ntslice*sizeof( *lof ) );

/* Use this filter to fiter the common-mode signal for each sub-array. We
   temporarily hijack the model smfData. */
            old_model = model->sdata[ 0 ]->pntr[ 0 ];
            model->sdata[ 0 ]->pntr[ 0 ] = lof;
            for( idx = 0; idx < res->ndat && *status == SAI__OK; idx++ ) {

/* Copy the common-mode signal to the "lof" array. */
               memcpy( lof, com_datas[ idx ], ntslice*sizeof( *lof ) );

/* Filter the "lof" array to retain just the low frequencies. */
               smf_filter_execute( wf, model->sdata[ 0 ], filt, -1, whiten, status );

/* Remove the low frequencies from the total common-mode signal, to
   leave just the high frequencies. */
               p1 = lof;
               p2 = com_datas[ idx ];
               for( itime = 0; itime < ntslice; itime++,p1++,p2++ ){
                  if( *p1 != VAL__BADD && *p2 != VAL__BADD ) {
                     *p2 -= *p1;
                  } else {
                     *p2 = VAL__BADD;
                  }
               }
            }

/* Re-instate the data array pointer in the model smfData. */
            model->sdata[ 0 ]->pntr[ 0 ] = old_model;

/* Free the filter, etc. */
            filt = smf_free_smfFilter( filt, status );
            kfmap = astAnnul( kfmap );
            lof = astFree( lof );

/* Now form the standard deviation at each time slice of the high
   frequency common-mode signals. The resulting sigma values are left in
   com_datas[ 0 ]. */
            for( iw = 0; iw < nw; iw++ ) {
               pdata = job_data + iw;
               pdata->operation = 6;
               thrAddJob( wf, 0, pdata, smf1_calcmodel_com, 0, NULL, status );
            }
            thrWait( wf, status );

/* Find the total of the variances, and the number of variances. */
            nvar = 0;
            svar = 0.0;
            for( iw = 0; iw < nw; iw++ ) {
               pdata = job_data + iw;
               nvar += pdata->nvar;
               svar += pdata->svar;
            }

/* Find the square root of the mean variance, and convert it to a
   threshold to apply to the stanadrad deviations array. */
            sig_limit *= sqrt( svar/nvar );

/* Convert the "sig_wing" parameter from a multiple of the filter width
   to a number of samples. */
            sig_wing = (int)( sig_wing*period + 0.5 );
            if( sig_wing < 1 ) sig_wing = 1;

/* Flag all time slices for which the standard deviation of the individual
   common-mode signals (the high frequency part) exceeds the above limit. */
            for( iw = 0; iw < nw; iw++ ) {
               pdata = job_data + iw;
               pdata->operation = 7;
               pdata->idx_lo = 0;
               pdata->idx_hi = res->ndat - 1;
               pdata->limit = sig_limit;
               pdata->wing = sig_wing;
               thrAddJob( wf, 0, pdata, smf1_calcmodel_com, 0, NULL, status );
            }
            thrWait( wf, status );

            nb = 0;
            for( iw = 0; iw < nw; iw++ ) {
               pdata = job_data + iw;
               nb += pdata->nb;
            }

            msgOutiff( MSG__DEBUG, "", "  %zu timeslices set bad due to high dispersion "
                       "between common-mode signals.", status, nb );
         }

/* Free resourcess. */
         for( idx = 0; idx < res->ndat && *status == SAI__OK; idx++ ) {
            com_datas[ idx ] =  astFree( com_datas[ idx ] );
         }

      }

/* If we are using a GAI model, evaluate the gains and offsets of each
   bolometer block by doing a least squares linear fit between the residuals
   in the bolometer block, and the current estimate of COM. Additionally
   flag time slices within unusual bolometer blocks with the SMF_Q_COM flag.
   The correlation coefficient for such blocks is set to VAL__BADD. */
      for( icom = 0; icom < ncom && *status == SAI__OK; icom++ ) {
         if( gai ) {
            int gai_flags = freeze_flags ? 7 : 6;

            if( perarray ) {
               smf_find_gains( wf, gai_flags, res->sdata[ icom ], mask,
                               lut ? lut->sdata[ icom ] : NULL,
                               model->sdata[ icom ]->pntr[0], kmap,
                               ( SMF__Q_GOOD & ~SMF__Q_RING ),
                               badqual, gai->sdata[ icom ], NULL, status );
            } else {
               smf_find_gains_array( wf, gai_flags, res, mask, lut,
                                     model->sdata[ icom ]->pntr[0], kmap,
                                     ( SMF__Q_GOOD & ~SMF__Q_RING ),
                                     badqual, gai, NULL, status );
            }
         }

/* Set the index of the first and last subarray that contributes to the
   current COM model. */
         if( perarray ) {
            idx_lo = icom;
            idx_hi = icom;
         } else {
            idx_lo = 0;
            idx_hi = res->ndat - 1;
         }

/* Subtract the COM estimate from every bolometer (or if SMF__DIMM_PCACOM
   flag is set, use the COM model to fill gaps in every bolometer). */
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->icom = icom;
            pdata->idx_lo = idx_lo;
            pdata->idx_hi = idx_hi;
            pdata->operation = ( flags & SMF__DIMM_PCACOM ) ? 8 : 5;

            thrAddJob( wf, 0, pdata, smf1_calcmodel_com, 0, NULL, status );
         }
         thrWait( wf, status );
      }
   }

/* Free resources allocated in this function. */
L999:
   job_data = astFree( job_data );

/* End the AST context, thus deleting any AST objects created in this
   function. */
   astEnd;

/* The kmap pointer was allocated before the ATS context was started and
   so needs to be annulled explicitly. */
   kmap = astAnnul( kmap );
}

static void smf1_calcmodel_com( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_calcmodel_com

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_calmodel_com.

*  Invocation:
*     smf1_calcmodel_com( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfCalcModelComData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfCalcModelComData *pdata;
   dim_t ibolo;
   dim_t idx;
   dim_t itime;
   dim_t jlim;
   dim_t jtime;
   dim_t nbolo;
   dim_t ntslice;
   dim_t nvar;
   double *fillwork = NULL;
   double *gai_data;
   double *model_data;
   double *pb;
   double *pm;
   double *pmi[ 4 ];
   double *pn;
   double *pr;
   double *pw;
   double *pwg;
   double *pwoff;
   double *res_data;
   double *resbuf;
   double *wg;
   double *wgtbuf = NULL;
   double *woff;
   double modelvalue;
   double s1;
   double s2;
   double svar;
   double v;
   int *lut_data;
   int *pl;
   int iblock;
   int masked;
   int ns1;
   int state;
   size_t gbstride;
   size_t gcstride;
   size_t izero;
   smf_qual_t *pq0[ 4 ];
   smf_qual_t *pq;
   smf_qual_t *qua_data;
   smf_qual_t badqual;
   smf_qual_t qmask;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfCalcModelComData *) job_data_ptr;

   badqual = pdata->badqual;

/* Add or remove the previous estimate of COM back to/from the residuals.
   Or use the COM model to fill gaps in the residuals (operation 8).
   ================================================================== */
   if( pdata->operation == 1 || pdata->operation == 5 || pdata->operation == 8 ) {

/* Allocate work space */
      wg = astMalloc( ( pdata->t2 - pdata->t1 + 1 )*sizeof( *wg ) );
      woff = astMalloc( ( pdata->t2 - pdata->t1 + 1 )*sizeof( *woff ) );

/* Get a pointer to the model data. The same one is used for all
   subarrays from idx_lo to idx_hi. */
      model_data = pdata->model->sdata[ pdata->icom ]->pntr[ 0 ];

/* Do each required subarray. */
      for( idx = pdata->idx_lo; idx <= pdata->idx_hi; idx++ ) {

/* Get the number of bolometers and time slices for this subarray. */
         smf_get_dims( pdata->res->sdata[ idx ],  NULL, NULL, &nbolo, &ntslice,
                       NULL, NULL, NULL, status );

/* Report an error if the number of time slices or bolometers is not the
   same as for the first array. */
         if( ntslice != pdata->ntslice && *status == SAI__OK ) {
            errRepf( "", "smf1_calcmodel_com: array idx=%d has %d time slices "
                     "- expecting %d.", status, (int) idx, (int) ntslice,
                     (int) pdata->ntslice );
            break;
         } else if( nbolo != pdata->nbolo && *status == SAI__OK ) {
            errRepf( "", "smf1_calcmodel_com: array idx=%d has %d bolometer "
                     "- expecting %d.", status, (int) idx, (int) nbolo,
                     (int) pdata->nbolo );
            break;
         }

/* Get required data pointers. These are known to be bolo-ordered. */
         res_data = pdata->res->sdata[ idx ]->pntr[ 0 ];
         qua_data = smf_select_qualpntr( pdata->res->sdata[ idx ], NULL, status );
         gai_data = pdata->gai ? pdata->gai->sdata[ idx ]->pntr[ 0 ] : NULL;
         lut_data = pdata->lut ? pdata->lut->sdata[ idx ]->pntr[ 0 ] : NULL;

/* Get the strides in the GAI model. */
         if( gai_data ) {
            smf_get_dims( pdata->gai->sdata[ idx ],  NULL, NULL, NULL, NULL,
                          NULL, &gbstride, &gcstride, status );
         } else {
            gbstride = 0;
            gcstride = 0;
         }

/* Loop over all bolometers. */
         izero = 0;
         for( ibolo = 0; ibolo < nbolo; ibolo++ ) {

/* Skip bad bolometers */
            if( !(qua_data[ izero ] & SMF__Q_BADB ) ) {

/* Get the gain and offset for each time slice being processed by this
   thread. They are returned in "wg" and "woff". */
               smf_gandoff( ibolo, pdata->t1, pdata->t2, ntslice, gbstride,
                            gcstride, gai_data, pdata->nblock, pdata->gain_box,
                            wg, woff, NULL, status );

/* Get pointers to the first time slice to be processed by this thread,
   for the current bolometer. */
               pr = res_data + izero + pdata->t1;
               pq = qua_data + izero + pdata->t1;
               pl = lut_data ? lut_data + izero + pdata->t1 : NULL;
               pm = model_data + pdata->t1;
               pwg = wg;
               pwoff = woff;

/* Loop over the time slices to be processed by this thread. */
               for( itime = pdata->t1; itime <= pdata->t2;
                    itime++,pr++,pq++,pm++,pwg++,pwoff++,pl++ ) {

/* Scale the common mode using the gain and offset for the current
   bolometer to get the COM model value. */
                  if( (*pm) != VAL__BADD ) {
                     modelvalue = (*pwg)*(*pm) + (*pwoff);

/* If required, fill flagged residual samples using the COM model. */
                     if( pdata->operation == 8 ) {
                        masked = ( lut_data && *pl != VAL__BADI &&
                                   pdata->mask && !pdata->mask[ *pl ] );
                        if( *pq || masked ) (*pr) = modelvalue;

/* Add the COM model value back onto the residuals. We include samples that
   have been flagged by this function as "unusual" on previous iterations.
   Note, SMF__Q_MOD does not include SMF__Q_COM). */
                     } else if( !( (*pq) & SMF__Q_MOD ) ) {
                        if( pdata->operation == 1 ) {
                           (*pr) += modelvalue;

/* If we have not yet passed the iteration at which COM flagging is
   frozen, ensure no samples have the SMF__Q_COM flag. */
                           if( !pdata->freeze_flags ) (*pq) &= ~badqual;

/* Subtract the COM model value form the residuals. */
                        } else {
                           (*pr) -= modelvalue;
                        }
                     }
                  }
               }
            }

/* Increment the index to sample zero in the next bolometer. */
            izero += ntslice;
         }
      }

/* Free works space. */
      wg = astFree( wg );
      woff = astFree( woff );

/* Reset GAI model to default value.
   =============================== */
   } else if( pdata->operation == 2 ) {

      for( idx = pdata->idx_lo; idx <= pdata->idx_hi; idx++ ) {
         gai_data = pdata->gai->sdata[ idx ]->pntr[ 0 ];
         gai_data += pdata->b1*pdata->nblock*3;
         for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {
            for( iblock = 0; iblock < pdata->nblock; iblock++ ) {
               *(gai_data++) = 1.0; /* Gain */
            }

            for( iblock = 0; iblock < pdata->nblock; iblock++ ) {
               *(gai_data++) = 0.0; /* Offset */
            }

            for( iblock = 0; iblock < pdata->nblock; iblock++ ) {
               *(gai_data++) = 1.0; /* Correlation coefficient */
            }

         }
      }

/* Form new estimate of the COM model.
   ================================== */
   } else if( pdata->operation == 3 ) {
      double sigma;
      double thr_hi;
      double thr_lo;
      int iter;

/* Allocate work space needed for filling holes in each time slices. */
      if( pdata->fill ) fillwork = astMalloc( 2*pdata->ncol*pdata->nrow*sizeof(*fillwork ) );

/* Quality mask that includes samples previously flagged by the ringing
   filter. */
      qmask = ( SMF__Q_FIT & ~SMF__Q_RING );

/* Initialise a pointer to the next model value. */
      if( pdata->icom >= 0 ) {
         pm = pdata->model->sdata[ pdata->icom ]->pntr[ 0 ];
      } else {
         pm = pdata->com_data;
      }
      pm += pdata->t1;

/* Buffer to hold all bolometer residuals at a single time slice. */
      resbuf = astMalloc( pdata->nbolo*( pdata->idx_hi - pdata->idx_lo + 1 )*
                          sizeof( *resbuf ) );

/* Buffer to hold all bolometer weights at a single time slice. */
      if( pdata->noi ) {
         wgtbuf = astMalloc( pdata->nbolo*( pdata->idx_hi - pdata->idx_lo + 1 )*
                             sizeof( *wgtbuf ) );
      }

/* Loop over the time slices to be processed by this thread. */
      for( itime = pdata->t1; itime <= pdata->t2 && *status == SAI__OK;
           itime++,pm++ ) {

/* Initialise the thresholds to include all bolometer values. */
         thr_lo = VAL__MIND;
         thr_hi = VAL__MAXD;

/* Do any required sigma-clipping iterations. */
         for( iter = 0; iter < pdata->niter; iter++ ) {

/* Initialise pointers to the buffers holding the normalised residual,
   and the weights. */
            pb = resbuf;
            pw = wgtbuf;

/* Loop over all subarrays that contribute to the current common-mode
   model. */
            for( idx = pdata->idx_lo; idx <= pdata->idx_hi; idx++ ) {

/* Get required data pointers. These are known to be bolo-ordered.
   Increment them to point to the first time slice processed by this thread. */
               pr = pdata->res->sdata[ idx ]->pntr[ 0 ];
               pr += itime;
               pn = ( pdata->noi ) ? pdata->noi->sdata[ idx ]->pntr[ 0 ] : NULL;
               if( pn ) pn += (itime % pdata->nointslice);
               pq = itime + smf_select_qualpntr( pdata->res->sdata[ idx ], NULL, status );
               if( pdata->lut ) {
                  pl = pdata->lut->sdata[ idx ]->pntr[ 0 ];
                  pl += itime;
               } else {
                  pl = NULL;
               }

/* Loop over all bolometers. */
               for( ibolo = 0; ibolo < pdata->nbolo; ibolo++ ) {

/* Check the sample has not been flagged as unusable. */
                  if( !(*pq & qmask) && *pr != VAL__BADD &&
                      ( !pn || ( *pn != VAL__BADD && *pn > 0.0 ) ) ) {

/* If a mask and LUT have been supplied, check that the sample is not
   masked out. */
                     if( !pdata->mask || !pl || *pl == VAL__BADI ||
                          pdata->mask[ *pl ] ) {

/* Check that the bolometer value is within the current clipping limits.
   If so, store it in the sample buffer. */
                        if( *pr >= thr_lo && *pr <= thr_hi ) {
                           *(pb++) = *pr;

/* Also store the weight if required. */
                           if( wgtbuf ) *(pw++) = 1.0/( (*pn)*(*pn) );

/* If required store a magic value in the buffer that indicates that the
   bolometer value needs to be replaced by interpolation from the
   surrounding spatial neighbours. Entirely bad bolometers are excluded
   from this process. */
                        } else if( pdata->fill ) {
                           *(pb++) = FILLVAL;
                           if( wgtbuf ) *(pw++) = FILLVAL;
                        }

                     } else if( pdata->fill ) {
                        *(pb++) = FILLVAL;
                        if( wgtbuf ) *(pw++) = FILLVAL;
                     }

                  } else if( pdata->fill ){
                     if( !(*pq & SMF__Q_BADB) ) {
                        *(pb++) = FILLVAL;
                        if( wgtbuf ) *(pw++) = FILLVAL;
                     } else {
                        *(pb++) = VAL__BADD;
                        if( wgtbuf ) *(pw++) = VAL__BADD;
                     }
                  }

/* Increment residual and quality pointers to point to the next
   bolometer. */
                  pr += pdata->ntslice;
                  if( pn ) pn += pdata->nointslice;
                  if( pl ) pl += pdata->ntslice;
                  pq += pdata->ntslice;
               }

/* If required, replace bad values in the buffer by interpolation from their
   spatial neighbours. This avoids bias in the COM value due to the
   spatial distribution of flagged bolometer values. */
               if( pdata->fill ) {
                  smf_fill2d( 50, 5, FILLVAL, pdata->ncol, pdata->nrow,
                              resbuf + ( idx - pdata->idx_lo )*pdata->nbolo,
                              fillwork, status );
                  if( wgtbuf ) {
                     smf_fill2d( 50, 5, FILLVAL, pdata->ncol, pdata->nrow,
                                 wgtbuf + ( idx - pdata->idx_lo )*pdata->nbolo,
                                 fillwork, status );
                  }
               }
            }

/* Find the mean and sigma of the samples now in the buffer. */
            *pm = smf_sigmaclipD( (int)( pb - resbuf ), resbuf, wgtbuf, 0.0,
                                  1, &sigma, status );

/* Update the thresholds for the next iteration. */
            thr_lo = *pm - pdata->nsigma*sigma;
            thr_hi = *pm + pdata->nsigma*sigma;
         }
      }

/* Free resources. */
      resbuf = astFree( resbuf );
      wgtbuf = astFree( wgtbuf );
      fillwork = astFree( fillwork );

/* Combined individual common-mode signals into a single COM model.
   ============================================================== */
   } else if( pdata->operation == 4 ) {

/* Initialise a pointer to the next output model value. */
      pm = pdata->model->sdata[ 0 ]->pntr[ 0 ];
      pm += pdata->t1;

/* Initialise pointers to the next input model value for each subarray. */
      for( idx = pdata->idx_lo; idx <= pdata->idx_hi; idx++ ) {
         pmi[ idx ]  = pdata->com_datas[ idx ];
         pmi[ idx ] += pdata->t1;
      }

/* Loop over the time slices to be processed by this thread. */
      for( itime = pdata->t1; itime <= pdata->t2 && *status == SAI__OK;
           itime++ ) {

/* Initialise running sums. */
         s1 = 0.0;
         ns1 = 0;

/* Loop over all sub-arrays, adding good values into the running sums. */
         for( idx = pdata->idx_lo; idx <= pdata->idx_hi; idx++ ) {
            v = *(pmi[ idx ]++);
            if( v != VAL__BADD ) {
               s1 += v;
               ns1++;
            }
         }

/* Store the mean. */
         *(pm++) = ns1 ? s1/ns1 : VAL__BADD;
      }


/* Find the standard deviation of the high frequency components of the
   separate common-mode signals.
   ============================================================== */
   } else if( pdata->operation == 6 ) {

/* Initialise the um of the variances and the number of variances. */
      nvar =0;
      svar = 0.0;

/* Initialise a pointer to the next output standard deviation value. */
      pm  = pdata->com_datas[ 0 ] + pdata->t1;

/* Initialise pointers to the next input model value for each subarray. */
      for( idx = pdata->idx_lo; idx <= pdata->idx_hi; idx++ ) {
         pmi[ idx ]  = pdata->com_datas[ idx ] + pdata->t1;
      }

/* Loop over the time slices to be processed by this thread. */
      for( itime = pdata->t1; itime <= pdata->t2 && *status == SAI__OK;
           itime++ ) {

/* Initialise running sums. */
         s1 = 0.0;
         s2 = 0.0;
         ns1 = 0;

/* Loop over all sub-arrays, adding good values into the running sums. */
         for( idx = pdata->idx_lo; idx <= pdata->idx_hi; idx++ ) {
            v = *(pmi[ idx ]++);
            if( v != VAL__BADD ) {
               s1 += v;
               s2 += v*v;
               ns1++;
            }
         }

/* Store the standard deviation, and increment the statistics for the
   variance values. */
         if( ns1 > 1 ) {
            s1 /= ns1;
            s2 = s2/ns1 - s1*s1;

            if( s2 > 0.0 ) {
               *(pm++) = sqrt( s2 );
               svar += s2;

            } else {
               *(pm++) = 0.0;
            }

            nvar++;

         } else {
            *(pm++) = VAL__BADD;
         }
      }

/* Return the sum of the variances and the number of variances. */
      pdata->svar = svar;
      pdata->nvar = nvar;

/* Flag time slices with high dispersion between the individual
   common-mode signals.
   ============================================================== */
   } else if( pdata->operation == 7 ) {
      dim_t nb = 0;

/* Initialise a pointer to the next dispersion value. */
      pm  = pdata->com_datas[ 0 ] + pdata->t1;

/* Store a pointer to the first COM model value value. */
      model_data = pdata->model->sdata[ 0 ]->pntr[ 0 ];

/* Pointers to the quality values for each subarray. */
      for( idx = pdata->idx_lo; idx <= pdata->idx_hi; idx++ ) {
         pq0[ idx ] = smf_select_qualpntr( pdata->res->sdata[ idx ], NULL,
                                           status );
      }

/* Set a flag if we are in a block of high-dispersion samples. */
      state = 0;

/* Loop over the time slices to be processed by this thread. */
      for( itime = pdata->t1; itime <= pdata->t2 && *status == SAI__OK;
           itime++,pm++ ) {

/* If we have just entered a block of high-dispersion time slices... */
         if( state == 0 ) {
            if( *pm != VAL__BADD && *pm > pdata->limit ) {
               state = 1;
               nb++;

/* Ensure all the previous "wing" samples are also bad. */
               if( itime > pdata->wing ) {
                  jtime = itime - pdata->wing;
               } else {
                  jtime = 0;
               }
               if( jtime < pdata->t1 ) jtime = pdata->t1;
               for( ; jtime <= itime; jtime++ ) {

                  for( idx = pdata->idx_lo; idx <= pdata->idx_hi; idx++ ) {
                     pq = pq0[ idx ] + jtime;
                     for( ibolo = 0; ibolo < pdata->nbolo; ibolo++ ) {
                        *pq |= badqual;
                        pq += pdata->ntslice;
                     }
                  }

               }
            }

/* If we have just left a block of high-dispersion time slices... */
         } else {
            if( *pm != VAL__BADD && *pm <= pdata->limit ) {
               state = 0;

/* Ensure all the next "wing" samples are also bad. */
               jlim = itime + pdata->wing - 1;
               if( jlim > pdata->t2 ) jlim = pdata->t2;
               for( jtime = itime; jtime <= jlim; jtime++ ) {
                  for( idx = pdata->idx_lo; idx <= pdata->idx_hi; idx++ ) {
                     pq = pq0[ idx ] + jtime;
                     for( ibolo = 0; ibolo < pdata->nbolo; ibolo++ ) {
                        *pq |= badqual;
                        pq += pdata->ntslice;
                     }
                  }
               }

/* If we are in the middle of a block of high-dispersion time slices... */
            } else {
               nb++;
               for( idx = pdata->idx_lo; idx <= pdata->idx_hi; idx++ ) {
                  pq = pq0[ idx ] + itime;
                  for( ibolo = 0; ibolo < pdata->nbolo; ibolo++ ) {
                     *pq |= badqual;
                     pq += pdata->ntslice;
                  }
               }
            }
         }
      }

      pdata->nb = nb;

/* Report an error if the worker was to do an unknown job.
   ====================================================== */
   } else {
      *status = SAI__ERROR;
      errRepf( "", "smf1_calcmodel_com: Invalid operation (%d) supplied.",
               status, pdata->operation );
   }
}








