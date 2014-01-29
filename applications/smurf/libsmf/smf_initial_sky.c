/*
*+
*  Name:
*     smf_initial_sky

*  Purpose:
*     Subtract the initial sky estimate from the cleaned data.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_initial_sky( ThrWorkForce *wf, AstKeyMap *keymap,
*                          smfDIMMData *dat, int *iters, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     keymap = AstKeyMap * (Given)
*        Configuration parameters that control the map-maker.
*     dat = smfDIMMData * (Given)
*        Struct of pointers to information required by model calculation.
*     iters = int * (Returned)
*        If the initial sky NDF was created by a previous run of makemap
*        that was interupted using control-C, "*iters" will be returned
*        holding the number of iterations that were completed before the
*        map was created. Otherwise, -1 is returned in "*iters".
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     Non-zero if an initial sky estimate was subtracted from the data.
*     Zero otherwise.

*  Description:
*     If the IMPORTSKY configuration parameter is set, this function
*     reads a sky map in using the ADAM parameter specified by the
*     IMPORTSKY. It then stores this map as the current map in the
*     iterative map-making process. It then samples the map at each
*     bolometer position and subtracts the bolometer value from the
*     current residuals.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     22-OCT-2012 (DSB):
*        Original version.
*     3-JUL-2013 (DSB):
*        - Added argument iters.
*        - Ensure the bad bits mask is set so that the mapped NDF data
*        array is masked by the quality array.
*        - Import variance from supplied NDF if available.
*     10-DEC-2013 (DSB):
*        - Re-structured to avoid setting map pixels bad if they are
*        flagged only by FLT or COM masking. That is, map pixels should
*        be bad if and only if they are flagged by AST masking.
*        - Report an error if the NDF contains unexpected quality names.
*        - Clear and RING or COM flags in the quality array of the time series data.
*     23-JAN-2014 (DSB):
*        Mask the AST map in smf_calcmodel_ast as normal, rather than masking it here.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012-2013 Science & Technology Facilities Council.
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
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "dat_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"

/* Local data types */
typedef struct smfInitialSkyData {
   dim_t b1;
   dim_t b2;
   dim_t ntslice;
   size_t bstride;
   size_t tstride;
   smf_qual_t *qua_data;
} SmfInitialSkyData;

/* Prototypes for local static functions. */
static void smf1_initial_sky( void *job_data_ptr, int *status );

void xxdump( const char *text, smfData *data, int *status );

int smf_initial_sky( ThrWorkForce *wf, AstKeyMap *keymap, smfDIMMData *dat,
                     int *iters, int *status ) {

/* Local Variables: */
   SmfInitialSkyData *job_data = NULL; /* Data describing worker jobs */
   SmfInitialSkyData *pdata = NULL; /* Data describing worker jobs */
   char refparam[ DAT__SZNAM ];/* Name for reference NDF parameter */
   const char *cval;          /* The IMPORTSKY string value */
   dim_t bolostep;            /* Number of bolos per thread */
   dim_t idx;                 /* Index within subgroup */
   dim_t nbolo;               /* Number of bolometers */
   dim_t ntslice;             /* Number of time slices */
   double *ptr;               /* Pointer to NDF Data array */
   double *vptr;              /* Pointer to NDF Variance array */
   int indf1;                 /* Id. for supplied reference NDF */
   int indf2;                 /* Id. for used section of reference NDF */
   int iw;                    /* Thread index */
   int nel;                   /* Number of mapped NDF pixels */
   int nw;                    /* Number of worker threads */
   int result;                /* Returned flag */
   int there;                 /* Is there a smurf extension in the NDF? */
   size_t bstride;            /* bolo stride */
   size_t size;               /* Size of mapped array */
   size_t tstride;            /* Time slice stride in data array */
   smfArray *qua=NULL;        /* Pointer to QUA at chunk */
   smf_qfam_t family;         /* The family of quality flags found in the NDF */
   smf_qual_t *qptr;          /* Pointer to mapped quality values */
   smf_qual_t *qua_data=NULL; /* Pointer to quality data */

/* Initialise the returned value to indicate no sky has been subtractred. */
   result = 0;

/* Assume the sky map was not created by an interupted previous run of
   makemap. */
   *iters = -1;

/* Check inherited status. */
   if( *status != SAI__OK ) return result;

/* Begin an AST context. */
   astBegin;

/* The IMPORTSKY config parameter should have the name of the ADAM
   parameter to use for acquiring the NDF that contains the initial sky
   estimate. If IMPORTSKY is "1", use REF. */
   cval = NULL;
   astMapGet0C( keymap, "IMPORTSKY", &cval );
   if( cval ) {
      if( !astChrMatch( cval, "REF" ) &&
          !astChrMatch( cval, "MASK2" ) &&
          !astChrMatch( cval, "MASK3" ) ) {
         astMapGet0I( keymap, "IMPORTSKY", &result );
         cval = ( result > 0 ) ? "REF" : NULL;
      }
      if( cval ) {
         result = 1;
         strcpy( refparam, cval );
         astChrCase( NULL, refparam, 1, 0 );
      }
   }

/* Do nothing more if we are not subtracting an initial sky from the data. */
   if( result ) {

/* Begin an NDF context. */
      ndfBegin();

/* Get an identifier for the NDF using the associated ADAM parameter. */
      ndfAssoc( refparam, "READ", &indf1, status );

/* Tell the user what we are doing. */
      ndfMsg( "N", indf1 );
      msgOut( "", "Using ^N as the initial guess at the sky", status );

/* Get a section from this NDF that matches the bounds of the map. */
      ndfSect( indf1, 2, dat->lbnd_out, dat->ubnd_out, &indf2, status );

/* Map the quality array section, and copy it into the map buffer. */
      ndfState( indf2, "QUALITY", &there, status );
      if( there ) {
         qptr = smf_qual_map( wf, indf2, "READ", &family, &size, status );
         if( *status == SAI__OK ) {
            if( family != SMF__QFAM_MAP ) {
               *status = SAI__ERROR;
               ndfMsg( "N", indf1 );
               errRep( "", "Don't know how to interpret the quality flags "
                       "in the initial sky NDF (^N).", status );
            } else {
               memcpy( dat->mapqual, qptr, dat->msize*sizeof(*qptr));

/* Also copy it into another array where it can be accessed by smf_get_mask. We
   only need to do this once. */
               if( ! dat->initqual ) dat->initqual = astStore( NULL, qptr,
                                                               dat->msize*sizeof(*qptr));
            }
         }
         qptr = smf_qual_unmap( wf, indf2, family, qptr, status );

      } else {
         qptr = NULL;
      }

/* Ensure masked values are not set bad in the mapped data array. */
      ndfSbb( 0, indf2, status );

/* Map the data array section, and copy it into the map buffer. */
      ndfMap( indf2, "DATA", "_DOUBLE", "READ", (void **) &ptr, &nel, status );
      if( *status == SAI__OK ) {
         memcpy( dat->map, ptr, dat->msize*sizeof(*ptr));
      }

/* Map the variance array section, and copy it into the map buffer. */
      ndfState( indf2, "VARIANCE", &there, status );
      if( there ) {
         ndfMap( indf2, "VARIANCE", "_DOUBLE", "READ", (void **) &vptr, &nel, status );
         if( *status == SAI__OK ) {
            memcpy( dat->mapvar, vptr, dat->msize*sizeof(*vptr));
         }
      }

/* If the NDF was created by a previous run of makemap that was interupted
   using control-C, it will contain a NUMITER item in the smurf extension,
   which gives the number of iterations that were completed before the
   map was created. Obtain and return this value, if it exists. */
      ndfXstat( indf1, SMURF__EXTNAME, &there, status );
      if( there ) ndfXgt0i( indf1, SMURF__EXTNAME, "NUMITER", iters,
                            status );

/* End the NDF context. */
      ndfEnd( status );

/* Apply any existinction correction to the cleaned bolometer data. */
      if( dat->ext ) smf_calcmodel_ext( wf, dat, 0, keymap, dat->ext, 0,
                                        status);

/* Sample the above map at the position of each bolometer sample and
   subtract the sampled value from the cleaned bolometer value. */
      smf_calcmodel_ast( wf, dat, 0, keymap, NULL, SMF__DIMM_PREITER, status);

/* Remove any existinction correction to the modifed bolometer data. */
      if( dat->ext ) smf_calcmodel_ext( wf, dat, 0, keymap, dat->ext,
                                        SMF__DIMM_INVERT, status);

/* Now clear quality flags SMF__Q_COM and SMF__Q_RING in the bolometer data.
   This is usually done when undoing the COM model. But the COM model is  not
   undone at the start of the first iteration, so when running skyloop (which
   only performs one iteration on each invocation of makemap) we need
   to clear the flags here instead. How many threads do we get to play with */
      nw = wf ? wf->nworker : 1;

/* Allocate job data for threads. */
      job_data = astCalloc( nw, sizeof(*job_data) );

/* Process each sub-array in turn. */
      qua = dat->qua[ 0 ];
      for( idx = 0; idx < qua->ndat; idx++ ) {

/* Obtain dimensions of the data */
         smf_get_dims( qua->sdata[ idx ],  NULL, NULL, &nbolo, &ntslice,
                       NULL, &bstride, &tstride, status);

/* Get pointers to quality values. */
         qua_data = ( qua->sdata[ idx ]->pntr )[0];

/* Find how many bolometers to process in each worker thread. */
         bolostep = nbolo/nw;
         if( bolostep == 0 ) bolostep = 1;

/* Store information for use by the worker threads. */
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->b1 = iw*bolostep;
            if( iw < nw - 1 ) {
               pdata->b2 = pdata->b1 + bolostep - 1;
            } else {
               pdata->b2 = nbolo - 1 ;
            }

            pdata->ntslice = ntslice;
            pdata->qua_data = qua_data;
            pdata->bstride = bstride;
            pdata->tstride = tstride;
            thrAddJob( wf, 0, pdata, smf1_initial_sky, 0, NULL, status );
         }

         thrWait( wf, status );
      }

      job_data = astFree( job_data );
   }

/* End the AST context. */
   astEnd;

/* Return the pointer to the boolean mask. */
   return result;
}

static void smf1_initial_sky( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_initial_sky

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_initial_sky.

*  Invocation:
*     smf1_initial_sky( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfInitialSkyData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfInitialSkyData *pdata;
   dim_t ibolo;
   dim_t itime;
   size_t ibase;
   smf_qual_t *pq;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfInitialSkyData *) job_data_ptr;

/* Loop round all bolos to be processed by this thread, maintaining the
   index of the first time slice for the current bolo. */
   ibase = pdata->b1*pdata->bstride;
   for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {

/* Check that the whole bolometer has not been flagged as bad. */
      pq = pdata->qua_data + ibase;
      if( !( *pq & SMF__Q_BADB ) ) {

/* Loop round all time slices. */
         for( itime = 0; itime < pdata->ntslice; itime++ ) {

/* Clear any SMF__Q_RING or SMF__Q_COM flags. */
            *pq &= ( ~SMF__Q_RING & ~SMF__Q_COM );

/* Move on to the next time slice. */
            pq += pdata->tstride;
         }
      }

/* Increment the index of the first value associated with the next
   bolometer. */
      ibase += pdata->bstride;
   }

}

