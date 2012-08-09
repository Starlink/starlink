/*
*+
*  Name:
*     smf_resampmap

*  Purpose:
*     Resample a supplied 2D array into a SCUBA-2 time series cube.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_resampmap( ThrWorkForce *wf, smfData *data, AstSkyFrame *abskyfrm,
*                     AstMapping *sky2map, int moving,
*                     int slbnd[ 2 ], int subnd[ 2 ], int interp,
*                     const double params[], double sigma, double *in_data,
*                     double *out_data, int *ngood, int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads that will do the re-binning.
*     data = smfData * (Given)
*        Pointer to the smfData structure describing the template time
*        series file. Must be time-ordered (see smf_dataOrder.c).
*     abskyfrm = AstSkyFrame * (Given)
*        A SkyFrame that specifies the coordinate system used to describe
*        the spatial axes of the input sky map. This should represent
*        absolute sky coordinates rather than offsets even if "moving" is
*        non-zero.
*     sky2map = AstFrameSet * (Given)
*        A Mapping from 2D sky coordinates in the sky map to 2D spatial
*        grid coordinates in the sky map.
*     moving = int (Given)
*        A flag indicating if the telescope is tracking a moving object. If
*        so, each time slice is shifted so that the position specified by
*        TCS_AZ_BC1/2 is mapped on to the same pixel position in the
*        sky map.
*     slbnd = int [ 2 ] (Given)
*        The lower pixel index bounds of the sky map.
*     subnd = int [ 2 ] (Given)
*        The upper pixel index bounds of the sky map.
*     interp = int (Given)
*        Specifies the scheme to be used for interpolating the sky map data
*        array to find each output bolometer sample value. See docs for
*        astResample (SUN/211) for the allowed values.
*     params = const double[] (Given)
*        An optional pointer to an array of double which should contain any
*        additional parameter values required by the interpolation scheme.
*        See docs for astResample (SUN/211) for further information. If no
*        additional parameters are required, this array is not used and a
*        NULL pointer may be given.
*     sigma = double (Given)
*        Standard deviation of Gaussian noise to add to returned data.
*        Ignored if zero.
*     in_data = double * (Given)
*        The 2D data array for the input sky map.
*     out_data = double * (Returned)
*        The 3D data array for the output time series array. Must be
*        time-ordered (see smf_dataOrder.c).
*     ngood = int * (Returned)
*        Returned holding the number of good values in the output time
*        series.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     The data array of the supplied sky map is resampled at the
*     bolometer sample positions specified by the input template. The
*     resampled values are stored in the output time series cube.

*  Authors:
*     David S Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     8-JUN-2011 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* System includes */
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

#define FUNC_NAME "smf_resampmap"


/* Structure containing information about blocks of time slices that each
   thread will process */
typedef struct smfResampMapData {
   AstMapping *sky2map;
   AstSkyFrame *abskyfrm;
   const double *params;
   dim_t ndbolo;
   double *in;
   double *out;
   double sigma;
   int interp;
   int moving;
   int ngood;
   int sky_dim[ 2 ];
   size_t t1;
   size_t t2;
   smfData *data;
   gsl_rng *r;
} smfResampMapData;

/* Prototypes for local functions. */
static void smf1ResampMap( void *job_data_ptr, int *status );


void smf_resampmap( ThrWorkForce *wf, smfData *data, AstSkyFrame *abskyfrm,
                    AstMapping *sky2map, int moving, int slbnd[ 2 ],
                    int subnd[ 2 ], int interp, const double params[],
                    double sigma, double *in_data, double *out_data, int *ngood,
                    int *status ){

/* Local Variables */
   const gsl_rng_type *type;
   dim_t nbolo;
   dim_t ntslice;
   dim_t step;
   gsl_rng *r;
   int iw;
   int nw;
   smfResampMapData *job_data;
   smfResampMapData *pdata;

/* Initialise returned values. */
   *ngood = 0;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Note the number of time slices in the time series, and the number of
   bolometer values per time slice. We know the data is time-ordered. */
   smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, NULL, NULL, NULL,
                 status );

/* Store the number of workers in the work force. */
   nw = wf ? wf->nworker : 1;

/* Allocate job data for threads */
   job_data = astMalloc( nw*sizeof(*job_data) );
   if( *status == SAI__OK ) {

/* Create a default GSL random number generator. */
      if( sigma > 0.0 ) {
         type = gsl_rng_default;
         r = gsl_rng_alloc (type);
      } else {
         r = NULL;
      }

/* Get the number of time slices to process in each thread. */
      if( nw > (int) ntslice ) {
         step = 1;
      } else {
         step = ntslice/nw;
      }

/* Set up the job data for each thread. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;

/* The first and last time slice to be processed by the thread. */
         pdata->t1 = iw*step;
         pdata->t2 = ( iw + 1 )*step - 1;

/* Pointer to the first output data element for the first time slice. */
         pdata->out = out_data + pdata->t1*nbolo;

/* Pointer to the first input (i.e. sky map) data element. */
         pdata->in = in_data;

/* Ensure that the last thread picks up any left-over tslices */
         if( iw == nw - 1 ) pdata->t2 = ntslice - 1;

/* Make deep copies of AST objects and unlock them so that each thread
   can then lock them for their own exclusive use */
         pdata->abskyfrm = astCopy( abskyfrm );
         astUnlock( pdata->abskyfrm, 1 );
         pdata->sky2map = astCopy( sky2map );
         astUnlock( pdata->sky2map, 1 );

/* Similarly, make a copy of the smfData, including only the header
   information which each thread will need in order to make calls to
   smf_rebin_totmap */
         pdata->data = smf_deepcopy_smfData( data, 0, SMF__NOCREATE_FILE |
                                             SMF__NOCREATE_DA |
                                             SMF__NOCREATE_FTS |
                                             SMF__NOCREATE_DATA |
                                             SMF__NOCREATE_VARIANCE |
                                             SMF__NOCREATE_QUALITY, 0, 0,
                                             status );
         smf_lock_data( pdata->data, 0, status );


/* Other items required by the worker thread. */
         pdata->moving = moving;
         pdata->interp = interp;
         pdata->params = params;
         pdata->sky_dim[ 0 ] = subnd[ 0 ] - slbnd[ 0 ] + 1;
         pdata->sky_dim[ 1 ] = subnd[ 1 ] - slbnd[ 1 ] + 1;
         pdata->r = r;
         pdata->sigma = sigma;
         pdata->ngood = 0;

      }

/* Add each job to the job queue. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         (void) thrAddJob( wf, 0, pdata, smf1ResampMap, 0, NULL, status );
      }

/* Wait until all of the jobs have completed */
      thrWait( wf, status );

/* Free resources and count the totql number of good values in the output
   cube. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         *ngood += pdata->ngood;

         if( pdata->data ) {
            smf_lock_data( pdata->data, 1, status );
            smf_close_file( &(pdata->data), status );
         }
         astLock( pdata->abskyfrm, 0 );
         pdata->abskyfrm = astAnnul( pdata->abskyfrm );

         astLock( pdata->sky2map, 0 );
         pdata->sky2map = astAnnul( pdata->sky2map );
      }

      job_data = astFree( job_data );
      if( r ) gsl_rng_free( r );
   }

}








/* Function to be executed in thread: fill time slices t1 to t2 with
   resampled values. */

static void smf1ResampMap( void *job_data_ptr, int *status ) {

/* Local Variables: */
   AstMapping *fullmap;
   AstMapping *sky2map;
   AstSkyFrame *abskyfrm;
   dim_t ibolo;
   dim_t itime;
   dim_t nbolo;
   dim_t t1;
   dim_t t2;
   double *in;
   double *out;
   double sigma;
   const double *params;
   gsl_rng *r;
   int interp;
   int lbnd_bolo[ 2 ];
   int lbnd_sky[ 2 ];
   int moving;
   int nbad;
   int ubnd_bolo[ 2 ];
   int ubnd_sky[ 2 ];
   smfData *data;
   smfResampMapData *pdata;

/* Pointer to the data that this thread will process */
   pdata = job_data_ptr;

/* Extract values from pdata */
   abskyfrm = pdata->abskyfrm;
   data = pdata->data;
   in = pdata->in;
   interp = pdata->interp;
   moving = pdata->moving;
   out = pdata->out;
   params = pdata->params;
   sky2map = pdata->sky2map;
   r = pdata->r;
   sigma = pdata->sigma;
   t1 = pdata->t1;
   t2 = pdata->t2;
   ubnd_sky[ 0 ] = pdata->sky_dim[ 0 ];
   ubnd_sky[ 1 ] = pdata->sky_dim[ 1 ];

/* Lock the supplied AST object pointers for exclusive use by this
   thread.  The invoking thread should have unlocked them before
   starting this job. */
   astLock( abskyfrm, 0 );
   astLock( sky2map, 0 );
   smf_lock_data( data, 1, status );

/* Set lower grid bounds. */
   lbnd_bolo[ 0 ] = 1;
   lbnd_bolo[ 1 ] = 1;
   lbnd_sky[ 0 ] = 1;
   lbnd_sky[ 1 ] = 1;

/* Set bolometer array GRID upper bounds, and total number of bolometers. */
   ubnd_bolo[ 0 ] = (data->dims)[ 0 ];
   ubnd_bolo[ 1 ] = (data->dims)[ 1 ];
   nbolo =  ubnd_bolo[ 0 ]* ubnd_bolo[ 1 ];

/* Loop round every time slice to be processed by this thread. */
   for( itime = t1; itime <= t2; itime++ ){

/* Calculate the full bolometer-grid to skymap-grid transformation for
   the current time slice */
      fullmap = smf_rebin_totmap( data, itime, abskyfrm, sky2map, moving,
                                  status );

/* skip if we did not get a mapping this time round */
      if (*status == SAI__OK && !fullmap) continue;

/* Invert it as required by astResample. */
      astInvert( fullmap );

/* Resample the sky map at the positions of the bolometer samples in the
   current time slice, and store in the output array. */
      nbad = astResampleD( fullmap, 2, lbnd_sky, ubnd_sky, in, NULL,
                           interp, NULL, params, AST__USEBAD, 0.1, 100,
                           VAL__BADD, 2, lbnd_bolo, ubnd_bolo,
                           lbnd_bolo, ubnd_bolo,  out, NULL );

/* Return the number of good output sample values. */
      pdata->ngood = nbolo - nbad;


/* Add Gaussian noise to the output values. */
      if( r ) {
         for( ibolo = 0; ibolo < nbolo; ibolo++ ) {
            out[ ibolo ] += gsl_ran_gaussian( r, sigma );
         }
      }

/* Update the pointer to the first output data element for the next time
   slice. */
      out += nbolo;

/* Free resources. */
      fullmap = astAnnul( fullmap );
   }

/* Unlock the supplied AST object pointers so that other threads can use
   them. */
   smf_lock_data( data, 0, status );
   astUnlock( abskyfrm, 1 );
   astUnlock( sky2map, 1 );
}
