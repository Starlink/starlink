/*
*+
*  Name:
*     smf_rebinmap

*  Purpose:
*     Map-maker that simply rebins the supplied input file into the output.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_rebinmap( ThrWorkForce *wf, smfData *data, double *bolovar,
*                   int index, int size, AstFrameSet *outfset, int spread,
*                   const double params[], int moving, int genvar,
*                   dim_t *lbnd_out, dim_t *ubnd_out, double *map, *variance,
*                   double *weights, int64_t *nused, fts2Port fts_port,
*                   int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads that will do the re-binning.
*     data = smfData * (Given)
*        Pointer to smfData struct holding input data. The smfData will
*        be closed when the job has completed.
*     bolovar = double * (Given)
*        Pointer to array giving variance of each bolometer. Can be NULL.
*        If not NULL, it should be a pointer to a dynamically allocated
*        array. This array will be freed using smf_free when the job has
*        completed.
*     index = int (Given)
*        Index of element in igrp
*     size = int (Given)
*        Number of elements in igrp
*     outfset = AstFrameSet * (Given)
*        Frameset containing the sky->output map mapping
*     spread = int (Given)
*        Integer code for pixel-spreading scheme
*     params[] = const double (Given)
*        Array of additional parameters for certain schemes
*     moving = int (Given)
*        Flag to denote whether the object is moving
*     genvar = int (Given)
*        If zero, no output variances are calculated. Otherwise, output
*        variances are calculated on the basis of the spread of input
*        data values.
*     lbnd_out = dim_t * (Given)
*        2-element array pixel coord. for the lower bounds of the output map
*     ubnd_out = dim_t * (Given)
*        2-element array pixel coord. for the upper bounds of the output map
*     map = double* (Returned)
*        A pointer to an array containing room for "nw" contiguous versions
*        of the output data array (where "nw" is the number of worker threads
*        in the supplied workforce).
*     variance = double * (Returned)
*        A pointer to an array containing room for "nw" contiguous versions
*        of the output variance array.
*     weights = double * (Returned)
*        A pointer to an array containing room for "nw" contiguous versions
*        of the relative weighting for each pixel in the output map. If
*        "genvar" is non-zero, two doubles are needed for each output pixel.
*        Otherwise, only one double is needed for each output pixel.
*     nused = int64_t * (Given and Returned)
*        A pointer to a int64_t that is updated to hold the total number
*        of input samples that have been pasted into the output array so far.
*     fts_port = fts2Port (Given)
*        The FTS-2 port.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function uses astRebinSeq to paste all time slices in the supplied
*     input NDF into the output image, using a simple regridding of data.

*  Authors:
*     EC: Edward Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     AGG: Andy Gibb (UBC)
*     DSB: David Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     2006-02-02 (EC):
*        Initial version.
*     2006-02-13 (TIMJ):
*        Use astSetC rather than astSet
*        Avoid an additional dereference
*     2006-03-23 (AGG):
*        Updated API: now takes a smfData rather than a Grp
*     2006-07-26 (TIMJ):
*        sc2head not actually used.
*     2007-01-25 (AGG):
*        Rewrite to take account of moving objects
*     2007-02-27 (AGG):
*        Minor refactor for improved status handling
*     2007-05-3 (DSB):
*        Adapt to new astRebinSeq signature.
*     2007-07-11 (DSB):
*        - Use astConvert to convert between input and output skyframes,
*        rather than input frameset and output skyframe. Only get the
*        azel->output skyframe FrameSet for moving sources if the input
*        skyframe is not azel.
*        - Avoid remapping the FrameSet more often than is necessary when
*        setting and clearing the SkyRef attributes.
*     2007-07-12 (EC):
*        -Replaced calculation of bolo2map with a call to smf_rebincube_totmap
*        -Changed name of smf_rebincube_totmap to smf_rebin_totmap
*     2008-02-12 (AGG):
*        Updated API to allow processing of bad bolometer masks. These
*        updates deprecate smf_bbrebinmap.
*     2008-02-13 (AGG):
*        Add parameters for pixel spreading scheme
*     2008-02-15 (AGG):
*        Enable AST__GENVAR to return variances
*     2008-03-11 (AGG):
*        Remove old bad bolometer-mask code: masking is now done with
*        quality flags
*     2008-04-16 (AGG):
*        Add genvar parameter
*     2008-04-18 (AGG):
*        Set lbnd to 1,1
*     2008-06-04 (TIMJ):
*        astRebinSeq requires GRID output bounds not PIXEL
*     2008-6-16 (DSB):
*        Re-written to use threads.
*     2008-10-21 (EC):
*        Added bolovar to interface
*     2008-11-24 (DSB):
*        Added nused to interface
*     2008-11-25 (DSB):
*        Free the bolovar array once the job has completed.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research
*     Council. Copyright (C) 2005-2008 University of British Columbia
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

/* Standard includes */
#include <stdio.h>
#include <stdint.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "star/thr.h"

/* SMURF includes */
#include "libsmf/smf.h"

void smf_rebinmap( ThrWorkForce *wf, smfData *data, double *bolovar,
                   int index, int size, AstFrameSet *outfset, int spread,
                   const double params[], int moving, int genvar,
                   dim_t *lbnd_out, dim_t *ubnd_out, double *map, double *variance,
                   double *weights, int64_t *nused, fts2Port fts_port,
                   int *status ) {

/* Local Variables */
   AstMapping *dummy = NULL;     /* A dummy Mapping */
   AstMapping *sky2map=NULL;     /* Mapping from celestial->map coordinates */
   AstSkyFrame *abskyfrm = NULL; /* Output SkyFrame (always absolute) */
   AstSkyFrame *oskyfrm = NULL;  /* SkyFrame from the output WCS Frameset */
   dim_t i;                      /* Loop counter */
   dim_t ldim[ 2 ];              /* Output array lower GRID bounds */
   double *p1;                   /* Pointer to next data value */
   double *p;                    /* Pointer to next data value */
   int ijob;                     /* Job identifier */
   int j;                        /* Worker index */
   int nw;                       /* Number of worker threads */
   smfRebinMapData *pdata = NULL;/* Pointer to data for a single thread */

   static dim_t nel;             /* # of elements in output map/var array */
   static dim_t nelw;            /* # of elements in output weights array */
   static int rebinflags;        /* Control the rebinning procedure */
   static dim_t udim[ 2 ];       /* Output array upper GRID bounds */
   static smfRebinMapData *job_data = NULL; /* Data for all jobs */

/* Check the inherited status */
   if (*status != SAI__OK) return;

/* Check we have valid input data */
   if ( data == NULL ) {
      *status = SAI__ERROR;
      errRep( "", "Input data struct is NULL", status );
   }

/* And a valid FrameSet */
   if ( outfset == NULL ) {
      *status = AST__OBJIN;
      errRep( "", "Supplied FrameSet is NULL", status );
   }

/* Store the number of workers in the work force. */
   nw = wf ? wf->nworker : 1;

/* Do some initialisation on the first invocation of this function. */
   if( index == 1 ) {

/* Retrieve the skyframe and the sky2map mapping from the output frameset
   (actually map2sky). */
      oskyfrm = astGetFrame( outfset, AST__CURRENT );
      sky2map = astGetMapping( outfset, AST__BASE, AST__CURRENT );

/* Invert it to get output SKY to output map coordinates */
      astInvert( sky2map );

/* Create a SkyFrame in absolute coordinates */
      abskyfrm = astCopy( oskyfrm );
      astClear( abskyfrm, "SkyRefIs" );
      astClear( abskyfrm, "SkyRef(1)" );
      astClear( abskyfrm, "SkyRef(2)" );

/* Calculate number of elements in the final output array. */
      udim[ 0 ] = ubnd_out[ 0 ] - lbnd_out[ 0 ] + 1;
      udim[ 1 ] = ubnd_out[ 1 ] - lbnd_out[ 1 ] + 1;
      nel = udim[ 0 ]*udim[ 1 ];

/* Calculate number of elements in the final weights array. */
      nelw = ( genvar && variance ? 2 : 1 )*nel;

/* Store flags for astRebinSeqD. */
      rebinflags = AST__USEBAD;
      if( genvar && variance ) rebinflags |= AST__GENVAR;
      if( bolovar ) rebinflags |= AST__VARWGT;

/* The worker threads in the work force perform descrete jobs. In the
   context of this function, a single job consists of pasting all the
   time slices from a single input file into an output array. Each such
   job is described by a smfRebinMapData structure. We now allocate memory
   to hold an array of these structures. The number of elements in this
   array need only equal the number of workers in the workforce (rather
   than the number of input files), since we will re-use them. */
      job_data = astMalloc( sizeof( smfRebinMapData )*nw );
      if( job_data ) {

/* Initialise the components within these structures that are the same for
   all input files. Each worker thread pastes its inputs into a different
   output array. This is done to avoid clashes caused by multiple workers
   writing to the same output array at the same time. Once all input files
   have been pasted, these separate output arrays are combined together to
   form the final output array.

   We take deep copies of any AST objects which are needed by the worker
   threads, and then unlock them so that the worker threads can lock them
   for their own exclusive use (they are initially locked by the thread
   that created them - i.e. this thread). */
         for( j = 0; j < nw; j++ ) {
            pdata = job_data + j;

            pdata->data = NULL;
            pdata->rebinflags = rebinflags;
            pdata->abskyfrm = astCopy( abskyfrm );
            astUnlock( pdata->abskyfrm, 1 );
            pdata->sky2map = astCopy( sky2map );
            astUnlock( pdata->sky2map, 1 );
            pdata->moving = moving;
            pdata->spread = spread;
            pdata->params = params;
            pdata->udim[ 0 ] = ubnd_out[ 0 ] - lbnd_out[ 0 ] + 1;
            pdata->udim[ 1 ] = ubnd_out[ 1 ] - lbnd_out[ 1 ] + 1;
            pdata->map = map + nel*j;
            pdata->variance = variance ? variance + nel*j : NULL;
            pdata->weights = weights + nelw*j;
            pdata->ijob = -1;
            pdata->fts_port = fts_port;
         }
      }

/* Fill the supplied arrays with zeros on the first invocation. */
      p = map;
      p1 = p + nel*nw;
      while( p < p1 ) *(p++) = 0.0;

      if( variance ) {
         p = variance;
         p1 = p + nel*nw;
         while( p < p1 ) *(p++) = 0.0;
      }

      p = weights;
      p1 = p + nelw*nw;
      while( p < p1 ) *(p++) = 0.0;

/* Free AST pointers that are no longer needed. */
      oskyfrm = astAnnul( oskyfrm );
      sky2map = astAnnul( sky2map );
      abskyfrm = astAnnul( abskyfrm );

/* Initialise the number of input samples used so far. */
      *nused = 0;
   }

/* Attempt to find a smfRebinMapData that is not currently being used by
   a worker thread (indicated by its job identifier - the "ijob"
   component - being set to -1). */
   for( j = 0; j < nw; j++ ) {
      pdata = job_data + j;
      if( pdata->ijob == -1 ) break;
   }

/* If all smfRebinMapData structures are currently in use, wait until
   a job completes. When this happens, note the identifier for the job
   that has just completed. */
   if( pdata->ijob != -1 ) {
      ijob = thrJobWait( wf, status );

/* Find the smfRebinMapData that was used by the job that has just
   completed, and indicate it is no longer being used by setting its job
   identifier to -1. Also lock all AST objects within the smfData for use
   by this thread, close then the corresponding smfData, and update the
   total number of input samples that have been pasted into the output. */
      for( j = 0; j < nw; j++ ) {
         pdata = job_data + j;
         if( pdata->ijob == ijob ) {
            pdata->ijob = -1;
            smf_lock_data( pdata->data, 1, status );
            smf_close_file( wf, &(pdata->data), status );
            pdata->bolovar = astFree( pdata->bolovar );
            *nused += pdata->nused;
            break;
         }
      }

/* Report an error if we could not find the smfRebinMapData. */
      if( pdata->ijob != -1 && *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSeti( "I", ijob );
         errRep( "", "smf_rebinmap: Orphaned job (^I) encountered.",
                 status );
      }
   }

/* Fill in the details of the current input file, and then submit a new
   job to be performed by the work force. This job will paste the input
   file into the output array associated with the smfRebinMapData being
   used. The thrAddJob function returns the identifier for the new job.
   Store this identifier in the smfRebinMapData to indicate that the
   smfRebinMapData is now being used. The actual work of pasting the
   slices into the output array is done by the smf_rebinslices function.
   We need to unlock all AST objects within the smfData so that the
   smf_rebinslices function can lock them. They will be re-locked when
   the job completes. */
   smf_lock_data( data, 0, status );
   pdata->nused = 0;
   pdata->data = data;
   pdata->bolovar = bolovar;
   pdata->ijob = thrAddJob( wf, THR__REPORT_JOB, pdata, smf_rebinslices,
                              0, NULL, status );

/* Finalise things if we have just submitted a job to paste the last input
   file. */
   if( index == size ) {

/* Wait until all submitted jobs have completed. */
      thrWait( wf, status );

/* Free the resources used by the array of smfRebinMapData structures. */
      for( j = 0; j < nw; j++ ) {
         pdata = job_data + j;

         if( pdata->data ) {
            smf_lock_data( pdata->data, 1, status );
            smf_close_file( wf, &(pdata->data), status );
            pdata->bolovar = astFree( pdata->bolovar );
            *nused += pdata->nused;
         }

         astLock( pdata->abskyfrm, 0 );
         pdata->abskyfrm = astAnnul( pdata->abskyfrm );

         astLock( pdata->sky2map, 0 );
         pdata->sky2map = astAnnul( pdata->sky2map );
      }
      job_data = astFree( job_data );

/* Add the data array used by the second and subsequent smfRebinMapData
   into the data array used by the first (this is the data array that
   will be retained as the final output data array). */
      p1 = map + nel;
      for( j = 1; j < nw; j++ ) {
         p = map;
         for( i = 0; i < nel; i++ ) *(p++) += *(p1++);
      }

/* Likewise, add the variance array used by the second and subsequent
   smfRebinMapData into the variance array used by the first. */
      if( variance ) {
         p1 = variance + nel;
         for( j = 1; j < nw; j++ ) {
            p = variance;
            for( i = 0; i < nel; i++ ) *(p++) += *(p1++);
         }
      }

/* Likewise, add the weights array used by the second and subsequent
   smfRebinMapData into the weights array used by the first. */
      p1 = weights + nelw;
      for( j = 1; j < nw; j++ ) {
         p = weights;
         for( i = 0; i < nelw; i++ ) *(p++) += *(p1++);
      }

/* Normalise the data and variance arrays. A dummy mapping is specified (it
   is not actually used for anything since we are not adding any more data
   into the output arrays). */
      ldim[ 0 ] = 1;
      ldim[ 1 ] = 1;
      dummy = (AstMapping *) astUnitMap( 2, " " );
      astRebinSeq8D( dummy, 0.0, 2, NULL, NULL, NULL, NULL, spread,
                     params, AST__REBINEND | rebinflags, 0.1, 1000000,
                     VAL__BADD, 2, ldim, udim, NULL, NULL, map,
                     variance, weights, nused );
      dummy = astAnnul( dummy );
   }
}
