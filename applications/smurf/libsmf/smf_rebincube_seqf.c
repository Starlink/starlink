/*
*+
*  Name:
*     smf_rebincube_seqf

*  Purpose:
*     A multi-threading wrapper for astRebinSeqF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_rebincube_seqf( ThrWorkForce *workforce, int njobs,
*                              double *blk_bot, AstMapping *this, double wlim,
*                              int ndim_in, const dim_t lbnd_in[],
*                              const dim_t ubnd_in[], const float in[],
*                              const float in_var[], int spread,
*                              const double params[], int flags,
*                              double tol, int maxpix, float badval,
*                              int ndim_out, const dim_t lbnd_out[],
*                              const dim_t ubnd_out[], const dim_t lbnd[],
*                              const dim_t ubnd[], float out[], float out_var[],
*                              double weights[], int64_t *nused, int *status );

*  Arguments:
*     workforce = ThrWorkForce * (Given)
*        Pointer to a pool of threads, between which the requested rebinning
*        can be divided.
*     njobs = int (Given)
*        The number of jobs into which to divide each re-binning.
*     blk_bot = double * (Given)
*        An array that holds the first input channel number within each
*        block of the input spectrum (plus the first channel number
*        beyond the last block). See "Description" below for an
*        explanation of how these blocks are used. If "njobs" is 1,
*        the array should contain 2 elements (enclosing the whole portion
*        of the input spectrum that is mapped into the output cube).
*        Otherwise, the array should contain ( 2*njobs + 1 ) elements.
*
*     <see docs for astRebinSeq for a description of the other arguments>
*
*     status = int * (Given and Returned)
*        A pointer to the inherited status value.

*  Description:
*     This function uses several threads to re-bin the supplied cube
*     using the supplied Mapping. Each thread invokes astRebinSeq to
*     re-bin a block of input spectral channels into the output. These
*     blocks are chosen so that the corresponding blocks of output channels
*     never overlap (in general, adjacent input channels will be pasted
*     into overlapping blocks of output channels due to the blurring
*     produced by the requested spreading scheme). This is achieved by
*     splitting the input spectral range processed by each thread up into
*     two adjacent blocks, and processing these blocks in two passes. On
*     the first pass, each thread processes the first of its two adjacent
*     spectral blocks. Once all threads have completed the first pass, the
*     second pass commences. In the second pass, each thread processes the
*     second of its two adjacent spectral blocks.
*
*     It is assumed that the spectral axis is axis 1 in the input and
*     axis 3 in the output.

*  Authors:
*     David S Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     21-MAY-2008 (DSB):
*        Initial version.
*     14-MAY-2014 (DSB):
*        Do not attempt to rebin empty blocks.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008,2014 Science & Technology Facilities Council.
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

/* System includes */
#include <stdio.h>
#include <stdint.h>
#include <math.h>
#include <pthread.h>

/* Starlink includes */
#include "ast.h"
#include "ems.h"
#include "sae_par.h"
#include "star/thr.h"

/* Smurf includes */
#include "smf.h"
#include "smf_typ.h"

void smf_rebincube_seqf( ThrWorkForce *workforce, int njobs,
                         double *blk_bot, AstMapping *this, double wlim,
                         int ndim_in, const dim_t lbnd_in[],
                         const dim_t ubnd_in[], const float in[],
                         const float in_var[], int spread,
                         const double params[], int flags,
                         double tol, int maxpix, float badval,
                         int ndim_out, const dim_t lbnd_out[],
                         const dim_t ubnd_out[], const dim_t lbnd[],
                         const dim_t ubnd[], float out[], float out_var[],
                         double weights[], int64_t *nused, int *status ){

/* Local Variables */
   int i;                            /* Thread index */
   int j;                            /* Axis index */
   dim_t lbndt[ 3 ];                 /* Lower bounds of input block */
   dim_t ubndt[ 3 ];                 /* Upper bounds of input block */
   smfRebinSeqArgs *data = NULL;     /* Pointer to data for a single thread */
   static dim_t *block_lbnd = NULL;  /* Lower bounds of input block */
   static dim_t *block_ubnd = NULL;  /* Upper bounds of input block */
   static smfRebinSeqArgs *job_data = NULL; /* Data for all jobs */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* If not using multiple threads, just call astRebinSeqF, restricting the
   input spectral range to the supplied block. */
   if( njobs == 1 ) {

      lbndt[ 0 ] = (dim_t) ( blk_bot[ 0 ] + 0.5 );
      ubndt[ 0 ] = (dim_t) ( blk_bot[ 1 ] - 0.5 );

      for( j = 1; j < ndim_in; j++ ) {
         lbndt[ j ] = lbnd[ j ];
         ubndt[ j ] = ubnd[ j ];
      }

      astRebinSeq8F( this, wlim, ndim_in, lbnd_in, ubnd_in, in, in_var, spread,
                     params, flags, tol, maxpix, badval, ndim_out, lbnd_out,
                     ubnd_out, lbndt, ubndt, out, out_var, weights, nused );

/* If handling multiple threads... */
   } else {

/* Begin an AST context. */
      astBegin;

/* Ensure we have memory to hold the data describing each job. This memory
   is retained between invocations of this function and grows as necessary
   to accomodate changing numbers of jobs (but in practice the number of
   jobs per spectrum will probably not change between invocations). This
   is done to avoid the over-head of allocating and deallocting the memory
   in every invocation. Likewise allocate memory to hold the bounds of the
   spectral block being processed. */
      job_data = astGrow( job_data, sizeof( smfRebinSeqArgs ), njobs );

      block_lbnd = astGrow( block_lbnd, sizeof( *block_lbnd ), njobs*ndim_in );
      block_ubnd = astGrow( block_ubnd, sizeof( *block_ubnd ), njobs*ndim_in );

/* Check pointers can be used safely. */
      if( astOK ) {

/* Prepare EMS for multi-threaded work. */
         emsMark();

/* Copy the supplied argument values into the job data structure. */
         for( i = 0; i < njobs; i++ ) {
            data = job_data + i;

            data->is_double = 0;
            data->wlim = wlim;
            data->ndim_in = ndim_in;
            data->lbnd_in = lbnd_in;
            data->ubnd_in = ubnd_in;
            data->in = in;
            data->in_var = in_var;
            data->spread = spread;
            data->params = params;
            data->flags = flags;
            data->tol = tol;
            data->maxpix = maxpix;
            data->badval_f = badval;
            data->ndim_out = ndim_out;
            data->lbnd_out = lbnd_out;
            data->ubnd_out = ubnd_out;
            data->out = out;
            data->out_var = out_var;
            data->weights = weights;
            data->nused = 0;

/* A thread can only access AST Objects if they are locked for its own
   exclusive use (an error is reported by AST if an Object is used that
   is not currently locked by the calling thread). Since each thread
   running astRebinSeqF will need to access the supplied Mapping
   simultaneously, we get round this by taking a deep copy of the
   supplied Mapping for each thread. Each copy is then unlocked so that
   the new thread can lock it succesfully. */
            data->this = astCopy( this );
            astUnlock( data->this, 1 );

/* Set the bounds on the first (i.e. spectral) axis of the input block to be
   processed by each thread so that it covers the first of the two adjacent
   spectral blocks. */
            data->lbnd = block_lbnd + i*ndim_in;
            data->ubnd = block_ubnd + i*ndim_in;
            (data->lbnd)[ 0 ] = (dim_t)( blk_bot[ 2*i ] + 0.5 );
            (data->ubnd)[ 0 ] = (dim_t)( blk_bot[ 2*i + 1 ] - 0.5 );

/* Set the bounds on the other axes so that it covers the whole range of
   that axis. */
            for( j = 1; j < ndim_in; j++ ) {
               (data->lbnd)[ j ] = lbnd[ j ];
               (data->ubnd)[ j ] = ubnd[ j ];
            }

/* Skip empty blocks. */
            if( (data->ubnd)[ 0 ] >= (data->lbnd)[ 0 ] ) {

/* Add this job to the workforce. It may start immediately if a worker
   thread is available, otherwise it will start as soon as a worker
   becomes available. The worker thread does its work by invoking the
   smf_rebinseq_thread function, passing it the data structure
   created above. */
               thrAddJob( workforce, 0, data, smf_rebinseq_thread,
                          0, NULL, status );
            }
         }

/* Wait until the work force has done all the re-binning. This call blocks
   until all worker threads have completed. */
         thrWait( workforce, status );

/* Loop round all the jobs. */
         for( i = 0; i < njobs; i++ ) {
            data = job_data + i;

/* Increment the total number of input pixels pasted into the output, and
   reset ready for pass 2. */
            *nused += data->nused;
            data->nused = 0;
         }

/* Tell EMS we have finished the first batch of multi-threaded work. */
         emsRlse();

/* Pass one is complete. If no error has ocurred, start pass two to do
   the intervening spectral blocks. */
         if( *status == SAI__OK ) {

/* Prepare EMS for multi-threaded work. */
            emsMark();

/* Loop round each job. We re-use the data structures created in pass 1,
   modifying any necessary fields so that they are appropriate for pass 2. */
            for( i = 0; i < njobs && *status == SAI__OK; i++ ) {
               data = job_data + i;

/* Set the bounds on the first (i.e. spectral) axis of the input block to be
   processed by each job so that it covers the second of the two adjacent
   spectral blocks. */
               (data->lbnd)[ 0 ] = (dim_t)( blk_bot[ 2*i + 1 ] + 0.5 );
               (data->ubnd)[ 0 ] = (dim_t)( blk_bot[ 2*i + 2 ] - 0.5 );

/* Skip empty blocks. */
               if( (data->ubnd)[ 0 ] >= (data->lbnd)[ 0 ] ) {

/* Add the new job to the workforce. */
                  thrAddJob( workforce, 0, data, smf_rebinseq_thread,
                             0, NULL, status );
               }
            }

/* Wait for the work force to complete the re-binning. This call blocks until
   all worker threads have completed. */
            thrWait( workforce, status );

/* All jobs are now complete. */
            for( i = 0; i < njobs; i++ ) {
               data = job_data + i;

/* Increment the total number of input pixels pasted into the output. */
               *nused += data->nused;
            }

/* Tell EMS that we have finished multi-threaded work. */
            emsRlse();
         }

/* The Mappings used in the threads should have been left in an unlocked
   state when the threads completed. Lock them now for use by this thread
   so that they can be annulled by the final astEnd. */
         for( i = 0; i < njobs; i++ ) {
            data = job_data + i;
            astLock( data->this, 0 );
         }
      }

/* End the AST context. */
      astEnd;
   }
}

