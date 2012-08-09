/*
*+
*  Name:
*     smf_calc_qu

*  Purpose:
*     Calculate a pair of Q and U images from a block of time slices.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_calc_qu( ThrWorkForce *wf, smfData *data, int block_start,
*                       int block_end, int ipolcrd, int qplace, int uplace,
*                       NdgProvenance *oprov, AstFitsChan *fc, int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData * (Given)
*        Pointer to the time series data.
*     block_start
*        Index of the time slice at start of block.
*     block_end
*        Index of the time slice at end of block.
*     ipolcrd
*        Indicates the reference direction for half-waveplate angles:
*        0 = FPLANE, 1 = AZEL, 2 = TRACKING. In all case, the reference
*        direction is the positive direction of the second axis.
*     qplace = int (Given)
*        A placeholder identifying the location at which to store the
*        the output NDF holding the Q image.
*     uplace = int (Given)
*        A placeholder identifying the location at which to store the
*        the output NDF holding the U image.
*     oprov = NdgProvenance * (Given)
*        Pointer to a structure holding the provenance information to
*        store in the Q and U NDFs.
*     fc = AstFitsChan * (Given)
*        Pointer to a FitsChan holding the FITS headers to store in the
*        FITS extensions of the Q and U NDFs.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function creates a pair of 2D NDFs - one holding Q values and
*     the other holding U values. Each NDF holds a Q or U value for each
*     bolometer. Each NDF has its own WCS component, and receives the
*     supplied provenance information and FITS headers. The Q and U values
*     are based on the time series data between time slices "block_start"
*     and "block_end". The spatial position of each bolometer is assumed
*     not to move significantly over the duration of this block of time
*     slices. The Q and U bvalues stored in the output NDFs are referenced
*     to north in the tracking system.

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1-FEB-2011 (DSB):
*        Original version.
*     9-AUG-2012 (DSB):
*        Add POLANAL Frames to the WCS FrameSet fo each output NDF. This
*        communicates the reference direction to POLPACK.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science and Technology Facilities Council.
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

#include <stdio.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Local data types: */
typedef struct smfCalcQUJobData {
   const JCMTState *allstates;
   dim_t b1;
   dim_t b2;
   dim_t nbolo;
   dim_t ntslice;
   double *dat;
   double *ipq;
   double *ipu;
   int ipolcrd;
   int block_start;
   int block_end;
   size_t bstride;
   size_t tstride;
   smf_qual_t *qua;
} smfCalcQUJobData;

/* Prototypes for local functions */
static void smf1_calc_qu_job( void *job_data, int *status );



void smf_calc_qu( ThrWorkForce *wf, smfData *data, int block_start,
                  int block_end, int ipolcrd, int qplace, int uplace,
                  NdgProvenance *oprov, AstFitsChan *fc, int *status ){

/* Local Variables: */
   int bstep;                 /* Bolometer step between threads */
   dim_t nbolo;               /* No. of bolometers */
   dim_t ncol;                /* No. of columns of bolometers */
   dim_t nrow;                /* No. of rows of bolometers */
   dim_t ntslice;             /* Number of time-slices in data */
   double *ipq;               /* Pointer to output Q array */
   double *ipu;               /* Pointer to output U array */
   int el;                    /* Number of mapped array elements */
   int indfq;                 /* Identifier for NDF holding Q values */
   int indfu;                 /* Identifier for NDF holding Q values */
   int lbnd[ 2 ];             /* Lower pixel bounds of output NDF */
   int iworker;               /* Index of a worker thread */
   int nworker;               /* No. of worker threads */
   int ubnd[ 2 ];             /* Upper pixel bounds of output NDF */
   size_t bstride;            /* Stride between adjacent bolometer values */
   size_t tstride;            /* Stride between adjacent time slice values */
   smfCalcQUJobData *job_data = NULL; /* Pointer to all job data */
   smfCalcQUJobData *pdata = NULL;/* Pointer to next job data */
   smfHead *hdr;              /* Pointer to data header this time slice */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Convenience pointers. */
   hdr = data->hdr;

/* Obtain number of time slices - will also check for 3d-ness. Also get
   the dimensions of the bolometer array and the strides between adjacent
   bolometer values. */
   smf_get_dims( data, &nrow, &ncol, &nbolo, &ntslice, NULL, &bstride,
                 &tstride, status );

/* Report an error if the block of time slices extends of either end. */
   if( block_start < 0 || block_end >= (int) ntslice ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSeti( "S", block_start );
         msgSeti( "E", block_end );
         msgSeti( "N", ntslice );
         errRep( " ", "smf_calc_qu: invalid block of time slices - ^S to "
                 "^E (^N time slices are available).", status );
      }
   }

/* Create the two output NDFs. Each one is a 2D array with dimensions
   equal to the bolometer array. */
   lbnd[ 0 ] = 1;
   lbnd[ 1 ] = 1;
   ubnd[ 0 ] = ncol;
   ubnd[ 1 ] = nrow;
   ndfNew( "_DOUBLE", 2, lbnd, ubnd, &qplace, &indfq, status );
   ndfNew( "_DOUBLE", 2, lbnd, ubnd, &uplace, &indfu, status );

/* Store any supplied provenance in both NDFs. */
   if( oprov ) {
      ndgWriteProv( oprov, indfq, 1, status );
      ndgWriteProv( oprov, indfu, 1, status );
   }

/* Store any supplied FITS headers in both NDFs.*/
   if( fc && astGetI( fc, "NCard" ) > 0 ) {
      kpgPtfts( indfq, fc, status );
      kpgPtfts( indfu, fc, status );
   }

/* Store the WCS frameSet in both NDFs. First get the FrameSet for the
   central time slice in the block, set its current Frame to the tracking
   frame, and then store it in the two NDFs. */
   smf_tslice_ast( data, ( block_start + block_end )/2, 1, status);
   astSetC( hdr->wcs, "System",
            sc2ast_convert_system( (data->hdr->allState)[0].tcs_tr_sys,
                                    status ) );
   ndfPtwcs( hdr->wcs, indfq, status );
   ndfPtwcs( hdr->wcs, indfu, status );

/* Map the Data array in each NDF. */
   ndfMap( indfq, "Data", "_DOUBLE", "WRITE", (void **) &ipq, &el, status );
   ndfMap( indfu, "Data", "_DOUBLE", "WRITE", (void **) &ipu, &el, status );

/* Create structures used to pass information to the worker threads. */
   nworker = wf ? wf->nworker : 1;
   job_data = astMalloc( nworker*sizeof( *job_data ) );

/* Check the above pointers can be used safely. */
   if( *status == SAI__OK ) {

/* Determine which bolometers are to be processed by which threads. */
      bstep = nbolo/nworker;
      if( bstep < 1 ) bstep = 1;

      for( iworker = 0; iworker < nworker; iworker++ ) {
         pdata = job_data + iworker;
         pdata->b1 = iworker*bstep;
         pdata->b2 = pdata->b1 + bstep - 1;
      }

/* Ensure that the last thread picks up any left-over bolometers */
      pdata->b2 = nbolo - 1;

/* Store all the other info needed by the worker threads, and submit the
   jobs to calculate the Q and U values in each bolo, and then wait for
   them to complete. */
      for( iworker = 0; iworker < nworker; iworker++ ) {
         pdata = job_data + iworker;

         pdata->bstride = bstride;
         pdata->dat = data->pntr[0];;
         pdata->nbolo = nbolo;
         pdata->ntslice = ntslice;
         pdata->qua = smf_select_qualpntr( data, NULL, status );;
         pdata->tstride = tstride;
         pdata->allstates = hdr->allState;
         pdata->ipq = ipq;
         pdata->ipu = ipu;
         pdata->ipolcrd = ipolcrd;
         pdata->block_start = block_start;
         pdata->block_end = block_end;

/* Pass the job to the workforce for execution. */
         thrAddJob( wf, THR__REPORT_JOB, pdata, smf1_calc_qu_job, 0, NULL,
                      status );
      }

/* Wait for the workforce to complete all jobs. */
      thrWait( wf, status );
   }

/* Add POLANAL Frames to the WCS FrameSet in each output NDF. This Frame
   is used by POLPACK to determine the reference direction of the Stokes
   vectors (celestial north in this case). */
   smf_polext( indfq, 0, 0.0, status );
   smf_polext( indfu, 0, 0.0, status );

/* Free the two output NDFs. */
   ndfAnnul( &indfq, status );
   ndfAnnul( &indfu, status );

/* Free other resources. */
   job_data = astFree( job_data );
}


static void smf1_calc_qu_job( void *job_data, int *status ) {
/*
*  Name:
*     smf1_calc_qu_job

*  Purpose:
*     Calculate Q and U for a block of bolometers.

*  Invocation:
*     void smf1_calc_qu_job( void *job_data, int *status )

*  Arguments:
*     job_data = void * (Given)
*        Pointer to the data needed by the job. Should be a pointer to a
*        smfCalcQUJobData structure.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine calculate the Q and U values for each bolometer in a
*     block of bolometers. It runs within a thread instigated by
*     smf_calc_qu.

*/

/* Local Variables: */
   const JCMTState *allstates;/* Pointer to array of JCMTState structures */
   const JCMTState *state;    /* JCMTState info for current time slice */
   dim_t b1;                  /* First bolometer index */
   dim_t b2;                  /* Last bolometer index */
   dim_t ibolo;               /* Bolometer index */
   dim_t nbolo;               /* Total number of bolometers */
   dim_t ntslice;             /* Number of time-slices in data */
   double *dat;               /* Pointer to start of input data values */
   double *din0;              /* Pointer to input data array for 1st time */
   double *din;               /* Pointer to input data array for bolo/time */
   double *ipq;               /* Pointer to output Q array */
   double *ipu;               /* Pointer to output U array */
   double angle;              /* Phase angle for FFT */
   double q;                  /* Output Q value */
   double s1;                 /* Sum of weighted cosine terms */
   double s2;                 /* Sum of weighted sine terms */
   double u;                  /* Output U value */
   int block_end;             /* Last time slice to process */
   int block_start;           /* First time slice to process */
   int ipolcrd;               /* Reference direction for pol_ang */
   int itime;                 /* Time slice index */
   int n;                     /* Number of contributing values in S1 and S2 */
   size_t bstride;            /* Stride between adjacent bolometer values */
   size_t tstride;            /* Stride between adjacent time slice values */
   smfCalcQUJobData *pdata;   /* Pointer to job data */
   smf_qual_t *qin0;          /* Pointer to input quality array for 1st time */
   smf_qual_t *qin;           /* Pointer to input quality array for bolo/time */
   smf_qual_t *qua;           /* Pointer to start of input quality values */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer to the job data, and then extract its contents into a
   set of local variables. */
   pdata = (smfCalcQUJobData *) job_data;

   b1 = pdata->b1;
   b2 = pdata->b2;
   bstride = pdata->bstride;
   dat = pdata->dat;
   nbolo = pdata->nbolo;
   ntslice = pdata->ntslice;
   qua = pdata->qua;
   tstride = pdata->tstride;
   allstates = pdata->allstates;
   ipq = pdata->ipq;
   ipu = pdata->ipu;
   ipolcrd = pdata->ipolcrd;
   block_start = pdata->block_start;
   block_end = pdata->block_end;

/* Check we have something to do. */
   if( b1 < nbolo ) {

/* Increment the output Q and U pointers to point to the first bolometer
   processed by this thread. */
      ipq += b1;
      ipu += b1;

/* Initialise pointers to the first time slice data and quality value for
   the first bolometer to be processed. */
      din0 = dat + bstride*b1;
      qin0 = qua + bstride*b1;

/* Loop round all bolometers to be processed by this thread. */
      for( ibolo = b1; ibolo <= b2; ibolo++ ) {

/* If the whole bolometer is bad, just use bad q and u values. */
         if( *qin0 & SMF__Q_BADB ) {
            u = VAL__BADD;
            q = VAL__BADD;

/* If the bolometer is good, calculate and store the q and u values. */
         } else {

/* Initialise pointers to the next time slice data and quality value for
   the current bolometer. */
            din = din0;
            qin = qin0;

/* Initialise the sums used to find Q and U at this bolometer. */
            s1 = 0.0;
            s2 = 0.0;
            n = 0.0;

/* Loop round all time slices. */
            state = allstates;
            for( itime = block_start; itime <= block_end; itime++,state++ ) {

/* Check the input sample has not been flagged during cleaning and is
   not bad. */
               if( !( *qin & SMF__Q_FIT ) && *din != VAL__BADD ) {

/* Get the angle between the half-waveplate and north in the tracking
   system. */
                  angle = state->pol_ang;
                  if( ipolcrd == 0 ) {
                     angle -= state->tcs_tr_ang;
                  } else if( ipolcrd == 1 ) {
                     angle -= state->tcs_tr_ang - state->tcs_az_ang ;
                  }

/* Increment the sums needed to find the Fourier component of the time
   series corresponding to the frequency inroduced by by the rotation of
   the half wave plate. Note, the effective analyser angle rotates twice as
   fast as the half-wave plate which is why there is a factor of 4 here
   rather than a factor of 2. An angle of zero corresponds to north in
   the tracking system. */
                  angle *= 4;
                  s1 += (*din)*cos( angle );
                  s2 += (*din)*sin( angle );
                  n++;
               }

/* Update pointers to the next time slice data and quality value for
   the current bolometer. */
               din += tstride;
               qin += tstride;
            }

/* Calculate the q and u values in the output NDF. The error on these values
   will be enormous if there are not many values, so put a hard lower limit
   of 5 samples. */
            if( n > 4 ) {
               q = 4*s1/n;
               u = 4*s2/n;

/* Rotate the (q,u) vector so that an angle of zero corresponds to celestial
   north. */
            } else {
               q = VAL__BADD;
               u = VAL__BADD;
            }
         }

/* Store the q and u values in the output NDFs, and increment the pointers
   so that they point to the next output values. */
         *(ipq++) = q;
         *(ipu++) = u;

/* Update the pointers to the first time slice data and quality value for
   the next bolometer. */
         din0 += bstride;
         qin0 += bstride;
      }
   }
}


