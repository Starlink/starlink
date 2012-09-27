/*
*+
*  Name:
*     smf_uncalc_iqu

*  Purpose:
*     Calculate analysed intensities from a set of I, Q and U values.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_uncalc_iqu( ThrWorkForce *wf, smfData *data, int nel,
*                          double *idata, double *qdata, double *udata,
*                          int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData * (Given)
*        Pointer to the time series data.
*     nel
*        The number of values in each array.
*     idata = double * (Given and Returned)
*        On entry an array of I values. On exit, an array of analysed
*        intensity values.
*     qdata = double * (Given)
*        An array of Q values.
*     udata = double * (Given)
*        An array of U values.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function converts the supplied arrays holding (I,Q,U) values
*     into analysed intensity, and returns them in place of the I values.

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     24-SEP-2012 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
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
typedef struct smfUncalcIQUJobData {
   const JCMTState *allstates;
   dim_t b1;
   dim_t b2;
   dim_t nbolo;
   double *ipq;
   double *ipu;
   double *ipi;
   int ipolcrd;
   int ntslice;
   int old;
   size_t bstride;
   size_t tstride;
} smfUncalcIQUJobData;

/* Prototypes for local functions */
static void smf1_uncalc_iqu_job( void *job_data, int *status );


/* Old data has POL_ANG given in arbitrary integer units where
   SMF__MAXPOLANG is equivalent to 2*PI. Store the factor to convert such
   values into radians. */
#define TORADS (2*AST__DPI/SMF__MAXPOLANG)


void smf_uncalc_iqu( ThrWorkForce *wf, smfData *data,
                     double *idata, double *qdata, double *udata,
                     int *status ){

/* Local Variables: */
   const JCMTState *state;    /* JCMTState info for current time slice */
   dim_t nbolo;               /* No. of bolometers */
   dim_t ntslice;             /* Number of time-slices in data */
   int bstep;                 /* Bolometer step between threads */
   int itime;                 /* Time slice index */
   int iworker;               /* Index of a worker thread */
   int ntime;                 /* Time slices to check */
   int nworker;               /* No. of worker threads */
   int old;                   /* Data has old-style POL_ANG values? */
   size_t bstride;            /* Stride between adjacent bolometer values */
   size_t tstride;            /* Stride between adjacent time slice values */
   smfHead *hdr;              /* Pointer to data header this time slice */
   smfUncalcIQUJobData *job_data = NULL; /* Pointer to all job data */
   smfUncalcIQUJobData *pdata = NULL;/* Pointer to next job data */
   char headval[ 81 ];        /* FITS header value */
   int ipolcrd;               /* Reference direction for waveplate angles */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Convenience pointer. */
   hdr = data->hdr;

/* Check the half-waveplate and analyser were in the beam. */
   headval[ 0 ] = 0;
   smf_getfitss( hdr, "POLWAVIN", headval, sizeof(headval), status );
   if( strcmp( headval, "Y" ) && *status == SAI__OK ) {
      smf_smfFile_msg( data->file, "N", 0, "" );
      *status = SAI__ERROR;
      errRep( " ", "Half-waveplate was not in the beam for "
              "input NDF ^N.", status );
   }

   headval[ 0 ] = 0;
   smf_getfitss( hdr, "POLANLIN", headval, sizeof(headval), status );
   if( strcmp( headval, "Y" ) && *status == SAI__OK ) {
      smf_smfFile_msg( data->file, "N", 0, "" );
      *status = SAI__ERROR;
      errRep( " ", "Analyser was not in the beam for input "
              "NDF ^N.", status );
   }

/* Get the reference direction for JCMTSTATE:POL_ANG values. */
   smf_getfitss( hdr, "POL_CRD", headval, sizeof(headval), status );
   if( !strcmp( headval, "FPLANE" ) ) {
      ipolcrd = 0;
   } else if( !strcmp( headval, "AZEL" ) ) {
      ipolcrd = 1;
   } else if( !strcmp( headval, "TRACKING" ) ) {
      ipolcrd = 2;
   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      smf_smfFile_msg( data->file, "N", 0, "" );
      msgSetc( "V", headval );
      errRep( " ", "Input NDF ^N contains unknown value "
              "'^V' for FITS header 'POL_CRD'.", status );
   }

/* Obtain number of time slices - will also check for 3d-ness. Also get
   the dimensions of the bolometer array and the strides between adjacent
   bolometer values. */
   smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, NULL, &bstride,
                 &tstride, status );

/* Create structures used to pass information to the worker threads. */
   nworker = wf ? wf->nworker : 1;
   job_data = astMalloc( nworker*sizeof( *job_data ) );

/* Check the above pointers can be used safely. */
   if( *status == SAI__OK ) {

/* Go through the first thousand POL_ANG values to see if they are in
   units of radians (new data) or arbitrary encoder units (old data).
   They are assumed to be in radians if no POL_ANG value is larger than
   20. */
      old = 0;
      state = hdr->allState;
      ntime = ( ntslice > 1000 ) ? 1000 : ntslice;
      for( itime = 0; itime < ntime; itime++,state++ ) {
         if( state->pol_ang > 20 ) {
            old = 1;
            msgOutif( MSG__VERB, "","   POL2 data contains POL_ANG values "
                      "in encoder units - converting to radians.", status );
            break;
         }
      }

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
   jobs to calculate the analysed intensity values in each bolo, and then
   wait for them to complete. */
      for( iworker = 0; iworker < nworker; iworker++ ) {
         pdata = job_data + iworker;

         pdata->bstride = bstride;
         pdata->nbolo = nbolo;
         pdata->tstride = tstride;
         pdata->allstates = hdr->allState;
         pdata->ipi = idata;
         pdata->ipq = qdata;
         pdata->ipu = udata;
         pdata->ipolcrd = ipolcrd;
         pdata->old = old;
         pdata->ntslice = ntslice;

/* Pass the job to the workforce for execution. */
         thrAddJob( wf, THR__REPORT_JOB, pdata, smf1_uncalc_iqu_job, 0, NULL,
                      status );
      }

/* Wait for the workforce to complete all jobs. */
      thrWait( wf, status );
   }

/* Free resources. */
   job_data = astFree( job_data );
}


static void smf1_uncalc_iqu_job( void *job_data, int *status ) {
/*
*  Name:
*     smf1_uncalc_iqu_job

*  Purpose:
*     Calculate I, Q and U for a block of bolometers.

*  Invocation:
*     void smf1_uncalc_iqu_job( void *job_data, int *status )

*  Arguments:
*     job_data = void * (Given)
*        Pointer to the data needed by the job. Should be a pointer to a
*        smfUncalcIQUJobData structure.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine calculate the I, Q and U values for each bolometer in
*     a block of bolometers. It runs within a thread instigated by
*     smf_uncalc_iqu.

*/

/* Local Variables: */
   const JCMTState *allstates;/* Pointer to array of JCMTState structures */
   const JCMTState *state;    /* JCMTState info for current time slice */
   dim_t b1;                  /* First bolometer index */
   dim_t b2;                  /* Last bolometer index */
   dim_t ibolo;               /* Bolometer index */
   dim_t nbolo;               /* Total number of bolometers */
   double *iin;               /* Pointer to I array for each bolometer*/
   double *ipi0;              /* Pointer to input I array for 1st time */
   double *ipi;               /* Pointer to supplied I array */
   double *ipq0;              /* Pointer to input Q array for 1st time */
   double *ipq;               /* Pointer to supplied Q array */
   double *ipu0;              /* Pointer to input U array for 1st time */
   double *ipu;               /* Pointer to supplied U array */
   double *qin;               /* Pointer to Q array for each bolometer*/
   double *uin;               /* Pointer to U array for each bolometer*/
   double angle;              /* Phase angle for FFT */
   int ipolcrd;               /* Reference direction for pol_ang */
   int itime;                 /* Time slice index */
   int ntslice;               /* Number of time slices */
   int old;                   /* Data has old-style POL_ANG values? */
   size_t bstride;            /* Stride between adjacent bolometer values */
   size_t tstride;            /* Stride between adjacent time slice values */
   smfUncalcIQUJobData *pdata;   /* Pointer to job data */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer to the job data, and then extract its contents into a
   set of local variables. */
   pdata = (smfUncalcIQUJobData *) job_data;

   b1 = pdata->b1;
   b2 = pdata->b2;
   bstride = pdata->bstride;
   nbolo = pdata->nbolo;
   tstride = pdata->tstride;
   allstates = pdata->allstates;
   ipi = pdata->ipi;
   ipq = pdata->ipq;
   ipu = pdata->ipu;
   ipolcrd = pdata->ipolcrd;
   ntslice = pdata->ntslice;
   old = pdata->old;

/* Check we have something to do. */
   if( b1 < nbolo ) {

/* Initialise pointers to the first time slice I, Q and U values for
   the first bolometer to be processed. */
      ipi0 = ipi + bstride*b1;
      ipq0 = ipq + bstride*b1;
      ipu0 = ipu + bstride*b1;

/* Loop round all bolometers to be processed by this thread. */
      for( ibolo = b1; ibolo <= b2; ibolo++ ) {

/* Initialise pointers to the next I, Q and U time slice values for
   the current bolometer. */
         iin = ipi0;
         qin = ipq0;
         uin = ipu0;

/* Loop round all time slices. */
         state = allstates;
         for( itime = 0; itime < ntslice; itime++,state++ ) {

/* Get the POL_ANG value for this time slice. */
            angle = state->pol_ang;

/* Check the I, Q, U and angle values are good. */
            if( *iin != VAL__BADD && *qin != VAL__BADD &&
                *uin != VAL__BADD && angle != VAL__BADD ) {

/* If POL_ANG is stored in arbitrary encoder units, convert to radians. */
               if( old ) angle = angle*TORADS;

/* Get the angle between the half-waveplate and north in the tracking
   system. */
               if( ipolcrd == 0 ) {
                  angle -= state->tcs_tr_ang;
               } else if( ipolcrd == 1 ) {
                  angle -= state->tcs_tr_ang - state->tcs_az_ang ;
               }

/* Calculate the analysed intensity and store it in place of the I value.
   Note, the effective analyser angle rotates twice as fast as the half-wave
   plate which is why there is a factor of 4 here rather than a factor of 2.
   An angle of zero corresponds to north in the tracking system. */
               angle *= 4;
               *iin = 0.5*( (*iin) + (*qin)*cos( angle ) + (*uin)*sin( angle ) );
            } else {
               *iin = VAL__BADD;
            }

/* Update pointers to the next I, Q and U time slice values. */
            iin += tstride;
            qin += tstride;
            uin += tstride;
         }

/* Update the pointers to the first I, Q and U time slice values for
   the next bolometer. */
         ipi0 += bstride;
         ipq0 += bstride;
         ipu0 += bstride;
      }
   }
}


