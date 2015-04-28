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
*                          double *angdata, int pasign, double paoff,
*                          double angrot, double amp4, double phase4,
*                          int harmonic, int *status );

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
*     angdata = double * (Given)
*        An array holding the angle from the reference direction used by
*        the supplied Q and U values, to the focal plane Y axis, in radians,
*        at each time slice. Positive rotation is in the same sense as
*        rotation from focal plane X to focal plane Y.
*     pasign = int (Given)
*        Should be supplied non-zero if a positive POL_ANG value
*        corresponds to rotation from focal plane X to focal plane Y axis,
*        and zero otherwise.
*     paoff = double (Given)
*        The angle from the fixed analyser to the have-wave plate for a
*        POL_ANG value of zero, in radians. Measured positive in the same
*        sense as rotation from focal plane X to focal plane Y.
*     angrot = double (Given)
*        The angle from the focal plane X axis to the fixed analyser, in
*        radians. Measured positive in the same sense as rotation from focal
*        plane X to focal plane Y.
*     amp4 = double (Given)
*        Ignored if "harmonic" is not 4. It gives the amplitude of the
*        4Hz signal to include in the returned analysed intensity signal, as
*        a fraction of the total intensity. This is an alternative to setting
*        using "harmonic" to 2.
*     phase4 = double (Given)
*        Ignored if "harmonic" is not 4. It gives the phase offset for the
*        4Hz signal to include in the returned Q and U signal, in radians.
*     harmonic = int  (Given)
*        The harmonic of the half-wave plate rotation from which the Q
*        and U values should be derived. This should normally be 4, but
*        other values can be used to investigate the effects of asymetry in
*        the half-wave plate, etc.
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
*     7-JAN-2013 (DSB):
*        Use focal plane Y axis as the reference direction.
*     8-JAN-2013 (DSB):
*        Added arguments pasign, paoff and angrot.
*     20-SEP-2013 (DSB):
*        Added parameter "harmonic".
*     29-APR-2015 (DSB):
*        Added arguments amp4 and phase4.
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
   double *ipang;
   double *ipi;
   double *ipq;
   double *ipu;
   double angfac;
   double angrot;
   double paoff;
   double amp4;
   double phase4;
   int ipolcrd;
   int ntslice;
   int old;
   int pasign;
   size_t bstride;
   size_t tstride;
} smfUncalcIQUJobData;

/* Prototypes for local functions */
static void smf1_uncalc_iqu_job( void *job_data, int *status );


/* Old data has POL_ANG given in arbitrary integer units where
   SMF__MAXPOLANG is equivalent to 2*PI. Store the factor to convert such
   values into radians. */
#define TORADS (2*AST__DPI/SMF__MAXPOLANG)


void smf_uncalc_iqu( ThrWorkForce *wf, smfData *data, double *idata,
                     double *qdata, double *udata, double *angdata,
                     int pasign, double paoff, double angrot, double amp4,
                     double phase4, int harmonic, int *status ){

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
   ipolcrd = 0;
   if( !strcmp( headval, "AZEL" ) ) {
      ipolcrd = 1;
   } else if( !strcmp( headval, "TRACKING" ) ) {
      ipolcrd = 2;
   } else if( strcmp( headval, "FPLANE" ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      smf_smfFile_msg( data->file, "N", 0, "" );
      msgSetc( "V", headval );
      errRep( " ", "Input NDF ^N contains unknown value "
              "'^V' for FITS header 'POL_CRD'.", status );
   }

/* Can only handle POL_CRD = FPLANE at the moment. */
   if( ipolcrd != 0 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( "", "smf_uncalc_iqu: currently only POL_CRD = FPLANE is "
               "supported.", status );
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
         pdata->ipang = angdata;
         pdata->ipolcrd = ipolcrd;
         pdata->old = old;
         pdata->ntslice = ntslice;
         pdata->pasign = pasign ? +1: -1;
         pdata->paoff = paoff;
         pdata->angrot = angrot;
         pdata->angfac = harmonic/4.0;
         pdata->amp4 = amp4;
         pdata->phase4 = phase4;

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
   double *ipang;             /* Pointer to supplied FP orientation array */
   double *ipi0;              /* Pointer to input I array for 1st time */
   double *ipi;               /* Pointer to supplied I array */
   double *ipq0;              /* Pointer to input Q array for 1st time */
   double *ipq;               /* Pointer to supplied Q array */
   double *ipu0;              /* Pointer to input U array for 1st time */
   double *ipu;               /* Pointer to supplied U array */
   double *qin;               /* Pointer to Q array for each bolometer*/
   double *uin;               /* Pointer to U array for each bolometer*/
   double amp4;
   double phase4;
   double angfac;
   double angle;              /* Phase angle for FFT */
   double angrot;             /* Angle from focal plane X axis to fixed analyser */
   double cosval;             /* Cos of twice reference rotation angle */
   double ival;               /* I  value */
   double paoff;              /* WPLATE value corresponding to POL_ANG=0.0 */
   double phi;                /* Angle from fixed analyser to effective analyser */
   double qval;               /* Q value wrt fixed analyser */
   double sinval;             /* Sin of twice reference rotation angle */
   double uval;               /* U value wrt fixed analyser */
   double wplate;             /* Angle from fixed analyser to have-wave plate */
   int ipolcrd;               /* Reference direction for pol_ang */
   int itime;                 /* Time slice index */
   int ntslice;               /* Number of time slices */
   int old;                   /* Data has old-style POL_ANG values? */
   int pasign;                /* +1 or -1 indicating sense of POL_ANG value */
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
   pasign = pdata->pasign;
   paoff = pdata->paoff;
   angrot = pdata->angrot;
   angfac = pdata->angfac;
   amp4 = pdata->amp4;
   phase4 = pdata->phase4;

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

/* Initialise a pointer to the anti-clockwise angle from the Q/U reference
   direction to the focal plane Y axis, at the next time slice. */
         ipang = pdata->ipang;

/* Loop round all time slices. */
         state = allstates;
         for( itime = 0; itime < ntslice; itime++,state++,ipang++ ) {

/* Get the POL_ANG value for this time slice. */
            angle = state->pol_ang;

/* Check the I, Q, U and angle values are good. */
            if( *iin != VAL__BADD && *qin != VAL__BADD &&
                *uin != VAL__BADD && angle != VAL__BADD &&
                *ipang != VAL__BADD ) {

/* If POL_ANG is stored in arbitrary encoder units, convert to radians. */
               if( old ) angle = angle*TORADS;

/* Following SUN/223 (section "Single-beam polarimetry"/"The Polarimeter"),
   get the angle from the fixed analyser to the half-waveplate axis, in radians.
   Positive rotation is from focal plane axis 1 (x) to focal plane axis 2 (y).

   Not sure about the sign of tcs_az/tr_ang at the moment so do not use them
   yet. */
               wplate = 0.0;
               if( ipolcrd == 0 ) {
                  wplate = pasign*angle + paoff;

               } else if( *status == SAI__OK ) {
                  *status = SAI__ERROR;
                  errRepf( "", "smf_uncalc_iqu: currently only POL_CRD = "
                           "FPLANE is supported.", status );
               }

/*
               if( ipolcrd == 1 ) {
                  wplate += state->tcs_az_ang;
               } else if( ipolcrd == 2 ) {
                  wplate += state->tcs_tr_ang;
               }
*/

/* Get the angle from the fixed analyser to the effective analyser
   position (see SUN/223 again). The effective analyser angle rotates twice
   as fast as the half-wave plate which is why there is a factor of 2 here. */
               phi = 2*wplate;

/* Rotate the Q,U values so that they refer to a reference direction
   parallel to the fixed analyser. */
               angle = 2*( *ipang + angrot - AST__DPIBY2 );
               cosval = cos( angle );
               sinval = sin( angle );
               qval = (*qin)*cosval + (*uin)*sinval;
               uval = (*uin)*cosval - (*qin)*sinval;

/* Calculate the analysed intensity and store it in place of the I value.
   A phi value of zero corresponds to the fixed analyser (i.e. the new Q/U
   reference direction). Allow the angle to be scaled by some user-specified
   factor. This is to allow the investigation of other harmonics. */
               ival = *iin;
               *iin = 0.5*( ival + qval*cos( 2*phi*angfac ) +
                                   uval*sin( 2*phi*angfac ) );

/* If producing the usuall 8Hz harmonic, optionally add in a 4Hz
   component to the signal. */
               if( angfac == 1 && amp4 != 0.0 ) {
                  *iin += amp4*ival*sin( phi + phase4 );
               }

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


