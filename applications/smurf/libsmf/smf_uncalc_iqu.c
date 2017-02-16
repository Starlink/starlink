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
*     void smf_uncalc_iqu( ThrWorkForce *wf, smfData *data, double *idata,
*                          double *qdata, double *udata, double *angdata,
*                          int pasign, double paoff, double angrot, double amp2,
*                          double phase2, double amp4, double phase4,
*                          double amp16, double phase16, int ipform,
*                          const double *pldata, const double *qinst,
*                          const double *uinst, const double *c0,
*                          const double *p0, const double *p1,
*                          const double *angc, int harmonic, double offset,
*                          int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData * (Given)
*        Pointer to the time series data.
*     idata = double * (Given and Returned)
*        On entry an array of I values. On exit, an array of analysed
*        intensity values. These are assumed to incorporate the POL2
*        degradation factor of 1.35.
*     qdata = double * (Given)
*        An array of Q values. These are assumed to incorporate the POL2
*        degradation factor of 1.35.
*     udata = double * (Given)
*        An array of U values. These are assumed to incorporate the POL2
*        degradation factor of 1.35.
*     angdata = double * (Given)
*        An array holding the angle from the reference direction used by
*        the supplied Q and U values, to the focal plane Y axis, in radians,
*        at each time slice (at each time slice, all bolometers are
*        assumed to have the same reference direction in focal plane
*        coordinates). Positive rotation is in the same sense as rotation
*        from focal plane X to focal plane Y.
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
*     amp2 = double (Given)
*        Ignored if "harmonic" is not 4. It gives the amplitude of the
*        2Hz signal to include in the returned analysed intensity signal, as
*        a fraction of the total intensity. This is an alternative to setting
*        using "harmonic" to 1.
*     amp4 = double (Given)
*        Ignored if "harmonic" is not 4. It gives the amplitude of the
*        4Hz signal to include in the returned analysed intensity signal, as
*        a fraction of the total intensity. This is an alternative to setting
*        using "harmonic" to 2.
*     amp16 = double (Given)
*        Ignored if "harmonic" is not 4. It gives the amplitude of the
*        16Hz signal to include in the returned analysed intensity signal, as
*        a fraction of the total intensity. This is an alternative to setting
*        using "harmonic" to 8.
*     phase2 = double (Given)
*        Ignored if "harmonic" is not 4. It gives the phase offset for the
*        2Hz signal to include in the returned Q and U signal, in radians.
*     phase4 = double (Given)
*        Ignored if "harmonic" is not 4. It gives the phase offset for the
*        4Hz signal to include in the returned Q and U signal, in radians.
*     phase16 = double (Given)
*        Ignored if "harmonic" is not 4. It gives the phase offset for the
*        16Hz signal to include in the returned Q and U signal, in radians.
*     ipform = int (Given):
*        Indicates the IP model to use: 0="NONE, 1 ="PL1", 2="JK", 3="USER",
*        4="PL2", 5 = "PL3".
*     pldata = const double * (Given):
*        The parameters of the PL1, PL2 or PL3 IP model. Only used if
*        "ipform" is 1, 4 or 5. The length of this array should be 3 for PL1
*        and 4 for PL2/PL3.
*     qinst = const double * (Given)
*        Only used if "ipform" is 3. An array of normalised Q values
*        for each bolometer. The instrumental Q seen by each bolometer is
*        found by multiplying the corresponding total intensity by this
*        factor. The fixed analyser is assumed to be the reference direction.
*     uinst = const double * (Given)
*        Only used if "ipform" is 3. An array of normalised U values
*        for each bolometer. The instrumental U seen by each bolometer is
*        found by multiplying the corresponding total intensity by this
*        factor. The fixed analyser is assumed to be the reference direction.
*     c0 = const double * (Given)
*        Only used if "ipform" is 2. An array of C0 values for each
*        bolometer. C0 is one of terms in the POL2 instrumental polarisation
*        model created by Doug Johnstone and James Kennedy.
*     p0 = const double * (Given)
*        Only used if "ipform" is 2. An array of P0 values for each
*        bolometer. P0 is one of terms in the POL2 instrumental polarisation
*        model created by Doug Johnstone and James Kennedy.
*     p1 = const double * (Given)
*        Only used if "ipform" is 2. An array of P1 values for each
*        bolometer. P1 is one of terms in the POL2 instrumental polarisation
*        model created by Doug Johnstone and James Kennedy.
*     angc = const double * (Given)
*        Only used if "ipform" is 2. An array of ANGC values for each
*        bolometer. ANGC is one of terms in the POL2 instrumental polarisation
*        model created by Doug Johnstone and James Kennedy.
*     harmonic = int  (Given)
*        The harmonic of the half-wave plate rotation from which the Q
*        and U values should be derived. This should normally be 4, but
*        other values can be used to investigate the effects of asymetry in
*        the half-wave plate, etc.
*     offset = double (Given)
*        The constant total intensity (in pW) caused by electronic
*        offset rather than sky emission. This value is assumed to be the
*        same for all bolometers and time-slices. It is subtracted off
*        the supplied total intensity before calculating the returned
*        analysed intensity values.
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
*     30-APR-2015 (DSB):
*        Added arguments qinst and uinst.
*     11-MAY-2015 (DSB):
*        Added arguments amp2, phase2, amp16 and phase16.
*     3-SEP-2015 (DSB):
*        Added arguments c0, p0, p1 and angc.
*     7-SEP-2015 (DSB):
*        Check for bad c0, p0, p1 and angc values.
*     3-DEC-2015 (DSB):
*        Added support for PL1 IP model.
*     2-JUN-2016 (DSB):
*        Added argument "offset".
*     15-FEB-2017 (DSB):
*        Added support for PL2 and PL3 IP model.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     Copyright (C) 2017 East Asian Observatory.
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
   const double *angc;
   const double *c0;
   const double *p0;
   const double *p1;
   const double *pldata;
   const double *qinst;
   const double *uinst;
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
   double amp2;
   double amp4;
   double amp16;
   double offset;
   double phase2;
   double phase4;
   double phase16;
   int ipform;
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
                     int pasign, double paoff, double angrot, double amp2,
                     double phase2, double amp4, double phase4, double amp16,
                     double phase16, int ipform, const double *pldata,
                     const double *qinst, const double *uinst, const double *c0,
                     const double *p0, const double *p1, const double *angc,
                     int harmonic, double offset, int *status ){

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
         pdata->offset = offset;
         pdata->amp2 = amp2;
         pdata->amp4 = amp4;
         pdata->amp16 = amp16;
         pdata->phase2 = phase2;
         pdata->phase4 = phase4;
         pdata->phase16 = phase16;
         pdata->ipform = ipform;
         pdata->pldata = pldata;
         pdata->qinst = qinst;
         pdata->uinst = uinst;
         pdata->c0 = c0;
         pdata->p0 = p0;
         pdata->p1 = p1;
         pdata->angc = angc;

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
   double amp16;
   double amp2;
   double amp4;
   double angfac;
   double angle;              /* Phase angle for FFT */
   double angrot;             /* Angle from focal plane X axis to fixed analyser */
   double ca;
   double cb;
   double cc;
   double cd;
   double cosval;             /* Cos of twice reference rotation angle */
   double ip_qi;              /* normalised instrument Q for current bolo */
   double ip_ui;              /* normalised instrument U for current bolo */
   double ival;               /* I  value */
   double k;
   double offset;
   double p1;
   double paoff;              /* WPLATE value corresponding to POL_ANG=0.0 */
   double phase16;
   double phase2;
   double phase4;
   double phi;                /* Angle from fixed analyser to effective analyser */
   double qval;               /* Q value wrt fixed analyser */
   double sinval;             /* Sin of twice reference rotation angle */
   double t1;
   double uval;               /* U value wrt fixed analyser */
   double wplate;             /* Angle from fixed analyser to have-wave plate */
   int ipform;
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
   amp2 = pdata->amp2;
   phase2 = pdata->phase2;
   amp4 = pdata->amp4;
   phase4 = pdata->phase4;
   amp16 = pdata->amp16;
   phase16 = pdata->phase16;
   ipform = pdata->ipform;
   ca = pdata->pldata[0];
   cb = pdata->pldata[1];
   cc = pdata->pldata[2];
   cd = pdata->pldata[3];
   offset = pdata->offset;

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
                *ipang != VAL__BADD ){

/* The instrumental polarisation seen by this bolometer. These are
   normalised Q and U values. Multiply them by the total intensity to
   get the instrument Q and U, with respect to the fixed analyser. */
               if( ipform == 3 ) {
                  ip_qi = pdata->qinst[ ibolo ];
                  ip_ui = pdata->uinst[ ibolo ];

               } else if( ipform == 2 ) {
                  if( pdata->c0[ ibolo ] != VAL__BADD &&
                      pdata->angc[ ibolo ] != VAL__BADD &&
                      pdata->p0[ ibolo ] != VAL__BADD &&
                      pdata->p1[ ibolo ] != VAL__BADD ) {
                     t1 = AST__DD2R*( pdata->c0[ ibolo ] + pdata->angc[ ibolo ] )
                          + state->tcs_az_ac2;
                     ip_qi = pdata->p0[ ibolo ]*cos( 2*pdata->angc[ ibolo ]*AST__DD2R ) +
                             pdata->p1[ ibolo ]*cos( 2*t1 );
                     ip_ui = pdata->p0[ ibolo ]*sin( 2*pdata->angc[ ibolo ]*AST__DD2R ) +
                             pdata->p1[ ibolo ]*sin( 2*t1 );
                  } else {
                     ip_qi = VAL__BADD;
                     ip_ui = VAL__BADD;
                  }

               } else if( ipform == 1 ) {
                  p1 = ca + cb*state->tcs_az_ac2 + cc*state->tcs_az_ac2*state->tcs_az_ac2;
                  ip_qi = p1*cos( -2*state->tcs_az_ac2 );
                  ip_ui = p1*sin( -2*state->tcs_az_ac2 );

               } else if( ipform == 4 || ipform == 5 ) {
                  p1 = ca + cb*state->tcs_az_ac2 + cc*state->tcs_az_ac2*state->tcs_az_ac2;
                  ip_qi = p1*cos( -2*( state->tcs_az_ac2 - cd ) );
                  ip_ui = p1*sin( -2*( state->tcs_az_ac2 - cd ) );

               } else {
                  ip_qi = 0.0;
                  ip_ui = 0.0;
               }

/* Check the IP values are good. */
               if( ip_qi != VAL__BADD && ip_ui != VAL__BADD ) {

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

/* Get the total intensity of the sky emission. This is the supplied total
   intensity value minus the electronic offset. */
                  ival = *iin - offset;

/* POL2 introduces a loss of about 1.35 in the analysed signal strength
   measured by SCUBA-2. The input I, Q and U values supplied to unmakemap
   are assumed to include this degradation, and so no further degradation
   is applied here. However, some IP models incorporate the degradation
   into their parameter values and some do not. If the IP model is *not*
   PL3, the IP model gives the Q/U to subtract from the *degraded* Q and
   U signals, as a function of the NON-degraded I signal. This means we
   need to boost the supplied I values (which are assumed to be degraded)
   to get the non-degraded I values before using the IP model. The PL3
   IP model gives the Q/U to subtract from the degraded Q and U signals,
   as a function of the degraded I signal, and so no change is needed. */
                  k = ( ipform != 5 ) ? 1.35 : 1.0;

/* Add in the instrumental polarisation. This is assumed to be fixed in
   focal plane coords, which means it is also fixed with respect to the
   analyser. */
                  qval += ip_qi*ival*k;
                  uval += ip_ui*ival*k;

/* Calculate the analysed intensity and store it in place of the I value.
   A phi value of zero corresponds to the fixed analyser (i.e. the new Q/U
   reference direction). Allow the angle to be scaled by some user-specified
   factor. This is to allow the investigation of other harmonics. */
                  *iin = 0.5*( ival + qval*cos( 2*phi*angfac ) +
                                      uval*sin( 2*phi*angfac ) ) + offset;

/* If producing the usuall 8Hz harmonic, optionally add in 2, 4 and 16 Hz
   components to the signal. */
                  if( angfac == 1 ) {
                     if( amp2 != 0.0 ) *iin += amp2*ival*sin( phi/2 + phase2 );
                     if( amp4 != 0.0 ) *iin += amp4*ival*sin( phi + phase4 );
                     if( amp16 != 0.0 ) *iin += amp16*ival*sin( 4*phi + phase16 );
                  }

               } else {
                  *iin = VAL__BADD;
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


