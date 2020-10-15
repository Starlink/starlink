/*
*+
*  Name:
*     smf_fit_qui

*  Purpose:
*     Produce three down-sampled smfDatas holding Q, U and I from a
*     single POL2- smfData.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_fit_qui( ThrWorkForce *wf, smfData *idata, smfData **odataq,
*                  smfData **odatau, smfData **odatai, smfData **odataf,
*                  dim_t box, int ipolcrd, int pasign, double paoff,
*                  double angrot, const char *north, int harmonic,
*                  double ang0, double *polang, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads.
*     idata = smfData * (Given)
*        Pointer to an input smfData struct holding POL-2 analysed
*        intensity time-streams.
*     odataq = smfData ** (Given and Returned)
*        Pointer to a newly created smfData struct holding the Q time-streams.
*     odatau = smfData ** (Given and Returned)
*        Pointer to a newly created smfData struct holding the U time-streams.
*     odatai = smfData ** (Given and Returned)
*        Pointer to a newly created smfData struct holding the I time-streams.
*        May be NULL.
*     odataf = smfData ** (Given and Returned)
*        Pointer to a newly created smfData struct holding the fit to the
*        supplied analysed intensity time-streams (sampled at the same
*        rate as "idata"). May be NULL.
*     box = dim_t (Given)
*        Length, in waveplate cycles, of a single fitting box.
*     ipolcrd
*        Indicates the reference direction for half-waveplate angles:
*        0 = FPLANE, 1 = AZEL, 2 = TRACKING. In all case, the reference
*        direction is the positive direction of the second axis.
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
*     north = const char * (Given)
*        If not NULL, the returned Q/U values use north in the specified
*        SKY system as their reference direction. Otherwise, the reference
*        direction is the focal plane Y axis. The value of "north" should
*        be the AST "System" Value corresponding to the required SKY
*        system (e.g. "AZEL", "FK5", etc).
*     harmonic = int  (Given)
*        The harmonic(s) of the half-wave plate rotation from which the Q
*        and U values should be derived. This should normally be 4, but
*        other values can be used to investigate the effects of asymetry in
*        the half-wave plate, etc. The returned Q and U values are the
*        sum of the requested harmonics. For instance if "harmonic" is 13,
*        i.e. 1 + 4 + 8, then the returned Q and U values will be the sum
*        of the individual Q and U values formed from the first, fourth and
*        eigth harmonic.
*     ang0 = double (Given)
*        The HWP angle, in radians, at which each fitting box starts.
*     polang = double * (Given)
*        Array of HWP angles (POL_ANG) - one for every time slice.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine creates three new smfData structures holding Q, U and I
*     values derived from a single supplied smfData holding POL-2 analysed
*     intensity. Each (Q,U,I) triplet is found by doing a least squares
*     fit to the corresponding analysed intensity data over a short range
*     of time. The fitted function includes first, second, fourth and eight
*     harmonics of the waveplate, together with a linear background. Each
*     bolometer is processed independently.
*
*     For each bolometer, the data within each "box" rotations of the
*     waveplate is fitted using the following function ("w" is the angle
*     of the waveplate, and "itime" is the zero-based offset of the time
*     slice into the section of fitted data):
*
*        y = A*sin(4*w) + B*cos(4*w) + C*sin(2*w) + D*cos(2*w) +
*            E*sin(w) + F*cos(w) + G*itime + H + J*sin(8*w) + K*cos(8*w)
*
*     If NT is the number of time slices in the section of fitted data, the
*     returned Q, U and I values are then:
*
*        U = 2*A
*        Q = 2*B
*        I = 2*( G*NT/2 + H )
*
*     This is assuming "harmonic" is supplied set to 4. If it is set to
*     1, the returned Q/U values are (I remains unchanged):
*
*        U = 2*E
*        Q = 2*F
*
*     If "harmonic" is set to 2, the returned Q/U values are:
*
*        U = 2*C
*        Q = 2*D
*
*     If "harmonic" is set to 8, the returned Q/U values are:
*
*        U = 2*J
*        Q = 2*K
*
*     The Q and U values are specified with respect to either north, or
*     focal plane Y axis (see argument "north").
*
*     Care is taken to ensure that each fitting box spans exactly the same
*     range of "w" values. This is needed because neither the HWP rotation
*     speed nor the sampling rate are exactly constant.
*
*     The residuals of the fit in each box are used to determine
*     notional variances for the retruned Q, U and I. These are not
*     "real" variances, but are just a scaled form of th residuals
*     variance using a scalaing factor that seems to give a reasonable
*     similarity to the noise in the Q and U values measured in several
*     test observations.

*  Notes:
*     - It is assumed that all output smfData structures have the same
*     dimensions.
*     - The output smfData structures have no variance or quality
*     component. Bad values are used to flag missing I/Q/U values within
*     the each Data array.

*  Authors:
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     8-MAY-2015 (DSB):
*        Initial version
*     21-SEP-2015 (DSB):
*        Check for sections of the time stream where the sample rate goes
*        bananas, resulting in far fewer samples per rotation of the HWP
*        than usual. For instamce, in s8a20150918_00018_0017 around sample
*        4040, the sample rate drops briefly from 178 Hz to 9 Hz.
*     29-SEP-2015 (DSB):
*        Added parameter "harmonic".
*     30-MAY-2016 (DSB):
*        Added parameter "odataf".
*     14-SEP-2016 (DSB):
*        Check consistently for bad POL_ANG values.
*     16-JUL-2018 (DSB):
*        Resample the JCMTState info using the correct scaling factor.
*        Previously, the scaling factor did not take account of the fact
*        that some input time slices are not used. This resulted in the
*        wrong pointing info being assigned to samples, with an
*        increasing error along the time stream, resulting in the source
*        being smeared. The degree of smearing was related to how many input
*        slices were unused and so varied from observation to observation.
*     17-JUL-2018 (DSB):
*        Yesterday's fix cleared up cases of really bad scaling - e.g. where
*        a potentially large section of the time stream was ignored because
*        of jumps in the POL_ANG value. But it leaves smaller scaling issues
*        in a significant number of observations. This can cause makemap to
*        create badly blurred images The problem was that the JCMTState
*        resampling code was assuming that all boxes are the same length,
*        which is not the case. So now, instead of using smfDownsamp1, each
*        important JCMTState value in the output is calculated within this
*        function as the average of the input JCMTState values for the time
*        slices that contibute to each output I, Q or U value.
*     5-FEB-2020 (DSB):
*        Added argument polang. This allows the same POL_ANG values to
*        be used with each subarray, instead of reading them out of the
*        JCMTState structures associated with the specific supplied subarray.
*        This is because some observations (e.g. 20190529_00045) have
*        blocks of bad angles in some subarrays that correspond to good
*        angles in other arrays. This results in the output time-stream
*        data files being of different lengths for different subarrays.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2015,2016 East Asian Observatory.
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
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"
#include "libsmf/smf_err.h"

#include <gsl/gsl_rng.h>
#include <gsl/gsl_linalg.h>


/* Local data types: */
typedef struct smfFitQUIJobData {
   AstFrameSet *wcs;
   const JCMTState *allstates;
   dim_t *box_starts;
   dim_t b1;
   dim_t b2;
   dim_t box_size;
   dim_t nbolo;
   dim_t ncol;
   double *dat;
   double *ipi;
   double *ipf;
   double *ipq;
   double *ipu;
   double *ipv;
   double *polang;
   double angrot;
   double paoff;
   int harmonic;
   int ipolcrd;
   const char *north;
   int pasign;
   int setbad;
   smf_qual_t *qua;
} smfFitQUIJobData;

typedef struct smfFitQUIJob2Data {
   dim_t nstep;
   dim_t bytestride;
   char *pntstart;
   int isang;
   double *result;
} smfFitQUIJob2Data;

/* Prototypes for local functions */
static void smf1_fit_qui_job( void *job_data, int *status );
static void smf1_fit_qui_job2( void *job_data, int *status );
static void smf1_find_boxes( dim_t intslice, double ang0, dim_t box,
                             dim_t *ontslice, dim_t **box_starts, dim_t *lolim,
                             dim_t *hilim, double *polang, double *scale,
                             int *status );

/* Number of free parameters in the fit */
#define NPAR 10

/* Number of running sums need to calculate the i,q,u fit. */
#define NSUM ( NPAR*(3+NPAR) )/2

/* Macro to simplify resampling of individual JCMTState fields */
#define RESAMPSTATE(member,isangle) \
         p2data = astMalloc( sizeof( *p2data ) ); \
         if( *status == SAI__OK ) { \
            p2data->nstep = box_size; \
            p2data->bytestride = sizeof(JCMTState); \
            p2data->pntstart = (char *) &((instate+istart)->member); \
            p2data->isang = isangle;   \
            p2data->result = &((outstate+itime)->member); \
            thrAddJob( wf, THR__FREE_JOBDATA, p2data, \
                       smf1_fit_qui_job2, 0, NULL, status ); \
         }


void smf_fit_qui( ThrWorkForce *wf, smfData *idata, smfData **odataq,
                  smfData **odatau, smfData **odatai, smfData **odataf,
                  dim_t box, int ipolcrd, int pasign, double paoff,
                  double angrot, const char *north, int harmonic,
                  double ang0, double *polang, int *status ){

/* Local Variables: */
   AstFrameSet *wcs;        /* WCS FrameSet for current time slice */
   JCMTState *instate=NULL; /* Pointer to input JCMTState */
   JCMTState *outstate=NULL;/* Pointer to output JCMTState */
   const char *usesys;      /* Tracking system */
   dim_t *box_starts;       /* Array holding time slice at start of each box */
   dim_t box_size;          /* First time slice in box */
   dim_t bstep;             /* Bolometer step between threads */
   dim_t hilim;             /* Max no. of samples in a box */
   dim_t i;                 /* loop counter */
   dim_t iend;              /* Input time index at start of next fitting box */
   dim_t intslice;          /* ntslice of idata */
   dim_t istart;            /* Input time index at start of fitting box */
   dim_t itime;             /* Time slice index */
   dim_t itstart;           /* Index of first used input time slice */
   dim_t lolim=0;           /* Min no. of samples in a box */
   dim_t nbolo;             /* No. of bolometers */
   dim_t ncol;              /* No. of columns of bolometers in the array */
   dim_t ntime;             /* Time slices to check */
   dim_t ondata;            /* ndata of odata */
   dim_t ontslice;          /* ntslice of odata */
   double *ptr;             /* Pointer to next POL_ANG value */
   double scale;            /* how much longer new samples are */
   int iworker;             /* Index of a worker thread */
   int nodd;                /* No. of strange box lengths found and ignored */
   int nworker;             /* No. of worker threads */
   int setbad;              /* Set all output values bad for this slice? */
   smfData *indksquid=NULL; /* Pointer to input dksquid data */
   smfFitQUIJob2Data *p2data = NULL; /* Pointer to JCMTState resampling data */
   smfFitQUIJobData *job_data = NULL; /* Pointer to all job data */
   smfFitQUIJobData *pdata = NULL;/* Pointer to next job data */
   smfHead *ghdr;           /* Pointer to other data header */
   smfHead *ihdr;           /* Pointer to input data header */
   smfHead *ohdr;           /* Pointer to output data header */
   smf_qual_t *qua;         /* Input quality pointer */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Check supplied arguments. */
   if( !idata || !odataq || !odatau ) {
      *status = SAI__ERROR;
      errRep( "", "smf_fit_qui: NULL inputs supplied", status );
      return;
   }

   if( idata->ndims != 3 ) {
      *status = SAI__ERROR;
      errRep( "", "smf_fit_qui: idata is not 3-dimensional", status );
      return;
   }

   if( harmonic > 15 ) {
      *status = SAI__ERROR;
      errRepf( "", "smf_fit_qui: supplied 'harmonic' value (%d) is illegal",
               status, harmonic );
      return;
   }

/* Ensure the supplied smfData is time-ordered. So "bstride" is 1 and "tstride"
   is nbolo. */
   smf_dataOrder( wf, idata, 1, status );

/* Dimensions of input. */
   smf_get_dims( idata, NULL, &ncol, &nbolo, &intslice, NULL, NULL, NULL,
                 status );

/* Store a pointer to the quality array for the input smfData. */
   qua = smf_select_qualpntr( idata, NULL, status );;

/* Go through the first thousand POL_ANG values to see if they are in
   units of radians (new data) or arbitrary encoder units (old data).
   They are assumed to be in radians if no POL_ANG value is larger than
   20. This function can only handle new data. */
   ihdr = idata->hdr;
   ptr = polang;
   ntime = ( intslice > 1000 ) ? 1000 : intslice;
   for( itime = 0; itime < ntime; itime++,ptr++ ) {
      if( *ptr != VAL__BADD && *ptr > 20 ) {
         *status = SAI__ERROR;
         errRep( " ","   POL2 data contains POL_ANG values in encoder "
                 "units - connot fit to such old data.", status );
         break;
      }
   }

/* Find the input time slice at which each fitting box starts, and the
   length of the output time axis (in time-slices). */
   smf1_find_boxes( intslice, ang0, box, &ontslice, &box_starts, &lolim,
                    &hilim, polang, &scale, status );

/* Adjust intslice to hold the number of used input time slices rather
   than the total number of input time slices. Also record the index of the
   first used input time slice. */
   itstart = box_starts[ 0 ];
   intslice = box_starts[ ontslice ] - itstart;

/* First copy everything from input to output except for the data that needs
   to be downsampled */

/* We want to copy everything in the smfHead except for allState. So we
   make a copy of the allState pointer, and then set it to NULL in the
   header before the copy */
   if( ihdr ) {
     instate = ihdr->allState;
     ihdr->allState = NULL;
   }

/* Similarly, we want everything in the smfDa except for the dksquid. */
   if( idata->da ) {
     indksquid = idata->da->dksquid;
     idata->da->dksquid = NULL;
   }

/* Create copies, storing them in the supplied  output smfData
   structures. Omit the header for U and I, as we will be copying the Q
   header into them.  */
   *odataq = smf_deepcopy_smfData( wf, idata, 0, SMF__NOCREATE_DATA |
                                SMF__NOCREATE_VARIANCE | SMF__NOCREATE_QUALITY,
                                0, 0, status );

   *odatau = smf_deepcopy_smfData( wf, idata, 0, SMF__NOCREATE_DATA |
                                SMF__NOCREATE_VARIANCE | SMF__NOCREATE_QUALITY |
                                SMF__NOCREATE_HEAD, 0, 0, status );

   if( odatai ) {
      *odatai = smf_deepcopy_smfData( wf, idata, 0, SMF__NOCREATE_DATA |
                                SMF__NOCREATE_VARIANCE | SMF__NOCREATE_QUALITY |
                                SMF__NOCREATE_HEAD, 0, 0, status );
   }

/* If created, the smfData to receive the fitted time-streams includes
   the data and original header since it need not be down-sampled. */
   if( odataf ) {
      *odataf = smf_deepcopy_smfData( wf, idata, 0,
                                SMF__NOCREATE_VARIANCE | SMF__NOCREATE_QUALITY,
                                0, 0, status );
   }

/* Restore values in idata now that we're done */
   if( instate ) ihdr->allState = instate;
   if( indksquid ) idata->da->dksquid = indksquid;

/* Store the required length for the output time axis. The time axis is
   axis two because the data is time-ordered. */
   (*odataq)->dims[ 2 ] = ontslice;
   (*odatau)->dims[ 2 ] = ontslice;
   if( odatai) (*odatai)->dims[ 2 ] = ontslice;

/* Get output dimensions - assumed to be the same for all three outputs. */
   ondata = ontslice*idata->dims[0]*idata->dims[1];

/* Allocate the data arrays for the outputs. */
   (*odataq)->pntr[0] = astCalloc( ondata, sizeof(double) );
   (*odatau)->pntr[0] = astCalloc( ondata, sizeof(double) );
   if( odatai ) (*odatai)->pntr[0] = astCalloc( ondata, sizeof(double) );

/* Allocate arrays for the output variances. */
   (*odataq)->pntr[1] = astCalloc( ondata, sizeof(double) );
   (*odatau)->pntr[1] = astCalloc( ondata, sizeof(double) );
   if( odatai ) (*odatai)->pntr[1] = astCalloc( ondata, sizeof(double) );

/* Down-sample all the JCMTState values in the smfHead by selecting the
   input time slice that is closest to the box centre. These approximate
   values will be replaced by more accurate value for the important,
   fast-changing, fields later. */
   ohdr = (*odataq)->hdr;

   ohdr->curframe = (dim_t) (((double) ohdr->curframe + 0.5) / scale);
   ohdr->nframes = ontslice;
   ohdr->steptime *= scale;
   strcpy( ohdr->dlabel, "Q" );
   strncpy( ohdr->title, "POL-2 Stokes parameter Q", SMF__CHARLABEL );

   instate = ihdr->allState;
   if( instate ) {

      ohdr->allState = astCalloc( ontslice, sizeof(*instate) );
      outstate = ohdr->allState;

      if( *status == SAI__OK ) {
         dim_t frame;  /* index of nearest neighbour JCMTState */

         for( i=0; i<ontslice; i++ ) {
            frame = ( box_starts[ i ] + box_starts[ i + 1 ] )/2;
            memcpy( outstate + i, instate + frame, sizeof(*instate) );
         }
      }
   }

/* Create structures used to pass information to the worker threads. */
   nworker = wf ? wf->nworker : 1;
   job_data = astMalloc( nworker*sizeof( *job_data ) );
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

/* Loop round all output time slices. */
      nodd = 0;
      for( itime = 0; itime < ontslice; itime++ ) {

/* Get the index of the first input time slice that contributes to the
   current output time slice. */
         istart = box_starts[ itime ];

/* Get the number of input time slices that contribute to the output time
   slice (iend is the index of the first sample in the next box). */
         iend = box_starts[ itime + 1 ];
         box_size = iend - istart;

/* Down-sample the more important fast-changing fields (like pointing)
   properly. The output value is the average of the input values within
   the box corresponding to the current output time slice. Each value
   is averaged in a different workforce job. Take account of the 0-360
   wrap-around for angular values. */
         RESAMPSTATE( rts_end, 0);

         RESAMPSTATE( smu_az_jig_x, 0);
         RESAMPSTATE( smu_az_jig_y, 0);
         RESAMPSTATE( smu_az_chop_x, 0);
         RESAMPSTATE( smu_az_chop_y, 0);
         RESAMPSTATE( smu_tr_jig_x, 0);
         RESAMPSTATE( smu_tr_jig_y, 0);
         RESAMPSTATE( smu_tr_chop_x, 0);
         RESAMPSTATE( smu_tr_chop_y, 0);

         RESAMPSTATE( tcs_tai, 0);
         RESAMPSTATE( tcs_airmass, 0);

/* Second coordinates (Dec, El etc) cannot wrap 0 to 360 so we do not need
   to test for those cases */
         RESAMPSTATE( tcs_az_ang, 1);
         RESAMPSTATE( tcs_az_ac1, 1);
         RESAMPSTATE( tcs_az_ac2, 0);
         RESAMPSTATE( tcs_az_dc1, 1);
         RESAMPSTATE( tcs_az_dc2, 0);
         RESAMPSTATE( tcs_az_bc1, 1);
         RESAMPSTATE( tcs_az_bc2, 0);

         RESAMPSTATE( tcs_tr_ang, 1);
         RESAMPSTATE( tcs_tr_ac1, 1);
         RESAMPSTATE( tcs_tr_ac2, 0);
         RESAMPSTATE( tcs_tr_dc1, 1);
         RESAMPSTATE( tcs_tr_dc2, 0);
         RESAMPSTATE( tcs_tr_bc1, 1);
         RESAMPSTATE( tcs_tr_bc2, 0);

         RESAMPSTATE( tcs_en_dc1, 1);
         RESAMPSTATE( tcs_en_dc2, 0);

         RESAMPSTATE( tcs_dm_abs, 1);
         RESAMPSTATE( tcs_dm_rel, 0);

/* Wait for all the above jobs to finish. */
         thrWait( wf, status );

/* Check this box is of a usable length. If not, set the box size to zero
   and increment the number of strange boxes. */
         if( box_size < lolim || box_size > hilim ) {
            msgOutiff( MSG__VERB, " ", "%s POL_ANG block: input samples %zu to %zu "
                       "(output sample %zu).", status, (box_size < lolim)?"Short":"Long",
                       istart, box_starts[ itime + 1 ], itime );
            nodd++;
            box_size = 0;
         }

/* If we are using north as the reference direction, get the WCS FrameSet
   for the input time slice that is at the middle of the output time
   slice, and set its current Frame to the requested frame. */
         setbad = 0;
         if( north ) {
            smf_tslice_ast( idata, istart + box_size/2, 1, NO_FTS, status );
            wcs = ihdr->wcs;
            if( !strcmp( north, "TRACKING" ) ) {
               usesys = sc2ast_convert_system( (ihdr->allState)[0].tcs_tr_sys,
                                               status );
            } else {
               usesys = north;
            }

/* If this time slice has not got usable WCS info, set the setbad flag to
   indicate that it should be filled it with bad values. */
            if( wcs ) {
               astSetC( wcs, "System", usesys );
            } else {
               setbad = 1;
            }

         } else {
            wcs = NULL;
         }

/* Now enter the parallel code in which each thread calculates the values
   for a range of bolometers at the current output slice. */
         for( iworker = 0; iworker < nworker; iworker++ ) {
            pdata = job_data + iworker;

            pdata->dat = ((double *) idata->pntr[0] ) + istart*nbolo;
            pdata->qua = qua + istart*nbolo;
            pdata->allstates = ihdr->allState + istart;

            pdata->ipi = odatai ? ( (double*) (*odatai)->pntr[0] ) + itime*nbolo : NULL;
            pdata->ipf = odataf ? ( (double*) (*odataf)->pntr[0] ) + istart*nbolo : NULL;
            pdata->ipq = ( (double*) (*odataq)->pntr[0] ) + itime*nbolo;
            pdata->ipu = ( (double*) (*odatau)->pntr[0] ) + itime*nbolo;
            pdata->ipv = ( (double*) (*odataq)->pntr[1] ) + itime*nbolo;

            pdata->setbad = setbad;
            pdata->nbolo = nbolo;
            pdata->ncol = ncol;
            pdata->box_size = box_size;
            pdata->ipolcrd = ipolcrd;
            pdata->pasign = pasign ? +1: -1;
            pdata->paoff = paoff;
            pdata->angrot = angrot;
            pdata->polang = polang + istart;
            pdata->harmonic = harmonic;
            if( wcs ) {
               pdata->wcs = astCopy( wcs );
               astUnlock( pdata->wcs, 1 );
            } else {
               pdata->wcs = NULL;
            }

/* Pass the job to the workforce for execution. */
            thrAddJob( wf, THR__REPORT_JOB, pdata, smf1_fit_qui_job, 0, NULL,
                         status );
         }

/* Wait for the workforce to complete all jobs. */
         thrWait( wf, status );

/* Lock and annul the AST objects used by each thread. */
         if( wcs ) {
            for( iworker = 0; iworker < nworker; iworker++ ) {
               pdata = job_data + iworker;
               astLock( pdata->wcs, 0 );
               pdata->wcs = astAnnul( pdata->wcs );
            }
         }
      }

      if( nodd ) {
         msgOutf( "", "WARNING: %d block(s) of POL_ANG values had unusual lengths "
                  "(i.e. outside the range of %zu to %zu samples) and were ignored.",
                  status, nodd, lolim, hilim );
      }

/* Add a keyword to the Q header indicating the polarimetric reference
   direction. */
      smf_fits_updateS( ohdr, "POLNORTH", north ? north : "FPLANE",
                        north ? "Pol ref dir is tracking north" :
                                "Pol ref dir is focal plane Y", status );

/* Copy the Q header to the other outputs. */
      ghdr = smf_deepcopy_smfHead( ohdr, status );
      (*odatau)->hdr = ghdr;
      if( *status == SAI__OK ) {
         strcpy( ghdr->dlabel, "U" );
         strncpy( ghdr->title, "POL-2 Stokes parameter U", SMF__CHARLABEL );
      }

      if( odatai ) {
         ghdr = smf_deepcopy_smfHead( ohdr, status );
         (*odatai)->hdr = ghdr;
         if( *status == SAI__OK ) {
            strcpy( ghdr->dlabel, "I" );
            strncpy( ghdr->title, "POL-2 Stokes parameter I", SMF__CHARLABEL );
         }
      }
   }

/* Copy the variances from the Q smfData into the U and (and I) smfData. */
   if( *odataq && *status == SAI__OK ) {
      if( *odatau ) {
         memcpy( (*odatau)->pntr[1], (*odataq)->pntr[1], ondata*sizeof(double));
      }
      if( odatai && *odatai ) {
         memcpy( (*odatai)->pntr[1], (*odataq)->pntr[1], ondata*sizeof(double));
      }
   }

/* Ensure all smfDatas are time-ordered. */
   smf_dataOrder( wf, idata, 1, status );
   if( odatai && *odatai ) smf_dataOrder( wf, *odatai, 1, status );
   if( *odataq ) smf_dataOrder( wf, *odataq, 1, status );
   if( *odatau ) smf_dataOrder( wf, *odatau, 1, status );

/* Free resources. */
   job_data = astFree( job_data );
   box_starts = astFree( box_starts );
}

static void smf1_fit_qui_job( void *job_data, int *status ) {
/*
*  Name:
*     smf1_fit_qui_job

*  Purpose:
*     Calculate I, Q and U for a block of bolometers.

*  Invocation:
*     void smf1_fit_qui_job( void *job_data, int *status )

*  Arguments:
*     job_data = void * (Given)
*        Pointer to the data needed by the job. Should be a pointer to a
*        smfFitQUIJobData structure.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine calculate the I, Q and U values for each bolometer in
*     a block of bolometers. It runs within a thread instigated by
*     smf_fit_qui.

*/

/* Local Variables: */
   AstFrameSet *wcs;          /* WCS FrameSet for current time slice */
   AstMapping *g2s;           /* GRID to SKY mapping */
   AstMapping *s2f;           /* SKY to focal plane mapping */
   const JCMTState *allstates;/* Pointer to array of JCMTState structures */
   const JCMTState *state;    /* JCMTState info for current time slice */
   dim_t b1;                  /* First bolometer index */
   dim_t b2;                  /* Last bolometer index */
   dim_t box_size;            /* NFirst time slice in box */
   dim_t ibolo;               /* Bolometer index */
   dim_t ibox;
   dim_t nbolo;               /* Total number of bolometers */
   dim_t ncol;
   double *dat;               /* Pointer to start of input data values */
   double *din;               /* Pointer to input data array for bolo/time */
   double *ipf;               /* Pointer to output fit array */
   double *ipi;               /* Pointer to output I array */
   double *ipq;               /* Pointer to output Q array */
   double *ipu;               /* Pointer to output U array */
   double *ipv;               /* Pointer to output weights array */
   double *pfit;              /* Pointer to output fit array */
   double *pm;
   double *ptr;
   double *polang;
   double *ps;
   double angle;              /* Phase angle for FFT */
   double angrot;             /* Angle from focal plane X axis to fixed analyser */
   double c1;
   double c2;
   double c4;
   double c8;
   double cosval;             /* Cos of angrot */
   double fit;
   double fx[2];              /* Focal plane X coord at bolometer and northern point*/
   double fy[2];              /* Focal plane Y coord at bolometer and northern point*/
   double gx;                 /* GRID X coord at bolometer */
   double gy;                 /* GRID Y coord at bolometer */
   double matrix[ NPAR*NPAR ];
   double paoff;              /* WPLATE value corresponding to POL_ANG=0.0 */
   double phi;                /* Angle from fixed analyser to effective analyser */
   double qval;
   double res;
   double s1;                 /* Sum of weighted cosine terms */
   double s2;                 /* Sum of weighted sine terms */
   double s4;
   double s8;
   double sinval;             /* Sin of angrot */
   double solution[ NPAR ];
   double sum1;               /* Sum of squared residuals */
   double sums[NSUM];         /* Sum of bolometer values */
   double sx[2];              /* SKY X coord at bolometer and northern point*/
   double sy[2];              /* SKY Y coord at bolometer and northern point*/
   double tr_angle;
   double twophi;
   double uval;
   double vector[ NPAR ];
   double wplate;             /* Angle from fixed analyser to have-wave plate */
   gsl_matrix_view gsl_m;
   gsl_vector_view gsl_b;
   gsl_vector_view gsl_x;
   int harmonic;
   int ipolcrd;               /* Reference direction for pol_ang */
   int nsum1;
   int pasign;                /* +1 or -1 indicating sense of POL_ANG value */
   smfFitQUIJobData *pdata;   /* Pointer to job data */
   smf_qual_t *qin;           /* Pointer to input quality array for bolo/time */
   smf_qual_t *qua;           /* Pointer to start of input quality values */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an AST context */
   astBegin;

/* Create views of the matrix and vector buffers that can be used by GSL. */
   gsl_m = gsl_matrix_view_array( matrix, NPAR, NPAR );
   gsl_b = gsl_vector_view_array( vector, NPAR );
   gsl_x = gsl_vector_view_array( solution, NPAR );

/* Get a pointer to the job data, and then extract its contents into a
   set of local variables. */
   pdata = (smfFitQUIJobData *) job_data;

   b1 = pdata->b1;
   b2 = pdata->b2;
   nbolo = pdata->nbolo;
   ncol = pdata->ncol;

   dat = pdata->dat + b1;
   qua = pdata->qua + b1;
   allstates = pdata->allstates;
   pfit = pdata->ipf ? pdata->ipf + b1 : NULL;

   ipi = pdata->ipi ? pdata->ipi + b1 : NULL;
   ipq = pdata->ipq + b1;
   ipu = pdata->ipu + b1;
   ipv = pdata->ipv + b1;

   ipolcrd = pdata->ipolcrd;
   pasign = pdata->pasign;
   paoff = pdata->paoff;
   angrot = pdata->angrot;
   box_size = pdata->box_size;
   harmonic = pdata->harmonic;
   polang = pdata->polang;

   wcs = pdata->wcs;
   if( wcs ) {
      astLock( wcs, 0 );

/* Get the mapping from GRID to SKY. */
      g2s = astSimplify( astGetMapping( wcs, AST__BASE, AST__CURRENT ));

/* Get the mapping from SKY to focal plane (x,y) (the index of the FPLANE
   Frame is fixed at 3 by file sc2ast.c). */
      s2f = astSimplify( astGetMapping( wcs, AST__CURRENT, 3 ) );

   } else{
      g2s = s2f = NULL;
   }

/* Check we have something to do. */
   if( b1 < nbolo && *status == SAI__OK ) {

/* Loop round all bolometers to be processed by this thread. */
      for( ibolo = b1; ibolo <= b2; ibolo++,qua++,dat++ ) {

/* If the returned Stokes parameters are to be with respect to North, get
   the angle from north at the current bolometer to focal plane Y, measured
   positive in the sense of rotation from focal plane Y to focal plane X
   (note this angle may change across the focal plane due to focal plane
   distortion). Otherwise, use zero. */
         if( wcs ) {

/* Get the grid coords of the current bolometer, and transform them to SKY
   coordinates using the FrameSet. */
            gx = ibolo % ncol + 1;
            gy = ibolo / ncol + 1;
            astTran2( g2s, 1, &gx, &gy, 1, sx, sy );

/* Increment the sky position slightly to the north. */
            sx[ 1 ] = sx[ 0 ];
            sy[ 1 ] = sy[ 0 ] + 1.0E-6;

/* Transform both sky positions into focal plane coords. */
            astTran2( s2f, 2, sx, sy, 1, fx, fy );

/* Get the angle from north to focal plane Y, measured positive in the
   sense of rotation from focal plane Y to focal plane X. */
            if( fx[0] != VAL__BADD && fy[0] != VAL__BADD &&
                fx[1] != VAL__BADD && fy[1] != VAL__BADD ) {
               tr_angle = atan2( fx[0] - fx[1], fy[1] - fy[0] );
            } else {
               tr_angle = VAL__BADD;
            }

         } else {
            tr_angle = 0.0;
         }

/* If the whole bolometer is bad, put bad values into the outputs. */
         if( pdata->setbad || *qua & SMF__Q_BADB || tr_angle == VAL__BADD || box_size == 0 ) {
            if( ipi ) *(ipi++) = VAL__BADD;
            *(ipq++) = VAL__BADD;
            *(ipu++) = VAL__BADD;
            *(ipv++) = VAL__BADD;

/* If the bolometer is good, calculate and store the output i, q and u
   values. */
         } else {

/* Initialise pointers to the first input data value, quality value and
   state info to be used in the current fitting box. */
            din = dat;
            qin = qua;
            state = allstates;
            ptr = polang;

/* Form the sums needed to calculate the best fit Q, U and I. This
   involves looping over all input samples that fall within the fitting box
   centred on the current output sample. The 44 sums are stored in the
   "sums" array. Initialise it to hold zeros.  */
            memset( sums, 0, NSUM*sizeof(*sums) );
            for( ibox = 0; ibox <  box_size; ibox++,state++,ptr++ ) {

/* Get the POL_ANG value for this time slice. */
               angle = *ptr;

/* Check the input sample has not been flagged during cleaning and is
   not bad. */
               if( state->jos_drcontrol == 0 && !( *qin & SMF__Q_FIT ) &&
                   *din != VAL__BADD && angle != VAL__BADD ) {

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
                     errRepf( "", "smf_fit_qui: currently only POL_CRD = "
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
                  twophi = 2*phi;

/* Form the trig values needed for the sums. */
                  s8 = sin( 2*twophi );
                  c8 = cos( 2*twophi );
                  s4 = sin( twophi );
                  c4 = cos( twophi );
                  s2 = sin( phi );
                  c2 = cos( phi );
                  s1 = sin( wplate );
                  c1 = cos( wplate );

/* Update the sums. The order of the following lines define the index
   within "sums" at which each sum is stored. */
                  ps = sums;
                  *(ps++) += s4*s4;
                  *(ps++) += s4*c4;
                  *(ps++) += s4*s2;
                  *(ps++) += s4*c2;
                  *(ps++) += s4*s1;
                  *(ps++) += s4*c1;
                  *(ps++) += s4*ibox;
                  *(ps++) += s4;
                  *(ps++) += s4*(*din);

                  *(ps++) += s2*c4;
                  *(ps++) += s2*s2;
                  *(ps++) += s2*c2;
                  *(ps++) += s2*s1;
                  *(ps++) += s2*c1;
                  *(ps++) += s2*ibox;
                  *(ps++) += s2;
                  *(ps++) += s2*(*din);

                  *(ps++) += s1*c4;
                  *(ps++) += s1*c2;
                  *(ps++) += s1*s1;
                  *(ps++) += s1*c1;
                  *(ps++) += s1*ibox;
                  *(ps++) += s1;
                  *(ps++) += s1*(*din);

                  *(ps++) += c4*c4;
                  *(ps++) += c4*c2;
                  *(ps++) += c4*c1;
                  *(ps++) += c4*ibox;
                  *(ps++) += c4;
                  *(ps++) += c4*(*din);

                  *(ps++) += c2*c2;
                  *(ps++) += c2*c1;
                  *(ps++) += c2*ibox;
                  *(ps++) += c2;
                  *(ps++) += c2*(*din);

                  *(ps++) += c1*c1;
                  *(ps++) += c1*ibox;
                  *(ps++) += c1;
                  *(ps++) += c1*(*din);

                  *(ps++) += ibox*ibox;
                  *(ps++) += ibox;
                  *(ps++) += ibox*(*din);

                  *(ps++) += 1.0;
                  *(ps++) += *din;

                  *(ps++) += s4*s8;
                  *(ps++) += s4*c8;

                  *(ps++) += s2*s8;
                  *(ps++) += s2*c8;

                  *(ps++) += s1*s8;
                  *(ps++) += s1*c8;

                  *(ps++) += s8*c4;
                  *(ps++) += s8*c2;
                  *(ps++) += s8*c1;
                  *(ps++) += s8*ibox;
                  *(ps++) += s8;
                  *(ps++) += s8*(*din);
                  *(ps++) += s8*s8;
                  *(ps++) += s8*c8;

                  *(ps++) += c4*c8;

                  *(ps++) += c2*c8;

                  *(ps++) += c1*c8;

                  *(ps++) += c8*ibox;
                  *(ps++) += c8;
                  *(ps++) += c8*(*din);
                  *(ps++) += c8*c8;
               }

               din += nbolo;
               qin += nbolo;

            }

/* Now find the parameters of the best fit. First check that there were
   sufficient good samples in the fitting box. */
            if( sums[42] > 0.8*box_size ) {

/* Copy the sums to the correct elements of the 10x10 matrix. */
               pm = matrix;
               *(pm++) = sums[ 0 ];
               *(pm++) = sums[ 1 ];
               *(pm++) = sums[ 2 ];
               *(pm++) = sums[ 3 ];
               *(pm++) = sums[ 4 ];
               *(pm++) = sums[ 5 ];
               *(pm++) = sums[ 6 ];
               *(pm++) = sums[ 7 ];
               *(pm++) = sums[ 44 ];
               *(pm++) = sums[ 45 ];


               *(pm++) = sums[ 1 ];
               *(pm++) = sums[ 24 ];
               *(pm++) = sums[ 9 ];
               *(pm++) = sums[ 25 ];
               *(pm++) = sums[ 17 ];
               *(pm++) = sums[ 26 ];
               *(pm++) = sums[ 27 ];
               *(pm++) = sums[ 28 ];
               *(pm++) = sums[ 50 ];
               *(pm++) = sums[ 58 ];

               *(pm++) = sums[ 2 ];
               *(pm++) = sums[ 9 ];
               *(pm++) = sums[ 10 ];
               *(pm++) = sums[ 11 ];
               *(pm++) = sums[ 12 ];
               *(pm++) = sums[ 13 ];
               *(pm++) = sums[ 14 ];
               *(pm++) = sums[ 15 ];
               *(pm++) = sums[ 46 ];
               *(pm++) = sums[ 47 ];

               *(pm++) = sums[ 3 ];
               *(pm++) = sums[ 25 ];
               *(pm++) = sums[ 11 ];
               *(pm++) = sums[ 30 ];
               *(pm++) = sums[ 18 ];
               *(pm++) = sums[ 31 ];
               *(pm++) = sums[ 32 ];
               *(pm++) = sums[ 33 ];
               *(pm++) = sums[ 51 ];
               *(pm++) = sums[ 59 ];

               *(pm++) = sums[ 4 ];
               *(pm++) = sums[ 17 ];
               *(pm++) = sums[ 12 ];
               *(pm++) = sums[ 18 ];
               *(pm++) = sums[ 19 ];
               *(pm++) = sums[ 20 ];
               *(pm++) = sums[ 21 ];
               *(pm++) = sums[ 22 ];
               *(pm++) = sums[ 48 ];
               *(pm++) = sums[ 49 ];

               *(pm++) = sums[ 5 ];
               *(pm++) = sums[ 26 ];
               *(pm++) = sums[ 13 ];
               *(pm++) = sums[ 31 ];
               *(pm++) = sums[ 20 ];
               *(pm++) = sums[ 35 ];
               *(pm++) = sums[ 36 ];
               *(pm++) = sums[ 37 ];
               *(pm++) = sums[ 52 ];
               *(pm++) = sums[ 60 ];

               *(pm++) = sums[ 6 ];
               *(pm++) = sums[ 27 ];
               *(pm++) = sums[ 14 ];
               *(pm++) = sums[ 32 ];
               *(pm++) = sums[ 21 ];
               *(pm++) = sums[ 36 ];
               *(pm++) = sums[ 39 ];
               *(pm++) = sums[ 40 ];
               *(pm++) = sums[ 53 ];
               *(pm++) = sums[ 61 ];

               *(pm++) = sums[ 7 ];
               *(pm++) = sums[ 28 ];
               *(pm++) = sums[ 15 ];
               *(pm++) = sums[ 33 ];
               *(pm++) = sums[ 22 ];
               *(pm++) = sums[ 37 ];
               *(pm++) = sums[ 40 ];
               *(pm++) = sums[ 42 ];
               *(pm++) = sums[ 54 ];
               *(pm++) = sums[ 62 ];

               *(pm++) = sums[ 44 ];
               *(pm++) = sums[ 50 ];
               *(pm++) = sums[ 46 ];
               *(pm++) = sums[ 51 ];
               *(pm++) = sums[ 48 ];
               *(pm++) = sums[ 52 ];
               *(pm++) = sums[ 53 ];
               *(pm++) = sums[ 54 ];
               *(pm++) = sums[ 56 ];
               *(pm++) = sums[ 57 ];

               *(pm++) = sums[ 45 ];
               *(pm++) = sums[ 58 ];
               *(pm++) = sums[ 47 ];
               *(pm++) = sums[ 59 ];
               *(pm++) = sums[ 49 ];
               *(pm++) = sums[ 60 ];
               *(pm++) = sums[ 61 ];
               *(pm++) = sums[ 62 ];
               *(pm++) = sums[ 57 ];
               *(pm++) = sums[ 64 ];

/* Copy the remaining sums to the correct elements of the 8 vector. */
               pm = vector;
               *(pm++) = sums[ 8 ];
               *(pm++) = sums[ 29 ];
               *(pm++) = sums[ 16 ];
               *(pm++) = sums[ 34 ];
               *(pm++) = sums[ 23 ];
               *(pm++) = sums[ 38 ];
               *(pm++) = sums[ 41 ];
               *(pm++) = sums[ 43 ];
               *(pm++) = sums[ 55 ];
               *(pm++) = sums[ 63 ];

/* Find the solution to the 10x10 set of linear equations. The matrix is
   symmetric and positive-definite so use Cholesky decomposition.  */
               memset( solution, 0, NPAR*sizeof(*solution) );
               gsl_set_error_handler_off();
               if( gsl_linalg_cholesky_decomp( &gsl_m.matrix ) != 0 ) {
                  *status = SAI__ERROR;
                  errRepf( "", "smf_fit_qui: Error returned by "
                          "gsl_linalg_cholesky_decomp (bolo %zu)", status, ibolo );
               } else if( gsl_linalg_cholesky_solve( &gsl_m.matrix, &gsl_b.vector,
                                                     &gsl_x.vector ) != 0 ) {
                  *status = SAI__ERROR;
                  errRepf( "", "smf_fit_qui: Error returned by "
                          "gsl_linalg_cholesky_solve  (bolo %zu)", status, ibolo );
               }

/* Get the required Q/U values. Which coefficients to use depends on the
   requested harmonic (usually 4). The "solution" array holds A, B, C,
   etc, in order (see prologue description). */
               if( harmonic & 4 ) {
                  qval = 2*solution[ 1 ];
                  uval = 2*solution[ 0 ];
               } else {
                  qval = 0.0;
                  uval = 0.0;
               }

               if( harmonic & 1 ) {
                  qval += 2*solution[ 5 ];
                  uval += 2*solution[ 4 ];
               }

               if( harmonic & 2 ) {
                  qval += 2*solution[ 3 ];
                  uval += 2*solution[ 2 ];
               }

               if( harmonic & 8 ) {
                  qval += 2*solution[ 9 ];
                  uval += 2*solution[ 8 ];
               }

/* Modify Q and U so they use the requested reference direction, and store in
   the output arrays. */
               cosval = cos( 2*( angrot - tr_angle ) );
               sinval = sin( 2*( angrot - tr_angle ) );
               *(ipq++) = -qval*cosval + uval*sinval;
               *(ipu++) = -qval*sinval - uval*cosval;

/* Store the correspoinding I value. */
               if( ipi ) *(ipi++) = 2*( solution[ 6 ]*(box_size/2) + solution[ 7 ] );

/* Loop over the data again in the same way to calculate the variance of the
   residuals between the above fit and the supplied data. */
               din = dat;
               qin = qua;
               state = allstates;
               ipf = pfit;
               ptr = polang;

               sum1 = 0.0;
               nsum1 = 0;

               for( ibox = 0; ibox <  box_size; ibox++,state++,ptr++ ) {
                  angle = *ptr;

                  if( !( *qin & SMF__Q_FIT ) && *din != VAL__BADD &&
                        angle != VAL__BADD ) {
                     wplate = pasign*angle + paoff;
/*
                        if( ipolcrd == 1 ) {
                           wplate += state->tcs_az_ang;
                        } else if( ipolcrd == 2 ) {
                           wplate += state->tcs_tr_ang;
                        }
*/
                     phi = 2*wplate;
                     twophi = 2*phi;

                     s8 = sin( 2*twophi );
                     c8 = cos( 2*twophi );
                     s4 = sin( twophi );
                     c4 = cos( twophi );
                     s2 = sin( phi );
                     c2 = cos( phi );
                     s1 = sin( wplate );
                     c1 = cos( wplate );

                     fit = solution[0]*s4 +
                           solution[1]*c4 +
                           solution[2]*s2 +
                           solution[3]*c2 +
                           solution[4]*s1 +
                           solution[5]*c1 +
                           solution[6]*ibox +
                           solution[7] +
                           solution[8]*s8 +
                           solution[9]*c8;

                     res = *din - fit;
                     if( pfit ) *ipf = fit;

                     sum1 += res*res;
                     nsum1++;

                  } else if( pfit ) {
                     *ipf = VAL__BADD;
                  }

                  if( pfit ) ipf += nbolo;
                  din += nbolo;
                  qin += nbolo;
               }

/* Calculate the variance of the residuals, and then scale it to get the
   notional variance for the returned Q,. U and I values. The scaling
   factor is determined emprically to get reasonable agreement between these
   notional variances and the noise actually seen in the Q and U values
   for 10 test observations. The reason for storing these as Q/U variances
   rather than as a weights component in the SMURF extension is so that
   makemap can pick them up easily and use them to initialise the NOI
   model, which is used for weighting the bolometer data when forming the
   COM model on the first iteration. */
               *(ipv++) = 0.0253*sum1/nsum1;

/* Store bad values if there were too few good samples in the fitting
   box. */
            } else {
               if( ipi ) *(ipi++) = VAL__BADD;
               *(ipq++) = VAL__BADD;
               *(ipu++) = VAL__BADD;
               *(ipv++) = VAL__BADD;

               if( pfit ) {
                  ipf = pfit;

                  for( ibox = 0; ibox <  box_size; ibox++,state++ ) {
                     *ipf = VAL__BADD;
                     ipf += nbolo;
                  }
               }
            }
         }
         if( pfit ) pfit++;
      }
   }

   if( wcs ) {
      g2s = astAnnul( g2s );
      s2f = astAnnul( s2f );
      astUnlock( wcs, 1 );
   }

/* End the AST context */
   astEnd;
}


static void smf1_find_boxes( dim_t intslice, double ang0, dim_t box,
                             dim_t *ontslice, dim_t **box_starts, dim_t *lolim,
                             dim_t *hilim, double *polang, double *scale,
                             int *status ) {
/*
*  Name:
*     smf1_find_boxes

*  Purpose:
*     Find the length of the output time axis, and find the input time slices
*     at the start and end of each fitting box.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     void smf1_find_boxes( dim_t intslice, double ang0, dim_t box,
*                           dim_t *ontslice, dim_t **box_starts, dim_t *lolim,
*                           dim_t *hilim, double *polang, double *scale,
*                           int *status )

*  Arguments:
*     intslice = dim_t (Given)
*        Number of time slices in input.
*     ang0 = double (Given)
*        The HWP angle, in radians, at which each fitting box starts.
*     box = dim_t (Given)
*        Number of complete HWP rotations per fitting box.
*     ontslice = dim_t * (Returned)
*        Number of time slices in output. There is one output time slice
*        for each fitting box.
*     box_starts = dim_t ** (Returned)
*        Pointer to a newly allocated array holding the input time slice
*        index at which each fitting box starts. This array has length
*        equal to (*ontslice) + 1  - the extra last element is the index
*        of the first input time slice beyond the end of the last fitting
*        box.
*     lolim = dim_t * (Returned)
*        The lowest box length that should be used, as a number of time
*        slices.
*     hilim = dim_t * (Returned)
*        The highest box length that should be used, as a number of time
*        slices.
*     polang = double * (Given)
*        Array of POL_ANG values to be used.
*     scale = double * (Given)
*        Returned holding the mean number of input slices per output
*        slice, averaged over just those output slices with lengths
*        between *lolim and *hilim.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine determines the start and end of each box over which
*     to perform a least squares fit. It is needed because the sampling
*     rate and half-waveplate (HWP) rotation speed are not necessarily
*     constant.

*/

/* Local Variables: */
   char more;
   dim_t length;
   dim_t iel;
   dim_t itime;
   dim_t nrot;
   double *ptr;
   double s1;
   double s2;

/* Initialise returned values. */
   *ontslice = 0;
   *box_starts = NULL;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* If the HWP angle is above "ang0", move on until the HWP angle wraps back to
   zero. */
   nrot = 0;
   itime = 0;
   ptr = polang;
   more = 1;
   while( more && ( *ptr == VAL__BADD || *ptr > ang0 ) ) {
      if( ++itime == intslice ) more = 0;
      ptr++;
   }

/* HWP angles vary between 0 and 2*PI in a roughly linear manner, but
   with some sudden steps. When the HWP angle reaches 2*PI it wraps
   back round to zero. Find the first time slice for which the HWP
   angle is greater than "ang0" (a safe value in the middle of the range). */
   while( more && ( *ptr == VAL__BADD || *ptr <= ang0 ) ) {
      if( ++itime == intslice ) more = 0;
      ptr++;
   }

/* This is the start of the first box. */
   if( more ) {
      iel = (*ontslice)++;
      *box_starts = astGrow( *box_starts, *ontslice, sizeof( **box_starts ) );
      if( *status == SAI__OK ) (*box_starts)[ iel ] = itime;
   }

/* Loop over all rotations. */
   s1 = 0.0;
   s2 = 0.0;
   while( 1 ) {

/* Move on until the HWP angle wraps round back to zero. */
      while( more && ( *ptr == VAL__BADD || *ptr > ang0 ) ) {
         if( ++itime == intslice ) more = 0;
         ptr++;
      }

/* Move on until the HWP angle again exceeds "ang0". */
      while( more && ( *ptr == VAL__BADD || *ptr <= ang0 ) ) {
         if( ++itime == intslice ) more = 0;
         ptr++;
      }

/* Break if we have reached the end of the time line. */
      if( !more ) break;

/* Increment the number of HWP rotations covered so far. */
      nrot++;

/* If the box now encompasses the correct number of rotations, mark the
   current time slice as the start of the next box. */
      if( nrot == box ) {
         nrot = 0;
         iel = (*ontslice)++;
         *box_starts = astGrow( *box_starts, *ontslice, sizeof( **box_starts ) );
         if( *status == SAI__OK ) {
            (*box_starts)[ iel ] = itime;
            if( iel > 0 ) {
               length = itime-(*box_starts)[iel-1];
               s1 += length;
               s2 += length*length;
            }
         }
      }
   }

/* The number of output time slices will be one less than the number of
   values in "box_Starts", because the last value in "box_starts" is the end
   of the last box. */
   (*ontslice)--;

/* Find the mean and standard deviation of the box lengths (measured as a
   number of time slices). */
   s1 /= (*ontslice);
   s2 = s2/(*ontslice) - s1*s1;
   s2 = ( s2 > 0.0 ) ? sqrt( s2 ) : 0.0;

   msgOutiff( MSG__VERB, " ", "Mean POL_ANG block length: %g", status, s1 );
   msgOutiff( MSG__VERB, " ", "Sigma of POL_ANG block lengths: %g", status, s2 );

/* Return the limits for usable box lengths. */
   *lolim = (dim_t) ( s1 - 3.0*s2 );
   *hilim = (dim_t) ( s1 + 3.0*s2 );

/* Find and return the mean length of the usable boxes. */
   s1 = 0.0;
   s2 = 0.0;
   for( iel = 0; iel < *ontslice; iel++ ){
      length = (*box_starts)[ iel + 1 ] - (*box_starts)[ iel ];
      if( length >= *lolim && length <= *hilim ){
         s1 += length;
         s2 += 1.0;
      }
   }
   *scale = s1/s2;
   msgOutiff( MSG__VERB, " ", "Mean POL_ANG block length (excluding unusual blocks): %g",
              status, *scale );
}




static void smf1_fit_qui_job2( void *job_data, int *status ) {
/*
*  Name:
*     smf1_fit_qui_job2

*  Purpose:
*     Find the average value of a single JCMTState variable over a
*     single fitting box.

*  Invocation:
*     void smf1_fit_qui_job( void *job_data, int *status )

*  Arguments:
*     job_data = void * (Given)
*        Pointer to the data needed by the job. Should be a pointer to a
*        smfFitQUIJob2Data structure.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine calculate the average value of a single JCMTState
*     variable over a single fitting box.

*/

/* Local Variables: */
   char *pnt;
   double centre;
   double sum;
   double val;
   dim_t bytestride;
   dim_t istep;
   dim_t nstep;
   dim_t nsum;
   smfFitQUIJob2Data *pdata;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer to the structure holding the required input variables. */
   pdata = (smfFitQUIJob2Data *) job_data;

/* Extract some commonly used variables into local variables */
   nstep = pdata->nstep;
   bytestride = pdata->bytestride;

/* Initialise running sumes etc needed to form the average. */
   sum = 0.0;
   nsum = 0;
   pnt = pdata->pntstart;

/* If the values being averaged are angular values, we need to take
   account of wrap-around form 360 degs to zero degs. */
   if( pdata->isang ) {

/* Every iunput value is normalised into a range which is within +/- 180
   degs of the first good value (the "centre" value). Indicate we have not
   yet found a good centre value. Then loop through all the input values. */
      centre = VAL__BADD;
      for( istep = 0; istep < nstep; istep++ ) {

/* Get the current input value. The strid ebetween adjacent values may
   not be a whole number of doubles, so we use a "char *" pointer to
   point to each value, which can be incremented by an arbitrary number
   of bytes. */
         val = *((double *)pnt);
         if( val != VAL__BADD ) {

/* If this is the first good value, use it as the centre of the angular
   range into which all other values are to be normalised. */
            if( centre == VAL__BADD ) {
               centre = val;

/* If this is not the first good value, add multiples of 360 degrees
   until the value is within +/- 180 degrees of the centre value. */
            } else {
               while( val > centre + AST__DPI ) val -= 2*AST__DPI;
               while( val <= centre - AST__DPI ) val += 2*AST__DPI;
            }

/* Increment the running sums. */
            sum += val;
            nsum++;
         }

/* Move the "char *" pointer on to the next value. */
         pnt += bytestride;
      }

/* If the values being averaged are not angular values, just do a simple
   average. */
   } else {
      for( istep = 0; istep < nstep; istep++ ) {
         val = *((double *)pnt);
         if( val != VAL__BADD ) {
            sum += val;
            nsum++;
         }
         pnt += bytestride;
      }
   }

/* Form the returned average value. */
   if( nsum > 0 ) {
      *(pdata->result) = sum/nsum;
   } else {
      *(pdata->result) = VAL__BADD;
   }
}


