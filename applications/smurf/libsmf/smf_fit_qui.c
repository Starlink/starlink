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
*                  smfData **odatau, smfData **odatai, dim_t box,
*                  int ipolcrd, int pasign, double paoff, double angrot,
*                  int north, double **weights, int *status )

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
*     north = int (Given)
*        If non-zero, the returned Q/U values use north in the tracking
*        system as their reference direction. Otherwise, the reference
*        direction is the focal plane Y axis.
*     weights = double ** (Returned)
*        Address of a pointer in which to return a pointer to a newly
*        allocated array holding the weight associated with each element
*        of each of the three returned smfDatas. The same weights should
*        be used with all three returned smfDatas. Each weight is the
*        reciprocal of the variance of the residuals between the supplied
*        time stream data and the least squares fit.
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
*     slice into the box):
*
*        y = A*sin(4*w) + B*cos(4*w) + C*sin(2*w) + D*cos(2*w) +
*            E*sin(w) + F*cos(w) + G*itime + H + J*sin(8*w) + K*cos(8*w)
*
*     The returned Q, U and I values are then:
*
*        U = 2*A
*        Q = 2*B
*        I = 2*( G*box/2 + H )
*
*     The Q and U values are specified with respect to either tracking
*     north, or focal plane Y axis (see argument "north").
*
*     Care is taken to ensure that each fitting box spans exactly the same
*     range of "w" values. This is needed because the POL_ANG values are
*     not exactly regular.

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
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2015 East Asian Observatory.
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
   const JCMTState *allstates;
   dim_t b1;
   dim_t b2;
   dim_t nbolo;
   dim_t intslice;
   dim_t ontslice;
   double *dat;
   double *ipi;
   double *ipq;
   double *ipu;
   double *ipw;
   double angrot;
   double paoff;
   dim_t *box_starts;
   int ipolcrd;
   int north;
   int pasign;
   smf_qual_t *qua;
} smfFitQUIJobData;

/* Prototypes for local functions */
static void smf1_fit_qui_job( void *job_data, int *status );
static void smf1_find_boxes( dim_t intslice, const JCMTState *allstates, dim_t box,
                             dim_t *ontslice, dim_t **box_starts, int *status );

/* Number of free parameters in the fit */
#define NPAR 10

/* Number of running sums need to calculate the i,q,u fit. */
#define NSUM ( NPAR*(3+NPAR) )/2

/* Macro to simplify resampling of individual JCMTState fields */
#define RESAMPSTATE(in,out,member,intslice,ontslice,isang) smf_downsamp1D( wf, &(in->member),sizeof(JCMTState),1,intslice,&(out->member), sizeof(JCMTState),1,ontslice,1,1,isang,status );


void smf_fit_qui( ThrWorkForce *wf, smfData *idata, smfData **odataq,
                  smfData **odatau, smfData **odatai, dim_t box, int ipolcrd,
                  int pasign, double paoff, double angrot, int north,
                  double **weights, int *status ){

/* Local Variables: */
   JCMTState *instate=NULL; /* Pointer to input JCMTState */
   JCMTState *outstate=NULL;/* Pointer to output JCMTState */
   dim_t *box_starts;       /* Array holding time slice at start of each box */
   dim_t intslice;          /* ntslice of idata */
   dim_t itime;             /* Time slice index */
   dim_t nbolo;             /* No. of bolometers */
   dim_t ntime;             /* Time slices to check */
   dim_t ondata;            /* ndata of odata */
   dim_t ontslice;          /* ntslice of odata */
   double scale;            /* how much longer new samples are */
   int bstep;               /* Bolometer step between threads */
   int iworker;             /* Index of a worker thread */
   int nworker;             /* No. of worker threads */
   size_t i;                /* loop counter */
   smfData *indksquid=NULL; /* Pointer to input dksquid data */
   smfFitQUIJobData *job_data = NULL; /* Pointer to all job data */
   smfFitQUIJobData *pdata = NULL;/* Pointer to next job data */
   smfHead *hdr;            /* Pointer to data header this time slice */

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

/* Ensure the supplied smfData is bolo-ordered. So "tstride" is 1 and "bstride"
   is nbolo. */
   smf_dataOrder( wf, idata, 0, status );

/* Dimensions of input. */
   smf_get_dims( idata, NULL, NULL, &nbolo, &intslice, NULL, NULL, NULL,
                 status );

/* Go through the first thousand POL_ANG values to see if they are in
   units of radians (new data) or arbitrary encoder units (old data).
   They are assumed to be in radians if no POL_ANG value is larger than
   20. This function can only handle new data. */
   hdr = idata->hdr;
   instate = hdr->allState;
   ntime = ( intslice > 1000 ) ? 1000 : intslice;
   for( itime = 0; itime < ntime; itime++,instate++ ) {
      if( instate->pol_ang > 20 ) {
         *status = SAI__ERROR;
         errRep( " ","   POL2 data contains POL_ANG values in encoder "
                 "units - connot fit to such old data.", status );
         break;
      }
   }

/* Find the input time slice at which each fitting box starts, and the
   length of the output time axis (in time-slices). */
   smf1_find_boxes( intslice, hdr->allState, box, &ontslice, &box_starts,
                    status );

/* Time axis scaling factor. */
   scale = (double) intslice / (double) ontslice;

/* First copy everything from input to output except for the data that needs
   to be downsampled */

/* We want to copy everything in the smfHead except for allState. So we
   make a copy of the allState pointer, and then set it to NULL in the
   header before the copy */
   if( idata->hdr ) {
     instate = idata->hdr->allState;
     idata->hdr->allState = NULL;
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

/* Restore values in idata now that we're done */
   if( instate ) idata->hdr->allState = instate;
   if( indksquid ) idata->da->dksquid = indksquid;

/* Store the required length for the output time axis. The time axis is
   axis zero because the data is bolo-rdered. */
   (*odataq)->dims[ 0 ] = ontslice;
   (*odatau)->dims[ 0 ] = ontslice;
   if( odatai) (*odatai)->dims[ 0 ] = ontslice;

/* Get output dimensions - assumed to be the same for all three outputs. */
   ondata = ontslice*idata->dims[1]*idata->dims[2];

/* Allocate the data arrays for the outputs. */
   (*odataq)->pntr[0] = astCalloc( ondata, sizeof(double) );
   (*odatau)->pntr[0] = astCalloc( ondata, sizeof(double) );
   if( odatai ) (*odatai)->pntr[0] = astCalloc( ondata, sizeof(double) );
   *weights = astCalloc( ondata, sizeof(double) );

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

/* Store all the other info needed by the worker threads, and submit the
   jobs to calculate the Q and U values in each bolo, and then wait for
   them to complete. */
      for( iworker = 0; iworker < nworker; iworker++ ) {
         pdata = job_data + iworker;

         pdata->dat = idata->pntr[0];
         pdata->ipi = odatai ? (*odatai)->pntr[0] : NULL;
         pdata->ipq = (*odataq)->pntr[0];
         pdata->ipu = (*odatau)->pntr[0];
         pdata->ipw = *weights;
         pdata->nbolo = nbolo;
         pdata->intslice = intslice;
         pdata->ontslice = ontslice;
         pdata->qua = smf_select_qualpntr( idata, NULL, status );;
         pdata->allstates = hdr->allState;
         pdata->ipolcrd = ipolcrd;
         pdata->pasign = pasign ? +1: -1;
         pdata->paoff = paoff;
         pdata->box_starts = box_starts;
         pdata->angrot = angrot;
         pdata->north = north;

/* Pass the job to the workforce for execution. */
         thrAddJob( wf, THR__REPORT_JOB, pdata, smf1_fit_qui_job, 0, NULL,
                      status );
      }

/* Wait for the workforce to complete all jobs. */
      thrWait( wf, status );

/* Down-sample the smfHead -------------------------------------------------*/
      smfHead *hdr = (*odataq)->hdr;

      hdr->curframe = (dim_t) (((double) hdr->curframe + 0.5) / scale);
      hdr->nframes = ontslice;
      hdr->steptime *= scale;
      strcpy( hdr->dlabel, "Q" );
      strncpy( hdr->title, "POL-2 Stokes parameter Q", SMF__CHARLABEL );

/* Down-sample all the JCMTState values using nearest neighbours */
      instate = idata->hdr->allState;
      if( instate ) {

         hdr->allState = astCalloc( ontslice, sizeof(*instate) );
         outstate = hdr->allState;

         if( *status == SAI__OK ) {
            size_t frame;  /* index of nearest neighbour JCMTState */

            for( i=0; i<ontslice; i++ ) {
               frame = (size_t) round(((double) i + 0.5)*scale);
               memcpy( outstate + i, instate + frame, sizeof(*instate) );
            }

/* Then go back and properly down-sample the more important fast-changing
   fields like pointing. Note that since there are approximate values there
   already we need to explicitly re-initialize to 0. */

            RESAMPSTATE(instate, outstate, rts_end, intslice, ontslice, 0);

            RESAMPSTATE(instate, outstate, smu_az_jig_x, intslice, ontslice, 0);
            RESAMPSTATE(instate, outstate, smu_az_jig_y, intslice, ontslice, 0);
            RESAMPSTATE(instate, outstate, smu_az_chop_x, intslice, ontslice, 0);
            RESAMPSTATE(instate, outstate, smu_az_chop_y, intslice, ontslice, 0);
            RESAMPSTATE(instate, outstate, smu_tr_jig_x, intslice, ontslice, 0);
            RESAMPSTATE(instate, outstate, smu_tr_jig_y, intslice, ontslice, 0);
            RESAMPSTATE(instate, outstate, smu_tr_chop_x, intslice, ontslice, 0);
            RESAMPSTATE(instate, outstate, smu_tr_chop_y, intslice, ontslice, 0);

            RESAMPSTATE(instate, outstate, tcs_tai, intslice, ontslice, 0);
            RESAMPSTATE(instate, outstate, tcs_airmass, intslice, ontslice, 0);

/* Second coordinates (Dec, El etc) can not wrap 0 to 360 so we do not need
   to test for those cases */
            RESAMPSTATE(instate, outstate, tcs_az_ang, intslice, ontslice, 1);
            RESAMPSTATE(instate, outstate, tcs_az_ac1, intslice, ontslice, 1);
            RESAMPSTATE(instate, outstate, tcs_az_ac2, intslice, ontslice, 0);
            RESAMPSTATE(instate, outstate, tcs_az_dc1, intslice, ontslice, 1);
            RESAMPSTATE(instate, outstate, tcs_az_dc2, intslice, ontslice, 0);
            RESAMPSTATE(instate, outstate, tcs_az_bc1, intslice, ontslice, 1);
            RESAMPSTATE(instate, outstate, tcs_az_bc2, intslice, ontslice, 0);

            RESAMPSTATE(instate, outstate, tcs_tr_ang, intslice, ontslice, 1);
            RESAMPSTATE(instate, outstate, tcs_tr_ac1, intslice, ontslice, 1);
            RESAMPSTATE(instate, outstate, tcs_tr_ac2, intslice, ontslice, 0);
            RESAMPSTATE(instate, outstate, tcs_tr_dc1, intslice, ontslice, 1);
            RESAMPSTATE(instate, outstate, tcs_tr_dc2, intslice, ontslice, 0);
            RESAMPSTATE(instate, outstate, tcs_tr_bc1, intslice, ontslice, 1);
            RESAMPSTATE(instate, outstate, tcs_tr_bc2, intslice, ontslice, 0);

            RESAMPSTATE(instate, outstate, tcs_en_dc1, intslice, ontslice, 1);
            RESAMPSTATE(instate, outstate, tcs_en_dc2, intslice, ontslice, 0);

            RESAMPSTATE(instate, outstate, tcs_dm_abs, intslice, ontslice, 1);
            RESAMPSTATE(instate, outstate, tcs_dm_rel, intslice, ontslice, 0);

/* Wait for all the above smf_downsamp1 jobs to finish. */
            thrWait( wf, status );

         }
      }

/* Copy the Q header to the other outputs. */
      hdr = smf_deepcopy_smfHead( (*odataq)->hdr, status );
      (*odatau)->hdr = hdr;
      if( *status == SAI__OK ) {
         strcpy( hdr->dlabel, "U" );
         strncpy( hdr->title, "POL-2 Stokes parameter U", SMF__CHARLABEL );
      }

      if( odatai ) {
         hdr = smf_deepcopy_smfHead( (*odataq)->hdr, status );
         (*odatai)->hdr = hdr;
         if( *status == SAI__OK ) {
            strcpy( hdr->dlabel, "I" );
            strncpy( hdr->title, "POL-2 Stokes parameter I", SMF__CHARLABEL );
         }
      }
   }

/* Free resources. */
   job_data = astFree( job_data );
   box_starts = astFree( box_starts );

/* Ensure all smfDatas are time-ordered. */
   smf_dataOrder( wf, idata, 1, status );
   if( odatai && *odatai ) smf_dataOrder( wf, *odatai, 1, status );
   if( *odataq ) smf_dataOrder( wf, *odataq, 1, status );
   if( *odatau ) smf_dataOrder( wf, *odatau, 1, status );

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
   const JCMTState *allstates;/* Pointer to array of JCMTState structures */
   const JCMTState *state;    /* JCMTState info for current time slice */
   dim_t *box_starts;         /* First time slice in each box */
   dim_t b1;                  /* First bolometer index */
   dim_t b2;                  /* Last bolometer index */
   dim_t box_size;            /* NFirst time slice in box */
   dim_t ibolo;               /* Bolometer index */
   dim_t ibox;
   dim_t intslice;            /* Number of time-slices in input data */
   dim_t istart;              /* Input time index at start of fitting box */
   dim_t itime;               /* Time slice index */
   dim_t nbolo;               /* Total number of bolometers */
   dim_t ontslice;            /* Number of time-slices in output data */
   double *dat;               /* Pointer to start of input data values */
   double *din;               /* Pointer to input data array for bolo/time */
   double *ipi;               /* Pointer to output I array */
   double *ipq;               /* Pointer to output Q array */
   double *ipu;               /* Pointer to output U array */
   double *ipw;               /* Pointer to output weights array */
   double *pm;
   double *ps;
   double angle;              /* Phase angle for FFT */
   double angrot;             /* Angle from focal plane X axis to fixed analyser */
   double c1;
   double c2;
   double c4;
   double c8;
   double cosval;             /* Cos of angrot */
   double fit;
   double matrix[ NPAR*NPAR ];
   double sum1;               /* Sum of squared residuals */
   double paoff;              /* WPLATE value corresponding to POL_ANG=0.0 */
   double phi;                /* Angle from fixed analyser to effective analyser */
   double res;
   double s1;                 /* Sum of weighted cosine terms */
   double s2;                 /* Sum of weighted sine terms */
   double s4;
   double s8;
   double sinval;             /* Sin of angrot */
   double solution[ NPAR ];
   double sums[NSUM];         /* Sum of bolometer values */
   double tr_angle;
   double twophi;
   double vector[ NPAR ];
   double wplate;             /* Angle from fixed analyser to have-wave plate */
   gsl_matrix_view gsl_m;
   gsl_vector_view gsl_b;
   gsl_vector_view gsl_x;
   int ipolcrd;               /* Reference direction for pol_ang */
   int nsum1;
   int pasign;                /* +1 or -1 indicating sense of POL_ANG value */
   smfFitQUIJobData *pdata;   /* Pointer to job data */
   smf_qual_t *qin;           /* Pointer to input quality array for bolo/time */
   smf_qual_t *qua;           /* Pointer to start of input quality values */

/* Check inherited status */
   if( *status != SAI__OK ) return;

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
   intslice = pdata->intslice;
   ontslice = pdata->ontslice;

   dat = pdata->dat + b1*intslice;
   qua = pdata->qua + b1*intslice;
   allstates = pdata->allstates;

   ipi = pdata->ipi ? pdata->ipi + b1*ontslice : NULL;
   ipq = pdata->ipq + b1*ontslice;
   ipu = pdata->ipu + b1*ontslice;
   ipw = pdata->ipw + b1*ontslice;

   ipolcrd = pdata->ipolcrd;
   pasign = pdata->pasign;
   paoff = pdata->paoff;
   angrot = pdata->angrot;
   box_starts = pdata->box_starts;

/* Check we have something to do. */
   if( b1 < nbolo && *status == SAI__OK ) {

/* Loop round all bolometers to be processed by this thread. */
      for( ibolo = b1; ibolo <= b2; ibolo++ ) {

/* If the whole bolometer is bad, fill all outputs with bad values. */
         if( *qua & SMF__Q_BADB ) {
            for( itime = 0; itime < ontslice; itime++ ) {
               if( ipi ) *(ipi++) = VAL__BADD;
               *(ipq++) = VAL__BADD;
               *(ipu++) = VAL__BADD;
               *(ipw++) = VAL__BADD;
            }

/* If the bolometer is good, calculate and store the i, q and u values. */
         } else {

/* Loop over all output data values for the current bolometer. */
            for( itime = 0; itime <  ontslice; itime++ ) {

/* Get the index of the first input time slice to include in the fit that
   produces the I, Q and U values for the current output time slice. */
               istart = box_starts[ itime ];

/* Get the number of input time slices to include in the fit. */
               box_size = box_starts[ itime + 1 ] - istart;

/* Initialise pointers to the first input data value, quality value and
   state info to be used in the current fitting box. */
               din = dat + istart;
               qin = qua + istart;
               state = allstates + istart;

/* Form the sums needed to calculate the best fit Q, U and I. This
   involves looping over all input samples that fall within the fitting box
   centred on the current output sample. The 44 sums are stored in the
   "sums" array. Initialise it to hold zeros.  */
               memset( sums, 0, NSUM*sizeof(*sums) );
               for( ibox = 0; ibox <  box_size; ibox++,state++,din++,qin++ ) {

/* Get the POL_ANG value for this time slice. */
                  angle = state->pol_ang;

/* If the returned STokes parameters are to be with respect to Tracking
   North, get the angle from tracking north to focal plane Y, measured
   positive in the sense of rotation from focal plane Y to focal plane X.
   Otherwise, use zero. */
                  tr_angle = pdata->north ? state->tcs_tr_ang : 0.0;

/* Check the input sample has not been flagged during cleaning and is
   not bad. */
                  if( !( *qin & SMF__Q_FIT ) && *din != VAL__BADD &&
                      angle != VAL__BADD && tr_angle != VAL__BADD ) {

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
                  gsl_linalg_cholesky_decomp( &gsl_m.matrix );
                  gsl_linalg_cholesky_solve( &gsl_m.matrix, &gsl_b.vector,
                                             &gsl_x.vector );

/* Modify Q and U so they use the requested reference direction, and store in
   the output arrays. */
                  cosval = cos( 2*( angrot - tr_angle ) );
                  sinval = sin( 2*( angrot - tr_angle ) );
                  *(ipq++) = 2*( -solution[ 1 ]*cosval + solution[ 0 ]*sinval );
                  *(ipu++) = 2*( -solution[ 1 ]*sinval - solution[ 0 ]*cosval );

/* Store the correspoinding I value. */
                  if( ipi ) *(ipi++) = solution[ 6 ]*box_size + 2*solution[ 7 ];

/* Loop over the data again in the same way to calculate the variance of the
   residuals between the above fit and the supplied data. */
                  istart = box_starts[ itime ];
                  box_size = box_starts[ itime + 1 ] - istart;
                  din = dat + istart;
                  qin = qua + istart;
                  state = allstates + istart;
                  sum1 = 0.0;
                  nsum1 = 0;



double qfit, ufit, ifit;
if( 0 && ibolo == 526 && itime < 2000 ) {
   if( itime == 0 ) {
      printf("xxx # itimeout ibox itimein din fit qfit ufit ifit s0 s1 s6 s7 wp pop\n");
   }
}




                  for( ibox = 0; ibox <  box_size; ibox++,state++,din++,qin++ ) {
                     angle = state->pol_ang;
                     tr_angle = pdata->north ? state->tcs_tr_ang : 0.0;

                     if( !( *qin & SMF__Q_FIT ) && *din != VAL__BADD &&
                           angle != VAL__BADD && tr_angle != VAL__BADD ) {
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

                        sum1 += res*res;
                        nsum1++;


if( 0 && ibolo == 526 && itime < 2000 ) {
   qfit = solution[0]*s4;
   ufit = solution[1]*c4;
   ifit = solution[6]*ibox + solution[7];

   printf("xxx %zu %zu %zu %.20g %.20g %.20g %.20g %.20g %.20g "
          "%.20g %.20g %.20g %.20g %g\n", itime,
          ibox, ibox + istart, *din, fit,
          qfit, ufit, ifit, solution[0],  solution[1],
          solution[6],  solution[7], wplate, sums[42] );

}

                     }
                  }

/* The returned weight is the reciprocal of the variance of the
   residuals. */
                  *(ipw++) = nsum1/sum1;

/* Store bad values if there were too few good samples in the fitting
   box. */
               } else {
                  if( ipi ) *(ipi++) = VAL__BADD;
                  *(ipq++) = VAL__BADD;
                  *(ipu++) = VAL__BADD;
                  *(ipw++) = VAL__BADD;
               }
            }
         }

/* Move the pointers on to the first input sample for the next bolometer. */
         qua += intslice;
         dat += intslice;
      }
   }
}


static void smf1_find_boxes( dim_t intslice, const JCMTState *allstates, dim_t box,
                             dim_t *ontslice, dim_t **box_starts, int *status ) {
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
*     smf1_find_boxes( dim_t intslice, const JCMTState *allstates, dim_t box,
*                      dim_t *ontslice, dim_t **box_starts, int *status )

*  Arguments:
*     intslice = dim_t (Given)
*        Number of time slices in input.
*     allstates = const JCMTState * (Given)
*        Pointer to the JCMT state information for all input time slices.
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
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine determines the start and end of each box over which
*     to perform a least squares fit. It is needed because the
*     half-waveplate (HWP) angles (the POL_ANG values in the JCMT state
*     array) are not completely regular.

*/

/* Local Variables: */
   dim_t itime;
   const JCMTState *state;
   char more;
   dim_t iel;
   dim_t nrot;
   double ang0 = 0.5*AST__DPI;

/* Initialise returned values. */
   *ontslice = 0;
   *box_starts = NULL;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* If the HWP angle is above PI, move on until the HWP angle wraps back to
   zero. */
   nrot = 0;
   itime = 0;
   state = allstates;
   more = 1;
   while( more && state->pol_ang > ang0 ) {
      if( ++itime == intslice ) more = 0;
      state++;
   }

/* HWP angles vary between 0 and 2*PI in a roughly linear manner, but
   with some sudden steps. When the HWP angle reaches 2*PI it wraps
   back round to zero. Find the first time slice for which the HWP
   angle is greater than PI (a safe value in the middle of the range). */
   while( more && state->pol_ang <= ang0 ) {
      if( ++itime == intslice ) more = 0;
      state++;
   }

/* This is the start of the first box. */
   if( more ) {
      iel = (*ontslice)++;
      *box_starts = astGrow( *box_starts, *ontslice, sizeof( **box_starts ) );
      if( *status == SAI__OK ) (*box_starts)[ iel ] = itime;
   }

/* Loop over all rotations. */
   while( 1 ) {

/* Move on until the HWP angle wraps round back to zero. */
      while( more && state->pol_ang > ang0 ) {
         if( ++itime == intslice ) more = 0;
         state++;
      }

/* Move on until the HWP angle again exceeds PI. */
      while( more && state->pol_ang <= ang0 ) {
         if( ++itime == intslice ) more = 0;
         state++;
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
         if( *status == SAI__OK ) (*box_starts)[ iel ] = itime;
      }
   }

/* The number of output time slices will be one less than the number of
   values in "box_Starts", because the last value in "box_starts" is the end
   of the last box. */
   (*ontslice)--;
}









