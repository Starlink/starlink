/*
*+
*  Name:
*     smf_calc_iqu

*  Purpose:
*     Calculate I, Q and U images from a block of time slices.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_calc_iqu( ThrWorkForce *wf, smfData *data, int block_start,
*                       int block_end, int ipolcrd, int qplace, int uplace,
*                       int iplace, NdgProvenance *oprov, AstFitsChan *fc,
*                       int pasign, double paoff, double angrot,
*                       int submean, int harmonic, int *status );

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
*     iplace = int (Given)
*        A placeholder identifying the location at which to store the
*        the output NDF holding the I image. May be NDF__NOPL if no I
*        images are required.
*     oprov = NdgProvenance * (Given)
*        Pointer to a structure holding the provenance information to
*        store in the I, Q and U NDFs.
*     fc = AstFitsChan * (Given)
*        Pointer to a FitsChan holding the FITS headers to store in the
*        FITS extensions of the I, Q and U NDFs.
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
*     submean = int  (Given)
*        If non-zero, subtract the mean bolometer value from each time
*        slice before using them to calculate Q and U.
*     harmonic = int  (Given)
*        The harmonic of the half-wave plate rotation from which the Q
*        and U values should be derived. This should normally be 4, but
*        other values can be used to investigate the effects of asymetry in
*        the half-wave plate, etc.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function creates three 2D NDFs - one holding I values, one holding
*     Q values and the other holding U values. Each NDF holds a I, Q or U
*     value for each bolometer. Each NDF has its own WCS component, and
*     receives the supplied provenance information and FITS headers. The
*     I, Q and U values are based on the time series data between time slices
*     "block_start" and "block_end". The spatial position of each bolometer
*     is assumed not to move significantly over the duration of this block of
*     time slices. The I, Q and U values stored in the output NDFs are
*     referenced to the focal plane Y axis, and are defined using the
*     conventions described in SUN/223. Positive polarisation angles are in
*     the same sense as rotation from the focal plane X axis to the focal
*     plane Y axis. The current WCS Frame in the output NDFs is "SKY".

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1-FEB-2011 (DSB):
*        Original version.
*     9-AUG-2012 (DSB):
*        Add POLANAL Frames to the WCS FrameSet fo each output NDF. This
*        communicates the reference direction to POLPACK.
*     21-SEP-2012 (DSB):
*        Renamed from smf_calc_qu because it now returns I images in
*        addition to Q and U.
*     24-SEP-2012 (DSB):
*        Fix bug in addressing of allStates array.
*     4-OCT-2012 (DSB):
*        Reverse the pixels along the X axis of the output NDFs so that
*        they presents a normal right-handed view of the sky.
*     24-OCT-2012 (DSB):
*        Swap Q and U. This is a result of Per's analysis of the polarised
*        background with the calibrator in the beam.
*     17-DEC-2012 (DSB):
*        Use focal plane Y instead of north as the reference direction for Q and U.
*     8-JAN-2013 (DSB):
*        Added arguments pasign, paoff and angrot.
*     14-JAN-2013 (DSB):
*        Added argument submean.
*     12-MAR-2013 (DSB):
*        Added calculation of variances.
*     26-MAR-2013 (DSB):
*        Added parameter "harmonic".
*     10-JUL-2013 (DSB):
*        Take account of moving sources.
*     2-JUL-2015 (DSB):
*        Take account of focal plane rotation during the course of one
*        stare position.
*     21-AUG-2015 (DSB):
*        - More mapping simplifications to overcome possible numerical 
*        problems with focal plane distortion.
*        - Calculate Q/U/I using matrix inversion rather than assuming 
*        that sums of trig functions can be taken to be zero.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011-2013 Science and Technology Facilities Council.
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
typedef struct smfCalcIQUJobData {
   const JCMTState *allstates;
   dim_t b1;
   dim_t b2;
   dim_t nbolo;
   double fpr0;
   double fprinc;
   double *dat;
   double *ipi;
   double *ipq;
   double *ipu;
   double *ipiv;
   double *ipqv;
   double *ipuv;
   double angfac;
   double angrot;
   double paoff;
   int block_end;
   int block_start;
   int ipolcrd;
   int gotvar;
   int ncol;
   int old;
   int pasign;
   size_t bstride;
   size_t tstride;
   smf_qual_t *qua;
   double *mean;
   int action;
} smfCalcIQUJobData;

/* Prototypes for local functions */
static void smf1_calc_iqu_job( void *job_data, int *status );
static void smf1_solv3x3( double a11, double a12, double a13,
                          double a21, double a22, double a23,
                          double a31, double a32, double a33,
                          double d1, double d2, double d3,
                          double *x, double *y, double *z );
static double smf1_det3x3( double a11, double a12, double a13,
                           double a21, double a22, double a23,
                           double a31, double a32, double a33 );


/* Old data has POL_ANG given in arbitrary integer units where
   SMF__MAXPOLANG is equivalent to 2*PI. Store the factor to convert such
   values into radians. */
#define TORADS (2*AST__DPI/SMF__MAXPOLANG)

/* The angle in radians to be integrated over to produce a single Q/U pair. */
#define ROT_PER_SAMPLE  4*AST__DPI

void smf_calc_iqu( ThrWorkForce *wf, smfData *data, int block_start,
                  int block_end, int ipolcrd, int qplace, int uplace,
                  int iplace, NdgProvenance *oprov, AstFitsChan *fc,
                  int pasign, double paoff, double angrot, int submean,
                  int harmonic, int *status ){

/* Local Variables: */
   AstCmpMap *cm1;
   AstCmpMap *cm2;
   AstFrameSet *wcs;          /* WCS FrameSet for output NDFs */
   AstMapping *fpmap1;
   AstMapping *fpmap2;
   AstMapping *cm3;
   AstMapping *oskymap;
   AstMapping *totmap;
   AstSkyFrame *oskyfrm;
   AstWinMap *wm;             /* Mapping to reverse the X GRID axis */
   const JCMTState *state;    /* JCMTState info for current time slice */
   const char *usesys;        /* Used system string */
   dim_t itime;               /* Time slice index */
   dim_t nbolo;               /* No. of bolometers */
   dim_t ncol;                /* No. of columns of bolometers */
   dim_t nrow;                /* No. of rows of bolometers */
   dim_t ntime;               /* Time slices to check */
   dim_t ntslice;             /* Number of time-slices in data */
   double *ipi;               /* Pointer to output I array */
   double *ipiv;              /* Pointer to output I variance array */
   double *ipq;               /* Pointer to output Q array */
   double *ipqv;              /* Pointer to output Q variance array */
   double *ipu;               /* Pointer to output U array */
   double *ipuv;              /* Pointer to output U variance array */
   double *mean;
   double ang_data[2];
   double fox[2];
   double foy[2];
   double fpr0;
   double fprinc;
   double fx[2];
   double fy[2];
   double ina[ 2 ];           /* Bolometer coords at bottom left */
   double inb[ 2 ];           /* Bolometer coords at top right */
   double outa[ 2 ];          /* NDF GRID coords at bottom left */
   double outb[ 2 ];          /* NDF GRID coords at top right */
   int bstep;                 /* Bolometer step between threads */
   int el;                    /* Number of mapped array elements */
   int gotvar;                /* Were any output variances created? */
   int indfi;                 /* Identifier for NDF holding I values */
   int indfq;                 /* Identifier for NDF holding Q values */
   int indfu;                 /* Identifier for NDF holding Q values */
   int iworker;               /* Index of a worker thread */
   int lbnd[ 2 ];             /* Lower pixel bounds of output NDF */
   int moving;
   int nworker;               /* No. of worker threads */
   int old;                   /* Data has old-style POL_ANG values? */
   int tstep;                 /* Time slice step between threads */
   int ubnd[ 2 ];             /* Upper pixel bounds of output NDF */
   size_t bstride;            /* Stride between adjacent bolometer values */
   size_t tstride;            /* Stride between adjacent time slice values */
   smfCalcIQUJobData *job_data = NULL; /* Pointer to all job data */
   smfCalcIQUJobData *pdata = NULL;/* Pointer to next job data */
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

/* Report an error if the block of time slices extends off either end. */
   if( block_start < 0 || block_end >= (int) ntslice ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSeti( "S", block_start );
         msgSeti( "E", block_end );
         msgSeti( "N", ntslice );
         errRep( " ", "smf_calc_iqu: invalid block of time slices - ^S to "
                 "^E (^N time slices are available).", status );
      }
   }

/* Create the output NDFs. Each one is a 2D array with dimensions
   equal to the bolometer array. */
   lbnd[ 0 ] = 1;
   lbnd[ 1 ] = 1;
   ubnd[ 0 ] = ncol;
   ubnd[ 1 ] = nrow;
   ndfNew( "_DOUBLE", 2, lbnd, ubnd, &qplace, &indfq, status );
   ndfNew( "_DOUBLE", 2, lbnd, ubnd, &uplace, &indfu, status );
   if( iplace != NDF__NOPL ) {
      ndfNew( "_DOUBLE", 2, lbnd, ubnd, &iplace, &indfi, status );
   } else {
      indfi = NDF__NOID;
   }

/* Store any supplied provenance in all NDFs. */
   if( oprov ) {
      ndgWriteProv( oprov, indfq, 1, status );
      ndgWriteProv( oprov, indfu, 1, status );
      if( indfi != NDF__NOID ) ndgWriteProv( oprov, indfi, 1, status );
   }

/* Store any supplied FITS headers in all NDFs.*/
   if( fc && astGetI( fc, "NCard" ) > 0 ) {
      kpgPtfts( indfq, fc, status );
      kpgPtfts( indfu, fc, status );
      if( indfi != NDF__NOID )  kpgPtfts( indfi, fc, status );
   }

/* Store the WCS frameSet in all NDFs. First get the FrameSet for the
   central time slice in the block, and set its current Frame to the
   tracking frame. */
   smf_tslice_ast( data, ( block_start + block_end )/2, 1, NO_FTS, status);
   usesys = sc2ast_convert_system( (data->hdr->allState)[0].tcs_tr_sys,
                                    status );
   astSetC( hdr->wcs, "System", usesys );

/* Get the Mapping from focal plane coords to bolometer grid coords. This
   is the same for all time slices. sc2ast ensures that frame 3 is FPLANE. */
   fpmap1 = astGetMapping( hdr->wcs, 3, AST__BASE );

/* Take a copy and then reverse the X axis of the GRID Frame by remaping the
   base Frame using a WinMap. This produces a pixel grid such as you would
   see by looking up at the sky from underneath the array, rather than looking
   down at the ground from above the array. NB is this needed for 450 *and*
   850 data, or just 850 ? */
   wcs = astCopy( hdr->wcs );
   ina[ 0 ] = 1.0;
   inb[ 0 ] = ncol;
   ina[ 1 ] = 1.0;
   inb[ 1 ] = nrow;

   outa[ 0 ] = ncol;
   outb[ 0 ] = 1.0;
   outa[ 1 ] = 1.0;
   outb[ 1 ] = nrow;

   wm = astWinMap( 2, ina, inb, outa, outb, " " );
   astRemapFrame( wcs, AST__BASE, wm );
   wm = astAnnul( wm );

/* Get the Mapping from output grid coords to focal plane coords. */
   fpmap2 = astGetMapping( wcs, AST__BASE, 3 );

/* If the target is moving (assumed to be the case if the tracking
   system is AZEL or GAPPT), make the FrameSet current Frame represent
   offsets from the reference position (i.e. the moving target), and
   indicate that the offset coord system should be used for alignment. */
   if( !strcmp( usesys, "AZEL" ) || !strcmp( usesys, "GAPPT" ) ){
      astSet( wcs, "SkyRefIs=Origin,AlignOffset=1" );
      moving = 1;
   } else {
      moving = 0;
   }

/* Store the FrameSet in the output NDFs. */
   ndfPtwcs( wcs, indfq, status );
   ndfPtwcs( wcs, indfu, status );
   if( indfi != NDF__NOID ) ndfPtwcs( wcs, indfi, status );

/* Map the Data array in each NDF. */
   ndfMap( indfq, "Data", "_DOUBLE", "WRITE", (void **) &ipq, &el, status );
   ndfMap( indfu, "Data", "_DOUBLE", "WRITE", (void **) &ipu, &el, status );
   if( indfi != NDF__NOID ) {
      ndfMap( indfi, "Data", "_DOUBLE", "WRITE", (void **) &ipi, &el, status );
   } else {
      ipi = NULL;
   }

/* Map the Variance array in each NDF. */
   ndfMap( indfq, "Variance", "_DOUBLE", "WRITE", (void **) &ipqv, &el, status );
   ndfMap( indfu, "Variance", "_DOUBLE", "WRITE", (void **) &ipuv, &el, status );
   if( indfi != NDF__NOID ) {
      ndfMap( indfi, "Variance", "_DOUBLE", "WRITE", (void **) &ipiv, &el, status );
   } else {
      ipiv = NULL;
   }

/* If required, allocate memory to hold the mean bolometer value at each
   time slice. */
   mean = submean ? astMalloc( ntslice*sizeof( *mean ) ) : NULL;

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

/* If required, find the mean bolometer value at each time slice. */
      if( submean ) {

/* Determine which time-slices are to be processed by which threads. */
         tstep = ntslice/nworker;
         if( tstep < 1 ) tstep = 1;

         for( iworker = 0; iworker < nworker; iworker++ ) {
            pdata = job_data + iworker;
            pdata->block_start = iworker*tstep;
            if( iworker < nworker - 1 ) {
               pdata->block_end = pdata->block_start + tstep - 1;
            } else {
               pdata->block_end = ntslice - 1;
            }
         }

/* Store all the other info needed by the worker threads, and submit the
   jobs to calculate the Q and U values in each bolo, and then wait for
   them to complete. */
         for( iworker = 0; iworker < nworker; iworker++ ) {
            pdata = job_data + iworker;

            pdata->bstride = bstride;
            pdata->dat = data->pntr[0];
            pdata->nbolo = nbolo;
            pdata->qua = smf_select_qualpntr( data, NULL, status );;
            pdata->tstride = tstride;
            pdata->mean = mean;
            pdata->action = 1;

/* Pass the job to the workforce for execution. */
            thrAddJob( wf, THR__REPORT_JOB, pdata, smf1_calc_iqu_job, 0, NULL,
                         status );
         }

/* Wait for the workforce to complete all jobs. */
         thrWait( wf, status );

      }

/* Get the Frame representing absolute sky coords in the output NDF,
   and the Mapping from sky to grid in the output NDF. */
      oskyfrm = astCopy( astGetFrame( wcs, AST__CURRENT ) );
      astSet( oskyfrm, "SkyRefIs=Ignored" );
      oskymap = astGetMapping( wcs, AST__CURRENT, AST__BASE );
      wcs = astAnnul( wcs );

/* Find the first and last time slices, calculate the angle between the
   focal pane Y axis at the time slice, and the focal plane Y axis in
   the output NDF (aligning them on the sky - the output NDF WCS is taken
   from the central time slice). For intervening time-slices, the angle
   is found by linear interpolation between the extreme time slices. */
      for( el = 0; el < 2; el++ ) {

/* Get the mapping from GRID coords in the input time slice to GRID
   coords in the output. */
         totmap = smf_rebin_totmap( data, el?ntslice-1:0, oskyfrm, oskymap,
                                    moving, NO_FTS, status );

/* Modify it to be the Mapping from focal plane coords in the input time
   slice to focal plane coords in the output. */
         cm1 = astCmpMap( fpmap1, totmap, 1, " " );
         cm2 = astCmpMap( cm1, fpmap2, 1, " " );
         cm3 = astSimplify( cm2 );

/* Use this Mapping to convert two points on the focal plane Y axis from
   the input to the output. */
         fx[0] = 0.0;
         fy[0] = 0.0;
         fx[1] = 0.0;
         fy[1] = 4.0;
         astTran2( cm3, 2, fx, fy, 1, fox, foy );

/* The angle from the focal plane Y axis in the output to the focal plane
   Y axis in the input time slice, measured positive in sense of rotation
   from Fy to Fx. */
         ang_data[ el ] = atan2( fox[1]-fox[0], foy[1]-foy[0] );

/* Free resources for this time slice. */
         totmap = astAnnul( totmap );
         cm1 = astAnnul( cm1 );
         cm2 = astAnnul( cm2 );
         cm3 = astAnnul( cm3 );
      }

/* Annul objects. */
      oskymap = astAnnul( oskymap );
      oskyfrm = astAnnul( oskyfrm );
      fpmap1 = astAnnul( fpmap1 );
      fpmap2 = astAnnul( fpmap2 );

/* Get the constants of the linear relationship between focal plane
   rotation and time slice index "fpr = fpr0 + itime*fprinc". */
      fpr0 = ang_data[ 0 ];
      fprinc = ( ang_data[ 1 ] - fpr0 )/( ntslice - 1 );

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
         pdata->qua = smf_select_qualpntr( data, NULL, status );;
         pdata->tstride = tstride;
         pdata->allstates = hdr->allState;
         pdata->ipq = ipq;
         pdata->ipu = ipu;
         pdata->ipi = ipi;
         pdata->ipqv = ipqv;
         pdata->ipuv = ipuv;
         pdata->ipiv = ipiv;
         pdata->ipolcrd = ipolcrd;
         pdata->block_start = block_start;
         pdata->block_end = block_end;
         pdata->old = old;
         pdata->ncol = ncol;
         pdata->pasign = pasign ? +1: -1;
         pdata->paoff = paoff;
         pdata->angrot = angrot;
         pdata->fpr0 = fpr0;
         pdata->fprinc = fprinc;
         pdata->angfac = harmonic/4.0;
         pdata->action = 0;
         pdata->mean = mean;

/* Pass the job to the workforce for execution. */
         thrAddJob( wf, THR__REPORT_JOB, pdata, smf1_calc_iqu_job, 0, NULL,
                      status );
      }

/* Wait for the workforce to complete all jobs. */
      thrWait( wf, status );

/* See if any thread produced non-bad variance values. */
      gotvar = 0;
      for( iworker = 0; iworker < nworker; iworker++ ) {
         pdata = job_data + iworker;
         if( pdata->gotvar ) gotvar = 1;
      }

/* If no variances were created, erase the Variance component and tell
   the user. */
      ndfUnmap( indfq, "*", status );
      ndfUnmap( indfu, "*", status );
      if( ipi ) ndfUnmap( indfi, "*", status );

      if( !gotvar ) {
         ndfReset( indfq, "Variance", status );
         ndfReset( indfu, "Variance", status );
         if( ipi ) ndfReset( indfi, "Variance", status );
         msgOut( "", "Warning: Insufficient input data to produce variances",
                 status );
      }
   }

/* Add POLANAL Frames to the WCS FrameSet in each output NDF. This Frame
   is used by POLPACK to determine the reference direction of the Stokes
   vectors (focal plane Y in this case, i.e. zero-based axis 1 ). */
   smf_polext( indfq, 0, 0.0, "FPLANE", 1, status );
   smf_polext( indfu, 0, 0.0, "FPLANE", 1, status );
   if( ipi ) smf_polext( indfi, 0, 0.0, "FPLANE", 1, status );

/* Free the two output NDFs. */
   ndfAnnul( &indfq, status );
   ndfAnnul( &indfu, status );
   if( ipi ) ndfAnnul( &indfi, status );

/* Free other resources. */
   job_data = astFree( job_data );
   mean = astFree( mean );
}


static void smf1_calc_iqu_job( void *job_data, int *status ) {
/*
*  Name:
*     smf1_calc_iqu_job

*  Purpose:
*     Calculate I, Q and U for a block of bolometers.

*  Invocation:
*     void smf1_calc_iqu_job( void *job_data, int *status )

*  Arguments:
*     job_data = void * (Given)
*        Pointer to the data needed by the job. Should be a pointer to a
*        smfCalcIQUJobData structure.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine calculate the I, Q and U values for each bolometer in
*     a block of bolometers. It runs within a thread instigated by
*     smf_calc_iqu.

*/

/* Local Variables: */
   const JCMTState *allstates;/* Pointer to array of JCMTState structures */
   const JCMTState *state;    /* JCMTState info for current time slice */
   dim_t b1;                  /* First bolometer index */
   dim_t b2;                  /* Last bolometer index */
   dim_t ibolo;               /* Bolometer index */
   dim_t ipix;                /* Pixel index */
   dim_t nbolo;               /* Total number of bolometers */
   double *dat;               /* Pointer to start of input data values */
   double *din0;              /* Pointer to input data array for 1st time */
   double *din;               /* Pointer to input data array for bolo/time */
   double *ipi;               /* Pointer to output I array */
   double *ipiv;
   double *ipq;               /* Pointer to output Q array */
   double *ipqv;
   double *ipu;               /* Pointer to output U array */
   double *ipuv;
   double *pm;                /* Pointer to next time slice mean value */
   double ang;
   double angfac;
   double angle;              /* Phase angle for FFT */
   double angle_l;
   double angrot;             /* Angle from focal plane X axis to fixed analyser */
   double ca;
   double sa;
   double cosval;             /* Cos of twice reference rotation angle */
   double dang;
   double den;
   double fpr0;
   double fprinc;
   double i;                  /* Output I value */
   double paoff;              /* WPLATE value corresponding to POL_ANG=0.0 */
   double phi;                /* Angle from fixed analyser to effective analyser */
   double q0;                 /* Q value with respect to fixed analyser */
   double q;                  /* Output Q value */
   double rot;                /* Rotation angle included in current s1/2/3 values */
   double rot_target;
   double s1;                 /* Sum of weighted cosine terms */
   double s2;                 /* Sum of weighted sine terms */
   double s3;                 /* Sum of weights */
   double s4;
   double s5;
   double s6;
   double s7;
   double s8;
   double sinval;             /* Sin of twice reference rotation angle */
   double sum;                /* Sum of bolometer values */
   double sw;
   double swi;
   double swii;
   double swq;
   double swqq;
   double swu;
   double swuu;
   double sww;
   double u0;                 /* U value with respect to fixed analyser */
   double u;                  /* Output U value */
   double v;                  /* Analysed intensity to use */
   double vi;
   double vq0;
   double vq;
   double vu0;
   double vu;
   double wplate;             /* Angle from fixed analyser to have-wave plate */
   int block_end;             /* Last time slice to process */
   int block_start;           /* First time slice to process */
   int ipolcrd;               /* Reference direction for pol_ang */
   int itime;                 /* Time slice index */
   int itime_start;           /* Time slice index at start of section */
   int limit2;                /* Min no of good i/p values for a good single estimate */
   int limit;                 /* Min no of good i/p values for a good o/p value */
   int n;                     /* Number of contributing values in S1, S2 and S3 */
   int ncol;                  /* No. of bolometers in one row */
   int nn;                    /* Number of good bolometer values */
   int nrot;
   int old;                   /* Data has old-style POL_ANG values? */
   int pasign;                /* +1 or -1 indicating sense of POL_ANG value */
   size_t bstride;            /* Stride between adjacent bolometer values */
   size_t tstride;            /* Stride between adjacent time slice values */
   smfCalcIQUJobData *pdata;  /* Pointer to job data */
   smf_qual_t *qin0;          /* Pointer to input quality array for 1st time */
   smf_qual_t *qin;           /* Pointer to input quality array for bolo/time */
   smf_qual_t *qua;           /* Pointer to start of input quality values */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer to the job data, and then extract its contents into a
   set of local variables. */
   pdata = (smfCalcIQUJobData *) job_data;

   b1 = pdata->b1;
   b2 = pdata->b2;
   bstride = pdata->bstride;
   dat = pdata->dat;
   nbolo = pdata->nbolo;
   qua = pdata->qua;
   tstride = pdata->tstride;
   allstates = pdata->allstates;
   ipi = pdata->ipi;
   ipq = pdata->ipq;
   ipu = pdata->ipu;
   ipiv = pdata->ipiv;
   ipqv = pdata->ipqv;
   ipuv = pdata->ipuv;
   ipolcrd = pdata->ipolcrd;
   block_start = pdata->block_start;
   block_end = pdata->block_end;
   old = pdata->old;
   ncol = pdata->ncol;
   pasign = pdata->pasign;
   paoff = pdata->paoff;
   angrot = pdata->angrot;
   angfac = pdata->angfac;
   fpr0 = pdata->fpr0;
   fprinc = pdata->fprinc;

/* Assume we are not returning any variance values. */
   pdata->gotvar = 0;

/* Calculate Q and U if required. */
   if( pdata->action == 0 ) {

/* Check we have something to do. */
      if( b1 < nbolo ) {

/* The minimum number of samples required for a good output value. Half
   of the available input samples must be good. */
         limit = 0.5*( block_end - block_start );
         limit2 = 10;

/* Initialise pointers to the first time slice data and quality value for
   the first bolometer to be processed in the current block of time slices. */
         din0 = dat + bstride*b1 + tstride*block_start;
         qin0 = qua + bstride*b1 + tstride*block_start;

/* Loop round all bolometers to be processed by this thread. */
         for( ibolo = b1; ibolo <= b2; ibolo++ ) {

/* If the whole bolometer is bad, just use bad q and u values. */
            if( *qin0 & SMF__Q_BADB ) {
               i = VAL__BADD;
               u = VAL__BADD;
               q = VAL__BADD;
               vi = VAL__BADD;
               vu = VAL__BADD;
               vq = VAL__BADD;

/* If the bolometer is good, calculate and store the q and u values. */
            } else {

/* Initialise pointers to the next time slice data and quality value for
   the current bolometer. */
               din = din0;
               qin = qin0;

/* Initialise the sums used to find Q and U at this bolometer. */
               s1 = 0.0;
               s2 = 0.0;
               s3 = 0.0;
               s4 = 0.0;
               s5 = 0.0;
               s6 = 0.0;
               s7 = 0.0;
               s8 = 0.0;

               n = 0.0;
               rot = 0.0;
               angle_l = VAL__BADD;

               swq = 0.0;
               swqq = 0.0;
               swu = 0.0;
               swuu = 0.0;
               swi = 0.0;
               swii = 0.0;
               sw = 0.0;
               sww = 0.0;
               nrot = 0;
               rot_target = ROT_PER_SAMPLE;

/* Loop round all time slices. */
               pm = pdata->mean;
               if( pm )  pm += block_start;
               state = allstates + block_start;
               itime_start = block_start;
               for( itime = block_start; itime <= block_end; itime++,state++ ) {

/* Get the POL_ANG value for this time slice. */
                  angle = state->pol_ang;

/* Check the input sample has not been flagged during cleaning and is
   not bad. */
                  if( !( *qin & SMF__Q_FIT ) && *din != VAL__BADD &&
                      angle != VAL__BADD && ( !pm || *pm != VAL__BADD ) ) {

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
                        errRepf( "", "smf_calc_iqu: currently only POL_CRD = "
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

/* Increment the sums needed to find the Fourier component of the time
   series corresponding to the frequency introduced by the rotation of
   the half wave plate. */
                     angle = 2*phi;

/* Allow the angle to be scaled by some user-specified factor. This is to
   allow the investigation of other harmonics. */
                     angle *= angfac;

/* If we have now done the required amount of rotation, calculate new Q and
   U values. */
                     if( rot >= rot_target ) {

/* If we have sufficient points, calculate the I, Q and U values for the
   revolution that has just ended, and then update the running sums used
   to find the final values and variances. */
                        if( n > limit2 ) {
                           smf1_solv3x3( s4/2, s6/2, s8/2, s5/2, s8/2,
                                         s7/2, n/2, s4/2, s5/2, s1, s2, s3,
                                         &i, &q0, &u0 );

/* Rotate the Q and U values to take account of the difference between the
   orientation of the focal plane at the middle time slice included in
   the current values, and the focal plane in the output NDF. */
                           if( i != VAL__BADD ) {
                              ang = fpr0 + 0.5*fprinc*( itime_start + itime - 1 );
                              cosval = cos(2*ang);
                              sinval = sin(2*ang);
                              q = q0*cosval + u0*sinval;
                              u = -q0*sinval + u0*cosval;

                              swq += n*q;
                              swqq += n*q*q;
                              swu += n*u;
                              swuu += n*u*u;
                              swi += n*i;
                              swii += n*i*i;
                              sw += n;
                              sww += n*n;
                              nrot++;
                           }
                           itime_start = itime;
                        }

/* Prepare for a new revolution. */
                        s1 = 0.0;
                        s2 = 0.0;
                        s3 = 0.0;
                        s4 = 0.0;
                        s5 = 0.0;
                        s6 = 0.0;
                        s7 = 0.0;
                        s8 = 0.0;
                        n = 0;
                        rot_target += ROT_PER_SAMPLE;
                     }

/* Increment the total rotation angle since the last calculation of Q and U. */
                     dang = ( angle_l != VAL__BADD ) ? angle - angle_l : 0.0;
                     while( dang < -AST__DPI ) dang += 2*AST__DPI;
                     rot += dang;
                     angle_l = angle;

/* Increment the sums to include the current time slice. */
                     v = pm ? *din - *pm : *din;

                     ca = cos( angle );
                     sa = sin( angle );
                     s1 += v*ca;
                     s2 += v*sa;
                     s3 += v;
                     s4 += ca;
                     s5 += sa;
                     s6 += ca*ca;
                     s7 += sa*sa;
                     s8 += sa*ca;
                     n++;
                  }

/* Update pointers to the next time slice data and quality value for
   the current bolometer. */
                  din += tstride;
                  qin += tstride;
                  if( pm ) pm++;
               }

/* Add in the I, Q and U values determined from the final block. */
               if( n > limit2 ) {
                  smf1_solv3x3( s4/2, s6/2, s8/2, s5/2, s8/2,
                                s7/2, n/2, s4/2, s5/2, s1, s2, s3,
                                &i, &q0, &u0 );

                  if( i != VAL__BADD ) {
                     ang = fpr0 + 0.5*fprinc*( itime_start + itime - 1 );
                     cosval = cos(2*ang);
                     sinval = sin(2*ang);
                     q = q0*cosval + u0*sinval;
                     u = -q0*sinval + u0*cosval;

                     swq += n*q;
                     swqq += n*q*q;
                     swu += n*u;
                     swuu += n*u*u;
                     swi += n*i;
                     swii += n*i*i;
                     sw += n;
                     sww += n*n;
                     nrot++;
                  }
               }

/* Calculate the mean q, u and i values variances. These use the
   fixed analyser in the output as the reference direction. */
               q = VAL__BADD;
               u = VAL__BADD;
               i = VAL__BADD;
               vq = VAL__BADD;
               vu = VAL__BADD;
               vi = VAL__BADD;

               if( sw > limit && nrot > 0 ) {
                  msgOutiff( MSG__DEBUG, "", "Bolo %d split into %d values "
                             "(%g samples per value)", status, (int) ibolo,
                             nrot, sw/nrot );

                  q0 = swq/sw;
                  u0 = swu/sw;
                  i = swi/sw;

/* Modify Q and U so they use the focal plane Y as the reference direction. */
                  cosval = cos(2*angrot);
                  sinval = sin(2*angrot);
                  q = -q0*cosval + u0*sinval;
                  u = -q0*sinval - u0*cosval;

/* If we had at least 4 rotations, also calculate the variances, and
   rotate them. */
                  den = sw*sw - sww;
                  if( den > 0 && nrot > 3 ) {
                     vq0 = sww*( swqq/sw - q0*q0 )/den;
                     vu0 = sww*( swuu/sw - u0*u0 )/den;
                     vi = sww*( swii/sw - i*i )/den;

                     vq = cosval*cosval*vq0 + sinval*sinval*vu0;
                     vu = sinval*sinval*vq0 + cosval*cosval*vu0;

                     pdata->gotvar = 1;
                  }
               }
            }

/* Calculate the vector index into the output NDFs at which to store the
   current bolometer. This implements a reversal of the pixels along each
   row, in order to produce the usual right-handed view of the sky. */
            ipix = ncol + ibolo - 2*( ibolo % ncol ) - 1;

/* Store the values in the output NDFs. */
            ipq[ ipix ] = q;
            ipu[ ipix ] = u;
            if( ipi ) ipi[ ipix ] = i;

            ipqv[ ipix ] = vq;
            ipuv[ ipix ] = vu;
            if( ipiv ) ipiv[ ipix ] = vi;

/* Update the pointers to the first time slice data and quality value for
   the next bolometer. */
            din0 += bstride;
            qin0 += bstride;
         }
      }

/* Calculate mean value in each time slice if required. */
   } else {

      pm = pdata->mean + block_start;
      for( itime = block_start; itime <= block_end; itime++,pm++ ) {
         din = dat + itime*tstride;
         qin = qua + itime*tstride;

         sum = 0;
         nn = 0;

         for( ibolo = 0; ibolo < nbolo; ibolo++ ) {

            if( !( *qin & SMF__Q_FIT ) && *din != VAL__BADD ) {
               sum += *din;
               nn++;

            }

            din += bstride;
            qin += bstride;
         }

         if( nn > 0 ) {
            *pm = sum/nn;
         } else {
            *pm = VAL__BADD;
         }
      }
   }
}




/* Solve 3x3 linear equations using Cramer's rule. */
static void smf1_solv3x3( double a11, double a12, double a13,
                          double a21, double a22, double a23,
                          double a31, double a32, double a33,
                          double d1, double d2, double d3,
                          double *x, double *y, double *z ){


   double d = smf1_det3x3( a11, a12, a13,
                           a21, a22, a23,
                           a31, a32, a33 );

   double dx = smf1_det3x3( d1, a12, a13,
                            d2, a22, a23,
                            d3, a32, a33 );

   double dy = smf1_det3x3( a11, d1, a13,
                            a21, d2, a23,
                            a31, d3, a33 );

   double dz = smf1_det3x3( a11, a12, d1,
                            a21, a22, d2,
                            a31, a32, d3 );

   if( d != 0.0 ) {
      *x = dx/d;
      *y = dy/d;
      *z = dz/d;
   } else {
      *x = VAL__BADD;
      *y = VAL__BADD;
      *z = VAL__BADD;
   }

}


/* Return the deteminant of a 3x3 matrix */
static double smf1_det3x3( double a11, double a12, double a13,
                           double a21, double a22, double a23,
                           double a31, double a32, double a33 ){
   double sd1 = a22*a33 - a23*a32;
   double sd2 = a21*a33 - a23*a31;
   double sd3 = a21*a32 - a22*a31;
   return a11*sd1 - a12*sd2 + a13*sd3;
}





