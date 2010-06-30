/*
*+
*  Name:
*     smf_fix_steps

*  Purpose:
*     Detect and correct any steps in the DC level in each bolometer.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_fix_steps( smfWorkForce *wf, smfData *data,
*                         smf_qual_t *quality, double dcthresh,
*                         dim_t dcmedianwidth, dim_t dcfitbox, int dcmaxsteps,
*                         int dclimcorr, size_t *nsteps, int *status )

*  Arguments:
*     wf = smfWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData * (Given and Returned)
*        The data that will be repaired (in-place)
*     quality = smf_qual_t * (Given and Returned)
*        If set, use this buffer instead of QUALITY associated with data.
*        If NULL, use the QUALITY associated with data. Locations of steps
*        will have bit SMF__Q_JUMP set.
*     dcthresh = double (Given)
*        N-sigma threshold for DC jump to be detected.
*     dcmedianwidth = dim_t (Given)
*        The width in samples of the median box used to determine the
*        typical value on either side of a potential step.
*     dcfitbox = dim_t (Given)
*        Length of box (in samples) over which each linear fit is
*        performed. If zero, no steps will be corrected.
*     dcmaxsteps = int (Given)
*        The maximum number of steps that can be corrected in each minute
*        of good data (i.e. per 12000 samples) from a bolometer before the
*        entire bolometer is flagged as bad. A value of zero will cause a
*        bolometer to be rejected if any steps are found in the bolometer
*        data stream.
*     dclimcorr = int (Given)
*        The detection threshold for steps that occur at the same time in
*        many bolometers. Set it to zero to suppress checks for correlated
*        steps. If dclimcorr is greater than zero, and a step is found at
*        the same time in more than "dclimcorr" bolometers, then all
*        bolometers are assumed to have a step at that time, and the step
*        is fixed no matter how small it is.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Coming soon...

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-MAR-2010 (DSB):
*        Original version.
*     25-MAR-2010 (DSB):
*        - Change dcminstepwidth from 20 to 0.8*dcmedianwidth (instantaneous
*        steps should have a width of dcmedianwidth).
*        - Change the way the median filtering is done to use one filter
*        rather than two. This should be faster.
*        - Reject blocks of steep samples if the median value at the
*        start and end of the block are insufficiently different.
*        - Extract median filtering code into another routine.
*     13-MAY-2010 (DSB):
*        - The RMS value in the gradient array could be badly affected
*        by very large steps. So do three 3*sigma rejection iterations
*        to make the rms more robust.
*        - Replace incorrect "break" with "continue". This could
*        cause the whole function to return early if a bolometer
*        with too few usable values was found.
*        - Correct counting of rejected bolometers (returned in *nsteps).
*     18-MAY-2010 (DSB):
*        Ensure the first and last DCMEDIANWIDTH samples in each bolometer
*        are continuous with the intermediate data.
*     21-MAY-2010 (DSB):
*        Added dclimcorr.
*     24-MAY-2010 (DSB):
*        - Do not alter the padding values at start and end of each bolometer
*        time series.
*        - Apodize the initial and final correction for each bolometer
*        time series, in the same way that smf_apodize does.
*     25-MAY-2010 (DSB):
*        - Increase dcmaxstepwidth from 1.8*dcmedianwidth to
*        3.0*dcmedianwidth (Remo has data which shows steps that include
*        two distinct sub-steps, separated by around 50 samples).
*        - Change dcminstepgap from 50 to 0.5*dcmedianwidth to allow
*        such sub-steps (sometimes) to be processed as two separate steps.
*        - Re-organise the debugging facilities.
*        - If a step occurs too close to the start or end to be fixed,
*        flag all samples as a jump up tp the start or end.
*     28-MAY-2010 (DSB):
*        - Exclude data previously flagged as a jump when fitting data before
*        and after a candidate step.
*        - Fix incorrect indexing of quality array when steps are found close
*        to the start or end of the time series.
*        - Clip at 2 sigma (not 3) when finding the RMS gradient, and
*        then correct for the heavy clipping using the factor appropriate
*        for pure Gaussian noise. heavier clipping does better in the
*        presence of lots of steps.
*     11-JUN-2010 (DSB):
*        Report the number of fixed steps if in verbose mode.
*     25-JUN-2010 (DSB):
*        Apodisation is now done later in smf_execute_filter, so there
*        is no need to apodise in this function.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

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
#include "libsmf/smf_typ.h"
#include "libsmf/smf_err.h"

/* Other includes */
#include <gsl/gsl_sort.h>

/* A magic unsigned value */
#define INIT 999999999

#ifdef DEBUG_STEPS

#define DEBUG_ARGS , TimeData *timedata
#define DEBUG_VALS , timedata

#define IN_BLOCK 1
#define UP_STEP 2
#define DOWN_STEP 4
#define CHECK_1 8
#define CHECK_2 16
#define CHECK_3 32

#define RECORD_BOLO (ibolo==807)

#define TOPCAT(fd, x) \
   if( x != VAL__BADD ) { \
      fprintf( fd, "%g ", x ); \
   } else { \
      fprintf( fd, "null " ); \
   }

typedef struct TimeData {
   double indata;
   int inquality;
   double outdata;
   int outquality;
   int ibolo;
   double jump;
   double median;
   int in_rms;
   int flags;
   int istep_width;
   double step_width;
   double nsign;
   double change;
   double fac;
   double diff;
   double thresh;
   double line_start;
   double line_end;
   double rmslo;
   double rmshi;
} TimeData;

typedef struct BoloData {
   int ibolo;
   double rms_jump;
   double thresh;
} BoloData;


#else

#define DEBUG_ARGS
#define DEBUG_VALS

#endif


/* Prototypes for private functions. */
static void smf1_step_linefit( int box, float minfrac, int stride, double *dat,
                               smf_qual_t *qua, double nsigma, double *m,
                               double *c, double *rms, int *status );

static int smf1_step_correct( int nblock, int *blocks, double *work,
                              smf_qual_t *qua, double *dat, size_t base,
                              dim_t ibolo, size_t bstride, size_t
			      tstride, dim_t ntslice, int ntime, int
			      itime_lo, int itime_hi, int *common, int
			      maxsteps, dim_t dcfitbox, int dcminbox,
			      float dcminpop, double dcthresh, double
			      dcthresh2, int dcmaxstepwidth, int pad_start,
                              int itime_start, int itime_end,
                              int *nfixed, int *status DEBUG_ARGS );


void smf_fix_steps( smfWorkForce *wf, smfData *data, smf_qual_t *quality,
                    double dcthresh, dim_t dcmedianwidth, dim_t dcfitbox,
                    int dcmaxsteps, int dclimcorr, size_t *nsteps, int *status ) {

/* Local Variables */
   dim_t ibolo;                /* Index of bolometer */
   dim_t nbolo;                /* Number of bolometers */
   dim_t ntslice;              /* Number of time slices */
   double *dat = NULL;         /* Pointer to bolo data */
   double *pw1;
   double *pw2;
   double *w1;
   double *w2;
   double *work;
   double diff2;
   double diff2_limit;
   double diff;
   double end_value;
   double fac;
   double rms;
   double start_value;
   double thresh;
   double tsum2;
   int *block;
   int *blocks;
   int *common;
   int nfixed;
   int iblock;
   int iter;
   int itime;                  /* Index of time slice */
   int itime_hi;
   int itime_lo;
   int jtime;                  /* Index of time slice */
   int nblock;
   int nsign;
   int ntime;                  /* Number of time slices in usable range */
   int pad_end;
   int pad_start;
   int step_end;
   int step_limit;
   int step_start;
   int step_width;
   int tpop;
   size_t base;                /* Index to start of current bolo */
   size_t bstride;             /* Bolo stride */
   size_t itime_end;           /* Time index at end of usable data stream */
   size_t itime_start;         /* Time index at start of usable data stream */
   size_t ns;
   size_t tstride;             /* Bolo stride */
   smf_qual_t *pq1;
   smf_qual_t *qua = NULL;  /* Pointer to quality flags */

/* Assign values to various configuration parameters that have not yet
   been made public. */
   double dcthresh2 = 7.0;
   float dcminpop = 0.05;
   int dcmediangap = 3;
   double dcminsignratio = 0.8;
   int dcminstepwidth = 0.7*dcmedianwidth;
   int dcmaxstepwidth = 3.0*dcmedianwidth;
   int dcminbox = 100;
   int dcminstepgap = 0.5*dcmedianwidth;

/* Initialise... */
   ns = 0;
   nfixed = 0;

#ifdef DEBUG_STEPS
   static int nentry = 0;
   char buf[ 200 ];

   sprintf( buf, "bolo_%d.asc", ++nentry );
   FILE *fd1 = fopen( buf, "w" );
   fprintf( fd1, "# dcthresh=%g\n", dcthresh );
   fprintf( fd1, "# dctmedianwidth=%d\n", dcmedianwidth );
   fprintf( fd1, "# dcfitbox=%d\n", dcfitbox );
   fprintf( fd1, "# dcmaxsteps=%d\n", dcmaxsteps );
   fprintf( fd1, "# ibolo rms_jump thresh\n");

   sprintf( buf, "data_%d.asc", nentry );
   FILE *fd2 = fopen( buf, "w" );
   fprintf( fd2, "# itime indata inquality outdata outquality ibolo jump "
            "median in_rms flags istep_width step_width nsign change fac "
            "diff thresh line_start line_end rmslo rmshi\n");
#endif



/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Check we have double precision data. */
   if( !smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status ) ) return;

/* Get pointers to data and quality arrays. */
   dat = data->pntr[ 0 ];
   if( quality ) {
      qua = quality;
   } else {
      qua = data->pntr[ 2 ];
   }

/* Report an error if either are missing. */
   if( !qua ) {
      *status = SAI__ERROR;
      errRep( "", "smf_fix_steps: No valid QUALITY array was provided", status );

   } else if( !dat ) {
      *status = SAI__ERROR;
      errRep( "", "smf_fix_steps: smfData does not contain a DATA component",
              status );
   }

/* Get the data dimensions and strides. */
   smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, NULL, &bstride,
                 &tstride, status );

/* Report an error if the data stream is too short for the box size. */
   if( dcmediangap + dcfitbox*2 > ntslice && *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti( "NTSLICE", ntslice );
      msgSeti( "dcfitbox", dcfitbox );
      errRep( " ", "smf_fix_steps: Can't find jumps: ntslice=^NTSLICE, "
              "must be > dcfitbox (=^dcfitbox)*2", status );
   }

/* Check for valid threshold */
   if( dcthresh <= 0  && *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSetd( "DCTHRESH", dcthresh );
      errRep( " ", "smf_fix_steps: Can't find jumps: dcthresh "
              "(^dcthresh) must be > 0", status );
   }

   if( dcmedianwidth <= 3  && *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti( "V", dcmedianwidth );
      errRep( " ", "smf_fix_steps: Can't find jumps: dcmedianwidth "
              "(^V) must be > 3", status );
   }

/* Find, and optionally repair, DC steps. */
   if( dcfitbox && (*status == SAI__OK) ) {

/* Identify the first and last samples before/after padding. */
      smf_get_goodrange( qua, ntslice, tstride, SMF__Q_PAD,
                         &itime_start, &itime_end, status );
      pad_start = itime_start;
      pad_end = itime_end;

/* Identify the first and last samples before/after padding. */
      smf_get_goodrange( qua, ntslice, tstride, SMF__Q_BOUND,
                         &itime_start, &itime_end, status );

/* Store the number of time slices in the good range. */
      ntime = itime_end - itime_start + 1;

/* Check here for bad status before allocating memory */
      if( *status != SAI__OK ) return;

#ifdef DEBUG_STEPS
   TimeData *timedata = astMalloc( ntime*sizeof( *timedata ) );
   BoloData *bolodata = astMalloc( sizeof( *bolodata ) );
#endif

/* Allocate work arrays. */
      w1 = astMalloc( sizeof( *w1 )*dcmedianwidth );
      w2 = astMalloc( sizeof( *w2 )*dcmedianwidth );
      work = astMalloc( sizeof( *work )*ntime );
      blocks = astMalloc( sizeof( *blocks )*200 );
      common = astMalloc( sizeof( *common )*ntime );

/* The "common" array has an element for every used time slice, and each
   element holds the number of bolometers that have a candidate step at
   the time slice. Initialise this array to hold zero for all time
   slices. */
      memset( common, 0, sizeof( *common )*ntime );

/* Get the index of the time slice mid way between the two median boxes
   at the first and last usable time slices. Use the upper of the two
   middle indices if there is an even number. The itime_lo and itime_hi
   values are offsets relative to itime_start. */
      itime_lo = dcmedianwidth + dcmediangap/2;
      itime_hi = ntime - dcmedianwidth - ( dcmediangap + 1 )/2;

/* Loop round all usable bolometers. "base" holds the offset to the start
   of the usable data for the bolometer.  */
      for( ibolo = 0; ibolo < nbolo && *status==SAI__OK; ibolo++ ) {
         base = ibolo*bstride + itime_start*tstride;
         if( !(qua[ base ] & SMF__Q_BADB) ) {

#ifdef DEBUG_STEPS

   if( RECORD_BOLO ) {
      int kk;
      double *pd = dat + base;
      smf_qual_t *pq = qua + base;
      for( kk = 0; kk < ntime; kk++ ) {
         timedata[ kk ].indata = !( *pq & SMF__Q_MOD ) ? *pd : VAL__BADD;
         timedata[ kk ].inquality = (int) *pq;
         timedata[ kk ].outdata = VAL__BADD;
         timedata[ kk ].outquality = 0;
         timedata[ kk ].ibolo = ibolo;
         timedata[ kk ].jump = VAL__BADD;
         timedata[ kk ].median = VAL__BADD;
         timedata[ kk ].in_rms = 0;
         timedata[ kk ].flags = 0;
         timedata[ kk ].istep_width = 0;
         timedata[ kk ].step_width = VAL__BADD;
         timedata[ kk ].nsign = VAL__BADD;
         timedata[ kk ].change = VAL__BADD;
         timedata[ kk ].fac = VAL__BADD;
         timedata[ kk ].diff = VAL__BADD;
         timedata[ kk ].thresh = VAL__BADD;
         timedata[ kk ].line_start = VAL__BADD;
         timedata[ kk ].line_end = VAL__BADD;
         timedata[ kk ].rmslo = VAL__BADD;
         timedata[ kk ].rmshi = VAL__BADD;

         pd += tstride;
         pq += tstride;
      };
   }

   bolodata->ibolo = ibolo;
   bolodata->rms_jump = VAL__BADD;
   bolodata->thresh = VAL__BADD;

#endif

/* Indicate nothing found yet. */
            nblock = 0;

/* Smooth the bolometer data stream using a median block filter. Put the
   smoothed data in "work". */
            smf_median_smooth( dcmedianwidth, SMF__FILT_MEDIAN, ntime,
                               dat + base, qua + base,
                               tstride, SMF__Q_MOD, work, w1, w2, status );

/* For each time slice, find the difference between the median value
   before and after the time slice. We leave a small gap between the two
   median boxes to allow for some rise time in any potential step. Find
   the RMS value of these differences (iterate a few times, ignoring
   differences larger than 2*RMS, to reduce the effect of really big
   jumps on the RMS value). */
            diff2_limit = VAL__MAXD;

            for( iter = 0; iter < 3; iter++ ) {
               tpop = 0;
               tsum2 = 0.0;
               pw1 = work + dcmedianwidth/2;
               pw2 = pw1 + dcmedianwidth + dcmediangap;
               for( itime = itime_lo; itime <= itime_hi; itime++,pw1++,pw2++ ) {
                  if( *pw1 != VAL__BADD && *pw2 != VAL__BADD ) {
                     diff = *pw2 - *pw1;
                     diff2 = diff*diff;
                     if( diff2 < diff2_limit ) {
                        tsum2 += diff*diff;
                        tpop++;

#ifdef DEBUG_STEPS
      if( RECORD_BOLO ) timedata[ itime ].in_rms = 1;
   } else {
      if( RECORD_BOLO ) timedata[ itime ].in_rms = 0;
#endif

                     }

#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      timedata[ itime ].jump = diff;
      timedata[ itime ].median = work[ itime ];
   }
#endif


                  }
               }

               if( tpop > SMF__MINSTATSAMP ) {
                  rms = sqrtf( tsum2/tpop );
                  diff2_limit = 2.0*rms;
                  diff2_limit *= diff2_limit;
               } else {
                  rms = VAL__BADD;
                  break;
               }
            }

/* If the time stream contains too few values, flag the entire bolometer as
   bad, and pass on to the next bolometer. */
            if( rms == VAL__BADD ) {
               msgOutiff( MSG__DEBUG, "", "smf_fix_steps: flagging "
                          "entire bad bolo %" DIM_T_FMT ", due to "
                          "insufficient samples", status, ibolo );
               pq1= qua + ibolo*bstride;
               for( itime = 0; itime < (int) ntslice; itime++) {
                 *pq1 |= SMF__Q_BADB;
                  pq1 += tstride;
               }
               ns++;
               continue;
            }

/* Correct for the severe 2 sigma clipping in the estimation of rms. */
            rms *= 1.28;

/* Indicate we are currently looking for the start of a step. */
            step_start = -1;
            step_end = -1;
            step_limit = -1;
            start_value = VAL__BADD;
            end_value = VAL__BADD;
            nsign = 0;

/* Find the minimum significant gradient. */
            thresh = rms*dcthresh;

#ifdef DEBUG_STEPS
   bolodata->rms_jump = rms;
   bolodata->thresh = thresh;
#endif



/* Scan through the range of time slices for which both median box values
   can be found. The "itime" variable gives the index of the central
   sample in the gap between the two median boxes (the upper of the two
   central samples if dcmediangap is even). */
            pw1 = work + dcmedianwidth/2;
            pw2 = pw1 + dcmedianwidth + dcmediangap;
            for( itime = itime_lo; itime <= itime_hi; itime++,pw1++,pw2++ ) {

/* Get the difference at this time */
               if( *pw1 != VAL__BADD && *pw2 != VAL__BADD ) {
                  diff = *pw2 - *pw1;

/* If the absolute difference between the two median values is greater than the
   minimum significant step height, note the sign of the difference. */
                  if( diff > thresh ) {
                     diff = 1.0;
                  } else if( diff < -thresh ) {
                     diff = -1.0;
                  } else {
                     diff = 0.0;
                  }

/* Skip times for which the median values of the data before and after
   the time are insufficiently different. */
                  if( diff != 0.0 ) {

#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      if( diff > 0 ) {
         timedata[ itime ].flags |= UP_STEP;
      } else {
         timedata[ itime ].flags |= DOWN_STEP;
      }
   }
#endif


/* If we are currently looking for the start of a new step, record the
   index of the step start, and the median value before the start. */
                     if( step_start == -1 ) {
                        step_start = itime;
                        start_value = *pw1;
                        nsign = 0;
                     }

/* Update the index of the step end, the earliest time at which the step
   can be considered complete, and the median value after the end. */
                     step_end = itime;
                     step_limit = itime + dcminstepgap;
                     end_value = *pw2;

/* Find the difference between the number of positive and negative
   excursions. A clear step should be predominantly either positive or
   negative, although in practice some contrary values can exist
   in a genuine step. */
                     if( diff > 0.0 ) {
                        nsign++;
                     } else {
                        nsign--;
                     }

/* If the difference between the two median values is smaller than the
   minimum significant step height, and we are currently looking for the
   end of a step, and we have reached the earliest possible time at
   which the step could end, we now know where the step ends. */
                  } else if( step_start != -1 && itime >= step_limit ) {

/* If the step rise is too short or too long, we ignore the step. Also,
   if the sign of the step is insufficiently clear, we ignore it. */
                     step_width = step_end - step_start + 1;

#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      for( jtime = step_start; jtime <= step_end; jtime++ ) {
         timedata[ jtime ].flags |= CHECK_1;
         timedata[ jtime ].istep_width = step_width;
         timedata[ jtime ].step_width = (double)(step_width-dcminstepwidth)/(double)(dcmaxstepwidth-dcminstepwidth);
         timedata[ jtime ].nsign = (double)abs(nsign)/(double)(dcminsignratio*step_width);
      }
      printf("dcminstepwidth=%d dcmaxstepwidth=%d dcminsignratio=%g\n", dcminstepwidth, dcmaxstepwidth, dcminsignratio);
   }
#endif

                     if( step_width >= dcminstepwidth &&
                         step_width <= dcmaxstepwidth &&
                         abs( nsign ) >= dcminsignratio*step_width ) {

/* We also ignore it if the total change in data value across the step is
   too low. Raise the threshold in proportion to the width of the step if
   the step is wider than the width expected for an instantaneous step. */
                        fac = (double) step_width/(double)( dcmedianwidth
                                                          + dcminstepgap );
                        if( fac < 1.0 ) fac = 1.0;

#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      for( jtime = step_start; jtime <= step_end; jtime++ ) {
         timedata[ jtime ].flags |= CHECK_2;
         timedata[ jtime ].fac = fac;
         timedata[ jtime ].change = fabs( start_value-end_value)/(thresh*fac);
      }
   }
#endif

                        if( fabs( start_value - end_value ) > thresh*fac ) {

/* Store the indices of the step start and end in the "blocks" array,
   expanding the array if necessary. */
                           iblock = nblock++;
                           blocks = astGrow( blocks, 2*nblock, sizeof( *blocks ) );
                           if( *status == SAI__OK ) {
                              block = blocks + 2*iblock;
                              block[ 0 ] = step_start;
                              block[ 1 ] = step_end;

#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      for( jtime = step_start; jtime <= step_end; jtime++ ) {
         timedata[ jtime ].flags |= IN_BLOCK;
      }
   }
#endif



                           }
                        }
                     }

/* Indicate we are now looking for the start of a new step. */
                     step_start = -1;
                  }
               }
            }

/* Now measure each candidate jump and apply all required corrections to
   the bolometer time stream. */
            ns += smf1_step_correct( nblock, blocks, work, qua, dat, base,
                                     ibolo, bstride, tstride, ntslice,
                                     ntime, itime_lo, itime_hi, common,
                                     dcmaxsteps*( ((double) tpop)/12000.0 ),
                                     dcfitbox, dcminbox, dcminpop, dcthresh,
                                     dcthresh2, dcmaxstepwidth, pad_start,
                                     itime_start, itime_end, &nfixed,
                                     status DEBUG_VALS );
         }












#ifdef DEBUG_STEPS
   if( !(qua[ base ] & SMF__Q_BADB) ) {
      if( RECORD_BOLO ) {
         double *pd = dat + base;
         smf_qual_t *pq = qua + base;
         for( itime = 0; itime < ntime; itime++ ) {
            timedata[ itime ].outdata = !( *pq & SMF__Q_MOD ) ? *pd : VAL__BADD;
            timedata[ itime ].outquality = (int) *pq;
            pd += tstride;
            pq += tstride;

            fprintf( fd2, "%d ", itime);
            TOPCAT( fd2, timedata[itime].indata );
            fprintf( fd2, "%d ", timedata[itime].inquality);
            TOPCAT( fd2, timedata[itime].outdata );
            fprintf( fd2, "%d ", timedata[itime].outquality);
            fprintf( fd2, "%d ", timedata[itime].ibolo);
            TOPCAT( fd2, timedata[itime].jump );
            TOPCAT( fd2, timedata[itime].median );
            fprintf( fd2, "%d ", timedata[itime].in_rms );
            fprintf( fd2, "%d ", timedata[itime].flags );
            fprintf( fd2, "%d ", timedata[itime].istep_width );
            TOPCAT( fd2, timedata[itime].step_width );
            TOPCAT( fd2, timedata[itime].nsign );
            TOPCAT( fd2, timedata[itime].change );
            TOPCAT( fd2, timedata[itime].fac );
            TOPCAT( fd2, timedata[itime].diff );
            TOPCAT( fd2, timedata[itime].thresh );
            TOPCAT( fd2, timedata[itime].line_start );
            TOPCAT( fd2, timedata[itime].line_end );
            TOPCAT( fd2, timedata[itime].rmslo );
            TOPCAT( fd2, timedata[itime].rmshi );
            fprintf( fd2, "\n" );


         }
      }

      fprintf( fd1, "%d ", bolodata->ibolo );
      TOPCAT( fd1, bolodata->rms_jump );
      TOPCAT( fd1, bolodata->thresh );
      fprintf( fd1, "\n" );
   }
#endif






      }

/* A large, detected, step at a certain time often corresponds to smaller
   undetected steps at the same time in other bolometers. If required, force
   a step correction in every bolometer at times for which more than
   "dclimcorr" bolometers showed a large detected step above. */
      if( dclimcorr > 0 ) {

/* The "common" array holds the number of bolometers for which a step was
   detected above at each time slice. Locate blocks of time slices at
   which more than "dclimcorr" bolometers were found to have large steps
   above. */
         step_start = -1;
         nblock = 0;
         for( jtime = 0; jtime < ntime; jtime++ ) {

            if( common[ jtime ] > dclimcorr ) {
               if( step_start == -1 ) step_start = jtime;

            } else {
               if( step_start != -1 ) {
                  step_end = jtime - 1;
                  step_width = step_end - step_start + 1;

                  iblock = nblock++;
                  blocks = astGrow( blocks, 2*nblock, sizeof( *blocks ) );
                  if( *status == SAI__OK ) {
                     block = blocks + 2*iblock;
                     block[ 0 ] = step_start;
                     block[ 1 ] = step_end;
                  }

                  step_start = -1;
               }
            }
         }

/* If any correlated steps were found, correct every bolometer at the
   position of each correlated jump. */
         if( nblock > 0 ) {
            for( ibolo = 0; ibolo < nbolo && *status==SAI__OK; ibolo++ ) {
               base = ibolo*bstride + itime_start*tstride;
               if( !(qua[ base ] & SMF__Q_BADB) ) {

                  ns += smf1_step_correct( nblock, blocks, work, qua, dat,
                                           base, ibolo, bstride, tstride,
					   ntslice, ntime, itime_lo,
					   itime_hi, NULL, 0, dcfitbox,
					   dcminbox, dcminpop, 0.0,
					   dcthresh2, dcmaxstepwidth, pad_start,
                                           itime_start, itime_end,
                                           &nfixed, status DEBUG_VALS );

               }
            }
         }
      }

/* Free workspace */
      w1 = astFree( w1 );
      w2 = astFree( w2 );
      work = astFree( work );
      blocks = astFree( blocks );
      common = astFree( common );

#ifdef DEBUG_STEPS
   timedata = astFree( timedata );
   bolodata = astFree( bolodata );
#endif

   }

/* Report the number of fixed steps */
   msgOutiff( MSG__VERB, " ", "smf_fix_steps: fixed %d steps.",
              status, nfixed );

/* Report the number of rejected bolometers. */
   if( ns > 0 ) {
      msgOutiff( MSG__VERB, " ", "smf_fix_steps: flagged %zu bad bolos.",
                 status, ns );
   }

/* Return the number of rejected bolometers. */
   if( nsteps ) {
     *nsteps = ns;
   }

#ifdef DEBUG_STEPS
   fclose( fd1 );
   fclose( fd2 );
#endif


}




static void smf1_step_linefit( int box, float minfrac, int stride, double *dat,
                               smf_qual_t *qua, double nsigma, double *m,
                               double *c, double *rms, int *status ){

/* Local Variables: */
   double *pd;
   double cl;
   double denom;
   double ml;
   double sx2;
   double sx;
   double sxx2;
   double sxx;
   double sxy2;
   double sxy;
   double sy2;
   double sy;
   double syy2;
   double syy;
   double thresh;
   int i;
   int minpop;
   int niter;
   int pop2;
   int pop;
   smf_qual_t *pq;

/* Initialise returned values. */
   *m = VAL__BADD;
   *c = VAL__BADD;
   *rms = VAL__BADD;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Get the required sums, including all the supplied points that have
   good quality. */
   sy = 0.0;
   sx = 0.0;
   sxy = 0.0;
   sxx = 0.0;
   syy = 0.0;
   pop = 0;

   pq = qua;
   pd = dat;
   for( i = 0; i < box; i++ ) {
      if( !( *pq & SMF__Q_GOOD ) ) {
         sy += *pd;
         sx += i;
         sxy += i*( *pd );
         sxx += i*i;
         syy += ( *pd )*( *pd );
         pop++;
      }
      pq += stride;
      pd += stride;
   }

/* Set the minimum allowed population. */
   minpop = pop*minfrac;
   if( minpop < 4 ) minpop = 4;

/* Produce a copy of these stats so that we can retain the original full
   stats for subsequent loops. These "..2" values are the statistics for
   the points that have not been flagged as aberrant. So far no points
   have been flagged as aberrant. */
   sy2 = sy;
   sx2 = sx;
   sxy2 = sxy;
   sxx2 = sxx;
   syy2 = syy;
   pop2 = pop;

/* Nwo Now loop until we have convergence, or we have done two passes. */
   ml = VAL__BADD;
   cl = VAL__BADD;
   niter = 0;
   while( 1 ) {

/* Get the gain and offset of the least squares linear fit to the data
   values that have not been flagged as aberrant. Also find the RMS about
   the line. */
      denom =  pop2*sxx2 - sx2*sx2;
      if( denom > 0 && pop2 > minpop ) {
         *m =  ( pop2*sxy2 - sx2*sy2 )/denom;
         *c =  ( sxx2*sy2 - sx2*sxy2 )/denom;
         *rms = pop2*syy2 - sy2*sy2 - denom*( *m )*( *m );
         *rms = ( *rms > 0.0 ) ? sqrtf( *rms )/pop2: 0.0;

/* If the slope and offset have not changed, we have convergence. */
         if( ( *m == ml && *c == cl ) || ++niter == 3 ) break;

/* Save the current slope and offset so we can check for convergence on
   the next pass. */
         ml = *m;
         cl = *c;

/* Re-establish the statistics for the full set of data values. */
         sy2 = sy;
         sx2 = sx;
         sxy2 = sxy;
         sxx2 = sxx;
         syy2 = syy;
         pop2 = pop;

/* Identify aberrant points and remove them from these sums. Note, doing
   it like this means that points that were flagged as aberrant on previous
   passes round the convergence loop may not be aberrant now. Which is
   good. Also, subtracting the bad points from the previous statistics
   will involve less arithmatic (and so be quicker) than forming new
   statistics from scratch. */
         thresh = (*rms)*nsigma;
         pq = qua;
         pd = dat;
         for( i = 0; i < box; i++ ) {

            if( !( *pq & SMF__Q_GOOD ) && fabs( *pd - ( i*(*m) + (*c) ) ) > thresh ){
               sy2 -= *pd;
               sx2 -= i;
               sxy2 -= i*( *pd );
               sxx2 -= i*i;
               syy2 -= ( *pd )*( *pd );
               pop2--;
            }
            pq += stride;
            pd += stride;
         }

      } else {
         denom = 0;
         break;
      }
   }

/* Return bad values if the line is undefined. */
   if( denom == 0.0 ) {
      *m = VAL__BADD;
      *c = VAL__BADD;
   }
}


/* Loop round correcting steps at supplied times. */

static int smf1_step_correct( int nblock, int *blocks, double *work,
                              smf_qual_t *qua, double *dat, size_t base,
                              dim_t ibolo, size_t bstride, size_t tstride,
                              dim_t ntslice, int ntime, int itime_lo,
                              int itime_hi, int *common, int maxsteps,
                              dim_t dcfitbox, int dcminbox, float dcminpop,
                              double dcthresh, double dcthresh2,
                              int dcmaxstepwidth, int pad_start,
                              int itime_start, int itime_end, int *nfixed,
                              int *status DEBUG_ARGS ){

/* Local Variables: */
   double *pd;
   double *pw;
   double avecorr;
   double chi;
   double clo;
   double corr;
   double diff;
   double end_value;
   double mhi;
   double mlo;
   double rmshi;
   double rmslo;
   double start_value;
   double sumcorr;
   double thresh;
   int *block;
   int boxend;
   int boxlen;
   int boxstart;
   int end_prev;
   int iblock;
   int itime;
   int jtime;
   int ncorr;
   int nstep;
   int result;
   int start_next;
   int step_end;
   int step_start;
   int step_width;
   smf_qual_t *pq1;
   smf_qual_t *pq;

/* Initialise returned value */
   result = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Initialise the sums used for finding the average correction. */
   ncorr = 0;
   sumcorr = 0.0;

/* Now loop round all blocks. Each block specifies the start and end time
   for a single step rise or fall. */
   nstep = 0;
   corr = 0.0;
   block = blocks;
   pw = work;
   itime = 0;
   pq = qua + base;

   for( iblock = 0; iblock < nblock; iblock++) {
      step_start = block[ 0 ];
      step_end = block[ 1 ];
      step_width = step_end - step_start + 1;

/* Find the start of next step (if any) and the end of the previous step
   (if any). */
      start_next = ( iblock + 1 < nblock ) ? block[ 2 ] : ntime;
      end_prev = ( iblock > 0 ) ? block[ -1 ] : 0;

/* If required, increment the number of bolometers that have a high gradient
   at the steps in this block. */
      if( common ) {
         for( jtime = step_start; jtime <= step_end; jtime++ ){
            common[ jtime ]++;
         }
      }

/* Store the current correction up to the start of the current block. */
      for( ; itime < step_start; itime++,pw++ ) {
         if( !( *pq & SMF__Q_MOD ) ) {
            *pw = corr;
            ncorr++;
            sumcorr += corr;
         } else {
            *pw = VAL__BADD;
         }
         pq += tstride;
      }

/* We now fit a straight line to the data just before the step. The fit
   box ends at the start of the step, and usually extends for "dcfitbox"
   samples. However, the box is truncated at the end of the previous step
   (so long as this leaves at least DCMAXSTEPWIDTH samples in the box),
   or at the start of the time stream. */
      boxend = step_start - 1;
      boxstart = end_prev + 1;
      boxlen = boxend - boxstart + 1;
      if( boxlen < dcmaxstepwidth ){
         boxlen = dcmaxstepwidth;
         boxstart = boxend - boxlen + 1;
      }
      if( boxstart < 0 ) boxstart = 0;
      if( boxstart < boxend - (int) dcfitbox + 1 ) boxstart = boxend - dcfitbox + 1;
      boxlen = boxend - boxstart + 1;

/* Fit the straight line to the data determined above, and get the slope and
   gradient. The independent variable of the fit is the offset from the start
   of the box (i.e. boxstart). If the box length is less than dcminbox,
   we skip this block. */
      if( boxlen > dcminbox ) {
         smf1_step_linefit( boxlen, dcminpop, tstride,
                            dat + base + boxstart*tstride,
                            qua + base + boxstart*tstride,
                            dcthresh2, &mlo, &clo, &rmslo, status );

/* If the box length is less than dcminbox, we skip this block. */
      } else {
         mlo = VAL__BADD;

/* If the box is short because it has extended to the start of the time
   series, then flag all the samples from the start of the time series
   as a jump, so that they will not be used. */
         if( boxstart == 0 ) {
            pq1 = qua + base + boxstart*tstride;
            for( jtime = 0; jtime < boxlen; jtime++ ) {
               *pq1 |= SMF__Q_JUMP;
                pq1 += tstride;
            }
         }

      }

/* Get the data value at the centre of the step as implied by the
   above fitted line. */
      if( mlo != VAL__BADD ) {
         start_value = mlo*( boxlen + step_width/2 ) + clo;

#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      timedata[ boxstart ].line_end = clo;
      timedata[ step_start ].line_start = mlo*boxlen + clo;
   }
#endif

      } else {
         start_value = VAL__BADD;
      }

/* In the same way fit a line to the data just after the step, and get
   the data value at the centre of the step as implied by the fitted line. */
      boxstart = step_end + 1;
      boxend = start_next - 1;
      boxlen = boxend - boxstart + 1;
      if( boxlen < dcmaxstepwidth ){
         boxlen = dcmaxstepwidth;
         boxend = boxstart + boxlen - 1;
      }
      if( boxend > ntime - 1 ) boxend = ntime - 1;
      if( boxend > boxstart + (int) dcfitbox - 1 ) boxend = boxstart + dcfitbox - 1;
      boxlen = boxend - boxstart + 1;

      if( boxlen > dcminbox ) {
         smf1_step_linefit( boxlen, dcminpop, tstride,
                            dat + base + boxstart*tstride,
                            qua + base + boxstart*tstride,
                            dcthresh2, &mhi, &chi, &rmshi, status );
      } else {
         mhi = VAL__BADD;

         if( boxend == ntime - 1 ) {
            pq1 = qua + base + boxstart*tstride;
            for( jtime = 0; jtime < boxlen; jtime++ ) {
               *pq1 |= SMF__Q_JUMP;
                pq1 += tstride;
            }
         }
      }

      if( mhi != VAL__BADD ) {
         end_value = mhi*( -step_width/2 ) + chi;

#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      timedata[ boxstart ].line_start = chi;
      timedata[ boxstart + boxlen ].line_end = mhi*boxlen + chi;
   }
#endif


      } else {
         end_value = VAL__BADD;
      }

/* Check the above two lines are defined. */
      if( end_value != VAL__BADD && start_value != VAL__BADD ) {

/* Check the step is large enough. */
         diff = end_value - start_value;
         if( rmslo > rmshi ) {
            thresh = rmslo*dcthresh;
         } else {
            thresh = rmshi*dcthresh;
         }




#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      for( jtime = step_start; jtime <= step_end; jtime++ ) {
         timedata[ jtime ].flags |= CHECK_3;
         timedata[ jtime ].diff = diff;
         timedata[ jtime ].thresh = thresh;
         timedata[ jtime ].rmshi = rmshi;
         timedata[ jtime ].rmslo = rmslo;
      }
   }
#endif



         if( fabs( diff ) >= thresh ) {

/* Increment the number of steps found in this bolometer. */
            nstep++;

/* Increment the current correction. */
            corr -= diff;

/* Store bad corrections over the duration of the step itself. */
            for( ; itime <= step_end; itime++,pw++ ) {
               *pw = VAL__BADD;
               pq += tstride;
            }
         }
      }
      block += 2;

   }

/* Assign the current correction to the remaining time slices. */
   for( ; itime < ntime; itime++,pw++ ) {
      if( !( *pq & SMF__Q_MOD ) ) {
         *pw = corr;
         ncorr++;
         sumcorr += corr;
      } else {
         *pw = VAL__BADD;
      }
      pq += tstride;
   }

/* If this bolometer has no usable corrections or has too many steps, set
   the entire bolo bad. */
   if( ncorr == 0 || ( maxsteps > 0 && nstep > maxsteps ) ) {
      result = 1;
      pq1 = qua + ibolo*bstride;
      for( jtime = 0; jtime < (int) ntslice; jtime++) {
        *pq1 |= SMF__Q_BADB;
         pq1 += tstride;
      }
      msgOutiff( MSG__DEBUG, " ", "smf_fix_steps: "
                 "flagging bad bolo %" DIM_T_FMT,
                 status, ibolo );
      ncorr = 0;

/* Otherwise, add the correction onto the original data and flag each
   jump. Modify the corrections to have an average value of zero. */
   } else {
      avecorr = sumcorr/ncorr;

/* The first dcmedianwidth slices after the padding section have not been
   tested for steps, so all we can do is pass them through unchanged
   (except for adding the initial correction to ensure they are continuous
   with the corrected data). Note, "itime_lo" and "itime_hi" are offsets
   from "itime_start", not from zero. */
      pd = dat + ibolo*bstride + pad_start*tstride;
      for( itime = pad_start; itime < itime_lo + itime_start; itime++) {
         if( *pd != VAL__BADD ) *pd -= avecorr;
         pd += tstride;
      }

/* Add on the full correction over the useful central section. */
      pd = dat + base + itime_lo*tstride;
      pq = qua + base + itime_lo*tstride;
      pw = work + itime_lo;

      corr = 0;
      for( ; itime <= itime_hi + itime_start; itime++,pw++ ) {
         if( *pw != VAL__BADD ) {
             corr = *pw - avecorr;
         } else {
            *pq |= SMF__Q_JUMP;
         }
         *pd += corr;

         pd += tstride;
         pq += tstride;
      }

/* The last dcmedianwidth slices before the final padding section have not
   been tested for steps, so all we can do is pass them through unchanged
   (except for adding the initial correction to ensure they are continuous
   with the corrected data). */
      for( ; itime <= itime_end; itime++) {
         if( *pd != VAL__BADD ) *pd += corr;
         pd += tstride;
      }

/* Incrment the total number of steps fixed so far. */
      *nfixed += nstep;
   }

   return result;
}


