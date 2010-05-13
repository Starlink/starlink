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
*                         unsigned char *quality, double dcthresh,
*                         dim_t dcmedianwidth, dim_t dcfitbox, int dcmaxsteps,
*                         size_t *nsteps, int *status )

*  Arguments:
*     wf = smfWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData * (Given and Returned)
*        The data that will be repaired (in-place)
*     quality = unsigned char * (Given and Returned)
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
*     nsteps = size_t* (Returned)
*        Number of bolometers rejected because they had too many steps
*        (i.e. more than indicated by "dcmaxsteps").
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

#define RECORD_BOLO (ibolo==669||ibolo==232||ibolo==144||ibolo==826||ibolo==1105)

#define TOPCAT(fd, x) \
   if( x != VAL__BADD ) { \
      fprintf( fd, "%g ", x ); \
   } else { \
      fprintf( fd, "null " ); \
   }

typedef struct Tmp {
   double indata;
   double outdata;
   int inquality;
   int outquality;
   int ibolo;
   double jump;
   int flag;
   double lostart;
   double loend;
   double histart;
   double hiend;
   double lorms;
   double hirms;
   double step_hgt;
   double median;
   double merit;
} Tmp;

#endif


/* Prototypes for private functions. */
static void smf1_step_linefit( int box, float minfrac, int stride, double *dat,
                               unsigned char *qua, double nsigma, double *m,
                               double *c, double *rms, int *status );












void smf_fix_steps( smfWorkForce *wf, smfData *data, unsigned char *quality,
                    double dcthresh, dim_t dcmedianwidth, dim_t dcfitbox,
                    int dcmaxsteps, size_t *nsteps, int *status ) {

/* Local Variables */
   dim_t ibolo;                /* Index of bolometer */
   dim_t nbolo;                /* Number of bolometers */
   dim_t ntslice;              /* Number of time slices */
   double *dat = NULL;         /* Pointer to bolo data */
   double *pd;
   double *pw1;
   double *pw2;
   double *pw;
   double *w1;
   double *w2;
   double *work;
   double avecorr;
   double chi;
   double clo;
   double corr;
   double diff;
   double diff2;
   double diff2_limit;
   double end_value;
   double fac;
   double mhi;
   double mlo;
   double rms;
   double rmshi;
   double rmslo;
   double start_value;
   double sumcorr;
   double thresh;
   double tsum2;
   int *block;
   int *blocks;
   int boxlen;
   int boxstart;
   int iblock;
   int iter;
   int itime;                  /* Index of time slice */
   int itime_hi;
   int itime_lo;
   int jtime;                  /* Index of time slice */
   int nblock;
   int ncorr;
   size_t ns;
   int nsign;
   int nstep;
   int ntime;                  /* Number of time slices in usable range */
   int step_end;
   int step_limit;
   int step_start;
   int step_width;
   int tpop;
   size_t base;                /* Index to start of current bolo */
   size_t bstride;             /* Bolo stride */
   size_t itime_end;           /* Time index at end of usable data stream */
   size_t itime_start;         /* Time index at start of usable data stream */
   size_t tstride;             /* Bolo stride */
   unsigned char *pq1;
   unsigned char *pq;
   unsigned char *qua = NULL;  /* Pointer to quality flags */


/* Assign values to various configuration parameters that have not yet
   been made public. */
   double dcthresh2 = 7.0;
   float dcminpop = 0.05;
   int dcmediangap = 3;
   int dcminsignratio = 0.8;
   int dcminstepwidth = 0.7*dcmedianwidth;
   int dcmaxstepwidth = 1.8*dcmedianwidth;
   int dcminbox = 100;
   int dcminstepgap = 50;

/* Initialise returned values. */
   ns = 0;


#ifdef DEBUG_STEPS
   static int nentry = 0;
   char buf[ 200 ];

   sprintf( buf, "bolo_%d.asc", ++nentry );
   FILE *fd1 = fopen( buf, "w" );
   fprintf( fd1, "# dcthresh=%g\n", dcthresh );
   fprintf( fd1, "# dctmedianwidth=%d\n", dcmedianwidth );
   fprintf( fd1, "# dcfitbox=%d\n", dcfitbox );
   fprintf( fd1, "# dcmaxsteps=%d\n", dcmaxsteps );
   fprintf( fd1, "# ibolo ngood nstep rms thresh rejected\n");

   sprintf( buf, "data_%d.asc", nentry );
   FILE *fd2 = fopen( buf, "w" );
   fprintf( fd2, "# itime indata inquality outdata outquality ibolo jump "
                 "flag lostart loend histart hiend lorms hirms step_hgt "
                 "median merit\n");
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

/* Identify the first and last samples before/after padding+apodization */
      smf_get_goodrange( qua, ntslice, 1, SMF__Q_BOUND,
                         &itime_start, &itime_end, status );

/* Store the number of time slices in the good range. */
      ntime = itime_end - itime_start + 1;


#ifdef DEBUG_STEPS
   Tmp *tmp = astMalloc( ntime*sizeof( *tmp ) );
#endif

/* Allocate work arrays. */
      w1 = astMalloc( sizeof( *w1 )*dcmedianwidth );
      w2 = astMalloc( sizeof( *w2 )*dcmedianwidth );
      work = astMalloc( sizeof( *work )*ntime );
      blocks = astMalloc( sizeof( *blocks )*200 );

/* Get the index of the time slice mid way between the two median boxes
   at the first and last usable time slices. Use the upper of the two
   middle indices if there is an even number. */
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
      pd = dat + base;
      pq = qua + base;
      for( kk = 0; kk < ntime; kk++ ) {
         tmp[ kk ].ibolo = ibolo;
         tmp[ kk ].indata = !( *pq & SMF__Q_MOD ) ? *pd : VAL__BADD;
         tmp[ kk ].inquality = (int) *pq;
         tmp[ kk ].outdata = VAL__BADD;
         tmp[ kk ].outquality = 0;
         tmp[ kk ].jump = VAL__BADD;
         tmp[ kk ].flag = 0;
         tmp[ kk ].lostart = VAL__BADD;
         tmp[ kk ].loend = VAL__BADD;
         tmp[ kk ].histart = VAL__BADD;
         tmp[ kk ].hiend = VAL__BADD;
         tmp[ kk ].lorms = VAL__BADD;
         tmp[ kk ].hirms= VAL__BADD;
         tmp[ kk ].step_hgt = VAL__BADD;
         tmp[ kk ].median = VAL__BADD;
         tmp[ kk ].merit = VAL__BADD;

         pd += tstride;
         pq += tstride;
      };
   }

#endif

/* Indicate nothing found yet. */
            nstep = 0;
            nblock = 0;

/* Smooth the bolometer data stream using a median block filter. Put the
   smoothed data in "work". */
            smf_median_smooth( dcmedianwidth, ntime, dat + base, qua + base,
                               tstride, SMF__Q_MOD, work, w1, w2, status );

/* For each time slice, find the difference between the median value
   before and after the time slice. We leave a small gap between the two
   median boxes to allow for some rise time in any potential step. Find
   the RMS value of these differences.  Iterate a few times, ignoring
   differences larger than 3*RMS, to reduce the effect of really big
   jumps. */
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
                     }

   #ifdef DEBUG_STEPS
      tmp[ itime ].jump = diff;
      tmp[ itime ].median = work[ itime ];
   #endif


                  }
               }

               if( tpop > SMF__MINSTATSAMP ) {
                  rms = sqrtf( tsum2/tpop );
                  diff2_limit = 3.0*rms;
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

/* Indicate we are currently looking for the start of a step. */
            step_start = -1;
            step_end = -1;
            step_limit = -1;
            start_value = VAL__BADD;
            end_value = VAL__BADD;

/* Find the minimum significant gradient. */
            thresh = rms*dcthresh;

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
   tmp[ itime ].flag |= 1;
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
   tmp[step_end].merit = fabs( start_value - end_value )/( thresh*fac );
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
                           }
                        }
                     }

/* Indicate we are now looking for the start of a new step. */
                     step_start = -1;
                  }
               }
            }

/* Initialise the sums used for finding the average correction. */
            ncorr = 0;
            sumcorr = 0.0;

/* Now loop round all blocks found above. Each block specifies the start
   and end time for a single step rise or fall. */
            corr = 0.0;
            block = blocks;
            pw = work;
            itime = 0;
            pq = qua + base;

            for( iblock = 0; iblock < nblock; iblock++) {
               step_start = block[ 0 ];
               step_end = block[ 1 ];
               step_width = step_end - step_start + 1;

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


#ifdef DEBUG_STEPS
   for( jtime = step_start; jtime <= step_end; jtime++ ) {
      tmp[ jtime ].flag |= 2;
   }
#endif


/* We now fit a straight line to the data just before the step. We use a
   box of length "dcfitbox", ending at the start of the step. */
               boxstart = step_start - dcfitbox - 1;
               if( boxstart < 0 ) {
                  boxstart = 0;
                  boxlen = step_start - boxstart - 1;
               } else {
                  boxlen = dcfitbox;
               }

/* Fit the straight line to the data determined above, and get the slope and
   gradient. The independent variable of the fit is the offset from the start
   of the box (i.e. boxstart). If the box length is less than dcminbox,
   we skip this block. */
               if( boxlen > dcminbox ) {
                  smf1_step_linefit( boxlen, dcminpop, tstride,
                                     dat + base + boxstart*tstride,
                                     qua + base + boxstart*tstride,
                                     dcthresh2, &mlo, &clo, &rmslo, status );
               } else {
                  mlo = VAL__BADD;
               }

/* Get the data value at the centre of the step as implied by the
   above fitted line. */
               if( mlo != VAL__BADD ) {
                  start_value = mlo*( boxlen + step_width/2 ) + clo;

#ifdef DEBUG_STEPS
   tmp[ boxstart ].lostart = clo;
   tmp[ boxstart + boxlen ].loend = mlo*boxlen + clo;
   tmp[ step_start ].lorms = rmslo;
#endif


               } else {
                  start_value = VAL__BADD;
               }

/* In the same way fit a line to the data just after the step, and get
   the data value at the centre of the step as implied by the fitted line. */
               boxstart = step_end + 1;
               boxlen = ntime - boxstart;
               if( boxlen > (int) dcfitbox ) boxlen = dcfitbox;

               if( boxlen > dcminbox ) {
                  smf1_step_linefit( boxlen, dcminpop, tstride,
                                     dat + base + boxstart*tstride,
                                     qua + base + boxstart*tstride,
                                     dcthresh2, &mhi, &chi, &rmshi, status );
               } else {
                  mhi = VAL__BADD;
               }

               if( mhi != VAL__BADD ) {
                  end_value = mhi*( -step_width/2 ) + chi;

#ifdef DEBUG_STEPS
   tmp[ boxstart ].histart = chi;
   tmp[ boxstart + boxlen ].hiend = mhi*boxlen + chi;
   tmp[ step_end ].hirms = rmshi;
#endif


               } else {
                  end_value = VAL__BADD;
               }

/* Check the above two lines are defined. */
               if( end_value != VAL__BADD && start_value != VAL__BADD ) {


#ifdef DEBUG_STEPS
   for( jtime = step_start; jtime <= step_end; jtime++ ) {
      tmp[ jtime ].flag |= 4;
   }
#endif

/* Check the step is large enough. */
                  diff = end_value - start_value;
                  if( rmslo > rmshi ) {
                     thresh = rmslo*dcthresh;
                  } else {
                     thresh = rmshi*dcthresh;
                  }

#ifdef DEBUG_STEPS
   tmp[ (step_end + step_start )/2].step_hgt = diff;
#endif

                  if( fabs( diff ) >= thresh ) {

#ifdef DEBUG_STEPS
   for( jtime = step_start; jtime <= step_end; jtime++ ) {
      tmp[ jtime ].flag |= 8;
   }
#endif


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

/* Assign the current correction to the remaing time slices. */
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
            if( ncorr == 0 || nstep > dcmaxsteps*( ((double) tpop)/12000.0 ) ) {
               ns++;
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

               pd = dat + base + itime_lo*tstride;
               pq = qua + base + itime_lo*tstride;
               pw = work + itime_lo;

               corr = 0;
               for( itime = itime_lo; itime <= itime_hi; itime++,pw++ ) {
                  if( *pw != VAL__BADD ) {
                      corr = *pw - avecorr;
                  } else {
                     *pq |= SMF__Q_JUMP;
                  }
                  *pd += corr;

                  pd += tstride;
                  pq += tstride;
               }
            }

#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      pd = dat + base;
      pq = qua + base;
      for( itime = 0; itime < ntime; itime++ ) {
         tmp[ itime ].outdata = !( *pq & SMF__Q_MOD ) ? *pd : VAL__BADD;
         tmp[ itime ].outquality = (int) *pq;
         pd += tstride;
         pq += tstride;

         fprintf( fd2, "%d ", itime);
         TOPCAT( fd2, tmp[itime].indata );
         fprintf( fd2, "%d ", tmp[itime].inquality);
         TOPCAT( fd2, tmp[itime].outdata );
         fprintf( fd2, "%d ", tmp[itime].outquality);
         fprintf( fd2, "%d ", tmp[itime].ibolo);
         TOPCAT( fd2, tmp[itime].jump );
         fprintf( fd2, "%d ", tmp[itime].flag);
         TOPCAT( fd2, tmp[itime].lostart );
         TOPCAT( fd2, tmp[itime].loend );
         TOPCAT( fd2, tmp[itime].histart );
         TOPCAT( fd2, tmp[itime].hiend );
         TOPCAT( fd2, tmp[itime].lorms );
         TOPCAT( fd2, tmp[itime].hirms );
         TOPCAT( fd2, tmp[itime].step_hgt );
         TOPCAT( fd2, tmp[itime].median );
         TOPCAT( fd2, tmp[itime].merit );
         fprintf( fd2, "\n" );
      }
   }
   fprintf( fd1, "%d %d %d %g %g %d\n", ibolo, tpop, nstep, rms, thresh, (ncorr==0) );
} else {
   fprintf( fd1, "%d null null null null null\n", ibolo );
#endif


         }
      }

/* Free workspace */
      w1 = astFree( w1 );
      w2 = astFree( w2 );
      work = astFree( work );
      blocks = astFree( blocks );

#ifdef DEBUG_STEPS
   tmp = astFree (tmp );
#endif

   }

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
                               unsigned char *qua, double nsigma, double *m,
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
   unsigned char *pq;

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
      if( !( *pq & SMF__Q_MOD ) ) {
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

            if( !( *pq & SMF__Q_MOD ) && fabs( *pd - ( i*(*m) + (*c) ) ) > thresh ){
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


