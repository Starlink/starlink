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
*                         double dcthresh2, dim_t dcbox, int dcflag,
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
*     dcthresh2 = double (Given)
*        N-sigma threshold used for rejecting aberrant points in the
*        linear fit to the data on either side of a candidate jump.
*     dcbox = dim_t (Given)
*        Length of box (in samples) over which each linear fit is
*        performed. If zero, no steps will be corrected.
*     dcflag = int (Given)
*        The maximum number of steps that can be corrected in a
*        bolometer before the entire bolometer is flagged as bad. A value
*        of zero will cause a bolometer to be rejected if any steps are
*        found in the bolometer data stream.
*     nsteps = size_t* (Returned)
*        Number of bolometers rejected because they had too many steps
*        (i.e. more that "dcflag" steps).
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

/* Prototypes for private functions. */
static void smf1_step_linefit( int box, float minfrac, int stride, double *dat,
                               unsigned char *qua, double nsigma, double *m,
                               double *c, double *rms, int *status );

static double smf1_running_median( dim_t box, double **dat,
                                  unsigned char **qua, size_t tstride,
                                  unsigned char mask, double *w1, double *w2,
                                  dim_t *iold, dim_t *inbox, int *status );



void smf_fix_steps( smfWorkForce *wf, smfData *data, unsigned char *quality,
                    double dcthresh, double dcthresh2, dim_t dcbox,
                    int dcflag, size_t *nsteps, int *status ) {

/* Local Variables */
   dim_t ibolo;                /* Index of bolometer */
   dim_t inboxa;               /* No. of values in current lower median box */
   dim_t inboxb;               /* No. of values in current upper median box */
   dim_t iolda;                /* Index of oldest value in "w2a" */
   dim_t ioldb;                /* Index of oldest value in "w2b" */
   dim_t nbolo;                /* Number of bolometers */
   dim_t ntslice;              /* Number of time slices */
   double *dat = NULL;         /* Pointer to bolo data */
   double *pd;
   double *pmdata;
   double *pmdatb;
   double *pw2;
   double *pw;
   double *w1a;
   double *w1b;
   double *w2a;
   double *w2b;
   double *work2;
   double *work;
   double avecorr;
   double chi;
   double clo;
   double corr;
   double diff;
   double end_value;
   double meda;
   double medb;
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
   int itime;                  /* Index of time slice */
   int itime_hi;
   int itime_lo;
   int jtime;                  /* Index of time slice */
   int limit;
   int nblock;
   int ncorr;
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
   unsigned char *pmquaa;
   unsigned char *pmquab;
   unsigned char *pq1;
   unsigned char *pq;
   unsigned char *qua = NULL;  /* Pointer to quality flags */


/* Assign values to various configuration parameters that have not yet
   been made public. */
   float dcminpop = 0.05;
   int dcmediangap = 3;
   int dcmedianwidth = 40;
   int dcminsignratio = 0.8;
   int dcminstepwidth = 20;
   int dcmaxstepwidth = 80;
   int dcminbox = 100;
   int dcminstepgap = 50;

/* Initialise returned values. */
   *nsteps = 0;

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
   if( dcmediangap + dcbox*2 > ntslice && *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti( "NTSLICE", ntslice );
      msgSeti( "dcbox", dcbox );
      errRep( " ", "smf_fix_steps: Can't find jumps: ntslice=^NTSLICE, "
              "must be > dcbox (=^dcbox)*2", status );
   }

/* Check for valid threshold */
   if( dcthresh <= 0  && *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti( "DCTHRESH", dcthresh );
      errRep( " ", "smf_fix_steps: Can't find jumps: dcthresh "
              "(^dcthresh) must be > 0", status );
   }

   if( dcthresh2 <= 0  && *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti( "DCTHRESH2", dcthresh2 );
      errRep( " ", "smf_fix_steps: Can't find jumps: dcthresh2 "
              "(^dcthresh2) must be > 0", status );
   }

/* Find, and optionally repair, DC steps. */
   if( dcbox && (*status == SAI__OK) ) {

/* Identify the first and last samples before/after padding+apodization */
      smf_get_goodrange( qua, ntslice, 1, SMF__Q_PAD|SMF__Q_APOD,
                         &itime_start, &itime_end, status );

/* Store the number of time slices in the good range. */
      ntime = itime_end - itime_start + 1;

/* Allocate work arrays. */
      w1a = astMalloc( sizeof( *w1a )*dcmedianwidth );
      w2a = astMalloc( sizeof( *w2a )*dcmedianwidth );
      w1b = astMalloc( sizeof( *w1b )*dcmedianwidth );
      w2b = astMalloc( sizeof( *w2b )*dcmedianwidth );
      work = astMalloc( sizeof( *work )*ntime );
      work2 = astMalloc( sizeof( *work2 )*ntime );
      blocks = astMalloc( sizeof( *blocks )*200 );

/* Get the index of the time slice mid way between the two median boxes
   at the first and last usable time slices. */
      itime_lo = dcmedianwidth + ( dcmediangap - 1 )/2;
      itime_hi = ntime - dcmedianwidth - dcmediangap/2 - 1;

/* Initialise the start and end of the work array that holds the
   correction for each time slice. */
      for( itime = 0; itime <= itime_lo; itime++ ) work[ itime ] = 0.0;
      for( itime = itime_hi; itime < ntime; itime++ ) work[ itime ] = 0.0;

/* Loop round all usable bolometers. "base" holds the offset to the start
   of the usable data for the bolometer.  */
      for( ibolo = 0; ibolo < nbolo && *status==SAI__OK; ibolo++ ) {
         base = ibolo*bstride + itime_start*tstride;
         if( !(qua[ base ] & SMF__Q_BADB) ) {

/* Indicate nothing found yet. */
            nstep = 0;
            nblock = 0;

/* Initialise pointers to the oldest (i.e.lowest index) value in the two
   median boxes. */
            pmdata = dat + base;
            pmquaa = qua + base;
            pmdatb = pmdata + (dcmedianwidth + dcmediangap)*tstride;
            pmquab = pmquaa + (dcmedianwidth + dcmediangap)*tstride;

/* Initialise the "iold" values to indicate that smf1_running_median
   should initialise the description of the median box before calculating
   the median. */
            iolda = ioldb = INIT;

/* Store the differences between the two median box values in the "work2"
   array. */
            pw2 = work2 + itime_lo;
            for( itime = itime_lo; itime < itime_hi; itime++,pw2++ ) {

/* Get the median values of the data in the two boxes. */
               meda = smf1_running_median( dcmedianwidth, &pmdata, &pmquaa,
                                          tstride, SMF__Q_MOD, w1a, w2a,
                                          &iolda, &inboxa, status );

               medb = smf1_running_median( dcmedianwidth, &pmdatb, &pmquab,
                                          tstride, SMF__Q_MOD, w1b, w2b,
                                          &ioldb, &inboxb, status );

/* Check they are both defined, and store the difference. */
               if( meda != VAL__BADD && medb != VAL__BADD ) {
                  diff = medb - meda;
                  *pw2 = diff;
                  tsum2 += diff*diff;
                  tpop++;
               } else {
                  *pw2 = VAL__BADD;
               }
            }

/* Get the RMS difference value. If not defined, flag the entire
   bolometer as bad. */
            if( tpop > 10 ) {
               rms = sqrtf( tsum2/tpop );
            } else {
               msgOutiff( MSG__DEBUG, "", "smf_fix_steps: flagging "
                          "entire bad bolo %" DIM_T_FMT ", due to "
                          "insufficient samples", status, ibolo );
               pq1= qua + ibolo*bstride;
               for( itime = 0; itime < (int) ntslice; itime++) {
                 *pq1 |= SMF__Q_BADB;
                  pq1 += tstride;
               }
               break;
            }

/* Indicate we are currently looking for the start of a step. */
            step_start = -1;

/* Initialise the sums used for finding the average correction. */
            ncorr = 0;
            sumcorr = 0.0;

/* Find the minimum significant step height. */
            thresh = rms*dcthresh;

/* Scan through the range of time slices for which both median box values
   can be found. The "itime" variable gives the index of the central
   sample in the gap between the two median boxes (the lower of the two
   central samples if dcmediangap is even). */
            for( itime = itime_lo; itime < itime_hi; itime++ ) {

/* Initialise the correction for this time slice to be the same as for
   the previous time slice. */
               work[ itime ] = work[ itime - 1 ];
               if( work[ itime ] != VAL__BADD ) {
                  ncorr++;
                  sumcorr += work[ itime ];
               }

/* Get the difference at this time */
               diff = work2[ itime ];
               if( diff != VAL__BADD ) {

/* If the difference between the two median values is greater than the
   minimum significant step height... */
                  if( diff > thresh ) {
                     diff = 1.0;
                  } else if( diff < -thresh ) {
                     diff = -1.0;
                  } else {
                     diff = 0.0;
                  }

                  if( diff != 0.0 ) {

/* And we are currently looking for the start of a new step, record the
   index of the step start. */
                     if( step_start == -1 ) {
                        step_start = itime;
                        nsign = 0;
                     }

/* Update the index of the step end, and also update the earliest time
   at which the step can be considered complete. */
                     step_end = itime;
                     step_limit = itime + dcminstepgap;

/* Find the difference between the number of positive and negative
   excursions. A clear step should be predominantly either positive or
   negative, although in practice a few some contrary values can exist
   in a genuine step. */
                     if( diff > 0.0 ) {
                        nsign++;
                     } else {
                        nsign--;
                     }

/* If the difference between the two median values is smaller than the
   minimum significant step height, and we are currently looking for the
   end of a step, and the we have reached the earliest possible time at
   which the step could end, we now know where the step ends. */
                  } else if( step_start != -1 && itime >= step_limit ) {

/* If the step rise is too short or too long, we ignore the step. Also,
   if the sign of the step is insufficiently clear, we ignore it. */
                     step_width = step_end - step_start + 1;
                     if( step_width >= dcminstepwidth &&
                         step_width <= dcmaxstepwidth &&
                         abs( nsign ) >= dcminsignratio*step_width ) {

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

/* Indicate we are now looking for the start of a new step. */
                     step_start = -1;
                  }
               }
            }

/* Now loop round all blocks found above. Each block specifies the start
   and end time for a single step rise or fall. */
            block = blocks;
            for( iblock = 0; iblock < nblock; iblock++) {
               step_start = block[ 0 ];
               step_end = block[ 1 ];

/* We now fit a straight line to the data just before the step. We use a
   box of length "dcbox", ending at the start of the step, but we shorten
   the box if necessary so that its start is no sooner than the end of
   the previous block. */
               limit = ( iblock > 0 ) ? step_start - block[ -1 ] : (int) dcbox;
               if( limit < dcminbox ) limit = dcminbox;
               if( limit > step_start ) limit = step_start;
               boxlen = ( (int) dcbox < limit ) ? (int) dcbox : limit;
               boxstart = step_start - boxlen;

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
               } else {
                  start_value = VAL__BADD;
               }

/* In the same way fit a line to the data just after the step, and get
   the data value at the centre of the step as implied by the fitted line. */
               limit = ( iblock < nblock - 1 ) ? block[ 2 ] - step_end : (int) dcbox;
               if( limit < dcminbox ) limit = dcminbox;
               if( limit > ntime - 1 - step_end ) limit = ntime - 1 - step_end;
               boxlen = ( (int) dcbox < limit ) ? (int) dcbox : limit;
               boxstart = step_end + 1;

               if( boxlen > dcminbox ) {
                  smf1_step_linefit( boxlen, dcminpop, tstride,
                                     dat + base + boxstart*tstride,
                                     qua + base + boxstart*tstride,
                                     dcthresh2, &mhi, &chi, &rmshi, status );
               } else {
                  mhi = VAL__BADD;
               }

               if( mhi != VAL__BADD ) {
                  end_value = mhi*(-step_width/2) + chi;
               } else {
                  end_value = VAL__BADD;
               }

/* Check the above two lines are defined. */
               if( end_value != VAL__BADD && start_value != VAL__BADD ) {

/* Check the step is large enough. The sigma is taken as the rms about
   the fitted line, which will include general undulations as well as
   the uncorrelated noise. */
                  diff = end_value - start_value;
                  if( fabs( diff ) >= thresh ) {

/* Increment the number of steps found in this bolometer. If we have found
   too many steps, just set the entire bolo bad and break out of the time
   loop to do the next bolo. */
                     if( ++nstep > dcflag ) {
                        (*nsteps)++;
                        pq1 = qua + ibolo*bstride;
                        for( jtime = 0; jtime < (int) ntslice; jtime++) {
                          *pq1 |= SMF__Q_BADB;
                           pq1 += tstride;
                        }
                        msgOutiff( MSG__DEBUG, " ", "smf_fix_steps: "
                                   "flagging bad bolo %" DIM_T_FMT,
                                   status, ibolo );
                        break;
                     }

/* Note the original correction at the start of the step. */
                     corr = work[ step_start - 1 ];

/* Store bad corrections over the duration of the step itself. */
                     pw = work + step_start;
                     for( jtime = step_start; jtime <= step_end; jtime++,pw++ ) {
                        if( *pw != VAL__BADD ) {
                           sumcorr -= *pw;
                           ncorr--;
                           *pw = VAL__BADD;
                        }
                     }

/* Modify the correction for samples following the step. */
                     corr -= diff;
                     pw = work + step_end + 1;
                     for( jtime = step_end + 1; jtime <= itime; jtime++,pw++ ) {
                        if( *pw != VAL__BADD ) {
                           sumcorr -= *pw;
                           ncorr--;
                        }
                        sumcorr += corr;
                        ncorr++;
                        *pw = corr;
                     }
                  }
               }
               block += 2;
            }

/* Modify the corrections to have an average value of zero. */
            if( ncorr ) {
               avecorr = sumcorr/ncorr;
               pw = work + itime_lo;
               for( itime = itime_lo; itime <= itime_hi; itime++,pw++ ) {
                  if( *pw != VAL__BADD ) *pw -= avecorr;
               }

/* Add the correction onto the original data and flag each jump. */
               pd = dat + base + itime_lo*tstride;
               pq = qua + base + itime_lo*tstride;
               pw = work + itime_lo;
               corr = 0;
               for( itime = 0; itime < ntime; itime++,pd++,pq++,pw++ ) {
                  if( *pw != VAL__BADD ) {
                      corr = *pw;
                  } else {
                     *pq |= SMF__Q_JUMP;
                  }
                  *pd += corr;
               }
            }
         }
      }

/* Free workspace */
      w1a = astFree( w1a );
      w2a = astFree( w2a );
      w1b = astFree( w1b );
      w2b = astFree( w2b );
      work = astFree( work );
      work2 = astFree( work2 );
      blocks = astFree( blocks );
   }


   if( *nsteps > 0 ) {
      msgOutiff( MSG__VERB, " ", "smf_fix_steps: flagged %d bad bolos.",
                 status, (int) *nsteps );
   }

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





static double smf1_running_median( dim_t box, double **dat,
                                  unsigned char **qua, size_t tstride,
                                  unsigned char mask, double *w1, double *w2,
                                  dim_t *iold, dim_t *inbox, int *status ){

/* Local Variables: */
   dim_t ibox;                 /* Index within box */
   double *pdat = NULL;        /* Pointer to next bolo data value */
   double *pw1 = NULL;         /* Pointer to next "w1" value */
   double *pw2 = NULL;         /* Pointer to next "w2" value */
   double dnew;                /* Data value being added into the filter box */
   double dold;                /* Data value being removed from the filter box */
   double median;              /* Median value in current filter box */
   int iadd;                   /* Index within box at which to store new value */
   int iremove;                /* Index within box of element to be removed */
   unsigned char *pqua = NULL; /* Pointer to next quality flag */

/* Initialise */
   median = VAL__BADD;

/* Check inherited status */
   if( *status != SAI__OK ) return median;

/* Save the original pointers. */
   pdat = *dat;
   pqua = *qua;

/* If the supplied "iold" value is INIT, first initialise the filter
   box to contain the first "box" values from the current bolometer
   time-series. Do not store bad or flagged values in the filter box. The
   good values are stored at the start of the "w1" array, with no gaps.
   The "w2" array holds all values in the box, good or bad, in the order
   they occur in the time-series (i.e. un-sorted). */
   if( *iold == INIT ) {
      pw1 = w1;
      pw2 = w2;
      for( ibox = 0; ibox < box; ibox++ ) {

         if( !( *pqua & mask ) && *pdat != VAL__BADD ) {
            *(pw2++) = *(pw1++) = *pdat;
         } else {
            *(pw2++) = VAL__BADD;
         }

/* Get pointers to the next data and quality values for the current
   bolometer. */
         pdat += tstride;
         pqua += tstride;
      }

/* Initialise the index at which to store the next bolometer data value in the
   "w2" array. The first new value added to the box will over-write element
   zero - the oldest value in the box. */
      *iold = 0;

/* Note the number of good values stored in the filter box. */
      *inbox = (int)( pw1 - w1 );

/* If there are any bad data values, pad out the w1 array with bad
   values. */
       for( ibox = *inbox; ibox < box; ibox++ ) w1[ ibox ] = VAL__BADD;

/* If any good values are stored in the filter box, we now sort them. */
       if( *inbox > 0 ) gsl_sort( w1, 1, *inbox );

/* If the supplied "iold" value is not negative, advance the box by one
   sample and update the supplied values accordingly. */
   } else {

/* Get the data value for the time slice that is about to enter the filter
   box. Set it bad if it is flagged in the quality array. */
      dnew = pdat[ box ];
      if( pqua[ box ] & mask ) dnew = VAL__BADD;

/* Get the data value for the time slice that is about to leave the filter
   box. */
      dold = w2[ *iold ];

/* Store the new value in "w2" in place of the old value, and then
   increment the index of the next "w2" value to be removed, wrapping back
   to the start when the end of the array is reached. */
      w2[ (*iold)++ ] = dnew;
      if( *iold == box ) *iold = 0;

/* If the new value to be added into the box is good... */
      if( dnew != VAL__BADD ) {

/* Find the index (iadd) within the w1 box at which to store the new
   value so as to maintain the ordering of the values in the box. Could do
   a binary chop here, but the box size is presumably going to be very
   small and so it's probably not worth it. At the same time, look for
   the value that is leaving the box (if it is not bad). */
         iremove = -1;
         iadd = -1;

         if( dold != VAL__BADD ) {
            for( ibox = 0; ibox < *inbox; ibox++ ) {
               if( iremove == -1 && w1[ ibox ] == dold ) {
                  iremove = ibox;
                  if( iadd != -1 ) break;
               }
               if( iadd == -1 && w1[ ibox ] >= dnew ) {
                  iadd = ibox;
                  if( iremove != -1 ) break;
               }
            }

         } else {
            for( iadd = 0; iadd < (int) *inbox; iadd++ ) {
               if( w1[ iadd ] >= dnew ) break;
            }
         }

/* If the new value is larger than any value currently in w1, we add it
   to the end. */
         if( iadd == -1 ) iadd = *inbox;

/* If the value being removed is bad, shuffle all the good values greater
   than the new value up one element, and increment the number of good
   values for this bolometer box. */
         if( iremove == -1 ) {
            for( ibox = *inbox; (int) ibox > iadd; ibox-- ) {
               w1[ ibox ] = w1[ ibox - 1 ];
            }
            (*inbox)++;

/* If the value being removed is good and the value being added is greater
   than the value being removed, shuffle all the intermediate values down
   one element within the box. */
         } else if( (int) iadd > iremove ) {
            for( ibox = iremove; (int) ibox < iadd - 1; ibox++ ) {
               w1[ ibox ] = w1[ ibox + 1 ];
            }
            iadd--;

/* If the value being removed is good and the value being added is less
   than or equal to the value being removed, shuffle all the intermediate
   values up one element within the box. */
         } else {
            for( ibox = iremove; (int) ibox > iadd; ibox-- ) {
               w1[ ibox ] = w1[ ibox - 1 ];
            }
         }

/* Store the new value in the box. */
         w1[ iadd ] = dnew;

/* If the value being added is bad but the value being removed is good... */
      } else if( dold != VAL__BADD ){

/* Find the index (iremove) within the w1 box at which is stored the value
   that is leaving the box. */
         iremove = -1;
         for( iremove = 0; iremove < (int) *inbox; iremove++ ) {
            if( w1[ iremove ] == dold ) break;
         }

/* Move all the larger values down one element in "w1" to fill the gap
   left by the removal. */
         for( iremove++; iremove < (int) *inbox; iremove++ ) {
            w1[ iremove - 1 ] = w1[ iremove ];
         }

/* Over-write the un-used last element with a bad value, and decrement
   the number of values in "w1". */
         w1[ iremove - 1 ] = VAL__BADD;
         (*inbox)--;
      }

/* Update the supplied pointers. */
      *dat += tstride;
      *qua += tstride;
   }

/* If the current sorted filter box contains an odd number of good values,
   use the central good value as the median value. If the box contains an
   even number of good values, use the mean of the two central values as
   the median value. If the box is empty use VAL__BADD. */
   if( *inbox == 0 ) {
      median = VAL__BADD;

   } else if( *inbox % 2 == 1 ) {
      median = w1[ *inbox/2 ];

   } else {
      ibox = *inbox/2;
      median = 0.5*( w1[ ibox ] + w1[ ibox - 1 ] );
   }

/* Return the median value. */
   return median;
}



