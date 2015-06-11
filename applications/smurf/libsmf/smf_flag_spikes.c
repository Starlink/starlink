/*
*+
*  Name:
*     smf_flag_spikes

*  Purpose:
*     Flag n-sigma excursions (spikes) from the rolling median of a data stream

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_flag_spikes( ThrWorkForce *wf, smfData *data, smf_qual_t mask,
*                       double thresh, size_t box, size_t *nflagged,
*                       int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     data = smfData * (Given and Returned)
*        The data that will be flagged. Locations of spikes
*        will have bit SMF__Q_SPIKE set on exit.
*     mask = smf_qual_t (Given)
*        Define which bits in quality are relevant to ignore data in
*        the calculation.
*     thresh = double (Given)
*        Number of standard deviations at which to clip spikes.
*     box = size_t (Given)
*        The number of time slices to include in the median filter box.
*     nflagged = size_t * (Returned)
*        The number of new samples that were flagged. May be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Each bolometer is processed independently. At each time slice, the
*     median value of the current bolometer in a 1D box centred on the time
*     slice, and containing "box" time slices, is found. If the residual
*     between the time slice value and the median value is greater than
*     "thresh" times the local noise level, the time slice is flagged as
*     a spike within the quality mask.
*
*     The local noise level is the standard deviation of the time slice
*     values in a 1D box containing "box" time slices, and which is
*     adjacent to the filter box used for determing the median value. That
*     is, the high end of the noise box is just below the low end of the
*     median filter box. This introduces a slight asymentry in the noise,
*     but this should not matter unless the noise varies significantly on
*     a time scale shorter than the box size. The advantage of using an
*     offset noise box is that time slices found to contain a spike can be
*     omitted from the noise box, thus preventing the spike from upsetting
*     the local noise estimate.
*
*     Some complication is introduced into the coding is an attempt to
*     speed up the median filtering.

*  Notes:
*     - No spikes are ever flagged in the first and last "box/2" time-slices.

*  Authors:
*     David Berry (JAC, Hawaii)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-01-10 (DSB):
*        Initial Version
*     2010-09-14 (EC):
*        Parallelize over blocks of bolometers
*     2015-06-11 (DSB):
*        Five years later, we discover that time-based de-spiking has
*        been completely broken the whole time because of incorrect
*        indexing within the parallel code.

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
*     Copyright (C) 2010 Univeristy of British Columbia.
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

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Other includes */
#include <gsl/gsl_sort.h>

/* ------------------------------------------------------------------------ */
/* Local variables and functions */

/* Structure containing information about blocks of bolos that each
   thread will process */
typedef struct {
  size_t b1;               /* Index of first bolometer of block */
  size_t b2;               /* Index of last bolometer of block */
  size_t box;              /* box size */
  size_t bstride;          /* bolometer stride for res/qua */
  double *dat;             /* pointer to the bolometer data */
  int ijob;                /* Job identifier */
  smf_qual_t mask;         /* quality bit mask */
  dim_t nbolo;             /* number of bolometers */
  size_t nflag;            /* number of spikes flagged in this block */
  dim_t ntime;             /* number of time slices */
  smf_qual_t *qua;         /* pointer to the quality array */
  double thresh;           /* threshold for spikes */
  size_t tstride;          /* time stride for res/qua */
} smfFlagSpikesData;

/* Function to be executed in thread: find spikes in block of bolos */

static void smfFlagSpikesPar( void *job_data_ptr, int *status );

static void smfFlagSpikesPar( void *job_data_ptr, int *status ) {
   size_t box;                 /* Box size */
   size_t bstride;             /* Vector stride between bolometer samples */
   double *dat = NULL;         /* Pointer to bolo data */
   double dnew;                /* Data value being added into the filter box */
   double dold;                /* Data value being removed from the filter box*/
   int iadd;                   /* Index within box at which to store new value*/
   dim_t ibolo;                /* Bolometer index */
   dim_t ibox;                 /* Index within box */
   dim_t iold;                 /* Index of oldest value in "w2" */
   dim_t inbox;                /* Number of values in current filter box */
   int inoise;                 /* Index within noisebox element to be removed */
   int iremove;                /* Index within box of element to be removed */
   dim_t itime;                /* Time-slice index */
   dim_t lasttime;             /* Last time-slice index to check for spikes */
   double lmedian;             /* Median value in previous filter box */
   smf_qual_t mask;            /* quality bit mask */
   double median;              /* Median value in current filter box */
   dim_t nbolo;                /* Number of bolometers */
   size_t newstride;           /* Vector stride to new time sample */
   size_t nflag;               /* Number of samples flagged */
   int nn;                     /* Number of good values in noise box */
   double noise;               /* Local noise estimate */
   double *noisebox = NULL;    /* Pointer to array holding values for
                                  calculating local noise */
   double nsum;                /* Sum of values in noise box */
   double nsum2;               /* Sum of squared values in noise box */
   dim_t ntime;                /* Number of time-slices */
   double *pdat = NULL;        /* Pointer to next bolo data value */
   double *pdat0 = NULL;       /* Pointer to first bolo data value */
   double *pdat1 = NULL;       /* Pointer to last bolo data value */
   smfFlagSpikesData *pdata=NULL;
   double *pn = NULL;          /* Pointer to next "noisebox" value */
   smf_qual_t *pqua = NULL;    /* Pointer to next quality flag */
   smf_qual_t *pqua0 = NULL;   /* Pointer to first bolo quality value */
   smf_qual_t *pqua1 = NULL;   /* Pointer to last bolo quality value */
   double *pw1 = NULL;         /* Pointer to next "w1" value */
   double *pw2 = NULL;         /* Pointer to next "w2" value */
   smf_qual_t *qua = NULL;     /* Pointer to quality flags */
   int spike;                  /* Is current time slice a spike? */
   double thresh;              /* threshold for spikes */
   size_t tstride;             /* Vector stride between time samples */
   double umedian;             /* Lagged median value */
   double *w1 = NULL;          /* Array holding sorted data values */
   double *w2 = NULL;          /* Array holding un-sorted data values */

/* Retrieve job data */
   pdata = job_data_ptr;
   box = pdata->box;
   bstride = pdata->bstride;
   dat = pdata->dat;
   mask = pdata->mask;
   nbolo = pdata->nbolo;
   ntime = pdata->ntime;
   qua = pdata->qua;
   thresh = pdata->thresh;
   tstride = pdata->tstride;

/* Debugging message indicating thread started work */
   msgOutiff(  SMF__TIMER_MSG, "",
              "smfFlagSpikesPar: thread starting on bolos %zu -- %zu",
              status, pdata->b1, pdata->b2 );

/* Initialise the number of spikes found. */
   nflag = 0;

/* Store the largest time slice index to be checked. */
   lasttime =  ntime - ( box + 1 )/2;

/* Store the offset from the central value in a filter box to the next
   time slice value to enter the filter box (i.e. the time slice just
   in front of the current filter box). */
   newstride =  ( ( box + 1 )/2 )*tstride;

/* Allocate work arrays. */
   w1 = astMalloc( sizeof( *w1 )*box );
   w2 = astMalloc( sizeof( *w2 )*box );
   noisebox = astMalloc( sizeof( *noisebox )*box );

/* Initialise pointers to the first data value and quality value for the
   first bolometer. */
   pdat0 = dat + pdata->b1*bstride;
   pqua0 = qua + pdata->b1*bstride;

/* Initialise pointers to the last data value and quality value for the first
   bolometer. */
   pdat1 = dat + ( ntime - 1 )*tstride + pdata->b1*bstride;
   pqua1 = qua + ( ntime - 1 )*tstride + pdata->b1*bstride;

/* We process each bolometer in turn for this block. */
   for( ibolo=pdata->b1; ibolo<=pdata->b2 && *status == SAI__OK; ibolo++ ) {

/* Ignore bad bolometers. */
      if( !( *pqua0 & SMF__Q_BADB ) ) {

/* Initialise the running sums used to calculate the standard deviation
   of the values in the initial noise box. */
         nsum = 0.0;
         nsum2 = 0.0;
         nn = 0;

/* The highest element in the noise box is the element just below the
   central element of the filter box (so the central filter box element
   does not affect the noise level estimate). Initially, the lower half
   of the noise box is off the edge of the array and so we fill it with bad
   values. */
         pn = noisebox;
         for( ibox = 0; ibox < ( box + 1 )/2; ibox++ ) *(pn++) = VAL__BADD;

/* Initialise the filter box to contain the first "box" values from the
   current bolometer time-series. Do not store bad or flagged values in
   the filter box. The good values are stored at the start of the "w1"
   array, with no gaps. The "w2" array holds all values in the box, good
   or bad, in the order they occur in the time-series (i.e. un-sorted). */
         pdat = pdat0;
         pqua = pqua0;
         pw1 = w1;
         pw2 = w2;
         for( ibox = 0; ibox < box; ibox++ ) {

            if( !( *pqua & mask ) && *pdat != VAL__BADD ) {
               *(pw2++) = *(pw1++) = *pdat;
            } else {
               *(pw2++) = VAL__BADD;
            }

/* Also store values these data values in the second half of the noise
   box. Check we have not reached the end of the noise box. */
            if( pn - noisebox < (int) box ) {

/* Store the value in the next element of the noise box, and if the value is
   good, update the running sums used for calculating the noise level. */
               if( ( *(pn++) = pw2[-1] ) != VAL__BADD ) {
                  nsum += *pdat;
                  nsum2 += (*pdat)*(*pdat);
                  nn++;
               }
            }

/* Get pointers to the next data and quality values for the current
   bolometer. */
            pdat += tstride;
            pqua += tstride;
         }

/* Calculate the standard deviation of the values in the initial noise box.
   We require at least three values. */
         if( nn > 3 ) {
            noise = ( nsum2 - (nsum*nsum)/nn )/( nn - 1 );
            if( noise > 0 ) {
               noise = sqrt( noise );
            } else {
               noise = VAL__BADD;
            }
         } else {
            noise = VAL__BADD;
         }

/* Store the index within the noise box at which to store the next value
   (the new value will over-write the oldest value in the noise box -
   i.e. the first element). */
         inoise = 0;

/* Initialise the index at which to store the next bolometer data value in the
   "w2" array. The first new value added to the box will over-write element
   zero - the oldest value in the box. */
         iold = 0;

/* Note the number of good values stored in the filter box. */
         inbox = (int)( pw1 - w1 );

/* If there are any bad data values, pad out the w1 array with bad
   values. */
         for( ibox = inbox; ibox < box; ibox++ ) w1[ ibox ] = VAL__BADD;

/* If any good values are stored in the filter box, we now sort them. */
         if( inbox > 0 ) gsl_sort( w1, 1, inbox );

/* Get pointers to the data value and quality value at the centre of the
   first filter box. */
         pdat = pdat0 + (box/2)*tstride;
         pqua = pqua0 + (box/2)*tstride;

/* Initialise the median value in the previous box  to "unknown". */
         lmedian = VAL__BADD;

/* Loop round all time slices, except for the first and last "box/2"
   slices, which are left unchanged. "itime" is the index of the time
   slice at the centre of the filter box. */
         for( itime = box/2; itime <= lasttime; itime++ ) {

/*  Assume this time slice is not a spike. */
            spike = 0;

/* If the current sorted filter box contains an odd number of good values,
   use the central good value as the median value. If the box contains an
   even number of good values, use the mean of the two central values as
   the median value. If the box is empty use VAL__BADD. */
            if( inbox == 0 ) {
               median = VAL__BADD;

            } else if( inbox % 2 == 1 ) {
               median = w1[ inbox/2 ];

            } else {
               ibox = inbox/2;
               median = 0.5*( w1[ ibox ] + w1[ ibox - 1 ] );
            }

/* If we have all the values we need, we can check for a spike. */
            if( !( *pqua & mask ) && *pdat != VAL__BADD &&
                 median != VAL__BADD && noise != VAL__BADD ) {

/* The median is a robust but intrinsically noisey estimator of the expected
   value. So we smooth the median values a bit by using the mean of the
   current median and the previous median. */
               if( lmedian != VAL__BADD ) {
                  umedian = 0.5*( median + lmedian );
               } else {
                  umedian = median;
               }

/* If the central value in the filter box deviates from the median by more than
   "thresh" times the current noise estimate, flag it as a spike. Note, we can
   modify the quality array directly because we will not be re-reading the value
   being modified (all the reading is done at the leading edge of the filter
   box). */
               if( fabs( *pdat - umedian ) > thresh*noise ) {
                 *pqua |= SMF__Q_SPIKE;
                  nflag++;
                  spike = 1;
               }
            }

/* Save the median in the current box so that we can use it to smooth the
   next box. */
            lmedian = median;

/* Now move the filter box on by one time sample so that we are ready for
   the next "itime" value. Do not do this if we have just done the last
   itime value. */
            if( itime < lasttime ) {

/* Get the data value for the time slice that is about to enter the filter
   box. Set it bad if it is flagged in the quality array. */
               dnew = pdat[ newstride ];
               if( pqua[ newstride ] & mask ) dnew = VAL__BADD;

/* Get the data value for the time slice that is about to leave the filter
   box. */
               dold = w2[ iold ];

/* Store the new value in "w2" in place of the old value, and then
   increment the index of the next "w2" value to be removed, wrapping back
   to the start when the end of the array is reached. */
               w2[ iold++ ] = dnew;
               if( iold == box ) iold = 0;

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
                     for( ibox = 0; ibox < inbox; ibox++ ) {
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
                     for( iadd = 0; iadd < (int) inbox; iadd++ ) {
                        if( w1[ iadd ] >= dnew ) break;
                     }
                  }

/* If the new value is larger than any value currently in w1, we add it
   to the end. */
                  if( iadd == -1 ) iadd = inbox;

/* If the value being removed is bad, shuffle all the good values greater
   than the new value up one element, and increment the number of good
   values for this bolometer box. */
                  if( iremove == -1 ) {
                     for( ibox = inbox; (int) ibox > iadd; ibox-- ) {
                        w1[ ibox ] = w1[ ibox - 1 ];
                     }
                     inbox++;

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
                  for( iremove = 0; iremove < (int) inbox; iremove++ ) {
                     if( w1[ iremove ] == dold ) break;
                  }

/* Move all the larger values down one element in "w1" to fill the gap
   left by the removal. */
                  for( iremove++; iremove < (int) inbox; iremove++ ) {
                     w1[ iremove - 1 ] = w1[ iremove ];
                  }

/* Over-write the un-used last element with a bad value, and decrement
   the number of values in "w1". */
                  w1[ iremove - 1 ] = VAL__BADD;
                  inbox--;
               }

/* Get the new value (the central value in the filter box) to add to the
   noise box. If the central value was found to be a spike, do not
   include it in the noise box as it would upset the estimate of the
   local noise. */
               if( spike ) {
                  dnew = VAL__BADD;
               } else {
                  dnew = *pdat;
                  if( *pqua & mask ) dnew = VAL__BADD;
               }

/* Get the old value to remove from the noise box. */
               dold = noisebox[ inoise ];

/* If the value being added is good, add it into the running sums used to
   calculate the standard deviation of the values in the local noise box. */
               if( dnew != VAL__BADD ) {
                  nsum += dnew;
                  nsum2 += dnew*dnew;
                  nn++;
               }

/* If the value being removed is good, remove it from the running sums used
   to calculate the standard deviation of the values in the local noise box. */
               if( dold != VAL__BADD ) {
                  nsum -= dold;
                  nsum2 -= dold*dold;
                  nn--;
               }

/* Calculate the standard deviation of the values in the new noise box.
   We require at least three values. */
               if( nn > 3 ) {
                  noise = ( nsum2 - (nsum*nsum)/nn )/( nn - 1 );
                  if( noise > 0 ) {
                     noise = sqrt( noise );
                  } else {
                     noise = VAL__BADD;
                  }
               } else {
                  noise = VAL__BADD;
               }

/* Store the new value in the noise box, over-writing the oldest value,
   and increment the index at which to store the next value. If the index
   reaches the end of the array, wrap around to the start of the array. */
               noisebox[ inoise++ ] = dnew;
               if( inoise == (int) box ) inoise = 0;
            }

/* Get pointers to the next central data and quality value for the current
   bolometer. */
            pdat += tstride;
            pqua += tstride;
         }
      }

/* Get pointers to the first data value and quality value for the next
   bolometer. */
      pdat0 += bstride;
      pqua0 += bstride;

/* Get pointers to the last data value and quality value for the next
   bolometer. */
      pdat1 += bstride;
      pqua1 += bstride;
   }

/* Free resources. */
   w1 = astFree( w1 );
   w2 = astFree( w2 );
   noisebox = astFree( noisebox );

/* Store number of flagged samples */
   pdata->nflag = nflag;

/* Debugging message indicating thread finished work */
   msgOutiff( SMF__TIMER_MSG, "",
              "smfFlagSpikesPar: thread finishing bolos %zu -- %zu",
              status, pdata->b1, pdata->b2 );
}

/* ------------------------------------------------------------------------ */

#define FUNC_NAME "smf_flag_spikes"

void smf_flag_spikes( ThrWorkForce *wf, smfData *data, smf_qual_t mask,
                      double thresh, size_t box, size_t *nflagged,
                      int *status ){

/* Local Variables */
   int i;                      /* Loop counter */
   smfFlagSpikesData *job_data=NULL;/* Array of job data for each thread */
   dim_t nbolo;                /* Number of bolometers */
   dim_t ntime;                /* Number of time-slices */
   double *dat = NULL;         /* Pointer to bolo data */
   smfFlagSpikesData *pdata=NULL;/* Pointer to job data */
   int nw;                     /* Number of worker threads */
   size_t bstride;             /* Vector stride between bolometer samples */
   size_t nflag;               /* Number of samples flagged */
   size_t tstride;             /* Vector stride between time samples */
   smf_qual_t *qua = NULL;     /* Pointer to quality flags */
   size_t step;                /* step size for dividing up work */
   int njobs=0;                /* Number of jobs to be processed */

/* Check inherited status. Also return immediately if no spike flagging
   is to be performed. */
   if( *status != SAI__OK || thresh == 0.0 ) return;

/* How many threads do we get to play with */
   nw = wf ? wf->nworker : 1;

/* Check we have double precision data. */
   smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status );

/* Get a pointer to the quality array to use. */
   qua = smf_select_qualpntr( data, NULL, status );

/* Report an error if we have no quality array. */
   if( !qua && *status == SAI__OK ) {
     *status = SAI__ERROR;
     errRep( " ", FUNC_NAME ": No valid QUALITY array was provided", status );
   }

/* Get a pointer to the data array to use. Report an error if we have
   no data array. */
   dat = data->pntr[0];
   if( !dat && *status == SAI__OK ) {
     *status = SAI__ERROR;
     errRep( " ", FUNC_NAME ": smfData does not contain a DATA component",
             status);
   }

/* Check the supplied thresh value is valid. */
   if( thresh <= 0 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSetd( "THRESH", thresh );
      errRep( " ", FUNC_NAME ": Can't find spikes: thresh=^THRESH, must be > 0",
              status);
   }

/* Check the supplied box value is valid. */
   if( box <= 2 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti( "BOX", box );
      errRep( " ", FUNC_NAME ": Can't find spikes: box=^BOX, must be > 2",
              status);
   }

/* Obtain data dimensions, and the stride between adjacent elements on
   each axis (bolometer and time). Use the existing data order to avoid
   the cost of re-ordering. */
   smf_get_dims( data,  NULL, NULL, &nbolo, &ntime, NULL, &bstride, &tstride,
                 status );

/* Check we have room for at least 3 boxes along the time axis. */
   if( 3*box > ntime && *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti( "BOX", box );
      msgSeti( "MAX", ntime/3 - 1 );
      errRep( " ", FUNC_NAME ": Can't find spikes: box=^BOX is too large, "
              " must be < ^MAX.", status);
   }

/* Set up the job data */

   if( nw > (int) nbolo ) {
     step = 1;
   } else {
     step = nbolo/nw;
     if( !step ) {
       step = 1;
     }
   }

   job_data = astCalloc( nw, sizeof(*job_data) );

   for( i=0; (*status==SAI__OK)&&i<nw; i++ ) {
     pdata = job_data + i;

     pdata->b1 = i*step;
     pdata->b2 = (i+1)*step-1;

/* if b1 is greater than the number of bolometers, we've run out of jobs... */
     if( pdata->b1 >= nbolo ) {
       break;
     }

/* increase the jobs counter */
     njobs++;

/* Ensure that the last thread picks up any left-over bolometers */
     if( (i==(nw-1)) && (pdata->b1<(nbolo-1)) ) {
       pdata->b2=nbolo-1;
     }

     pdata->ijob = -1;   /* Flag job as ready to start */
     pdata->box = box;
     pdata->bstride = bstride;
     pdata->dat = dat;
     pdata->mask = mask;
     pdata->thresh = thresh;
     pdata->nbolo = nbolo;
     pdata->ntime = ntime;
     pdata->qua = qua;
     pdata->tstride = tstride;
   }

/* Submit jobs to find spikes in each block of bolos */
   thrBeginJobContext( wf, status );
   for( i=0; (*status==SAI__OK)&&i<njobs; i++ ) {
     pdata = job_data + i;
     pdata->ijob = thrAddJob( wf, THR__REPORT_JOB, pdata,
                                smfFlagSpikesPar, 0, NULL, status );
   }

/* Wait until all of the submitted jobs have completed */
   thrWait( wf, status );
   thrEndJobContext( wf, status );

/* Count flagged samples from all of the jobs and free resources */
   nflag=0;
   if( job_data ) {
     for( i=0; i<njobs; i++ ) {
       pdata = job_data + i;
       nflag += pdata->nflag;
     }
     job_data = astFree( job_data );
   }

/* Return the number of flagged samples, if requested */
   if( nflagged ) *nflagged = nflag;

}
