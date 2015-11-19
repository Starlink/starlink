/*
*+
*  Name:
*     smf_fillgaps

*  Purpose:
*     Fill flagged regions of data with constrained realization of noise

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_fillgaps( ThrWorkForce *wf, smfData *data, smf_qual_t mask,
*                   int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData * (Given and Returned)
*        The data that will be flagged
*     mask = smf_qual_t (Given)
*        Define which bits in quality indicate locations of gaps to be filled.
*        Note, if this mask includes SMF__Q_PAD, then the padding at the
*        start and end of each bolometer time series will be replaced by
*        artifical noisey data that connects the first and last non-padding
*        samples smoothly.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Identify continuous blocks of each detector time series that match
*     the given quality mask (e.g. spikes). Replace the flagged block
*     of data with a constrained realization of noise: smoothly connect the
*     before/after boundaries of the gap to avoid ringing when filters are
*     applied.

*  Authors:
*     Edward Chapin (UBC)
*     David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-01-06 (EC):
*        Initial code stub
*     2010-01-07 (DSB):
*        Initial full version.
*     2010-01-09 (DSB):
*        Change to use GSL random number generator.
*     2010-01-15 (DSB):
*        Add multi-threading.
*     2010-01-22 (DSB):
*        Correct single-threaded case.
*     2010-02-10 (DSB):
*        - Change BOX from 10 to 50 to get a more reasonable estimate of
*        the noise.
*        - If a gap is found at the start or end of the time series, use
*        a gradient of zero when filling it, since the gradient estimate
*        within a single box can be very unrepresentative (i.e. unusually
*        high or low).
*     2010-02-18 (DSB):
*        Gaps at start or end were using the offset at time zero, rather
*        than the offset at the boundary of the good data.
*     2010-09-28 (DSB):
*        Replace padding if "mask" includes SMF__Q_PAD.
*     2010-09-30 (DSB):
*        Do not look for gaps within the padding.
*     2010-10-1 (DSB):
*        Correct use of pstart and pend.
*     2010-10-4 (DSB):
*        Fill the end-padding samples with artifical data if "mask"
*        contains "SMF__Q_PAD" - this now rolls the gradient off to zero
*        over 100 samples at start and end before using a cubic
*        interpolation. This prevents unusually high gradients at the
*        start or end of the time stream producing unrealisatically high
*        values in the cubic interpolation.
*     2010-10-19 (DSB):
*        Do not attempt to fill the padded area if the bolometer has no good values.
*     2012-03-05 (DSB):
*        Fix bug which prevented the last sample being filled if it was a
*        single isolated flagged value.
*     2012-06-05 (DSB):
*        Use a separate random number generator for each thread. This gives
*        repeatability, and also helps with convergence since each iteration
*        fills each gap using the same values.
*     2013-05-08 (DSB):
*        If filling using a box size of BOX leaves any bad values
*        unfilled, try again with a smaller box.
*     2013-05-09 (DSB):
*        If the box size becomes too small, fill using a linear fit without
*        noise.
*     2013-05-30 (DSB):
*        Replace padding with bad values in the data array so that fitting
*        boxes that extend into the padding can know to omit it.
*     2013-06-19 (DSB):
*        Major re-write to make it simpler, and to avoid replacing short
*        patches of good inter-gap data as if it was part of the
*        neighbouring gaps. The old version had problems when lots of short
*        gaps were being filled, as they were all merged into one big gap
*        and the filled data was thus very unconstrained (which is not
*        good for FLT masking).
*     2013-12-02 (DSB):
*        Changed so that it can be used on a smfData with no Quality
*        array (e.g. the COM model).

*  Copyright:
*     Copyright (C) 2010 Univeristy of British Columbia.
*     Copyright (C) 2010-2013 Science & Technology Facilities Council.
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
#include "libsmf/smf_err.h"

/* System includes */
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

/* Define the function name for error messages. */
#define FUNC_NAME "smf_fillgaps"

/* Define the width of the patch used to determine the mean level and
   noise adjacent to each flagged block. The current value is pretty well
   arbitrary. */
#define BOX 20

/* Define the minimum box size for which noise can be calculated. */
#define MINBOX 6

/* Structure containing information about blocks of bolos to be
   filled by each thread. */
typedef struct smfFillGapsData {
  int ntslice;                  /* Number of time slices */
  double *dat;                  /* Pointer to bolo data */
  gsl_rng *r;                   /* GSL random number generator */
  int fillpad;                  /* Fill PAD samples? */
  size_t b1;                    /* Index of first bolometer to be filledd */
  size_t b2;                    /* Index of last bolometer to be filledd */
  size_t bstride;               /* bolo stride */
  size_t tstride;               /* time slice stride */
  int pend;                     /* Last non-PAD sample */
  int pstart;                   /* First non-PAD sample */
  smf_qual_t *qua;              /* Pointer to quality array */
  smf_qual_t mask;              /* Quality mask for bad samples */
} smfFillGapsData;


/* Prototype for the function to be executed in each thread. */
static void smfFillGapsParallel( void *job_data_ptr, int *status );
static void smf1_fillgap( double *data, int pstart, int pend, size_t tstride,
                          int jstart, int jend, gsl_rng *r, int *status );



void  smf_fillgaps( ThrWorkForce *wf, smfData *data,
                    smf_qual_t mask, int *status ) {

/* Local Variables */
  const gsl_rng_type *type;     /* GSL random number generator type */
  dim_t bpt;                    /* Number of bolos per thread */
  dim_t i;                      /* Bolometer index */
  dim_t nbolo;                  /* Number of bolos */
  dim_t ntslice;                /* Number of time slices */
  double *dat=NULL;             /* Pointer to bolo data */
  int fillpad;                  /* Fill PAD samples? */
  size_t bstride;               /* bolo stride */
  size_t pend;                  /* Last non-PAD sample */
  size_t pstart;                /* First non-PAD sample */
  size_t tstride;               /* time slice stride */
  smfFillGapsData *job_data;    /* Structures holding data for worker threads */
  smfFillGapsData *pdata;       /* Pointer to data for next worker thread */
  smf_qual_t *qua=NULL;         /* Pointer to quality array */

/* Main routine */
  if (*status != SAI__OK) return;

/* Check we have double precision data floating point data. */
  if (!smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status )) return;

/* Pointers to data and quality */
  dat = data->pntr[0];
  qua = smf_select_qualpntr( data, NULL, status );

  if( !dat ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData does not contain a DATA component",status);
    return;
  }

 /* obtain data dimensions */
  smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, NULL, &bstride, &tstride,
                status );

  /* Determine how many bolometers to process in each thread, and create
     the structures used to pass data to the threads. */
  if( wf ) {
     bpt = nbolo/wf->nworker;
     if( wf->nworker*bpt < nbolo ) bpt++;
     job_data = astMalloc( sizeof( smfFillGapsData )*wf->nworker );
  } else {
     bpt = nbolo;
     job_data = astMalloc( sizeof( smfFillGapsData ) );
  }

  /* Find the indices of the first and last non-PAD sample. */
  if( qua ) {
     smf_get_goodrange( qua, ntslice, tstride, SMF__Q_PAD, &pstart, &pend,
                        status );
  } else {
    pstart = 0;
    pend = ntslice - 1;
  }

  /* Report an error if it is too short. */
  if( pend - pstart <= 2*BOX && *status == SAI__OK ) {
    *status = SAI__ERROR;
    errRepf( "", FUNC_NAME ": length of data (%d samples) is too small "
             "to fill gaps. Must have at least %d samples per bolometer.",
             status, (int) ( pend - pstart ), 2*BOX + 1 );
  }

  /* If the supplied "mask" value includes SMF__Q_PAD, then we will be
  replacing the zero-padded region at the start and end of each time series
  with artificial noisey data that connects the first and last data values
  smoothly. Remove SMF__Q_PAD from the mask. */
  if( mask & SMF__Q_PAD ) {
     mask &= ~SMF__Q_PAD;
     fillpad = 1;
  } else {
     fillpad = 0;
  }

  /* Get the default GSL randim number generator type. A separate random
     number generator is used for each worker thread so that the gap filling
     process does not depend on the the order in which threads are
     executed. */
  type = gsl_rng_default;

  /* Begin a job context. */
  thrBeginJobContext( wf, status );

  /* Loop over bolometer in groups of "bpt". */
  pdata = job_data;
  for( i = 0; i < nbolo; i += bpt, pdata++ ) {

    /* Store information for this group in the  next smfFillGapsData
       structure. */
    pdata->ntslice = ntslice;
    pdata->dat = dat;
    pdata->r = gsl_rng_alloc( type );
    pdata->b1 = i;
    pdata->b2 = i + bpt - 1;
    pdata->pend = pend;
    pdata->fillpad = fillpad;
    pdata->pstart = pstart;
    if( pdata->b2 >= nbolo ) pdata->b2 = nbolo - 1;
    pdata->bstride = bstride;
    pdata->tstride = tstride;
    pdata->qua = qua;
    pdata->mask = mask;

    /* Submit a job to the workforce to process this group of bolometers. */
    (void) thrAddJob( wf, 0, pdata, smfFillGapsParallel, 0, NULL, status );
  }

  /* Wait until all jobs in the current job context have completed, and
     then end the job context. */
  thrWait( wf, status );
  thrEndJobContext( wf, status );

  /* Free resources. */
  if( job_data ) {
    pdata = job_data;
    for( i = 0; i < nbolo; i += bpt, pdata++ ) {
      if( pdata->r ) gsl_rng_free( pdata->r );
    }
    job_data = astFree( job_data );
  }
}





/* Function to be executed in thread: fill gaps in all bolos from b1 to b2 */

static void smfFillGapsParallel( void *job_data_ptr, int *status ) {

/* Local Variables */
  dim_t i;                      /* Bolometer index */
  int j;                        /* Time-slice index */
  int ntslice;                  /* Number of time slices */
  double *dat = NULL;           /* Pointer to bolo data */
  double *pd;
  double a;                     /* Cubic interpolation coefficient */
  double b;                     /* Cubic interpolation coefficient */
  double c;                     /* Cubic interpolation coefficient */
  double cl;                    /* Offset of fit at left end of block */
  double cr;                    /* Offset of fit at right end of block */
  double d;                     /* Cubic interpolation coefficient */
  double dg;                    /* Gradient to remove  per sample */
  double dlen;                  /* Total number of samples being interpolated */
  double e;                     /* Linear interpolation coefficient */
  double f;                     /* Linear interpolation coefficient */
  double meanl;                 /* Mean value in left patch */
  double meanr;                 /* Mean value in right patch */
  double ml;                    /* Gradient of fit at left end of block */
  double mr;                    /* Gradient of fit at right end of block */
  double sigmal;                /* Standard deviation in left patch */
  double sigmar;                /* Standard deviation in right patch */
  double nx2;                   /* nx squared */
  double nx;                    /* Normalised distance into interpolation */
  double x[ 2*BOX ];            /* Array of sample positions */
  double y[ 2*BOX ];            /* Array of sample values */
  gsl_rng *r;                   /* GSL random number generator */
  int fillpad;                  /* Fill PAD samples ? */
  int good;                     /* Were any usable input values found? */
  int jj;                       /* Time-slice index */
  int jstart;                   /* Index of first flagged sample in block */
  int k;                        /* Loop count */
  int leftend;                  /* Index at end of left hand patch */
  int leftstart;                /* Index at start of left hand patch */
  int rightend;                 /* Index at end of right hand patch */
  int rightstart;               /* Index at start of right hand patch */
  size_t b1;                    /* Index of first bolometer to be filledd */
  size_t b2;                    /* Index of last bolometer to be filledd */
  size_t bstride;               /* bolo stride */
  int pend;                     /* Last non-PAD sample */
  int pstart;                   /* First non-PAD sample */
  size_t tstride;               /* time slice stride */
  smfFillGapsData *pdata = NULL;/* Pointer to job data */
  smf_qual_t *pq;
  smf_qual_t *qua = NULL;       /* Pointer to quality array */
  smf_qual_t mask;              /* Quality mask for bad samples */

  /* Pointer to the structure holding information needed by this thread. */
  pdata = (smfFillGapsData *) job_data_ptr;

  /* Copy data from the above structure into local variables. */
  b1 = pdata->b1;
  b2 = pdata->b2;
  bstride = pdata->bstride;
  dat = pdata->dat;
  ntslice = pdata->ntslice;
  qua = pdata->qua;
  r = pdata->r;
  tstride = pdata->tstride;
  mask = pdata->mask;
  pend = pdata->pend;
  pstart = pdata->pstart;
  fillpad = pdata->fillpad;

  /* Loop over bolometer */
  for( i = b1; i <= b2; i++ ) if( !qua || !(qua[ i*bstride ] & SMF__Q_BADB) ) {

    /* First, for simplicity, ensure that all flagged samples have bad data
       values. These are replaced with good values by the filling process. */
    if( qua ) {
      pd = dat + i*bstride;
      pq = qua + i*bstride;
      for( j = 0; j < ntslice; j++ ) {
        if( *pq & mask ) *pd = VAL__BADD;
        pd += tstride;
        pq += tstride;
      }
    }

    /* Indicate that the start of the next gap is no sooner than the first
       sample. The "jstart" value is used to record the index of the first
       sample in a gap (i.e. the first sample to be replaced). */
    jstart = pstart;

    /* Loop over time series. In this loop we fill gaps within the body
       of the time series. Filling of the padded regions at start and end
       is left until the loop has ended. */
    good = 0;
    pd = dat + i*bstride + pstart*tstride;
    for( j = pstart; j <= pend; j++ ) {

      /* If the current sample is good, check to see if it is the first
         good sample following a gap. If so, fill the gap. */
      if( *pd != VAL__BADD ) {
         if( jstart < j ) {
            smf1_fillgap( dat + i*bstride, pstart, pend, tstride, jstart,
                          j - 1, r, status );
         }

         /* Indicate the start of any subsequent gap is no sooner than the
            next sample. */
         jstart = j + 1;

         /* Indicate we have found some good samples. */
         good = 1;
      }

      pd += tstride;
    }

    /* If a gap extends to the last sample, fill it. */
    if( good && jstart <= pend )  smf1_fillgap( dat + i*bstride, pstart, pend,
                                                tstride, jstart, pend, r, status );

   /* Replace the padding at the start and end of the bolometer time series
      with a noisey curve that connects the first and last data samples
      smoothly. First, fit a straight line to the 2*BOX samples at the end
      of the time stream (i.e. the left end of the interpolated
      wrapped-around section). The above filling of gaps ensures the data
      values will not be bad, unless the whole bolometer is bad. */
    if( fillpad && good ) {
      leftstart = pend - 2*BOX + 1;
      leftend = pend;
      k = 0;
      for( jj = leftstart; jj <= leftend; jj++,k++ ) {
        x[ k ] = jj;
        y[ k ] = dat[ i*bstride + jj*tstride ];
      }

      kpg1Fit1d( 1, k, y, x, &ml, &cl, &sigmal, status );

      /* The main interpolation is performed using a single cubic curve
         that produces continuous values and gradients at the start and
	 end of the interpolation. However, an unconstrained cubic can
	 result in wild behaviour if there is a short sharp upturn in the
	 data values at the beginning or end of the data strea. In order
	 to contrain the cubic curve, the start and end of the data
	 stream are first padded with short sections that roll the
	 graient smoothly off to zero within a short length. The cubic
	 curve then joins the ends of the rolled-off sections (i.e. the
	 cubic has zero gardient at start and end). So roll the gradient
	 off to zero over 2*BOX samples at the end of the data stream, so
	 long as the padding area is big enough (we omit this gradient
	 roll off and just use the cubic interpolation otherwise). */
      meanl = ml*( pend + 1 ) + cl;
      leftstart = pend + 1;
      leftend = leftstart + 2*BOX;
      if( leftend < ntslice - 2*BOX ) {
         dg = ml/(2*BOX);
         for( jj = leftstart; jj <= leftend; jj++ ) {
            dat[ i*bstride + jj*tstride ] = meanl +
                                            gsl_ran_gaussian( r, sigmal );
            ml -= dg;
            meanl += ml;
         }
      } else {
         leftend = pend;
      }

      /* If possible fit a straight line to the 2*BOX samples at the start of
         the time series. */
      rightstart = pstart;
      rightend = pstart + 2*BOX - 1;
      k = 0;
      for( jj = rightstart; jj <= rightend; jj++,k++ ) {
        x[ k ] = jj;
        y[ k ] = dat[ i*bstride + jj*tstride ];
      }
      kpg1Fit1d( 1, k, y, x, &mr, &cr, &sigmar, status );

      /* Roll the gradient off to zero over 2*BOX samples at the start of the
         data stream, so long as the padding area is big enough (we omit this
         gradient roll off and just use the cubic interpolation otherwise). */
      meanr = mr*( pstart - 1 ) + cr;
      rightend = pstart - 1;
      rightstart = rightend - 2*BOX;
      if( rightstart > 2*BOX ) {
         dg = mr/(2*BOX);
         for( jj = rightend; jj >= rightstart; jj-- ) {
            dat[ i*bstride + jj*tstride ] = meanr +
                                            gsl_ran_gaussian( r, sigmar );
            mr -= dg;
            meanr -= mr;
         }
      } else {
         rightstart = pstart;
      }

      /* Form the coefficients needed for cubic interpolation between the
         end and the start. Cubic interpolation produces continuity in the
         gradients as well as the values. */

      dlen = ntslice - leftend + rightstart - 1;

      a = 2*( meanl - meanr ) + dlen*( ml + mr );
      b = 3*( meanr - meanl ) - dlen*( 2*ml + mr );
      c = ml*dlen;
      d = meanl;

      /* Form the coefficients needed for linear interpolation of the
         noise levels. */
      e = sigmal;
      f = sigmar - sigmal;

      /* Replace the padding at the end of the time stream. */
      for( jj = leftend + 1; jj < ntslice; jj++ ) {
         nx = ( jj - leftend )/dlen;
         nx2 = nx*nx;
         dat[ i*bstride + jj*tstride ] = a*nx2*nx + b*nx2 + c*nx + d +
                                         gsl_ran_gaussian( r, e + nx*f );
      }

      /* Replace the padding at the start of the time stream. */
      for( jj = 0; jj < rightstart; jj++ ) {
         nx = ( jj - leftend + ntslice )/dlen;
         nx2 = nx*nx;
         dat[ i*bstride + jj*tstride ] = a*nx2*nx + b*nx2 + c*nx + d +
                                         gsl_ran_gaussian( r, e + nx*f );
      }

/* If no good samples were found, replace them all with zero. */
    } else if( !good ) {
      pd = dat + i*bstride;
      for( j = 0; j < ntslice; j++ ) {
        *pd = 0.0;
        pd += tstride;
      }
    }
  }
}


/* Fill a single gap in a single bolometer time-stream. */
static void smf1_fillgap( double *data, int pstart, int pend, size_t tstride,
                          int jstart, int jend, gsl_rng *r, int *status ){


/* Local Variables: */
   double *pd;
   double fillval;
   double grad;
   double offset;
   double s1;
   double sigma;
   double sigmal;
   double sigmar;
   double vl;
   double vr;
   double x[ BOX ];
   double y[ BOX ];
   int jhi;
   int jlo;
   int k;
   int s2;
   int jj;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* If the gap is only a single sample, use the mean of the adjacent
   samples. These are sure to be good, since they would be part of the
   gap if they were bad. */
   if( jstart == jend ) {
      if( jstart > pstart ) {
         s1 = data[ ( jstart - 1 )*tstride ];
         s2 = 1;
      } else {
         s1 = 0.0;
         s2 = 0;
      }

      if( jend < pend ) {
         s1 += data[ ( jend + 1 )*tstride ];
         s2++;
      }

      data[ jstart*tstride ] = s2 ? s1/s2 : 0.0;

/* For larger gaps, we fill it with a straight line plus noise. */
   } else {

/* Find an estimate of the mean value and noise level at the start of the
   gap. We do this by fitting a straight line to the good data immediately
   before the start of the gap. Indicate we do not yet know the value or
   noise at the start of the gap. */
      vl = VAL__BADD;
      sigmal = VAL__BADD;

/* The last sample in the box is the first sample before the gap. The first
   sample in the box is BOX samples before that. */
      jhi = jstart - 1;
      jlo = jstart - BOX;

/* If the lower end of the box is off the start of the array, set it to
   the start of the array. */
      if( jlo < pstart ) jlo = pstart;

/* Check the box is large enough to fit. */
      if( jhi - jlo + 1 >= MINBOX ) {

/* Copy the box data values into two arrays suitable for kpg1Fit1d. */
         k = 0;
         pd = data + jlo*tstride;
         for( jj = jlo; jj <= jhi; jj++,k++ ) {
            x[ k ] = jj;
            y[ k ] = *pd;
            pd += tstride;
         }

/* Attempt to do the fit, annulling any error (e.g. caused by too few
   good values in the box - i.e. if there are other gaps very close to
   the one we are filling). */
         if( *status == SAI__OK ) {
            kpg1Fit1d( 1, k, y, x, &grad, &offset, &sigmal, status );
            if( *status != SAI__OK ) {
               errAnnul( status );
               sigmal = VAL__BADD;

/* If the fit succeeded, find the data value which the line has at the
   start of the gap. */
            } else {
               vl = grad*jstart + offset;
            }
         }
      }

/* If we cannot do the fit, use the mean of the (up to) 3 good values
   immediately prior to the gap, and use zero noise. */
      if( sigmal == VAL__BADD ) {
         s1 = 0.0;
         s2 = 0;

         jlo = jstart - 3;
         if( jlo < pstart ) jlo = pstart;
         pd = data + jlo*tstride;
         for( jj = jlo; jj <= jhi; jj++ ) {
            if( *pd != VAL__BADD ) {
               s1 += *pd;
               s2++;
            }
            pd += tstride;
         }
         if( s2 > 0 ) vl = s1/s2;
      }

/* Now find an estimate of the mean value and noise level at the end of the
   gap in the same way. Indicate we do not yet know the value or noise at the
   end of the gap. */
      vr = VAL__BADD;
      sigmar = VAL__BADD;

/* The first sample in the box is the first sample after the gap. The last
   sample in the box is BOX samples after that. */
      jlo = jend + 1;
      jhi = jend + BOX;

/* If the upper end of the box is off the end of the array, set it to
   the end of the array. */
      if( jhi > pend ) jhi = pend;

/* Check the box is large enough to fit. */
      if( jhi - jlo + 1 >= MINBOX ) {

/* Copy the box data values into two arrays suitable for kpg1Fit1d. */
         k = 0;
         pd = data + jlo*tstride;
         for( jj = jlo; jj <= jhi; jj++,k++ ) {
            x[ k ] = jj;
            y[ k ] = *pd;
            pd += tstride;
         }

/* Attempt to do the fit, annulling any error (e.g. caused by too few
   good values in the box - i.e. if there are other gaps very close to
   the one we are filling). */
         if( *status == SAI__OK ) {
            kpg1Fit1d( 1, k, y, x, &grad, &offset, &sigmar, status );
            if( *status != SAI__OK ) {
               errAnnul( status );
               sigmar = VAL__BADD;

/* If the fit succeeded, find the data value which the line has at the
   end of the gap. */
            } else {
               vr = grad*jend + offset;
            }
         }
      }

/* If we cannot do the fit, use the mean of the (up to) 3 good values
   immediately after the gap, and use zero noise. */
      if( sigmar == VAL__BADD ) {
         s1 = 0.0;
         s2 = 0;

         jhi = jend + 3;
         if( jhi > pend ) jhi = pend;
         pd = data + jlo*tstride;
         for( jj = jlo; jj <= jhi; jj++ ) {
            if( *pd != VAL__BADD ) {
               s1 += *pd;
               s2++;
            }
            pd += tstride;
         }
         if( s2 > 0 ) vr = s1/s2;
      }

/* If we have the mean value at start and end of the box, we fill it with
   a straight line that interpolates these values, with added noise if
   possible. */
      if( vl != VAL__BADD && vr != VAL__BADD ) {

/* Find the noise level to use. */
         if( sigmal != VAL__BADD && sigmar != VAL__BADD ) {
            sigma = 0.5*( sigmal + sigmar );
         } else if( sigmal != VAL__BADD ) {
            sigma = sigmal;
         } else if( sigmar != VAL__BADD ) {
            sigma = sigmar;
         } else {
            sigma = 0.0;
         }

/* Find the gradient and offset for the straight line used to create the
   replacement values for the gap. */
         grad = ( vr - vl )/ ( jend - jstart );
         offset = vl - grad*jstart;

/* Replace the gap values with the straight line values, plus noise. */
         pd = data + jstart*tstride;
         if( sigma > 0.0 ) {
            for( jj = jstart; jj <= jend; jj++ ) {
               *pd = grad*jj + offset + gsl_ran_gaussian( r, sigma );
               pd += tstride;
            }
         } else {
            for( jj = jstart; jj <= jend; jj++ ) {
               *pd = grad*jj + offset;
               pd += tstride;
            }
         }

/* If we do not have the mean value at start or end of the box, we fill it
   with a constant value. */
      } else {
         if( vl != VAL__BADD ) {
            fillval = vl;
         } else if( vr != VAL__BADD ) {
            fillval = vr;
         } else {
            fillval = 0.0;
         }
         pd = data + jstart*tstride;
         for( jj = jstart; jj <= jend; jj++ ) {
            *pd = fillval;
            pd += tstride;
         }
      }
   }
}


