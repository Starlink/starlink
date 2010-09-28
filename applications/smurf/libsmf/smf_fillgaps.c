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
*     smf_fillgaps( smfWorkForce *wf, smfData *data, smf_qual_t *quality,
*                   smf_qual_t mask, int *status )

*  Arguments:
*     wf = smfWorkForce * (Given)
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

*  Copyright:
*     Copyright (C) 2010 Univeristy of British Columbia.
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
#define BOX 50

/* Structure containing information about blocks of bolos to be
   filled by each thread. */
typedef struct smfFillGapsData {
  dim_t nbolo;                  /* Number of bolos */
  dim_t ntslice;                /* Number of time slices */
  double *dat;                  /* Pointer to bolo data */
  gsl_rng *r;                   /* GSL random number generator */
  size_t b1;                    /* Index of first bolometer to be filledd */
  size_t b2;                    /* Index of last bolometer to be filledd */
  size_t bstride;               /* bolo stride */
  size_t tstride;               /* time slice stride */
  size_t pend;                  /* Last non-PAD sample */
  size_t pstart;                /* First non-PAD sample */
  smf_qual_t *qua;              /* Pointer to quality array */
  smf_qual_t mask;              /* Quality mask for bad samples */
} smfFillGapsData;


/* Prototype for the function to be executed in each thread. */
static void smfFillGapsParallel( void *job_data_ptr, int *status );




void  smf_fillgaps( smfWorkForce *wf, smfData *data,
                    smf_qual_t mask, int *status ) {

/* Local Variables */
  const gsl_rng_type *type;     /* GSL random number generator type */
  dim_t bpt;                    /* Number of bolos per thread */
  dim_t i;                      /* Bolometer index */
  dim_t nbolo;                  /* Number of bolos */
  dim_t ntslice;                /* Number of time slices */
  double *dat=NULL;             /* Pointer to bolo data */
  gsl_rng *r;                   /* GSL random number generator */
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

  if( !qua ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": No valid QUALITY array was provided", status );
    return;
  }

  if( !dat ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData does not contain a DATA component",status);
    return;
  }

  if( *status == SAI__OK ) {
    /* obtain data dimensions */
    smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, NULL, &bstride, &tstride,
                  status );
  }

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

  /* Create a default GSL random number generator. */
  type = gsl_rng_default;
  r = gsl_rng_alloc (type);

  /* If the supplied "mask" value includes SMF__Q_PAD, then we will be
  replacing the zero-padded region at the start and end of each time series
  with artificial noisey data that connects the first and last data values
  smoothly. Remove SMF__Q_PAD from the mask, and find the indices of the
  first and last non-PAD sample. If we are not replacing zero-padded samples,
  set pstart and pend to indicate this. */
  if( mask & SMF__Q_PAD ) {
     mask &= ~SMF__Q_PAD;
     smf_get_goodrange( qua, ntslice, tstride, SMF__Q_PAD, &pstart, &pend,
                        status );
  } else {
     pstart = 0;
     pend = ntslice - 1;
  }

  /* Begin a job context. */
  smf_begin_job_context( wf, status );

  /* Loop over bolometer in groups of "bpt". */
  pdata = job_data;
  for( i = 0; i < nbolo; i += bpt, pdata++ ) {

    /* Store information for this group in the  next smfFillGapsData
       structure. */
    pdata->nbolo = nbolo;
    pdata->ntslice = ntslice;
    pdata->dat = dat;
    pdata->r = r;
    pdata->b1 = i;
    pdata->b2 = i + bpt - 1;
    pdata->pend = pend;
    pdata->pstart = pstart;
    if( pdata->b2 >= nbolo ) pdata->b2 = nbolo - 1;
    pdata->bstride = bstride;
    pdata->tstride = tstride;
    pdata->qua = qua;
    pdata->mask = mask;

    /* Submit a job to the workforce to process this group of bolometers. */
    (void) smf_add_job( wf, 0, pdata, smfFillGapsParallel, NULL, status );
  }

  /* Wait until all jobs in the current job context have completed, and
     then end the job context. */
  smf_wait( wf, status );
  smf_end_job_context( wf, status );

  /* Free resources. */
  gsl_rng_free( r );
  job_data = astFree( job_data );
}





/* Function to be executed in thread: fill gaps in all bolos from b1 to b2 */

static void smfFillGapsParallel( void *job_data_ptr, int *status ) {

/* Local Variables */
  dim_t i;                      /* Bolometer index */
  dim_t j;                      /* Time-slice index */
  dim_t nbolo;                  /* Number of bolos */
  dim_t ntslice;                /* Number of time slices */
  double *dat = NULL;           /* Pointer to bolo data */
  double a;                     /* Cubic interpolation coefficient */
  double b;                     /* Cubic interpolation coefficient */
  double c;                     /* Cubic interpolation coefficient */
  double cl;                    /* Offset of fit at left end of block */
  double cr;                    /* Offset of fit at right end of block */
  double d;                     /* Cubic interpolation coefficient */
  double dlen;                  /* Total number of samples being interpolated */
  double e;                     /* Linear interpolation coefficient */
  double f;                     /* Linear interpolation coefficient */
  double grad;                  /* Gradient of line joining patch mid-points */
  double meanl;                 /* Mean value in left patch */
  double meanr;                 /* Mean value in right patch */
  double ml;                    /* Gradient of fit at left end of block */
  double mr;                    /* Gradient of fit at right end of block */
  double offset;                /* Offset of line joining patch mid-points */
  double sigma;                 /* Mean standard deviation */
  double sigmal;                /* Standard deviation in left patch */
  double sigmar;                /* Standard deviation in right patch */
  double nx2;                   /* nx squared */
  double nx;                    /* Normalised distance into interpolation */
  double x[ BOX ];              /* Array of sample positions */
  double y[ BOX ];              /* Array of sample values */
  gsl_rng *r;                   /* GSL random number generator */
  int count;                    /* No. of unflagged since last flagged sample */
  int flagged;                  /* Is the current sample flagged? */
  int inside;                   /* Was previous sample flagged? */
  int jend;                     /* Index of last flagged sample in block */
  int jfinal;                   /* Final time-slice index */
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
  size_t pend;                  /* Last non-PAD sample */
  size_t pstart;                /* First non-PAD sample */
  size_t tstride;               /* time slice stride */
  smfFillGapsData *pdata = NULL;/* Pointer to job data */
  smf_qual_t *qua = NULL;       /* Pointer to quality array */
  smf_qual_t mask;              /* Quality mask for bad samples */

  /* Pointer to the structure holding information needed by this thread. */
  pdata = (smfFillGapsData *) job_data_ptr;

  /* Copy data from the above structure into local variables. */
  b1 = pdata->b1;
  b2 = pdata->b2;
  bstride = pdata->bstride;
  dat = pdata->dat;
  nbolo = pdata->nbolo;
  ntslice = pdata->ntslice;
  qua = pdata->qua;
  r = pdata->r;
  tstride = pdata->tstride;
  mask = pdata->mask;
  pend = pdata->pend;
  pstart = pdata->pstart;

   /* Pre-calculate a useful constant - the final used value of "j". */
  jfinal = ntslice - 1;

  /* Loop over bolometer */
  for( i = b1; i <= b2; i++ ) if( !(qua[ i*bstride ] & SMF__Q_BADB) ) {

    /* Initialise a flag to indicate that the current sample is not
       inside a block of flagged samples. */
    inside = 0;

    /* Initialise the count of unflagged samples following the previous
       block of flagged samples. */
    count = 0;

    /* Initialise the index of the first sample to be replaced. This
       initial value is only used if there are fewer than BOX unflagged
       samples before the first block of flagged samples. */
    jstart = 0;
    jend = -1;

    /* Loop over time series. In this loop we fill gaps within the body
       of the time series. Filling of the padded regions at start and end
       is left until the loop has ended. */
    for( j=0; j<ntslice; j++ ) {

      /* Is this sample flagged? Always condsider the last sample to be
         unflagged, so that any block of flagged samples at the end of
         the time-series is filled. Samples with bad data values are
         filled, as well as samples with the specified quality. */
      flagged = ( qua[ i*bstride + j*tstride ] & mask ) ||
                ( dat[ i*bstride + j*tstride ] == VAL__BADD );

      if( flagged && (int) j < jfinal ) {

        /* If this is the first flagged sample in a new block of flagged
           samples, set "inside" to indicate that we are now inside a
           block of flagged samples. */
        if( ! inside ) {
          inside = 1;

          /* If the number of unflagged samples since the end of the
             previous block of flagged samples is at least BOX, then
             record the index of the first sample to be replaced in this
             new block. If there have been fewer than BOX samples since
             the end of the previous block of flagged samples, we consider
             this new block to be an extension of the previous block,
             and so we do not change the "jstart" value (the few
             unflagged samples that were found between the two blocks
             of flagged samples will be replaced, together with the
             neighbouring flagged samples). We do this because we need at
             last BOX unflagged samples between adjacent pairs of flagged
             blocks. */
          if( count >= BOX ) jstart = j;
        }

      /* If this sample is not flagged (or if it is the final sample in
         the time-series)... */
      } else {

        /* If this is the first sample following a block of flagged samples,
           record the index of the last sample in the block (which may be
           the current sample if the current sample is the last sample), and
           indicate that we are no longer in a block. Also reset the count
           of unflagged samples following the end of the flagged block. */
        if( inside ) {
          if( flagged ) {
             jend = j;
          } else {
             jend = j - 1;
          }
          inside = 0;
          count = 0;
        }

        /* Increment the number of unflagged samples following the end
           of the previous flagged block. */
        count++;

        /* If we have now found BOX unflagged samples following the end of
           the previous block of flagged samples, we can replace the block.
           Also replace the block if we have reached the end of the time
           series. */
        if( ( count == BOX || (int) j == jfinal ) && jend >= jstart ) {

          /* If the block is only a single pixel wide, just replace it
             with the mean of the two neighbouring sample values. */
          if( jend == jstart ) {
              if( jend == 0 ) {
                 dat[ i*bstride + jend*tstride ] =
                         dat[ i*bstride + ( jend + 1 )*tstride ];
              } else if( jend == jfinal ) {
                 dat[ i*bstride + jend*tstride ] =
                         dat[ i*bstride + ( jend - 1 )*tstride ];
              } else {
                 dat[ i*bstride + jend*tstride ] = 0.5*(
                         dat[ i*bstride + ( jend + 1 )*tstride ] +
                         dat[ i*bstride + ( jend - 1 )*tstride ] );
              }

          /* Otherwise, we fill the block using a straight line plus
             noise... */
          } else {

            /* If possible fit a straight line to the BOX samples following
               the end of the flagged block. */
            rightstart = jend + 1;
            rightend = jend + BOX;
            if( rightend >= (int) ntslice ) rightend = ntslice - 1;
            if( rightend - rightstart > BOX/2 ) {
              k = 0;
              for( jj = rightstart; jj <= rightend; jj++,k++ ) {
                x[ k ] = jj;
                y[ k ] = dat[ i*bstride + jj*tstride ];
              }

              kpg1Fit1d( 1, k, y, x, &mr, &cr, &sigmar, status );

            } else {
              mr = VAL__BADD;
              cr = VAL__BADD;
              sigmar = VAL__BADD;
            }

            /* If possible fit a straight line to the BOX samples preceeding
               the start of the flagged block. */
            leftend = jstart - 1;
            leftstart = jstart - BOX;
            if( leftstart < 0 ) leftstart = 0;
            if( leftend - leftstart > BOX/2 ) {
              k = 0;
              for( jj = leftstart; jj <= leftend; jj++,k++ ) {
                x[ k ] = jj;
                y[ k ] = dat[ i*bstride + jj*tstride ];
              }
              kpg1Fit1d( 1, k, y, x, &ml, &cl, &sigmal, status );
            } else {
              ml = VAL__BADD;
              cl = VAL__BADD;
              sigmal = VAL__BADD;
            }

            /* Find the mean noise level. */
            if( sigmal != VAL__BADD && sigmar != VAL__BADD ) {
               sigma = 0.5*( sigmal + sigmar );
            } else if( sigmal != VAL__BADD ) {
               sigma = sigmal;
            } else {
               sigma = sigmar;
            }

            /* Find the gradient and offset for the straight line used to
               create the replacement values for the flagged block. */
            if( jstart <= 0 || ml == VAL__BADD || cl == VAL__BADD  ) {
              grad = 0.0;
              offset = mr*( jend + 1 ) + cr;

            } else if( jend >= jfinal || mr == VAL__BADD || cr == VAL__BADD  ) {
              grad = 0.0;
              offset = ml*( jstart - 1 ) + cl;

            } else {
              meanl = ml*( jstart - 1 ) + cl;
              meanr = mr*( jend + 1 ) + cr;
              grad = ( meanr - meanl )/( jend - jstart + 2 );
              offset = meanl - grad*( jstart - 1 );
            }

            /* If at least one of the straight line fits above was
               succesful, the flagged block is replaced by a straight line
               plus noise. */
            if( sigma != VAL__BADD ) {
              for( jj = jstart; jj <= jend; jj++ ) {
                dat[ i*bstride + jj*tstride ] = grad*jj + offset +
                                                gsl_ran_gaussian( r, sigma );


              }
            }
          }
        }
      }
    }

   /* Replace the padding at the start and end of the bolometer time series
      with a noisey curve that connects the first and last data samples
      smoothly. First, fit a straight line to the BOX samples at the end of
      the time stream. The above filling of gaps ensures the data values
      will not be bad. */
    if( pstart > 0 && pend < ntslice - 1 ) {
      leftstart = pend - BOX + 1;
      leftend = pend;
      k = 0;
      for( jj = leftstart; jj <= leftend; jj++,k++ ) {
        x[ k ] = jj;
        y[ k ] = dat[ i*bstride + jj*tstride ];
      }

      kpg1Fit1d( 1, k, y, x, &ml, &cl, &sigmal, status );

      /* If possible fit a straight line to the BOX samples at the start of
         the time series. */
      rightstart = pstart;
      rightend = pstart + BOX - 1;
      k = 0;
      for( jj = rightstart; jj <= rightend; jj++,k++ ) {
        x[ k ] = jj;
        y[ k ] = dat[ i*bstride + jj*tstride ];
      }
      kpg1Fit1d( 1, k, y, x, &mr, &cr, &sigmar, status );

      /* Form the coefficients needed for cubic interpolation between the
      end and the start. Cubic interpolation produces continuity in the
      gradients as well as the values. */
      meanl = ml*( pend + 1 ) + cl;
      meanr = mr*( pstart - 1 ) + cr;

      dlen = pstart - pend + ntslice;

      a = 2*( meanl - meanr ) + dlen*( ml + mr );
      b = 3*( meanr - meanl ) - dlen*( 2*ml + mr );
      c = ml*dlen;
      d = meanl;


      /* Form the coefficients needed for linear interpolation of the
      noise levels. */
      e = sigmal;
      f = sigmar - sigmal;

      /* Replace the padding at the end of the time stream. */
      for( jj = pend + 1; jj < (int) ntslice; jj++ ) {
         nx = ( jj - pend )/dlen;
         nx2 = nx*nx;
         dat[ i*bstride + jj*tstride ] = a*nx2*nx + b*nx2 + c*nx + d +
                                         gsl_ran_gaussian( r, e + nx*f );
      }

      /* Replace the padding at the start of the time stream. */
      for( jj = 0; jj < (int) pstart; jj++ ) {
         nx = ( jj - pend + ntslice )/dlen;
         nx2 = nx*nx;
         dat[ i*bstride + jj*tstride ] = a*nx2*nx + b*nx2 + c*nx + d +
                                         gsl_ran_gaussian( r, e + nx*f );
      }

    }
  }
}


