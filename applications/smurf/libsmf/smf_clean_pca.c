/*
*+
*  Name:
*     smf_clean_pca

*  Purpose:
*     Clean smfData by removing the strongest correlations using PCA

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     size_t smf_clean_pca( ThrWorkForce *wf, smfData *data, size_t t_first,
*                           size_t t_last, double thresh, size_t ncomp,
*                           double lim, smfData **components,
*                           smfData **amplitudes, int flagbad, int sub,
*                           AstKeyMap *keymap, smf_qual_t mask, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData * (Given)
*        Pointer to the input smfData (assume that bolometer means have been
*        removed). Any bad or flagged samples are replaced by interpolated
*        data before doing the PCA analysis (the interpolated values are
*        included in the analysis).
*     t_first = size_t (Given)
*        First time slice of the data to be analyzed. Ignored if t_last
*        is zero.
*     t_last = size_t (Given)
*        Last time slice of the data to be cleaned. If set to zero, the full
*        time axis in the supplied smfData is used, excluding the padding
*        at start and end.
*     thresh = double (Given)
*        Outlier threshold for amplitudes to remove from data for
*        cleaning. If negative, the number of components to remove is specified
*        explicitly by "ncomp".
*     ncomp = size_t (Given)
*        The number of components to remove. Only used if "thresh" is
*        negative.
*     lim = double (Given)
*        The minimum fraction of good samples required in a bolometer for it
*        to be used in the determination of the principal components.
*     components = smfData ** (Returned)
*        New 3d cube of principal component time-series (ncomponents * 1 * time)
*        Can be NULL. Will only have length t_last-t_first+1.
*     amplitudes = smfData ** (Returned)
*        New 3d cube giving amplitudes of each component for each bolometer
*        (bolo X * bolo Y * component amplitude). Can be NULL.
*     flagbad = int (Given)
*        If set, compare each bolometer to the first component as a template
*        to decide whether the data are good or not. Not supported if t_first
*        and t_last don't correspond to the full length of data.
*     sub = int (Given)
*        If non-zero, the values returned in "data" are the supplied data
*        values minus the select PCA components. If zero, the values returned
*        in "data" are the select PCA components themselves.
*     keymap = AstKeyMap * (Given)
*        Keymap containing parameters that control how flagbad works. See
*        smf_find_gains for details.
*     mask = smf_qual_t (Given)
*        Define which bits in quality indicate locations of gaps to be filled
*        prior to doing the PCA analysis.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function uses principal component analysis (PCA) to remove
*     correlated signals from the data. A new set of basis vectors
*     (eigenvectors, or components henceforth) are determined for the
*     N working bolometer time series such that their statistical
*     covariances are 0. These components are normalized by their
*     standard deviations, and then each bolometer is expressed as a
*     linear combination of the components. Both the components, and
*     their amplitudes (eigenvalues) in each bolometer time series may
*     be optionally returned. The largest amplitude components are
*     generally assumed to be noise sources, and are identified as
*     outliers from the general population and removed.
*
*     In addition, this routine can be used to flag bad bolometers in
*     the same way that the common-mode routines work. Once the
*     decomposition into principal components is complete (but prior
*     to cleaning), most of the signal in each bolometer should
*     resemble the first, largest component (sky+fridge).A fit of this
*     template to each bolometer is a useful way of finding entire
*     bolometers (or portions) that are outliers, and their quality
*     arrays are flagged accordingly.

*  Authors:
*     EC: Ed Chapin (UBC)
*     DSB: David Berry (EAO)

*  History:
*     2011-03-16 (EC):
*        Initial version -- only does the projection, no filtering
*     2011-03-17 (EC):
*        -Add basic header to returned components smfData
*        -Parallelize as much as possible over exclusive time chunks
*     2011-10-12 (EC):
*        Add t_first, t_last
*     2015-06-03 (DSB):
*        - Check for bad values in the supplied data. These are now ignored,
*        which is equivalent to treating them as zero. This should be
*        reasonable given that the supplied data is known to have zero mean.
*        - Likewise ignore samples that are flagged as unfittable in the
*        quality array
*     2015-06-15 (DSB):
*        Add argument "sub".
*     2016-10-10 (DSB):
*        Determine the PCA components from the left hand singular vectors
*        (U) found by smf_svd rather than the right hand (V). U and V should
*        be the same (because the co-variance matrix is symetric), but U
*        seems to be less prone to changes as you change the number of
*        threads.
*     2016-10-13 (DSB):
*        - Add argument mask.
*        - Remove quality checking as it seems to result in less accurate
*        analysis (the reconstructed input data seems to be much noisier
*        if bad samples are ignored). Instead, fill all bad samples and
*        samples flagged  by "mask" using smf_fillgaps before doing the analysis.
*        - The padding included at start and end of each bolometer seems to
*        cause problems for the PCA model. So now, if t_last is supplied as
*        zero, t_first and t_last are set automatically to exclude padding.
*        - The filling described above can change the mean value in each
*        bolometer. So now we estimate and remove the mean value in each
*        bolometer after doing the filling. Therefore the means no longer
*        need to be removed before calling this function.
*     2016-11-08 (DSB):
*        - Added argument ncomp.
*        - Return the number of components removed as the function value.
*     2016-12-09 (DSB):
*        Check for components that are constant (i.e. have zero standard
*        deviation). Such components cannot be normalised by their
*        standard deviation. They are now set bad and excluded form all
*        further use. In fact all components that have very small sigma
*        compared to the other components are set bad.
*     2017-1-27 (DSB):
*        Correct calculation and use of log mean of the component sigma
*        values.
*     2019-3-27 (DSB):
*        - If the absolute number of components to remove is supplied, this
*        function can be speeded up a lot by only calculating the
*        specified number of components. The other - weaker - components
*        need not be calculated.
*        - When determining the principal components, do not include
*        boloemeters that too few good samples. This because gap-filled
*        samples seem to upset the calculation of the components.

*  Copyright:
*     Copyright (C) 2011 University of British Columbia.
*     Copyright (C) 2015, 2016 East Asian Observatory.
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
#include "ast.h"
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "prm_par.h"

/* GSL includes */
#include "gsl/gsl_linalg.h"
#include "gsl/gsl_statistics_double.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define CHECK 0

/* ------------------------------------------------------------------------ */
/* Local variables and functions */

/* Structure containing information about blocks of time slices to be
   processed by each thread. All threads read/write to/from mutually
   exclusive parts of data. */

typedef struct smfPCAData {
  double *amp;            /* matrix of components amplitudes for each bolo */
  size_t abstride;        /* bolo stride in amp array */
  size_t acompstride;     /* component stride in amp array */
  size_t bstride;         /* bolo stride */
  double *comp;           /* data cube of components */
  gsl_matrix *cov;        /* bolo-bolo covariance matrix */
  double *covwork;        /* work array for covariance calculation */
  size_t ccompstride;     /* component stride in comp array */
  size_t ctstride;        /* time stride in comp array */
  smfData *data;          /* Pointer to input data */
  size_t *goodbolo;       /* Local copy of global goodbolo */
  int ijob;               /* Job identifier */
  dim_t nbolo;            /* Number of detectors  */
  size_t ncalc;           /* Number of components to calculate */
  size_t ngoodbolo;       /* Number of good bolos */
  dim_t tlen;             /* Number of time slices */
  int operation;          /* 0=covar,1=eigenvect,2=projection */
  int sub;                /* Subtract the selected PCA components? */
  double *rms_amp;        /* VAL__BADD where modes need to be removed */
  size_t t1;              /* Index of first time slice for chunk */
  size_t t2;              /* Index of last time slice */
  size_t b1;              /* Index of first bolo for chunk */
  size_t b2;              /* Index of last bolo */
  size_t goodlim;         /* Min number of usable samples in a good bolo */
  size_t t_first;         /* First index for total data being analyzed */
  size_t t_last;          /* Last index for total data being analyzed */
  size_t tstride;         /* time slice stride */
  smf_qual_t *qua;        /* Quality array */
} smfPCAData;

void smfPCAParallel( void *job_data_ptr, int *status );

void smfPCAParallel( void *job_data_ptr, int *status ) {
  dim_t tlen;             /* number of time slices */
  double *amp=NULL;       /* matrix of components amplitudes for each bolo */
  double *comp=NULL;      /* data cube of components */
  double *covwork=NULL;   /* goodbolo * 3 work array for covariance */
  double *d=NULL;         /* Pointer to data array */
  double *pd=NULL;        /* Pointer to next data value */
  double *rms_amp=NULL;   /* VAL__BADD for components to remove */
  double mean;            /* Mean of bolometer values */
  double sum;             /* Sum of bolometer values */
  double v1;              /* A data value */
  double v2;              /* A data value */
  gsl_matrix *cov=NULL;   /* bolo-bolo covariance matrix */
  size_t *goodbolo;       /* Local copy of global goodbolo */
  size_t *pg;             /* Pointer to next goodbolo value */
  size_t abstride;        /* bolo stride in amp array */
  size_t acompstride;     /* component stride in amp array */
  size_t bstride;         /* bolo stride */
  size_t ccompstride;     /* component stride in comp array */
  size_t ctstride;        /* time stride in comp array */
  size_t i;               /* Loop counter */
  size_t j;               /* Loop counter */
  size_t k;               /* Loop counter */
  size_t l;               /* Loop counter */
  size_t ncalc;           /* number of calculated principal components */
  size_t ngoodbolo;       /* number good bolos */
  size_t t_first;         /* First time slice being analyzed */
  size_t t_last;          /* First time slice being analyzed */
  size_t tstride;         /* time slice stride */
  smfPCAData *pdata=NULL; /* Pointer to job data */
  smf_qual_t *pq;

  double check=0;

  if( *status != SAI__OK ) return;

  /* Pointer to the data that this thread will process */
  pdata = job_data_ptr;

  abstride = pdata->abstride;
  acompstride = pdata->acompstride;
  amp = pdata->amp;
  bstride = pdata->bstride;
  ccompstride = pdata->ccompstride;
  comp = pdata->comp;
  cov = pdata->cov;
  covwork = pdata->covwork;
  ctstride = pdata->ctstride;
  d = pdata->data ? pdata->data->pntr[0] : NULL;
  goodbolo = pdata->goodbolo;
  ngoodbolo = pdata->ngoodbolo;
  ncalc = pdata->ncalc;
  rms_amp = pdata->rms_amp;
  t_first = pdata->t_first;
  t_last = pdata->t_last;
  tlen = pdata->tlen;
  tstride = pdata->tstride;

  /*
  printf("----------------------\n\n");
  printf("t_first=%zu t_last=%zu tlen=%zu\n", t_first, t_last, tlen);
  printf("t1=%zu t2=%zu\n", pdata->t1, pdata->t2);
  printf("bstride=%zu tstride=%zu\n", bstride, tstride);
  printf("abstride=%zu acompstride=%zu\n", abstride, acompstride);
  printf("ctstride=%zu ccompstride=%zu\n", ctstride, ccompstride);
  printf("\n----------------------\n");
  */

  /* Check for valid inputs */
  if( !pdata ) {
    *status = SAI__ERROR;
    errRep( "", "smfPCAParallel: No job data supplied", status );
    return;
  }

  /* Debugging message indicating thread started work */
  msgOutiff( SMF__TIMER_MSG, "",
             "smfPCAParallel: op=%i thread starting on time slices %zu -- %zu",
             status, pdata->operation, pdata->t1, pdata->t2 );


  /* if t1 past end of the work, nothing to do so we return */
  if( pdata->t1 >= (t_first + tlen) ) {
    msgOutif( SMF__TIMER_MSG, "",
              "smfPCAParallel: nothing for thread to do, returning",
              status);
    return;
  }

  if( (pdata->operation == -2) && (*status==SAI__OK) ) {
    size_t ngood;

    /* Operation -2: identify usable bolometers (bolometers with more
       than "goodlim" good samples */

    pg = pdata->goodbolo + pdata->b1;
    for( i = pdata->b1; i <= pdata->b2; i++,pg++ ) {
       *pg = 0;
       pq = pdata->qua + i*bstride;
       if( !(*pq & SMF__Q_BADB) ) {
         pq += t_first*tstride;
         ngood = 0;
         for( k = t_first; k <= t_last; k++ ) {
            if( !(*pq & SMF__Q_GOOD ) ) ngood++;
            pq += tstride;
         }
         if( ngood > pdata->goodlim ) *pg = 1;
       }
    }


  } else if( (pdata->operation == -1) && (*status==SAI__OK) ) {

    /* Operation -1: remove mean from each bolometer in a block of bolos ---- */

    for( i=pdata->b1; i<=pdata->b2; i++ ) {
       pd = d + goodbolo[i]*bstride + t_first*tstride;

       sum = 0.0;
       for( k = t_first; k <= t_last; k++ ) {
          sum += *pd;
          pd += tstride;
       }

       mean = sum/tlen;

       pd = d+goodbolo[i]*bstride + t_first*tstride;
       for( k = t_first; k <= t_last; k++ ) {
          *pd -= mean;
          pd += tstride;
       }
    }

  } else if( (pdata->operation == 0) && (*status==SAI__OK) ) {
    /* Operation 0: accumulate sums for covariance calculation -------------- */

    check = 0;
    for( i=0; i<ngoodbolo; i++ ) {
      double sum_xy;

      /*msgOutiff( MSG__DEBUG, "", "   bolo %zu", status, goodbolo[i] );*/

      for( j=i; j<ngoodbolo; j++ ) {
        sum_xy = 0;

        for( k=pdata->t1; k<=pdata->t2; k++ ) {
          v1 = d[goodbolo[i]*bstride + k*tstride];
          v2 = d[goodbolo[j]*bstride + k*tstride];
          sum_xy += v1*v2;
        }

        /* Store sums in work array and normalize once all threads finish */
        covwork[ i + j*ngoodbolo ] = sum_xy;
        //printf("(%zu,%zu) %lg\n", i, j, sum_xy);

        check += sum_xy;
      }
    }

    if( CHECK ) printf("--- check %i: %lf\n", pdata->operation, check);

  } else if( (pdata->operation == 1) && (*status == SAI__OK) ) {
    /* Operation 1: normalized eigenvectors --------------------------------- */

    check = 0;

    for( i=0; i<ncalc; i++ ) {   /* loop over comp */
      double u;

      /*msgOutiff( MSG__DEBUG, "", "   bolo %zu", status, goodbolo[i] );*/

      for( j=0; j<ngoodbolo; j++ ) { /* loop over bolo */

        u = gsl_matrix_get( cov, i, j );  /* U should be equal to V, so
                                             use (V^T)^T (i.e. cov^T) as U */

        /* Calculate the vector. Note that t1 and t2 are absolute time
           pointers into the master data array, but comp only contains a
           subset from t_first to t_last */

        for( k=pdata->t1; k<=pdata->t2; k++ ) {
          l = k - t_first;
          v2 = d[goodbolo[j]*bstride + k*tstride];
          comp[i*ccompstride+l*ctstride] += v2 * u;
          check += v2 * u;
        }
      }
    }

    if( CHECK ) printf("--- check %i: %lf\n", pdata->operation, check);

  } else if( (pdata->operation == 2) && (*status == SAI__OK) ) {
    /* Operation 2: project data along eigenvectors ------------------------- */

    check = 0;

    //printf("t1=%zu t2=%zu\n", pdata->t1, pdata->t2);

    for( i=0; i<ngoodbolo; i++ ) {    /* loop over bolometer */
      /*msgOutiff( MSG__DEBUG, "", "   bolo %zu", status, goodbolo[i] );*/
      for( j=0; j<ncalc; j++ ) {  /* loop over component */
        if( comp[j*ccompstride] != VAL__BADD ) {
          for( k=pdata->t1; k<=pdata->t2; k++ ) {
            v1 = d[goodbolo[i]*bstride + k*tstride];
            l = k - t_first;
            amp[goodbolo[i]*abstride + j*acompstride] +=
                   v1 * comp[j*ccompstride + l*ctstride];
          }
        }
      }
    }

    for( i=0; i<(1280l*ncalc); i++ ) check += amp[i];

    if( CHECK ) {
       printf("--- check %i: %lf\n", pdata->operation, check);
    }

  } else if( (pdata->operation == 3) && (*status == SAI__OK) ) {
    /* Operation 3: clean --------------------------------------------------- */
    double a;
    double factor = pdata->sub ? 1 : -1;

    for( j=0; j<ncalc; j++ ) {        /* loop over component */
      if( rms_amp[j] == VAL__BADD && comp[j*ccompstride] != VAL__BADD ) {

        /* Bad values in rms_amp indicate components that we are
           removing. Subtract the component scaled by the amplitude for
           each bolometer at all relevant time-slices */

        for( i=0; i<ngoodbolo; i++ ) {    /* loop over bolometer */
          a =  factor*amp[goodbolo[i]*abstride + j*acompstride];
          if( a != 0.0 ) {
            for( k=pdata->t1; k<=pdata->t2; k++ ) {
              l = k - t_first;
              d[goodbolo[i]*bstride + k*tstride] -=
                a*comp[j*ccompstride + l*ctstride];
            }
          }
        }
      }
    }

  } else if( *status==SAI__OK ) {
    *status = SAI__ERROR;
    errRep( "", "smfPCAParallel"
            ": possible programming error: invalid operation number", status );
  }

  /* Debugging message indicating thread finished work */
  msgOutiff( SMF__TIMER_MSG, "",
             "smfPCAParallel: op=%i thread finishing time slices %zu -- %zu",
             status, pdata->operation, pdata->t1, pdata->t2 );
}

/* ------------------------------------------------------------------------ */


#define FUNC_NAME "smf_clean_pca"

size_t smf_clean_pca( ThrWorkForce *wf, smfData *data, size_t t_first,
                      size_t t_last, double thresh, size_t ncomp,
                      double lim, smfData **components, smfData **amplitudes,
                      int flagbad, int sub, AstKeyMap *keymap,
                      smf_qual_t mask, int *status ){

  double *amp=NULL;       /* matrix of components amplitudes for each bolo */
  size_t abstride;        /* bolo stride in amp array */
  size_t acompstride;     /* component stride in amp array */
  size_t bstride;         /* bolo stride */
  double *comp=NULL;      /* data cube of components */
  size_t ccompstride;     /* component stride in comp array */
  size_t ctstride;        /* time stride in comp array */
  gsl_matrix *cov=NULL;   /* bolo-bolo covariance matrix */
  size_t i;               /* Loop counter */
  int ii;                 /* Loop counter */
  size_t j;               /* Loop counter */
  smfPCAData *job_data=NULL;/* job data */
  size_t k;               /* Loop counter */
  size_t *goodbolo1=NULL; /* Indices of the good bolometers */
  size_t *goodbolo2=NULL; /* Indices of the high quality bolometers */
  dim_t nbolo;            /* number of bolos */
  dim_t ndata;            /* number of samples in data */
  size_t ncalc;           /* number of PCA components to calculate */
  size_t ngoodbolo1;      /* number good bolos */
  size_t ngoodbolo2;      /* number high quality bolos */
  dim_t ntslice;          /* number of time slices */
  int nw;                 /* total available worker threads */
  smfPCAData *pdata=NULL; /* Pointer to job data */
  smf_qual_t *qua=NULL;   /* Pointer to quality array */
  gsl_vector *s=NULL;     /* singular values for SVD */
  size_t bstep;           /* Bolo step size for job division */
  size_t step;            /* step size for job division */
  size_t tlen;            /* Length of the time-series used for PCA */
  size_t tstride;         /* time slice stride */
  gsl_vector *work=NULL;  /* workspace for SVD */

  if (*status != SAI__OK) return 0;

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Check for NULL smfData pointer */
  if( !data || !data->pntr[0]) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME
            ": possible programming error, NULL data supplied", status );
    return 0;
  }

  smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, &ndata, &bstride, &tstride,
                status );

  if( data->ndims != 3 ) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME
            ": possible programming error, smfData should be 3-dimensional",
            status );
    return 0;
  }

  if( data->dtype != SMF__DOUBLE ) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME
            ": possible programming error, smfData should be double precision",
            status );
    return 0;
  }

  if( ntslice <= 2 ) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME ": fewer than 2 time slices!", status );
    goto CLEANUP;
  }

  /* If the range of time slices has not been specified, use the total
     range excluding padding and apodizing. */
  qua = smf_select_qualpntr( data, 0, status );
  if( !t_last ) {
     if( qua ) {
        smf_get_goodrange( qua, ntslice, tstride, (SMF__Q_PAD | SMF__Q_APOD),
                           &t_first, &t_last, status );
     } else {
        t_last = ntslice-1;
     }
  }

  if( t_last > (ntslice-1) ) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME ": t_last is set past the last time slice!",
            status );
    goto CLEANUP;
  }

  if( (t_last < t_first) || ( (t_last - t_first) < 1 ) ) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME ": t_last - t_first must be > 1", status );
    goto CLEANUP;
  }

  tlen = t_last - t_first + 1;

  if( flagbad && (tlen != ntslice ) ) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME
            ": flagbad unsupported if t_first/last do not span full data",
            status );
    goto CLEANUP;
  }

  if( qua ) {
    /* If quality supplied, identify good bolometers (i.e. bolos that
       have not been entirely rejected). */
    ngoodbolo1 = 0;
    for( i=0; i<nbolo; i++ ) {
      if( !(qua[i*bstride]&SMF__Q_BADB) ) {
        ngoodbolo1++;
      }
    }

    /* Now remember which were the good bolometers */
    goodbolo1 = astCalloc( ngoodbolo1, sizeof(*goodbolo1) );
    ngoodbolo1 = 0;
    for( i=0; i<nbolo; i++ ) {
      if( !(qua[i*bstride]&SMF__Q_BADB) ) {
        goodbolo1[ngoodbolo1] = i;
        ngoodbolo1++;
      }
    }

  } else {
    /* Otherwise assume all bolometers are good */
    ngoodbolo1 = nbolo;
    goodbolo1 = astCalloc( ngoodbolo1, sizeof(*goodbolo1) );
    for( i=0; i<ngoodbolo1; i++ ) {
      goodbolo1[i] = i;
    }
  }

  if( ngoodbolo1 <= 2 ) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME ": fewer than 2 working bolometers!", status );
    goto CLEANUP;
  }

  /* Now identify the "high quality" bolometers. These are bolometers from
     which few samples have been rejected. Rejected samples are gap-filled
     below, and too much gap-filling can badly affect the determination of
     the principal components. So we exclude from the analysis bolometers
     with many rejected samples. */

  /* Allocate job data for threads */
  job_data = astCalloc( nw, sizeof(*job_data) );

  /* If quality supplied, identify the bolometers to be used in the
     analysis. Bolometers that have a lot of gaps are not used as they
     can badly affect the determination of the principal components. */
  if( qua ) {
    goodbolo2 = astMalloc( nbolo*sizeof(goodbolo2) );

    if( nw > (int) nbolo ) {
      bstep = 1;
    } else {
      bstep = nbolo/nw;
    }

    for( ii=0; (*status==SAI__OK)&&(ii<nw); ii++ ) {
      pdata = job_data + ii;
      pdata->b1 = ii*bstep;
      if( ii == nw - 1 ) {
         pdata->b2 = nbolo - 1;
      } else {
         pdata->b2 = (ii+1)*bstep - 1;
      }

      pdata->goodbolo = goodbolo2;
      pdata->bstride = bstride;
      pdata->tstride = tstride;
      pdata->t_first = t_first;
      pdata->t_last = t_last;
      pdata->qua = qua;
      pdata->goodlim = lim*tlen;
      pdata->operation = -2;

      thrAddJob( wf, 0, pdata, smfPCAParallel, 0, NULL, status );
    }

    /* Wait until all of the submitted jobs have completed */
    thrWait( wf, status );

    /* The above stores a boolean flag for each bolo in "goodbolo2".
       Instead, store the indices of the usable bolometers at the start
       of "goodbolo2". */
    ngoodbolo2 = 0;
    for( i = 0; i < nbolo; i++ ) {
      if( goodbolo2[ i ] ) goodbolo2[ ngoodbolo2++ ] = i;
    }

    if( ngoodbolo2 < ngoodbolo1 ) {
      msgOutiff( MSG__DEBUG, " ", FUNC_NAME ": excluding %zu poor bolometers "
                  "from the component calculations", status, ngoodbolo1-ngoodbolo2 );
    }

    if( ngoodbolo2 <= 2 ) {
      *status = SAI__ERROR;
      errRep( " ", FUNC_NAME ": fewer than 2 high quality bolometers!", status );
      goto CLEANUP;
    }

  } else{
    ngoodbolo2 = ngoodbolo1;
    goodbolo2 = goodbolo1;
  }

/* If "thresh" is negative, we already know how many PCA components will
   be removed, in which case we do not need to calculate the other PCA
   components. Decide how many PCA components to calculate. */
  if( thresh < 0.0 ) {
     ncalc = ncomp;
     if( ncalc > ngoodbolo2 ) ncalc = ngoodbolo2;
  } else {
     ncalc = ngoodbolo2;
  }

  /* Fill bad values and values flagged via "mask" (except entirely bad
     bolometers) with interpolated data values. */
  mask &= ~SMF__Q_BADB;
  smf_fillgaps( wf, data, mask, status );

  /* Allocate arrays */
  amp = astCalloc( nbolo*ncalc, sizeof(*amp) );
  comp = astCalloc( ncalc*tlen, sizeof(*comp) );
  cov = gsl_matrix_alloc( ngoodbolo2, ngoodbolo2 );
  s = gsl_vector_alloc( ngoodbolo2 );
  work = gsl_vector_alloc( ngoodbolo2 );

  /* These strides will make comp time-ordered */
  ccompstride = 1;
  ctstride = ncalc;

  /* These strides will also make amp look time-ordered (sort-of: the time
     axis is now the component number */
  abstride = 1;
  acompstride = nbolo;

  /* Set up the division of labour for threads: independent blocks of time */

  if( nw > (int) tlen ) {
    step = 1;
  } else {
    step = tlen/nw;
  }

  if( nw > (int) ngoodbolo2 ) {
    bstep = 1;
  } else {
    bstep = ngoodbolo2/nw;
  }

  for( ii=0; (*status==SAI__OK)&&(ii<nw); ii++ ) {
    pdata = job_data + ii;

    /* Blocks of time slices */
    pdata->t1 = ii*step + t_first;
    pdata->t2 = (ii+1)*step + t_first - 1;

    /* Blocks of bolometers. */
    pdata->b1 = ii*bstep;
    pdata->b2 = (ii+1)*bstep - 1;

    /* Ensure that the last thread picks up any left-over tslices */
    if( (ii==(nw-1)) ) {
       pdata->t2 = t_first + tlen - 1;
       pdata->b2 = ngoodbolo2 - 1;
    }

    /* Ensure we don't try to use more bolos or tslices than we have. This
       can happen for instance if the number of good bolos is smaller than
       the number of threads. */
    if( pdata->t2 >= t_first + tlen ) pdata->t2 = t_first + tlen - 1;
    if( pdata->b2 >= ngoodbolo2 ) pdata->b2 = ngoodbolo2 - 1;

    /* initialize work data */
    pdata->amp = NULL;
    pdata->abstride = abstride;
    pdata->acompstride = acompstride;
    pdata->bstride = bstride;
    pdata->comp = comp;
    pdata->cov = NULL;
    pdata->covwork = NULL;
    pdata->ccompstride = ccompstride;
    pdata->ctstride = ctstride;
    pdata->data = data;
    pdata->goodbolo = goodbolo2;
    pdata->ijob = -1;
    pdata->nbolo = nbolo;
    pdata->ngoodbolo = ngoodbolo2;
    pdata->t_first = t_first;
    pdata->t_last = t_last;
    pdata->tlen = tlen;
    pdata->operation = 0;
    pdata->tstride = tstride;
    pdata->ncalc = ncalc;

    /* Each thread will accumulate the projection of its own portion of
       the time-series. We'll add them to the master amp at the end */
    pdata->amp = astCalloc( nbolo*ncalc, sizeof(*(pdata->amp)) );

    /* Each thread will accumulate sums of x, y, and x*y for each bolo when
       calculating the covariance matrix */
    pdata->covwork = astCalloc( ngoodbolo2*ngoodbolo2,
                                sizeof(*(pdata->covwork)) );
  }

  if( *status == SAI__OK ) {

    /* Remove the mean from each gap-filled bolometer time stream ---------------------*/

    msgOutif( MSG__VERB, "", FUNC_NAME ": removing bolometer means...",
              status );

    for( ii=0; ii<nw; ii++ ) {
      pdata = job_data + ii;
      pdata->operation = -1;
      thrAddJob( wf, 0, pdata, smfPCAParallel, 0, NULL, status );
    }

    /* Wait until all of the submitted jobs have completed */
    thrWait( wf, status );



    /* Measure the covariance matrix using parallel code ---------------------*/

    msgOutif( MSG__VERB, "", FUNC_NAME
              ": measuring bolo-bolo covariance matrix...", status );

    /* Set up the jobs to calculate sums for each time block and submit */
    for( ii=0; ii<nw; ii++ ) {
      pdata = job_data + ii;
      pdata->operation = 0;
      pdata->ijob = thrAddJob( wf, THR__REPORT_JOB, pdata, smfPCAParallel,
                                 0, NULL, status );
    }

    /* Wait until all of the submitted jobs have completed */
    thrWait( wf, status );

    /* We now have to add together all of the sums from each thread and
       normalize */
    if( *status == SAI__OK ) {
      for( i=0; i<ngoodbolo2; i++ ) {
        for( j=i; j<ngoodbolo2; j++ ) {
          double c;
          double *covwork=NULL;
          double sum_xy;

          sum_xy = 0;

          for( ii=0; ii<nw; ii++ ) {
            pdata = job_data + ii;
            covwork = pdata->covwork;

            sum_xy += covwork[ i + j*ngoodbolo2 ];
          }

          c = sum_xy / ((double)tlen-1);

          gsl_matrix_set( cov, i, j, c );
          gsl_matrix_set( cov, j, i, c );
        }
      }
    }
  }

  /* Factor cov = u s v^T, noting that the SVD routine calculates v^T in
     in-place of cov. --------------------------------------------------------*/

  msgOutif( MSG__VERB, "", FUNC_NAME
            ": perfoming singular value decomposition...", status );

  smf_svd( wf, ngoodbolo2, cov->data, s->data, NULL, 10*VAL__EPSD,
           1, status );
  if( CHECK ) {
    double check=0;

    for( i=0; i<ngoodbolo2; i++ ) {
      for( j=0; j<ngoodbolo2; j++ ) {
        check += gsl_matrix_get( cov, j, i );
      }
    }

    printf("--- check inverted: %lf\n", check);
  }

  /* Calculate normalized eigenvectors with parallel code --------------------*/

  msgOutif( MSG__VERB, "", FUNC_NAME
            ": calculating statistically-independent components...", status );

  /* The above calculation tells us what linear combinations of the original
     bolometer time series will give us the statistically independent new
     set of basis vectors (components), which we then normalize by their RMS. */

  /* Set up the jobs to calculate sums for each time block and submit */
  if( *status == SAI__OK ) {
    for( ii=0; ii<nw; ii++ ) {
      pdata = job_data + ii;
      pdata->cov = cov;
      pdata->operation = 1;
      pdata->ijob = thrAddJob( wf, THR__REPORT_JOB, pdata, smfPCAParallel,
                                 0, NULL, status );
    }
  }

  /* Wait until all of the submitted jobs have completed */
  thrWait( wf, status );

  /* Then normalize. Some of the components may have zero amplitude and
     so cannot be used (i.e. we are trying to use more components than
     there is evidence for in the data). So we check for zero sigma. In
     fact, we check for silly small sigma, not just zero sigma. Any
     component for which the sigma is less than 1E-10 of the log-mean
     sigma is excluded. */
  {
    double *sigmas = astMalloc( ncalc*sizeof( *sigmas ) );
    double check = 0;
    double s1 = 0.0;
    int s2 = 0;
    int nlow = 0;

    for( i=0; (*status==SAI__OK)&&(i<ncalc); i++ ) {
      double sigma;

      smf_stats1D( comp + i*ccompstride, ctstride, tlen, NULL, 0,
                   0, NULL, &sigma, NULL, NULL, status );

      /* Apparently we need this to get the normalization right */
      sigma *= sqrt((double) tlen);

      if( *status == SAI__OK ) {
        if( sigma > 0.0 ) {
           for( k=0; k<tlen; k++ ) {
             comp[i*ccompstride + k*ctstride] /= sigma;
             sigmas[ i ] = sigma;
             s1 += log10( sigma );
             s2++;
           }
        } else {
           for( k=0; k<tlen; k++ ) {
             comp[i*ccompstride + k*ctstride] = VAL__BADD;
             sigmas[ i ] = VAL__BADD;
           }
           nlow++;
        }
      }
    }

    /* Exclude any components that have a silly small standard deviation
       (less that 1E-10 of the logmean of all components). Any with zero
       standard deviation will already have been excluded. */
    if( s2 > 0 ) {
       double thresh = 1E-10*pow( 10.0, s1/s2 );
       for( i=0; i<ncalc; i++ ) {
          if( sigmas[ i ] != VAL__BADD && sigmas[ i ] < thresh ) {
             for( k=0; k<tlen; k++ ) {
                comp[i*ccompstride + k*ctstride] = VAL__BADD;
                nlow++;
             }
          }
       }
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": rejecting %d (out of %zu) components"
               " because they are too weak to normalise", status, nlow,
               ngoodbolo2 );

    for( i=0; i<ncalc*tlen; i++ ) {
      if( comp[i] != VAL__BADD ) check += comp[i];
    }

    sigmas = astFree( sigmas );

    //printf("--- check component: %lf\n", check);
  }
  /* Now project the data along each of these normalized basis vectors
     to figure out the amplitudes of the components in each bolometer
     time series. In fact, empirically, it seems that these amplitudes
     are proportional to the square root of the associated eigenvalues
     calculated by smf_svd above. The constant of proportionality
     depends on the number of good bolometers, but probably also on the
     overall strength of the signal in the data, and so is not easy to
     calculate.
   ------------------------------------------------------------*/

  msgOutif( MSG__VERB, "", FUNC_NAME
              ": calculating component amplitudes in each bolo...", status );

  /* Set up the jobs. From here on the parallel code uses the full set of
     all good bolometers, not just the high quality bolometers. */
  if( *status == SAI__OK ) {

    if( nw > (int) ngoodbolo1 ) {
      bstep = 1;
    } else {
      bstep = ngoodbolo1/nw;
    }

    for( ii=0; ii<nw; ii++ ) {
      pdata = job_data + ii;

      pdata->b1 = ii*bstep;
      pdata->b2 = (ii+1)*bstep - 1;

      if( (ii==(nw-1)) ) pdata->b2 = ngoodbolo1 - 1;
      if( pdata->b2 >= ngoodbolo1 ) pdata->b2 = ngoodbolo1 - 1;

      pdata->goodbolo = goodbolo1;
      pdata->ngoodbolo = ngoodbolo1;

      pdata->operation = 2;
      pdata->ijob = thrAddJob( wf, THR__REPORT_JOB, pdata, smfPCAParallel,
                                 0, NULL, status );
    }
  }

  /* Wait until all of the submitted jobs have completed */
  thrWait( wf, status );

  /* Add all of the amp arrays together from the threads */
  if( *status == SAI__OK ) {
    size_t index;

    for( ii=0; ii<nw; ii++ ) {
      pdata = job_data + ii;

      for( i=0; i<ngoodbolo1; i++ ) {    /* Loop over good bolo */
        for( j=0; j<ncalc; j++ ) {       /* Loop over component */
          index = goodbolo1[i]*abstride + j*acompstride;
          amp[index] += pdata->amp[index];
        }
      }
    }
  }

  if( CHECK ){
    double check=0;

    for( i=0; i<nbolo*ncalc; i++ ) {
      check += amp[i];
    }
    printf("--- check combined amp: %lf\n", check);
  }

  if( CHECK ){
    double check=0;
    for( i=0; i<ncalc*tlen; i++ ) {
      if( comp[i] != VAL__BADD ) check += comp[i];
    }

    printf("--- check component A: %lf\n", check);
  }

  /* Check to see if the amplitudes are mostly negative or positive. If
     mostly negative, flip the sign of both the component and amplitudes */
  if( *status == SAI__OK ) {
    double total;
    for( j=0; j<ncalc; j++ ) {    /* loop over component */
      total = 0;
      for( i=0; i<ngoodbolo1; i++ ) {  /* loop over bolometer */
        total += amp[goodbolo1[i]*abstride + j*acompstride];
      }

      /* Are most amplitudes negative for this component? */
      if( total < 0 ) {
        /* Flip sign of the amplitude */
        for( i=0; i<ngoodbolo1; i++ ) { /* loop over bolometer */
          amp[goodbolo1[i]*abstride + j*acompstride] =
            -amp[goodbolo1[i]*abstride + j*acompstride];
        }

        /* Flip sign of the component */
        for( k=0; k<tlen; k++ ) {
           if(  comp[j*ccompstride + k*ctstride] != VAL__BADD ) {
              comp[j*ccompstride + k*ctstride] *= -1;
           }
        }
      }
    }
  }

  /* Finally, copy the master amp array back into the workspace for
     each thread */
  if( *status == SAI__OK ) {
    for( ii=0; ii<nw; ii++ ) {
      pdata = job_data + ii;
      memcpy( pdata->amp, amp, sizeof(*(pdata->amp))*nbolo*ncalc );
    }
  }

  if( CHECK ){
    double check=0;
    for( i=0; i<ncalc*tlen; i++ ) {
      if( comp[i] != VAL__BADD ) check += comp[i];
    }

    printf("--- check component B: %lf\n", check);
  }

  /* Flag outlier bolometers if requested ------------------------------------*/

  if( (*status==SAI__OK) && flagbad ) {
    smfArray *data_array=NULL;
    smfArray *gain_array=NULL;
    smfGroup *gain_group=NULL;
    AstKeyMap *kmap=NULL;         /* Local keymap */
    AstObject *obj=NULL;          /* Used to avoid compiler warnings */
    double *template=NULL;

    /* Obtain pointer to sub-keymap containing bolo rejection parameters.
       Since this is an essentially identical operation to what we do with
       COM, just lift its parameters for now */
    astMapGet0A( keymap, "COM", &obj );
    kmap = (AstKeyMap *) obj;
    obj = NULL;

    /* Create a 1d array containing a copy of the first component as a
       template */
    template = astCalloc( ntslice, sizeof(*template) );
    if( *status == SAI__OK ) {
      for( i=0; i<ntslice; i++ ) {
        template[i] = comp[i*ctstride];
      }
    }

    /* We need a smfData to store the gains of the template
       temporarily. Do this using smf_model_create (which uses
       smfArray's as inputs/outputs) to ensure that we get the
       dimensions that smf_find_gains will be expecting. */
    data_array = smf_create_smfArray( status );
    smf_addto_smfArray( data_array, data, status );

    smf_model_create( wf, NULL, &data_array, NULL, NULL, NULL,NULL, NULL, 1,
                      SMF__GAI, data->isTordered, NULL, 0, NULL, NULL,
                      NO_FTS, NULL, &gain_group, &gain_array, keymap, status );

    /* Compare bolometers to the template in order to flag outliers */
    smf_find_gains( wf, 0, data, NULL, NULL, template, kmap, SMF__Q_GOOD,
                    SMF__Q_COM, gain_array->sdata[0], NULL, status );

    /* Clean up */
    template = astFree( template );
    if( data_array ) {
      /* Data doesn't belong to us, so avoid freeing */
      data_array->owndata = 0;
      smf_close_related( wf, &data_array, status );
    }
    if( gain_array ) smf_close_related( wf, &gain_array, status );
    if( gain_group ) smf_close_smfGroup( &gain_group, status );
    if( kmap ) kmap = astAnnul( kmap );
  }

  /* Cleaning ----------------------------------------------------------------*/

  if( (*status == SAI__OK) && thresh ) {
    int converge=0;          /* Set if converged */
    size_t ngood;            /* Number of values that are still good */
    double *rms_amp=NULL;    /* RMS amplitude across bolos each component */
    double sum;
    double sum_sq;
    size_t iter=0;
    double x;
    int nsum;

    rms_amp = astCalloc( ncalc, sizeof(*rms_amp) );

    /* If we know how many components to remove, flag them by setting
       them bad in the above array. */
    if( thresh < 0.0 ) {
      ncomp = ncalc;
      for( i=0; i<ncalc; i++ ) rms_amp[i] = VAL__BADD;
      msgOutiff( MSG__VERB, "", FUNC_NAME ": will remove %zu / %zu components...",
                 status, ncalc, ngoodbolo2 );

    /* Otherwise, first calculate the RMS of the amplitudes across the array
       for each component. This will be a positive number whose value
       gives a typical amplitude of the component that can be compared
       to other components */
    } else if( *status == SAI__OK ) {

      for( i=0; i<ncalc; i++ ) {        /* Loop over component */
        sum = 0;
        sum_sq = 0;
        nsum = 0;
        for( j=0; j<ngoodbolo1; j++ ) {      /* Loop over bolo */
          x = amp[i*acompstride + goodbolo1[j]*abstride];
          if( x != 0.0 ) {
             sum += x;
             sum_sq += x*x;
             nsum++;
          }
        }

        if( nsum > 0 ) {
           rms_amp[i] = sqrt( sum_sq / ((double)nsum) );
        } else {
           rms_amp[i] = VAL__BADD;
        }
      }

      /* We determine the number of components to remove using a
         sigma-clipping algorithm. Perform an iterative clip using the mean
         and standard deviation of the component amplitude RMS's. Note that
         the RMS is always massively dominated by the first mode, so we will
         always assume that it should be removed to help things be more
         well-behaved. */

      rms_amp[0] = VAL__BADD;
      while( !converge && (*status==SAI__OK) ) {
        double m;                   /* mean */
        int new;                    /* Set if new values flagged */
        double sig;                 /* standard deviation */

        /* Update interation counter */
        iter ++;

        /* Measure mean and standard deviation of non-flagged samples */
        smf_stats1D( rms_amp, 1, ncalc, NULL, 0, 0, &m, &sig, NULL, &ngood,
                     status );

        msgOutiff( MSG__DEBUG, "", FUNC_NAME
                   ": iter %zu mean=%lf sig=%lf ngood=%zu", status,
                   iter, m, sig, ngood );

        /* Flag new outliers */
        new = 0;
        for( i=0; i<ngood; i++ ) {
          if( (rms_amp[i]!=VAL__BADD) && ((rms_amp[i]-m) > sig*thresh) ) {
            new = 1;
            rms_amp[i] = VAL__BADD;
            ngood--;
          }
        }

        /* Converged if no new values flagged */
        if( !new ) converge = 1;

        /* Trap huge numbers of iterations */
        if( iter > 50 ) {
          *status = SAI__ERROR;
          errRep( "", FUNC_NAME ": more than 50 iterations!", status );
        }

        /* If we have less than 10% of the original modes (or 2,
           whichever is larger), generate bad status */
        if( (ngood <= 2) || (ngood < 0.1*ncalc) ) {
          *status = SAI__ERROR;
          errRepf( "", FUNC_NAME ": only %zu of %zu modes remain!", status,
                   ncalc, ngood );
        }
      }

      ncomp = ncalc - ngood;
      msgOutiff( MSG__VERB, "", FUNC_NAME
                  ": after %zu clipping iterations, will remove "
                  "%zu / %zu components...", status, iter, ncomp, ncalc );

    }




    /* Now that we know which modes to remove, call the parallel routine
       for doing the hard work. If we are returning the selected PCA
       components rather than the cleaned data, zero the data array first. */
    if( !sub ) memset( data->pntr[0], 0, ndata*sizeof(double) );

    for( ii=0; ii<nw; ii++ ) {
      pdata = job_data + ii;
      pdata->operation = 3;
      pdata->sub = sub;
      pdata->rms_amp = rms_amp;
      pdata->ijob = thrAddJob( wf, THR__REPORT_JOB, pdata, smfPCAParallel,
                                 0, NULL, status );
    }

    /* Wait until all of the submitted jobs have completed */
    thrWait( wf, status );

    rms_amp = astFree( rms_amp );

    msgOutiff( MSG__VERB, "", FUNC_NAME ": %zu components removed.",
              status, ncomp );

  }

  /* Returning components? ---------------------------------------------------*/
  if( (*status==SAI__OK) && components ) {
    dim_t dims[3];
    int lbnd[3];
    smfHead *hdr=NULL;

    dims[0] = ncalc;
    dims[1] = 1;
    lbnd[0] = 0;
    lbnd[1] = 0;

    if( data->isTordered ) { /* T is 3rd axis in data if time-ordered */
      dims[2] = tlen;
      lbnd[2] = data->lbnd[2] + t_first;
    } else {                 /* T is 1st axis in data if bolo-ordered */
      dims[2] = tlen;
      lbnd[2] = data->lbnd[0] + t_first;
    }

    /* Copy the header if one was supplied with the input data. This
       will at least give us a sensible time axis and allow us to run
       components through sc2fft if stored to disk, although the
       bolometer axes will be misleading. Should probably strip off
       the time axis properly, e.g. code that was removed from
       smf_model_createHdr.c in commit
       2c342cd4f7d9ab33e4fea5bc250eaae9804a229f
    */
    if( data->hdr ) {
      hdr = smf_deepcopy_smfHead( data->hdr, status );
    }

    *components = smf_construct_smfData( NULL, NULL, hdr, NULL, NULL,
                                         SMF__DOUBLE, NULL, NULL,
                                         SMF__QFAM_TSERIES, NULL, 0,
                                         1, dims, lbnd, 3, 0, 0, NULL,
                                         NULL, status );

    if( *status == SAI__OK ) {
      (*components)->pntr[0] = comp;
      comp = NULL;  /* Set to NULL to avoid freeing before exiting */
    }
  }

  /* Returning amplitudes? */
  if( (*status==SAI__OK) && amplitudes ) {
    dim_t dims[3];
    int lbnd[3];

    smf_qual_t *q=NULL;   /* Quality array that just maps SMF__Q_BADB */

    if( qua ) {
      q = astCalloc( nbolo*ncalc, sizeof(*q) );

      if( *status == SAI__OK ) {
        for( i=0; i<nbolo; i++ ) {        /* bolometer */
          for( j=0; j<ncalc; j++ ) {  /* component amplitude */
            q[i*abstride + j*acompstride] = qua[i*bstride]&SMF__Q_BADB;
          }
        }
      }
    }

    if( data->isTordered ) { /* if time-ordered bolos first 2 dims */
      dims[0] = data->dims[0];
      dims[1] = data->dims[1];
      lbnd[0] = data->lbnd[0];
      lbnd[1] = data->lbnd[1];
    } else {                 /* if bolo-ordered bolos last 2 dims */
      dims[0] = data->dims[1];
      dims[1] = data->dims[2];
      lbnd[0] = data->lbnd[1];
      lbnd[1] = data->lbnd[2];
    }

    /* last dimension enumerates component */
    dims[2] = ncalc;
    lbnd[2] = 0;


    *amplitudes = smf_construct_smfData( NULL, NULL, NULL, NULL, NULL,
                                         SMF__DOUBLE, NULL, q,
                                         SMF__QFAM_TSERIES, NULL,
                                         0, 1, dims, lbnd, 3, 0, 0, NULL,
                                         NULL, status );
    if( *status==SAI__OK ) {
      (*amplitudes)->pntr[0] = amp;
      amp = NULL;  /* Set to NULL to avoid freeing before exiting */
    }
  }

  if( comp && CHECK ) {
    double check=0;
    for( i=0; i<ncalc*tlen; i++ ) {
      if( comp[i] != VAL__BADD ) check += comp[i];
    }

    printf("--- check component again: %lf\n", check);
  }

  /* Clean up */
 CLEANUP:
  amp = astFree( amp );
  comp = astFree( comp );
  if( goodbolo1 != goodbolo2 ) goodbolo2 = astFree( goodbolo2 );
  goodbolo1 = astFree( goodbolo1 );
  if( cov ) gsl_matrix_free( cov );
  if( s ) gsl_vector_free( s );
  if( work ) gsl_vector_free( work );

  if( job_data ) {
    for( ii=0; ii<nw; ii++ ) {
      pdata = job_data + ii;
      if( pdata->covwork ) pdata->covwork = astFree( pdata->covwork );
      if( pdata->amp ) pdata->amp = astFree( pdata->amp );
    }
    job_data = astFree(job_data);
  }

  return ncomp;

}
