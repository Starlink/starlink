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
*     smf_clean_pca( smfData *data, double thresh, smfData **components,
*                    smfData **amplitudes, int *status )

*  Arguments:
*     data = smfData * (Given)
*        Pointer to the input smfData
*     thresh = double (Given)
*        Outlier threshold for amplitudes to remove from data for cleaning
*     components = smfData ** (Returned)
*        New 3d cube of principal component time-series (ncomponents x 1 x time)
*     amplitudes = smfData ** (Returned)
*        New 3d map giving amplitudes of each component for each bolometer
*        (bolometer, component amplitude)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:

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

*  Notes:
*     The input bolometer time series are assumed to have had their
*     means removed before entry.

*  Authors:
*     Ed Chapin (UBC)

*  History:
*     2011-03-16 (EC):
*        Initial version -- only does the projection, no filtering

*  Copyright:
*     Copyright (C) 2011 University of British Columbia.
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

#define FUNC_NAME "smf_clean_pca"

void smf_clean_pca( smfData *data, double thresh, smfData **components,
                    smfData **amplitudes, int *status ) {

  double *amp=NULL;       /* matrix of components amplitudes for each bolo */
  size_t abstride;        /* bolo stride in amp array */
  size_t acompstride;     /* component stride in amp array */
  size_t bstride;         /* bolo stride */
  double *comp=NULL;      /* data cube of components */
  size_t ccompstride;     /* component stride in comp array */
  size_t ctstride;        /* time stride in comp array */
  gsl_matrix *cov=NULL;   /* bolo-bolo covariance matrix */
  double *d = NULL;       /* Pointer to data array */
  size_t i;               /* Loop counter */
  size_t j;               /* Loop counter */
  size_t k;               /* Loop counter */
  size_t *goodbolo=NULL;  /* Indices of the good bolometers for analysis */
  dim_t nbolo;            /* number of bolos */
  dim_t ndata;            /* number of samples in data */
  dim_t ngoodbolo;        /* number good bolos = number principal components */
  dim_t ntslice;          /* number of time slices */
  smf_qual_t *qua=NULL;   /* Pointer to quality array */
  gsl_vector *s=NULL;     /* singular values for SVD */
  size_t tstride;         /* time slice stride */
  gsl_matrix *v=NULL;     /* orthogonal square matrix for SVD */
  gsl_vector *work=NULL;  /* workspace for SVD */

  if (*status != SAI__OK) return;

  /* Check for NULL smfData pointer */
  if( !data || !data->pntr[0]) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME
            ": possible programming error, NULL data supplied", status );
    return;
  }

  smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, &ndata, &bstride, &tstride,
                status );

  if( data->ndims != 3 ) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME
            ": possible programming error, smfData should be 3-dimensional",
            status );
    return;
  }

  if( data->dtype != SMF__DOUBLE ) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME
            ": possible programming error, smfData should be double precision",
            status );
    return;
  }

  qua = smf_select_qualpntr( data, 0, status );

  if( qua ) {
    /* If quality supplied, identify good bolometers */
    ngoodbolo = 0;
    for( i=0; i<nbolo; i++ ) {
      if( !(qua[i*bstride]&SMF__Q_BADB) ) {
        ngoodbolo++;
      }
    }

    /* Now remember which were the good bolometers */
    goodbolo = astCalloc( ngoodbolo, sizeof(*goodbolo), 1 );
    ngoodbolo = 0;
    for( i=0; i<nbolo; i++ ) {
      if( !(qua[i*bstride]&SMF__Q_BADB) ) {
        goodbolo[ngoodbolo] = i;
        ngoodbolo++;
      }
    }

  } else {
    /* Otherwise assume all bolometers are good */
    ngoodbolo = nbolo;
    goodbolo = astCalloc( ngoodbolo, sizeof(*goodbolo), 1 );
    for( i=0; i<ngoodbolo; i++ ) {
      goodbolo[i] = i;
    }
  }

  if( ngoodbolo <= 2 ) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME ": fewer than 2 working bolometers!", status );
    goto CLEANUP;
  }

  /* Allocate arrays */

  amp = astCalloc( nbolo*nbolo, sizeof(*amp),1 );
  comp = astCalloc( ngoodbolo*ntslice, sizeof(*comp),1 );

  cov = gsl_matrix_alloc( ngoodbolo, ngoodbolo );
  s = gsl_vector_alloc( ngoodbolo );
  v = gsl_matrix_alloc( ngoodbolo, ngoodbolo );
  work = gsl_vector_alloc( ngoodbolo );

  /* These strides will make comp time-ordered */
  ccompstride = 1;
  ctstride = ngoodbolo;

  /* These strides will also make amp look time-ordered (sort-of: the time
     axis is now the component number */
  abstride = 1;
  acompstride = nbolo;

  /* input bolo data pointer */
  d = data->pntr[0];

  if( *status == SAI__OK ) {
    double c;

    msgOutif( MSG__DEBUG, "", FUNC_NAME
              ": measuring bolo-bolo covariance matrix...", status );

    /* Measure the covariance matrix */
    for( i=0; i<ngoodbolo; i++ ) {
      msgOutiff( MSG__DEBUG, "", "   bolo %zu", status, goodbolo[i] );

      for( j=i; j<ngoodbolo; j++ ) {
        c = gsl_stats_covariance( d + goodbolo[i]*bstride, tstride,
                                  d + goodbolo[j]*bstride, tstride, ntslice );

        gsl_matrix_set( cov, i, j, c );
        gsl_matrix_set( cov, j, i, c );
      }
    }
  }

  /* First factor cov = u s v^T, noting that the gsl routine
     calculates U in cov in-place. */

  msgOutif( MSG__DEBUG, "", FUNC_NAME
            ": perfoming singular value decomposition...", status );

  gsl_linalg_SV_decomp( cov, v, s, work );

  /* The above calculation tells us what linear combinations of the original
     bolometer time series will give us the statistically independent new
     set of basis vectors (components), which we then normalize by their RMS. */

  msgOutif( MSG__DEBUG, "", FUNC_NAME
            ": calculating statistically-independent components...", status );

  if( *status==SAI__OK ) for( i=0; i<ngoodbolo; i++ ) {   /* loop over comp */
    double sigma;
    double u;

    msgOutiff( MSG__DEBUG, "", "   bolo %zu", status, goodbolo[i] );

    if( *status==SAI__OK ) for( j=0; j<ngoodbolo; j++ ) { /* loop over bolo */

        u = gsl_matrix_get( cov, j, i );

      /* Calculate the vector */
      for( k=0; k<ntslice; k++ ) {
        comp[i*ccompstride+k*ctstride] += d[goodbolo[j]*bstride + k*tstride]*u;
      }
    }

      /* Then normalize */
      smf_stats1D( comp + i*ccompstride, ctstride, ntslice, NULL, 0,
                   0, NULL, &sigma, NULL, status );

      if( *status == SAI__OK ) for( k=0; k<ntslice; k++ ) {
        comp[i*ccompstride + k*ctstride] /= sigma;
      }
  }

  /* Now project the data along each of these normalized basis vectors
     to figure out the amplitudes of the components in each bolometer
     time series. */

  msgOutif( MSG__DEBUG, "", FUNC_NAME
              ": calculating component amplitudes in each bolo...", status );

  if( *status == SAI__OK ) {
    for( i=0; i<ngoodbolo; i++ ) {    /* loop over bolometer */
      msgOutiff( MSG__DEBUG, "", "   bolo %zu", status, goodbolo[i] );
      for( j=0; j<ngoodbolo; j++ ) {  /* loop over component */
        for( k=0; k<ntslice; k++ ) {
          amp[goodbolo[i]*abstride + j*acompstride] +=
            d[goodbolo[i]*bstride + k*tstride] *
            comp[j*ccompstride + k*ctstride];
        }
      }
    }
  }

  /* Returning components? */
  if( (*status==SAI__OK) && components ) {
    dim_t dims[3];
    int lbnd[3];

    dims[0] = ngoodbolo;
    dims[1] = 1;
    lbnd[0] = 0;
    lbnd[1] = 0;

    if( data->isTordered ) { /* T is 3rd axis in data if time-ordered */
      dims[2] = data->dims[2];
      lbnd[2] = data->lbnd[2];
    } else {                 /* T is 1st axis in data if bolo-ordered */
      dims[2] = data->dims[0];
      lbnd[2] = data->lbnd[0];
    }

    *components = smf_construct_smfData( NULL, NULL, NULL, NULL, NULL,
                                         SMF__DOUBLE, NULL, NULL,
                                         SMF__QFAM_TSERIES, NULL,
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
      q = astCalloc( nbolo*ngoodbolo, sizeof(*q), 1 );

      if( *status == SAI__OK ) {
        for( i=0; i<nbolo; i++ ) {        /* bolometer */
          for( j=0; j<ngoodbolo; j++ ) {  /* component amplitude */
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
    dims[2] = ngoodbolo;
    lbnd[2] = 0;


    *amplitudes = smf_construct_smfData( NULL, NULL, NULL, NULL, NULL,
                                         SMF__DOUBLE, NULL, q,
                                         SMF__QFAM_TSERIES, NULL,
                                         1, dims, lbnd, 3, 0, 0, NULL,
                                         NULL, status );
    if( *status==SAI__OK ) {
      (*amplitudes)->pntr[0] = amp;
      amp = NULL;  /* Set to NULL to avoid freeing before exiting */
    }
  }

  /* Clean up */
 CLEANUP:
  if( amp ) amp = astFree( amp );
  if( comp ) comp = astFree( comp );
  if( goodbolo ) goodbolo = astFree( goodbolo );
  if( cov ) gsl_matrix_free( cov );
  if( s ) gsl_vector_free( s );
  if( v ) gsl_matrix_free( v );
  if( work ) gsl_vector_free( work );

}
