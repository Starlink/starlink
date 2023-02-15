/*
*+
*  Name:
*     smf_subtract_plane3

*  Purpose:
*     Fit and subtract a 2d plane from each time slice

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void smf_subtract_plane3( ThrWorkForce *wf, smfData *data,
*                         const dim_t mdims[],
*                         const int lut[], int * status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData * (Given and Returned)
*        The data to be filtered (performed in-place)
*     mdims = const dim_t[] (Given)
*        Dimensions of map.
*     lut = const int * (Given)
*        Lookup table with a coordinate corresponding to each item in
*        "data". It is assumed that this is the same size as the data
*        array in "data" and in ICD order.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is a support routine for the PLN iterative map-maker model. It fits a
*     2-d plane to each time slice using the coordinates supplied in the look
*     up table.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     Currently only works for a single sub-array. To fit a plane simultaneously
*     on multiple subarrays will require a smfArray for "data" and "lut".
*
*     If we do not know the position of the sample we do not include it in the
*     fit. This will only happen if the map we are reconstructing is smaller than
*     the area covered by the scan pattern. In general this is a bad idea for the
*     map-maker anyhow so we have decided not to worry about it for the moment.
*     We have two options for doing the fit in this situation:
*     - Realise that some bolometers have bad coordinates at this time slice and
*     just use bolometer coordinates instead. This should be acceptable in the
*     single subarray case but not in the multi-subarray case.
*     - For multiple subarrays recalculate the astrometry using smf_tslice_ast
*     and astTranGrid. This will be slow but it may be that we only need to do
*     it for a few time slices that are off the edge of the map.

*  History:
*     2010-05-13 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
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

/* GSL includes */
#include "gsl/gsl_multifit.h"

/* SMURF */
#include "smf.h"
#include "smf_typ.h"
#include "mers.h"

/* Starlink */
#include "sae_par.h"
#include "prm_par.h"

void
smf_subtract_plane3( ThrWorkForce *wf, smfData *data,
                     const dim_t mdims[],
                     const int lut[], int * status ) {

  dim_t bstride = 0;        /* Bolometer stride */
  double * boldata = NULL;     /* Bolometer data */
  dim_t i;
  gsl_matrix *mapxy = NULL;  /* Matrix of input positions */
  gsl_matrix *mcov = NULL;  /* Covariance matrix */
  dim_t nbolo = 0;           /* Number of bolometers */
  dim_t nfits = 0;          /* Number of fits attempted */
  dim_t ntslice = 0;         /* Number of time slices */
  gsl_vector *planefit = NULL; /* Solution vector */
  gsl_vector *psky = NULL;   /* Vector containing sky brightness */
  smf_qual_t * qua = NULL;/* Relevant quality information */
  dim_t tstride = 0;        /* Time stride */
  gsl_vector *weights = NULL; /* Weights for sky brightness vector */
  gsl_multifit_linear_workspace *work = NULL; /* Workspace */
  double *xpos = NULL;       /* X coordinates of a bolometer */
  double *ypos = NULL;       /* Y coordinates of a bolometer */

  const int ncoeff = 3;  /* */

  if (*status != SAI__OK) return;

  /* ensure we are working with time slices */
  smf_dataOrder( wf, data, 1, status );

  /* Select quality */
  qua = smf_select_qualpntr( data, NULL, status );

  /* get dimension information */
  smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, NULL, &bstride,
                &tstride, status );

  boldata = (data->pntr)[0];

  /* Allocate GSL workspace */
  work = gsl_multifit_linear_alloc( nbolo, ncoeff );
  mapxy = gsl_matrix_alloc( nbolo, ncoeff );
  psky = gsl_vector_alloc( nbolo );
  weights = gsl_vector_alloc( nbolo );
  planefit = gsl_vector_alloc( ncoeff );
  mcov = gsl_matrix_alloc( ncoeff, ncoeff );

  /* get some memory for the lut coordinates so we do not need to calculate
     it multiple times. */
  xpos = astMalloc( nbolo*sizeof(*xpos) );
  ypos = astMalloc( nbolo*sizeof(*ypos) );


  /* We need to loop over each time slice populating the matrices */
  for ( i = 0; i < ntslice; i++ ) {
    dim_t toff = i*tstride;
    dim_t ngood = 0;
    double chisq = 0.0;
    dim_t bol;

    for ( bol = 0; bol < nbolo; bol++) {
      dim_t offset = toff + bol*bstride;
      double weight = 1.0;
      xpos[bol] = VAL__BADD;
      ypos[bol] = VAL__BADD;

      /* calculate the bolometer position and we ignore bolometers that we do
         not know about */
      if (lut[offset] == VAL__BADI) {
        weight = 0.0;
      } else {
        xpos[bol] = (double)( lut[offset] % mdims[0] );
        ypos[bol] = floor( lut[offset] / mdims[0] );
      }

      /* if the bolometer sample is not good we skip it (use zero weight) */
      if ( (qua && (qua[offset]&SMF__Q_FIT)) || boldata[offset] == VAL__BADD ) {
        weight = 0.0;
      }

      /* Fill in the matrices */
      gsl_matrix_set( mapxy, bol, 0, 1.0 );
      gsl_matrix_set( mapxy, bol, 1, ypos[bol] );
      gsl_matrix_set( mapxy, bol, 2, xpos[bol] );

      gsl_vector_set( psky, bol, boldata[offset] );
      gsl_vector_set( weights, bol, weight);

      /* keep track of how many good coordinates we have */
      if (weight > 0.0) ngood++;
    }

    /* do the fit if we have enough points */
    if (ngood > 100 ) {
      double sky0 = 0.0;
      double dskyy = 0.0;
      double dskyx = 0.0;

      nfits++;
      gsl_multifit_wlinear( mapxy, weights, psky, planefit, mcov, &chisq, work );

      sky0 = gsl_vector_get( planefit, 0 );
      dskyy = gsl_vector_get( planefit, 1 );
      dskyx = gsl_vector_get( planefit, 2 );

      /* Subtract the fit */
      for ( bol = 0; bol < nbolo; bol++) {
        dim_t offset = toff + bol*bstride;

        if ( ( !qua || (qua && !(qua[offset] & SMF__Q_MOD)) ) && boldata[offset] != VAL__BADD ) {
          double sky = sky0 + dskyy * ypos[bol] + dskyx * xpos[bol];
          boldata[offset] -= sky;
        }
      }

    }
  }

  msgOutiff( MSG__VERB, "", "    Calculated a plane for %zu time slices",
             status, nfits );

  /* Free up GSL workspace */
  gsl_multifit_linear_free( work );
  gsl_matrix_free( mapxy );
  gsl_vector_free( psky );
  gsl_vector_free( weights );
  gsl_vector_free( planefit );
  gsl_matrix_free( mcov );

  xpos = astFree( xpos );
  ypos = astFree( ypos );

}
