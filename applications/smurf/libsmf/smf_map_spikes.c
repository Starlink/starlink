/*
*+
*  Name:
*     smf_map_spikes

*  Purpose:
*     Flag outlier data points that land in each pixel of the map

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_map_spikes( smfData *data, smfData *variance, double weightnorm,
*                     int *lut, smf_qual_t mask, double *map, int *hitsmap,
*                     double *mapvar, double thresh, size_t *nflagged,
*                     int *status )

*  Arguments:
*     data = smfData* (Given)
*        Pointer to data stream to be flagged. Quality will be flagged
*        with SMF__Q_SPIKE.
*     variance = smfData* (Given) Pointer to smfData containing
*        variance. Can be either 2d (one value for each bolo), or 3d
*        (time-varying for each bolo). In the former case ndims should
*        still be 3, but the length of the time dimension should be 0
*        (e.g. a NOI model component created by smf_model_create).
*     weightnorm = double (Given)
*        Normalization for data weights
*     lut = int* (Given)
*        1-d LUT for indices of data points in map (same dimensions as data)
*     mask = smf_qual_t (Given)
*        Define which bits in quality are relevant to ignore data in
*        the calculation.
*     map = double* (Given)
*        The current map estimate (can be NULL).
*     hitsmap = int* (Given)
*        Number of samples that land in a pixel.
*     mapvar = double* (Given)
*        Variance of each pixel in map
*     thresh = doublge (Given)
*        N-sigma threshold for spike detection
*     nflagged = size_t * (Returned)
*        The number of new samples that were flagged. May be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine flags data points that are thresh-sigma away from
*     the value of the map in each pixel. After running smf_rebinmap1,
*     we have an estimated uncertainty in the weighted mean of each
*     map pixel, sigma_m, and we also know the number of hits,
*     N. Therefore we can estimate the population variance of weighted
*     data points that went into each map pixel as
*
*                   sigma^2 = sigma_m^2 * sqrt(N)
*
*     The population we are considering are weighted data points, where
*     each weight, w_i = norm/var^2, i.e. inverse variance weighting times
*     a normalization, weightnorm, for which we also have an average value
*     calculated by smf_rebinmap1. We simply check to see if the difference
*     between the data point and the mean (stored in the map), multiplied
*     by this weight, exceeds the requested threshold beyond the population
*     variance.
*
*     If map is NULL, it is assumed that the data have already had the
*     map value subtracted, and are assumed to be scattered about zero.

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2009-08-06 (EC):
*        Initial version.
*     2010-09-24 (EC):
*        Finish and get it working properly.

*  Notes:

*  Copyright:
*     Copyright (C) 2009-2010 University of British Columbia
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

#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_map_spikes"

void smf_map_spikes( smfData *data, smfData *variance, double weightnorm,
                     int *lut, smf_qual_t mask, double *map, int *hitsmap,
                     double *mapvar, double thresh, size_t *nflagged,
                     int *status ) {

  /* Local Variables */
  double *dat=NULL;          /* Pointer to data array */
  size_t dbstride;           /* bolo stride of data */
  dim_t di;                  /* data array index */
  dim_t dsize;               /* total number of elements in data */
  size_t dtstride;           /* tstride of data */
  size_t i;                  /* Loop counter */
  size_t j;                  /* Loop counter */
  dim_t nbolo;               /* number of bolos */
  size_t nflag=0;            /* Number of samples flagged */
  dim_t ntslice;             /* number of time slices */
  double popvar;             /* estimate of population variance */
  smf_qual_t * qual = NULL;  /* Quality to update for flagging */
  double thisweight;
  double threshsq;           /* square of thresh */
  double val;                /* weighted offset value */
  double *var=NULL;          /* Pointer to variance array */
  size_t vbstride;           /* bolo stride of variance */
  dim_t vi;                  /* variance array index */
  dim_t vnbolo;              /* number of bolos in variance */
  dim_t vntslice;            /* number of bolos in variance */
  size_t vtstride;           /* tstride of variance */


  /* Main routine */
  if (*status != SAI__OK) return;

  /* Check inputs */
  if( !data || !variance || !lut || !mapvar || !hitsmap ) {
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME ": Null inputs", status );
    return;
  }

  if( (!data->pntr[0]) || (!variance->pntr[0]) ) {
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME ": supplied data or variance is empty", status );
    return;
  }

  dat = data->pntr[0];
  qual = smf_select_qualpntr( data, NULL, status );
  smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, &dsize, &dbstride,
                &dtstride, status );

  var = variance->pntr[0];
  smf_get_dims( variance, NULL, NULL, &vnbolo, &vntslice, NULL, &vbstride,
                &vtstride, status );

  if( thresh <= 0 ) {
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME ": thresh must be > 0!", status );
    return;
  }

  threshsq = thresh*thresh;

  /* Check that the variance dimensions are compatible with data */
  if( (vnbolo != nbolo) || ( (vntslice>1) && (vntslice!=ntslice) ) ) {
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME ": variance dimensions incompatible with data",
           status );
    return;
  }

  /* Loop over data points and flag outliers */
  for( i=0; i<nbolo; i++ ) {

    if( !(qual[i*dbstride]&SMF__Q_BADB) ) for( j=0; j<ntslice; j++ ) {

      di = i*dbstride + j*dtstride;
      vi = i*vbstride + (j%vntslice)*vtstride;

      if( (lut[di] != VAL__BADI) && !(qual[di]&mask) && (var[vi] != 0) &&
        (mapvar[lut[di]] != VAL__BADD) ) {

        /* What is the weighted offset of this data point from the mean */
        thisweight = (1/var[vi])*weightnorm;

        if( map ) val = (dat[di] - map[lut[di]]);
        else val = dat[di];

        val *= thisweight;

        /* What is the estimated population variance? */
        popvar = mapvar[lut[di]]*hitsmap[lut[di]];

        /* Flag it if it's an outlier */
        if( val*val > threshsq*popvar ) {
          qual[di] |= SMF__Q_SPIKE;
          nflag++;
        }
      }
    }
  }

  /* Return nflagged if requested */
  if( nflagged ) *nflagged = nflag;
}
