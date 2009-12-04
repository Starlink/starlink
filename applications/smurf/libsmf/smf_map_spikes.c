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
*     smf_map_spikes( smfData *data, smfData *variance, int *lut, 
*                     unsigned char *qual, unsigned char mask, 
*                     double *map, int *hitsmap, double *mapvar,
*                     dim_t msize, double thresh, size_t *nflagged,
*                     int *status )

*  Arguments:
*     data = smfData* (Given)
*        Pointer to data stream to be flagged
*     variance = smfData* (Given) Pointer to smfData containing
*        variance. Can be either 2d (one value for each bolo), or 3d
*        (time-varying for each bolo). In the former case ndims should
*        still be 3, but the length of the time dimension should be 0
*        (e.g. a NOI model component created by smf_model_create).
*     lut = int* (Given)
*        1-d LUT for indices of data points in map (same dimensions as data)
*     qual = usigned char* (Given and Returned)
*        Quality array with same dimensions of data for setting new
*        SMF__Q_SPIKE flags when outliers encountered.
*     mask = unsigned char (Given)
*        Define which bits in quality are relevant to ignore data in
*        the calculation.
*     map = double* (Given)
*        The current map estimate 
*     hitsmap = int* (Given)
*        Number of samples that land in a pixel.
*     mapvar = double* (Given)
*        Variance of each pixel in map 
*     msize = dim_t (Given)
*        Number of pixels in map
*     thresh = doublge (Given)
*        N-sigma threshold for spike detection
*     nflagged = size_t * (Returned)
*        The number of new samples that were flagged. May be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine flags data points that are thresh-sigma away from the value
*     of the map in each pixel.
*     
*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2009-08-06 (EC):
*        Initial version.

*  Notes:

*  Copyright:
*     Copyright (C) 2009 University of British Columbia
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

void smf_map_spikes( smfData *data, smfData *variance, int *lut,
                     unsigned char *qual, unsigned char mask, double *map, 
                     int *hitsmap, double *mapvar,
                     dim_t msize __attribute__((unused)), double thresh,
                     size_t *nflagged, int *status ) {

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
  double threshsq;           /* square of thresh */
  double *var=NULL;          /* Pointer to variance array */
  size_t vbstride;           /* bolo stride of variance */
  dim_t vi;                  /* variance array index */
  dim_t vnbolo;              /* number of bolos in variance */
  dim_t vntslice;            /* number of bolos in variance */
  size_t vtstride;           /* tstride of variance */
  

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Check inputs */
  if( !data || !variance || !lut || !qual || !map || !mapvar || !hitsmap ) {
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

      /* Check that the LUT, data and variance values are valid */
      if( (lut[di] != VAL__BADI) && !(qual[di]&mask) && (var[vi] != 0) ) {

        if( (dat[di]-map[lut[di]])*(dat[di]-map[lut[di]]) > threshsq*var[i] ) {
          qual[di] |= SMF__Q_SPIKE;
          nflag++;
        }
      }    
    }
  }

  /* Return nflagged if requested */
  if( nflagged ) *nflagged = nflag;
}
