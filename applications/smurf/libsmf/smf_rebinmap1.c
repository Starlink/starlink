/*
*+
*  Name:
*     smf_rebinmap1

*  Purpose:
*     Accumulate data directly into a map using a LUT

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_rebinmap1( smfData *data, smfData *variance, int *lut,
*                    unsigned char *qual, unsigned char mask,
*                    int sampvar, int flags, double *map,
*                    double *mapweight, unsigned int *hitsmap,
*                    double *mapvar, dim_t msize, double *scalevariance,
*                    int *status )

*  Arguments:
*     data = smfData* (Given)
*        Pointer to data stream to be re-gridded
*     variance = smfData* (Given)
*        Pointer to smfData containing variance (ignore if NULL pointer). Can
*        be either 2d (one value for each bolo), or 3d (time-varying for
*        each bolo). In the former case ndims should still be 3, but the
*        length of the time dimension should be 0 (e.g. a NOI model component
*        created by smf_model_create).
*     lut = int* (Given)
*        1-d LUT for indices of data points in map (same dimensions as data)
*     qual = usigned char* (Given)
*        If specified, use this QUALITY array to decide which samples
*        to use (provided mask). Otherwise data are only ignored if set
*        to VAL__BADD. Same dimensions as data.
*     mask = unsigned char (Given)
*        Use with qual to define which bits in quality are relevant to
*        ignore data in the calculation.
*     int sampvar (Given)
*        If set, calculate mapvar from the (weighted) sample variance of data
*        that land in the pixel. Otherwise a theoretical variance is calculated
*        by propagating the variance on each sample into the pixel.
*     int flags (Given)
*        Flags to control the rebinning process (see astRebin flags)
*     map = double* (Returned)
*        The output map array
*     mapweight = double* (Returned)
*        Relative weighting for each pixel in map.
*     hitsmap = unsigned int* (Returned)
*        Number of samples that land in a pixel.
*     mapvar = double* (Returned)
*        Variance of each pixel in map
*     msize = dim_t (Given)
*        Number of pixels in map
*     scalevariance = double* (Returned)
*        If sampvar set, calculate average scale factor to be applied
*        to input variances such that error propagation would give the
*        same variance as that calculated from the sample scatter in
*        each pixel.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function does a simple regridding of data into a map. If a
*     variance array is supplied it is used to calculate weights. Optionally
*     return a hitsmap (number of samples that land in a pixel).
*
*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-05-17 (EC):
*        Initial version.
*     2006-08-16 (EC):
*        Rebin the case that no variance array is given
*     2008-01-22 (EC):
*        Added hitsmap calculation
*     2008-04-03 (EC):
*        - Added QUALITY to interface
*     2008-04-23 (EC):
*        Added sample variance calculation
*     2008-04-29 (EC):
*        Flag map/weight/variance pixels with < SMF__MINSTATSAMP hits as bad
*     2008-07-03 (EC):
*        Use dim_t for dsize/msize
*     2009-07-30 (EC):
*        Use smfDatas for data & variance in preparation for 2d variance arrays
*     {enter_further_changes_here}

*  Notes:
*     If the variance map is calculated from the scatter of data in each pixel,
*     rather than using the Gaussian error propagation formula, the expression
*     used is:
*                  sigma^2 =  sum(w_i)*sum(w_i*x_i^2) - [sum(w_i*x_i)]^2
*                             ------------------------------------------
*                                          N*[sum(w_i)]^2
*
*     Where  sigma^2 = estimated variance on the mean in this pixel
*                w_i = i'th sample weight (1/variance if supplied, 1 otherwise)
*                x_i = i'th data sample
*                  N = number of samples in this pixel

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2006-2009 University of British Columbia
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

#define FUNC_NAME "smf_rebinmap1"

void smf_rebinmap1( smfData *data, smfData *variance, int *lut,
                    unsigned char *qual, unsigned char mask,
                    int sampvar, int flags, double *map,
                    double *mapweight, unsigned int *hitsmap,
                    double *mapvar, dim_t msize, double *scalevariance,
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
  dim_t ntslice;             /* number of time slices */
  double scalevar;           /* variance scale factor */
  double scaleweight;        /* weights for calculating scalevar */
  double thisweight;         /* The weight at this point */
  double *var=NULL;          /* Pointer to variance array */
  size_t vbstride;           /* bolo stride of variance */
  dim_t vi;                  /* variance array index */
  dim_t vnbolo;              /* number of bolos in variance */
  dim_t vntslice;            /* number of bolos in variance */
  size_t vtstride;           /* tstride of variance */


  /* Main routine */
  if (*status != SAI__OK) return;

  /* Check inputs */
  if( !data || !map || !lut || !mapweight || !mapvar || !hitsmap ) {
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME ": Null inputs", status );
    return;
  }

  if( !data->pntr[0] ) {
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME ": supplied data is empty", status );
    return;
  }

  dat = data->pntr[0];
  smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, &dsize, &dbstride,
                &dtstride, status );

  if( variance ) {
    var = variance->pntr[0];
    smf_get_dims( variance, NULL, NULL, &vnbolo, &vntslice, NULL, &vbstride,
                  &vtstride, status );

    /* Check that the variance dimensions are compatible with data */
    if( (vnbolo != nbolo) || ( (vntslice>1) && (vntslice!=ntslice) ) ) {
      *status = SAI__ERROR;
      errRep(" ", FUNC_NAME ": variance dimensions incompatible with data",
             status );
      return;
    }
  }

  /* If this is the first data to be accumulated zero the arrays */
  if( flags & AST__REBININIT ) {
    memset( map, 0, msize*sizeof(*map) );
    memset( mapweight, 0, msize*sizeof(*mapweight) );
    memset( mapvar, 0, msize*sizeof(*mapvar) );
    memset( hitsmap, 0, msize*sizeof(*hitsmap) );
  }

  if( var ) {
    /* Accumulate data and weights in the case that variances are given*/

    if( sampvar ) {
      /* Measure weighted sample variance for varmap */
      if( qual ) {       /* QUALITY checking version */
	for( i=0; i<nbolo; i++ ) {
          if( !(qual[i*dbstride]&SMF__Q_BADB) ) for( j=0; j<ntslice; j++ ) {

            di = i*dbstride + j*dtstride;
            vi = i*vbstride + (j%vntslice)*vtstride;

            /* Check that the LUT, data and variance values are valid */
            if( (lut[di] != VAL__BADI) && !(qual[di]&mask) && (var[vi] != 0) ){
              thisweight = 1/var[vi];
              map[lut[di]] += thisweight*dat[di];
              mapweight[lut[di]] += thisweight;
              hitsmap[lut[di]] ++;

              /* Calculate this sum to estimate E(x^2) */
              mapvar[lut[di]] += thisweight*dat[di]*dat[di];
            }
          }
	}
      } else {           /* VAL__BADD checking version */
	for( i=0; i<nbolo; i++ ) {
          for( j=0; j<ntslice; j++ ) {

            di = i*dbstride + j*dtstride;
            vi = i*vbstride + (j%vntslice)*vtstride;

            /* Check that the LUT, data and variance values are valid */
            if( (lut[di] != VAL__BADI) && (dat[di] != VAL__BADD) &&
                (var[vi] != VAL__BADD) && (var[vi] != 0) ) {

              thisweight = 1/var[vi];
              map[lut[di]] += thisweight*dat[di];
              mapweight[lut[di]] += thisweight;
              hitsmap[lut[di]] ++;

              /* Calculate this sum to estimate E(x^2) */
              mapvar[lut[di]] += thisweight*dat[di]*dat[di];
            }
          }
	}
      }

    } else {
      /* Otherwise use simple error propagation for varmap */

      if( qual ) {       /* QUALITY checking version */
	for( i=0; i<nbolo; i++ ) {
          if( !(qual[i*dbstride]&SMF__Q_BADB) ) for( j=0; j<ntslice; j++ ) {

            di = i*dbstride + j*dtstride;
            vi = i*vbstride + (j%vntslice)*vtstride;

            /* Check that the LUT, data and variance values are valid */
            if( (lut[di] != VAL__BADI) && !(qual[di]&mask) && (var[vi] != 0) ){
              thisweight = 1/var[vi];
              map[lut[di]] += thisweight*dat[di];
              mapweight[lut[di]] += thisweight;
              hitsmap[lut[di]] ++;
            }
          }
        }
      } else {           /* VAL__BADD checking version */
	for( i=0; i<nbolo; i++ ) {
          for( j=0; j<ntslice; j++ ) {

            di = i*dbstride + j*dtstride;
            vi = i*vbstride + (j%vntslice)*vtstride;

            /* Check that the LUT, data and variance values are valid */
            if( (lut[di] != VAL__BADI) && (dat[di] != VAL__BADD) &&
                (var[vi] != VAL__BADD) && (var[vi] != 0) ) {
              thisweight = 1/var[vi];
              map[lut[di]] += thisweight*dat[di];
              mapweight[lut[di]] += thisweight;
              hitsmap[lut[di]] ++;
            }
          }
	}
      }
    }
  } else {
    /* Accumulate data and weights when no variances are given. In this case
       the variance map is always estimated from the sample variance */

    if( qual ) {       /* QUALITY checking version */
      for( i=0; i<nbolo; i++ ) {
        if( !(qual[i*dbstride]&SMF__Q_BADB) ) for( j=0; j<ntslice; j++ ) {

          di = i*dbstride + j*dtstride;

          /* Check that the LUT, data and variance values are valid */
          if( (lut[di] != VAL__BADI) && !(qual[di]&mask) ) {
            map[lut[di]] += dat[di];
            mapweight[lut[di]] ++;
            hitsmap[lut[di]] ++;

            /* Calculate this sum to estimate E(x^2) */
            mapvar[lut[di]] += dat[di]*dat[di];
          }
        }
      }
    } else {           /* VAL__BADD checking version */
      for( i=0; i<dsize; i++ ) {
	/* Check that the LUT, data and variance values are valid */
	if( (lut[i] != VAL__BADI) && (dat[i] != VAL__BADD) ) {
	  map[lut[i]] += dat[i];
	  mapweight[lut[i]] ++;
	  hitsmap[lut[i]] ++;

	  /* Calculate this sum to estimate E(x^2) */
	  mapvar[lut[i]] += dat[i]*dat[i];
	}
      }
    }
  }

  /* If this is the last data to be accumulated re-normalize */
  if( flags & AST__REBINEND ) {

    if( sampvar || !var ) {
      /* Variance also needs re-normalization in sampvar case */

      scaleweight=0;
      scalevar=0;

      for( i=0; i<msize; i++ ) {
        if( !mapweight[i] ) {
          /* If 0 weight set pixels to bad */
          mapweight[i] = VAL__BADD;
          map[i] = VAL__BADD;
          mapvar[i] = VAL__BADD;
	} else {
	  /* Otherwise re-normalize */
	  thisweight = 1/mapweight[i];
	  map[i] *= thisweight;

          /* variance only reliable if we had enough samples */
          if( hitsmap[i] >= SMF__MINSTATSAMP ) {
            mapvar[i] = (mapvar[i]*thisweight - map[i]*map[i])/hitsmap[i];

            /* Work out average scale factor so that supplied weights
               would produce the same map variance estimate as the
               sample variance calculation that we just did. The average
               value is weighted by number of hits in the pixel to weight
               well-sampled pixels more heavily */

            scalevar += hitsmap[i]*mapvar[i]*mapweight[i];
            scaleweight += hitsmap[i];
          } else {
            mapvar[i] = VAL__BADD;
          }
	}
      }

      /* Re-normalize scalevar */
      if( scaleweight ) {
        scalevar /= scaleweight;

        if( scalevariance ) {
          *scalevariance = scalevar;
        }
      }

    } else {
      /* Re-normalization for error propagation case */

      for( i=0; i<msize; i++ ) {
	if( !mapweight[i] ) {
	  /* If 0 weight set pixels to bad */
	  mapweight[i] = VAL__BADD;
	  map[i] = VAL__BADD;
	  mapvar[i] = VAL__BADD;
	} else {
	  /* Otherwise re-normalize */
	  mapvar[i] = 1/mapweight[i];
	  map[i] *= mapvar[i];

          /* variance only reliable if we had enough samples */
          if( hitsmap[i] < SMF__MINSTATSAMP ) {
            mapvar[i] = VAL__BADD;
          }
	}
      }
    }
  }
}
