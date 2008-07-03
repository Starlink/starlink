/*
*+
*  Name:
*     smf_simplerebinmap

*  Purpose:
*     Accumulate data directly into a map using a LUT

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_simplerebinmap( double *data, double *variance, int *lut, 
*                         unsigned char *qual, unsigned char mask,
*                         dim_t dsize, int sampvar, int flags, double *map,
*                         double *mapweight, unsigned int *hitsmap,
*                         double *mapvar, dim_t msize, int *status ) {

*  Arguments:
*     data = double* (Given)
*        Pointer to data stream to be re-gridded
*     variance = double* (Given)
*        Pointer to array giving data variance (ignore if NULL pointer)
*     lut = int* (Given)
*        1-d LUT for indices of data points in map
*     qual = usigned char* (Given)
*        If specified, use this QUALITY array to decide which samples
*        to use (provided mask). Otherwise data are only ignored if set
*        to VAL__BADD.
*     mask = unsigned char (Given)
*        Use with qual to define which bits in quality are relevant to
*        ignore data in the calculation.
*     dsize = dim_t (Given)
*        Number of elements in data stream
*     int sampvar (Given)
*        If set, calculate mapvar from the (weighted) sample variance of data
*        that land in the pixel. Otherwise a theoretical variance is calculated
*        by propagating the variance on each sample into the pixel. 
*     int flags (Given)
*        Flags to control the rebinning process (see astRebin flags)
*     map = double* (Returned)
*        The output map array 
*     mapweight = double* (Returned)
*        Relative weighting for each pixel in map
*     hitsmap = unsigned int* (Returned)
*        Number of samples that land in a pixel.
*     mapvar = double* (Returned)
*        Variance of each pixel in map 
*     msize = dim_t (Given)
*        Number of pixels in map
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function does a simple regridding of data into a map. If a 
*     variance array is supplied it is used to calculated weights. Optionally
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
*     {enter_further_changes_here}

*  Notes:
*     The "sample variance" method isn't correct. I've fudged an approximate
*     formula for the standard error (squared) when the data are assigned
*     arbitrary weights. Needs to be fixed up. 

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
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

#define FUNC_NAME "smf_simplerebin"

void smf_simplerebinmap( double *data, double *variance, int *lut, 
			 unsigned char *qual, unsigned char mask, dim_t dsize, 
			 int sampvar, int flags, double *map, 
			 double *mapweight, unsigned int *hitsmap, 
			 double *mapvar, dim_t msize, int *status ) {

  /* Local Variables */
  dim_t i;                   /* Loop counter */
  double thisweight;         /* The weight at this point */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Check inputs */
  if( !data || !map || !lut || !mapweight || !mapvar || !hitsmap ) {
    *status = SAI__ERROR;
    errRep("FUNC_NAME", "Null inputs", status ); 
    return;
  }

  /* If this is the first data to be accumulated zero the arrays */
  if( flags & AST__REBININIT ) { 
    memset( map, 0, msize*sizeof(*map) );
    memset( mapweight, 0, msize*sizeof(*mapweight) );
    memset( mapvar, 0, msize*sizeof(*mapvar) );
    memset( hitsmap, 0, msize*sizeof(*hitsmap) );
  }

  if( variance ) {
    /* Accumulate data and weights in the case that variances are given*/

    if( sampvar ) {
      /* Measure weighted sample variance for varmap */
      
      if( qual ) {       /* QUALITY checking version */
	for( i=0; i<dsize; i++ ) {
	  /* Check that the LUT, data and variance values are valid */
	  if( (lut[i] != VAL__BADI) && !(qual[i]&mask) && (variance[i] != 0) ){
	    thisweight = 1/variance[i];
	    map[lut[i]] += thisweight*data[i];
	    mapweight[lut[i]] += thisweight;  	    
	    hitsmap[lut[i]] ++;

	    /* Calculate this sum to estimate E(x^2) */
	    mapvar[lut[i]] += thisweight*data[i]*data[i];
	  }
	}
      } else {           /* VAL__BADD checking version */	
	for( i=0; i<dsize; i++ ) {
	  /* Check that the LUT, data and variance values are valid */
	  if( (lut[i] != VAL__BADI) && (data[i] != VAL__BADD) && 
	      (variance[i] != VAL__BADD) && (variance[i] != 0) ) {        

	    thisweight = 1/variance[i];
	    map[lut[i]] += thisweight*data[i];
	    mapweight[lut[i]] += thisweight;  	    
	    hitsmap[lut[i]] ++;
	    
	    /* Calculate this sum to estimate E(x^2) */
	    mapvar[lut[i]] += thisweight*data[i]*data[i];
	  }
	}
      }

    } else {
      /* Otherwise use simple error propagation for varmap */

      if( qual ) {       /* QUALITY checking version */
	for( i=0; i<dsize; i++ ) {
	  /* Check that the LUT, data and variance values are valid */
	  if( (lut[i] != VAL__BADI) && !(qual[i]&mask) && (variance[i] != 0) ){
	    thisweight = 1/variance[i];
	    map[lut[i]] += thisweight*data[i];
	    mapweight[lut[i]] += thisweight;  	    
	    hitsmap[lut[i]] ++;
	  }
	}
      } else {           /* VAL__BADD checking version */	
	for( i=0; i<dsize; i++ ) {
	  /* Check that the LUT, data and variance values are valid */
	  if( (lut[i] != VAL__BADI) && (data[i] != VAL__BADD) && 
	      (variance[i] != VAL__BADD) && (variance[i] != 0) ) {        
	    thisweight = 1/variance[i];
	    map[lut[i]] += thisweight*data[i];
	    mapweight[lut[i]] += thisweight;  	    
	    hitsmap[lut[i]] ++;
	  }
	}
      }
    }
  } else {
    /* Accumulate data and weights when no variances are given. In this case
       the variance map is always estimated from the sample variance */

    if( qual ) {       /* QUALITY checking version */
      for( i=0; i<dsize; i++ ) {
	/* Check that the LUT, data and variance values are valid */
	if( (lut[i] != VAL__BADI) && !(qual[i]&mask) ) {
	  map[lut[i]] += data[i];
	  mapweight[lut[i]] ++;
	  hitsmap[lut[i]] ++;

	  /* Calculate this sum to estimate E(x^2) */
	  mapvar[lut[i]] += data[i]*data[i];
	}
      }
    } else {           /* VAL__BADD checking version */
      for( i=0; i<dsize; i++ ) {
	/* Check that the LUT, data and variance values are valid */
	if( (lut[i] != VAL__BADI) && (data[i] != VAL__BADD) ) {
	  map[lut[i]] += data[i];
	  mapweight[lut[i]] ++;
	  hitsmap[lut[i]] ++;

	  /* Calculate this sum to estimate E(x^2) */
	  mapvar[lut[i]] += data[i]*data[i];
	}
      }
    }
  }

  /* If this is the last data to be accumulated re-normalize */
  if( flags & AST__REBINEND ) { 

    if( sampvar || !variance ) {
      /* Variance also needs re-normalization in sampvar case */
      
      for( i=0; i<msize; i++ ) {      
	if( !mapweight[i] || (hitsmap[i] < SMF__MINSTATSAMP) ) {
	  /* If 0 weight set pixels to bad */
	  mapweight[i] = VAL__BADD;
	  map[i] = VAL__BADD;
	  mapvar[i] = VAL__BADD;
	} else {
	  /* Otherwise re-normalize */
	  thisweight = 1/mapweight[i];
	  map[i] *= thisweight;
	  mapvar[i] = (mapvar[i]*thisweight - map[i]*map[i])/hitsmap[i];
	}
      }
    } else {
      /* Re-normalization for error propagation case */

      for( i=0; i<msize; i++ ) {      
	if( !mapweight[i] || (hitsmap[i] < SMF__MINSTATSAMP) ) {
	  /* If 0 weight set pixels to bad */
	  mapweight[i] = VAL__BADD;
	  map[i] = VAL__BADD;
	  mapvar[i] = VAL__BADD;
	} else {
	  /* Otherwise re-normalize */
	  mapvar[i] = 1/mapweight[i];
	  map[i] *= mapvar[i];
	}
      }
    }
  }
}
