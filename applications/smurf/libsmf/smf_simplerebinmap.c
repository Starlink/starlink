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
*     smf_simplerebinmap( double *data, double *variance, int *lut, int size, 
*                         double *map, double *mapweight, double *mapvar,
*         	          int flags, int *status );

*  Arguments:
*     data = double* (Given)
*        Pointer to data stream to be re-gridded
*     variance = double* (Given)
*        Pointer to array giving data variance
*     lut = int* (Given)
*        1-d LUT for indices of data points in map
*     dsize = int (Given)
*        Number of elements in data stream
*     int flags (Given)
*        Flags to control the rebinning process (see astRebin flags)
*     map = double* (Returned)
*        The output map array 
*     mapweight = double* (Returned)
*        Relative weighting for each pixel in map
*     mapvar = double* (Returned)
*        Variance of each pixel in map
*     msize = int (Given)
*        Number of pixels in map
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function does a simple regridding of data into a map
*     
*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-05-17 (EC):
*        Initial version.
*     {enter_further_changes_here}

*  Notes:
*

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

void smf_simplerebinmap( double *data, double *variance, int *lut, int dsize, 
			 int flags, double *map, double *mapweight, 
			 double *mapvar, int msize, int *status ) {

  /* Local Variables */
  dim_t i;                   /* Loop counter */
  
  /* Main routine */
  if (*status != SAI__OK) return;

  /* If this is the first data to be accumulated zero the arrays */
  if( (flags & AST__REBININIT) == AST__REBININIT ) { 
    memset( map, 0, msize*sizeof(double) );
    memset( mapweight, 0, msize*sizeof(double) );
    memset( mapvar, 0, msize*sizeof(double) );
  }

  /* Accumulate data and weights */
  for( i=0; i<dsize; i++ ) {
    /* Check that the LUT, data and variance values are valid */
    if( (lut[i] != VAL__BADI) && (data[i] != VAL__BADD) && 
	(variance[i] != VAL__BADD) && (variance[i] != 0) ) {

      map[lut[i]] += data[i]/variance[i];
      mapweight[lut[i]] += 1/variance[i];
    }
  }

  /* If this is the last data to be accumulated re-normalize */
  if( (flags & AST__REBINEND) == AST__REBINEND ) { 
    for( i=0; i<msize; i++ ) {      
      if( mapweight[i] == 0 ) {
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
