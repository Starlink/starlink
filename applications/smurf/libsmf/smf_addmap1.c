/*
*+
*  Name:
*     smf_addmap1

*  Purpose:
*     Weighted addition of two maps with the same dimensions.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_addmap1( dim_t contchunk, double *map1, double *mapweight1,
*                  double *boloweight1, int *hitsmap1, double *mapvar1,
*                  smf_qual_t *mapqual1, double *map2, double *boloweight2,
*                  int *hitsmap2, double *mapvar2, smf_qual_t *mapqual2,
*                  dim_t msize, double chunkweight2, int *status ) {

*  Arguments:
*     contchunk = dim_t (Given)
*        The zero-based index of the chunk being added.
*     map1 = double* (Given and Returned)
*        The first map
*     mapweight1 = double* (Given and Returned)
*        The sum of the weights for each pixel in the map. Each of these
*        weights is the reciprocal of a single map variance times the
*        associated chunk weight.
*     boloweight1 = double* (Given and Returned)
*        The sum of the weights of the bolometer values that fall in the
*        pixel. Each of these weights is the reciprocal of the
*        corresponding bolometer variance. The variance of the bolometer
*        values need to be distinguished from the variance of the map pixel
*        values.
*     hitsmap1 = int* (Given and Returned)
*        Number of samples that land in map1 pixels
*     mapvar1 = double* (Given and Returned)
*        Variance of each pixel in map1
*     mapqual1 = smf_qual_t* (Given and Returned)
*        Quality map1
*     map2 = double* (Given)
*        The second map
*     boloweight2 = double* (Given)
*        Sum of bolometer weights for each pixel in map2
*     hitsmap2 = int* (Given)
*        Number of samples that land in map2 pixels
*     mapvar2 = double* (Given)
*        Variance of each pixel in map2
*     mapqual2 = smf_qual_t* (Given)
*        Quality map2
*     msize = dim_t (Given)
*        Number of pixels in the maps
*     chunkweight2 = double (Given)
*        The weight for the new map (map2).
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine adds all of the pixels from map2 to map1 using inverse
*     variance weighting.
*
*  Authors:
*     EC: Edward Chapin (UBC)
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-04-16 (EC):
*        Initial version.
*     2008-07-03 (EC):
*        Use dim_t for msize
*     2010-04-20 (EC):
*        Handle map quality arrays.
*     2011-09-01 (EC):
*        The weightsmap does not necessarily need to be 1/varmap!
*     2013-10-08 (DSB):
*        Renamed arguments "mapweight1/2" to "boloweight1/2", and added
*        new arguments "mapweight1" and "chunkweight2". These changes
*        allow different chunks to be weighted differently in the final
*        coadded map.
*     2013-12-09 (DSB):
*        Added argument "contchunk". Now needed since "map1==map2" is no
*        longer a valid check that the first contchunk is being processed.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008,2010-2011 University of British Columbia.
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

#define FUNC_NAME "smf_addmap1"

void smf_addmap1( dim_t contchunk, double *map1, double *mapweight1, double *boloweight1,
                  int *hitsmap1, double *mapvar1, smf_qual_t *mapqual1,
                  double *map2, double *boloweight2, int *hitsmap2,
                  double *mapvar2, smf_qual_t *mapqual2, dim_t msize,
                  double chunkweight2, int *status ) {

  /* Local Variables */
  dim_t i;                   /* Loop counter */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Check for NULL inputs */
  if( (map1==NULL) || (boloweight1==NULL) || (hitsmap1==NULL) ||
      (mapvar1==NULL) || (map2==NULL) || (boloweight2==NULL) ||
      (hitsmap2==NULL) || (mapvar2==NULL) ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Addmap failed due to NULL inputs.", status);
      return;
  }

  /* If this is the first chunk, just update the mapweight array. */
  if( contchunk == 0 ) {
    for( i=0; i<msize; i++ ) {
       if( mapvar1[ i ] != VAL__BADD ) {
          mapweight1[ i ] = chunkweight2/mapvar1[ i ];
          boloweight1[ i ] *= chunkweight2;
          hitsmap1[ i ] *= chunkweight2;
       }
    }

  /* Otherwise, loop over every pixel and store the weighted values in
     arrays associated with map1 */
  } else {
    for( i=0; i<msize; i++ ) {
      if( (map1[i] == VAL__BADD) || (mapvar1[i] == VAL__BADD) ) {

        /* If bad pixel in map1 just copy map2 regardless */
        map1[i] = map2[i];
        mapweight1[i] = chunkweight2/mapvar2[ i ];
        boloweight1[i] = boloweight2[i];
        hitsmap1[i] = hitsmap2[i];
        mapvar1[i] = mapvar2[i];
        mapqual1[i] = mapqual2[i];

      } else if( (map2[i] != VAL__BADD) && (mapvar2[i] != VAL__BADD) ) {

        /* Add together if both maps have good pixels */
        if( (mapvar1[i]<=0) || (mapvar2[i]<=0) ) {
          *status = SAI__ERROR;
          errRepf("", FUNC_NAME ": invalid variance(s) <=0 detected (%g and %g)", status,
                  mapvar1[i], mapvar2[i]);
          return;

        } else {
          double w1 = mapweight1[ i ];
          double w2 = chunkweight2/mapvar2[ i ];
          double wnew = w1 + w2;
          map1[ i ] = ( map1[i]*w1 + map2[i]*w2 )/wnew;
          mapvar1[ i ] = ( mapvar1[ i ]*w1*w1 + chunkweight2*w2 )/( wnew*wnew );
          boloweight1[ i ] += boloweight2[i]*chunkweight2;
          hitsmap1[ i ] += hitsmap2[i]*chunkweight2;
          mapqual1[ i ] &= mapqual2[i];
          mapweight1[ i ] = wnew;
        }
      }
    }
  }
}
