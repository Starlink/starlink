/*
*+
*  Name:
*     smf_find_median

*  Purpose:
*     Find the median in a data array.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     dim_t *smf_find_median( const float *farray, const double *darray, dim_t nel,
*                             dim_t *hist, float *median, int *status )

*  Arguments:
*     farray = const float * (Given)
*        Pointer to an array of single precision data for which the
*        median is to be returned. Only one of "farray" and "darray"
*        should be non-NULL.
*     darray = const float * (Given)
*        Pointer to an array of double precision data for which the
*        median is to be returned. Only one of "farray" and "darray"
*        should be non-NULL.
*     nel = dim_t (Given)
*        The number of elements in farray or darray (which ever is
*        supplied).
*     hist = dim_t * (Given & Returned)
*        Pointer to an array to use for the histogram. If NULL is
*        supplied, a new array is allocated.
*     median = float * (Returned)
*        Pointer to a float in which to returned the median value in
*        farray or darray (which ever is supplied). Will be set to VAL__BADR
*        on error.
*     status = int * (Given and Returned)
*        Inherited status value.

*  Returned Value:
*     A pointer to the histogram array that was used. This will be a copy
*     of "hist" if "hist" was supplied non-NULL, or a pointer to a new array
*     if "hist" was supplied NULL. This pointer can be passed to subsequent
*     calls to this function as the "hist" argument. It should be freed
*     using astFree when it is no longer needed.

*  Description:
*     This function finds the median in a given array. It uses a histogram
*     approach rather than a sorting approach for speed.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     23-APR-2008 (DSB):
*        Initial version.
*     10-JUL-2008 (TIMJ):
*        - Fix bug and optimize when nel == 1
*        - Report error if result can not be allocated
*        - Use dim_t in API
*     21-NOV-2008 (TIMJ):
*        Sort data if the number of data points are below a threshold.
*        Do this since it is more accurate than histogram techniques
*        but is too slow for large data arrays.
*     15-JAN-2009 (DSB):
*        Avoid changing the supplied array when sorting is used.
*     15-JAN-2009 (TIMJ):
*        Declare const arguments
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008, 2009 Science & Technology Facilities Council.
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
#include "sae_par.h"
#include "prm_par.h"
#include "mers.h"
#include "star/kaplibs.h"
#include "smf.h"
#include "smf_err.h"

dim_t *smf_find_median( const float *farray, const double *darray, dim_t nel,
                        dim_t *hist, float *median, int *status ){

  const dim_t threshold = 10000;  /* point at which we abandon sorting */

/* Local Variables */
   double *tdarray;
   double dmean;
   double dmedian;
   double dmode;
   double dsum;
   double valmax;
   double valmin;
   float *tfarray;
   float fvalmax;
   float fvalmin;
   dim_t *result;
   int numbin;

/* pre-fill */
   *median = VAL__BADR;

/* Check the inherited status */
   if( *status != SAI__OK ) return hist;

/* Sanity check */
   if (!farray && !darray) {
     *status = SAI__ERROR;
     errRep(" ", "smf_find_median called with both darray and farray NULL"
            " (possible programming error)", status);
     return NULL;
   }

/* Special case a single bin */
   if (nel == 1) {
     if (farray) {
       *median = farray[0];
     } else {
       if (darray[0] != VAL__BADD) *median = darray[0];
     }
     return hist;
   }

/* See if we should use sort to find the median. If so, take a copy of
   the supplied data first to avoid re-ordering the supplied array */
   if (nel <= threshold) {
     int neluse;
     if ( farray ) {
       tfarray = astStore( NULL, farray, nel*sizeof( *tfarray ) );
       kpg1Medur( 1, (int) nel, tfarray, median, &neluse, status );
       tfarray = astFree( tfarray );

     } else {
       tdarray = astStore( NULL, darray, nel*sizeof( *tdarray ) );
       kpg1Medud( 1, (int) nel, tdarray, &dmedian, &neluse, status );
       tdarray = astFree( tdarray );
       *median = ( dmedian != VAL__BADD ) ? dmedian : VAL__BADR;

     }
     return NULL;
   }

/* Decide on the number of bins in the histogram. This is chosen so that
   each bin has an average population of 2 points. */
   if( nel < VAL__MAXI ){
      numbin = (int) nel/2;
   } else {
      numbin = VAL__MAXI/2;
   }

/* Ensure we have an array at least this big. */
   result = astGrow( hist, numbin, sizeof( *result ) );
   if( result ) {

/* First deal with single precision data... */
      if( farray ) {

/* Initialise the max and min values in the data to bad so that kpg1Ghstx
   will find them itself. */
         fvalmax = VAL__BADR;
         fvalmin = VAL__BADR;

/* Form the histogram. */
         kpg1Ghst8r( 1, nel, farray, NULL, 0.0, numbin, 0, &fvalmax, &fvalmin, result,
                     status );

/* Convert the max and min data value to double precision. */
         valmax = ( fvalmax != VAL__BADR ) ? fvalmax : VAL__BADD;
         valmin = ( fvalmin != VAL__BADR ) ? fvalmin : VAL__BADD;

/* Now deal with double precision data. */
      } else {

/* Initialise the max and min values in the data to bad so that kpg1Ghstx
   will find them itself. */
         valmax = VAL__BADD;
         valmin = VAL__BADD;

/* Form the histogram. */
         kpg1Ghst8d( 1, nel, darray, NULL, 0.0, numbin, 0, &valmax, &valmin, result,
                     status );
      }

/* An error will have been reported by kpg1Ghstr if all values in the
   array are equal. Annul this error and use the constant value as the
   returned median value. */
      if( *status == SAI__ERROR ) {
         errAnnul( status );
         dmedian = valmax;

/* Otherwise, find the median value in the histogram. */
      } else {
         kpg1Hsstp8( numbin, result, valmax, valmin, &dsum, &dmean, &dmedian,
                     &dmode, status );
      }

/* Convert the median value to single precision. */
      *median = ( dmedian != VAL__BADD ) ? dmedian : VAL__BADR;
   } else {
       *status = SMF__NOMEM;
       msgSetk( "NB", numbin);
       errRep(" ", "smf_find_median unable to allocate memory for histogram"
              " of ^NB bins", status);
   }

/* Return a pointer to the histogram array. */
   return result;
}

