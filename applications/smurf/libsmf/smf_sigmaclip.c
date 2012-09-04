/*
*+
*  Name:
*     smf_sigmaclip

*  Purpose:
*     Find the weighted mean of a supplied set of values, using N-sigma
*     clipping to reject outliers.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     double smf_sigmaclip( int nval, double *val, double *wgt,
*                           double nsigma, int niter, double *stddev,
*                           int *status )

*  Arguments:
*     nval = int (Given)
*        The number of points supplied.
*     val = double * (Given)
*        Pointer to the array of "nval" values.
*     wgt = double * (Given)
*        Pointer to the array of "nval" weights. May be NULL, in which
*        all weights are assumed to be 1.0.
*     nsigma = double (Given)
*        The number of standard deviations at which to reject values.
*     niter = int (Given)
*        The number of rejection iterations to perform.
*     stddev = double * (Returned)
*        The standard deviation of the clipped data values. May be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned value:
*     The weighted mean of the supplied values, excluding those rejected
*     by the n-sigma clipping algorithm. VAL__BADD is returned if an
*     error has occurred, or if no good data is supplied.

*  Description:
*     The weighted mean and standard deviation of all supplied values is
*     found. Any supplied points which are further than "nsigma" times the
*     standard deviation away from the wieghted mean are rejected. The
*     weighted mean and standard deviation of the remaining input values
*     are found. This process is repeated "niter" times.

*  Notes:
*     - VAL__BADD values in the "val" and "wgt" arrays are handled
*     correctly.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     6-MAY-2012 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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

/* SMURF includes */
#include "libsmf/smf.h"

double smf_sigmaclip( int nval, double *val, double *wgt, double nsigma,
                      int niter, double *stddev, int *status ){

/* Local Variables: */
   double *pw;
   double *pv;
   double hilim;
   double lolim;
   double result;
   double sigma;
   double swv;
   double swvv;
   double sw;
   double wv;
   int iter;
   int ival;

/* Initialise */
   result = VAL__BADD;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Initialise upper and lower limits on acceptable input values. */
   lolim = VAL__MIND;
   hilim = VAL__MAXD;

/* Do the required number of iterations. */
   for( iter = 0; iter < niter; iter++ ) {

/* Initialise running sums. */
      sw = 0.0;
      swv = 0.0;
      swvv = 0.0;

/* If weights are provided... */
      if( wgt ) {

/* Loop round all input values. */
         pv = val;
         pw = wgt;
         for( ival = 0; ival < nval; ival++,pv++,pw++ ) {

/* If the input value and weight are good, and the value is between the
   current acceptable limits, add the input value into the running sums. */
            if( *pv != VAL__BADD && *pw != VAL__BADD &&
                *pv >= lolim && *pv <= hilim ) {
               wv = ( *pv )*( *pw );
               swv += wv;
               swvv += wv*( *pv );
               sw += *pw;
            }
         }

/* If no weights are provided. */
      } else {

/* Loop round all input values. */
         pv = val;
         for( ival = 0; ival < nval; ival++,pv++ ) {

/* If the input value and weight are good, and the value is between the
   current acceptable limits, add the input value into the running sums. */
            if( *pv != VAL__BADD && *pv >= lolim && *pv <= hilim ) {
               swv += *pv;
               swvv += ( *pv )*( *pv );
               sw += 1.0;
            }
         }
      }

/* Find the weighted mean. */
      if( sw != 0.0 ) {
         result = swv/sw;

/* Also find the weighted standard deviation and thus find new acceptable
   data limts for the next iteration. */
         sigma = swvv/sw - result*result;
         sigma = ( sigma > 0.0 ) ? sqrt( sigma ) : 0.0;
         lolim = result - nsigma*sigma;
         hilim = result + nsigma*sigma;
         if( stddev ) *stddev = sigma;

/* Abort, returning a bad value if no good data is found. */
      } else {
         result = VAL__BADD;
         if( stddev ) *stddev = VAL__BADD;
         break;
      }
   }

/* Return the result. */
   return result;
}


