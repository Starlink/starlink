#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include <math.h>

void kpg1Fit1d( int lbnd, int ubnd, const double *y, const double *x,
                double *m, double *c, double *rms, int *status ){
/*
*  Name:
*     kpg1Fit1d

*  Purpose:
*     Fits a least-squares straight line to supplied data.

*  Language:
*     C.

*  Invocation:
*     void kpg1Fit1d( int lbnd, int ubnd, const double *y, const double *x,
*                     double *m, double *c, double *rms, int *status )

*  Description:
*     A straight line is fitted to the data supplied in X and Y, using
*     the least-squares criterion. Data points lying further than three
*     standard deviations from the the fitted line are rejected and the
*     gradient and intercept are updated to exclude the rejected points.
*     The returned values of M and C are the gradient and intercept of
*     the fit, so that y = M.x + C. The RMS residual of the Y data from
*     the fit, excluding rejected points, is returned in RMS.
*
*     An error is reported if there are less than two good data values
*     in Y, or if the X values cover a range of zero.

*  Arguments:
*     lbnd
*        The lower bound of the X and Y arrays.
*     ubnd
*        The upper bound of the X and Y arrays.
*     y
*        The Y data values. Any bad values are ignored.
*     x
*        The X positions corresponding to each Y data value.
*     m
*        Pointer to a double in which to return the gradient.
*     c
*        Pointer to a double in which to return the intercept.
*     rms
*        Pointer to a double in which to return the RMS residual
*        between the Y values and the fit.
*     status
*        The inherited status.

*  Notes:
*     - The "lbnd" and "ubnd" arguments serve only to determine the
*     number of elements in the supplied "x" and "y" arrays.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     8-JAN-2010 (DSB):
*        Original version (transliterated form kpg1_fit1d.f).
*     30-SEP-2010 (DSB):
*        Do a single 3 sigma clip.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/

/* Local Variables: */
   double denom;     /* Denominator */
   double sx;        /* Sum of X values */
   double sxx;       /* Sum of X squared values */
   double sxy;       /* Sum of X*Y values */
   double sy;        /* Sum of Y values */
   double syy;       /* Sum of Y squared values */
   double fitval;    /* The value of the fitted line */
   double thresh;    /* Rejection threshold */
   double xval;      /* X value */
   double yval;      /* Y value */
   int i;            /* Loop count */
   int n;            /* No. of points in sums */
   int nel;          /* No. of elements in array */

/* Initialise */
   *m = VAL__BADD;
   *c = VAL__BADD;
   *rms = VAL__BADD;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise the running sums to zero. */
   sy = 0.0;
   sx = 0.0;
   sxy = 0.0;
   sxx = 0.0;
   syy = 0.0;
   n = 0;

/* Loop round finding the sums of all the necessary terms. */
   nel = ubnd - lbnd + 1;
   for( i = 0; i < nel; i++ ) {
      xval = x[ i ];
      yval = y[ i ];
      if( xval != VAL__BADD && yval != VAL__BADD ) {
         sy += yval;
         sx += xval;
         sxy += xval*yval;
         sxx += xval*xval;
         syy += yval*yval;
         n++;
      }
   }

/* Report an error if there are less than 2 good data values. */
   if( n == 0 ) {
      *status = SAI__ERROR;
      errRep( " ", "kpg1Fit1d: No good data values suupplied", status );

   } else if( n == 1 ) {
      *status = SAI__ERROR;
      errRep( " ", "kpg1Fit1d: Only 1 good data value found", status );
   }

/* Form the denominator used to calculate the returned values. */
   denom =  n*sxx - sx*sx;

/* Report an error if the denominator is zero. */
   if( denom == 0 ) {
      *status = SAI__ERROR;
      errRep( " ", "kpg1Fit1d: All supplied X values are equal", status );
   }

/* Form the gradient. */
   if( *status == SAI__OK ) {
      *m =  ( n*sxy - sx*sy )/denom;

/* Form the intercept. */
      *c =  ( sxx*sy - sx*sxy ) /denom;

/* Form the RMS residual. */
      *rms =  ( syy + ( 2.0*sx*sy*sxy - sy*sy*sxx - sxy*sxy*n )/denom)/n;
      if( *rms <= 0.0 ) {
         *rms = 0.0;
      } else {
         *rms = sqrt( *rms );
      }

/* Get the rejection threshold. */
      thresh = 3.0*( *rms );

/* For each point that is tto far from the line, remove the point from
   the running sums */
      for( i = 0; i < nel; i++ ) {
         xval = x[ i ];
         yval = y[ i ];
         if( xval != VAL__BADD && yval != VAL__BADD ) {
            fitval = ( *m )*xval + ( *c );
            if( fabs( yval - fitval ) > thresh ) {
               sy -= yval;
               sx -= xval;
               sxy -= xval*yval;
               sxx -= xval*xval;
               syy -= yval*yval;
               n--;
            }
         }
      }

/* Report an error if there are less than 2 good data values. */
      if( n == 0 ) {
         *status = SAI__ERROR;
         errRep( " ", "kpg1Fit1d: No good data values suupplied", status );

      } else if( n == 1 ) {
         *status = SAI__ERROR;
         errRep( " ", "kpg1Fit1d: Only 1 good data value found", status );
      }

/* Form the denominator used to calculate the returned values. */
      denom =  n*sxx - sx*sx;

/* Report an error if the denominator is zero. */
      if( denom == 0 ) {
         *status = SAI__ERROR;
         errRep( " ", "kpg1Fit1d: All supplied X values are equal", status );
      }

/* Form the gradient. */
      if( *status == SAI__OK ) {
         *m =  ( n*sxy - sx*sy )/denom;

/* Form the intercept. */
         *c =  ( sxx*sy - sx*sxy ) /denom;

/* Form the RMS residual. */
         *rms =  ( syy + ( 2.0*sx*sy*sxy - sy*sy*sxx - sxy*sxy*n )/denom)/n;
         if( *rms <= 0.0 ) {
            *rms = 0.0;
         } else {
            *rms = sqrt( *rms );
         }
      }
   }
}
