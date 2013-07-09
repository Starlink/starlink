#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "kaplibs.h"
#include <math.h>

void kpg1Fit1d( int lbnd, int ubnd, const double y[], const double x[],
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     8-JAN-2010 (DSB):
*        Original version (transliterated form kpg1_fit1d.f).
*     30-SEP-2010 (DSB):
*        Do a single 3 sigma clip.
*     1-OCT-2010 (DSB):
*        - Re-form the expressions for *rms to avoid heavy rounding errors.
*        - Handle perfect straight lines.
*     5-JUL-2013 (DSB):
*        Account for rounding errors when checking for zero values.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/

/* Local Variables: */
   double d1,d2,d3,d4,d5; /* Various difference values */
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
     if( *status == SAI__OK ) {
       *status = SAI__ERROR;
       errRepf( " ", "kpg1Fit1d: Out of %d data values supplied none were good", status, nel );
     }
     return;

   } else if( n == 1 ) {
     if( *status == SAI__OK ) {
       *status = SAI__ERROR;
       errRepf( " ", "kpg1Fit1d: Out of %d data values supplied only 1 was good", status, nel );
     }
     return;
   }

/* Form the requied differences, checking for values that are so small
   that they are effectively zero. */
   denom =  n*sxx - sx*sx;
   if( fabs( denom ) <= 10*VAL__EPSD*(n*sxx + sx*sx) ) denom = 0.0;
   d1 =  n*sxy - sx*sy;
   if( fabs( d1 ) <= 10*VAL__EPSD*(n*sxy + sx*sy) ) d1 = 0.0;
   d2 = sxx*sy - sx*sxy;
   if( fabs( d2 ) <= 10*VAL__EPSD*( sxx*sy + sx*sxy ) ) d2 = 0.0;
   d3 = syy - sy*sy/n;
   if( fabs( d3 ) <= 10*VAL__EPSD*( syy + sy*sy/n ) ) d3 = 0.0;
   d4 = sxy - sx*sy/n;
   if( fabs( d4 ) <= 10*VAL__EPSD*( sxy + sx*sy/n ) ) d4 = 0.0;
   d5 =  d3 - (*m)*d4;
   if( fabs( d5 ) <= 10*VAL__EPSD*( d3 + (*m)*d4 ) ) d5 = 0.0;

/* Report an error if the denominator is zero. */
   if( denom == 0 ) {
     if( *status == SAI__OK ) {
       *status = SAI__ERROR;
       errRepf( " ", "kpg1Fit1d: All %d supplied X values are equal", status, nel );
     }
     return;
   }

/* Form the gradient. */
   if( *status == SAI__OK ) {
      *m =  d1/denom;

/* Form the intercept. */
      *c =  d2/denom;

/* Form the RMS residual. */
      *rms = d5/n;

/* Do not reject any points if the RMS is zero (e.g. if the supplied data
   is a perfect straight line). */
      if( *rms <= 0.0 ) {
         *rms = 0.0;

/* If the data is not a perfect straight line, rejected 3.sigma outliers
   and recalculate the fit. */
      } else {
         *rms = sqrt( *rms );

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
           if( *status == SAI__OK ) {
             *status = SAI__ERROR;
             errRep( " ", "kpg1Fit1d: No good data values supplied after sigma clipping", status );
           }
         } else if( n == 1 ) {
           if( *status == SAI__OK ) {
             *status = SAI__ERROR;
             errRep( " ", "kpg1Fit1d: Only 1 good data value found afte sigma clipping", status );
           }
         }

/* Form the denominator used to calculate the returned values. */
         denom =  n*sxx - sx*sx;

/* Report an error if the denominator is zero. */
         if( denom == 0 ) {
           if( *status == SAI__OK ) {
             *status = SAI__ERROR;
             errRep( " ", "kpg1Fit1d: All supplied X values are equal after sigma clipping", status );
           }
         }

/* Form the gradient. */
         if( *status == SAI__OK ) {
            *m =  ( n*sxy - sx*sy )/denom;

/* Form the intercept. */
            *c =  ( sxx*sy - sx*sxy ) /denom;

/* Form the RMS residual. */
            *rms = (syy - sy*sy/n - (*m)*(sxy - sx*sy/n))/n;
            if( *rms <= 0.0 ) {
               *rms = 0.0;
            } else {
               *rms = sqrt( *rms );
            }
         }
      }
   }
}
