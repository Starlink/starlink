/*
*+
*  Name:
*     smf_rolling_fit

*  Purpose:
*     Smooth part of a 1D data array by doing a least squares linear fit
*     at each point.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_rolling_fit( dim_t box, float wlim, dim_t el, dim_t start,
*                           dim_t end, const double *dat, double *grad,
*                           double *off, double *rms, int *status )

*  Arguments:
*     box = dim_t (Given)
*        The width of the fitting box. If an even value is supplied, the
*        supplied value will be incremented by one to make it odd.
*     wlim = double (Given)
*        A value in the range 0.0 to 1.0 that gives the minimum fraction of
*        valid input values that a fitting box must contain in order to
*        produce a valid output value. If a value smaller than 0.0 or
*        larger than 1.0 is supplied, then an output value will be bad if
*        and only if the corresponding input value is bad.
*     el = dim_t (Given)
*        The length of the "dat" array.
*     start = dim_t (Given)
*        The index of the first array element of "dat" to smooth.
*     end = dim_t (Given)
*        The index of the last array element of "dat" to smooth.
*     dat = const double * (Given)
*        The data array. Any VAL__BADD values are ignored.
*     grad = double * (Returned)
*        An output array holding the gradient at each point in the input
*        array. The supplied array should have "end-start+1" elements. The
*        gradient for input element "start" will be put in element zero
*        of this array, etc.
*     off = double * (Returned)
*        An output array holding the value of the fitted line at the
*        centre of each fitting box. The supplied array should have
*        "end-start+1" elements. The offset for input element "start" will
*        be put in element zero of this array, etc.
*     rms = double * (Returned)
*        An output array holding the RMS deviation of the input data
*        about the fitted line at each point. The supplied array should have
*        "end-start+1" elements. The RMS for input element "start" will
*        be put in element zero of this array, etc. A NULL pointer may be
*        supplied if the RMS values are not required.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     A least squares linear fit is performed to the values in a box of
*     width "box" centred on element "start" of array "dat". The gradient,
*     offset and RMS deviation of the fitted line are returned in element
*     zero of arrays "grad", "off" and "rms". The box is then advanced
*     one sample within the "dat" array, and a new fit is performed, the
*     gradient, offset and RMS being returned in element one of arrays
*     "grad", "off" and "rms". This process is continued until a fit has
*     been performed to a box centred on element "end" of the "dat" array.
*
*     A simple "rolling box" approach is used in which the statistics of
*     the values in the box are modified for each point by adding in the
*     value at the leading edge of the box and removing the value at the
*     trailing edge of the box.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     23-AUG-2010 (DSB):
*        Original version.
*     14-NOV-2010 (DSB):
*        Avoid integer truncation in sums.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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

void smf_rolling_fit( dim_t box, float wlim, dim_t el, dim_t start,
                      dim_t end, const double *dat, double *grad,
                      double *off, double *rms, int *status ){

/* Local Variables: */
   const double *pcen;         /* Pointer to central data value */
   const double *pnew;         /* Pointer to next data value */
   const double *pold;         /* Pointer to oldest data value */
   dim_t hb;                   /* Half box size */
   dim_t i;                    /* Index into input/output arrays */
   dim_t ihi;                  /* Highest index to include in box */
   dim_t ilo;                  /* Lowest index to include in box */
   dim_t inew;                 /* Index of *pnew value */
   dim_t iold;                 /* Index of *pold value */
   dim_t minpop;               /* Min no of valid i/p values for a valid o/p value */
   dim_t pop;                  /* Number of good values in box */
   double *pg;                 /* Pointer to next output gradient value */
   double *po;                 /* Pointer to next output offset value */
   double *pr;                 /* Pointer to next output RMS value */
   double c;                   /* Offset */
   double denom;               /* Denominator for fit equations */
   double dev;                 /* RMS deviation of input data about line */
   double m;                   /* Gradient */
   double sx;                  /* Sum of index values */
   double sxx;                 /* Sum of index*index values */
   double sxy;                 /* Sum of data*index values */
   double sy;                  /* Sum of data values */
   double syy;                 /* Sum of data*data values */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Ensure the box size is odd. */
   hb = box/2;
   box = 2*hb + 1;

/* Store the minimum number of valid input values that must be present in
   a fit box to create a valid output value. */
   if( wlim >= 0 && wlim <= 1.0 ) {
      minpop = wlim*box + 0.5;
      if( minpop == 0 ) {
         minpop = 1;
      } else if( minpop > box ) {
         minpop = box;
      }
   } else {
      minpop = 0;
   }

/* Initialise the required running sums to hold the sums of the data in
   a group of "box" data values centred in input element "start". The
   independent variable in the fit is the index into the input array. This
   means the "x*y" product summed in "sxy" does not depend on the current
   centre of the box, which is good. */
   sy = 0.0;
   sx = 0.0;
   sxy = 0.0;
   sxx = 0.0;
   syy = 0.0;
   pop = 0;
   pnew = dat;

   ilo = start - hb;
   if( ilo < 0 ) ilo = 0;

   ihi = start + hb;
   if( ihi >= el ) ihi = el - 1;

   pnew = dat + ilo;
   for( i = ilo; i <= ihi; i++,pnew++ ) {
      if( *pnew != VAL__BADD ) {
         sy += *pnew;
         sx += i;
         sxy += i*( *pnew );
         sxx += ( (double) i )*( (double) i );
         syy += ( *pnew )*( *pnew );
         pop++;
      }
   }

/* The "pnew" pointer points to the input data value that is about to be
   added to the fit box. Initialise another pointer "pold" to point to
   the input data value that is about to leave the fit box. This may
   initially point to an element before the start of the input array
   (e.g. if "start" is zero). */
   pold = dat + start - hb;

/* Initialise another pointer "pcen" to point to the central data value
   in the fit box. */
   pcen = dat + start;

/* Store the indices of the new and old values. */
   iold = pold - dat;
   inew = pnew - dat;

/* Initialise output array pointers. */
   pg = grad;
   po = off;
   pr = rms;

/* Calculate the gradient, offset and RMS for each required point. */
   for( i = start; i <= end; i++,pold++,pcen++,pnew++,iold++,inew++ ) {

/* Calculate the gradient, offset and RMS deviation of the least squares
   fit to the data currently in the fittting box. The offset here ("c") is
   the value of the line at the start of the input array (i==0). */
      denom =  pop*sxx - sx*sx;
      if( denom > 0 && pop > 0 ) {
         m =  ( pop*sxy - sx*sy )/denom;
         c =  ( sxx*sy - sx*sxy )/denom;
         dev = pop*syy - sy*sy - denom*( m )*( m );
         dev = ( dev > 0.0 ) ? sqrt( dev )/pop: 0.0;

/* Correct the offset to be the value of the line at the centre of the
   box. */
         c += m*i;

/* If "wlim" is between zero and one, check we have sufficient valid
   samples in the fit box. If so store the fit parameters in the output
   arrays, otherwise store bad values. */
         if( minpop > 0 ) {
            if( pop > minpop ) {
               *(pg++) = m;
               *(po++) = c;
               if( pr ) *(pr++) = dev;

            } else {
               *(pg++) = VAL__BADD;
               *(po++) = VAL__BADD;
               if( pr ) *(pr++) = VAL__BADD;
            }

/* If "wlim" is not between zero and one, store the fit parameters in the
   output arrays if the central input value is good, otherwise store bad
   values. */
         } else if( *pcen != VAL__BADD ) {
            *(pg++) = m;
            *(po++) = c;
            if( pr ) *(pr++) = dev;

         } else {
            *(pg++) = VAL__BADD;
            *(po++) = VAL__BADD;
            if( pr ) *(pr++) = VAL__BADD;
         }

/* Store bad values if the fit cannot be calculated. */
      } else {
         *(pg++) = VAL__BADD;
         *(po++) = VAL__BADD;
         if( pr ) *(pr++) = VAL__BADD;
      }

/* If there will be another pass through the "i" loop... */
      if( i < end ) {

/* Add the next input value into the running sums. */
         if( inew < el && *pnew != VAL__BADD ) {
            sy += *pnew;
            sx += inew;
            sxy += inew*( *pnew );
            sxx += ( (double) inew )*( (double) inew );
            syy += ( *pnew )*( *pnew );
            pop++;
         }

/* Remove the oldest input value from the running sums. There is nothing
   to add in on the last pass through the "i" loop. */
         if( iold >= 0 && *pold != VAL__BADD ) {
            sy -= *pold;
            sx -= iold;
            sxy -= iold*( *pold );
            sxx -= ( (double) iold )*( (double) iold );
            syy -= ( *pold )*( *pold );
            pop--;
         }
      }
   }
}

