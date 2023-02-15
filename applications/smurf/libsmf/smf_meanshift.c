/*+
*  Name:
*     smf_meanshift

*  Purpose:
*     Smoothes a 1-D array with a mean-shift filter.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     void smf_meanshift( double *indata, double *outdata, dim_t nel,
*                         dim_t stride, int box1, double box2, smf_qual_t *qual,
*                         smf_qual_t mask, float wlim, int *status )

*  Arguments:
*     indata = double * (Given)
*        Pointer to the input 1D array to be smoothed.
*     outdata = double * (Returned)
*        Pointer to the output 1D array containing the smoothed data.
*     nel = dim_t (Given)
*        Number of points to be filtered, each separated by "stride"
*        elements.
*     stride = dim_t (Given)
*        The stride between samples in the "indata" array.
*     box1 = int (Given)
*        The size of the spatial top-hat filter box (in array elements).
*        If an even number is supplied, the next higher odd number is used.
*     box2 = double (Given)
*        The size of the data value top-hat filter box (in data units).
*     qual = smf_qual_t * (Given)
*        If specified, use this QUALITY array to decide which samples
*        to use (see "mask"). Otherwise data are only ignored if set
*        to VAL__BAD<X>.
*     mask = smf_qual_t (Given)
*        Use with qual to define which bits in quality are relevant to
*        ignore data in the calculation.
*     wlim = double (Given)
*        A value in the range 0.0 to 1.0 that gives the minimum fraction of
*        valid input values that a filter box must contain in order to
*        produce a valid output value. If a value smaller than 0.0 or
*        larger than 1.0 is supplied, then an output value will be bad if
*        and only if the corresponding input value is bad.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine replaces each element of the supplied 1D array with
*     a value determined by a mean-shift filter.

*  Authors:
*     David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1-DEC-2011 (DSB):
*        Initial version.
*     21-SEP-2012 (DSB):
*        Half box sizes were undefined when using bad value masking.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.

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
#include "ast.h"

/* SMURF includes */
#include "smf.h"

void smf_meanshift( double *indata, double *outdata, dim_t nel, dim_t stride,
                    int box1, double box2, smf_qual_t *qual, smf_qual_t mask,
                    float wlim, int *status) {

/* Local variables */
   dim_t count;             /* Number of samples in box */
   dim_t hb1;               /* Half width of box1 */
   dim_t icentral;          /* Index of central element in current filter box */
   dim_t iout;              /* Index of current output element */
   dim_t isum;              /* Sum of index values in the box */
   dim_t j;                 /* Index of box element */
   dim_t jhi;               /* Highest index of elements in box*/
   dim_t jlo;               /* Lowest index of elements in box*/
   dim_t minin;             /* Min no of valid i/p values for a valid o/p value */
   double *pin;             /* Pointer to central input value */
   double *pout;            /* Pointer to next output value */
   double *pp;              /* Pointer to next input value */
   double hb2;              /* Half-wodth of box2 */
   double icentral_new;     /* Index of central element in new filter box */
   double sum;              /* Sum of data values in the box */
   double vcentral;         /* Value of central element in current filter box */
   double vcentral_new;     /* Value of central element in new filter box */
   int iter;                /* Iteration count */
   smf_qual_t *pq;          /* Pointer to next input quality */
   smf_qual_t *qin;         /* Pointer to central input quality */

/* Check the inherited status. */
   if (*status != SAI__OK) return;

/* Return if the filtering operation would produce no change (i.e. if the
   box is of width 1 or the array only has 1 point). */
   if( box1 <= 1 || box2 <= 0.0 || nel <= 1 ) return;

/* Store the minimum number of valid input values that must be present in
   a filter box to create a valid output value. */
   if( wlim >= 0.0f && wlim <= 1.0f ) {
      minin = wlim*box1 + 0.5;
      if( minin == 0 ) {
         minin = 1;
      } else if( minin > box1 ) {
         minin = box1;
      }
   } else {
      minin = 1;
   }

/* Initialise a pointer to the input data value at the centre of the
   current filter box. */
   pin = indata;

/* Initialise a pointer to the current output data value. */
   pout = outdata;

/* Get the half width of each box. */
   hb1 = box1/2;
   hb2 = box2/2;

/* First deal with cases where we are using a quality array. */
   if( qual ) {

/* Initialise a pointer to the input quality value at the centre of the
   current filter box. */
      qin = qual;

/* Loop round all output elements. */
      for( iout = 0; iout < nel; iout++,pout++ ) {

/* Cannot do a mean-shift filter if the central value is bad. */
         if( ( (*qin) & mask ) || *pin == VAL__BADD ) {
            *pout = VAL__BADD;
         } else {

/* Note the initial central pixel position and value. */
            icentral = iout;
            vcentral = *pin;

/* Loop until the filtered value converges, or 20 iterations have been
   done. */
            for( iter = 0; iter < 1; iter++ ) {

/* Initialise the statistics of the values in the current filter box. */
               sum = 0.0;
               isum = 0;
               count = 0;

/* Loop over the box of input samples centred on the current central pixel.
   Ignore samples off the ends of the array. */
               jlo = icentral - hb1;
               if( jlo < 0 ) jlo = 0;

               jhi = icentral + hb1;
               if( jhi >= nel ) jhi = nel - 1;

               pq = qual + jlo*stride;
               pp = indata + jlo*stride;
               for( j = jlo; j <= jhi; j++ ) {

/* Check the input value is good. */
                  if( !( (*pq) & mask ) && *pp != VAL__BADD ) {

/* Check the difference between the input value and the current central
   filter value is no more than half of "box2" . If not, increment the
   statistics. */
                     if( fabs( *pp - vcentral ) <= hb2 ) {
                        sum += *pp;
                        isum += j;
                        count++;
                     }
                  }

                  pp += stride;
                  pq += stride;
               }

/* Form the new central pixel position and value. */
               if( count >= minin ) {
                  vcentral_new = sum/count;
                  icentral_new = (dim_t)( 0.5 + ((float)isum)/count);

/* If the process has converged, break. */
                  if( icentral_new == icentral &&
                     fabs( vcentral_new - vcentral ) < hb2*0.1  ) {
                     break;

/* Otherwise, update the central position and value in the filter box and
   loop. */
                  } else {
                     icentral = icentral_new;
                     vcentral = vcentral_new;
                  }

/* Break with a bad output value if too few pixels were left in the
   filter box. */
               } else {
                  vcentral_new = VAL__BADD;
                  break;
               }
            }

/* Store the output mean value. */
            *pout = vcentral_new;
         }

         pin += stride;
         qin += stride;
      }

/* Now deal with cases where we are using bad values. */
   } else {

/* Loop round all output elements. */
      for( iout = 0; iout < nel; iout++,pout++ ) {

/* Cannot do a mean-shift filter if the central value is bad. */
         if( *pin == VAL__BADD ) {
            *pout = VAL__BADD;
         } else {

/* Note the initial central pixel position and value. */
            icentral = iout;
            vcentral = *pin;

/* Loop until the filtered value converges, or 20 iterations have been
   done. */
            for( iter = 0; iter < 20; iter++ ) {

/* Initialise the statistics of the values in the current filter box. */
               sum = 0.0;
               isum = 0;
               count = 0;

/* Loop over the box of input samples centred on the current central pixel.
   Ignore samples off the ends of the array. */
               jlo = icentral - hb1;
               if( jlo < 0 ) jlo = 0;

               jhi = icentral + hb1;
               if( jhi >= nel ) jhi = nel - 1;

               pp = indata + jlo*stride;
               for( j = jlo; j <= jhi; j++ ) {

/* Check the input value is good. */
                  if( *pp != VAL__BADD ) {

/* Check the difference between the input value and the current central
   filter value is no more than half of "box2" . If not, increment the
   statistics. */
                     if( fabs( *pp - vcentral ) <= hb2 ) {
                        sum += *pp;
                        isum += j;
                        count++;
                     }
                  }

                  pp += stride;
               }

/* Form the new central pixel position and value. */
               if( count >= minin ) {
                  vcentral_new = sum/count;
                  icentral_new = (dim_t)( 0.5 + (float)isum/(float)count );

/* If the process has converged, break. */
                  if( icentral_new == icentral &&
                     fabs( vcentral_new - vcentral ) < hb2*0.1  ) {
                     break;

/* Otherwise, update the central position and value in the filter box and
   loop. */
                  } else {
                     icentral = icentral_new;
                     vcentral = vcentral_new;
                  }

/* Break with a bad output value if too few pixels were left in the
   filter box. */
               } else {
                  vcentral_new = VAL__BADD;
                  break;
               }
            }

/* Store the output mean value. */
            *pout = vcentral_new;
         }

         pin += stride;
      }
   }
}
