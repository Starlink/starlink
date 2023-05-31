/*
 *+
*  Name:
*     KPG1_IEEED

*  Purpose:
*     A Fortran callable function to convert NaN and Inf values to a
*     specified other value.

*  Language:
*     C

*  Invocation:
*     CALL KPG1_IEEED( EL, VAL, IN, OUT )

*  Description:
*     The routine uses the finite() system call to check each value in
*     in the suppled data array. Any values which are found to be Inf or
*     NaN are replaced by the supplied constant value in the output array.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements in the data array.
*     VAL = DOUBLE PRECISION (Given)
*        The value with which to replace any NaN or Inf values.
*     IN( EL ) = DOUBLE PRECISION (Given)
*        The input data array.
*     OUT( EL ) = DOUBLE PRECISION (Returned)
*        The output data array.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-JUN-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
#include <math.h>
#include "f77.h"

F77_SUBROUTINE(kpg1_ieeed)( INTEGER(el), DOUBLE(val), DOUBLE_ARRAY(in),
                            DOUBLE_ARRAY(out) )
{

   GENPTR_INTEGER(el)
   GENPTR_INTEGER(val)
   GENPTR_DOUBLE_ARRAY(in)
   GENPTR_DOUBLE_ARRAY(out)

   double *pin, *pout, *end;

/* Store a pointer to the first element not in the input array. */
   end = in + (*el);

/* Initialise a pointer to the first element in the output array. */
   pout = out;

/* Loop round every element in the input array. */
   for( pin = in; pin < end; pin++ ) {

/* If this element is not finite (i.e. if it is a NaN or an Inf) replace
   it with the supplied replacement value in the output array. Otherwise,
   store the input value in the output array. Increment the pointer to the
   next element of the output array. */
      if( !isfinite( *pin ) ) {
         *(pout++) = *val;
      } else {
         *(pout++) = *pin;
      }
   }
}
