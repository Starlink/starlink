/*+
 *  Name:
 *     ISHIFT

 *  Purpose:
 *     Perform bit shift on a given Fortran INTEGER and return the result.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     RESULT = ISHIFT( I1, I2 )

 *  Description:
 *     This function is designed to be callable from Fortran. It performs
 *     a bit shift upon a given Fortran INTEGER. The bits are shifted by
 *     NSHIFT places. If NSHIFT is negative, shift right and fill with 
 *     zeros. If NSHIFT is positive, shift left and wrap the bits around 
 *     to the least significant end.

 *  Arguments:
 *     I = INTEGER (Given)
 *        The INTEGER to be used for the bit shift.
 *     NSHIFT = INTEGER (Given)
 *        The number of places and direction of the bit shift.

 *  Returned Value:
 *     ISHIFT = INTEGER
 *        The result of the bit shift operation.

 *  Authors:
 *     PCTR: P.C.T. Rees (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     7-FEB-1992 (PCTR):
 *        Original version.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *- */

/* Include Statements: */
#include "ncar.h"

/* Function Definitons: */
INTEGER ISHIFT( INTEGER *i, INTEGER *nshift ){

/* Type Declarations: */
   unsigned int jshift;
   int          nbits;

/* Check if the bit shift is to the left or right. */
   if ( *nshift < 0 )
   {

   /* The bit shift is to the right. */
      nbits = ( *nshift < -INT_BITS ? INT_BITS : -*nshift );
      jshift = ( (unsigned int) *i >> nbits );
   } else {

   /* The bit shift is to the left, so fill the least significant bits 
    * with zeros. */
      nbits = *nshift % INT_BITS;
      jshift = ( (unsigned int) *i << nbits ) | 
               ( (unsigned int) *i >> ( INT_BITS - nbits ) );
   }

/* Return the result. */
   return( (INTEGER) jshift );
}
