/*+
 *  Name:
 *     bit_or1ub_c.c
 *
 *  Purpose:
 *     Returns the bit-wise OR of every element of the unsigned byte
 *     array ARRAY with value MASK.
 *
 *  Language:
 *
 *     Starlink ANSI C
 *
 *  Invokation :
 *
 *     CALL BIT_OR1UB( N, ARRAY, MASK, STATUS )
 *
 *  Description:
 *
 *  Authors:
 *
 *     David J. Allan (ROSAT,University of Birmingham)
 *
 *  History:
 *
 *     24-Feb-1994 (DJA):
 *        Original version.
 *- */

#include "sae_par.h"			/* Starlink standard constants */
#include "f77.h"			/* Fortran <-> C interfacing */


F77_SUBROUTINE(bit_or1ub)( INTEGER(n), UBYTE_ARRAY(array),
                           UBYTE(mask), INTEGER(status) )
  {
  GENPTR_INTEGER(n)
  GENPTR_UBYTE_ARRAY(array)
  GENPTR_UBYTE(mask)
  GENPTR_INTEGER(status)

  int		i;			/* Loop over array */

  if ( *status != SAI__OK )
    return;

  for( i=0; i<(*n) ; i++ )
    array[i] |= (*mask);
  }
