/*+
 *  Name:
 *     bit_notub_c.c
 *
 *  Purpose:
 *     Returns the bit-wise NOT of its unsigned byte argument.
 *
 *  Language:
 *
 *     Starlink ANSI C
 *
 *  Invokation :
 *
 *     RESULT = BIT_NOTUB( VAL )
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


F77_UBYTE_FUNCTION(bit_notub)( UBYTE(a) )
  {
  GENPTR_UBYTE(a)

  return ((*a) ^ 255);
  }
