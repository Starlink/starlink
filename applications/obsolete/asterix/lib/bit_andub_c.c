/*+
 *  Name:
 *     bit_andub_c.c
 *
 *  Purpose:
 *     Returns the bit-wise AND of its two unsigned byte arguments.
 *
 *  Language:
 *
 *     Starlink ANSI C
 *
 *  Invokation :
 *
 *     RESULT = BIT_ANDUB( VAL1, VAL2 )
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


F77_UBYTE_FUNCTION(bit_andub)( UBYTE(a), UBYTE(b) )
  {
  GENPTR_UBYTE(a)
  GENPTR_UBYTE(b)

  return ((*a) & (*b));
  }
