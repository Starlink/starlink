/*+
 *  Name:
 *     util_ploc_c.c
 *
 *  Purpose:
 *     Returns the address of an argument when used as a Fortran function.
 *     This routine is direct replacement for %LOC on all platforms - it
 *     is needed becausse the SUN version doesn't always work.
 *
 *  Language:
 *
 *     Starlink ANSI C
 *
 *  Description:
 *
 *     This routine is direct replacement for %LOC on all platforms - it
 *     is needed becausse the SUN version doesn't always work.
 *
 *  Authors:
 *
 *     David J. Allan (BHVAD::DJA)
 *
 *  History:
 *
 *     9-DEC-1992 (DJA):
 *        Original version.
 *- */

/* Include Statements: */
#include "f77.h"                       /* c <-> FORTRAN interfacing */
#include "cnf.h"                       /* c <-> FORTRAN strings */

F77_INTEGER_FUNCTION(util_ploc)( INTEGER(arg) )
  {
  GENPTR_INTEGER(arg)			/* Any old argument */

  return (int) arg;
  }
