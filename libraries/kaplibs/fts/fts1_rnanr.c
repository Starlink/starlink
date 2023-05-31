/*
*+
*  Name:
*     FTS1_RNANR

*  Purpose:
*     Replaces NaN values with bad values in a vector of 32-bit IEEE
*     floating-point numbers.

*  Language:
*     C

*  Invocation:
*     CALL FTS1_RNANR( EL, BUF, STATUS )

*  Description:
*     This routine replaces any IEEE not-a-number (NaN) values present
*     in a vector of 32-bit IEEE-754 floating-point numbers to the
*     standard _REAL bad-pixel value.  Also converted to the standard
*     bad-pixel value are values who exponent is greater than maximum
*     provided by the host machine.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of IEEE numbers to be processed.  An expression must
*        not be given.
*     BUF( EL ) = REAL (Given and Returned)
*        The IEEE numbers to be cleansed of NaN values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  References:
*     -  "IEEE Standard for Binary Floating-Point Arithmetic",
*     ANSI/IEEE 754, 1985.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1996 December 20 (MJC):
*        Original version.
*     1999 June 1 (TDCA):
*        Modified to use finite() and isnan() functions.
*     2004 September 2 (TIMJ):
*        Use prm_par.h rather than hand-coded bad value
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*- */

/*  Global Constants: */
# include <math.h>                     /* finite and isnan function declarations */
# include "sae_par.h"                  /* Environment global constants */
# include "f77.h"
# include "prm_par.h"                  /* Bad value definition */

F77_SUBROUTINE(fts1_rnanr)( INTEGER(el), REAL(buf), INTEGER(status) )

{

/* Note that pointers are given to make calling from Fortran possible.
*  All calling arguments must be variables or expressions.   */

/* Arguments Given: */
  GENPTR_INTEGER(el)

/* Arguments Given and Returned: */
  GENPTR_REAL(buf)

/* Status: */
  GENPTR_INTEGER(status)

/* Local Variables: */
  int i;                  /* Loop counter */
/*. */

/* Check global status. */
    if ( *status != SAI__OK )
        return;

/* Loop for every element of the array to be converted. */
    for ( i=0; i<*el; i++ ) {

/* Check for not a number (NaN) or +/- Infinity, and assign
   the element the bad-pixel value if necessary. */
        if ( !isfinite( buf[ i ]) || isnan( buf[ i ] ) ) buf[ i ] = VAL__BADR;
    }
}








