/*
*+
*  Name:
*     FTS1_RNAND

*  Purpose:
*     Replaces NaN values with bad values in a vector of 64-bit IEEE
*     floating-point numbers.

*  Language:
*     C

*  Invocation:
*     CALL FTS1_RNAND( EL, BUF, STATUS )

*  Description:
*     This routine replaces any IEEE not-a-number (NaN) values present
*     in a vector of 64-bit IEEE-754 floating-point numbers to the
*     standard _DOUBLE bad-pixel value.  Also converted to the standard
*     bad-pixel value are values who exponent is greater than maximum
*     provided by the host machine.  

*  Arguments:
*     EL = INTEGER (Given)
*        The number of IEEE numbers to be processed.  An expression must
*        not be given.
*     BUF( EL ) = DOUBLE PRECISION (Given and Returned)
*        The IEEE numbers to be cleansed of NaN values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  References:
*     -  "IEEE Standard for Binary Floating-Point Arithmetic",
*     ANSI/IEEE 754, 1985.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 December 20 (MJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*- */
      
/*  Global Constants: */
# include "sae_par.h"                  /* Environment global constants */
# include "f77.h"
# include "float.h"                    /* Special floating-point constants */

/*  Global Variables: */
# define VAL__BADD -DBL_MAX            /* Undefined value */

  union ieeef {
        double value;                     /* the raw number */
        struct {                          /* IEEE components bit fields */
           unsigned int fract1   : 32;    /* Fraction part 1 */
           unsigned int fract2   : 20;    /* Fraction part 2 */
           unsigned int exponent : 11;    /* Exponent with bias */
           unsigned int sign     :  1;    /* Sign bit */
        } fform; 
  } ;

F77_SUBROUTINE(fts1_rnand)( INTEGER(el), DOUBLE(buf), INTEGER(status) )

{

/* Note that pointers are given to make calling from Fortran possible.
*  All calling arguments must be variables or expressions.   */

/* Arguments Given: */
  GENPTR_INTEGER(el)

/* Arguments Given and Returned: */
  GENPTR_DOUBLE(buf)

/* Status: */
  GENPTR_INTEGER(status)

/* Local Variables: */
  int i;                  /* Loop counter */
  union ieeef ieee;       /* IEEE number */

/*. */

/* Check global status. */
    if ( *status != SAI__OK ) 
        return;

/* Loop for every element of the array to be converted. */
    for ( i=0; i<*el; i++ ) {

/* Copy the element into the union. */
        ieee.value = buf[ i ]; 

/* First check for not a number (NaN) or +/- Infinity.  Both have an
*  exponent of 2047.  The difference is whether the fraction is 0 or not.
*  NaN has a non-zero fraction.  However, we can regard both has been an
*  undefined value, hence we assign it the bad-pixel value. */
        if ( ieee.fform.exponent == 2047 )
           buf[i] = VAL__BADD;
    }
}
