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
# define VAL__BADR -FLT_MAX      /* Undefined value */

  union ieeef {
        float real;                       /* the raw number */
        struct {                          /* IEEE components bit fields */
           unsigned int fract    : 23;    /* Fraction */
           unsigned int exponent :  8;    /* Exponent with bias */
           unsigned int sign     :  1;    /* Sign bit */
        } fform; 
  } ;

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
  union ieeef ieee;       /* IEEE number */

/*. */

/* Check global status. */
    if ( *status != SAI__OK ) 
        return;

/* Loop for every element of the array to be converted. */
    for ( i=0; i<*el; i++ ) {

/* Copy the element into the union. */
        ieee.real = buf[ i ]; 

/* Check for not a number (NaN) or +/- Infinity.  Both have an 
*  exponent of 255.  The difference is whether the fraction is 0 or not.
*  NaN has a non-zero fraction.  However, we can regard both has been an
*  undefined value, hence we assign it the bad-pixel value. */
        if ( ieee.fform.exponent == 255 )
           buf[i] = VAL__BADR;

    }
}
