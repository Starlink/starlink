      DOUBLE PRECISION FUNCTION KPS1_POLYV( ORDER, C, X )
*+
*  Name:
*     KPG1_POLYV

*  Purpose:
*     Evaluate a polynomial at a given position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = KPS1_POLYV( ORDER, C, X )

*  Description
*     This routine evaluates a polynomial of the given order and
*     coefficients at the selected position.

*  Arguments:
*     ORDER = INTEGER (Given)
*        The order of polynomial.
*     C( ORDER + 1 ) = DOUBLE PRECISION (Given)
*        The polynomial coefficients.
*     X = DOUBLE PRECISION (Given)
*        The position at which to evaluate the polynomial.

*  Result:
*     The value of the polynomial.

*  Authors:
*     PWD: Peter W. Draper (JAC, Durham University)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-SEP-2005 (PWD):
*        Original version.
*     2006 April 12 (MJC):
*        Remove unused variable.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Arguments Given:
      INTEGER ORDER     
      DOUBLE PRECISION C( ORDER + 1 )
      DOUBLE PRECISION X

*  Local variables:
      INTEGER I                 ! Do-loop increment variables

*.

*  Evaluate fit for the given coordinate. Use Horner's Rule for speed
*  by avoiding evaluation of powers.
      KPS1_POLYV = C( ORDER + 1 )
      DO I = ORDER, 1, -1
         KPS1_POLYV = KPS1_POLYV * X + C( I )
      END DO
      END
