      SUBROUTINE SPD_UAAED( NX, X, NPAR, MODPAR, Y, STATUS )
*+
*  Name:
*     SPD_UAAE{DR}

*  Purpose:
*     Calculate N-Gauss-plus-constant data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_UAAED( NX, X, NPAR, MODPAR, Y, STATUS )

*  Description:
*     This routine calculates the sum of N Gauss components plus a
*     constant value. The constant and Gauss parameters are given in the
*     MODPAR array. An array of X values is accepted and a corresponding
*     array of Y values returned.

*  Arguments:
*     NX = INTEGER (Given)
*        Size of arrays X and Y.
*     X( NX ) = DOUBLE PRECISION (Given)
*        Given x value array.
*     NPAR = INTEGER (Given)
*        Size of MODPAR array, (NPAR-1)/3 is the number of Gauss
*        components.
*     MODPAR( NPAR ) = DOUBLE PRECISION (Given)
*        The model parameters. MODPAR(1) is the continuum level,
*        MODPAR(2:4) are centre, peak, FWHM of the first components,
*        MODPAR(5:7) for the second component, etc.
*     Y( NX ) = DOUBLE PRECISION (Returned)
*        Data array with model values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     2-MAY-1991 (HME):
*        Original version (NUMFIT).
*     31-MAY-1991 (HME):
*        Avoid divide by SIG if SIG=0.
*        Extrapolate the polynomial fit by turning the series of
*        Chebyshev polynomials into an ordinary polynomial (with
*        argument x', but not restricted to [-1,+1]).
*     26-JUN-1991 (HME):
*        SPFFDT.
*     09 May 1994 (hme):
*        Adapted to become SPD_UAAED. Use Fortran function for Gauss.
*        No safeguard against delta distributions any more.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NX
      DOUBLE PRECISION X( NX )
      INTEGER NPAR
      DOUBLE PRECISION MODPAR( NPAR )

*  Arguments Returned:
      DOUBLE PRECISION Y( NX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, C               ! Loop indices

*  Internal Referneces:
      DOUBLE PRECISION SPD_UAADD         ! Gauss function value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Outer loop is for vector element. Inner loop is for component.
      DO 2 I = 1, NX
         Y(I) = MODPAR(1)
         DO 1 C = 2, NPAR, 3
            Y(I) = Y(I) +
     :         SPD_UAADD( MODPAR(C), MODPAR(C+1), MODPAR(C+2), X(I) )
 1       CONTINUE
 2    CONTINUE

*  Return.
      END
