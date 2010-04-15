      SUBROUTINE PON_PLOTPOLY( XMINP, XMAXP, NPOLY, COEFF )
*+
*  Name:
*     PON_PLOTPOLY

*  Purpose:
*     Plot a polynomial curve.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_PLOTPOLY( XMINP, XMAXP, NPOLY, COEFF )

*  Description:
*     Use PON_EVALPOLY to evaluate the polynomial and plot the curve
*     using calls to PGDRAW.

*  Arguments:
*     NPOLY = INTEGER (Given)
*        Number of data.
*     XMINP = REAL (Given)
*        Minimum X value.
*     XMAXP = REAL (Given)
*        Minimum Y value.
*     COEFF( NPOLY ) = DOUBLE PRECISION (Given)
*        Polynomial coefficients.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     22-JUN-1992 (JBVAD::PAH):
*        Original version.
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     3-JUN-1994 (PDRAPER):
*        PON_EVALCOPY is a REAL function.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER NPOLY

      REAL XMINP
      REAL XMAXP

      DOUBLE PRECISION COEFF( NPOLY )

*  External References:
      REAL PON_EVALPOLY         ! Polynomial evaluation

*  Local Variables:
      INTEGER I                  ! Loop index

      DOUBLE PRECISION X         ! X value
      DOUBLE PRECISION XSTEP     ! X increment

*.

      CALL PGMOVE( XMINP, PON_EVALPOLY( DBLE( XMINP ), NPOLY, COEFF ) )
      XSTEP = DBLE( XMAXP-XMINP )/500.0D0
      X = DBLE( XMINP )

      DO 10 I = 1, 500
         X = X + XSTEP
         CALL PGDRAW( REAL( X ), PON_EVALPOLY( X, NPOLY, COEFF ) )
 10   CONTINUE

      END
* $Id$
