      SUBROUTINE LOSET( IAPER )
*+
*  Name:
*     SUBROUTINE LOSET

*  Description:
*     The contents of CMDISH are specified.
*     The line of constant wavelength is assumed to be perpendicular
*     to the dispersion line.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOSET( IAPER )

*  Arguments:
*     IAPER = INTEGER (Given)
*        The aperture index.

*  Method:
*     Assume that all data are valid.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     03-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Deficiencies:
*     The expression should be reformulated so that use of FLOAT
*     coefficients is more effective.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER IAPER     ! Aperture index

*  Global Variables:
      INCLUDE 'CMDISH'

*  Local Variables:
      REAL*8 ANGLE      ! angle of wavelength dispersion
      REAL*8 W1         ! W-value
      REAL*8 W2
      REAL*8 X1         ! X-value
      REAL*8 X2
      REAL*8 Y1         ! Y-value
      REAL*8 Y2
*.

      CORD = IAPER
      A0 = A( 1 )
      A1 = A( 2 )
      A2 = 0.0
      B0 = B( 1 )
      B1 = B( 2 )
      B2 = 0.0
      ANGLE = ATAN( A1 / B1 )
      DXDR = COS( ANGLE )
      DYDR = -SIN( ANGLE )
      WC = ( 768.0 / 2.0 - B0 ) / B1

*   Average displacement for unit wavelength interval.
      W1 = WC - 1.0
      W2 = WC + 1.0
      CALL WTOG( 0.0d0, W1, X1, Y1 )
      CALL WTOG( 0.0d0, W2, X2, Y2 )
      DRDW = SQRT( (X2 - X1) ** 2 + (Y2 - Y1) ** 2 ) / ABS( W2 - W1 )

      END
