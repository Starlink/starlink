      REAL*8 FUNCTION BRK_DRK( K, ALPHA, MORD, WV, RFAC )
*+
*  Name:
*     REAL*8 FUNCTION BRK_DRK

*  Purpose:
*     Calculates partial d(R)/d(k), where R is the ripple
*     correction factor and k is a ripple parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = BRK_DRK( K, ALPHA, MORD, WV, RFAC )

*  Arguments:
*     K = REAL*8 (Given)
*        Ripple parameter 'k'.
*     ALPHA = REAL*8 (Given)
*        Ripple parameter 'Alpha'.
*     MORD = INTEGER (Given)
*        Order number.
*     WV = REAL*8 (Given)
*        Wavelength.
*     RFAC = REAL*8 (Given)
*        Value of 'R' at WV.

*  Returned Value:
*     BRK_DRK = REAL*8
*        Value of partial d(R)/d(k).

*  Method:
*     See Barker (1984).

*  Authors:
*     IDH: Ian Howarth (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     ??-AUG-84 (IDH):
*       IUEDR Vn. 1.3
*     29-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     05-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     14-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Local Constants:
      REAL*8 PI        ! Pi.
      PARAMETER ( PI = 3.1415926535898d0 )

*  Arguments Given:
      REAL*8 K         ! Ripple parameter 'k'.
      REAL*8 ALPHA     ! Ripple parameter 'Alpha'.

      INTEGER MORD     ! Order number.

      REAL*8 WV        ! Wavelength.
      REAL*8 RFAC      ! Value of 'R' at WV.

*  Local variables:
      REAL*8 FACTOR
      REAL*8 FACTOR1
      REAL*8 FACTOR2
*.

      FACTOR = PI * ALPHA * ( DBLE( MORD ) - K / WV )

*  Calculate partial d(R)/d(k).
      IF ( FACTOR .EQ. 0.0 ) THEN
         BRK_DRK = 0.0

      ELSE
         FACTOR1 = PI * ALPHA * SIN( 2.0 * FACTOR )
         FACTOR1 = FACTOR1 / FACTOR ** 2
         FACTOR2 = 2.0 * RFAC / ( DBLE( MORD ) - K / WV )
         BRK_DRK = ( FACTOR1 - FACTOR2 ) / WV
      END IF

      END
