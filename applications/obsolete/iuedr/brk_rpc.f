      REAL*8 FUNCTION BRK_RPC( K, ALPHA, MORD, WV )
*+
*  Name:
*     REAL*8 FUNCTION BRK_RPC

*  Purpose:
*     Calculates ripple correction 'R'.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = BRK_RPC( K, ALPHA, MORD, WV )

*  Arguments:
*     K = REAL*8 (Given)
*        Ripple parameter 'k'.
*     ALPHA = REAL*8 (Given)
*        Ripple parameter 'Alpha'.
*     MORD = INTEGER (Given)
*        Order number.
*     WV = REAL*8 (Given)
*        Wavelength.

*  Returned Value:
*     BRK_RPC = REAL*8
*        Ripple correction R.

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
*     30-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     05-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     16-FEB-95 (MJC):
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

*  Local variables:
      REAL*8 FACTOR

      FACTOR = PI * ALPHA * ( DBLE( MORD ) - K / WV )
*.

*  Calculate ripple correction.
       IF ( ABS( FACTOR ) .LT. 1.0E-04 ) THEN
          BRK_RPC = 1.0

       ELSE
          BRK_RPC = ( SIN( FACTOR ) ) ** 2 / FACTOR ** 2
       END IF

       END
