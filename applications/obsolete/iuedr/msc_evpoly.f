      REAL*8 FUNCTION MSC_EVPOLY( NP, P, X )
*+
*  Name:
*     REAL*8 FUNCTION MSC_EVPOLY

*  Purpose:
*     Evaluate polynomial p(x).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     REAL*8 = MSC_EVPOLY( NP, P, X )

*  Method:
*     This evaluates a polynomial of any order (including negative
*     or zero cases).

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     25-JUN-81 (JRG):
*       IUEDR Vn. 1.0
*     31-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     06-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER NP       ! Number of coefficients.

      REAL*8 P( NP )   ! Polynomial coefficients.
      REAL*8 X         ! Independent variable.

*  Local Variables:
      INTEGER I        ! Loop index.

      REAL*8 Y         ! High precision result temporary.
      REAL*8 XP        ! High precision independent variable.

*.

      I = 1
      Y = 0.0D0
      XP = 1.D0

 100  CONTINUE
      IF ( .NOT. ( I .LE. NP ) ) THEN
         MSC_EVPOLY = Y
         RETURN

      ELSE
         Y = Y + P( I ) * XP
         XP = XP * X
         I = I + 1
         GO TO 100
      END IF

      END
