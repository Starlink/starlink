      SUBROUTINE WTOU( R, W, U, V )
*+
*  Name:
*     SUBROUTINE WTOU

*  Description:
*     The coordinates (R,W) are undistorted to (U,V) using the
*     distortion data in CMDISH.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WTOU( R, W, U, V )

*  Arguments:
*     R = REAL*8 (Given)
*        R coordinate.
*     W = REAL*8 (Given)
*        W coordinate.
*     U = REAL*8 (Returned)
*        U coordinate.
*     V = REAL*8 (Returned)
*        V coordinate.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     31-DEC-81 (JRG):
*       IUEDR Vn. 1.0
*     20-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     16-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      REAL*8 R     ! R-value
      REAL*8 W     ! W-value

*  Arguments Returned:
      REAL*8 U     !  U-value
      REAL*8 V     !  V-value

*  Local Variables:
      REAL*8 S     ! S-value
      REAL*8 L     ! L-value
*.

      CALL WTOR( R, W, S, L )
      CALL RTOU( S, L, U, V )

      END
