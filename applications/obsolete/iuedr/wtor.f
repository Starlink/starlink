      SUBROUTINE WTOR( R, W, S, L )
*+
*  Name:
*     SUBROUTINE WTOR

*  Description:
*     The coordinates (R,W) are undistorted to (S,L).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WTOR( R, W, S, L )

*  Arguments:
*     R = REAL*8 (Given)
*        R coordinate.
*     W = REAL*8 (Given)
*        W coordinate.
*     S = REAL*8 (Returned)
*        S coordinate.
*     L = REAL*8 (Returned)
*        L coordinate.

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
      REAL*8 S     ! raw S-value
      REAL*8 L     ! raw L-value

*  Local Variables:
      REAL*8 X     ! geometric S-value
      REAL*8 Y     ! geometric L-value
*.

      CALL WTOG( R, W, X, Y )
      CALL GTOR( X, Y, S, L )

      END
