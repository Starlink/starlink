      SUBROUTINE WTOG( R, W, X, Y )
*+
*  Name:
*     SUBROUTINE WTOG

*  Description:
*     The coordinates (R,W) are undistorted to (X,Y) using the
*     distortion data in CMDISH.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WTOG( R, W, X, Y )

*  Arguments:
*     R = REAL*8 (Given)
*        R-coordinate value.
*     W = REAL*8 (Given)
*        W-coordinate value.
*     X = REAL*8 (Returned)
*        X-coordinate value.
*     Y = REAL*8 (Returned)
*        Y-coordinate value.

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
*     09-AUG-94 (MJC):
*       IUEDR Vn. 3.1-2
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      REAL*8 R   ! R-value.
      REAL*8 W   ! W-value.

*  Arguments Returned:
      REAL*8 X   ! Geometric S-value.
      REAL*8 Y   ! Geometric L-value.

*  Global Variables:
      INCLUDE 'CMDISH'
*.

      X = A0 + W * ( A1 + W * A2 ) + DXDR * R
      Y = B0 + W * ( B1 + W * B2 ) + DYDR * R

      END
