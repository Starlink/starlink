      SUBROUTINE SETUV( ANGLE, X0, Y0 )
*+
*  Name:
*     SUBROUTINE SETUV

*  Description:
*     The U-axis is at ANGLE degrees to the X-axis and is such that
*     (U=0,V=0) is at (X0,Y0).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SETUV( ANGLE, X0, Y0 )

*  Arguments:
*     ANGLE = REAL*8 (Given)
*        Angle between U-axis and X-axis in degrees.
*     X0 = REAL*8 (Given)
*        X-coordinate of faceplate centre.
*     Y0 = REAL*8 (Given)
*        Y-coordinate of faceplate centre.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     04-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      REAL*8 ANGLE      ! angle between U and X in degrees
      REAL*8 X0         ! central X-value
      REAL*8 Y0         ! central Y-value

*  Global Variables:
      INCLUDE 'CMROTR'

*  Local Constants:
      REAL*8 MU         ! reflection sign constant
      PARAMETER ( MU = -1.0d0 )

      REAL*8 RADIAN     ! radians per degree
      PARAMETER ( RADIAN = 0.01745329d0 )
*      PARAMETER ( RADIAN = 0.017453292520d0 )
*.

*   (X,Y) to (U,V).
      DUDX = COS( ANGLE * RADIAN )
      DVDY = MU * DUDX
      DVDX = SIN( ANGLE * RADIAN )
      DUDY = -MU * DVDX
      DU = -DUDX * X0 - DUDY * Y0
      DV = -DVDX * X0 - DVDY * Y0

*   (U,V) to (X,Y).
      DXDU = MU * DVDY
      DXDV = -MU * DUDY
      DX = X0
      DYDU = -MU * DVDX
      DYDV = MU * DUDX
      DY = Y0

      END
