      SUBROUTINE sla_VXV (VA, VB, VC)
*+
*     - - - -
*      V X V
*     - - - -
*
*  Vector product of two 3-vectors (single precision)
*
*  Given:
*      VA      real(3)     first vector
*      VB      real(3)     second vector
*
*  Returned:
*      VC      real(3)     vector result
*
*  P.T.Wallace   Starlink   March 1986
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL VA(3),VB(3),VC(3)

      REAL VW(3)
      INTEGER I


*  Form the vector product VA cross VB
      VW(1)=VA(2)*VB(3)-VA(3)*VB(2)
      VW(2)=VA(3)*VB(1)-VA(1)*VB(3)
      VW(3)=VA(1)*VB(2)-VA(2)*VB(1)

*  Return the result
      DO I=1,3
         VC(I)=VW(I)
      END DO

      END
