      SUBROUTINE sla_DVXV (VA, VB, VC)
*+
*     - - - - -
*      D V X V
*     - - - - -
*
*  Vector product of two 3-vectors  (double precision)
*
*  Given:
*      VA      dp(3)     first vector
*      VB      dp(3)     second vector
*
*  Returned:
*      VC      dp(3)     vector result
*
*  P.T.Wallace   Starlink   March 1986
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION VA(3),VB(3),VC(3)

      DOUBLE PRECISION VW(3)
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
