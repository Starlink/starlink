      SUBROUTINE sla_MXV (RM, VA, VB)
*+
*     - - - -
*      M X V
*     - - - -
*
*  Performs the 3-D forward unitary transformation:
*
*     vector VB = matrix RM * vector VA
*
*  (single precision)
*
*  Given:
*     RM       real(3,3)    matrix
*     VA       real(3)      vector
*
*  Returned:
*     VB       real(3)      result vector
*
*  P.T.Wallace   Starlink   March 1986
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL RM(3,3),VA(3),VB(3)

      INTEGER I,J
      REAL W,VW(3)


*  Matrix RM * vector VA -> vector VW
      DO J=1,3
         W=0.0
         DO I=1,3
            W=W+RM(J,I)*VA(I)
         END DO
         VW(J)=W
      END DO

*  Vector VW -> vector VB
      DO J=1,3
         VB(J)=VW(J)
      END DO

      END
