      SUBROUTINE sla_IMXV (RM, VA, VB)
*+
*     - - - - -
*      I M X V
*     - - - - -
*
*  Performs the 3-D backward unitary transformation:
*
*     vector VB = (inverse of matrix RM) * vector VA
*
*  (single precision)
*
*  (n.b.  the matrix must be unitary, as this routine assumes that
*   the inverse and transpose are identical)
*
*  Given:
*     RM       real(3,3)    matrix
*     VA       real(3)      vector
*
*  Returned:
*     VB       real(3)      result vector
*
*  P.T.Wallace   Starlink   November 1984
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL RM(3,3),VA(3),VB(3)

      INTEGER I,J
      REAL W,VW(3)



*  Inverse of matrix RM * vector VA -> vector VW
      DO J=1,3
         W=0.0
         DO I=1,3
            W=W+RM(I,J)*VA(I)
         END DO
         VW(J)=W
      END DO

*  Vector VW -> vector VB
      DO J=1,3
         VB(J)=VW(J)
      END DO

      END
