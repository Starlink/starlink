      SUBROUTINE sla_DIMXV (DM, VA, VB)
*+
*     - - - - - -
*      D I M X V
*     - - - - - -
*
*  Performs the 3-D backward unitary transformation:
*
*     vector VB = (inverse of matrix DM) * vector VA
*
*  (double precision)
*
*  (n.b.  the matrix must be unitary, as this routine assumes that
*   the inverse and transpose are identical)
*
*  Given:
*     DM       dp(3,3)    matrix
*     VA       dp(3)      vector
*
*  Returned:
*     VB       dp(3)      result vector
*
*  P.T.Wallace   Starlink   March 1986
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION DM(3,3),VA(3),VB(3)

      INTEGER I,J
      DOUBLE PRECISION W,VW(3)



*  Inverse of matrix DM * vector VA -> vector VW
      DO J=1,3
         W=0D0
         DO I=1,3
            W=W+DM(I,J)*VA(I)
         END DO
         VW(J)=W
      END DO

*  Vector VW -> vector VB
      DO J=1,3
         VB(J)=VW(J)
      END DO

      END
