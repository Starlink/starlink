      SUBROUTINE sla_DMXM (A, B, C)
*+
*     - - - - -
*      D M X M
*     - - - - -
*
*  Product of two 3x3 matrices:
*
*      matrix C  =  matrix A  x  matrix B
*
*  (double precision)
*
*  Given:
*      A      dp(3,3)        matrix
*      B      dp(3,3)        matrix
*
*  Returned:
*      C      dp(3,3)        matrix result
*
*  To comply with the ANSI Fortran 77 standard, A, B and C must
*  be different arrays.  However, the routine is coded so as to
*  work properly on the VAX and many other systems even if this
*  rule is violated.
*
*  P.T.Wallace   Starlink   5 April 1990
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION A(3,3),B(3,3),C(3,3)

      INTEGER I,J,K
      DOUBLE PRECISION W,WM(3,3)


*  Multiply into scratch matrix
      DO I=1,3
         DO J=1,3
            W=0D0
            DO K=1,3
               W=W+A(I,K)*B(K,J)
            END DO
            WM(I,J)=W
         END DO
      END DO

*  Return the result
      DO J=1,3
         DO I=1,3
            C(I,J)=WM(I,J)
         END DO
      END DO

      END
