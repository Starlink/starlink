*-----------------------------------------------------------------------

      SUBROUTINE FRQFIX (N, X, NC, COEFF)

*  Routine to apply polynomial correction to raw frequency array, and
*  return corrected array in the same location. Note that the correction
*  polynomial is applied in the sense:
*          corrected = raw + polynomial
*  Polynomial coefficients work on supplied frequencies (assumed MHz)

      IMPLICIT NONE

*     Formal parameters

      INTEGER*4 N           ! number of points in frequency array
      INTEGER*4 NC          ! number of polynomial correction coefficients
      REAL*4    X(N)        ! Raw and corrected frequency array
      REAL*4    COEFF(NC)   ! Coefficients.

*     Other variables

      INTEGER  I, J, K      ! counting variables
      REAL     V            ! temporary store for x(i)
      REAL     W            ! accumulator for correction

*  Ok? then do it.

*     First check how many terms supplied are actually non-zero

      J = NC
      DO WHILE (COEFF(J).EQ.0.0 .AND. J.GE.1)
        J = J - 1
      END DO
      IF (J.EQ.0) RETURN

      DO I = 1, N
        V = X(I)
        W = COEFF(J)
        IF (J.GE.2) THEN
          DO K = 2, J
            W = V*W + COEFF(J+1-K)
          END DO
        END IF
        X(I) = X(I) + W
      END DO

      RETURN
      END

*-----------------------------------------------------------------------
