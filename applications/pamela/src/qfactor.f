      REAL FUNCTION QFACTOR(N)
*
* Evaluates LOG(NDATAfactorial). For large data
* values it calls a Gamma function evaluator.
*
      IMPLICIT NONE
      INTEGER N, NTOP, J, MAXSTORE
      PARAMETER (MAXSTORE=50)
      REAL A(MAXSTORE), GAMMLN
      DATA NTOP,A(1)/0,0./
*
      IF(N.LE.NTOP) THEN
        QFACTOR = A(N+1)
      ELSE IF(N+1.LE.MAXSTORE) THEN
        DO J=NTOP+1,N
          A(J+1) = A(J) + LOG(REAL(J))
        END DO
        QFACTOR = A(N+1)
        NTOP = N
      ELSE
        QFACTOR = GAMMLN(REAL(N+1))
      END IF
      RETURN
      END
