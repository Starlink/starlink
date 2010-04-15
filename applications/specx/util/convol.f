C-----------------------------------------------------------------------

      SUBROUTINE CONVOL (CFUN, N, BUF, NQ)

C   Routine to convolve data array in stack with function/array CFUN(N)

      IMPLICIT  NONE

*     Formal parameters:

      REAL      CFUN(*)
      INTEGER   N
      REAL      BUF(*)
      INTEGER   NQ

*     Include files

      INCLUDE 'FLAGCOMM'
      INCLUDE 'STACKCOMM'

*     Local variables

      INTEGER   I, J, K
      INTEGER   N1, N2
      REAL      WEIGHT

*     Functions

      INTEGER   NTOT

*  Ok, go...

      N1 = NTOT (NQ-1) + 1
      N2 = NTOT (NQ)

      DO I = N1, N2
        BUF(I) = 0.0
      END DO

      DO I = N1, N2

        WEIGHT = 0.0
        DO J = 1, N
          K = I + J - N/2 - 1
          IF (DATA(K).NE.BADPIX_VAL .AND. K.GE.N1 .AND. K.LE.N2) THEN
            BUF(I) = BUF(I) + CFUN(J)*DATA(K)
            WEIGHT = WEIGHT + CFUN(J)
          END IF
        END DO

        IF (WEIGHT.NE.0.0) THEN
          BUF(I) = BUF(I)/WEIGHT
        ELSE
          BUF(I) = BADPIX_VAL
        END IF

      END DO

C   Copy back into data

      DO I = N1, N2
        DATA(I) = BUF(I)
      END DO

      RETURN
      END


