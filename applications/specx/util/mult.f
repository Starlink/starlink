C-----------------------------------------------------------------------

      SUBROUTINE MULT (XF, NQ)

C  Multiply data in quadrant NQ by factor XF

      IMPLICIT  NONE

      REAL     XF
      INTEGER  NQ

      INCLUDE 'FLAGCOMM'
      INCLUDE 'STACKCOMM'

      INTEGER  I
      INTEGER  NQ1, NQ2

      LOGICAL DOQUAD

*  Ok, go...

      CALL QLIM (NQ, NQ1, NQ2)

      DO I = NQ1, NQ2
        IF (DOQUAD(I))   CALL QMULT (I, XF, BADPIX_VAL)
      END DO

      RETURN
      END


