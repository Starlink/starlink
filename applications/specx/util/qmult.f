C-----------------------------------------------------------------------

      SUBROUTINE QMULT (NQ, XF, BADVAL)

C  Multiply a single quadrant of DATA by factor XF

      IMPLICIT  NONE

      INTEGER  NQ
      REAL     XF
      REAL     BADVAL

      INCLUDE 'STACKCOMM'

      INTEGER  N1, N2
      INTEGER  NP

      INTEGER  NTOT

*  Ok, go...

      N1 = NTOT (NQ-1)+1
      N2 = NTOT (NQ)
      DO NP = N1, N2
        IF (DATA(NP).NE.BADVAL) THEN
          DATA(NP) = DATA(NP) * XF
        END IF
      END DO

      RETURN
      END


