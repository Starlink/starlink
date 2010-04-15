C-----------------------------------------------------------------------

      SUBROUTINE QLIM (NQ, NQ1, NQ2)

C   Returns first and last quadrants for DO-loop processing. If NQ
C   is equal to zero returns 1 and NQUAD, else NQ and NQ.

      INCLUDE 'STACKCOMM'

      NQ1=1
      NQ2=NQUAD
      IF(NQ.NE.0)   THEN
        IF(NQ.LE.NQUAD) THEN
          NQ1=NQ
          NQ2=NQ
        END IF
      END IF

      RETURN
      END


