C-----------------------------------------------------------------------

      INTEGER FUNCTION ICHKQ2 (NQ)

C   Routine to check that number of quadrants in data in both X and Y registers
C   is the same (up to the maximum number set by MASK )

      INCLUDE 'FLAGCOMM'
      INCLUDE 'STACKCOMM'

      ICHKQ2=0

      CALL XY
      NQUAD2 = NQUAD
      CALL XY

C   Find highest unmasked quadrant

      J=8
      DO WHILE (MASK(J).EQ.0)
        J=J-1
      END DO
      NMAX=J

      IF(NQ.EQ.0)   THEN
        IF(NQUAD2.LT.NQUAD.AND.NQUAD2.LT.NMAX)  ICHKQ2=1
      ELSE
        IF(NQUAD2.LT.NQ.AND.NQUAD2.LT.NMAX)  ICHKQ2=2
      END IF

      RETURN
      END


