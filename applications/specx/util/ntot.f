C-----------------------------------------------------------------------

      INTEGER FUNCTION NTOT (NQ)

C   Utility routine to calculate # data-points in the X-register to the end
C   of quadrant NQ.

      INCLUDE 'SPECX_PARS'
      INCLUDE 'STACKCOMM'

      NTOT = 0
      IF (NQ.NE.0) THEN
        DO N = 1,MIN(NQ,NQMAX)
          NTOT = NTOT + NPTS(N)
        ENDDO
      END IF


      RETURN
      END
