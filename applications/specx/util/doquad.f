C-----------------------------------------------------------------------

      LOGICAL FUNCTION DOQUAD (NQ)

C   Returns a true value if quadrant NQ is to be used for processing
C   and display

      INCLUDE   'SPECX_PARS'
      INCLUDE   'FLAGCOMM'
      INCLUDE   'STACKCOMM'

      DOQUAD = .FALSE.

      IF (MASK(NQ).EQ.1 .AND. NQ.LE.NQUAD) THEN
        DOQUAD = .TRUE.
      END IF

      RETURN
      END


