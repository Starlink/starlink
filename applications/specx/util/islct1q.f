C-----------------------------------------------------------------------

      INTEGER FUNCTION ISLCT1Q (NQ, IFAIL)

C   Routine to get a single quadrant for reduction operations in which
C   only a single quadrant can be operated on at once.

      LOGICAL       DOQUAD
      INCLUDE       'STACKCOMM'
      INCLUDE       'FLAGCOMM'

      ISLCT1Q=0
      IFAIL=0

      IF(NQ.EQ.0)   THEN
        CALL GEN_GETI4('You must choose a single quadrant for this '//
     &             'operation',NQ,' ',NQ,JDEF)
      END IF

      IF(NQ.GT.NQUAD) THEN
        IFAIL=28
      ELSE IF(.NOT.DOQUAD(NQ)) THEN
        IFAIL=29
      ELSE
        ISLCT1Q=NQ
      END IF

      RETURN
      END


