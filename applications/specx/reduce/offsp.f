C-----------------------------------------------------------------------

      SUBROUTINE OFFSSP (NQ)

      IMPLICIT  NONE

*     Formal parameter:

      INTEGER   NQ

*     Common blocks:

      INCLUDE 'STACKCOMM'
      INCLUDE 'FLAGCOMM'

*     Local variables:

      INTEGER   J
      INTEGER   JDEF
      INTEGER   NQ1, NQ2
      INTEGER   NST

*     Functions:

      LOGICAL   DOQUAD
      INTEGER   NTOT

* Ok, go...

      CALL GEN_GETR4 ('Offset', OFF, 'F8.2', OFF, JDEF)
      CALL QLIM      (NQ, NQ1, NQ2)

      DO NQ = NQ1,NQ2
        IF(DOQUAD(NQ))   THEN
          NST = NTOT (NQ-1)
          DO J = 1, NPTS(NQ)
            IF (DATA(NST+J).NE.BADPIX_VAL) THEN
              DATA(NST+J) = DATA(NST+J) + OFF
            END IF
          END DO
        END IF
      END DO

      RETURN
      END


