C-----------------------------------------------------------------------

      SUBROUTINE FOLDSP (NQ, BAD)

C   Routine to find symmetric part of spectrum in each quadrant (about
C   centre freq).

      IMPLICIT  NONE

C     Formal parameters

      INTEGER   NQ
      REAL      BAD

C     Common blocks

      INCLUDE 'STACKCOMM'

C     Local variables

      INTEGER   J
      INTEGER   NST
      INTEGER   N1,  N2
      INTEGER   NQ1, NQ2
      REAL      SYMM

C     Functions

      LOGICAL   DOQUAD
      INTEGER   NTOT

C  ok, go...

      CALL QLIM (NQ, NQ1, NQ2)

      DO NQ = NQ1, NQ2
        IF (DOQUAD(NQ))  THEN
          NST = NTOT (NQ-1)
          DO J = 1, NPTS(NQ)/2
            N1 = NST + J
            N2 = NST + NPTS(NQ)+1-J

            IF (DATA(N1).EQ.BAD .AND. DATA(N2).EQ.BAD) THEN
              SYMM = BAD
            ELSE IF (DATA(N1).EQ.BAD) THEN
              SYMM = DATA(N2)
            ELSE IF (DATA(N2).EQ.BAD) THEN
              SYMM = DATA(N1)
            ELSE
              SYMM = (DATA(N1)+DATA(N2))*0.5
            END IF

            DATA(N1) = SYMM
            DATA(N2) = SYMM
          END DO
        END IF
      END DO

      RETURN
      END


