      SUBROUTINE GRWARN (TEXT)
*+
*     - - - - - - - -
*       G R W A R N     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Report a warning message with prefix "PGPLOT - "
*
*   Given
*      TEXT     c   Text of error message
*
*   D.L.Terrett  Starlink  Apr 1991
*+
      IMPLICIT NONE
      CHARACTER*(*) TEXT
      INCLUDE 'PGP_ERR'

      INTEGER   I

      IF (TEXT.NE.' ') THEN
          I = LEN(TEXT)
          DO WHILE (TEXT(I:I).EQ.' ')
              I = I-1
          END DO
          CALL MSG_SETC('MESS', TEXT(1:I))
          CALL ERR_REP('GRPGER','PGPLOT - ^MESS', GRPGER)
      END IF
      END
