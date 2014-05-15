      SUBROUTINE ARROWSTYLE(PARAMS,NPARAMS,ARROWSIZE,OUT_LU)
C+
C
C Subroutine:
C
C        A R R O W S T Y L E
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C PARAMS (<), NPARAMS (<), ARROWSIZE (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C  Interface for PGSAH subroutine
C
C
C
C
C-
C
      IMPLICIT NONE
      REAL PARAMS(*)
      REAL ARROWSIZE
      INTEGER NPARAMS,OUT_LU
      LOGICAL OK
C
      IF (NPARAMS.EQ.0) THEN
        CALL GET_PARAM('Filled or outline (1 or 2)',PARAMS(1),
     &                 OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS = 1
      ENDIF
C
      IF (NPARAMS.EQ.1) THEN
        CALL GET_PARAM('Angular size of arrowhead',PARAMS(2),
     &                 OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS = 2
      ENDIF
C
      IF (NPARAMS.EQ.2) THEN
        CALL GET_PARAM('Arrowhead cutaway angle',PARAMS(3),
     &                 OK,OUT_LU)
        NPARAMS = 3
        IF (.NOT.OK) GOTO 666
      ENDIF
C
      IF (NPARAMS.EQ.3) THEN
        CALL GET_PARAM('Size of arrow',PARAMS(4),
     &                 OK,OUT_LU)
        NPARAMS = 4
        IF (.NOT.OK) GOTO 666
      ENDIF
C
      CALL PGSAH(INT(PARAMS(1)),PARAMS(2),PARAMS(3))
      ARROWSIZE = PARAMS(4)
C
 666  CONTINUE
      END
