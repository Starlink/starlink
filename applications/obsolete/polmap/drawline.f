      SUBROUTINE DRAWLINE(NPARAMS,PARAMS,OUT_LU)
C+
C
C Subroutine:
C
C    D R A W L I N E
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<), PARAMS (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C Interface for PGDRAW used for userdefined lines on QU graphs etc
C
C
C-

      IMPLICIT NONE
      REAL PARAMS(*)
      INTEGER OUT_LU,NPARAMS
      REAL X1,X2,Y1,Y2
      LOGICAL OK
C
      IF (NPARAMS.EQ.0) THEN
        CALL GET_PARAM('X1',PARAMS(1),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=1
      ENDIF
C
      IF (NPARAMS.EQ.1) THEN
        CALL GET_PARAM('Y1',PARAMS(2),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=2
      ENDIF
C
      IF (NPARAMS.EQ.2) THEN
        CALL GET_PARAM('X2',PARAMS(3),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=3
      ENDIF
C
      IF (NPARAMS.EQ.3) THEN
        CALL GET_PARAM('Y2',PARAMS(4),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=4
      ENDIF
C
      X1=PARAMS(1)
      Y1=PARAMS(2)
      X2=PARAMS(3)
      Y2=PARAMS(4)
C
      CALL PGMOVE(X1,Y1)
      CALL PGDRAW(X2,Y2)
C
 666  CONTINUE
      END


