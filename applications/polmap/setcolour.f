      SUBROUTINE SETCOLOUR(NPARAMS,PARAMS,LCOL,OUT_LU)
C+
C
C Subroutine:
C
C    S E T C O L O U R
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<), PARAMS (<), LCOL (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C sets lcol variable
C
C-

      IMPLICIT NONE
      INTEGER NPARAMS
      INTEGER OUT_LU,LCOL
      REAL PARAMS(*)
      LOGICAL OK
      IF (NPARAMS.GT.1) THEN
       CALL WR_ERROR('Superfluous parameters ignored',OUT_LU)
      ENDIF
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Colour',PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
      ENDIF
      LCOL=INT(PARAMS(1))
      IF (LCOL.LT.1) THEN
        CALL WR_ERROR('Line style out of range',OUT_LU)
        GOTO 666
      ENDIF
      CALL PGSCI(LCOL)
 666  CONTINUE
      END
