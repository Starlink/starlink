      SUBROUTINE SETSYMB(NPARAMS,PARAMS,PSTYLE,OUT_LU)
C+
C
C Subroutine:
C
C   S E T S Y M B
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<), PARAMS (<), PSTYLE (<), OUT_LU (>)
C
C History:
C
C   May 1994 Created
C
C
C
C
C
C Get plotting symbol style
C
C-

      IMPLICIT NONE
      INTEGER NPARAMS,PSTYLE
      INTEGER OUT_LU
      REAL PARAMS(*)
      LOGICAL OK
      IF (NPARAMS.GT.1) THEN
       CALL WR_ERROR('Superfluous parameters ignored',OUT_LU)
      ENDIF
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Symbol',PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
      ENDIF
      PSTYLE=INT(PARAMS(1))
      IF (PSTYLE.LT.1) THEN
        CALL WR_ERROR('Symbol style out of range',OUT_LU)
        GOTO 666
      ENDIF
 666  CONTINUE
      END
