      SUBROUTINE SETLINE(NPARAMS,PARAMS,LSTYLE,OUT_LU)
C+
C
C Subroutine: 
C
C   S E T L I N E
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C NPARAMS (<), PARAMS (<), LSTYLE (>), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C 
C  
C Sets lstyle parameter
C
C
C-

      IMPLICIT NONE
      INTEGER NPARAMS
      INTEGER OUT_LU,LSTYLE
      REAL PARAMS(*)
      LOGICAL OK
      IF (NPARAMS.GT.1) THEN
       CALL WR_ERROR('Superfluous parameters ignored',OUT_LU)
      ENDIF
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Line style (1-5)',PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
      ENDIF
      LSTYLE=INT(PARAMS(1))
      IF ((LSTYLE.GT.5).OR.(LSTYLE.LT.1)) THEN
        CALL WR_ERROR('Line style out of range',OUT_LU)
        GOTO 666
      ENDIF
      CALL PGSLS(LSTYLE)
 666  CONTINUE
      END
