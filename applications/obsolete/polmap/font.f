      SUBROUTINE FONT(NPARAMS,PARAMS,OUT_LU)
C+
C
C Subroutine: 
C     
C        F O N T
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
C Interface for PGSCF
C
C
C
C-

      IMPLICIT NONE
      INTEGER NPARAMS
      INTEGER OUT_LU,I
      REAL PARAMS(*)
      LOGICAL OK
      IF (NPARAMS.GT.1) THEN
       CALL WR_ERROR('Superfluous parameters ignored',OUT_LU)
      ENDIF
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Font style (1-4)',PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
      ENDIF
      I=INT(PARAMS(1))
      IF ((I.GT.4).OR.(I.LT.1)) THEN
        CALL WR_ERROR('Font style out of range',OUT_LU)
        GOTO 666
      ENDIF
      CALL PGSCF(I)
 666  CONTINUE
      END
