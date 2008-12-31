      SUBROUTINE SET_PAPER(NPARAMS,PARAMS,OUT_LU)
C+
C
C Subroutine: 
C
C  S E T _ P A P E R
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
C Interface for pgpaper subroutine
C
C
C-

      IMPLICIT NONE
      INTEGER NPARAMS
      REAL PARAMS(*),ASPECT
      INTEGER OUT_LU
      LOGICAL OK
      
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Width in cm',PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS=1
      ENDIF
      IF (NPARAMS.EQ.1) THEN
       CALL GET_PARAM('Height in cm',PARAMS(2),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS=2
      ENDIF
      ASPECT=PARAMS(2)/PARAMS(1)
      PARAMS(1)=PARAMS(1)/25.4
      CALL PGPAPER(PARAMS(1),ASPECT)
 666  CONTINUE
      END







