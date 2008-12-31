      SUBROUTINE RV(NPARAMS,PARAMS,LAMBDA,NPTS,OUT_LU)
C+
C
C Subroutine: 
C
C   R V 
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C NPARAMS (<), PARAMS (<), LAMBDA (><), NPTS (<), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C Performs radial velocity corrections....
C
C
C
C
C
C-

      IMPLICIT NONE
      INTEGER NPARAMS,NPTS,I,OUT_LU
      REAL PARAMS(*)
      REAL LAMBDA(*),VEL,C,FAC
      LOGICAL OK
C
      C=2.99792458E10
      IF (NPTS.EQ.0) THEN
        CALL WR_ERROR('Current arrays are empty',OUT_LU)
        GOTO 666
       ENDIF
C
      IF (NPARAMS.GT.1) THEN
        CALL WR_ERROR('Superfluous parameters ignored',OUT_LU)
        GOTO 666
      ENDIF
C
      IF (NPARAMS.EQ.0) THEN
        CALL GET_PARAM('Radial velocity (km/s)',PARAMS(1),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=1
      ENDIF
C
      VEL=PARAMS(1)*1.E5
      FAC=1.-VEL/C
C
      DO I=1,NPTS
       LAMBDA(I)=LAMBDA(I)*FAC
      ENDDO
C
 666  CONTINUE 
      END
