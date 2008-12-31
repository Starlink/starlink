      SUBROUTINE XADD(NPARAMS,PARAMS,LAMBDA,NPTS,OUT_LU)
C+
C
C Subroutine: 
C
C     X A D D
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
C
C
C Add a constant to the x array
C
C
C
C-
      IMPLICIT NONE
      INCLUDE 'array_size.inc'
C
      REAL PARAMS(*)
      INTEGER NPARAMS
C
      REAL LAMBDA(*)
      INTEGER NPTS
      LOGICAL OK
C
C Misc.
C
      REAL DX
      INTEGER I,OUT_LU
C
      IF (NPARAMS.GT.1) THEN
        CALL WR_ERROR('Additional parameters ignored',OUT_LU)
      ENDIF
C
      IF (NPARAMS.EQ.0) THEN
        CALL GET_PARAM('Constant to add',PARAMS(1),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=1
      ENDIF
C
      DX=PARAMS(1)
C
      DO I=1,NPTS
       LAMBDA(I)=LAMBDA(I)+DX
      ENDDO
666   CONTINUE  
      END
