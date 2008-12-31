      SUBROUTINE ROTPA(NPARAMS,PARAMS,NPTS,Q,U,QV,UV,OUT_LU)
C+
C
C Subroutine: 
C
C
C    R O T P A
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C NPARAMS (<), PARAMS (<), NPTS (<), Q (><), U (><), QV (><), UV (><)
C OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C Rotates the position angle of the current polarization spectrum
C
C
C
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
      INTEGER NPTS
C
C The angle
C
      REAL THETA
C
C The polarization spectrum
C
      REAL Q(*)
      REAL U(*)
      REAL QV(*)
      REAL UV(*)
C
C Misc.
C
      INTEGER I
      REAL QQ
      REAL UU
      REAL QVAR
      REAL UVAR
      REAL QC
      REAL UC
      INTEGER NPARAMS
      REAL PARAMS(*)
      LOGICAL OK
C
      IF (NPARAMS.GT.1) THEN
       CALL WR_ERROR('Additional parameters ignored',OUT_LU)
      ENDIF
C
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Angle',PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS = 1
      ENDIF
C
      THETA = PARAMS(1)
      THETA = THETA*3.1415926/180.
C
C Get the matrix elements
C
      QC  =  COS(2.0*THETA)
      UC  =  SIN(2.0*THETA)
C
      DO I = 1,NPTS  
C
C Apply the rotation
C
          QQ  =  QC*Q(I) + UC*U(I)
          UU  =  QC*U(I) - UC*Q(I)
C
C Do the errors
C
          QVAR  =  0.5*(QV(I) + UV(I))
          UVAR  =  QVAR
C
          Q(I)  =  QQ
          U(I)  =  UU
          QV(I)  =  ABS(QVAR)
          UV(I)  =  ABS(UVAR)
      ENDDO
666   CONTINUE
      END
