      SUBROUTINE IQUADD(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                  STOKES_U,STOKES_UV,NPTS,OUT_LU)
C+
C
C Subroutine: 
C
C    I Q U A D D 
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C NPARAMS (<), PARAMS (<), STOKES_I (><), STOKES_Q (><), STOKES_QV (><),
C STOKES_U (><), STOKES_UV (><), NPTS (<), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C Adds a constant  I Q U vector to the current polarization spectrum.
C
C
C-

      IMPLICIT NONE
      INTEGER NPTS,NPARAMS,I,OUT_LU
      REAL PARAMS(*)
      REAL STOKES_I(*)
      REAL STOKES_Q(*)
      REAL STOKES_QV(*)
      REAL STOKES_U(*)
      REAL STOKES_UV(*)
      LOGICAL OK
C
      IF (NPARAMS.EQ.0) THEN 
        CALL GET_PARAM('Stokes I',PARAMS(1),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=1
      ENDIF
C
      IF (NPARAMS.EQ.1) THEN 
        CALL GET_PARAM('Stokes Q (%)',PARAMS(2),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=2
      ENDIF
C
      IF (NPARAMS.EQ.2) THEN 
        CALL GET_PARAM('Stokes U (%)',PARAMS(3),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=3
      ENDIF
C
      DO I=1,NPTS
        STOKES_I(I)=STOKES_I(I)+PARAMS(1)
        STOKES_Q(I)=STOKES_Q(I)+(PARAMS(1)*PARAMS(2))/100.
        STOKES_U(I)=STOKES_U(I)+(PARAMS(1)*PARAMS(3))/100.
      ENDDO
 666  CONTINUE
      END

