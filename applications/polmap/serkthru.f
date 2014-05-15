      SUBROUTINE SERKTHRU(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                   STOKES_U,STOKES_UV,LAMBDA,TITLE,NPTS,OUT_LU)
C+
C
C Subroutine:
C
C   S E R K T H R U
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<), PARAMS (<), STOKES_I (<), STOKES_Q (><), STOKES_QV (><),
C STOKES_U (><), STOKES_UV (><), LAMBDA (<), TITLE (><), NPTS (<),
C OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C Writes a Serkowski curve onto the current xgrid
C
C
C-

      IMPLICIT NONE
      REAL PARAMS(*)
      REAL LAMBDA(*)
      REAL STOKES_I(*)
      REAL STOKES_Q(*)
      REAL STOKES_QV(*)
      REAL STOKES_U(*)
      REAL STOKES_UV(*)
      CHARACTER*80 TITLE
C
      INTEGER NPTS,OUT_LU,NPARAMS,I
      REAL K,LAMMAX,QMAX,UMAX,Q,U,W,FAC
      LOGICAL OK
C
C Creates a Serkowski law that passes through a given polarization vector.
C
      IF (NPARAMS.EQ.0) THEN
        CALL GET_PARAM('Q (%)',PARAMS(1),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=1
      ENDIF
C
      IF (NPARAMS.EQ.1) THEN
        CALL GET_PARAM('U (%)',PARAMS(2),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=2
      ENDIF
C
      IF (NPARAMS.EQ.2) THEN
        CALL GET_PARAM('Wavelength',PARAMS(3),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=3
      ENDIF
C
      IF (NPARAMS.EQ.3) THEN
        CALL GET_PARAM('K',PARAMS(4),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=4
      ENDIF
C
      IF (NPARAMS.EQ.4) THEN
        CALL GET_PARAM('Lmax',PARAMS(5),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=5
      ENDIF
C
      PARAMS(1)=PARAMS(1)/100.
      PARAMS(2)=PARAMS(2)/100.
      Q=PARAMS(1)
      U=PARAMS(2)
      W=PARAMS(3)
      K=PARAMS(4)
      LAMMAX=PARAMS(5)
C
      FAC=EXP(-K*LOG(LAMMAX/W)**2)
C
      QMAX=Q/FAC
      UMAX=U/FAC
C
      DO I=1,NPTS
       STOKES_Q(I)=QMAX*EXP(-K*LOG(LAMMAX/LAMBDA(I))**2)*STOKES_I(I)
       STOKES_U(I)=UMAX*EXP(-K*LOG(LAMMAX/LAMBDA(I))**2)*STOKES_I(I)
       STOKES_QV(I)=0.
       STOKES_UV(I)=0.
      ENDDO
C
      TITLE='Serkowski Law'
C
 666  CONTINUE
      END


