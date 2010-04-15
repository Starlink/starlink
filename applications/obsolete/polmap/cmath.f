      SUBROUTINE CMATH(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                 STOKES_U,STOKES_UV,LAMBDA,NPTS,CH,OUT_LU)
C+
C
C Subroutine:
C
C     C M A T H
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<),PARAMS (<), STOKES_I (><), STOKES_Q (><), STOKES_QV(><),
C STOKES_U (><), STOKES_UV (><), LAMBDA (<), NPTS (<), CH (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C
C Adds or subtracts a constant (q,u) vector from the current polarization
C spectrum.
C-
C
      IMPLICIT NONE
      INTEGER OUT_LU
      INTEGER NPTS
      INCLUDE 'array_size.inc'
C
C The command parameters
C
      INTEGER NPARAMS
      REAL PARAMS(*)
      REAL STOKES_I(*)
      REAL STOKES_Q(*)
      REAL STOKES_QV(*)
      REAL STOKES_U(*)
      REAL STOKES_UV(*)
      REAL LAMBDA(*)
C
C Misc.
C
      INTEGER I
      CHARACTER*1 CH
      REAL Q_C,U_C
      LOGICAL OK
C
      IF (NPARAMS.GT.2) THEN
        CALL WR_ERROR('Additional parameters ignored',OUT_LU)
      ENDIF
C
      IF (NPARAMS.EQ.0) THEN
        CALL GET_PARAM('Q(%)',PARAMS(1),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=1
      ENDIF
C
      IF (NPARAMS.EQ.1) THEN
        CALL GET_PARAM('U(%)',PARAMS(2),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=2
      ENDIF
C
      IF (NPTS.EQ.0) THEN
        CALL WR_ERROR('Current arrays are empty',OUT_LU)
        GOTO 666
      ENDIF
C
      Q_C=PARAMS(1)/100.
      U_C=PARAMS(2)/100.
      IF (CH.EQ.'-') THEN
        Q_C=-Q_C
        U_C=-U_C
      ENDIF
C
      DO I=1,NPTS
C
       STOKES_Q(I)=((STOKES_Q(I)/STOKES_I(I))+Q_C)*STOKES_I(I)
       STOKES_U(I)=((STOKES_U(I)/STOKES_I(I))+U_C)*STOKES_I(I)
C
      ENDDO
 666  CONTINUE
      END


