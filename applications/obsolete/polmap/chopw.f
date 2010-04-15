      SUBROUTINE CHOPW(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                 STOKES_U,STOKES_UV,LAMBDA,NPTS,OUT_LU)
C+
C
C Subroutine:
C
C     C H O P W
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C
C Parameters:
C
C NPARAMS (<), PARAMS (<), STOKES_I (><), STOKES_Q(><), STOKES_QV(><),
C STOKES_U (><), STOKES_UV(><), LAMBDA (><), NPTS (<) ,OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C Restricts the current spectrum to a wavelength region.
C
C
C
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
C Temp arrays
C
      REAL TEMP_STOKES_I(MAXPTS)
      REAL TEMP_STOKES_Q(MAXPTS)
      REAL TEMP_STOKES_QV(MAXPTS)
      REAL TEMP_STOKES_U(MAXPTS)
      REAL TEMP_STOKES_UV(MAXPTS)
      REAL TEMP_LAMBDA(MAXPTS)
C
C Misc.
C
      INTEGER WS,WE,I
      REAL WMAX,WMIN
      LOGICAL OK
C
      IF (NPARAMS.GT.2) THEN
        CALL WR_ERROR('Additional parameters ignored',OUT_LU)
      ENDIF
C
      IF (NPARAMS.EQ.0) THEN
        CALL GET_PARAM('Minimum wavelength',PARAMS(1),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS = 1
      ENDIF
C
      IF (NPARAMS.EQ.1) THEN
        CALL GET_PARAM('Maximum wavelength',PARAMS(2),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS = 2
      ENDIF
C
      WMIN = PARAMS(1)
      WMAX = PARAMS(2)
      IF (WMIN.GE.WMAX) THEN
       CALL WR_ERROR('Minimum is larger than maximum',OUT_LU)
       GOTO 666
      ENDIF
      IF ( (WMAX.LT.LAMBDA(1)).OR.(WMIN.GT.LAMBDA(NPTS)) ) THEN
       CALL WR_ERROR('Out of range',OUT_LU)
       GOTO 666
      ENDIF
      CALL LOCATE(LAMBDA,NPTS,WMIN,WS)
      IF (WS.EQ.0) WS = 1
      CALL LOCATE(LAMBDA,NPTS,WMAX,WE)
      DO I = WS,WE
       TEMP_STOKES_I(I-WS+1) = STOKES_I(I)
       TEMP_STOKES_Q(I-WS+1) = STOKES_Q(I)
       TEMP_STOKES_QV(I-WS+1) = STOKES_QV(I)
       TEMP_STOKES_U(I-WS+1) = STOKES_U(I)
       TEMP_STOKES_UV(I-WS+1) = STOKES_UV(I)
       TEMP_LAMBDA(I-WS+1) = LAMBDA(I)
      ENDDO
      NPTS = WE-WS+1
      DO I = 1,NPTS
       STOKES_I(I) = TEMP_STOKES_I(I)
       STOKES_Q(I) = TEMP_STOKES_Q(I)
       STOKES_QV(I) = TEMP_STOKES_QV(I)
       STOKES_U(I) = TEMP_STOKES_U(I)
       STOKES_UV(I) = TEMP_STOKES_UV(I)
       LAMBDA(I) = TEMP_LAMBDA(I)
      ENDDO
666   CONTINUE
      END
