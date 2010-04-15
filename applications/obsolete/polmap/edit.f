      SUBROUTINE EDIT(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                 STOKES_U,STOKES_UV,LAMBDA,NPTS,OUT_LU)
C
C+
C
C Subroutine:
C              E D I T
C
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<), PARAMS (<), STOKES_I (><), STOKES_Q (><), STOKES_QV (><),
C STOKES_U (><), STOKES_UV (><), LAMBDA (><), NPTS (><), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C Used to "snip out" a region of the current polarization spectrum using
C two wavelength points defined using the cursor.
C
C-

C
      IMPLICIT NONE
      INTEGER OUT_LU
      INTEGER NPTS,NEWNPTS
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
      REAL WMAX,WMIN,Y
      CHARACTER*1 CH
      LOGICAL OK
C
      IF (NPARAMS.GT.2) THEN
        CALL WR_ERROR('Additional parameters ignored',OUT_LU)
      ENDIF
C
      IF (NPARAMS.EQ.0) THEN
       WRITE(OUT_LU,*) 'Press a key to set the start wavelength'
       CALL PGCURSE(PARAMS(1),Y,CH)
       PARAMS(2)=PARAMS(1)
       WRITE(OUT_LU,*) 'Press a key to set the end wavelength'
       CALL PGCURSE(PARAMS(2),Y,CH)
      ENDIF
C
      IF (NPARAMS.EQ.1) THEN
        CALL GET_PARAM('End wavelength',PARAMS(2),OK,OUT_LU)
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
      NEWNPTS=0
      DO I = 1,WS
       NEWNPTS=NEWNPTS+1
       TEMP_STOKES_I(I) = STOKES_I(I)
       TEMP_STOKES_Q(I) = STOKES_Q(I)
       TEMP_STOKES_QV(I) = STOKES_QV(I)
       TEMP_STOKES_U(I) = STOKES_U(I)
       TEMP_STOKES_UV(I) = STOKES_UV(I)
       TEMP_LAMBDA(I) = LAMBDA(I)
      ENDDO
      DO I=WE,NPTS
       NEWNPTS=NEWNPTS+1
       TEMP_STOKES_I(NEWNPTS)=STOKES_I(I)
       TEMP_STOKES_Q(NEWNPTS)=STOKES_Q(I)
       TEMP_STOKES_QV(NEWNPTS)=STOKES_QV(I)
       TEMP_STOKES_U(NEWNPTS)=STOKES_U(I)
       TEMP_STOKES_UV(NEWNPTS)=STOKES_UV(I)
       TEMP_LAMBDA(NEWNPTS)=LAMBDA(I)
      ENDDO
      NPTS = NEWNPTS
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








