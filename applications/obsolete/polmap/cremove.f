      SUBROUTINE CREMOVE(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                 STOKES_U,STOKES_UV,LAMBDA,NPTS,OUT_LU)
C+
C
C Subroutine:
C
C       C R E M O V E
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
C Removes continuum flux across a wavelength range defined interactively.
C The cursor is used to define to points and the continuum flux is found
C by a linear interpolation.
C
C-

C
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
      REAL TEMP_STOKES_I(MAXPTS)
      REAL TEMP_STOKES_Q(MAXPTS)
      REAL TEMP_STOKES_QV(MAXPTS)
      REAL TEMP_STOKES_U(MAXPTS)
      REAL TEMP_STOKES_UV(MAXPTS)
      REAL TEMP_LAMBDA(MAXPTS)
      REAL GRAD,ISTART,IEND,INT,NEWINT,NEWP

C
C Misc.
C
      INTEGER WS,WE,I,NEWNPTS
      REAL WMAX,WMIN
      CHARACTER*1 CH
C
C
      IF (NPARAMS.GT.0) THEN
        CALL WR_ERROR('Additional parameters ignored',OUT_LU)
      ENDIF
C
      WRITE(OUT_LU,*)
     &'Press a key to set the start wavelength-intensity'
      CALL PGCURSE(WMIN,ISTART,CH)
      WMAX=WMIN
      IEND=ISTART
      WRITE(OUT_LU,*)
     & 'Press a key to set the end wavelength/intensity'
      CALL PGCURSE(WMAX,IEND,CH)
C
      IF (WMIN.GE.WMAX) THEN
       CALL WR_ERROR('Minimum is larger than maximum',OUT_LU)
       GOTO 666
      ENDIF
C
      IF ( (WMAX.LT.LAMBDA(1)).OR.(WMIN.GT.LAMBDA(NPTS)) ) THEN
       CALL WR_ERROR('Out of range',OUT_LU)
       GOTO 666
      ENDIF
C
      CALL LOCATE(LAMBDA,NPTS,WMIN,WS)
      IF (WS.EQ.0) WS = 1
      CALL LOCATE(LAMBDA,NPTS,WMAX,WE)

      GRAD=(IEND-ISTART)/(WMAX-WMIN)
      NEWNPTS=0
      DO I=WS,WE-1
       INT=(LAMBDA(I)-WMIN)*GRAD+ISTART
       NEWINT=STOKES_I(I)-INT
       NEWP=100.*SQRT(STOKES_Q(I)**2+STOKES_U(I)**2)/NEWINT
       IF ((NEWINT.GT.0.).AND.(NEWP.LT.100)) THEN
        NEWNPTS=NEWNPTS+1
        TEMP_STOKES_I(NEWNPTS)=NEWINT
        TEMP_STOKES_Q(NEWNPTS)=STOKES_Q(I)
        TEMP_STOKES_QV(NEWNPTS)=STOKES_QV(I)
        TEMP_STOKES_U(NEWNPTS)=STOKES_U(I)
        TEMP_STOKES_UV(NEWNPTS)=STOKES_UV(I)
        TEMP_LAMBDA(NEWNPTS)=LAMBDA(I)
       ENDIF
      ENDDO
      DO I=1,NEWNPTS
       STOKES_I(I)=TEMP_STOKES_I(I)
       STOKES_Q(I)=TEMP_STOKES_Q(I)
       STOKES_QV(I)=TEMP_STOKES_QV(I)
       STOKES_U(I)=TEMP_STOKES_U(I)
       STOKES_UV(I)=TEMP_STOKES_UV(I)
       LAMBDA(I)=TEMP_LAMBDA(I)
      ENDDO
      NPTS=NEWNPTS
666   CONTINUE
      END








