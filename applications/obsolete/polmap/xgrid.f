      SUBROUTINE XGRID(NPARAMS,PARAMS,LAMBDA,STOKES_I,STOKES_Q,
     &                STOKES_QV,STOKES_U,STOKES_UV,NPTS,
     &                TITLE,OUT_LU)
C+
C
C Subroutine: 
C
C    X G R I D 
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C NPARAMS (<), PARAMS (<), LAMBDA (>), STOKES_I (>), STOKES_Q (>),
C STOKES_QV (>), STOKES_U (>), STOKES_UV (>), NPTS (>),
C TITLE (>), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C Creates an evenly spaced x grid
C
C
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
      REAL STOKES_I(*)
      REAL STOKES_Q(*)
      REAL STOKES_QV(*)
      REAL STOKES_U(*)
      REAL STOKES_UV(*)
      CHARACTER*(*) TITLE
      INTEGER NPTS
      LOGICAL OK
C
C Misc.
C
      REAL XSTART,XEND,DX
      INTEGER I,OUT_LU
C
      IF (NPARAMS.GT.3) THEN
        CALL WR_ERROR('Additional parameters ignored',OUT_LU)
      ENDIF
C
      IF (NPARAMS.EQ.0) THEN
        CALL GET_PARAM('X start',PARAMS(1),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=1
      ENDIF
C
      IF (NPARAMS.EQ.1) THEN
        CALL GET_PARAM('X end',PARAMS(2),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=2
      ENDIF
C
      IF (NPARAMS.EQ.2) THEN
        CALL GET_PARAM('Number of grid points',PARAMS(3),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
        NPARAMS=3
      ENDIF
C
      XSTART=PARAMS(1)
      XEND=PARAMS(2)
      IF (XEND.EQ.XSTART) THEN
       CALL WR_ERROR('No X-range',OUT_LU)
       GOTO 666
      ENDIF

      IF (INT(PARAMS(3)).GT.MAXPTS) THEN
       CALL WR_ERROR('Too many grid points',OUT_LU)
       GOTO 666
      ELSE
       NPTS=INT(PARAMS(3))
      ENDIF
C
      DX=(XEND-XSTART)/REAL(NPTS-1)
C
      DO I=1,NPTS
       LAMBDA(I)=XSTART+DX*REAL(I-1)
       STOKES_I(I)=0.
       STOKES_Q(I)=0.
       STOKES_QV(I)=0.
       STOKES_U(I)=0.
       STOKES_UV(I)=0.
      ENDDO
      TITLE='Grid'
666   CONTINUE  
      END
