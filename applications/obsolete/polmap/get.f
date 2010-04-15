      SUBROUTINE GET(TITLE,LAMBDA,STOKES_I,STOKES_Q,STOKES_QV,STOKES_U,
     &             STOKES_UV,NPTS,NPARAMS,PARAMS,
     &             STK_TITLE,STK_LAMBDA,STK_STOKES_I,STK_STOKES_Q,
     &             STK_STOKES_QV,STK_STOKES_U,STK_STOKES_UV,
     &             STK_NPTS,TOP_STK,OUT_LU)
C+
C
C Subroutine:
C
C          G E T
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C TITLE (>), LAMBDA (>), STOKES_I (>), STOKES_Q (>), STOKES_QV (>),
C STOKES_U (>), STOKES_UV (>), NPTS (>), NPARAMS (<), PARAMS (<),
C STK_TITLE (<), STK_LAMBDA (<), STK_STOKES_I (<), STK_STOKES_Q (<),
C STK_STOKES_QV (<), STK_STOKES_U (<), STK_STOKES_UV (<),
C STK_NPTS (<), TOP_STK (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C This routine gets a polarization spectrum from the stack.
C
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
      INCLUDE 'array_size.inc'
C
C The current arrays
C
      REAL LAMBDA(*)
      REAL STOKES_I(*)
      REAL STOKES_Q(*)
      REAL STOKES_QV(*)
      REAL STOKES_U(*)
      REAL STOKES_UV(*)
      CHARACTER*80 TITLE
      INTEGER NPTS
C
C The stack arrays
C
      REAL STK_LAMBDA(MAXPTS,MAXSPEC)
      REAL STK_STOKES_I(MAXPTS,MAXSPEC)
      REAL STK_STOKES_Q(MAXPTS,MAXSPEC)
      REAL STK_STOKES_QV(MAXPTS,MAXSPEC)
      REAL STK_STOKES_U(MAXPTS,MAXSPEC)
      REAL STK_STOKES_UV(MAXPTS,MAXSPEC)
      INTEGER STK_NPTS(MAXSPEC)
      CHARACTER*80 STK_TITLE(MAXSPEC)
      INTEGER TOP_STK
C
C The command parameters
C
      INTEGER NPARAMS
      REAL PARAMS(*)
C
C Misc.
C
      INTEGER I,J
      LOGICAL OK
C
      IF (NPARAMS.GT.1) THEN
       CALL WR_ERROR('Additional parameters ignored',OUT_LU)
      ENDIF
C
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Stack entry',PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS = 1
      ENDIF
C
      I = INT(PARAMS(1))
      IF ( (I.LT.1).OR.(I.GT.TOP_STK)) THEN
       CALL WR_ERROR('Parameter out of stack range',OUT_LU)
       GOTO 666
      ENDIF
C
      DO J = 1,STK_NPTS(I)
       STOKES_I(J) = STK_STOKES_I(J,I)
       STOKES_Q(J) = STK_STOKES_Q(J,I)
       STOKES_QV(J) = STK_STOKES_QV(J,I)
       STOKES_U(J) = STK_STOKES_U(J,I)
       STOKES_UV(J) = STK_STOKES_UV(J,I)
       LAMBDA(J) = STK_LAMBDA(J,I)
      ENDDO
      NPTS = STK_NPTS(I)
      TITLE = STK_TITLE(I)
      WRITE(OUT_LU,*) 'Got ',TITLE(1:60)
C
666   CONTINUE
      END
