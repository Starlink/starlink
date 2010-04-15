      SUBROUTINE LIST(TITLE,LAMBDA,STOKES_I,STOKES_Q,STOKES_QV,STOKES_U,
     &             STOKES_UV,NPTS,
     &             STK_TITLE,STK_LAMBDA,STK_STOKES_I,STK_STOKES_Q,
     &             STK_STOKES_QV,STK_STOKES_U,STK_STOKES_UV,
     &             STK_NPTS,TOP_STK,NPARAMS,PARAMS,OUT_LU)
C+
C
C Subroutine:
C
C          L I S T
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C TITLE (<), LAMBDA (<), STOKES_I (<), STOKES_Q (<), STOKES_QV (<)
C STOKES_U (<), STOKES_UV (<), NPTS (<),
C STK_TITLE (<), STK_LAMBDA (<), STK_STOKES_I (<), STK_STOKES_Q (<),
C STK_STOKES_QV (<), STK_STOKES_U (<), STK_STOKES_UV (<),
C STK_NPTS (<), TOP_STK (<), NPARAMS (<), PARAMS (<) ,OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C This subroutine lists the stack contents.
C
C
C
C
C-
      IMPLICIT NONE
      INCLUDE 'array_size.inc'
C
C The command parameters
C
      INTEGER NPARAMS
      REAL PARAMS(*)
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
C The current arrays
C
      REAL STOKES_I(*)
      REAL STOKES_Q(*)
      REAL STOKES_QV(*)
      REAL STOKES_U(*)
      REAL STOKES_UV(*)
      REAL LAMBDA(*)
      INTEGER NPTS,OUT_LU
      CHARACTER*(*) TITLE
C
      INTEGER I
C
C The formats
C
10    FORMAT(1X,I3,F8.1,F8.1,I6,2X,A40)
20    FORMAT(1X,A3,F8.1,F8.1,I6,2X,A40)
C
C IF "list 0" is called the current stack info is also listed
C
      IF ((NPARAMS.GT.0).AND.(INT(PARAMS(1)).EQ.0)) THEN
       WRITE(OUT_LU,*) ' No    Xs      Xe      N      Title'
       WRITE(OUT_LU,20) ' C',LAMBDA(1),LAMBDA(NPTS),NPTS,TITLE
       GOTO 666
      ENDIF
C
C The stack must have something to be listed
C
      IF (TOP_STK.GT.0) THEN
       WRITE(OUT_LU,*) ' No    Xs      Xe      N      Title'
       DO I = 1,TOP_STK
       WRITE(OUT_LU,10) I,STK_LAMBDA(1,I),STK_LAMBDA(STK_NPTS(I),I),
     &            STK_NPTS(I),STK_TITLE(I)
       ENDDO
      ELSE
       CALL WR_ERROR('Stack is empty',OUT_LU)
      ENDIF
666   CONTINUE
      END
