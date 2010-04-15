      SUBROUTINE PUT(TITLE,LAMBDA,STOKES_I,STOKES_Q,STOKES_QV,STOKES_U,
     &             STOKES_UV,NPTS,
     &             STK_TITLE,STK_LAMBDA,STK_STOKES_I,STK_STOKES_Q,
     &             STK_STOKES_QV,STK_STOKES_U,STK_STOKES_UV,
     &             STK_NPTS,TOP_STK,OUT_LU)
C+
C
C Subroutine:
C
C    P U T
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C TITLE (<), LAMBDA (<), STOKES_I (<), STOKES_Q (<), STOKES_QV (<),
C STOKES_U (<), STOKES_UV (<), NPTS (<), STK_TITLE (><), STK_LAMBDA (><)
C STK_STOKES_I (><), STK_STOKES_Q (><),
C STK_STOKES_QV (><), STK_STOKES_U (><), STK_STOKES_UV (><),
C STK_NPTS (><), TOP_STK (><) ,OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C
C Puts a polarization spectrum on to the top of the stack.
C
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
      INTEGER NPTS
      CHARACTER*80 TITLE
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
      INTEGER I
C
C If the stack is full
C
      IF (TOP_STK.EQ.MAXSPEC) THEN
       CALL WR_ERROR('Stack is full',OUT_LU)
       GOTO 666
      ENDIF
C
C Ensure there is something in the current arrays
C
      IF (NPTS.EQ.0) THEN
        CALL WR_ERROR('Current arrays are empty',OUT_LU)
        GOTO 666
      ENDIF
C
      TOP_STK = TOP_STK+1
      WRITE(OUT_LU,'(1X,A,I2)') 'Putting on stack entry ',TOP_STK
      DO I = 1,NPTS
       STK_STOKES_I(I,TOP_STK) = STOKES_I(I)
       STK_STOKES_Q(I,TOP_STK) = STOKES_Q(I)
       STK_STOKES_QV(I,TOP_STK) = STOKES_QV(I)
       STK_STOKES_U(I,TOP_STK) = STOKES_U(I)
       STK_STOKES_UV(I,TOP_STK) = STOKES_UV(I)
       STK_LAMBDA(I,TOP_STK) = LAMBDA(I)
      ENDDO
      STK_NPTS(TOP_STK) = NPTS
      STK_TITLE(TOP_STK) = TITLE
C
666   CONTINUE
      END
