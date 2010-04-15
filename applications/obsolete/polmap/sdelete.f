      SUBROUTINE  SDELETE(NPARAMS,PARAMS,
     &             STK_TITLE,STK_LAMBDA,STK_STOKES_I,STK_STOKES_Q,
     &             STK_STOKES_QV,STK_STOKES_U,STK_STOKES_UV,
     &             STK_NPTS,TOP_STK,OUT_LU)
C+
C
C Subroutine:
C
C   S D E L E T E
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<), PARAMS (<),
C STK_TITLE (><), STK_LAMBDA (><), STK_STOKES_I (><), STK_STOKES_Q (><),
C STK_STOKES_QV (><), STK_STOKES_U (><), STK_STOKES_UV (><),
C STK_NPTS (><), TOP_STK (><), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C
C This routine deletes the specified polarization spectra from the stack.
C
C
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
      INCLUDE 'array_size.inc'
C
C The stack entrys
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
      LOGICAL OK
      INTEGER DL,I,J,K
C
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Entry',PARAMS(1),OK,OUT_LU)
       NPARAMS = 1
       IF (.NOT.OK) GOTO 666
      ENDIF
C
C Heapsort the parameter list
C
      IF (NPARAMS.GT.1) THEN
       CALL SORT(NPARAMS,PARAMS)
      ENDIF
C
C Work through the list backwards. Doesn't disturb the stack too much
C
      DO I = NPARAMS,1,-1
       DL = INT(PARAMS(I))
       IF (DL.GT.TOP_STK) THEN
        CALL WR_ERROR('Stack entry out of range',OUT_LU)
        ELSE IF (DL.EQ.TOP_STK) THEN
        TOP_STK = TOP_STK-1
         ELSE
        DO J = DL,TOP_STK
         DO K = 1,STK_NPTS(J+1)
          STK_STOKES_I(K,J) = STK_STOKES_I(K,J+1)
          STK_STOKES_Q(K,J) = STK_STOKES_Q(K,J+1)
          STK_STOKES_QV(K,J) = STK_STOKES_QV(K,J+1)
          STK_STOKES_U(K,J) = STK_STOKES_U(K,J+1)
          STK_STOKES_UV(K,J) = STK_STOKES_UV(K,J+1)
          STK_LAMBDA(K,J) = STK_LAMBDA(K,J+1)
         ENDDO
         STK_TITLE(J) = STK_TITLE(J+1)
         STK_NPTS(J) = STK_NPTS(J+1)
        ENDDO
        TOP_STK = TOP_STK-1
        ENDIF
      ENDDO
666   CONTINUE
      END
