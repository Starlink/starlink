      SUBROUTINE WMERGE(NPARAMS,PARAMS,
     &             STK_TITLE,STK_LAMBDA,STK_STOKES_I,STK_STOKES_Q,
     &             STK_STOKES_QV,STK_STOKES_U,STK_STOKES_UV,
     &             STK_NPTS,TOP_STK,NPTS,LAMBDA,STOKES_I,STOKES_Q,
     &             STOKES_QV,STOKES_U,STOKES_UV,TITLE,OUT_LU)
C+
C
C Subroutine:
C
C    W M E R G E
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<), PARAMS (<),
C STK_TITLE (<), STK_LAMBDA (<), STK_STOKES_I (<), STK_STOKES_Q (<),
C STK_STOKES_QV (<), STK_STOKES_U (<), STK_STOKES_UV (<),
C STK_NPTS (<), TOP_STK (<), NPTS (><), LAMBDA (><), STOKES_I (><),
C STOKES_Q (><), STOKES_QV (><), STOKES_U (><), STOKES_UV (><),
C TITLE (>), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C Weighted merge of two polarization spectra
C
C
C-

      IMPLICIT NONE
      INTEGER OUT_LU
      INCLUDE 'array_size.inc'
      INTEGER NPARAMS
      REAL PARAMS(*)
      INTEGER SPEC1,SPEC2
      CHARACTER*80 STK_TITLE(MAXSPEC)
      REAL STK_LAMBDA(MAXPTS,MAXSPEC)
      INTEGER STK_NPTS(MAXSPEC)
      REAL STK_STOKES_I(MAXPTS,MAXPTS)
      REAL STK_STOKES_Q(MAXPTS,MAXPTS)
      REAL STK_STOKES_QV(MAXPTS,MAXPTS)
      REAL STK_STOKES_U(MAXPTS,MAXPTS)
      REAL STK_STOKES_UV(MAXPTS,MAXPTS)
      INTEGER TOP_STK
      REAL LAMBDA(*)
      REAL STOKES_I(*)
      REAL STOKES_Q(*)
      REAL STOKES_QV(*)
      REAL STOKES_U(*)
      REAL STOKES_UV(*)
      INTEGER NPTS
      INTEGER I
      REAL WGHT1,WGHT2,WEIGHT_TOT
      CHARACTER*(*) TITLE
      LOGICAL OK
C
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('First stack spectrum',PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS = 1
      ENDIF
C
      IF (NPARAMS.EQ.1) THEN
       CALL GET_PARAM('Second stack spectrum',PARAMS(2),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS = 2
      ENDIF
C
      IF (NPARAMS.EQ.2) THEN
       CALL GET_PARAM('First weight',PARAMS(3),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS = 3
      ENDIF
C
      IF (NPARAMS.EQ.3) THEN
       CALL GET_PARAM('Second weight',PARAMS(4),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS = 4
      ENDIF
C
      IF (NPARAMS.GT.4) THEN
       CALL WR_ERROR('Additional parameters ignored',OUT_LU)
      ENDIF
C
      SPEC1 = INT(PARAMS(1))
      SPEC2 = INT(PARAMS(2))
      WGHT1 = PARAMS(3)
      WGHT2 = PARAMS(4)
      WEIGHT_TOT=WGHT1+WGHT2
      WGHT1=WGHT1/WEIGHT_TOT
      WGHT2=WGHT2/WEIGHT_TOT
C
      IF ( (SPEC1.LT.0 ).OR.(SPEC1.GT.TOP_STK) ) THEN
       CALL WR_ERROR('First stack spectrum out of stack range',
     &                OUT_LU)
       GOTO 666
      ENDIF
C
      IF ( (SPEC2.LT.0 ).OR.(SPEC2.GT.TOP_STK) ) THEN
       CALL WR_ERROR('Second stack spectrum out of stack range',
     &                OUT_LU)
       GOTO 666
      ENDIF
C
      IF (STK_NPTS(SPEC1).NE.STK_NPTS(SPEC2)) THEN
       CALL WR_ERROR('Different number of datum points',OUT_LU)
       GOTO 666
      ENDIF
C
      OK=.TRUE.
      DO I = 1,STK_NPTS(SPEC1)
       IF (OK) THEN
        IF (STK_LAMBDA(I,SPEC1).NE.STK_LAMBDA(I,SPEC2)) THEN
         CALL WR_ERROR('X arrays differ. Please regrid.',OUT_LU)
         OK=.FALSE.
        ENDIF
       ENDIF
      ENDDO
      IF (.NOT.OK) GOTO 666
      DO I=1,STK_NPTS(SPEC1)
       STOKES_I(I)=STK_STOKES_I(I,SPEC1)*WGHT1+
     &             STK_STOKES_I(I,SPEC2)*WGHT2
       STOKES_Q(I)=STK_STOKES_Q(I,SPEC1)*WGHT1+
     &             STK_STOKES_Q(I,SPEC2)*WGHT2
       STOKES_QV(I)=STK_STOKES_QV(I,SPEC1)*WGHT1**2+
     &             STK_STOKES_QV(I,SPEC2)*WGHT2**2
       STOKES_U(I)=STK_STOKES_U(I,SPEC1)*WGHT1+
     &             STK_STOKES_U(I,SPEC2)*WGHT2
       STOKES_UV(I)=STK_STOKES_UV(I,SPEC1)*WGHT1**2+
     &             STK_STOKES_UV(I,SPEC2)*WGHT2**2
       LAMBDA(I)=STK_LAMBDA(I,SPEC1)
      ENDDO
      NPTS=STK_NPTS(SPEC1)
      TITLE='Merged data'
666   CONTINUE
      END

