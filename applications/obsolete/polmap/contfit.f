      SUBROUTINE CONTFIT(NPARAMS,PARAMS,TOP_STK,
     &   STK_STOKES_I,STK_STOKES_Q,STK_STOKES_QV,
     &   STK_STOKES_U,STK_STOKES_UV,STK_LAMBDA,
     &   STK_NPTS,STOKES_I,STOKES_Q,
     &   STOKES_QV,STOKES_U,STOKES_UV,LAMBDA,NPTS,NZONES,
     &   CONT_ST,CONT_EN,OUT_LU)
C+
C
C Subroutine: 
C
C
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C NPARAMS (<),PARAMS (<), TOP_STK (<),
C STK_STOKES_I (<), STK_STOKES_Q (<), STK_STOKES_QV (<),
C STK_STOKES_U (<), STK_STOKES_UV (<), STK_LAMBDA(<),
C STK_NPTS (<),STOKES_I (>), STOKES_Q (>),
C STOKES_QV (>),STOKES_U (>),STOKES_UV (>),LAMBDA (>),NPTS (>),NZONES (<),
C CONT_ST (<), CONT_EN (<), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C This subroutine fits a polynomial to the continuum bins of a stack
C entry and places the resulting fit in the current arrays.
C
C
C-

      IMPLICIT NONE
      INTEGER OUT_LU
      INCLUDE 'array_size.inc'
      INTEGER MAXORDER
      PARAMETER(MAXORDER=10)
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
      INTEGER TOP_STK
C
C Temporary storage
C
      REAL TMP_STOKES_I(MAXPTS)
      REAL TMP_STOKES_Q(MAXPTS)
      REAL TMP_STOKES_QV(MAXPTS)
      REAL TMP_STOKES_U(MAXPTS)
      REAL TMP_STOKES_UV(MAXPTS)
      REAL TMP_LAMBDA(MAXPTS)
C
C The current polarization spectrum
C
      REAL STOKES_I(MAXPTS)
      REAL STOKES_Q(MAXPTS)
      REAL STOKES_QV(MAXPTS)
      REAL STOKES_U(MAXPTS)
      REAL STOKES_UV(MAXPTS)
      REAL LAMBDA(MAXPTS)
      INTEGER NPTS
C
C The command parameters
C
      INTEGER NPARAMS
      REAL PARAMS(*)
C
C Misc.
C
      DOUBLE PRECISION A_I(MAXORDER)
      DOUBLE PRECISION A_Q(MAXORDER)
      DOUBLE PRECISION A_U(MAXORDER)
C
C The continuum bins
C
      REAL CONT_ST(*)
      REAL CONT_EN(*)
      INTEGER NZONES
C
C Misc.
C
C
C
      REAL TOT_BIN_SIZE
      INTEGER SP
      INTEGER ORD,I,J
      INTEGER TMP_NPTS,ST,EN
      DOUBLE PRECISION CONTBIN_I(20)
      DOUBLE PRECISION CONTBIN_Q(20)
      DOUBLE PRECISION CONTBIN_QV(20)
      DOUBLE PRECISION CONTBIN_U(20)
      DOUBLE PRECISION CONTBIN_UV(20)
      DOUBLE PRECISION CONTBIN_W(20)
      DOUBLE PRECISION CONTBIN_IV(20)
      DOUBLE PRECISION CHISQR,AVA
      DOUBLE PRECISION LAM
      DOUBLE PRECISION TEMP_I,TEMP_Q,TEMP_U
      LOGICAL OK
C
C Make sure all the parameters are present; if not prompt.
C
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Stack Entry',PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS = 1
      ENDIF
C
      IF (NPARAMS.EQ.1) THEN
       CALL GET_PARAM('Order',PARAMS(2),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS = 2
      ENDIF
C
      SP = INT(PARAMS(1))
      IF ( (SP.LT.0).OR.(SP.GT.TOP_STK) ) THEN
       CALL WR_ERROR('Stack entry out of range',OUT_LU)
       GOTO 666
      ENDIF
C
      IF (NZONES.EQ.0) THEN
       CALL WR_ERROR('No continuum bins defined',OUT_LU)
       GOTO 666
      ENDIF
C
      ORD = INT(PARAMS(2))
C
      IF (ORD.GT.(MAXORDER+1)) THEN
       CALL WR_ERROR('Order exceeds maximum possible',OUT_LU)
       GOTO 666
      ENDIF
C
      IF ((ORD+1).GE.NZONES) THEN
       CALL WR_ERROR('Not enough continuum bins defined',OUT_LU)
       GOTO 666
      ENDIF 
C
C Fill up the temp arrays.
C
      DO I = 1,STK_NPTS(SP)
       TMP_STOKES_I(I) = STK_STOKES_I(I,SP)
       TMP_STOKES_Q(I) = STK_STOKES_Q(I,SP)
       TMP_STOKES_QV(I) = STK_STOKES_QV(I,SP)
       TMP_STOKES_U(I) = STK_STOKES_U(I,SP)
       TMP_STOKES_UV(I) = STK_STOKES_UV(I,SP)
       TMP_LAMBDA(I) = STK_LAMBDA(I,SP)
      ENDDO
      TMP_NPTS = STK_NPTS(SP)
C
C Sum up the continuum bins to get the nzone points for the fit
C
      DO I = 1,NZONES
        CALL LOCATE(TMP_LAMBDA,TMP_NPTS,CONT_ST(I),ST)
        CALL LOCATE(TMP_LAMBDA,TMP_NPTS,CONT_EN(I),EN)
        IF ( (ST.EQ.0).OR.(EN.EQ.0)) THEN
         CALL WR_ERROR('Continuum bin out of wavelength range',OUT_LU)
         GOTO 666
        ENDIF
        CONTBIN_I(I) = 0.D0
        CONTBIN_Q(I) = 0.D0
        CONTBIN_QV(I) = 0.D0
        CONTBIN_U(I) = 0.D0
        CONTBIN_UV(I) = 0.D0
        TOT_BIN_SIZE = 0.
        CONTBIN_W(I) = DBLE((CONT_ST(I)+CONT_EN(I))/2.)
        DO J = ST,EN
         CONTBIN_I(I) = CONTBIN_I(I)+DBLE(TMP_STOKES_I(J))
         CONTBIN_Q(I) = CONTBIN_Q(I)+DBLE(TMP_STOKES_Q(J))
         CONTBIN_QV(I) = CONTBIN_QV(I)+DBLE(TMP_STOKES_QV(J))
         CONTBIN_U(I) = CONTBIN_U(I)+DBLE(TMP_STOKES_U(J))
         CONTBIN_UV(I) = CONTBIN_UV(I)+DBLE(TMP_STOKES_UV(J))
        ENDDO
        TOT_BIN_SIZE = REAL(EN-ST+1)
        CONTBIN_IV(I) = SQRT(CONTBIN_I(I))
        CONTBIN_I(I) = CONTBIN_I(I)/DBLE(TOT_BIN_SIZE)
        CONTBIN_IV(I) = CONTBIN_IV(I)/DBLE(TOT_BIN_SIZE)
        CONTBIN_Q(I) = CONTBIN_Q(I)/DBLE(TOT_BIN_SIZE)
        CONTBIN_QV(I) = SQRT(CONTBIN_QV(I)/DBLE(TOT_BIN_SIZE))
        CONTBIN_U(I) = CONTBIN_U(I)/DBLE(TOT_BIN_SIZE)
        CONTBIN_UV(I) = SQRT(CONTBIN_UV(I)/DBLE(TOT_BIN_SIZE))
      ENDDO
C
C Normalize the wavelengths to prevent overflow.
C
      AVA = 0.D0
      DO I = 1,NZONES
       AVA = AVA+CONTBIN_W(I)
      ENDDO
      AVA = AVA/DBLE(NZONES)
      DO I = 1,NZONES
       CONTBIN_W(I) = CONTBIN_W(I)/AVA
      ENDDO
C        
C Uses Bevington's polynomial least squares fitting routine
C
      CALL POLFIT(CONTBIN_W,CONTBIN_I,CONTBIN_IV,NZONES,ORD+1,
     &             1,A_I,CHISQR)
      CALL POLFIT(CONTBIN_W,CONTBIN_Q,CONTBIN_QV,NZONES,ORD+1,
     &             1,A_Q,CHISQR)
      CALL POLFIT(CONTBIN_W,CONTBIN_U,CONTBIN_UV,NZONES,ORD+1,
     &             1,A_U,CHISQR)
C
      DO I = 1,ORD+1
       A_I(I) = A_I(I) /AVA**(I-1)
       A_Q(I) = A_Q(I) /AVA**(I-1)
       A_U(I) = A_U(I) /AVA**(I-1)
      ENDDO
C
C Map the fit onto the wavelength array of the current stack.
C
      NPTS = STK_NPTS(SP)
      DO I = 1,NPTS
       LAMBDA(I) = TMP_LAMBDA(I)
       LAM = dble(LAMBDA(I))
       TEMP_I = A_I(ORD+1)
       TEMP_Q = A_Q(ORD+1)
       TEMP_U = A_U(ORD+1)
       DO J = ORD,1,-1
         TEMP_I = TEMP_I*LAM+A_I(J)
         TEMP_Q = TEMP_Q*LAM+A_Q(J)
         TEMP_U = TEMP_U*LAM+A_U(J)
       ENDDO
       STOKES_I(I) = REAL(TEMP_I)
       STOKES_Q(I) = REAL(TEMP_Q)
       STOKES_U(I) = REAL(TEMP_U)
       STOKES_QV(I) = 0.
       STOKES_UV(I) = 0.
      ENDDO
666   CONTINUE
      END
