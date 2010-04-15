      SUBROUTINE CONST_BIN(TOP_STK,NPARAMS,PARAMS,STK_LAMBDA,
     &   STK_STOKES_I,STK_STOKES_Q,STK_STOKES_QV,
     &   STK_STOKES_U,STK_STOKES_UV,STK_NPTS,STOKES_I,STOKES_Q,
     &   STOKES_QV,STOKES_U,STOKES_UV,LAMBDA,NPTS,OUT_LU)
C+
C
C Subroutine:
C
C      C O N S T _ B I N
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C TOP_STK (<), NPARAMS (<), PARAMS (<), STK_LAMBDA(<),
C STK_STOKES_I (<), STK_STOKES_Q (<), STK_STOKES_QV (<),
C STK_STOKES_U (<), STK_STOKES_UV (<), STK_NPTS (<), STOKES_I(>),
C STOKES_Q (<), STOKES_QV (<), STOKES_U (<), STOKES_UV (<),
C LAMBDA (<), NPTS (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C Bins a stack entry polarization spectrum to bins of constant error size.
C This routine bins all Stokes parameters to this bin size. The bin size
C will vary according roughly to root I.
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
      INCLUDE 'array_size.inc'
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
C
      INTEGER TOP_STK
C
C The current arrays
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
C
      REAL BIN_ERR
C
      INTEGER SP
      INTEGER BIN_NO,SPEC_BIN_NO
      REAL BIN_SIZE
      REAL BIN_VAR
C
      LOGICAL BIG_ENOUGH
      LOGICAL MORE_DATA
      LOGICAL OK
C
C Make sure the parameters are set up. Prompt for missing ones.
C
      IF (NPARAMS.EQ.0) THEN
         CALL GET_PARAM('Stack entry',PARAMS(1),OK,OUT_LU)
         IF (.NOT.OK) GOTO 666
         NPARAMS = 1
      ENDIF
C
      IF (NPARAMS.EQ.1) THEN
         CALL GET_PARAM('Bin size',PARAMS(2),OK,OUT_LU)
         IF (.NOT.OK) GOTO 666
         NPARAMS = 2
      ENDIF
C
      IF ((INT(PARAMS(1)).GT.TOP_STK).OR.(INT(PARAMS(1)).LT.1)) THEN
       CALL WR_ERROR('Stack entry out of range',OUT_LU)
       GOTO 666
      ENDIF
C
      SP = INT(PARAMS(1))
      BIN_ERR = PARAMS(2)
      BIN_NO = 1
      SPEC_BIN_NO = 1
      BIG_ENOUGH = .FALSE.
      MORE_DATA = .TRUE.
C
      DO WHILE (MORE_DATA)
        STOKES_I(BIN_NO) = 0.
        STOKES_Q(BIN_NO) = 0.
        STOKES_U(BIN_NO) = 0.
        STOKES_QV(BIN_NO) = 0.
        STOKES_UV(BIN_NO) = 0.
        BIN_VAR = 0.
        BIG_ENOUGH = .FALSE.
        BIN_SIZE = 0.
        LAMBDA(BIN_NO) = STK_LAMBDA(SPEC_BIN_NO,SP)
C
C If the error on the bin is too small include more...
C
        DO WHILE (.NOT.BIG_ENOUGH)
         STOKES_I(BIN_NO) = STOKES_I(BIN_NO)+
     &           STK_STOKES_I(SPEC_BIN_NO,SP)
         STOKES_Q(BIN_NO) = STOKES_Q(BIN_NO)+
     &           STK_STOKES_Q(SPEC_BIN_NO,SP)
         STOKES_QV(BIN_NO) = STOKES_QV(BIN_NO)+
     &           STK_STOKES_QV(SPEC_BIN_NO,SP)
         STOKES_U(BIN_NO) = STOKES_U(BIN_NO)+
     &           STK_STOKES_U(SPEC_BIN_NO,SP)
         STOKES_UV(BIN_NO) = STOKES_UV(BIN_NO)+
     &           STK_STOKES_UV(SPEC_BIN_NO,SP)
         BIN_VAR = BIN_VAR+(STK_STOKES_QV(SPEC_BIN_NO,SP)+
     &                    STK_STOKES_UV(SPEC_BIN_NO,SP))*0.5
         SPEC_BIN_NO = SPEC_BIN_NO+1
         BIN_SIZE = BIN_SIZE+1.
         IF (SPEC_BIN_NO.EQ.STK_NPTS(SP)) THEN
         BIG_ENOUGH = .TRUE.
         ELSE
         BIG_ENOUGH = (SQRT(ABS(BIN_VAR)) .LT.
     &                         BIN_ERR*STOKES_I(BIN_NO)/100.)
         ENDIF
        ENDDO
C
C The new bin is now of the correct error. Place it in the
C current arrays.
C
        LAMBDA(BIN_NO) = 0.5*(LAMBDA(BIN_NO)+STK_LAMBDA(SPEC_BIN_NO,SP))
        STOKES_I(BIN_NO) = STOKES_I(BIN_NO)/BIN_SIZE
        STOKES_Q(BIN_NO) = STOKES_Q(BIN_NO)/BIN_SIZE
        STOKES_QV(BIN_NO) = STOKES_QV(BIN_NO)/BIN_SIZE**2
        STOKES_U(BIN_NO) = STOKES_U(BIN_NO)/BIN_SIZE
        STOKES_UV(BIN_NO) = STOKES_UV(BIN_NO)/BIN_SIZE**2
        BIN_NO = BIN_NO+1
        MORE_DATA  =  (SPEC_BIN_NO .LT. STK_NPTS(SP))
       ENDDO
       NPTS = BIN_NO-1
C
666    CONTINUE
       END
