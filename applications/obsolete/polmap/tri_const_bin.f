      SUBROUTINE TRI_CONST_BIN(
     &   BIN_ERR,OLD_LAMBDA,
     &   OLD_STOKES_I,OLD_STOKES_Q,OLD_STOKES_QV,
     &   OLD_STOKES_U,OLD_STOKES_UV,OLD_NPTS,STOKES_I,TMP_I,STOKES_Q,
     &   STOKES_QV,STOKES_U,STOKES_UV,LAMBDA,NPTS,OUT_LU)
C+
C
C Subroutine: 
C
C
C    T R I _ C O N S T _ B I N
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C
C BIN_ERR (<), OLD_LAMBDA (<),
C OLD_STOKES_I (<), OLD_STOKES_Q (<), OLD_STOKES_QV (<),
C OLD_STOKES_U (<), OLD_STOKES_UV (<), OLD_NPTS (<), 
C STOKES_I (>), TMP_I (>), STOKES_Q (>),
C STOKES_QV (>), STOKES_U (>), STOKES_UV (>), LAMBDA (>), NPTS (>),
C OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C Bins a polariztion spectrum to constant error bins.
C
C
C
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
C
      INCLUDE 'array_size.inc'
C
      REAL OLD_LAMBDA(MAXPTS)
      REAL OLD_STOKES_I(MAXPTS)
      REAL OLD_STOKES_Q(MAXPTS)
      REAL OLD_STOKES_QV(MAXPTS)
      REAL OLD_STOKES_U(MAXPTS)
      REAL OLD_STOKES_UV(MAXPTS)
      REAL STOKES_I(MAXPTS)
      REAL TMP_I(MAXPTS)
      REAL STOKES_Q(MAXPTS)
      REAL STOKES_QV(MAXPTS)
      REAL STOKES_U(MAXPTS)
      REAL STOKES_UV(MAXPTS)
      REAL LAMBDA(MAXPTS)
      INTEGER OLD_NPTS
      INTEGER NPTS
C
C
C
      REAL BIN_ERR
C
C
      INTEGER BIN_NO
      INTEGER SPEC_BIN_NO
      REAL BIN_VAR
      INTEGER NO_IN_BIN
C
      LOGICAL BIG_ENOUGH
      LOGICAL MORE_DATA
C
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
        NO_IN_BIN = 0
        LAMBDA(BIN_NO) = OLD_LAMBDA(SPEC_BIN_NO)

        DO WHILE (.NOT.BIG_ENOUGH)
         STOKES_I(BIN_NO) = STOKES_I(BIN_NO)+
     &           OLD_STOKES_I(SPEC_BIN_NO)
         STOKES_Q(BIN_NO) = STOKES_Q(BIN_NO)+
     &           OLD_STOKES_Q(SPEC_BIN_NO)
         STOKES_QV(BIN_NO) = STOKES_QV(BIN_NO)+
     &           OLD_STOKES_QV(SPEC_BIN_NO)
         STOKES_U(BIN_NO) = STOKES_U(BIN_NO)+
     &           OLD_STOKES_U(SPEC_BIN_NO)
         STOKES_UV(BIN_NO) = STOKES_UV(BIN_NO)+
     &           OLD_STOKES_UV(SPEC_BIN_NO)
         BIN_VAR = BIN_VAR+(OLD_STOKES_QV(SPEC_BIN_NO)+
     &                    OLD_STOKES_UV(SPEC_BIN_NO))*0.5
         NO_IN_BIN=NO_IN_BIN+1
         SPEC_BIN_NO = SPEC_BIN_NO+1
         IF (SPEC_BIN_NO.EQ.OLD_NPTS) THEN
         BIG_ENOUGH = .TRUE.
         ELSE
         BIG_ENOUGH = (SQRT(ABS(BIN_VAR)) .LT. 
     &                         BIN_ERR*STOKES_I(BIN_NO)/100.)
         ENDIF
        ENDDO
        LAMBDA(BIN_NO) = 0.5*(LAMBDA(BIN_NO)+OLD_LAMBDA(SPEC_BIN_NO))
        IF (NO_IN_BIN.GT.0) THEN
        TMP_I(BIN_NO)=STOKES_I(BIN_NO)/REAL(NO_IN_BIN)
        ELSE
        TMP_I(BIN_NO)=0.
        ENDIF
        BIN_NO = BIN_NO+1
        MORE_DATA  =  (SPEC_BIN_NO .LT. OLD_NPTS)
       ENDDO
       NPTS = BIN_NO-1
666    CONTINUE
       END 
