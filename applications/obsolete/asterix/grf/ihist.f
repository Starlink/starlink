*+  IHIST - histogram pixels inside current box
      SUBROUTINE IHIST(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*     18 Sep 92 : V1.2-1 Fixed bug in data range calculation (DJA)
*     25 Jan 93 : V1.7-0 GCB GFX etc used (RJV)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      REAL MIN,MAX
      REAL HMIN,HMAX
      INTEGER NBIN
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IHIST Version 1.7-0')
*-
      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')

      ELSE

*  get number of bins required
        CALL PAR_GET0I('NBIN',NBIN,STATUS)

*  get dynamic storage for plot
        CALL IMG_GET1D(NBIN,STATUS)
        CALL ARR_INIT1R(0.0,NBIN,%VAL(I_DPTR_1D),STATUS)
        CALL ARR_INIT1R(0.0,NBIN,%VAL(I_VPTR_1D),STATUS)
        CALL ARR_INIT1B(QUAL__GOOD,NBIN,%VAL(I_QPTR_1D),STATUS)
        I_N_1D=NBIN

*  find range of data values
        CALL IHIST_RNG(%VAL(I_DPTR),%VAL(I_QPTR),MIN,MAX,STATUS)

*  estimate bin width and set axis values
        I_XWID_1D=(MAX-MIN)/REAL(NBIN)
        I_XSCALE_1D=I_XWID_1D
        I_XBASE_1D=MIN+0.5*I_XWID_1D
        CALL ARR_REG1R(I_XBASE_1D,I_XSCALE_1D,I_N_1D,%VAL(I_APTR_1D),
     :                                                         STATUS)
        CALL ARR_INIT1R(ABS(I_XSCALE_1D),I_N_1D,%VAL(I_WPTR_1D),STATUS)

*  construct histogram
        CALL IHIST_DOIT(%VAL(I_DPTR),%VAL(I_QPTR),%VAL(I_DPTR_1D),
     :                                                      STATUS)

*  set labels
        I_TITLE_1D='Histogram of count rates'
        I_LABEL_1D='Frequency'
        I_UNITS_1D='Percent'
        I_XLABEL_1D=I_LABEL
        I_XUNITS_1D=I_UNITS

*  plot it
        I_X1_1D=0.0
        I_X2_1D=MAX
        CALL ARR_RANG1R(I_N_1D,%VAL(I_DPTR_1D),HMIN,HMAX,STATUS)
        I_Y1_1D=0.0
        I_Y2_1D=REAL(INT(HMAX+5.0))

        CALL GCB_ATTACH('IMAGE',STATUS)
        CALL IMG_1DGCB(STATUS)
        CALL GCB_SETL('STEP_FLAG',.TRUE.,STATUS)
        CALL GCB_SETL('ERR_FLAG',.FALSE.,STATUS)
        CALL GCB_SETL('POLY_FLAG',.FALSE.,STATUS)
        CALL GCB_SETL('POINT_FLAG',.FALSE.,STATUS)

        CALL GDV_CLEAR(STATUS)
        CALL IMG_PLOT(STATUS)

*  flag display status
        I_DISP_1D=.TRUE.
        I_DISP=.FALSE.
        I_CLEAR=.FALSE.


      ENDIF

      END



*+  IHIST_RNG
      SUBROUTINE IHIST_RNG(D,Q,RMIN,RMAX,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL D(I_NX,I_NY)
      BYTE Q(I_NX,I_NY)
*    Export :
      REAL RMIN,RMAX
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ANDUB
*    Local constants :
*    Local variables :
      INTEGER I,J
*-

      IF (STATUS.EQ.SAI__OK) THEN

        RMIN=1.0E36
        RMAX=-RMIN

        IF (I_BAD) THEN

          DO J=I_IY1,I_IY2
            DO I=I_IX1,I_IX2

              IF (BIT_ANDUB(Q(I,J),I_MASK).EQ.QUAL__GOOD) THEN
                RMIN=MIN(RMIN,D(I,J))
                RMAX=MAX(RMAX,D(I,J))
              ENDIF

            ENDDO
          ENDDO

        ELSE

          DO J=I_IY1,I_IY2
            DO I=I_IX1,I_IX2

              RMIN=MIN(RMIN,D(I,J))
              RMAX=MAX(RMAX,D(I,J))

            ENDDO
          ENDDO

        ENDIF

      ENDIF

      END




*+  IHIST_DOIT
      SUBROUTINE IHIST_DOIT(D,Q,H,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL D(I_NX,I_NY)
      BYTE Q(I_NX,I_NY)
*    Export :
      REAL H(I_N_1D)
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ANDUB
*    Local constants :
*    Local variables :
      INTEGER I,J,K
      REAL TOT
*-

      IF (STATUS.EQ.SAI__OK) THEN

        TOT=0.0

        IF (I_BAD) THEN

          DO J=I_IY1,I_IY2
            DO I=I_IX1,I_IX2

              IF (BIT_ANDUB(Q(I,J),I_MASK).EQ.QUAL__GOOD) THEN
                K=INT(D(I,J)/I_XWID_1D)+1
                H(K)=H(K)+1.0
                TOT=TOT+1.0
              ENDIF

            ENDDO
          ENDDO

        ELSE

          DO J=I_IY1,I_IY2
            DO I=I_IX1,I_IX2

              K=INT(D(I,J)/I_XWID_1D)+1
              H(K)=H(K)+1.0
              TOT=TOT+1.0

            ENDDO
          ENDDO

        ENDIF

*  convert to percentages
        DO K=1,I_N_1D
          H(K)=H(K)/TOT*100.0
        ENDDO


      ENDIF

      END
