*+  TSLICE - define a slice for removal or keeping
      SUBROUTINE TSLICE(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*     May 94 : V1.7-0 (RJV)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
      INCLUDE 'TIMLIB(TIM_CMN)'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*1 CH
      REAL XC,YC
      REAL X1,X2
      REAL XL,XR
      INTEGER ISECT
      INTEGER SECT1,SECT2
      LOGICAL KEEP
      LOGICAL SEL(T_MAXSECT)
      LOGICAL LEFT,RIGHT
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'TSLICE Version 1.7-0')
*-
      CALL MSG_PRNT(VERSION)

      IF (.NOT.T_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: time-series system not active')
      ELSEIF (.NOT.T_DISP) THEN
        CALL MSG_PRNT('AST_ERR: time series must be plotted/re-plotted'
     :                   //' before using this command')

      ELSE

        CALL GCB_ATTACH('TIME',STATUS)

*  make note of segments currently selected
        DO ISECT=1,T_MAXSECT
          SEL(ISECT)=T_SEL(ISECT)
        ENDDO

        CALL PAR_GET0L('KEEP',KEEP,STATUS)
        IF (KEEP) THEN
          CALL MSG_PRNT('Select extent of data to keep...')
        ELSE
          CALL MSG_PRNT('Select extent of data to discard...')
        ENDIF
        CALL GFX_CURS(XC,YC,LEFT,RIGHT,CH,STATUS)
        CALL TIM_CURSTOSECT(XC,SECT1,STATUS)
        CALL TIM_CURSTOX(XC,X1,STATUS)
        CALL GFX_CURS(XC,YC,LEFT,RIGHT,CH,STATUS)
        CALL TIM_CURSTOSECT(XC,SECT2,STATUS)
        CALL TIM_CURSTOX(XC,X2,STATUS)
        IF (SECT1.NE.SECT2) THEN
          CALL MSG_PRNT('AST_ERR: extent must be within one segment')
        ELSE
          XL=MIN(X1,X2)
          XR=MAX(X1,X2)
          IF (KEEP) THEN
            CALL TSLICE_KEEP(SECT1,XL,XR,%VAL(T_APTR),%VAL(T_QPTR),
     :                                                       STATUS)
          ELSE
            CALL TSLICE_DISCARD(SECT1,XL,XR,%VAL(T_APTR),%VAL(T_QPTR),
     :                                                         STATUS)
          ENDIF

          T_QOK=.TRUE.
          IF (T_CHOPPED) THEN
            CALL TIM_CHOP(%VAL(T_QPTR),STATUS)
            IF (KEEP) THEN
              DO ISECT=1,T_MAXSECT
                T_SEL(ISECT)=SEL(ISECT)
              ENDDO
            ELSE
              DO ISECT=1,SECT1
                T_SEL(ISECT)=SEL(ISECT)
              ENDDO
              T_SEL(SECT1+1)=.TRUE.
              DO ISECT=SECT1+2,T_MAXSECT
                T_SEL(ISECT)=SEL(ISECT-1)
              ENDDO
            ENDIF
          ENDIF
          CALL TIM_SCALE(%VAL(T_APTR),%VAL(T_WPTR),STATUS)
          CALL GDV_CLEAR(STATUS)
          CALL TIM_PLOT(STATUS)
          IF (STATUS.EQ.SAI__OK) THEN
            T_DISP=.TRUE.
            T_CLEAR=.FALSE.
          ENDIF
        ENDIF
      ENDIF

      END


      SUBROUTINE TSLICE_KEEP(SECT,XL,XR,A,Q,STATUS)
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
      INCLUDE 'TIMLIB(TIM_CMN)'
*    Import :
      INTEGER SECT
      REAL XL,XR
      REAL A(T_NVAL)
*    Import/export :
      BYTE Q(T_NVAL)
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ORUB
*    Local constants :
*    Local variables :
      INTEGER IVAL
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  remove bottom bit
        IVAL=T_SECTPIX(1,SECT)
        DO WHILE (A(IVAL).LE.XL)
          Q(IVAL)=BIT_ORUB(Q(IVAL),QUAL__IGNORE)
          IVAL=IVAL+1
        ENDDO

*  remove top bit
        IVAL=T_SECTPIX(2,SECT)
        DO WHILE (XR.LE.A(IVAL))
          Q(IVAL)=BIT_ORUB(Q(IVAL),QUAL__IGNORE)
          IVAL=IVAL-1
        ENDDO

      ENDIF

      END


      SUBROUTINE TSLICE_DISCARD(SECT,XL,XR,A,Q,STATUS)
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
      INCLUDE 'TIMLIB(TIM_CMN)'
*    Import :
      INTEGER SECT
      REAL XL,XR
      REAL A(T_NVAL)
*    Import/export :
      BYTE Q(T_NVAL)
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ORUB
*    Local constants :
*    Local variables :
      INTEGER IVAL
*-
      IF (STATUS.EQ.SAI__OK) THEN

        DO IVAL=T_SECTPIX(1,SECT),T_SECTPIX(2,SECT)
          IF (A(IVAL).GE.XL.AND.A(IVAL).LT.XR) THEN
            Q(IVAL)=BIT_ORUB(Q(IVAL),QUAL_IGNORE)
          ENDIF
        ENDDO

      ENDIF

      END
