*+  TZAP - remove single point
      SUBROUTINE TZAP(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*      May 94: V1.7-0 (RJV)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*1 CH
      INTEGER ISECT
      REAL XC,YC
      REAL X
      LOGICAL LEFT,RIGHT
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'TZAP Version 1.7-0')
*-
      CALL MSG_PRNT(VERSION)

      IF (.NOT.T_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: time-series system not active')
      ELSEIF (.NOT.T_DISP) THEN
        CALL MSG_PRNT('AST_ERR: time series must be plotted/re-plotted'
     :                   //' before using this command')

      ELSE

        CALL USI_INIT()

        CALL GCB_ATTACH('TIME',STATUS)

        CALL MSG_PRNT('Select points to remove( press X to eXit)...')

        CH=' '
        DO WHILE (CH.NE.'X'.AND.CH.NE.'x')
        CALL GFX_CURS(XC,YC,LEFT,RIGHT,CH,STATUS)
          IF (CH.NE.'X'.AND.CH.NE.'x') THEN
            CALL TIM_CURSTOSECT(XC,ISECT,STATUS)
            CALL TIM_CURSTOX(XC,X,STATUS)
            CALL TZAP_REMOVE(ISECT,X,%VAL(T_APTR),%VAL(T_WPTR),
     :                 %VAL(T_DPTR),%VAL(T_VPTR),%VAL(T_QPTR),STATUS)
          ENDIF

        ENDDO

        CALL USI_CLOSE()

      ENDIF

      END




      SUBROUTINE TZAP_REMOVE(ISECT,X,A,W,D,V,Q,STATUS)
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
      INCLUDE 'TIM_CMN'
*    Import :
      INTEGER ISECT
      REAL X
      REAL A(T_NVAL)
      REAL W(T_NVAL)
      REAL D(T_NVAL)
      REAL V(T_NVAL)
*    Import/export :
      BYTE Q(T_NVAL)
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ORUB
*    Local constants :
*    Local variables :
      REAL HWID
      INTEGER IVAL
      INTEGER ECOL,PCOL
      LOGICAL EOK,POK
      LOGICAL FOUND
*-
      IF (STATUS.EQ.SAI__OK) THEN

        FOUND=.FALSE.
        IVAL=1
        DO WHILE (IVAL.LE.T_NVAL.AND..NOT.FOUND)
          HWID=W(IVAL)/2.0
          IF (X.GE.A(IVAL)-HWID.AND.X.LE.A(IVAL)+HWID) THEN
            FOUND=.TRUE.
          ELSE
            IVAL=IVAL+1
          ENDIF
        ENDDO

*  set quality to BAD and unplot
        IF (FOUND) THEN
          Q(IVAL)=BIT_ORUB(Q(IVAL),QUAL__BAD)
          T_QOK=.TRUE.
          CALL TIM_SETTR(ISECT,STATUS)

          CALL GCB_GETI('ERR_COLOUR',EOK,ECOL,STATUS)
          CALL GCB_SETI('ERR_COLOUR',0,STATUS)
          CALL GCB_GETI('POINT_COLOUR',POK,PCOL,STATUS)
          CALL GCB_SETI('POINT_COLOUR',0,STATUS)

          CALL GFX_ERR(1,1,1,A(IVAL),W(IVAL),D(IVAL),V(IVAL),STATUS)
          CALL GFX_POINT(1,1,1,A(IVAL),D(IVAL),STATUS)

          IF (EOK) THEN
            CALL GCB_SETI('ERR_COLOUR',ECOL,STATUS)
          ELSE
            CALL GCB_CANI('ERR_COLOUR',STATUS)
          ENDIF
          IF (POK) THEN
            CALL GCB_SETI('POINT_COLOUR',PCOL,STATUS)
          ELSE
            CALL GCB_CANI('POINT_COLOUR',STATUS)
          ENDIF

          CALL TIM_SETTR(0,STATUS)
        ENDIF

      ENDIF

      END
