*+  TSAVE - save current data to file
      SUBROUTINE TSAVE(STATUS)
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
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) OLOC
      INTEGER ISEG
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'TSAVE Version 1.4-0')
*-
      CALL MSG_PRNT(VERSION)

      IF (.NOT.T_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: time series system not active')

      ELSE

        CALL USI_INIT()

        CALL GCB_ATTACH('TIME',STATUS)

        CALL USI_ASSOCO('OUT','DATA',OLOC,STATUS)

        IF (STATUS.EQ.SAI__OK) THEN
          CALL MSG_PRNT(' ')
          IF (.NOT.T_CHOPPED) THEN
            CALL MSG_PRNT('Saving all data...')
            CALL TIM_SAVEALL(OLOC,STATUS)
          ELSEIF (T_NSEL.EQ.1) THEN
            CALL MSG_PRNT('Saving single segment...')
            ISEG=1
            DO WHILE (.NOT.T_SEL(ISEG))
              ISEG=ISEG+1
            ENDDO
            CALL TIM_SAVESEG(ISEG,OLOC,STATUS)
          ELSE
            CALL MSG_PRNT(
     :            'Saving selected segments to multiple dataset...')
            CALL TIM_SAVEMULT(OLOC,STATUS)
          ENDIF
        ENDIF

        CALL BDA_RELEASE(OLOC,STATUS)
        CALL USI_ANNUL(OLOC,STATUS)

        CALL USI_CLOSE()

      ENDIF

      END
