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
      INCLUDE 'ADI_PAR'
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER OFID,ISEG
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'TSAVE Version 2.2-0')
*-
      CALL MSG_PRNT(VERSION)

      IF (.NOT.T_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: time series system not active')

      ELSE

        CALL USI_INIT()

        CALL GCB_ATTACH('TIME',STATUS)

        CALL USI_CREAT('OUT',ADI__NULLID,OFID,STATUS)

        IF (STATUS.EQ.SAI__OK) THEN
          CALL MSG_PRNT(' ')
          IF (.NOT.T_CHOPPED) THEN
            CALL MSG_PRNT('Saving all data...')
            CALL TIM_SAVEALL(OFID,STATUS)
          ELSEIF (T_NSEL.EQ.1) THEN
            CALL MSG_PRNT('Saving single segment...')
            ISEG=1
            DO WHILE (.NOT.T_SEL(ISEG))
              ISEG=ISEG+1
            ENDDO
            CALL TIM_SAVESEG(ISEG,OFID,STATUS)
          ELSE
            CALL MSG_PRNT(
     :            'Saving selected segments to multiple dataset...')
            CALL TIM_SAVEMULT(OFID,STATUS)
          ENDIF
        ENDIF

        CALL USI_ANNUL( 'OUT', STATUS )

        CALL USI_CLOSE()

      ENDIF

      END
