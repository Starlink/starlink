*+  ISAVE1D - save current 1D data to file
      SUBROUTINE ISAVE1D(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER			OFID,BID
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ISAVE1D Version 2.0-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')

      ELSEIF (I_N_1D.LE.0) THEN
        CALL MSG_PRNT('AST_ERR: no current 1D data')

      ELSE

        IF ( I_N_AUX .EQ. 0 ) THEN
          CALL BDI_NEW( 'BinDS', 1, I_N_1D, 'REAL', BID, STATUS )
        ELSE
          CALL ADI_NEW0( 'MultiGraph', BID, STATUS )
        END IF
        CALL USI_CREAT( 'OUT', BID, OFID, STATUS )

        IF (STATUS.EQ.SAI__OK) THEN
          CALL GCB_ATTACH('IMAGE',STATUS)
          CALL IMG_1DGCB(STATUS)

          CALL MSG_PRNT(' ')
          CALL MSG_PRNT('Saving 1D data...')
          CALL IMG_SAVE1D(OFID,STATUS)
        ENDIF

        CALL USI_CANCL( 'OUT', STATUS )

      ENDIF

      CALL USI_CLOSE()

      END
