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
      CHARACTER*(DAT__SZLOC) OLOC
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ISAVE1D Version 1.7-0')
*-
      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (I_N_1D.LE.0) THEN
        CALL MSG_PRNT('AST_ERR: no current 1D data')

      ELSE

        CALL USI_ASSOCO('OUT','DATA',OLOC,STATUS)

        IF (STATUS.EQ.SAI__OK) THEN
          CALL GCB_ATTACH('IMAGE',STATUS)
          CALL IMG_1DGCB(STATUS)

          CALL MSG_PRNT(' ')
          CALL MSG_PRNT('Saving 1D data...')
          CALL IMG_SAVE1D(OLOC,STATUS)
        ENDIF

        CALL BDA_RELEASE(OLOC,STATUS)
        CALL USI_ANNUL(OLOC,STATUS)

      ENDIF

      END
