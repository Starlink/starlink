*+  ISAVE - save current image to file
      SUBROUTINE ISAVE(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    21 May 91 : V1.2-1 saves contour levels (RJV)
*     5 Jun 91 : V1.2-2 saves only currently selected region (RJV)
*    20 Jan 93 : V1.7-0 save GRAFIX control stuff (RJV)
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
      PARAMETER (VERSION = 'ISAVE Version 1.7-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')

      ELSE

        CALL USI_ASSOCO('OUT','IMAGE',OLOC,STATUS)

        IF (STATUS.EQ.SAI__OK) THEN
          CALL GCB_ATTACH('IMAGE',STATUS)
          CALL IMG_2DGCB(STATUS)

          CALL MSG_PRNT(' ')
          CALL MSG_PRNT('Saving image...')
          CALL IMG_SAVE(OLOC,STATUS)
        ENDIF

        CALL BDA_RELEASE(OLOC,STATUS)
        CALL USI_ANNUL(OLOC,STATUS)

      ENDIF

      CALL USI_CLOSE()

      END


