*+  ICLOSE - close down image processing system
      SUBROUTINE ICLOSE(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*      3 Sep 92 : V1.7-0 doesn't close device (RJV)
*      5 Feb 93 : V1.7-1 closes device if only one graphics context (RJV)
*     25 Nov 93 : V1.7-2 used BDA_ANNUL (RJV)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*20 DEV
      INTEGER NCONTXT
      LOGICAL ACTIVE
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ICLOSE Version 1.7-2')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      CALL PSF_RELEASE(I_PSF,STATUS)
      CALL PSF_CLOSE(STATUS)
      CALL BDA_ANNUL(I_LOC,STATUS)

      CALL GCB_ATTACH('IMAGE',STATUS)
      CALL GCB_DETACH(STATUS)

      CALL GDV_STATUS(ACTIVE,STATUS)
      IF (ACTIVE) THEN
        CALL GCB_QCONTXT(NCONTXT,STATUS)
        IF (NCONTXT.GT.0) THEN
          CALL MSG_PRNT('**** graphics device NOT closed ****')
          CALL MSG_PRNT('**** other contexts are active  ****')
        ELSE
          CALL MSG_PRNT('Closing device....')
          CALL GDV_DEVICE(DEV,STATUS)
          CALL MSG_SETC('DEV',DEV)
          CALL GDV_CLOSE(STATUS)
          CALL MSG_PRNT('^DEV now closed')
        ENDIF
      ENDIF

      I_OPEN=.FALSE.

      CALL AST_CLOSE()

      END
