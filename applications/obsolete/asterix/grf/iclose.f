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
*      6 Jan 95 : V1.8-0 closes ARD group (RJV)
*     10 Apr 95 : V1.8-1 closes position group (RJV)
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
      PARAMETER (VERSION = 'ICLOSE Version 1.8-1')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)
	print *,1
      CALL PSF_RELEASE(I_PSF,STATUS)
	print *,2
      CALL PSF_CLOSE(STATUS)
	print *,2
      CALL BDA_ANNUL(I_LOC,STATUS)
	print *,3

      CALL ARX_CLOSE(I_ARD_ID,STATUS)
	print *,4

      IF (I_NPOS.GT.0) THEN
        CALL GRP_DELET(I_POS_ID,STATUS)
        I_NPOS=0
      ENDIF
	print *,5

      CALL GCB_ATTACH('IMAGE',STATUS)
	print *,6
      CALL GCB_DETACH(STATUS)
	print *,7

      CALL GDV_STATUS(ACTIVE,STATUS)
	print *,8
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
	print *,9

      CALL AST_CLOSE()
	print *,10

      END

