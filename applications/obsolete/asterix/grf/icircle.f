*+  ICIRCLE - select a circular region of the plot
      SUBROUTINE ICIRCLE(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*     4 Dec 91 : v1.2-1 defaults offered in cursor mode (RJV)
*     1 Jul 93 : v1.2-2 GTR used (RJV)
*    16 Feb 94 : v1.2-3 IMG_GETCIRC used (RJV)
*    20 Sep 94 : v1.7-0 region mask incorporated (RJV)
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
      REAL XC,YC,RAD
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ICIRCLE Version 1.7-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image currently displayed')
      ELSE

*  make sure transformations are correct
        CALL GTR_RESTORE(STATUS)

*  get circle position
        CALL IMG_GETCIRC('XCENT','YCENT','RAD',XC,YC,RAD,STATUS)

*  set region mask
        CALL IMG_SETWHOLE(STATUS)
        CALL IMG_SETCIRC(XC,YC,RAD,.FALSE.,STATUS)

*  store values
        CALL IMG_STORECIRC(XC,YC,RAD,STATUS)

*  plot circle
        CALL IMG_CIRCLE(XC,YC,RAD,STATUS)


      ENDIF

      CALL USI_CLOSE()

      END

