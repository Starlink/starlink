*+  IGUI - interface with GUI
      SUBROUTINE IGUI(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*        rjv@star.sr.bham.ac.uk
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
      CHARACTER*132 ACTION,VALUE
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IGUI Version 1.8-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      CALL USI_GET0C('ACTION',ACTION,STATUS)

      CALL USI_CLOSE()

      END
