*+  GCLOSE - unload data from graphics system
      SUBROUTINE GCLOSE(STATUS)
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
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'GCLOSE Version 1.7-0')
*-
      CALL MSG_PRNT(VERSION)

      IF (G_OPEN) THEN
        IF (G_MULTI) THEN
          CALL HDS_CLOSE(G_MLOC,STATUS)
        ELSE
          CALL GCB_SAVE(G_LOC,STATUS)
          CALL BDA_RELEASE(G_LOC,STATUS)
          CALL HDS_CLOSE(G_LOC,STATUS)
        ENDIF
        G_OPEN=.FALSE.
        CALL GCB_DETACH(STATUS)
      ENDIF

      END
