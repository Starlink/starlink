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
      INTEGER N
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'GCLOSE Version 1.7-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (G_OPEN) THEN
        CALL GCB_ATTACH('GRAFIX',STATUS)
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

      CALL GCB_QCONTXT(N,STATUS)
      IF (N.GT.0) THEN
        CALL MSG_PRNT('****** Graphics device not closed ******')
        CALL MSG_PRNT('****** other contexts are active  ******')
      ELSE
        CALL GDV_CLOSE(STATUS)
      ENDIF

      CALL USI_CLOSE()

      END


