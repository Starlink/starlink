*+  GLOAD - load data into graphics system
      SUBROUTINE GLOAD(STATUS)
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
      CHARACTER*(DAT__SZLOC) ILOC
      CHARACTER*80 GDAT
      CHARACTER*20 DEV
      INTEGER NX,NY
      LOGICAL ACTIVE
      LOGICAL PRIM
      LOGICAL MULTI
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'GLOAD Version 1.7-0')
*-
      CALL MSG_PRNT(VERSION)

*  general initialisation if first time
      IF (.NOT.G_OPEN) THEN
        CALL AST_INIT()

      ELSE
*  close previous data if loaded
        IF (G_MULTI) THEN
          CALL HDS_CLOSE(G_MLOC,STATUS)
        ELSE
          CALL BDA_RELEASE(G_LOC,STATUS)
          CALL HDS_CLOSE(G_LOC,STATUS)
        ENDIF
        G_OPEN=.FALSE.
      ENDIF

*  get input image
      CALL PAR_GET0C('INP',GDAT,STATUS)
      CALL HDS_OPEN(GDAT,'UPDATE',ILOC,STATUS)
      CALL DAT_PRIM(ILOC,PRIM,STATUS)

      IF (STATUS.EQ.SAI__OK.AND..NOT.PRIM) THEN

        CALL GMD_QMULT(ILOC,MULTI,STATUS)
        IF (.NOT.MULTI) THEN
          CALL GFX_NDFTYPE(ILOC,STATUS)
        ENDIF

        IF (STATUS.EQ.SAI__OK) THEN
          G_OPEN=.TRUE.
          IF (MULTI) THEN
            G_MULTI=.TRUE.
            G_MLOC=ILOC
          ELSE
            G_MULTI=.FALSE.
            G_LOC=ILOC
          ENDIF
        ELSE
          CALL MSG_PRNT('AST_ERR: no data loaded')
          G_OPEN=.FALSE.
          CALL HDS_CLOSE(ILOC,STATUS)
        ENDIF

*  get graphics device
        CALL GDV_STATUS(ACTIVE,STATUS)
        IF (.NOT.ACTIVE.AND.STATUS.EQ.SAI__OK) THEN
          CALL PAR_GET0C('DEV',DEV,STATUS)
          CALL PAR_GET0I('NX',NX,STATUS)
          CALL PAR_GET0I('NY',NY,STATUS)
          CALL MSG_PRNT('Opening device...')
          CALL GDV_OPEN(DEV,NX,NY,STATUS)
        ENDIF

*  connect Grafix Control Block and load contents from file
        CALL GCB_ATTACH('GRAFIX',STATUS)
        IF (.NOT.MULTI) THEN
          CALL GCB_LOAD(ILOC,STATUS)
        ENDIF

*  set default attributes
        CALL GCB_SETDEF(STATUS)


      ENDIF

      END
