*+  GLOAD - load data into graphics system
      SUBROUTINE GLOAD(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*     18 Sep 1995 V2.0-0 : ADI port (DJA)
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
      CHARACTER*132	GDAT
      CHARACTER*20 DEV
      INTEGER NX,NY
      INTEGER			IFID			! Input dataset
      LOGICAL ACTIVE
      LOGICAL MULTI
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'GLOAD Version 2.0-0')
*-
      CALL MSG_PRNT(VERSION)

*  general initialisation if first time
      IF (.NOT.G_OPEN) THEN
        CALL AST_INIT()

      ELSE
*  close previous data if loaded
        CALL USI_INIT()
        IF (G_MULTI) THEN
          CALL ADI_ERASE(G_MFID,STATUS)
        ELSE
          CALL ADI_ERASE(G_ID,STATUS)
        ENDIF
        G_OPEN=.FALSE.

      END IF

*  Get input dataset
      CALL USI_GET0C( 'INP', GDAT, STATUS )
      CALL ADI_FOPEN( GDAT, 'MultiGraph|BinDS', 'UPDATE', IFID,
     :                STATUS )

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GMI_QMULT(IFID,MULTI,STATUS)
        IF (.NOT.MULTI) THEN
          CALL GFX_NDFTYPE(IFID,STATUS)
        ENDIF

        IF (STATUS.EQ.SAI__OK) THEN
          G_OPEN=.TRUE.
          G_MULTI = MULTI
          IF (MULTI) THEN
            G_MFID=IFID
          ELSE
            G_ID=IFID
          ENDIF
        ELSE
          CALL MSG_PRNT('AST_ERR: no data loaded')
          G_OPEN=.FALSE.
          CALL ADI_FCLOSE(IFID,STATUS)
        ENDIF

*  get graphics device
        CALL GDV_STATUS(ACTIVE,STATUS)
        IF (.NOT.ACTIVE.AND.STATUS.EQ.SAI__OK) THEN
          CALL USI_GET0C('DEV',DEV,STATUS)
          CALL USI_GET0I('NX',NX,STATUS)
          CALL USI_GET0I('NY',NY,STATUS)
          CALL MSG_PRNT('Opening device...')
          CALL GDV_OPEN(DEV,NX,NY,STATUS)
        ENDIF

*  connect Grafix Control Block and load contents from file
        CALL GCB_ATTACH('GRAFIX',STATUS)
        IF (.NOT.MULTI) THEN
          CALL GCB_FLOAD(IFID,STATUS)
        ENDIF

*  set default attributes
        CALL GCB_SETDEF(STATUS)

      ENDIF

      CALL USI_CLOSE()

      END
