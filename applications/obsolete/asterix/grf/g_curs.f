*+  G_CURS - puts cursor onto plot and retrieves position of point or box
      SUBROUTINE G_CURS(STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :   BHVAD::RJV
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
*    Local Constants :
*    Local variables :
      REAL X1,X2,Y1,Y2
      REAL XV1,XV2,YV1,YV2
      REAL XW1,XW2,YW1,YW2
      CHARACTER*10 CONTEXT
      LOGICAL ABS,SCALED
      LOGICAL MARK
      LOGICAL BOX
      INTEGER PLOT
      LOGICAL ACTIVE
      LOGICAL LEFT,RIGHT
*    Global Variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*-
      CALL GDV_STATUS(ACTIVE,STATUS)
      IF (ACTIVE) THEN
        CALL GCB_GETCONTXT(CONTEXT,STATUS)
        IF (CONTEXT.EQ.' ') THEN
          CALL MSG_PRNT('AST_ERR: no current graphics context')

        ELSEIF (CONTEXT.EQ.'GRAFIX') THEN
*  is current dataset multiple
          IF (G_MULTI) THEN
*  if so get required plot number
            CALL PAR_GET0I('PLOT',PLOT,STATUS)
*  and recreate coordinate transformation
            CALL GTR_RECALL(PLOT,XV1,XV2,YV1,YV2,ABS,
     :                              XW1,XW2,YW1,YW2,SCALED,STATUS)
            IF (STATUS.EQ.SAI__OK) THEN
              IF (ABS) THEN
                CALL PGVSIZE(XV1,XV2,YV1,YV2)
              ELSE
                CALL PGVPORT(XV1,XV2,YV1,YV2)
              ENDIF
              IF (SCALED) THEN
                CALL PGWNAD(XW1,XW2,YW1,YW2)
              ELSE
                CALL PGWINDOW(XW1,XW2,YW1,YW2)
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        CALL PAR_GET0L('MARK',MARK,STATUS)
        CALL PAR_GET0L('BOX',BOX,STATUS)
        X1=0.0
        Y1=0.0

        IF (STATUS.EQ.SAI__OK) THEN

*  get first point
          CALL GFX_CURSOR(X1,Y1,LEFT,RIGHT,STATUS)

*  output to parameter system
          CALL PAR_PUT0R('X1',X1,STATUS)
          CALL PAR_PUT0R('Y1',Y1,STATUS)

          IF (.NOT.BOX.AND.MARK) THEN
            CALL PGPOINT(1,X1,Y1,2)

          ELSEIF (BOX) THEN
*  get second corner of box
            X2=X1
            Y2=Y1

            CALL GFX_CURSOR(X2,Y2,LEFT,RIGHT,STATUS)

*  output to parameter system
            CALL PAR_PUT0R('X2',X2,STATUS)
            CALL PAR_PUT0R('Y2',Y2,STATUS)

            IF (MARK) THEN
              CALL PGMOVE(X1,Y1)
              CALL PGDRAW(X1,Y2)
              CALL PGDRAW(X2,Y2)
              CALL PGDRAW(X2,Y1)
              CALL PGDRAW(X1,Y1)

            ENDIF

          ENDIF
        ENDIF

      ELSE
        CALL MSG_PRNT('AST_ERR: no device currently active')
      ENDIF

      END
