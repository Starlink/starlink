*+  G_POS - set position of viewport in NDC
      SUBROUTINE G_POS(STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      CHARACTER*1 CH
      REAL X1,X2,Y1,Y2
      REAL XX1,XX2,YY1,YY2
      LOGICAL KEY,LEFT,RIGHT
*-

      CALL USI_GET0L('KEY',KEY,STATUS)

      IF (KEY) THEN
        CALL USI_GET0R('X1',X1,STATUS)
        CALL USI_GET0R('X2',X2,STATUS)
        CALL USI_GET0R('Y1',Y1,STATUS)
        CALL USI_GET0R('Y2',Y2,STATUS)
      ELSE
        CALL PGVPORT(0.0,1.0,0.0,1.0)
        CALL PGWINDOW(0.0,1.0,0.0,1.0)
        CALL MSG_PRNT('Select opposite corners...')
        CALL GFX_CURS(XX1,YY1,LEFT,RIGHT,CH,STATUS)
        CALL GFX_CURS(XX2,YY2,LEFT,RIGHT,CH,STATUS)
        X1=MIN(XX1,XX2)
        X2=MAX(XX1,XX2)
        Y1=MIN(YY1,YY2)
        Y2=MAX(YY1,YY2)
      ENDIF

      CALL PGVPORT(X1,X2,Y1,Y2)

      END
