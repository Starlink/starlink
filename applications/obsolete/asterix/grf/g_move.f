*+  G_MOVE - change current pen position
      SUBROUTINE G_MOVE(STATUS)

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
      REAL X,Y
      LOGICAL KEY,LEFT,RIGHT
*-
      CALL PAR_GET0L('KEY',KEY,STATUS)


      IF (KEY) THEN
        CALL PAR_GET0R('X',X,STATUS)
        CALL PAR_GET0R('Y',Y,STATUS)
      ELSE
        CALL MSG_PRNT('Select position to move to...')
        CALL GFX_CURS(X,Y,LEFT,RIGHT,CH,STATUS)
      ENDIF

      CALL PGMOVE(X,Y)


      END
