*+  G_PLOT - draw from current pen position to new position
      SUBROUTINE G_PLOT(STATUS)

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
      CALL USI_GET0L('KEY',KEY,STATUS)

      IF (KEY) THEN
        CALL USI_GET0R('X',X,STATUS)
        CALL USI_GET0R('Y',Y,STATUS)
      ELSE
        CALL MSG_PRNT('Select point to plot to...')
        CALL GFX_CURS(X,Y,LEFT,RIGHT,CH,STATUS)
      ENDIF

      CALL PGDRAW(X,Y)


      END
