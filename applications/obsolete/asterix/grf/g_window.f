*+  G _WINDOW - set window in world coords
      SUBROUTINE G_WINDOW(STATUS)

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
      REAL X1,X2,Y1,Y2
*-

      CALL USI_GET0R('X1',X1,STATUS)
      CALL USI_GET0R('X2',X2,STATUS)
      CALL USI_GET0R('Y1',Y1,STATUS)
      CALL USI_GET0R('Y2',Y2,STATUS)

      CALL PGWINDOW(X1,X2,Y1,Y2)

      END
