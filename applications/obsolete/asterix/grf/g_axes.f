*+  G_AXES - draw axes
      SUBROUTINE G_AXES(STATUS)

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
      CHARACTER*20 XOPT,YOPT
      REAL XTICK,YTICK
      INTEGER NXSUB,NYSUB
*-

      CALL USI_GET0C('XOPT',XOPT,STATUS)
      CALL USI_GET0R('XTICK',XTICK,STATUS)
      CALL USI_GET0I('XDIV',NXSUB,STATUS)

      CALL USI_GET0C('YOPT',YOPT,STATUS)
      CALL USI_GET0R('YTICK',YTICK,STATUS)
      CALL USI_GET0I('YDIV',NYSUB,STATUS)

      CALL PGBOX(XOPT,XTICK,NXSUB,YOPT,YTICK,NYSUB)

      END
