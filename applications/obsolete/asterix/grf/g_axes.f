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

      CALL PAR_GET0C('XOPT',XOPT,STATUS)
      CALL PAR_GET0R('XTICK',XTICK,STATUS)
      CALL PAR_GET0I('XDIV',NXSUB,STATUS)

      CALL PAR_GET0C('YOPT',YOPT,STATUS)
      CALL PAR_GET0R('YTICK',YTICK,STATUS)
      CALL PAR_GET0I('YDIV',NYSUB,STATUS)

      CALL PGBOX(XOPT,XTICK,NXSUB,YOPT,YTICK,NYSUB)

      END
