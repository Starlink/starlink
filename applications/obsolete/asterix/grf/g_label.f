*+  G_LABEL - write labels for x-axis, y-axis and top of plot
      SUBROUTINE G_LABEL(STATUS)

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
      CHARACTER*80 XLBL,YLBL,TLBL
*-

      CALL USI_GET0C('XLBL',XLBL,STATUS)
      CALL USI_GET0C('YLBL',YLBL,STATUS)
      CALL USI_GET0C('TOPLBL',TLBL,STATUS)

      CALL PGLABEL(XLBL,YLBL,TLBL)

      END
