*+  GTR_RECALL - recall coordinate transformation details
      SUBROUTINE GTR_RECALL(PLOT,X1,X2,Y1,Y2,A,XW1,XW2,YW1,YW2,S,
     :                                                       STATUS)

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
      INCLUDE 'GMD_PAR'
*    Import :
      INTEGER PLOT
*    Import-export :
*    Export :
      REAL X1,X2,Y1,Y2
      LOGICAL A
      REAL XW1,XW2,YW1,YW2
      LOGICAL S
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
*    Global variables :
      INCLUDE 'GTR_CMN'
*-

      IF (STATUS.EQ.SAI__OK) THEN

        IF (PLOT.LE.NPLOT) THEN
          X1=VPORT(1,PLOT)
          X2=VPORT(2,PLOT)
          Y1=VPORT(3,PLOT)
          Y2=VPORT(4,PLOT)
          A=ABS(PLOT)
          XW1=WIND(1,PLOT)
          XW2=WIND(2,PLOT)
          YW1=WIND(3,PLOT)
          YW2=WIND(4,PLOT)
          S=SCALED(PLOT)
        ELSE
          CALL MSG_PRNT('AST_ERR: invalid plot number')
          STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GTR_RECALL',STATUS)
        ENDIF

      ENDIF
      END
