*+  GTR_RESTORE - restore previous transformation settings
      SUBROUTINE GTR_RESTORE(STATUS)

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
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      REAL X1,X2,Y1,Y2
      LOGICAL A
      REAL XW1,XW2,YW1,YW2
      LOGICAL S
*    Global variables :
      INCLUDE 'GTR_CMN'
*-

      IF (STATUS.EQ.SAI__OK) THEN


        IF (NPLOT.GT.0) THEN
          X1=VPORT(1,NPLOT)
          X2=VPORT(2,NPLOT)
          Y1=VPORT(3,NPLOT)
          Y2=VPORT(4,NPLOT)
          A=ABS(NPLOT)
          XW1=WIND(1,NPLOT)
          XW2=WIND(2,NPLOT)
          YW1=WIND(3,NPLOT)
          YW2=WIND(4,NPLOT)
          S=SCALED(NPLOT)

          IF (A) THEN
            CALL PGVSIZE(X1,X2,Y1,Y2)
          ELSE
            CALL PGVPORT(X1,X2,Y1,Y2)
          ENDIF
          IF (S) THEN
            CALL PGWNAD(XW1,XW2,YW1,YW2)
          ELSE
            CALL PGWINDOW(XW1,XW2,YW1,YW2)
          ENDIF

        ELSE
          CALL MSG_PRNT('AST_ERR: no current transformation')
          STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GTR_RESTORE',STATUS)
        ENDIF

      ENDIF
      END
