*+  GTR_SAVE - store coordinate transformation details
      SUBROUTINE GTR_SAVE(MULTI,X1,X2,Y1,Y2,A,XW1,XW2,YW1,YW2,S,STATUS)

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
      LOGICAL MULTI
      REAL X1,X2,Y1,Y2
      LOGICAL A
      REAL XW1,XW2,YW1,YW2
      LOGICAL S
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
*    Global variables :
      INCLUDE 'GTR_CMN'
*-

      IF (STATUS.EQ.SAI__OK) THEN

        IF (MULTI) THEN
          NPLOT=NPLOT+1
        ELSE
          NPLOT=1
        ENDIF
        VPORT(1,NPLOT)=X1
        VPORT(2,NPLOT)=X2
        VPORT(3,NPLOT)=Y1
        VPORT(4,NPLOT)=Y2
        ABS(NPLOT)=A
        WIND(1,NPLOT)=XW1
        WIND(2,NPLOT)=XW2
        WIND(3,NPLOT)=YW1
        WIND(4,NPLOT)=YW2
        SCALED(NPLOT)=S
      ENDIF
      END
