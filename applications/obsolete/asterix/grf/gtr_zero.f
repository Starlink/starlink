*+  GTR_ZERO - reset coordinate transformation details
      SUBROUTINE GTR_ZERO(STATUS)

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
      INTEGER I,IPLOT
*    Global variables :
      INCLUDE 'GTR_CMN'
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO IPLOT=1,GMD__MXPLOT
          DO I=1,4
            VPORT(I,IPLOT)=0.0
            WIND(I,IPLOT)=0.0
          ENDDO
          ABS(IPLOT)=.FALSE.
          SCALED(IPLOT)=.FALSE.
        ENDDO
        NPLOT=0

      ENDIF
      END
