*+  GFX_DEFCOLS - get default colour table
      SUBROUTINE GFX_DEFCOLS(TAB,STATUS)

*    Description :
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
*    Import :
*    Import-export :
*    Export :
      REAL TAB(3,16)
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      REAL DCOL(3,16)
      INTEGER I,J
*    Local data :
      DATA DCOL/
     & 0.00,0.00,0.00, 0.00,0.00,1.00, 0.50,0.00,1.00, 1.00,0.00,1.00,
     & 1.00,0.00,0.50, 1.00,0.00,0.00, 1.00,0.26,0.00, 1.00,0.50,0.00,
     & 1.00,0.76,0.00, 0.76,1.00,0.00, 0.50,1.00,0.00, 0.50,1.00,0.50,
     & 0.00,1.00,0.50, 0.00,1.00,1.00, 0.50,1.00,1.00, 1.00,1.00,1.00/
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO I=1,3
          DO J=1,16
            TAB(I,J)=DCOL(I,J)
          ENDDO
        ENDDO

      ENDIF
      END
