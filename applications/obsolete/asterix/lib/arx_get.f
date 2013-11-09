*+  ARX_GET - get ARD text from group
      SUBROUTINE ARX_GET(ID,INDEX,TEXT,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'GRP_PAR'
*    Global Variables :
*    Import :
      INTEGER ID
      INTEGER INDEX
*    Import-Export :
*    Export :
      CHARACTER*(*) TEXT
*    Status :
      INTEGER STATUS
*    External references :
*    Functions :
*    Local Constants :
*    Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL GRP_GET(ID,INDEX,1,TEXT,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ARX_GET',STATUS)
        ENDIF

      ENDIF

      END
