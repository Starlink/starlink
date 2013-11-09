*+  ARX_PUT - put ARD text into group
      SUBROUTINE ARX_PUT(ID,INDEX,TEXT,STATUS)
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
      CHARACTER*(*) TEXT
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
*    Functions :
*    Local Constants :
*    Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL GRP_PUT(ID,1,TEXT,INDEX,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ARX_PUT',STATUS)
        ENDIF

      ENDIF

      END
