*+  ARX_WRITE - write ARD text from group to file
      SUBROUTINE ARX_WRITE(PAR,ID,STATUS)
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
*    Global Variables :
*    Import :
      CHARACTER*(*) PAR
      INTEGER ID
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
*    Functions :
*    Local Constants :
*    Local variables :
      CHARACTER*132 ARDOUT
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  get output file name
        CALL USI_GET0C(PAR,ARDOUT,STATUS)

        CALL GRP_LISTF(ARDOUT,0,0,'ARD text',ID,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ARX_WRITE',STATUS)
        ENDIF

      ENDIF

      END
