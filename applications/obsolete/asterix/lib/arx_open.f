*+  ARX_OPEN - open a group for storing ARD text
      SUBROUTINE ARX_OPEN(MODE,ID,STATUS)
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
      INCLUDE 'PAR_ERR'
      INCLUDE 'GRP_PAR'
*    Global Variables :
*    Import :
      CHARACTER*(*) MODE
*    Import-Export :
*    Export :
      INTEGER ID
*    Status :
      INTEGER STATUS
*    External references :
*    Functions :
*    Local Constants :
*    Local variables :
      CHARACTER*1 LMODE
*-
      IF (STATUS.EQ.SAI__OK) THEN

        LMODE=MODE(1:1)
        CALL CHR_UCASE(LMODE)
        IF (LMODE.EQ.'R') THEN
          CALL GRP_NEW('ARD input',ID,STATUS)
        ELSEIF (LMODE.EQ.'W') THEN
          CALL GRP_NEW('ARD output',ID,STATUS)
        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ARX_OPEN',STATUS)
        ENDIF

      ENDIF

      END
