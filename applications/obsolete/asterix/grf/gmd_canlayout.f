*+  GMD_CANLAYOUT - cancel LAYOUT attributes for multiple dataset
      SUBROUTINE GMD_CANLAYOUT(LOC,STATUS)
*    Description :
*    Method :
*    Authors :
*              (BHVAD::RJV)
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC) LOC	! locator to dataset
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables :
      LOGICAL THERE			! whether attribute there
*-

      IF (STATUS.EQ.SAI__OK) THEN


*  see if container structure there
        CALL DAT_THERE(LOC,'LAYOUT',THERE,STATUS)

        IF (THERE) THEN
          CALL DAT_ERASE(LOC,'LAYOUT',STATUS)
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GMD_CANLAYOUT',STATUS)
        ENDIF
      ENDIF
      END
