*+  BDI_UNMAP - unmaps all known components within structure
      SUBROUTINE BDI_UNMAP(FID,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Import :
      INTEGER FID	! locator to top level
*    Import/Export :
*    Export :
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER N				! index to top level
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL BDI_INQUIRE(FID,N,STATUS)
        IF (N.GT.0) THEN
          CALL BDA_UNMAP_INT(N,STATUS)
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP('BDA_UNMAP','from BDI_UNMAP',STATUS)
        ENDIF
      ENDIF
      END
