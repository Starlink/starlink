*+  SSI_LOCFLD - Locate an SSO field
      SUBROUTINE SSI_LOCFLD( ID, FLD, FID, STATUS )
*    Description :
*
*     Locate an old format SSDS field. A wrap up for all the old routines.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     17 Jun 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC)       LOC           ! SSDS locator
      CHARACTER*(*)                FLD           ! Field to find
*
*    Export :
*
      CHARACTER*(DAT__SZLOC)       FLOC          ! Field locator
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER ID
      INTEGER FID
*-

      IF(STATUS.EQ.SAI__OK) THEN
        CALL ADI1_GETLOC(ID,LOC,STATUS)
        CALL SSO_LOCFLD( LOC, FLD, FLOC, STATUS )
        CALL ADI1_PUTLOC( FLOC, FID, STATUS )
        IF ( STATUS.NE.SAI__OK ) THEN
          CALL AST_REXIT( 'SSI_LOCFLD', STATUS )
        ENDIF
      ENDIF
      END
