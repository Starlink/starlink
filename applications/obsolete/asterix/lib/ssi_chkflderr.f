*+  SSO_CHKFLDERR - Check existance of field error
      SUBROUTINE SSO_CHKFLDERR( LOC, FLD, OK, STATUS )
*    Description :
*
*     Check presence an old format SSDS field error. Assumes presence of
*     field itself.
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
      LOGICAL                      OK            ! Field exists?
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)       FLOC          ! Field structure
*-

*    Status ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Field exists?
        CALL SSO_CHKFLD( LOC, FLD, OK, STATUS )
        IF ( OK ) THEN

*        Locate field
          CALL SSO_LOCFLD( LOC, FLD, FLOC, STATUS )

*        Does error exist?
          CALL HDX_OK( FLOC, 'ERROR', OK, STATUS )

*        Free locator
          CALL DAT_ANNUL( FLOC, STATUS )

        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Field '//FLD//' does not exist', STATUS )
        END IF

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_CHKFLDERR', STATUS )
        END IF

      END IF

      END
