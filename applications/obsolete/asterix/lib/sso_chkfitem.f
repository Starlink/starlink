*+  SSO_CHKFITEM - Check existance and validity of a field item
      SUBROUTINE SSO_CHKFITEM( LOC, FLD, ITEM, OK, STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Aug 91 : Original (DJA)
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
      CHARACTER*(*)                ITEM          ! Field item to create
*
*    Export :
*
      LOGICAL                      OK            ! Field item is ok?
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

*      Locate field
        CALL SSO_LOCFLD( LOC, FLD, FLOC, STATUS )

*      Field component ok?
        CALL HDX_OK( FLOC, ITEM, OK, STATUS )

*      Free locator
        CALL DAT_ANNUL( FLOC, STATUS )

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_CHKFITEM', STATUS )
        END IF

      END IF

      END
