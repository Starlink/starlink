*+  SSO_CHKFLD - Check existance of field
      SUBROUTINE SSO_CHKFLD( LOC, FLD, OK, STATUS )
*    Description :
*
*     Check presence an old format SSDS field. A wrap up for all the old routines.
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
      CHARACTER*(DAT__SZLOC)       PLOC          ! POSIT structure
      CHARACTER*(DAT__SZLOC)       TLOC          ! Temp structure
*-

*    Status ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Get POSIT structure
        CALL DAT_FIND( LOC, 'POSIT', PLOC, STATUS )

*      Fields inside IMGCOR
        IF ( ( FLD .EQ. 'X_CORR' ) .OR. ( FLD .EQ. 'Y_CORR' ) ) THEN

*        Locate the IMGCOR box
          CALL DAT_THERE( PLOC, 'IMG_COORDS', OK, STATUS )
          IF ( OK ) THEN
            CALL DAT_FIND( PLOC, 'IMG_COORDS', TLOC, STATUS )

*          Find field
            CALL DAT_THERE( TLOC, FLD(1:1), OK, STATUS )
            CALL DAT_ANNUL( TLOC, STATUS )

          END IF

*      Fields inside CELCOR
        ELSE IF ( ( FLD .EQ. 'RA' ) .OR. ( FLD .EQ. 'DEC' ) ) THEN

*        Locate the CELCOR box
          CALL DAT_THERE( PLOC, 'CEL_COORDS', OK, STATUS )
          IF ( OK ) THEN
            CALL DAT_FIND( PLOC, 'CEL_COORDS', TLOC, STATUS )

*          Find field
            CALL DAT_THERE( TLOC, FLD, OK, STATUS )
            CALL DAT_ANNUL( TLOC, STATUS )

          END IF

*      Stuff just one level down
        ELSE

*        Locate the item
          CALL DAT_THERE( PLOC, FLD, OK, STATUS )

        END IF

*      Free locators
        CALL DAT_ANNUL( PLOC, STATUS )

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_CHKFLD', STATUS )
        END IF

      END IF

      END
