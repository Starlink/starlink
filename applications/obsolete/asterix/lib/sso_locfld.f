*+  SSO_LOCFLD - Locate an SSO field
      SUBROUTINE SSO_LOCFLD( LOC, FLD, FLOC, STATUS )
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
          CALL DAT_FIND( PLOC, 'IMG_COORDS', TLOC, STATUS )

*        Find field
          CALL DAT_FIND( TLOC, FLD(1:1), FLOC, STATUS )
          CALL DAT_ANNUL( TLOC, STATUS )

*      Fields inside CELCOR
        ELSE IF ( ( FLD .EQ. 'RA' ) .OR. ( FLD .EQ. 'DEC' ) ) THEN

*        Locate the CELCOR box
          CALL DAT_FIND( PLOC, 'CEL_COORDS', TLOC, STATUS )

*        Find field
          CALL DAT_FIND( TLOC, FLD, FLOC, STATUS )
          CALL DAT_ANNUL( TLOC, STATUS )

*      Stuff just one level down
        ELSE

*        Locate the item
          CALL DAT_FIND( PLOC, FLD, FLOC, STATUS )

        END IF

*      Free locators
        CALL DAT_ANNUL( PLOC, STATUS )

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_LOCFLD', STATUS )
        END IF

      END IF

      END
