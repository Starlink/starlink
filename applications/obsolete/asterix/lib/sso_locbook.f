*+  SSO_LOCBOOK - Locate an BOOK component based on file number
      SUBROUTINE SSO_LOCBOOK( LOC, FILE, FBLOC, STATUS )
*    Description :
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
      INTEGER                      FILE          ! File number
*
*    Export :
*
      CHARACTER*(DAT__SZLOC)       FBLOC         ! File's bookeeping structure
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)       BLOC          ! BOOK structure
*-

*    Status ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Get BOOK structure
        CALL DAT_FIND( LOC, 'BOOK', BLOC, STATUS )

*      Get the appropriate cell
        CALL DAT_CELL( BLOC, 1, FILE, FBLOC, STATUS )

*      Free BOOK structure
        CALL DAT_ANNUL( BLOC, STATUS )

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_LOCBOOK', STATUS )
        END IF

      END IF

      END
