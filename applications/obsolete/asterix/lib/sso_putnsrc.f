*+  SSO_PUTNSRC - Create SSDS POSIT object and set number of sources
      SUBROUTINE SSO_PUTNSRC( LOC, NSRC, STATUS )
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
      INTEGER                      NSRC          ! Number of sources
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)       PLOC          ! POSIT structure
*-

*    Status ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Create and locate POSIT box
        CALL DAT_NEW( LOC, 'POSIT', 'EXTENSION', 0, 0, STATUS )
        CALL DAT_FIND( LOC, 'POSIT', PLOC, STATUS )

*      Set number of sources
        CALL HDX_PUTI( PLOC, 'NSRC', 1, NSRC, STATUS )

*      Free locators
        CALL DAT_ANNUL( PLOC, STATUS )

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_PUTNSRC', STATUS )
        END IF

      END IF

      END
