*+  SSO_PUTPAR0C - Write a _CHAR parameter to an SSDS file
      SUBROUTINE SSO_PUTPAR0C( LOC, FILE, PAR, LEN, VALUE, STATUS )
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
*     19 Jun 91 : Original (DJA)
*
*      9 Mar 92 : Added FILE argument (DJA)
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
      INTEGER                      FILE          ! File slot
      CHARACTER*(*)                PAR           ! Paramneter to create
      INTEGER                      LEN           ! Length of character parameter
      CHARACTER*(*)                VALUE         ! parameter value
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)       BLOC          ! Book structure
*-

*    Status ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Locate BOOK structure
        CALL SSO_LOCBOOK( LOC, FILE, BLOC, STATUS )

*      Create new object
        CALL DAT_NEWC( BLOC, PAR, LEN, 0, 0, STATUS )

*      Write value
        CALL CMP_PUT0C( BLOC, PAR, VALUE, STATUS )

*      Free locator
        CALL DAT_ANNUL( BLOC, STATUS )

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_PUTPAR0C', STATUS )
        END IF

      END IF

      END
