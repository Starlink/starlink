*+  SSO_GETPAR0<T> - Get a parameter from an SSDS file
      SUBROUTINE SSO_GETPAR0<T>( LOC, FILE, PAR, VALUE, STATUS )
*
*    Description :
*
*     The value of the named parameter is returned for the FILE'th
*     component of the SSDS pointed to by LOC. If the SSDS is not a
*     set then FILE should be unity.
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
*
*    Export :
*
      <TYPE>                       VALUE         ! Parameter value
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

*      Get value
        CALL CMP_GET0<T>( BLOC, PAR, VALUE, STATUS )

*      Free locator
        CALL DAT_ANNUL( BLOC, STATUS )

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_GETPAR0<T>', STATUS )
        END IF

      END IF

      END
