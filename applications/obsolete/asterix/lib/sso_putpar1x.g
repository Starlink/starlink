*+  SSO_PUTPAR1<T> - Write a vector parameter to an SSDS file
      SUBROUTINE SSO_PUTPAR1<T>( LOC, FILE, PAR, NVAL, VALUE, STATUS )
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
      CHARACTER*(*)                PAR           ! Parameter to create
      INTEGER                      NVAL          ! Number of values
      <TYPE>                       VALUE(*)      ! Parameter values
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

*      Create component
        CALL DAT_NEW( BLOC, PAR, '<HTYPE>', 1, NVAL, STATUS )

*      Write values
        CALL CMP_PUT1<T>( BLOC, PAR, NVAL, VALUE, STATUS )

*      Free locator
        CALL DAT_ANNUL( BLOC, STATUS )

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_PUTPAR1<T>', STATUS )
        END IF

      END IF

      END
