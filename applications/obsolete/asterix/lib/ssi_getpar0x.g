*+  SSI_GETPAR0<T> - Get a parameter from an SSDS file
      SUBROUTINE SSI_GETPAR0<T>( ID, FILE, PAR, VALUE, STATUS )
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
      INTEGER			ID			!
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
      CHARACTER*(DAT__SZLOC)       LOC           ! SSDS locator
*-

*    Status ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

        CALL ADI1_GETLOC( ID, LOC, STATUS )
        CALL SSO_GETPAR0<T>( LOC, FILE, PAR, VALUE, STATUS )

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL AST_REXIT( 'SSI_GETPAR0<T>', STATUS )
        END IF

      END IF

      END
