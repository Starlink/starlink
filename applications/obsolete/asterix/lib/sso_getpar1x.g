*+  SSO_GETPAR1<T> - Read a vector parameter of type <COMM>[]
      SUBROUTINE SSO_GETPAR1<T>( LOC, FILE, PAR, MAXVAL, VALUE,
     :                                         ACTVAL, STATUS )
*    Description :
*
*     Read a vector parameter from an SSDS.
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
*     10 Mar 92 : Original (DJA)
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
      INTEGER                      FILE          ! BOOK structure slot
      CHARACTER*(*)                PAR           ! Parameter to read
      INTEGER                      MAXVAL        ! Maximum number of values
*
*    Export :
*
      <TYPE>                       VALUE(*)      ! Field item values
      INTEGER                      ACTVAL        ! Actual number of values
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

*      Locate BOOK structure
        CALL SSO_LOCBOOK( LOC, FILE, BLOC, STATUS )

*      Read value
        CALL CMP_GET1<T>( BLOC, PAR, MAXVAL, VALUE, ACTVAL, STATUS )

*      Free locator
        CALL DAT_ANNUL( BLOC, STATUS )

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_GETPAR1<T>', STATUS )
        END IF

      END IF

      END
