*+  SSO_GETFITEM1<T> - Read a field item of type <COMM>[]
      SUBROUTINE SSO_GETFITEM1<T>( LOC, FLD, ITEM, MAXVAL, VALUE,
     :                                           ACTVAL, STATUS )
*    Description :
*
*     Write a field item for field FLD.
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
      CHARACTER*(*)                ITEM          ! Field item to create
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
      CHARACTER*(DAT__SZLOC)       FLOC          ! Field structure
*-

*    Status ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Locate field
        CALL SSO_LOCFLD( LOC, FLD, FLOC, STATUS )

*      Read value
        CALL CMP_GET1<T>( FLOC, ITEM, MAXVAL, VALUE, ACTVAL, STATUS )

*      Free locators
        CALL DAT_ANNUL( FLOC, STATUS )

*      Tidy up
 99     IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_GETFITEM1<T>', STATUS )
        END IF

      END IF

      END
