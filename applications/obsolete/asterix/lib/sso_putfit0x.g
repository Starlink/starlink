*+  SSO_PUTFITEM0<T> - Write a field item of type <COMM>
      SUBROUTINE SSO_PUTFITEM0<T>( LOC, FLD, ITEM, VALUE, STATUS )
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
      <TYPE>                       VALUE         ! Field item value
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)       FLOC          ! Field structure

      LOGICAL                      THERE         ! Object already exists?
*-

*    Status ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Locate field
        CALL SSO_LOCFLD( LOC, FLD, FLOC, STATUS )

*      Erase existing item
        CALL DAT_THERE( FLOC, ITEM, THERE, STATUS )
        IF ( THERE ) THEN
          CALL DAT_ERASE( FLOC, ITEM, STATUS )
        END IF

*      Write value
        CALL HDX_PUT<T>( FLOC, ITEM, 1, VALUE, STATUS )

*      Free locators
        CALL DAT_ANNUL( FLOC, STATUS )

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_PUTFITEM0<T>', STATUS )
        END IF

      END IF

      END
