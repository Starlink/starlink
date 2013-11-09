*+  SSO_PUTFITEM1<T> - Write a field item of type <COMM>
      SUBROUTINE SSO_PUTFITEM1<T>( LOC, FLD, ITEM, NVAL, VALUE, STATUS )
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
      INTEGER                      NVAL          ! Number of values
      <TYPE>                       VALUE(*)      ! Field item values
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

*      Create new object
        CALL DAT_NEW( FLOC, ITEM, '<HTYPE>', 1, NVAL, STATUS )

*      Write values
        CALL CMP_PUT1<T>( FLOC, ITEM, NVAL, VALUE, STATUS )

*      Free locators
        CALL DAT_ANNUL( FLOC, STATUS )

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_PUTFITEM1<T>', STATUS )
        END IF

      END IF

      END
