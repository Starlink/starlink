************************************************************************
*+  AGI_1FPAR - Find a parameter in a picture

      SUBROUTINE AGI_1FPAR ( PICLOC, PTYPE, PARLOC, FOUND, STATUS )

*    Description :
*     Find a parameter in a picture structure.
*
*    Invocation :
*     CALL AGI_1FPAR ( PICLOC, PTYPE, PARLOC, FOUND, STATUS )
*
*    Method :
*     Initialise the returned variable
*     Check status on entry.
*     If parameter is in picture structure then
*        Get the locator to the parameter.
*     Endif
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     July 1988
*     September 1990  Initialise FOUND
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*    Import :

*     Locator to picture structure
      CHARACTER * ( DAT__SZLOC ) PICLOC

*     Name of parameter to find
      CHARACTER * ( * ) PTYPE

*    Export :

*     Locator to parameter. Undefined if .NOT. FOUND
      CHARACTER * ( DAT__SZLOC ) PARLOC

*     Flag to indicate if parameter has been found
      LOGICAL FOUND

*    Status :
      INTEGER STATUS
*-

*   Initialise the returned variable
      FOUND = .FALSE.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check if parameter is there
         CALL DAT_THERE( PICLOC, PTYPE, FOUND, STATUS )

*   Get locator to parameter
         PARLOC = ' '
         IF ( FOUND ) THEN
            CALL DAT_FIND( PICLOC, PTYPE, PARLOC, STATUS )
         ENDIF

      ENDIF

*      print*, '+++++ AGI_1FPAR +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

