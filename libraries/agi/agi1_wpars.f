************************************************************************
*+  AGI_1WPARS - Write all the parameters into a picture structure

      SUBROUTINE AGI_1WPARS ( PICLOC, PNAME, COMENT, DEVICE, NDC, WORLD,
     :                        MEMID, STATUS )

*    Description :
*     Write all the parameters into a picture structure
*
*    Invocation :
*     CALL AGI_1WPARS ( PICLOC, PNAME, COMENT, DEVICE, NDC, WORLD,
*    :                  MEMID, STATUS )
*
*    Method :
*     Check status on entry.
*     Write each of the parameters in turn.
*
*    Deficiencies :
*
*    Bugs :
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     Oct 1988
*     Jun 1990  Added MEMID parameter
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*    Import :

*     Locator to picture
      CHARACTER * ( DAT__SZLOC ) PICLOC

*     Name of picture
      CHARACTER * ( * ) PNAME

*     Description of picture contents
      CHARACTER * ( * ) COMENT

*     Array of device coordinates
      REAL DEVICE( 4 )

*     Array of normalised device coordinates
      REAL NDC( 4 )

*     Array of world coordinates
      REAL WORLD( 4 )

*     Memory identifier
      INTEGER MEMID

*    Status :
      INTEGER STATUS
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

*   Write the parameters into the picture structure
         CALL AGI_1WPARC( PICLOC, 'PNAME', PNAME, STATUS )
         CALL AGI_1WPARC( PICLOC, 'COMENT', COMENT, STATUS )
         CALL AGI_1WARPR( PICLOC, 'DEVICE', DEVICE, STATUS )
         CALL AGI_1WARPR( PICLOC, 'NDC', NDC, STATUS )
         CALL AGI_1WARPR( PICLOC, 'WORLD', WORLD, STATUS )
         CALL AGI_1WPARI( PICLOC, 'MEMID', MEMID, STATUS )

      ENDIF

*      print*, '+++++ AGI_1WPARS +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

