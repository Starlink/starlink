************************************************************************
*+  AGI_1RPARS - Read the parameters defining the picture

      SUBROUTINE AGI_1RPARS ( PICLOC, PNAME, COMENT, DEVICE, NDC, WORLD,
     :                        MEMID, FOUND, STATUS )

*    Description :
*     Read the parameters that describe the picture on the workstation
*
*    Invocation :
*      CALL AGI_1RPARS( PICLOC, PNAME, COMENT, DEVICE, NDC, WORLD,
*     :                 MEMID, FOUND, STATUS )
*
*    Method :
*     Check status on entry.
*     Read each of the parameters in turn.
*
*    Deficiencies :
*     This does not check that the parameter was found, so the output
*     arguments may be undefined. The found argument is returned false
*     if any of the parameters were not found.
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     July 1988
*     June 1990  Added MEMID parameter
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

*    Export :

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

*     Flag indicating if parameter has been found
      LOGICAL FOUND

*    Status :
      INTEGER STATUS

*    Local variables :
      LOGICAL FOUND1, FOUND2, FOUND3, FOUND4, FOUND5, FOUND6
*-

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Read each parameter in turn
         CALL AGI_1RPARC( PICLOC, 'PNAME', FOUND1, PNAME, STATUS )
         CALL AGI_1RPARC( PICLOC, 'COMENT', FOUND2, COMENT, STATUS )
         CALL AGI_1RARPR( PICLOC, 'DEVICE', FOUND3, DEVICE, STATUS )
         CALL AGI_1RARPR( PICLOC, 'NDC', FOUND4, NDC, STATUS )
         CALL AGI_1RARPR( PICLOC, 'WORLD', FOUND5, WORLD, STATUS )
         CALL AGI_1RPARI( PICLOC, 'MEMID', FOUND6, MEMID, STATUS )

*   Flag if any of the parameters were not found
         FOUND = FOUND1 .AND. FOUND2 .AND. FOUND3 .AND.
     :           FOUND4 .AND. FOUND5 .AND. FOUND6
      ENDIF

*      print*, '+++++ AGI_1RPARS +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

