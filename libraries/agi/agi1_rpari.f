************************************************************************
*+  AGI_1RPARI - Read the contents of an integer parameter

      SUBROUTINE AGI_1RPARI ( PICLOC, PTYPE, FOUND, PVAL, STATUS )

*    Description :
*     Read the contents of an integer parameter
*
*    Invocation :
*     CALL AGI_1RPARI( PICLOC, PTYPE, FOUND, PVAL, STATUS )
*
*    Method :
*     Check status on entry.
*     If the given parameter is there then
*        Read the contents of the array.
*     Endif
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     July 1990
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

*     Name of parameter to read
      CHARACTER * ( * ) PTYPE

*    Export :

*     Flag to indicate if parameter has been found
      LOGICAL FOUND

*     Content of parameter. Undefined if .NOT. FOUND
      INTEGER PVAL

*    Status :
      INTEGER STATUS

*    Local variables :
      CHARACTER * ( DAT__SZLOC ) PARLOC
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check if the parameter is present
         PARLOC = ' '
         CALL AGI_1FPAR( PICLOC, PTYPE, PARLOC, FOUND, STATUS )

*   Read contents of element
         IF ( FOUND ) THEN
            CALL DAT_GET0I( PARLOC, PVAL, STATUS )
            CALL DAT_ANNUL( PARLOC, STATUS )
            PARLOC = ' '
         ENDIF

      ENDIF

*      print*, '+++++ AGI_1RPARI +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

