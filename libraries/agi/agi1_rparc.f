************************************************************************
*+  AGI_1RPARC - Read the contents of a character parameter

      SUBROUTINE AGI_1RPARC ( PICLOC, PTYPE, FOUND, PVAL, STATUS )

*    Description :
*     Read the contents of a character parameter
*
*    Invocation :
*     CALL AGI_1RPARC( PICLOC, PTYPE, FOUND, PVAL, STATUS )
*
*    Method :
*     Check status on entry.
*     If the parameter is present then
*        Read the contents of the parameter into a temporary string.
*        Equate the output string with the temporary string.
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     July 1988
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'

*    Import :

*     Locator to picture
      CHARACTER * ( DAT__SZLOC ) PICLOC

*     Name of parameter to be read
      CHARACTER * ( * ) PTYPE

*    Export :

*     Flag to indicate if parameter has been found
      LOGICAL FOUND

*     Content of paramter. Undefined if .NOT. FOUND
      CHARACTER * ( * ) PVAL

*    Status :
      INTEGER STATUS

*    Local variables :
      CHARACTER * ( DAT__SZLOC ) PARLOC
      CHARACTER * ( AGI__CMAX ) CTEMP
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check if the parameter is present
         PARLOC = ' '
         CALL AGI_1FPAR( PICLOC, PTYPE, PARLOC, FOUND, STATUS )

*   Read contents of element
         IF ( FOUND ) THEN
            CALL DAT_GETC( PARLOC, 0, 0, CTEMP, STATUS )
            PVAL = CTEMP
            CALL DAT_ANNUL( PARLOC, STATUS )
            PARLOC = ' '
         ENDIF

      ENDIF

*      print*, '+++++ AGI_1RPARC +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

