************************************************************************
*+  AGI_1WPARI - Write an integer parameter to the database

      SUBROUTINE AGI_1WPARI ( PICLOC, PTYPE, PVAL, STATUS )

*    Description :
*     Write an integer parameter to the database
*
*    Invocation :
*     CALL AGI_1WPARI( PICLOC, PTYPE, PVAL, STATUS )
*
*    Method :
*     Check status on entry.
*     If the given parameter is not present then
*        Create the parameter.
*     Endif
*     Write the contents of the array into the parameter field.
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     Jun 1990
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

*     Name of parameter
      CHARACTER * ( * ) PTYPE

*     Value to write to the parameter
      INTEGER PVAL

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'agi_locs'

*    Local variables :
      LOGICAL FOUND

      CHARACTER * ( DAT__SZLOC ) PARLOC
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check if the parameter is present
         PARLOC = ' '
         CALL AGI_1FPAR( PICLOC, PTYPE, PARLOC, FOUND, STATUS )

*   If the structure is not there then create it
         IF ( .NOT. FOUND ) THEN
            CALL DAT_NEW0I( PICLOC, PTYPE, STATUS )
            CALL DAT_FIND( PICLOC, PTYPE, PARLOC, STATUS )
         ENDIF

*   Put value into element
         CALL DAT_PUT0I( PARLOC, PVAL, STATUS )
         CALL DAT_ANNUL( PARLOC, STATUS )
         PARLOC = ' '

*   Indicate that the database has been updated
         FLUSH = .TRUE.

      ENDIF

*      print*, '+++++ AGI_1WPARI +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

