************************************************************************
*+  AGI_1WPACT - Indicate which is the currently active picture.

      SUBROUTINE AGI_1WPACT ( WKNAME, VALUE, STATUS )

*    Description :
*     Indicate which is the currently active picture.
*
*    Invocation :
*     CALL AGI_1WPACT( WKNAME, VALUE, STATUS )
*
*    Method :
*     Check status on entry.
*     Get a locator to the top level database structure.
*     Get a loctor to the workstation.
*     Get a locator to the picture active element.
*     Store the current active picture in here.
*     Tidy up
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     July 1988
*     Amended July 1989  Read database locator from common block
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'agi_nam'

*    Import :

*     Name of the workstation
      CHARACTER * ( * ) WKNAME

*     Number of currently active picture
      INTEGER VALUE

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'agi_locs'

*    Local variables :
      LOGICAL FOUND

      CHARACTER * ( DAT__SZLOC ) TMPLOC, WKSLOC
*-

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Find the workstation
         CALL AGI_1FDB( FOUND, STATUS )
         IF ( FOUND ) THEN
            WKSLOC = ' '
            CALL AGI_1FWORK( WKNAME, WKSLOC, FOUND, STATUS )
            IF ( FOUND ) THEN

*   Find the 'pactive' structure
               TMPLOC = ' '
               CALL DAT_THERE( WKSLOC, AGI__ACNAM, FOUND, STATUS )
               IF ( FOUND ) THEN
                  CALL DAT_FIND( WKSLOC, AGI__ACNAM, TMPLOC, STATUS )

*   Put the new active number into the database
                  CALL DAT_PUTI( TMPLOC, 0, 0, VALUE, STATUS )
                  CALL DAT_ANNUL( TMPLOC, STATUS )
                  TMPLOC = ' '

*   Indicate that the database has been updated
                  FLUSH = .TRUE.
               ENDIF
               CALL DAT_ANNUL( WKSLOC, STATUS )
               WKSLOC = ' '

            ENDIF
         ENDIF
      ENDIF

*      print*, '+++++ AGI_1WPACT +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

