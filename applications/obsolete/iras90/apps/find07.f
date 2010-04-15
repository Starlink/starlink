      SUBROUTINE FIND07( MAXLEN, PRETSW, MENU, STATUS )
*+
*  Name:
*     FIND07

*  Purpose:
*     Checks that each source has a size and at least one waveband
*     required.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND07( MAXLEN, PRETSW, MENU, STATUS )

*  Description:
*     Checks that each source has a size and at least one waveband
*     required.
*     If a source is found without these reqirements it is displayed
*     and at the end of the subroutine the user is asked whether he
*     wishes to proceed or to return to adding size and wavebands.

*  Arguments:
*     MAXLEN = INTEGER (Given)
*        Number of lines per page on display
*     PRETSW = CHARACTER * ( * ) (Given)
*        Parameter RETURNSWMENU for user reqires program to return to
*        size and wavebands menu if source details are missing.
*     MENU = CHARACTER * ( 1 ) (Given and Returned)
*        Choice from add size and wavebands menu
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     FINDCRDD:
*        FIND39
*     MSG:
*        MSG_OUT
*     PAR:
*        PAR_CANCL, PAR_GET0L

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     26-SEP-1991 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Arguments Given:
      INTEGER MAXLEN
      CHARACTER * ( * )  PRETSW

*  Arguments Given and Returned:
      CHARACTER * ( 1 )  MENU

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL ALFOUN             ! Flag indicating that size and
                                 ! wavebands have been found for all
                                 ! sources
      LOGICAL SOERRF             ! Source is incorrect flag
      LOGICAL RETSW              ! .TRUE. If return to region size and
                                 ! wavebands menu is required
      INTEGER IJ                 ! DO loop control variable
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the flag indicating wavebands and sizes have been found for all
*  sources to .TRUE.
      ALFOUN = .TRUE.

*  Check each source in the source list
      DO 100 IJ = 1, NOFSO

*  Set the source is incorrect flag to false
         SOERRF = .FALSE.

*  Check that the in scan size is non zero and set source incorrect
*  flag if it is
         IF ( SOINSZ( IJ ) .EQ. 0.0 ) SOERRF = .TRUE.

*  Check that at least one of the wavebands is required (actually the
*  check is that if all the waveband required logicals are false the
*  source incorrect flag should be set)
         IF ( ( .NOT. SOWAB1( IJ) ) .AND.
     :        ( .NOT. SOWAB2( IJ) ) .AND.
     :        ( .NOT. SOWAB3( IJ) ) .AND.
     :        ( .NOT. SOWAB4( IJ) ) ) SOERRF = .TRUE.

*  If the source is incorrect
         IF ( SOERRF ) THEN

*  If this is the first source that is incorrect ie ALFOUN is still set
*  true display a heading to the list of incorrect sources
            IF ( ALFOUN ) THEN
               CALL MSG_OUT( ' ', ' The following sources do not '//
     :         'have a region size, and at least one waveband', STATUS )
               ALFOUN = .FALSE.
            END IF

*  Call FIND39 to list source details of  incorrect source
*  (with display mode = terminal (.true), and a dummy file descriptor
*  (1))
            CALL FIND39(  .TRUE., 1, 1, MAXLEN, IJ, .TRUE., STATUS )

*  Se source to be deleted flag to indicate that source is not to be
*  used in subsequent processing
            SOMADE( IJ ) = .TRUE.
         END IF
 100  CONTINUE

*  When all the sources have been examined check if an error has been
*  found
         IF ( .NOT. ALFOUN ) THEN

*  If the returntoswmenu parameter is entered as null the program
*  returns here for a retry
 200        CONTINUE

*  Reset the status in case this is a retry and the status is PAR__NULL
            STATUS = SAI__OK

*  Ask user whether he wishes to proceed or to return to add region size
*  and wavebands menu
            CALL PAR_GET0L( PRETSW, RETSW, STATUS )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
            CALL PAR_CANCL( PRETSW, STATUS )

*  Check whether the parameter was entered as abort !!, if so exit from
*  the subroutine
            IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  Check whether the parameter was entered as null !
            IF ( STATUS .EQ. PAR__NULL ) GO TO 200

*  If the user wants to return to the region size and wavebands menu
*  set Menu to 'M'
            IF ( RETSW ) THEN
               MENU = 'M'

*  Set all the source to be deleted flags to .FALSE.
               DO 300  IJ = 1, NOFSO
                  SOMADE( IJ ) = .FALSE.
 300           CONTINUE
            END IF

*  Endif is end of if there are errors in any sources ALFOUN = FALSE
         END IF

      END
