
      SUBROUTINE FIND11( ILEVEL, MAXLEN, PCONDE, PDENAM, MENU, SOPOS,
     : STATUS )
*+
*  Name:
*     FIND11

*  Purpose:
*     To delete one, or several, sources from the current list

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND11( ILEVEL, MAXLEN, PCONDE, PDENAM, MENU, SOPOS, STATUS )

*  Description:
*     To delete one, or several, sources from the current list
*
*     The subroutine consists of two sections. The first marks sources
*     for deletion as directed by the user. A flag is set in each source
*     to mark those which are to be deleted. At this stage the user can
*     recover a deleted source by respecifying its name. The second
*     section is that in which the deletions actually take place.
*
*     The user is asked for the name of the source he wishes to delete.
*     This is truncated to 8 characters and changed to upper case, as
*     well as being checked that it is a valid prefix for a file name.
*     The routine searches through the source list and presents to the
*     user all sources whos name matches the one he has specified.
*     Details of the source are displayed to the user and he is asked
*     whether he wishes to delete this source. The subroutine will
*     find and offer the user all the sources in the source list whos
*     name matches that he has specified. This is regardless of whether
*     the. source has already been marked for deletion and offers the
*     user the chance to reclaim any deleted by mistake.
*
*     If the source list is searched and no source with the name the
*     user has specified is found the program will inform the user.
*     The process is repeated until the user enters a ! for the
*     sourcename.
*
*     The second section is that in which the deletions actually take
*     place. Entries lower down the source list are moved up to
*     overwrite sources marked for deletion. Entries left at the end of
*     the list are zeroised and the correct number of sources is
*     calculated.
*
*     The program lists the last page of the source list (if ILEVEL = 2
*     or 4) The routine returns to FIND25 and the user is then given
*     the edit sources menu.

*  Arguments:
*     ILEVEL = INTEGER (Given)
*        Program interaction level
*     MAXLEN = INTEGER
*        Number of lines per page on display
*     PCONDE = CHARACTER * ( * ) (Given)
*        Parameter CONFIRMDELETE for confirmation source displayed is
*        to be deleted
*     PDENAM = CHARACTER * ( * ) (Given)
*        Parameter DELETESOURCENAME for source name to be deleted
*     MENU = CHARACTER * ( 1 )  (Given and Returned)
*        Choice from edit_data menu
*     SOPOS = INTEGER (Given and Returned)
*        Pointer to position in source common in which source is to
*        be deleted
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  External Routines Used:
*     FINDCRDD:
*        FIND27, FIND39
*     CHR:
*        CHR_UCASE
*     ERR:
*        ERR_ANNUL
*     MSG:
*        MSG_FMTC, MSG_OUT
*     PAR:
*        PAR_CANCL, PAR_GET0C, PAR_GET0L
*
*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     11-JUL-1991 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 general constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors
      INCLUDE 'CHR_ERR'          ! CHR routine errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Arguments Given:
      INTEGER ILEVEL
      INTEGER MAXLEN
      CHARACTER * ( * )  PCONDE
      CHARACTER * ( * )  PDENAM
*  Arguments Given and Returned:
      CHARACTER * ( 1 ) MENU
      INTEGER SOPOS
*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_ISNAM
      LOGICAL CHR_ISNAM ! CHR routine to test whether the string is a
                                 ! valid filename
*  Local Variables:
      LOGICAL CONDEL             ! .TRUE. if source is to be deleted
      LOGICAL DEFOUN             ! Source to be deleted has been found
                                 ! flag
      CHARACTER * ( 8 ) DELNAM   ! Name of source to be deleted
      INTEGER IK                 ! DO loop control variable
      INTEGER IL                 ! DO loop control variable
      LOGICAL TSOMAD             ! Temporary store for SOMADE for
                                 ! current source
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  *********************************************************************
*  Continue getting next source to delete details until the user enters
*  ! for source name to delete
*  *********************************************************************

 100     CONTINUE              ! Start of 'DO WHILE' loop

*  Display a heading to indicate next source
         CALL MSG_OUT( ' ', 'Next Source ', STATUS )

*  Set the next source to be deleted has been found flag to .FALSE.
         DEFOUN = .FALSE.

*  Start of loop which checks that the users source name is a valid file
*  name
 200        CONTINUE

*  Ask user for name of source to delete
         CALL PAR_GET0C( PDENAM, DELNAM, STATUS )

*  Change the delete source name to upper case
         CALL CHR_UCASE( DELNAM)

*  Cancel the parameter so that a new value is obtained next time
*  through this section
         CALL PAR_CANCL( PDENAM, STATUS )

*  Check whether the parameter was entered as abort !!, if so exit from
*  the subroutine
         IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  Check whether the source name was ! indicating that the user wants to
*  terminate the editing of sources
         IF ( STATUS .NE. PAR__NULL ) THEN

*  Check that the source name is a valid file name and give warning
*  and re input if not
            IF ( .NOT. CHR_ISNAM( DELNAM ) ) THEN
               CALL MSG_OUT( ' ', 'The source name was not a '//
     :         'valid filename, please reenter', STATUS )
               GO TO 200
            END IF

*  *********************************************************************
*  Search through list of existing sources to find one with matching
*  name
*  *********************************************************************
            DO 300 IK = 1, NOFSO
               IF ( DELNAM .EQ. SONAME(IK) ) THEN

*  Set the next source to be deleted has been found flag to .TRUE.
                  DEFOUN = .TRUE.

*  Set the source pointer SOPOS to this position in the source list
                  SOPOS = IK

*  Store the true delete/not_delete flag and set the value in SOMADE
*  to .TRUE. so that the source details can be displayed
                  TSOMAD = SOMADE( SOPOS )
                  SOMADE( SOPOS ) = .FALSE.

*  Call FIND39 to list source details of source to be deleted (with
*  display mode = terminal (.true), and a dummy file descriptor (1))
                  CALL FIND39( .TRUE., 1, 1, MAXLEN, SOPOS, .FALSE.,
     :            STATUS )

*  Restore the true delete/not_delete flag
                  SOMADE( SOPOS ) = TSOMAD

*  Ask user to confirm that source is to be deleted
                  CALL PAR_GET0L( PCONDE, CONDEL, STATUS )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
                  CALL PAR_CANCL( PCONDE, STATUS )

*  Check whether the parameter was entered as abort !!, if so exit from
*  the subroutine
                  IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  Check whether the parameter was entered as null !
                  IF ( STATUS .EQ. PAR__NULL ) THEN

*  If so reset the STATUS back to SAE__OK but do not change the source
*  delete/not-delete flag
                     CALL ERR_ANNUL( STATUS )

*  If the parameter is O.K
                  ELSE

*  Set the source marked for deletion flag for this source to the
*  confirm parameter value. This allows the user to delete the source
*  or to recover a source he deleted by mistake provided he has not yet
*  entered a ! to the delete sourc name request
                     SOMADE(SOPOS) = CONDEL
                  END IF

*  Check whether a list of sources should be displayed
                  IF ( ( ILEVEL .EQ. 2 ) .OR. ( ILEVEL .EQ. 4 ) ) THEN

*  Call subroutine to display a suitably centered page
                     CALL FIND27( MAXLEN, SOPOS, STATUS )
                  END IF

               END IF
 300        CONTINUE

*  *********************************************************************
*  Check whether the source to be deleted has been found and if not
*  display a warning message
*  *********************************************************************
            IF ( .NOT. DEFOUN ) THEN
               CALL MSG_FMTC( 'C1', 'A8', DELNAM )
               CALL MSG_OUT( ' ',
     :         'WARNING - source to deleted, ^c1, was not found',
     :         STATUS )

            END IF

*  Go to the begining of the next source to be deleted loop
            GO TO 100

         ELSE

*  *********************************************************************
*  If the user enters ! to the source name prompt, meaning no more
*  sources
*  *********************************************************************

*  Flush the error buffer which sets the STATUS back to SAI__OK.
            CALL ERR_ANNUL( STATUS )

*  Set SOPOS to the first position in source common so that it can be
*  used to mark the position into which the next non deleted line shall
*  be put
            SOPOS = 1

*  *********************************************************************
*  Loop to delete marked sources
*  *********************************************************************
            DO 400 IK = 1, NOFSO
               IF ( .NOT. SOMADE( IK ) ) THEN

*  Copy source details to correct position after deleting the not
*  required sources above it.
                  SONAME( SOPOS ) = SONAME( IK )
                  SOTITL( SOPOS ) = SOTITL( IK )
                  SOCO1( SOPOS )  = SOCO1( IK )
                  SOCO2( SOPOS )  = SOCO2( IK )
                  SOCOSY( SOPOS ) = SOCOSY( IK )
                  SORA( SOPOS )   = SORA( IK )
                  SODEC( SOPOS )  = SODEC( IK )
                  SOINSZ( SOPOS ) = SOINSZ( IK )
                  SOCRSZ( SOPOS ) = SOCRSZ( IK )
                  SOWAB1( SOPOS ) = SOWAB1( IK )
                  SOWAB2( SOPOS ) = SOWAB2( IK )
                  SOWAB3( SOPOS ) = SOWAB3( IK )
                  SOWAB4( SOPOS ) = SOWAB4( IK )
                  SOMADE( SOPOS ) = SOMADE( IK )

*  Add 1 to SOPOS to bring it to the next line in source common to be
*  filled
                  SOPOS = SOPOS + 1

               END IF
 400        CONTINUE

*  *********************************************************************
*  Clear the source common freed by deletions
*  *********************************************************************
            DO 500 IL = SOPOS, NOFSO
               SONAME( IL ) = ' '
               SOTITL( IL ) = ' '
               SOCO1( IL )  = ' '
               SOCO2( IL )  = ' '
               SOCOSY( IL ) = ' '
               SORA( IL )   = 0.0
               SODEC( IL )  = 0.0
               SOINSZ( IL ) = 0.0
               SOCRSZ( IL ) = 0.0
               SOWAB1( IL ) = .FALSE.
               SOWAB2( IL ) = .FALSE.
               SOWAB3( IL ) = .FALSE.
               SOWAB4( IL ) = .FALSE.
               SOMADE( IL ) = .FALSE.
 500        CONTINUE

*  Set the number of sources to the correct number ie SOPOS - 1
            NOFSO = SOPOS - 1

         END IF

*  Display final list of sources (with display mode = terminal (.true),
*  and a dummy file descriptor (1))
         IF ( ( ILEVEL .EQ. 2 ) .OR. ( ILEVEL .EQ. 4 ) ) THEN
            CALL FIND39( .TRUE., 1, MAXLEN, MAXLEN, NOFSO, .FALSE.,
     :      STATUS )
         END IF

*  *********************************************************************
*  The choice is changed to M so that the user can select from the
*  edit source list menu
*  *********************************************************************
      MENU = 'M'

      END
