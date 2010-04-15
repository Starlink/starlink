      SUBROUTINE FIND03( CONREQ, ILEVEL, MAXLEN, PCONAD, PSCOR1, PSCOR2,
     :     PSCOSY, PSNAME, PSTITL, SCS, MENU, SOPOS, STATUS )
*+
*  Name:
*     FIND03

*  Purpose:
*     Adds new sources to an empty new source list, or an existing list

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND03( CONREQ, ILEVEL, MAXLEN, PCONAD, PSCOR1, PSCOR2,
*     :     PSCOSY, PSNAME, PSTITL, SCS, MENU, SOPOS, STATUS )

*  Description:
*     Adds new sources to an empty new source list, or an existing
*     list.
*     The pointer to the next free space in source common is updated.
*     The user is asked for a new source name.
*     This is truncated to 8 characters and changed to upper case, as
*     well as being checked that it is a valid prefix for a file name.
*     The routine FIND02 is called to add details of source position
*     etc. to the next free space in source common, and the number of
*     sources is updated.
*     The process is repeated until the user enters a ! for the
*     filename.
*     The program lists the last page of the source list (if ILEVEL = 2
*     or 4 ) The routine returns to FIND25 and the user is then given
*     the edit sources menu.

*  Arguments:
*     CONREQ = LOGICAL (Given)
*        Set .TRUE. if added, and edited, sources are to be confirmed
*     ILEVEL = INTEGER (Given)
*        Program interaction level
*     MAXLEN = INTEGER
*        Number of lines per page on display
*     PCONAD = CHARACTER * ( * ) (Given)
*        Parameter CONFIRMADDEDIT for confirmation that added or edited
*        source has correct details
*     PSCOR1 = CHARACTER * ( * ) (Given)
*        Parameter SOURCECOORD1 for first coordinate of source position
*     PSCOR2 = CHARACTER * ( * ) (Given)
*        Parameter SOURCECOORD2 for second coordinate of source position
*     PSCOSY = CHARACTER * ( * ) (Given)
*        Parameter SOURCECOORDSYS for coordinate system in which source
*        positions are given
*     PSNAME = CHARACTER * ( * ) (Given)
*        Parameter SOURCENAME for source name to be used in EXCRDD file
*        names
*     PSTITL = CHARACTER * ( * ) (Given)
*        Parameter SOURCETITLE for title of source for headings
*     SCS = CHARACTER * ( IRA__SZSCS ) (Given)
*        Value of Coordinate system
*     MENU = CHARACTER * ( 1 )  (Given and Returned)
*        Choice from edit_data menu
*     SOPOS = INTEGER (Given and Returned)
*        Pointer to position in source common in which source is to
*        be entered
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     FINDCRDD:
*        FIND02, FIND39
*     CHR:
*        CHR_ISNAM, CHR_UCASE
*     ERR:
*        ERR_ANNUL
*     MSG:
*        MSG_OUT
*     PAR:
*        PAR_CANCL, PAR_GET0C

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     9-JUL-1991 (DCP):
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
      INCLUDE 'CHR_ERR'          ! CHR routine errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Arguments Given:
      LOGICAL CONREQ
      INTEGER ILEVEL
      INTEGER MAXLEN
      CHARACTER * ( * )  PCONAD
      CHARACTER * ( * )  PSCOR1
      CHARACTER * ( * )  PSCOR2
      CHARACTER * ( * )  PSCOSY
      CHARACTER * ( * )  PSNAME
      CHARACTER * ( * )  PSTITL
      CHARACTER * ( IRA__SZSCS )  SCS
*  Arguments Given and Returned:
      CHARACTER * ( 1 ) MENU
      INTEGER SOPOS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_ISNAM
      LOGICAL CHR_ISNAM ! CHR routine to test whether the string is a
                                 ! valid filename

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  *********************************************************************
*  Continue getting new source details until the user enters ! for
*  source name
*  *********************************************************************
 100     CONTINUE              ! Start of 'DO WHILE' loop

*  Set SOPOS the pointer to the next free space in the source common
*  block to number of sources (NOFSO) plus 1
         SOPOS = NOFSO + 1

*  Check whether the number of sources exceeds the room available for
*  them
         IF ( SOPOS .GT. MAXSO ) THEN

*  Print a message
            CALL MSG_OUT( ' ', 'There is no room for more sources'//
     :      'Please complete the processing for these sources'//
     :      ' and rerun the program to enter more sources ', STATUS )

         ELSE

*  Display a heading to indicate new source
            CALL MSG_OUT( ' ', 'Next Source ', STATUS )

*  Start of loop which checks that the users source name is a valid file
*  name
 200        CONTINUE

*  *********************************************************************
*  Ask user for new source name
*  *********************************************************************
            CALL PAR_GET0C( PSNAME, SONAME(SOPOS), STATUS )

*  Change the source name to upper case
            CALL CHR_UCASE( SONAME(SOPOS) )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
            CALL PAR_CANCL( PSNAME, STATUS )

*  Check whether the parameter was abort !!
            IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  Check whether the source name was ! indicating that the user wants to
*  terminate the input of new sources
            IF ( STATUS .NE. PAR__NULL ) THEN

*  Check that the source name is a valid file name and give warning
*  and re input if not
               IF ( .NOT. CHR_ISNAM( SONAME( SOPOS ) ) ) THEN
                  CALL MSG_OUT( ' ', 'The source name was not a '//
     :            'valid filename, please reenter', STATUS )
                  GO TO 200
               END IF

*  *********************************************************************
*  Call FIND02 to add a new source
*  *********************************************************************
               CALL FIND02( CONREQ, ILEVEL, MAXLEN, .TRUE., PCONAD,
     :         PSCOR1, PSCOR2, PSCOSY, PSNAME, PSTITL, SCS, SOPOS,
     :         STATUS )

*  Check the status on return from FIND02
               IF ( STATUS .NE. SAI__OK ) RETURN

*  Go to the begining of the next new source loop
               GO TO 100

            ELSE
*  *********************************************************************
*  If the user enters ! to the source name prompt, meaning no more
*  sources
*  *********************************************************************

*  Flush the error buffer which sets the STATUS back to SAI__OK.
               CALL ERR_ANNUL( STATUS )

*  Display final list of sources (with display mode = terminal (.true),
*  and a dummy file descriptor (1))
               IF ( ( ILEVEL .EQ. 2 ) .OR. ( ILEVEL .EQ. 4 ) ) THEN
                  CALL FIND39( .TRUE., 1, MAXLEN, MAXLEN, NOFSO,
     :            .FALSE., STATUS )
*  Check whether the STATUS on return from listing sources is PAR__NULL
*  and if so annul the error message
                  IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
               END IF

*  End if for if next source name is par__null indicating no more new
*  sources
            END IF

*  End if for check that there is room for more sources
         END IF

*  The choice is changed to M so that the user can select from the
*  edit source list menu
         MENU = 'M'


      END
