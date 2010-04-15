      SUBROUTINE FIND13( CONREQ, ILEVEL, MAXLEN, PCONAD, PEDNAM,
     :                  PSCOR1, PSCOR2, PSCOSY, PSNAME, PSTITL, SCS,
     :                  MENU, SOPOS, STATUS )
*+
*  Name:
*     FIND13

*  Purpose:
*     Edits one or many existing sources in a list of sources.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND13( CONREQ, ILEVEL, MAXLEN, PCONAD, PEDNAM, PSCOR1,
*     :     PSCOR2, PSCOSY, PSNAME, PSTITL, SCS, MENU, SOPOS, STATUS )

*  Description:
*     Edits one or many existing sources in a list of sources.
*     The user is asked for the name of the source he wishes to edit.
*     This is truncated to 8 characters and changed to upper case, as
*     well as being checked that it is a valid prefix for a file name.
*     The routine searches through the source list and presents to the
*     user all sources whos name matches the one he has specified.
*     For each source name found the routine FIND02 is called to edit
*     the details of the source position etc. If the user does not
*     want to edit this particular source he should enter ! to a
*     parameter request within FIND02. FIND02 will return to this
*     subroutine and the next source with the same name will be found.
*     If the source list is searched and no source with the name the
*     user has specified is found the program will inform the user.
*     The process is repeated until the user enters a ! for the
*     filename.
*     The program lists the last page of the source list (if ILEVEL = 2
*     or 4 ) The routine returns to FIND25 and the user is then given
*     the edit sources menu.
*
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
*     PEDNAM = CHARACTER * ( * ) (Given)
*        Parameter EDITSOURCENAME for source name to be edited
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
*        MSG_FMTC, MSG_OUT
*     PAR:
*        PAR_CANCL, PAR_GET0C

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
      CHARACTER * ( * )  PEDNAM
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

*  Local Variables:
      LOGICAL EDFOUN             ! Source to be edited has been found
                                 ! flag
      CHARACTER * ( 8 ) EDNAME   ! Name of source to be edited
      INTEGER IJ                 ! DO loop control variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  *********************************************************************
*  Continue getting next source to edit details until the user enters !
*  for edit source name
*  *********************************************************************

 100     CONTINUE              ! Start of 'DO WHILE' loop

*  Display a heading to indicate next source
         CALL MSG_OUT( ' ', 'Next Source ', STATUS )

*  Set the next source to be edited has been found flag to .FALSE.
         EDFOUN = .FALSE.

*  Program returns here if the name of the source to be edited is not a
*  valid filename
 200     CONTINUE

*  *********************************************************************
*  Ask user for name of source to edit
*  *********************************************************************
         CALL PAR_GET0C( PEDNAM, EDNAME, STATUS )

*  Change the edit source name to upper case
         CALL CHR_UCASE( EDNAME )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
         CALL PAR_CANCL( PEDNAM, STATUS )

*  Check whether the parameter was abort !!
         IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  Check whether the source name was ! indicating that the user wants to
*  terminate the editing of sources
         IF ( STATUS .NE. PAR__NULL ) THEN

*  Check that the source name is a valid file name and give warning
*  and re input if not
            IF ( .NOT. CHR_ISNAM( EDNAME ) ) THEN
               CALL MSG_OUT( ' ', 'The source name was not a '//
     :         'valid filename, please reenter', STATUS )
               GO TO 200
            END IF

*  Search through list of existing sources to find one with matching
*  name
            DO 300 IJ = 1, NOFSO
               IF ( EDNAME .EQ. SONAME(IJ) ) THEN

*  Set the next source to be edited has been found flag to .TRUE.
                  EDFOUN = .TRUE.

*  Set the source pointer SOPOS to this position in the source list
                  SOPOS = IJ
*  *********************************************************************
*  Call FIND02 to edit the source
*  *********************************************************************
                  CALL FIND02( CONREQ, ILEVEL, MAXLEN, .FALSE., PCONAD,
     :            PSCOR1, PSCOR2, PSCOSY, PSNAME, PSTITL, SCS,
     :            SOPOS, STATUS )

*  Check the status on return from FIND02
                 IF ( STATUS .NE. SAI__OK ) RETURN

               END IF
 300        CONTINUE

*  *********************************************************************
*  Check whether the source to be edited has been found and if not
*  display a warning message
*  *********************************************************************
            IF ( .NOT. EDFOUN ) THEN
               CALL MSG_FMTC( 'C1', 'A8', EDNAME )
               CALL MSG_OUT( ' ',
     :         'WARNING - source to edited, ^c1, was not found',
     :         STATUS )

            END IF

*  Go to the begining of the next source to be edited loop
            GO TO 100

         ELSE
*  *********************************************************************
*  If the user enters ! to the source name prompt, meaning no more
*  sources
*  *********************************************************************
*  Annul any errors which sets the STATUS back to SAI__OK.
            CALL ERR_ANNUL( STATUS )

*  Display final list of sources (with display mode = terminal (.true),
*  and a dummy file descriptor (1))
            IF ( ( ILEVEL .EQ. 2 ) .OR. ( ILEVEL .EQ. 4 ) ) THEN
               CALL FIND39( .TRUE., 1, MAXLEN, MAXLEN, NOFSO, .FALSE.,
     :         STATUS )
*  Check whether the STATUS on return from listing sources is PAR__NULL
*  and if so annul the error message
               IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
            END IF

         END IF

*  The choice is changed to M so that the user can select from the
*  edit source list menu
         MENU = 'M'


      END
