      SUBROUTINE FIND28( DISFIL, MAXLEN, PDISFI, PNEXTP, PSLFIL,
     :   SURVEY, MENU, STATUS )
*  Name:
*     FIND28

*  Purpose:
*     To display a paged list of source details

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND28( DISFIL, MAXLEN, PDISFI, PNEXTP, PSLFIL,
*     :   SURVEY, MENU, STATUS )

*  Description:
*     To display a list of source details.
*
*     The program determines whether the display is to be made to the
*     terminal or written to a file. This is done by looking at DISFIL.
*
*     The current setting of DISFIL is first stored to be restored at
*     the end of the subroutine.
*
*     If DISFIL is A (ask) the user is asked to supply a value of
*     B (both), D (display only) or F (file only).
*
*     If DISFIL is B (both) or D (display only) then the program calls
*     the subroutine FIND39 to display each page. In calling FIND39 the
*     display mode is set to terminal. It waits for the user to respond
*     to the request for the parameter NEXTPAGE before displaying the
*     next page.
*
*     If DISFIL is B (both) or F (file only) then the program asks the
*     user for a file name, and opens the file. It then calls FIND39 to
*     write the complete list. In calling FIND39 the display mode is set
*     to write to file. The program then closes the file.

*  Arguments:
*     DISFIL = CHARACTER * ( * ) (Given)
*        Value of the DISPLAYORFILE parameter
*     MAXLEN = INTEGER
*        Number of lines per page on display
*     PDISFI = CHARACTER * ( * ) (Given)
*        Parameter DISPLAYORFILE
*     PNEXTP = CHARACTER * ( * ) (Given)
*        Parameter NEXTPAGE to trigger next page of display
*     PSLFIL = CHARACTER * ( * ) (Given)
*        Parameter SOURCELISTFILE file name to which to file a source
*        list.
*     SURVEY = LOGICAL (Given)
*        If the region size and wavebands are to be printed this is set
*        .TRUE.
*     MENU = CHARACTER * ( 1 ) (Given and Returned)
*        Choice from  either data_to_use or edit_data menu
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     FINDCRDD:
*        FIND39
*     ERR:
*        ERR_ANNUL
*     FIO:
*        FIO_ASSOC, FIO_CANCL, FIO_DEACT
*     MSG:
*        MSG_OUT
*     PAR:
*        PAR_CANCL, PAR_CHOIC, PAR_DEF0C, PAR_GET0L, PAR_PROMT

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
      INCLUDE 'FIO_PAR'          ! FIO constants
      INCLUDE 'FIO_ERR'          ! FIO errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD


*  Arguments Given:
      CHARACTER * ( * )  DISFIL
      INTEGER MAXLEN
      CHARACTER * ( * )  PDISFI
      CHARACTER * ( * )  PNEXTP
      CHARACTER * ( * )  PSLFIL
      LOGICAL SURVEY

*  Arguments Given and Returned:
      CHARACTER * ( 1 ) MENU

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( FIO__SZMOD ) ACMODE ! Access mode for FIO
      INTEGER FD                 ! File descriptor from FIO
      INTEGER IM                 ! DO loop control variable
      INTEGER LALINE             ! Last line to be listed
      INTEGER NOLINE             ! Number of lines on last page of list
      LOGICAL NEXTPA             ! Output of a parameter which triggers
                                 ! next page of display
      INTEGER RECSZ              ! Record size for FIO (0=default)
      CHARACTER * ( 1 )TEMPDF    ! Temporary store for DISFIL
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If there are no sources display error message
      IF ( NOFSO .EQ. 0 ) THEN
         CALL MSG_OUT( ' ',
     :   'WARNING - Current list of sources is empty', STATUS )
      ELSE

*  *********************************************************************
*  Check the current value of the display or file parameter
*  *********************************************************************

*  First store the current value for restoring at the end of the
*  subroutine.
         TEMPDF = DISFIL

*  If the value of DISFIL is A ( ask ), or another invalid value
*  (ie not B, D or F ) ask user for a new value
 100     CONTINUE                 ! Start of 'DO WHILE' loop
         IF ( DISFIL .NE. 'B' ) THEN
            IF ( DISFIL .NE. 'D' ) THEN
               IF ( DISFIL .NE. 'F' ) THEN
                  CALL PAR_DEF0C( PDISFI, 'D', STATUS )
                  CALL PAR_PROMT( PDISFI, 'List to:- Display(D),'//
     :            ' File(F), Both(B), Neither(!)', STATUS )
                  CALL PAR_CHOIC( PDISFI, 'D', 'D,F,B', .FALSE.,
     :            DISFIL, STATUS )

*  Cancel the parameter for next time through
                  CALL PAR_CANCL( PDISFI, STATUS )

*  If the user enters abort !! return from the subroutine
                  IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  If the display of file parameter is null then do not display or
*  write an output file. Go to the tidy up at the end of the subroutine
                  IF ( STATUS .EQ. PAR__NULL ) THEN
                     GO TO 400
*  Else go back to begining of check value of display or file
                  ELSE
                     GO TO 100
                  END IF
*  End if for if not B,D or F (or null)
               END IF
            END IF
         END IF

*  *********************************************************************
*  If DISFIL is B ( both) or D ( display) display output to terminal
*  ********************************************************************
         IF ( ( DISFIL .EQ. 'B' ) .OR. ( DISFIL .EQ. 'D' ) ) THEN

*  *********************************************************************
*  List all the sources by pages
*  *********************************************************************
            DO 200 IM = 1, NOFSO, MAXLEN
               LALINE = IM + MAXLEN - 1

*  *********************************************************************
*  If this is the last page ie the number of sources for the page is
*  less than MAXLEN
*  *********************************************************************
               IF ( LALINE .GE. NOFSO ) THEN
                  LALINE = NOFSO
                  NOLINE = NOFSO - IM + 1
                  CALL FIND39( .TRUE. , 1, NOLINE, MAXLEN, LALINE,
     :            SURVEY, STATUS )
               ELSE

*  *********************************************************************
*  If this is a normal full page
*  *********************************************************************
                  CALL FIND39( .TRUE., 1, MAXLEN, MAXLEN, LALINE,
     :            SURVEY, STATUS )

*  Ask user if he wants the next page
                  CALL PAR_GET0L( PNEXTP, NEXTPA, STATUS )

*  Cancel the parameter for next time through
                  CALL PAR_CANCL( PNEXTP, STATUS )

*  Check status
*  If STATUS is abort !!
                  IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  If STATUS is null ! or reply to next page is no
                  IF ( ( STATUS .EQ. PAR__NULL )
     :            .OR. ( .NOT. NEXTPA ) ) THEN

*  Reset status so that the program can continue
                     IF ( STATUS .EQ. PAR__NULL )
     :                  CALL ERR_ANNUL( STATUS )

*  Jump out of page loop
                     GO TO 300

*  End of status check
                  END IF

               END IF

*  *********************************************************************
*  End of loop for each page
*  *********************************************************************
 200        CONTINUE

*  If next page parameter is entered as !, the program ceases to
*  display further pages and jumps out to here
 300        CONTINUE
         END IF


*  *********************************************************************
*  If DISFIL is B ( both) or F ( file ) put output to file
*  ********************************************************************
         IF ( ( ( DISFIL .EQ. 'B' ) .OR. ( DISFIL .EQ. 'F' ) ) ) THEN

*  Use default Fortran maximum record size ( 133 bytes)
            RECSZ = 0

*  Use access mode write
            ACMODE = 'WRITE'

*  *********************************************************************
*  Ask the user for an output file name. If this is ! the data is not
*  stored. If a valid file name is given the file is opened for writing.
*  *********************************************************************
            CALL FIO_ASSOC( PSLFIL, ACMODE, 'FORTRAN', RECSZ,
     :      FD, STATUS )

*  If the user enters abort !! return from the subroutine
            IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  If the filename association parameter is null then do not write an
*  output file. Annul the error which sets the STATUS back to SAI__OK
*  and display a message.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', ' The source positions will not be '//
     :        'saved, but you can continue processing', STATUS )

*  If a file is supplied by the user then write the source details to it
            ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  ********************************************************************
*  Call FIND39 to list all sources to the file
*  *********************************************************************
               CALL FIND39( .FALSE., FD, NOFSO, MAXLEN, NOFSO,
     :         SURVEY, STATUS )

*  *********************************************************************
*  Check whether an error has occured
*  *********************************************************************
               IF ( STATUS .NE. SAI__OK ) THEN

*  Annul any error message , which sets the status to SAI__OK.
                  CALL ERR_ANNUL( STATUS )
                END IF

*  Close the file and cancel the parameter
                CALL FIO_CANCL( PSLFIL, STATUS )

*  Deactivate FIO
                CALL FIO_DEACT( STATUS )

            END IF
         END IF
      END IF

*  If the DISPLAYORFILE parameter is entered as null, indicating that
*  no displays are to be produced the subroutine go to this point
 400  CONTINUE

*  Check for PAR__NULL and if it is annul any error which sets
*  the STATUS back to SAI__OK.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  Restore the correct value of DISFIL
      DISFIL = TEMPDF

*  The choice is changed to M so that the user can select from the
*  edit source list menu
      MENU = 'M'

      END
