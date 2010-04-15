      SUBROUTINE FIND44( MAXLEN, PAUNAM, PBANDS, PCROSS, PINSCA, MENU,
     : STATUS )
*+
*  Name:
*     FIND44

*  Purpose:
*     Routine adds or changes the region size and wavebands required
*     for a given source name. If multiple sources exist  with the same
*     name the user is offered each in turn.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND44( MAXLEN, PAUNAM, PBANDS, PCROSS, PINSCA, MENU,
*     : STATUS )

*  Description:
*     Routine adds or changes the region size and wavebands required
*     for a given source name. If multiple sources exist  with the same
*     name the user is offered each in turn.
*     Repeatedly ask the user for source names to be individually
*     modified until he responds with a !
*     Search for all source details with matching source names
*     For each match found display the source details and ask the
*     user for cross scan and in scan size and wavebands. The
*     original values are offered as default. He can opt for not
*     altering the source by entering ! to any of these
*     parameters.
*     The wavebands are modified by using the subroutine FIND22. In
*     which the existing values are offered as default, and only
*     an input parameter string containing at least one waveband
*     identifiers, 12, 25, 60, 100 is valid.
*     MENU is set to menu 'M'.

*  Arguments:
*     MAXLEN = INTEGER (Given)
*        Number of lines per page on display
*     PAUNAM = CHARACTER * ( * ) (Given)
*        Parameter AUGSOURCENAME for source name to be augmented with
*        region size and wavebands.
*     PBANDS = CHARACTER * ( * ) (Given)
*        Parameter BANDSREQ for bands required entered as 12,25,60,100
*     PCROSS = CHARACTER * ( * ) (Given)
*        Parameter CROSSCAN size of req. region in cross scan direction
*        in arc minutes
*     PINSCA = CHARACTER * ( * ) (Given)
*        Parameter INSCAN size of req. region in in scan direction
*        in arc minutes
*     MENU = CHARACTER * ( * ) (Given)
*        Choice from enter size and wavebands menu
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     FINDCRDD:
*        FIND22, FIND39
*     CHR:
*        CHR_ISNAM, CHR_UCASE
*     ERR:
*        ERR_ANNUL
*     MSG:
*        MSG_FMTC, MSG_OUT
*     PAR:
*        PAR_CANCL, PAR_DEF0R, PAR_GET0C, PAR_GET0R

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     19-SEP-1991 (DCP):
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
      CHARACTER * ( * )  PAUNAM
      CHARACTER * ( * )  PBANDS
      CHARACTER * ( * )  PCROSS
      CHARACTER * ( * )  PINSCA
      CHARACTER * ( 1 )  MENU

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_ISNAM
      LOGICAL CHR_ISNAM ! CHR routine to test whether the string is a
                        ! valid filename

*  Local Variables:
      LOGICAL AUFOUN             ! Source to be augmented has been found
                                 ! flag
      CHARACTER * ( 8 ) AUNAME   ! Name of source to be augmented
      REAL DECROS                ! Cross scan size in arc minutes from
                                 ! individual source for default
      REAL DEINSC                ! In scan size in arc minutes from
                                 ! individual source for default
      REAL ICROS                 ! Cross scan size in arc minutes for
                                 ! this source
      REAL ICROR                 ! Cross scan size in radians for
                                 ! this source
      REAL IINSC                 ! In scan size in arc minutes for
                                 ! this source
      REAL IINSR                 ! In scan size in radians for
                                 ! this source
      INTEGER IJ                 ! DO loop control variable
      INTEGER SOPOS              ! Position of source to be augmented in
                                 ! the source common
      LOGICAL SOWAB( 4 )         ! Temporary storage of waveband
                                 ! required logicals
      LOGICAL WAFOUN             ! Flag indicating that some wavebands
                                 ! are required for this source
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  *********************************************************************
*  Continue getting next source to be augmented with size and
*  waveband details, until the user enters ! for augment source name
*  *********************************************************************

 100  CONTINUE              ! Start of 'DO WHILE' loop

*  Display a heading to indicate next source
      CALL MSG_OUT( ' ', 'Next Source ', STATUS )

*  Set the next source to be augmented has been found flag to .FALSE.
      AUFOUN = .FALSE.

*  Program returns here if the name of the source to be augmented is
*  not a valid filename
 200  CONTINUE

*  *********************************************************************
*  Ask user for name of source to augment with region size and wavebands
*  *********************************************************************
      CALL PAR_GET0C( PAUNAM, AUNAME, STATUS )

*  Change the augment source name to upper case
      CALL CHR_UCASE( AUNAME )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
      CALL PAR_CANCL( PAUNAM, STATUS )

*  Check whether the parameter was abort !!
      IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  *********************************************************************
*  Check whether the source name was ! indicating that the user wants
*  to finish adding source names and wavebands individually
*  *********************************************************************
      IF ( STATUS .NE. PAR__NULL ) THEN

*  Check that the source name is a valid file name and give warning
*  and re input if not
         IF ( .NOT. CHR_ISNAM( AUNAME ) ) THEN
            CALL MSG_OUT( ' ', 'The source name was not a '//
     :      'valid filename, please reenter', STATUS )
            GO TO 200
         END IF

*  *********************************************************************
*  Search through list of existing sources to find one with matching
*  name
*  *********************************************************************
         DO 300 IJ = 1, NOFSO
            IF ( AUNAME .EQ. SONAME(IJ) ) THEN

*  Set the next source to be edited has been found flag to .TRUE.
               AUFOUN = .TRUE.

*  Set the source pointer SOPOS to this position in the source list
               SOPOS = IJ

*  *********************************************************************
*  Display the source and augment it with region size and wavebands
*  *********************************************************************

*  Call FIND39 to list source details of source to be augmented
               CALL FIND39( .TRUE., 1, 1, MAXLEN, SOPOS, .TRUE.,
     :         STATUS )

*  Change the arc minute values for cross scan and in scan to radians
               DEINSC = SOINSZ( SOPOS ) / AMTOR
               DECROS = SOCRSZ( SOPOS ) / AMTOR

*  If the default value for the inscan is zero set it to 120 arc min
               IF ( DEINSC .LE. 0.0 ) DEINSC = 120.0

*  Set the previous cross scan size as dynamic default for the cross
*  scan size.
               CALL PAR_DEF0R( PCROSS, DECROS, STATUS )

*  Obtain the cross scan size in arc minutes from the user
               CALL PAR_GET0R( PCROSS, ICROS, STATUS )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
               CALL PAR_CANCL( PCROSS, STATUS )

*  Set the previous in scan size as dynamic default for the in scan
*  size.
               CALL PAR_DEF0R( PINSCA, DEINSC, STATUS )

*  Obtain the in scan size in arc minutes from the user
               CALL PAR_GET0R( PINSCA, IINSC, STATUS )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
               CALL PAR_CANCL( PINSCA, STATUS )

*  Set source waveband logicals into temporary store to be used in
*  FIND22 both as input for forming default and as output of wavebands
*  required
               SOWAB( 1 ) = SOWAB1( SOPOS )
               SOWAB( 2 ) = SOWAB2( SOPOS )
               SOWAB( 3 ) = SOWAB3( SOPOS )
               SOWAB( 4 ) = SOWAB4( SOPOS )

*  Call FIND22 to ask the user for bands required and translate them
*  into logicals (including using source wavebands required as a
*  default)
               CALL FIND22( PBANDS, .TRUE., SOWAB, WAFOUN, STATUS )

*  Check whether any parameter was abort !!
               IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  Check whether any parameter was ! indicating that the user does not
*  want to augment this particularsource with matching name
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Store the new waveband requirements in the source waveband logicals
                  SOWAB1( SOPOS ) = SOWAB ( 1 )
                  SOWAB2( SOPOS ) = SOWAB ( 2 )
                  SOWAB3( SOPOS ) = SOWAB ( 3 )
                  SOWAB4( SOPOS ) = SOWAB ( 4 )

*  Change the in scan value to radians and store in SOINSZ
                  SOINSZ( SOPOS ) = IINSC * AMTOR
*  Change the cross scan value to radians and store in SOCRSZ
                  SOCRSZ( SOPOS ) = ICROS * AMTOR

*  If an error has occured ie status not O.K. and not NULL
               ELSE IF ( STATUS .NE. PAR__NULL ) THEN
                  CALL ERR_STAT( STATUS )
                  IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
               ELSE

*  If any of the parameters was entered as !, indicating do not augment
*  this source with the augment sourc name, annul any error message,
*  which sets the status to SAI__OK.
                  CALL ERR_ANNUL( STATUS )

               END IF
            END IF
 300     CONTINUE

*  *********************************************************************
*  Check whether the source to be augmented has been found and if not
*  display a warning message
*  *********************************************************************
            IF ( .NOT. AUFOUN ) THEN
               CALL MSG_FMTC( 'C1', 'A8', AUNAME )
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

*  Flush the error buffer which sets the STATUS back to SAI__OK.
               CALL ERR_ANNUL( STATUS )

         END IF

*  The choice is changed to M so that the user can select from the
*  edit source list menu
         MENU = 'M'

      END
