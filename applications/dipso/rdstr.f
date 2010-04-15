      SUBROUTINE RDSTR( COMM, PROMPT, DEFVAL, VALUE, STATUS )
*+
* Name:
*     RDSTR

*  Purpose:
*     Read a string from the user

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDSTR( COMM, PROMPT, DEFVAL, VALUE, STATUS )

*  Description:
*     The full prompt string is made up by appending the command
*     string, prompt, and default value. Three spaces are included
*     infront of the command name, and ":  " is included between the
*     command and prompt string. Trailing and leading spaces in the
*     command and prompt strings are ignored. The string " ? " is
*     appended to the end of the prompt string. The default value (if
*     any) is inclued between "/" characters.
*
*     A string is then read from standard input. If an I/O error occurs
*     it is reported and STATUS returned set to the corresponding
*     value. If a single or double exclamation is supplied, the status
*     values PAR__NULL or PAR__ABORT is returned. If an End-of-file is
*     detected on read, then PAR__NULL is returned. If an error occurs,
*     the VALUE argument is left unchanged.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The command name.
*     PROMPT = CHARACTER * ( * ) (Given)
*        The prompt string.
*     DEFVAL = CHARACTER * ( * ) (Given)
*        The default character string to use if blank value is
*        supplied.
*     VALUE = CHARACTER * ( * ) (Given and Returned)
*        The character string obtained from the user.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR__ constants

*  Arguments Given:
      CHARACTER * ( * ) COMM
      CHARACTER * ( * ) PROMPT
      CHARACTER * ( * ) DEFVAL

*  Arguments Given and Returned:
      CHARACTER * ( * ) VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :     EXCLAM*80,            ! Supplied string with no spaces
     :     FULLPR*255,           ! Full prompt string.
     :     LVALUE*255            ! Supplied text

      INTEGER
     :     F,                    ! Index of first non-blank character
     :     IOS,                  ! Fortran i/o status value
     :     L,                    ! Index of last non-blank character
     :     PRLEN                 ! Length of full prompt string

      LOGICAL
     :     MORE                  ! Has a value yet to be obtained?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct the full prompt string. Initialise it to hold three spaces.
*  (or 4 if on a VAX - the first space is used as a carriage control
*  character and so isn't displayed).
      FULLPR = ' '
      PRLEN = 3
C     PRLEN = 4

*  If a command name has been given, append it to the full prompt string
*  and convert it to upper case.
      CALL CHR_FANDL( COMM, F, L )
      IF( F .LE. L ) THEN
         CALL CHR_APPND( COMM( F : L ), FULLPR, PRLEN )
         CALL CHR_APPND( ':', FULLPR, PRLEN )
         CALL CHR_UCASE( FULLPR )
         PRLEN = PRLEN + 2
      END IF

*  If a non-blank prompt string was given, append it to the full prompt
*  string.
      CALL CHR_FANDL( PROMPT, F, L )
      IF( F .LE. L ) THEN
         CALL CHR_APPND( PROMPT( F : L ), FULLPR, PRLEN )
         PRLEN = PRLEN + 1
      END IF

*  If a default value has been given, add it to the full prompt.
      IF( DEFVAL .NE. ' ' ) THEN
         CALL CHR_APPND( '/', FULLPR, PRLEN )
         CALL CHR_APPND( DEFVAL, FULLPR, PRLEN )
         CALL CHR_APPND( '/', FULLPR, PRLEN )
         PRLEN = PRLEN + 1
      END IF

*  Add a question mark to the full prompt string.
      CALL CHR_APPND( '?', FULLPR, PRLEN )
      PRLEN = PRLEN + 1

*  Loop until a value is obtained.
      MORE = .TRUE.
      DO WHILE( MORE )

         CALL GETINP( 2, LVALUE, FULLPR( : PRLEN), IOS )

*  If an I/O error has occurred, report it and abort.
         IF( IOS .NE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'RDSTR_ERR1', 'Error obtaining a string '//
     :                    'from the terminal', STATUS )
            GO TO 999
         END IF

*  If a blank value was supplied, accept the default value if one was
*  supplied, and indicate that a value has been obtained.
         IF( LVALUE .EQ. ' '  ) THEN
            IF( DEFVAL .NE. ' ' ) THEN
               LVALUE = DEFVAL
               MORE = .FALSE.
            END IF

*  If the supplied value is not blank, return it.
         ELSE
            MORE = .FALSE.
         END IF

      END DO

*  Get a copy of the value with all spaces removed.
      EXCLAM = LVALUE
      CALL CHR_RMBLK( EXCLAM )

*  If a single exclamation was given, report a null value.
      IF( EXCLAM .EQ. '!' ) THEN
         STATUS = PAR__NULL
         CALL ERR_REP( 'RDSTR_ERR3', 'Null parameter value given.',
     :                 STATUS )

*  If a double exclamation was given, report a parameter abort.
      ELSE IF( EXCLAM .EQ. '!!' ) THEN
         STATUS = PAR__ABORT
         CALL ERR_REP( 'RDSTR_ERR4', 'Parameter request aborted.',
     :                 STATUS )
      END IF

*  If no error has occured, return the value.
      IF( STATUS .EQ. SAI__OK ) VALUE = LVALUE

*  Jump to here if an error occurs.
 999  CONTINUE

      END
