      SUBROUTINE GET0L( PARAMS, POS, OPT, COMM, PROMPT, DEFVAL, VALUE,
     :                  STATUS )
*+
*  Name:
*     GET0L

*  Purpose:
*     Obtain a logical parameter value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GET0L( PARAMS, POS, OPT, COMM, PROMPT, DEFVAL, VALUE, STATUS )

*  Description:
*     A string value is obtained from the list of suplied parameter
*     values or from the user. This value is converted into a logical value.
*     (TRUE, T, YES, Y, FALSE, F, NO, N are recognised, case insensitive). If
*     this fails the user is reprompted.

*  Arguments:
*     PARAMS = CHARACTER * ( * ) (Given)
*        A string containing the list of command parameters.
*     POS = INTEGER (Given)
*        The index of the required parameter within the list of all
*        possible parameters.
*     OPT = LOGICAL (Given)
*        Is the parameter an optional parameter? If so, then the
*        supplied default value will be returned if no value has
*        been supplied. Otherwise, the user is prompted if no value
*        has been supplied.
*     COMM = CHARACTER * ( * ) (Given)
*        The command name.
*     PROMPT = CHARACTER * ( * ) (Given)
*        The prompt string. "(Y/N)?" is appended to the end.
*     DEFVAL = LOGICAL (Given)
*        The default value.
*     VALUE = LOGICAL (Returned)
*        The returned value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-AUG-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PARAMS
      INTEGER POS
      LOGICAL OPT
      CHARACTER * ( * ) COMM
      CHARACTER * ( * ) PROMPT
      LOGICAL DEF
      LOGICAL DEFVAL

*  Arguments Returned:
      LOGICAL VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns used length of a string

*  Local Variables:
      CHARACTER
     :        DEFSTR*10,         ! Default text value
     :        PRM*80,            ! The used prompt string
     :        STR*10             ! Supplied text value

      INTEGER
     :        NC                 ! Used length of default text string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct the default string.
      IF( DEFVAL ) THEN
         DEFSTR = 'YES'
         NC = 3
      ELSE
         DEFSTR = 'NO'
         NC = 2
      END IF

*  Append "(Y/N)" to the prompt string. GET0C/RDSTR will add a "?" character
*  at the end.
      PRM = PROMPT
      PRM( CHR_LEN( PROMPT ) + 1: ) = ' (Y/N)'

*  Get the text string. APpend "(Y/N)" to the prompt strin
      CALL GET0C( PARAMS, POS, OPT, COMM, PRM, DEFSTR( : NC ), STR,
     :            STATUS )

*  Jump to here when a string value has been obtained.
 10   CONTINUE

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to convert the string to a logical value.
      CALL CHR_CTOL( STR, VALUE, STATUS )

*  If any error occurred, annul the error, report another error and flush
*  it. Then re-prompt the user.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'STR', STR )
         CALL ERR_REP( 'GET0L_ERR1', 'Unable to convert string '//
     :                 '''^STR'' to a logical value.', STATUS )
         CALL ERR_FLUSH( STATUS )

         CALL RDSTR( COMM, PRM, DEFSTR( : NC ), STR, STATUS )
         GO TO 10

      END IF

*  Jump to here if an error occurs.
 999  CONTINUE

      END
