      SUBROUTINE GET0C( PARAMS, POS, OPT, COMM, PROMPT, DEFVAL, VALUE,
     :                  STATUS )
*+
* Name:
*     GET0C

*  Purpose:
*     Obtain a string parameter value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GET0C( PARAMS, POS, OPT, COMM, PROMPT, DEFVAL, VALUE,
*                 STATUS )

*  Description:
*     If the supplied string PARAMS contains at least POS separate
*     words, the POS'th word is returned in VALUE. If PARAMS contains
*     less than POS words, the user is prompted for a string so long as
*     OPT is not TRUE (if OPT is TRUE, then DEFVAL is returned in VALUE
*     if PARAMS contains insufficient words). If POS is supplied equal
*     to zero, then the entire contents of PARAMS is returned (or the
*     user is prompted if params is empty, again depending on the value
*     of OPT).
*
*     A word is any contiguous block of non-blank characters, or a
*     string contained within single quotes.

*  Arguments:
*     PARAMS = CHARACTER * ( * ) (Given)
*        The string containing command parameters.
*     POS = INTEGER (Given)
*        The index of the required parameter within the list of all
*        possible parameters. If this is zero, then the required
*        parameter value consists of all words in the string, not just
*        one.
*     OPT = LOGICAL (Given)
*        Is the parameter an optional parameter? If so, then the
*        supplied default value will be returned if no value has
*        been supplied. Otherwise, the user is prompted if no value
*        has been supplied.
*     COMM = CHARACTER * ( * ) (Given)
*        The command name.
*     PROMPT = CHARACTER * ( * ) (Given)
*        The prompt string.
*     DEFVAL = CHARACTER * ( * ) (Given)
*        The default value.
*     VALUE = CHARACTER * ( * ) (Returned)
*        The parameter value. A blank string is returned if an
*        error occurs.
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
      INCLUDE 'PAR_ERR'          ! PAR__ error constants

*  Arguments Given:
      CHARACTER * ( * ) PARAMS
      INTEGER POS
      LOGICAL OPT
      CHARACTER * ( * ) COMM
      CHARACTER * ( * ) PROMPT
      CHARACTER * ( * ) DEFVAL

*  Arguments Returned:
      CHARACTER * ( * ) VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Index of last non-blank chatracter

*  Local Variables:
      INTEGER
     :        F,                 ! Index of first non-blank character
     :        L,                 ! Index of last non-blank character
     :        LVALUE             ! Used length of VALUE

*.

*  Initialise the returned value.
      VALUE = ' '

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If POS is zero (or less), use the entire string.
      IF( POS .LE. 0 ) THEN
         VALUE = PARAMS

*  Otherwise, try and find the required word within the supplied list
*  of parameter values.
      ELSE
         CALL FWORD( PARAMS, POS, VALUE, LVALUE )

      END IF

*  If no value was given on the command line...
      IF( VALUE .EQ. ' ' ) THEN

*  If the parameter is optional, return the default value.
         IF( OPT ) THEN
            VALUE = DEFVAL

*  If the parameter is mandatory, prompt the user for a value.
         ELSE
            VALUE = ' '
            CALL RDSTR( COMM, PROMPT, DEFVAL, VALUE, STATUS )
         END IF

      END IF

*  Report an error if the obtained string is a null ("!") or abort ("!!").
      CALL CHR_FANDL( VALUE, F, L )
      IF( F .LE. L .AND. STATUS .EQ. SAI__OK ) THEN

         IF( VALUE( F:L ) .EQ. '!' ) THEN
            STATUS = PAR__NULL
            CALL ERR_REP( 'GET0C_ERR1', 'Null parameter value given.',
     :                    STATUS )

         ELSE IF( VALUE( F:L ) .EQ. '!!' ) THEN
            STATUS = PAR__ABORT
            CALL ERR_REP( 'GET0C_ERR2', 'Parameter request aborted.',
     :                    STATUS )
         END IF

      END IF

*  If an error has occurred, return a blank string.
      IF( STATUS .NE. SAI__OK ) VALUE = ' '

      END
