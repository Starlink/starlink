      SUBROUTINE GETRNG( PARAMS, POS, OPT, COMM, PROMPT, MINVAL, MAXVAL,
     :                   TOP, BOT, STATUS )
*+
* Name:
*     GETRNG

*  Purpose:
*     Obtain a parameter value specifying a range of positive integers

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GETRNG( PARAMS, POS, OPT, COMM, PROMPT, MINVAL, MAXVAL, TOP,
*                  BOT, STATUS )

*  Description:
*     A text range specifying a range of positive integers in the
*     format "lo-hi" is obtained from the supplied list of parameter
*     values or from the user. If "lo" or "hi" is omited, they default
*     to the values supplied in MINVAL and MAXVAL. The upper and lower
*     limits are returned in TOP and BOT. If a single value is given
*     without a minus sign, TOP and BOT are set to the same value. If a
*     limit value is supplied which is outside the range MINVAL,
*     MAXVAL, then a warning is given and the user is re-prompted.
*     The values supplied in TOP and BOT are used as default values.

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
*        The prompt string.
*     MINVAL = INTEGER (Given)
*        The minimum allowable value.
*     MAXVAL = INTEGER (Given)
*        The maximum allowable value.
*     TOP = INTEGER (Given and Returned)
*        The upper limit.
*     BOT = INTEGER (Given and Returned)
*        The lower limit.
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
      INCLUDE 'PAR_ERR'          ! PAR_ error constants

*  Arguments Given:
      CHARACTER * ( * ) PARAMS
      INTEGER POS
      LOGICAL OPT
      CHARACTER * ( * ) COMM
      CHARACTER * ( * ) PROMPT
      INTEGER MINVAL
      INTEGER MAXVAL

*  Arguments Given and Returned:
      INTEGER TOP
      INTEGER BOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :        DEFVAL*40,         ! Default range specifier
     :        VALUE*40           ! Supplied range specifier

      INTEGER
     :        IP                 ! Index of last non-blank character
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct the default string.
      DEFVAL = ' '
      IP = 0
      CALL CHR_PUTI( MIN( BOT, TOP ), DEFVAL, IP )
      CALL CHR_APPND( '-', DEFVAL, IP )
      CALL CHR_PUTI( MAX( BOT, TOP ), DEFVAL, IP )

*  Get the text string.
      CALL GET0C( PARAMS, POS, OPT, COMM, PROMPT, DEFVAL( : IP ), VALUE,
     :            STATUS )

*  Try to decode it.
 10   CONTINUE
      CALL DECRNG( VALUE, MINVAL, MAXVAL, BOT, TOP, STATUS )

*  If any error occurred, other than a null or abort value being given,
*  flush the errors and re-prompt.
      IF( STATUS .NE. SAI__OK ) THEN

         IF( STATUS .NE. PAR__NULL .AND.
     :       STATUS .NE. PAR__ABORT ) THEN

            CALL ERR_FLUSH( STATUS )
            CALL RDSTR( COMM, PROMPT, DEFVAL, VALUE, STATUS )
            GO TO 10

         END IF

      END IF

      END
