      SUBROUTINE GET0I( PARAMS, POS, OPT, COMM, PROMPT, DEFVAL, VALUE,
     :                  STATUS )
*+
*  Name:
*     GET0I

*  Purpose:
*     Obtain a integer parameter value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GET0I( PARAMS, POS, OPT, COMM, PROMPT, DEFVAL, VALUE, STATUS )

*  Description:
*     A string value is obtained from the list of suplied parameter
*     values or from the user. This value is converted into a integer value.
*     If this fails the user is reprompted. The string 'BAD' is used to
*     represent the Starlink bad value VAL__BADI.

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
*     DEFVAL = INTEGER (Given)
*        The default value.
*     VALUE = INTEGER (Returned)
*        The returned value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-AUG-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      CHARACTER * ( * ) PARAMS
      INTEGER POS
      LOGICAL OPT
      CHARACTER * ( * ) COMM
      CHARACTER * ( * ) PROMPT
      LOGICAL DEF
      INTEGER DEFVAL

*  Arguments Returned:
      INTEGER VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Two strings equal apart from case?

*  Local Variables:
      CHARACTER
     :        DEFSTR*40,         ! Default text value
     :        STR*40             ! Supplied text value

      INTEGER
     :        NC                 ! Used length of default text string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct the default string.
      IF( DEFVAL .NE. VAL__BADI ) THEN
         CALL CHR_ITOC( DEFVAL, DEFSTR, NC )
      ELSE
         DEFSTR = 'BAD'
         NC = 3
      END IF

*  Get the text string.
      CALL GET0C( PARAMS, POS, OPT, COMM, PROMPT, DEFSTR( : NC ), STR,
     :            STATUS )

*  Jump to here when a string value has been obtained.
 10   CONTINUE

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  If the string is 'BAD' (case insensitive) then return a bad value.
      IF( CHR_SIMLR( STR, 'BAD' ) ) THEN
         VALUE = VAL__BADI

*  Otherwise, attempt to convert the string to a integer value.
      ELSE
         CALL CHR_CTOI( STR, VALUE, STATUS )

*  If any error occurred, annul the error, report another error and flush
*  it. Then re-prompt the user.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'STR', STR )
            CALL ERR_REP( 'GET0I_ERR1', 'Unable to convert string '//
     :                    '''^STR'' to an integer value.', STATUS )
            CALL ERR_FLUSH( STATUS )

            CALL RDSTR( COMM, PROMPT, DEFSTR( : NC ), STR, STATUS )
            GO TO 10

         END IF

      END IF

*  Jump to here if an error occurs.
 999  CONTINUE

      END
