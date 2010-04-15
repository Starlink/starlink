      SUBROUTINE GETCAT( PARAMS, POS, OPT, COMM, PROMPT, DEFVAL, MODE,
     :                   CI, FILE, STATUS )
*+
* Name:
*     GETCAT

*  Purpose:
*     Obtain an identifier for an existing catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GETCAT( PARAMS, POS, OPT, COMM, PROMPT, DEFVAL, MODE,
*                  CI, FILE, STATUS )

*  Description:
*     Uses the CAT library (see SUN/181) to open a catalogue. It may be
*     in any of the formats supported by the CAT library (eg FITS tables,
*     etc).

*  Arguments:
*     PARAMS = CHARACTER * ( * ) (Given)
*        A string containing the supplied command parameters.
*     POS = INTEGER (Given)
*        The index of the required parameter within the list of all
*        possible parameters.
*     OPT = LOGICAL (Given)
*        Is the parameter an optional parameter? If so, then the
*        supplied default value will be used if no value has
*        been supplied. Otherwise, the user is prompted if no value
*        has been supplied.
*     COMM = CHARACTER * ( * ) (Given)
*        The command name.
*     PROMPT = CHARACTER * ( * ) (Given)
*        The prompt string.
*     DEFVAL = CHARACTER * ( * ) (Given)
*        The default catalogue name.
*     MODE = CHARACTER * ( * ) (Given)
*        The access mode required READ or WRITE (not UPDATE).
*     CI = INTEGER (Returned)
*        The CAT identifier.
*     FILE = CHARACTER * ( * ) (Returned)
*        The specified catalogue file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If an error occurs, a null value is returned for CI.

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-JUL-1998 (DSB):
*        Original version.
*     23-JUL-2009 (TIMJ):
*        Use MSG_FLEVOK rather than MSG_IFLEV

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CAT_PAR'          ! CAT_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants

*  Arguments Given:
      CHARACTER * ( * ) PARAMS
      INTEGER POS
      LOGICAL OPT
      CHARACTER * ( * ) COMM
      CHARACTER * ( * ) PROMPT
      CHARACTER * ( * ) DEFVAL
      CHARACTER * ( * ) MODE

*  Arguments Returned:
      INTEGER CI
      CHARACTER * ( * ) FILE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :        NAME*80            ! Catalogue title

      LOGICAL
     :        ISVERB             ! Is VERBOSE or DEBUG
*.

*  Ensure a null identifier gets returned if an error has already occurred.
      CI = CAT__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Defer the reporting of errors.
      CALL ERR_MARK

*  Check the MSG message filtering level.
      ISVERB = MSG_FLEVOK( MSG__VERB, STATUS )

*  Get the catalogue name.
      CALL GET0C( PARAMS, POS, OPT, COMM, PROMPT, DEFVAL, FILE,
     :            STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Jump to here if a new catalogue name has been obtained.
 10   CONTINUE

*  Attempt to open the catalogue.
      CALL CAT_TOPEN( FILE, 'OLD', MODE, CI, STATUS )

*  If an error has occured,
      IF( STATUS .NE. SAI__OK ) THEN

*  If the MSG message filtering level is verbose flush all the error
*  messages. Otherwise, annul them.
         IF( ISVERB ) THEN
            CALL ERR_FLUSH( STATUS )
         ELSE
            CALL ERR_ANNUL( STATUS )
         END IF

*  Add a context message and flush it.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'GETCAT_ERR1', 'Failed to get access to an '//
     :                 'existing catalogue.', STATUS )
         CALL ERR_FLUSH( STATUS )

*  Get a new catalogue name from the user.
         CALL RDSTR( COMM, PROMPT, DEFVAL, FILE, STATUS )

*  If a new name was supplied, go round to try to open the catalogue.
         IF( STATUS .EQ. SAI__OK ) GO TO 10

*  If a catalogue has been obtained succesfully, display its name (unless
*  it is blank or undefined).
      ELSE
         NAME = ' '
         CALL CAT_TIQAC( CI, 'NAME', NAME, STATUS )

         IF( NAME .NE. ' ' ) THEN
            CALL MSG_SETC( 'NAME', NAME )
            CALL MSGOUT( COMM, 'Catalogue name - ''^NAME''', .FALSE.,
     :                   STATUS )
         END IF

      END IF

*  Re-report null and abort messages with more friendly messages.
 999  CONTINUE

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__NULL
         CALL ERR_REP( 'GETCAT_ERR', 'Null catalogue specified.',
     :                 STATUS )

      ELSE IF( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__ABORT
         CALL ERR_REP( 'GETCAT_ERR', 'Aborted attempt to open an ' //
     :                 'existing catalogue.', STATUS )

      END IF

*  If an error has been reported, attempt to annul the CAT identifier.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL CAT_TRLSE( CI, STATUS )
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
