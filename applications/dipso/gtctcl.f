      SUBROUTINE GTCTCL( PARAMS, POS, OPT, COMM, PROMPT, DEFVAL, CI,
     :                   GI, CNAME, STATUS )

*+
*  Name:
*     GTCTCL

*  Purpose:
*     Obtain a CAT identifier for a catalogue component named by the user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GTCTCL( PARAMS, POS, OPT, COMM, PROMPT, DEFVAL, CI,
*                  GI, CNAME, STATUS )

*  Description:
*     This routine obtains the name of a catalogue component, selected from
*     those available in the supplied catalogue. A CAT identifier (see
*     SUN/181) for the selected component is returned.

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
*        The default catalogue column name.
*     CI = INTEGER (Given)
*        The CAT catalogue identifier.
*     GI = INTEGER (Returned)
*        The returned CAT column identifier. Returned equal to CAT__NOID
*        if an error occurs.
*     CNAME = CHARACTER * ( * ) (Returned)
*        The column name. Returned blank if an error occurrs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JUL-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CAT_PAR'          ! CAT constants
      INCLUDE 'CAT_ERR'          ! CAT error constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Arguments Given:
      CHARACTER * ( * ) PARAMS
      INTEGER POS
      LOGICAL OPT
      CHARACTER * ( * ) COMM
      CHARACTER * ( * ) PROMPT
      CHARACTER * ( * ) DEFVAL
      INTEGER CI

*  Arguments Returned:
      INTEGER GI
      CHARACTER CNAME*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CATNAM*(CAT__SZCNM)! Catalogue name
      INTEGER IDTYP              ! Type of specified component
      INTEGER N                  ! Component index
*.

*  Initialise.
      GI = CAT__NOID
      CNAME = ' '

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Defer the reporting of errors.
      CALL ERR_MARK

*  Get the catalogue name for use in error messages.
      CALL CAT_TIQAC( CI, 'NAME', CATNAM, STATUS )

*  Get the column name.
      CALL GET0C( PARAMS, POS, OPT, COMM, PROMPT, DEFVAL, CNAME,
     :            STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Jump to here if a new column name has been obtained.
 10   CONTINUE

*  Attempt to get an identifier for the named column.
      CALL CAT_TIDNT( CI, CNAME, GI, STATUS )

*  If it exists, check that the component is a column. Report an error
*  otherwise.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL CAT_TIDTP( GI, IDTYP, STATUS )

         IF( IDTYP .NE. CAT__FITYP .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', CNAME )
            CALL ERR_REP( 'GTCTCL_1', '^NAME is not a catalogue column',
     :                    STATUS )
         END IF

*  If the component was not found, annul the verbose CAT message, and
*  re-report with a more concise message.
      ELSE IF( STATUS .EQ. CAT__NOCMP ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = CAT__NOCMP
         CALL MSG_SETC( 'NAME', CNAME )
         CALL MSG_SETC( 'CATNAM', CATNAM )
         CALL ERR_REP( 'GTCTCL_2', 'No column named ''^NAME'' '//
     :                  'was found in catalogue ''^CATNAM''.', STATUS )
      END IF

*  If an error has occurred, flush the error, display a list of valid
*  component names, and get a new value.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )

         CALL CAT_TNDNT( CI, CAT__FITYP, 1, GI, STATUS )
         N = 1
         DO WHILE( GI .NE. CAT__NOID )
            CALL CAT_TIQAC( GI, 'NAME', CNAME, STATUS )
            CALL MSG_SETC( 'NAMES', ' ' )
            CALL MSG_SETC( 'NAMES', CNAME )
            CALL MSG_SETC( 'NAMES', ',' )
            N = N + 1
            CALL CAT_TNDNT( CI, CAT__FITYP, N, GI, STATUS )
         END DO

         CALL MSG_OUT( 'GTCTCL_ERR', 'The following catalogue '//
     :                 'columns are available: ^NAMES', STATUS )

         CALL RDSTR( COMM, PROMPT, DEFVAL, CNAME, STATUS )

*  If a new name was supplied, go round to try to open the catalogue.
         IF( STATUS .EQ. SAI__OK ) GO TO 10

      END IF

*  Arrive here if an error occurs.
 999  CONTINUE

*  Release the returned identifier if an error has occurred.
      IF( STATUS .NE. SAI__OK ) CALL CAT_TRLSE( GI, STATUS )

*  Re-report the the error if a null or abort was supplied.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__NULL
         CALL ERR_REP( 'GETCAT_ERR', 'Null catalogue column specified.',
     :                 STATUS )

      ELSE IF( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__ABORT
         CALL ERR_REP( 'GETCAT_ERR', 'Aborted attempt to obtain a ' //
     :                 'catalogue column.', STATUS )

      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
