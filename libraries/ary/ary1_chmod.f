      SUBROUTINE ARY1_CHMOD( IACB, MODE, STATUS )
*+
*  Name:
*     ARY1_CHMOD

*  Purpose:
*     Check that the requested array mapping access mode is permitted.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_CHMOD( IACB, MODE, STATUS )

*  Description:
*     The routine checks that the requested mapping access mode is
*     permitted for an array. If it is not, then an error will be
*     reported. An error will also reported if the access mode string
*     supplied is not valid.  Otherwise the routine returns without
*     further action.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to an entry in the ACB.
*     MODE = CHARACTER * ( * ) (Given)
*        The requested access mode; one of 'READ', 'WRITE' or 'UPDATE'
*        (case insensitive).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  For WRITE or UPDATE mode, call ARY1_CHACC to check whether
*     WRITE access to the array is permitted.
*     -  For READ access, no checks are necessary.
*     -  Report an error if the requested access mode string is
*     invalid.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-MAY-1989 (RFWS):
*        Original version.
*     12-JUN-1989 (RFWS):
*        Modified for new common block structure.
*     6-SEP-1989 (RFWS):
*        Substantial re-write to use access control flags.
*     7-SEP-1989 (RFWS):
*        Fixed bug which was preventing WRITE and UPDATE access being
*        granted.
*     18-SEP-1989 (RFWS):
*        Further re-write to call ARY1_CHACC to check if access is
*        available instead of inspecting the access control flags
*        directly.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Arguments Given:
      INTEGER IACB
      CHARACTER * ( * ) MODE

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If UPDATE or WRITE access is requested, then check that WRITE access
*  to the array is available.
      IF ( CHR_SIMLR( MODE, 'WRITE' ) .OR.
     :     CHR_SIMLR( MODE, 'UPDATE' ) ) THEN
         CALL ARY1_CHACC( IACB, 'WRITE', STATUS )

*  No action is needed if READ access is requested.
      ELSE IF ( CHR_SIMLR( MODE, 'READ' ) ) THEN
         CONTINUE

*  Report an error if the MODE value supplied is not recognised.
      ELSE
         STATUS = ARY__FATIN
         CALL MSG_SETC( 'ROUTINE', 'ARY1_CHMOD' )
         CALL MSG_SETC( 'BADMODE', MODE )
         CALL ERR_REP( 'ARY1_CHMOD_MODE',
     :   'Routine ^ROUTINE called with an invalid MODE argument ' //
     :   'of ''^BADMODE'' (internal programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_CHMOD', STATUS )

      END
