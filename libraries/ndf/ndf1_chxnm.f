      SUBROUTINE NDF1_CHXNM( XNAME, STATUS )
*+
*  Name:
*     NDF1_CHXNM

*  Purpose:
*     Check an NDF extension name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CHXNM( XNAME, STATUS )

*  Description:
*     The routine checks the name of an NDF extension for standard
*     form. A standard name must be no more than NDF__SZXNM characters
*     long, must begin with an alphabetic character and continue with
*     alphanumeric characters (including underscore) only. If this test
*     fails, then an error is reported and a STATUS value set.
*     Otherwise, the routine returns without action.

*  Arguments:
*     XNAME = CHARACTER * ( * ) (Given)
*        The extension name to be checked.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Check that the extension name is not too long and that it has
*     the standard form. Report an error if appropriate.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-SEP-1989 (RFWS):
*        Original version.
*     26-SEP-1989 (RFWS):
*        Finished prologue.
*     23-NOV-1989 (RFWS):
*        Changed to use the NDF__SZXNM constant.
*     29-JAN-1990 (RFWS):
*        Removed checks on name registration.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) XNAME

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_ISNAM          ! Whether a string is a standard name
      INTEGER CHR_LEN            ! Significant length of string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the extension name is too long, or does not have the correct
*  standard form, then report an error.
      IF ( ( CHR_LEN( XNAME ) .GT. NDF__SZXNM ) .OR.
     :     ( .NOT. CHR_ISNAM( XNAME ) ) ) THEN
         STATUS = NDF__NSXNM
         CALL MSG_SETC( 'XNAME', XNAME )
         CALL ERR_REP( 'NDF1_CHXNM_NS',
     :   'Non-standard extension name ''^XNAME'' specified ' //
     :   '(possible programming error).', STATUS )
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CHXNM', STATUS )

      END
