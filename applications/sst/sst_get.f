      SUBROUTINE SST_GET( UNIT, LINE, STATUS )
*+
*  Name:
*     SST_GET

*  Purpose:
*     Read an input line.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_GET( UNIT, LINE, STATUS )

*  Description:
*     The routine reads an input line from a formatted sequential file
*     and handles any error which may arise.

*  Arguments:
*     UNIT = INTEGER (Given)
*        Fortran I/O unit attached to the file.
*     LINE = CHARACTER * ( * ) (Returned)
*        Line read from the file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-AUG-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_PAR'          ! FIO_ public constants

*  Arguments Given:
      INTEGER UNIT

*  Arguments Returned:
      CHARACTER * ( * ) LINE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( FIO__SZFNM ) FNAME ! File name
      INTEGER IOERR              ! READ error status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read the file.
      READ( UNIT, '( A )', IOSTAT = IOERR ) LINE

*  If an error occurred, then construct a message and report it.
      IF ( IOERR .NE. 0 ) THEN
         CALL FIO_SERR( IOERR, STATUS )
         INQUIRE ( UNIT = UNIT, NAME = FNAME )
         CALL MSG_SETC( 'FILE', FNAME )
         CALL MSG_SETI( 'UNIT', UNIT )
         CALL ERR_FIOER( 'MESSAGE', IOERR )
         CALL ERR_REP( 'SST_GET_ERR',
     :   'Error reading file ^FILE on Fortran unit ^UNIT - ^MESSAGE.',
     :   STATUS )
      END IF

      END
* @(#)sst_get.f   1.1   94/12/05 11:31:26   96/07/05 10:27:26
