      SUBROUTINE IRH1_READF( UNIT, LINE, EOF, STATUS )
*+
*  Name:
*     IRH1_READF

*  Purpose:
*     Read a record from a formatted sequential file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_READF( UNIT, LINE, EOF, STATUS )

*  Description:
*     The routine reads an input record from a formatted sequential file
*     and handles any error which may arise. If the end of file is 
*     reached, EOF is set true and LINE is returned blank, but no error
*     is reported.

*  Arguments:
*     UNIT = INTEGER (Given)
*        Fortran I/O unit attached to the file.
*     LINE = CHARACTER (Returned)
*        Line read from the file. Returned blank if the end of file 
*        has been reached.
*     EOF = LOGICAL (Returned)
*        True if the end of file has been reached. False otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-MAY-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE               ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard SAE constants
      INCLUDE 'FIO_PAR'           ! FIO_ public constants

*  Arguments Given:
      INTEGER   UNIT

*  Arguments Returned:
      CHARACTER LINE*(*)
      LOGICAL   EOF

*  Status:
      INTEGER   STATUS            ! Global status

*  Local Variables:
      CHARACTER FNAME*(FIO__SZFNM)! File name
      INTEGER   IOERR             ! READ error status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read the file.
      READ( UNIT, '(A)', END = 10, IOSTAT = IOERR ) LINE

*  If an error occurred, then construct a message and report it.
      IF ( IOERR .NE. 0 ) THEN
         CALL FIO_SERR( IOERR, STATUS )
         INQUIRE ( UNIT = UNIT, NAME = FNAME )
         CALL MSG_SETC( 'FILE', FNAME )
         CALL MSG_SETI( 'UNIT', UNIT )
         CALL ERR_FIOER( 'MESSAGE', IOERR )
         CALL ERR_REP( 'IRH1_READF_ERR',
     :   'Error reading file ^FILE on Fortran unit ^UNIT - ^MESSAGE.',
     :   STATUS )
      END IF

*  Indicate that the end of file has not yet been reached.
      EOF = .FALSE.

*  Skip the "end of file reached" section.
      GO TO 999

*  Arrive here if the end of file has been reached.
 10   CONTINUE
      EOF = .TRUE.
      LINE = ' '

 999  CONTINUE

      END
* $Id$
