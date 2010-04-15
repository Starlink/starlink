      SUBROUTINE SPOSB0( PARAM, INVER, SCS, LOGING, FD, STATUS )
*+
*  Name:
*     SPOSB0

*  Purpose:
*     Create a log file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPOSB0( PARAM, INVER, SCS, LOGING, FD, STATUS )

*  Description:
*     The supplied parameter is used to get the name of the log file. If
*     a null value is supplied, then no log file is created. Otherwise,
*     a file is created with the specified name, and a header is
*     written to it consisting of a comment line identifying SKYPOS as
*     the creator of the file, and a line identifying the sort of
*     coordinates which follow (this line is omitted if image
*     coordinates are being stored in the file).

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     INVER = LOGICAL (Given)
*        True if the inverse transformation from sky to image
*        coordinates is being performed.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system in which output sky coordinates are
*        being displayed.
*     LOGING = LOGICAL (Returned)
*        True if a log file has been opened.
*     FD = INTEGER (Returned)
*        The FIO descriptor for the opened log file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-JAN-1993 (DSB):
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
      CHARACTER PARAM*(*)
      LOGICAL INVER
      CHARACTER SCS*(*)

*  Arguments Returned:
      LOGICAL LOGING
      INTEGER FD

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If required open a log file. The file descriptor returned in FD is
*  used to access this file.
      CALL IRM_ASFIO( PARAM, 'WRITE', 'LIST', 80, FD, LOGING, STATUS )

*  If a null value was supplied, annul the error.
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  If a log file name was supplied, write a header to the log file.
*  The first line is a comment indicating the source of the file, and
*  the second is a line identifying the coordinate system of the
*  coordinate values which follow.
      IF( LOGING ) THEN

         CALL FIO_WRITE( FD, '#  Log file from IRAS90:SKYPOS',
     :                   STATUS )

         IF( .NOT. INVER ) THEN
            CALL FIO_WRITE( FD, SCS, STATUS )
         ELSE
            CALL FIO_WRITE( FD, '#  Image coordinates', STATUS )
         END IF

      END IF

      END
