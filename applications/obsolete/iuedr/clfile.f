      SUBROUTINE CLFILE( FILE, FD, STATUS )
*+
*  Name:
*     SUBROUTINE CLFILE

*  Purpose:
*     Closes a file and reverts to previous input level.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CLFILE( FILE, FD, STATUS )

*  Arguments:
*     FILE = BYTE( 81 ) (Given)
*        Name of the file to close.
*     FD = INTEGER (Given)
*        I/O unit for file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       AT4 version
*     06-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     30-SEP-94 (MJC):
*       IUEDR Vn. 3.1-6
*       FIO I/O unit support.
*     28-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Local Constants:
      INTEGER FILENAMESIZE      ! Maximum length of filename.
      PARAMETER ( FILENAMESIZE = 81 )

*  Arguments Given:
      BYTE FILE( FILENAMESIZE ) ! File name.

      INTEGER FD                ! File descriptor.

*  Status:
      INTEGER STATUS            ! Global status.

*  Local Variables:
      INTEGER ISTAT
      INTEGER FIOSTAT
*.

*  Don't pass error status to FIO_PUNIT as this makes ems go awry.
      ISTAT = SAI__OK
      FIOSTAT = SAI__OK

      CLOSE ( UNIT = FD, IOSTAT = ISTAT )
      CALL FIO_PUNIT( FD, FIOSTAT )
      IF ( ISTAT .NE. SAI__OK ) THEN
         CALL ERRSTR( FILE )
         CALL ERROUT( ': file close error\\', STATUS )

      ELSE IF ( FIOSTAT .NE. SAI__OK ) THEN
         CALL ERRSTR( FILE )
         CALL ERROUT( ': I/O unit release error\\', STATUS )
      END IF

      END
