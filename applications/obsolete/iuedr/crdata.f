      SUBROUTINE CRDATA( DSNEW, STATUS )
*+
*  Name:
*     SUBROUTINE CRDATA

*  Purpose:
*     Create a new data file and write contents.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CRDATA( DSNEW, STATUS )

*  Arguments:
*     DSNEW = BYTE( 81 ) (Given)
*        Name of the file to be created.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     22-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     07-SEP-94 (MJC):
*       IUEDR Vn. 3.1-3
*       SAEised
*     09-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMFILE'
      INCLUDE 'CMHEAD'

*  Arguments Given:
      BYTE DSNEW( 81 )   ! File name.

*  Status:
      INTEGER STATUS     ! Global status.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      NOHEAD = .FALSE.
      CACHAN = .TRUE.
      DACHAN = .TRUE.
      DQCHAN = .TRUE.

      CALL WRIMG( DSNEW, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL STR_MOVE( DSNEW, 81, DSNAME )
         NODSN = .FALSE.
      END IF

      END
