       SUBROUTINE CMD_ERR( STRING )
*+
*  Name:
*     SUBROUTINE CMD_ERR

*  Purpose:
*     Display error message.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CMD_ERR( STRING )

*  Arguments:
*     STRING = CHARACTER* ( * ) (Given)
*        Error message text.

*  Authors:
*     ???
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     ??-???-?? (???):
*       Original version.
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*( * ) STRING

*  Local Variables:
      INTEGER ISTAT
*.

      ISTAT = SAI__OK
      CALL IULOG( STRING( :LEN( STRING ) - 1 ), ISTAT )

      END
