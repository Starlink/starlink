      SUBROUTINE AGP_GDLST( STATUS )
*+
*  Name:
*     AGP_GDLST

*  Purpose:
*     Lists all known graphics devices.

*  Invocation:
*     CALL AGP_GDLST( STATUS )

*  Description:
*     This routine lists all known graphics devices, using MSG_OUT.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Authors:
*     DSB: Davis Berry (STARLINK)

*  History:
*     31-OCT-2001 (DSB):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS

*.

*  Check the inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

      CALL MSG_BLANK( STATUS )
      CALL PGLDEV
      CALL MSG_BLANK( STATUS )

      END
