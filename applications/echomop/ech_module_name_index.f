      INTEGER FUNCTION ECH_MODULE_NAME_INDEX( MOD_NAME )
*+
*  Name:
*     ECHOMOP - ECH_MODULE_NAME_INDEX

*  Purpose:
*     Get index of named module.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     24-APR-1996 (MJC):
*       Added Prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_MODULES.INC'

*  Arguments Given:
      CHARACTER*( * ) MOD_NAME

*  Local Variables:
      INTEGER I
*.
      DO I = 1, MAX_MODULES
         IF ( MODULE_NAME( I ) .EQ. MOD_NAME ) THEN
            ECH_MODULE_NAME_INDEX = I
            GO TO 100
         END IF
      END DO
  100 CONTINUE

      END
