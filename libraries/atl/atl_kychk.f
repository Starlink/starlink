      SUBROUTINE ATL_KYCHK( KEYMAP, KEY, ERRMSG, STATUS )
*+
*  Name:
*     ATL_KYCHK

*  Purpose:
*     Reports an error if a given key is not found in a KeyMap.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_KYCHK( KEYMAP, KEY, ERRMSG, STATUS )

*  Description:
*     This routine checks a supplied KeyMap for a supplied Key and
*     reports a supplied error if the key is not found.

*  Arguments:
*     KEYMAP = INTEGER (Given)
*        Pointer to the AST KeyMap.
*     KEY = CHARACTER * ( * ) (Given)
*        The key to check.
*     ERRMSG = CHARACTER * ( * ) (Given)
*        The error message to report if the key is not found. This may
*        include references to the MSG token "^K" which will hold the
*        supplied key name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-FEB-2008 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER KEYMAP
      CHARACTER KEY*(*)
      CHARACTER ERRMSG*(*)

*  Status:
      INTEGER STATUS               ! Global status

*.

*  Check for the key and report an error if it does not exist.
      IF( .NOT. AST_MAPHASKEY( KEYMAP, KEY, STATUS ) ) THEN
         IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'K', KEY )
            CALL ERR_REP( ' ', ERRMSG, STATUS )
         END IF
      END IF

      END
