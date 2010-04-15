      SUBROUTINE UNSETGLOBAL( STATUS )
*+
*  Name:
*     UNSETGLOBAL

*  Purpose:
*     Unsets the value of a global parameter.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL UNSETGLOBAL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine deletes the value of a global parameter.

*  Usage:
*     UNSETGLOBAL parameter

*  ADAM Parameters:
*     PARAMETER = _CHAR (Read)
*        Name of the global parameter to delete.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     28-APR-1997 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'         ! HDS/DAT constants
      INCLUDE 'PSX_ERR'         ! PSX error codes

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN          ! Used length of string

*  Local Variables:
      CHARACTER * ( DAT__SZNAM ) PARAM ! Global parameter name
      CHARACTER * ( DAT__SZLOC ) FLOC ! Locator to global file
      CHARACTER * ( 200 ) FNAME ! Global parameters filename
      LOGICAL EXISTS            ! Component exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the parameter name.
      CALL PAR_GET0C( 'PARAMETER', PARAM, STATUS )

*  Get the location of the global file.
      CALL ERR_MARK
      CALL PSX_GETENV( 'ADAM_USER', FNAME, STATUS )
      IF ( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL PSX_GETENV( 'HOME', FNAME, STATUS )
         FNAME = FNAME( :CHR_LEN( FNAME ) ) // '/adam/GLOBAL'
      ELSE
         FNAME = FNAME( :CHR_LEN( FNAME ) ) // '/GLOBAL'
      END IF
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Now open it.
         CALL ERR_MARK
         CALL HDS_OPEN( FNAME, 'UPDATE', FLOC, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Check if the component exists.
            CALL DAT_THERE( FLOC, PARAM, EXISTS, STATUS )
            IF ( EXISTS ) THEN

*  Delete it.
               CALL DAT_ERASE( FLOC, PARAM, STATUS )
            END IF

*  And close the file.
            CALL DAT_ANNUL( FLOC, STATUS )
         ELSE

*  Failed to open global file, so do nothing.
            CALL ERR_ANNUL( STATUS )
         END IF
         CALL ERR_RLSE
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'UNSETGLOBAL_ERR',
     :               'UNSETGLOBAL: Error unsetting global parameter',
     :               STATUS )
      END IF
      END
* $Id$
