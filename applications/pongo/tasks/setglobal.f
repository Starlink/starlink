      SUBROUTINE SETGLOBAL( STATUS )
*+
*  Name:
*     SETGLOBAL

*  Purpose:
*     Sets the values of global parameters.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SETGLOBAL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine sets the values of any global parameters. It is
*     a standalone replacement for the ICL SETGLOBAL command.

*  Usage:
*     SETGLOBAL parameter value

*  ADAM Parameters:
*     PARAMETER = _CHAR (Read)
*        Name of the global parameter to set.
*     VALUE = _CHAR (Read)
*        The values of the parameter.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     23-APR-1997 (PDRAPER):
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
      INCLUDE 'DAT_ERR'         ! HDS/DAT error codes
      INCLUDE 'PSX_ERR'         ! PSX error codes

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN          ! Used length of string

*  Local Variables:
      CHARACTER * ( DAT__SZNAM ) PARAM ! Global parameter name
      CHARACTER * ( 132 ) VALUE ! Global parameter value
      CHARACTER * ( DAT__SZLOC ) FLOC ! Locator to global file
      CHARACTER * ( 200 ) FNAME ! Global parameters filename
      INTEGER DIM( 1 )          ! Dummy array

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the parameter name and value.
      CALL PAR_GET0C( 'PARAMETER', PARAM, STATUS )
      CALL PAR_GET0C( 'VALUE', VALUE, STATUS )

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

*  Now open it.
      CALL ERR_MARK
      CALL HDS_OPEN( FNAME, 'UPDATE', FLOC, STATUS )
      IF ( STATUS .EQ. DAT__FILNF ) THEN

*  File doesn't exist, so create it.
         CALL ERR_ANNUL( STATUS )
         CALL HDS_NEW( FNAME, 'GLOBAL', 'STRUC', 0, DIM, FLOC, STATUS )
      END IF
      CALL ERR_RLSE
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Try to write the value.
         CALL ERR_MARK
         CALL CMP_PUT0C( FLOC, PARAM, VALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Failed, either for format conversion reasons, non-scalar or
*  object doesn't exist. So make sure the object is re-created
*  as a _CHAR*132
            CALL ERR_ANNUL( STATUS )
            CALL CMP_MODC( FLOC, PARAM, 132, 0, DIM, STATUS )
            CALL CMP_PUT0C( FLOC, PARAM, VALUE, STATUS )
         END IF
         CALL ERR_RLSE
      END IF

*  And close the file.
      CALL DAT_ANNUL( FLOC, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SETGLOBAL_ERR',
     :               'SETGLOBAL: Error setting global parameter',
     :               STATUS )
      END IF
      END
* $Id$
