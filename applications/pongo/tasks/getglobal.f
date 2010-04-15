      SUBROUTINE GETGLOBAL( STATUS )
*+
*  Name:
*     GETGLOBAL

*  Purpose:
*     Gets the value of a global parameter.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GETGLOBAL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine gets the value of a global parameter. It is
*     a standalone replacement for the ICL GETGLOBAL command. One
*     significant difference is that this routine fails silently if
*     the requested variable doesn't exist, or the GLOBAL itself
*     doesn't exist. In these cases the result is set to 'not_set'.

*  Usage:
*     GETGLOBAL parameter

*  ADAM Parameters:
*     PARAMETER = _CHAR (Read)
*        Name of the global parameter to set.
*     VALUE = _CHAR (Write)
*        The value of the parameter. Set 'not_set' if not available.

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
      INCLUDE 'PSX_ERR'         ! PSX error codes

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN          ! Used length of string

*  Local Variables:
      CHARACTER * ( DAT__SZNAM ) PARAM ! Global parameter name
      CHARACTER * ( DAT__SZTYP ) TYPE ! Parameter type
      CHARACTER * ( 132 ) VALUE ! Global parameter value
      CHARACTER * ( DAT__SZLOC ) FLOC ! Locator to global file
      CHARACTER * ( DAT__SZLOC ) PLOC ! Locator to parameter
      CHARACTER * ( DAT__SZLOC ) NLOC ! Locator to parname
      CHARACTER * ( 200 ) FNAME ! Global parameters filename
      INTEGER DIM( 1 )          ! Dummy array
      LOGICAL EXISTS            ! Component exists
      LOGICAL PRIM              ! Component is primitive

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

*  Default value is:
         VALUE = 'not_set'

*  Now open it.
         CALL ERR_MARK
         CALL HDS_OPEN( FNAME, 'READ', FLOC, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Check if the component exists.
            CALL DAT_THERE( FLOC, PARAM, EXISTS, STATUS )
            IF ( EXISTS ) THEN

*  Get a locator to it.
               CALL DAT_FIND( FLOC, PARAM, PLOC, STATUS )

*  Get the type.
               CALL DAT_TYPE( PLOC, TYPE, STATUS )
               IF ( TYPE .NE. 'ADAM_PARNAME' ) THEN
                  CALL DAT_PRIM( PLOC, PRIM, STATUS )
                  IF ( PRIM ) THEN
                     CALL DAT_GET0C( PLOC, VALUE, STATUS )
                  END IF
               ELSE

*  ADAM PARNAME structure, look for NAMEPTR.
                  CALL DAT_FIND( PLOC, 'NAMEPTR', NLOC, STATUS )
                  CALL DAT_GET0C( NLOC, VALUE, STATUS )
                  CALL DAT_ANNUL( NLOC, STATUS )
               END IF
               CALL DAT_ANNUL( PLOC, STATUS )
            END IF

*  And close the file.
            CALL DAT_ANNUL( FLOC, STATUS )
         ELSE

*  Failed to open global file, so no value.
            CALL ERR_ANNUL( STATUS )
         END IF

*  Now write the value out.
         CALL PAR_PUT0C( 'VALUE', VALUE, STATUS )
         CALL ERR_RLSE
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GETGLOBAL_ERR',
     :               'GETGLOBAL: Error getting global parameter',
     :               STATUS )
      END IF
      END
* $Id$
