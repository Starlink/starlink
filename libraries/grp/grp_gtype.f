      SUBROUTINE GRP_GTYPE( IGRP, TYPE, STATUS )
*+
*  Name:
*     GRP_GTYPE

*  Purpose:
*     Retrieve the type string stored with a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_GTYPE( IGRP, TYPE, STATUS )

*  Description:
*     The type string specified when the group was created is retrieved.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP group identifier.
*     TYPE = CHARACTER * ( * ) (Returned)
*        The group type. The maximum allowable type length is given by
*        the symbolic constant GRP__SZTYP. If the supplied variable is
*        too short, the type is truncated but no error is reported. If
*        an error occurs, the string is returned blank.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_TYPE( GRP__MAXG ) = CHARACTER (Read)
*           Group types.

*  Arguments Given:
      INTEGER IGRP

*  Arguments Returned:
      CHARACTER TYPE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.
*.

*  Ensure that a blank string is returned if an error condition
*  exists on entry.
      TYPE = ' '

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP, SLOT, STATUS )

*  If OK, get the group type string from the corresponding slot.
      IF ( STATUS .EQ. SAI__OK ) TYPE = CMN_TYPE( SLOT )

*  If an error occurred, give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GRP_GTYPE_ERR1',
     :                 'GRP_GTYPE: Unable to get the type string '//
     :                 'for a group.' , STATUS )
      END IF

      END
