      SUBROUTINE GRP_PTYPE( IGRP, TYPE, STATUS )
*+
*  Name:
*     GRP_PTYPE

*  Purpose:
*     Associate a new type string with a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_PTYPE( IGRP, TYPE, STATUS )

*  Description:
*     The given type string is stored with the group, replacing the
*     previous value.  The maximum length of the type string is given
*     by symbolic constant GRP__SZTYP. If the supplied type string is
*     longer than this, the title is truncated.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group.
*     TYPE = CHARACTER * ( * ) (Given)
*        The group type.
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
*        CMN_TYPE( GRP__MAXG ) = CHARACTER (Write)
*           Group types.

*  Arguments Given:
      INTEGER IGRP
      CHARACTER TYPE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP, SLOT, STATUS )

*  If OK, store the supplied type.
      IF( STATUS .EQ. SAI__OK ) CMN_TYPE( SLOT ) = TYPE

*  If an error occurred, give a context message.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GRP_PTYPE_ERR1',
     :                 'GRP_PTYPE: Unable to store a group type',
     :                 STATUS )
      END IF

      END
