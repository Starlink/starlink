      SUBROUTINE GRP_HEAD( IGRP1, IGRP2, STATUS )
*+
*  Name:
*     GRP_HEAD

*  Purpose:
*     Finds the group which is at the head of an owner-slave chain.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_HEAD( IGRP1, IGRP2, STATUS )

*  Description:
*     This routine climbs the chain of owners starting at the group
*     identified by IGRP1, until a group is found which has no owner. 
*     The identifier issued for this group is returned in IGRP2. If
*     the group identified by IGRP1 has no owner, then IGRP2 is returned
*     equal to IGRP1.

*  Arguments:
*     IGRP1 = INTEGER (Given)
*        A group identifier.
*     IGRP2 = INTEGER (Given)
*        The identifier for the group which is at the head of the
*        owner-slave chain. Returned equal to GRP__NOID if an error 
*        occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-SEP-1992 (DSB):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP public constants.

*  Arguments Given:
      INTEGER IGRP1

*  Arguments Returned:
      INTEGER IGRP2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.
*.

*  Ensure that an invalid identifier is returned if an error condition
*  exists on entry.
      IGRP2 = GRP__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP1, SLOT, STATUS )

*  Find the identifier issued for the group which is at the 
*  head of the owner-slave chain.
      CALL GRP1_IHEAD( SLOT, IGRP2, STATUS )

*  If an error has occurred, ensure an invalid identifier is returned
*  and give a context message.
      IF ( STATUS .NE. SAI__OK ) THEN
         IGRP2 = GRP__NOID
         CALL ERR_REP( 'GRP_HEAD_ERR1',
     :    'GRP_HEAD: Unable to find the head group in an owner-slave '//
     :    'chain.',STATUS )
      END IF

      END
