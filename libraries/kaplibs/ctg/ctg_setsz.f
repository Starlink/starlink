      SUBROUTINE CTG_SETSZ( IGRP, SIZE, STATUS )
*+
*  Name:
*     CTG_SETSZ

*  Purpose:
*     Reduces the size of a CTG group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTG_SETSZ( IGRP, SIZE, STATUS )

*  Description:
*     This routine should be used instead of GRP_SETSZ to set the size of
*     a group created by CTG. It sets the size of the supplied group, and
*     also sets the size of each of the supplemental groups associated with
*     the supplied group.

*  Arguments:
*     IGRP = INTEGER (Given)
*        The CTG group as returned by CTG_ASSOC, etc. This should be the last 
*        group in a GRP owner-slave chain.
*     SIZE = INTEGER (Given)
*        The new group size. Must be less than or equal to the size of the
*        smallest group in the chain.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-SEP-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.

*  Arguments Given:
      INTEGER IGRP
      INTEGER SIZE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGRP1, IGRP2       ! Group identifiers
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round all groups in the chain.
      IGRP1 = IGRP
      DO WHILE( IGRP1 .NE. GRP__NOID ) 

*  Set the size of this group.
        CALL GRP_SETSZ( IGRP1, SIZE, STATUS )

*  Find the owner of this group. GRP__NOID is returned if the group
*  has no owner.
        CALL GRP_OWN( IGRP1, IGRP2, STATUS ) 
        IGRP1 = IGRP2

      END DO

      END
