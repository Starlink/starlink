      SUBROUTINE GRP_VALID( IGRP, VALID, STATUS )
*+
*  Name:
*     GRP_VALID

*  Purpose:
*     Determine if a group identifier is valid.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_VALID( IGRP, VALID, STATUS )

*  Description:
*     Argument VALID is returned .TRUE. if the group identified by IGRP
*     is valid, and is returned .FALSE. otherwise.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier.
*     VALID = LOGICAL (Returned)
*        The status of the group.
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

*  Arguments Given:
      INTEGER IGRP

*  Arguments Returned:
      LOGICAL VALID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the validity of the identifier. SLOT is returned equal to zero
*  if the identifier is not valid.
      CALL GRP1_ID2SL( IGRP, SLOT )

*  Set the returned flag accordingly.
      IF( SLOT .EQ. 0 ) THEN
         VALID = .FALSE.

      ELSE
         VALID = .TRUE.

      END IF

      END
