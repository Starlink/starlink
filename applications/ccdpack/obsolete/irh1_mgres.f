      SUBROUTINE IRH1_MGRES( IDH, SIZE, MODGRP, STATUS )
*+
*  Name:
*     IRH1_MGRES

*  Purpose:
*     Set to zero all occurnces of a given group identifier within a
*     list of group identifiers.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_MGRES( IDH, SIZE, MODGRP, STATUS )

*  Description:
*     The list of group identifiers supplied in MODGRP is searched for
*     occurences of the identifier supplied in IDH. All such occurences
*     are replaced with zero.

*  Arguments:
*     IDH = INTEGER (Given)
*        An IRH identifier.
*     SIZE = INTEGER (Given)
*        The size of the MODGRP array.
*     MODGRP( SIZE ) = INTEGER (Given and Returned)
*        A list of IRH identifiers.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JUN-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER IDH
      INTEGER SIZE

*  Arguments Given and Returned:
      INTEGER MODGRP( SIZE )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round all elements in the group, replacing all occurences of the
*  group specified by IDH, with zero.
      DO I = 1, SIZE
         IF( MODGRP( I ) .EQ. IDH ) MODGRP( I ) = 0
      END DO

      END
* $Id$
