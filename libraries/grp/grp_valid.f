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

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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
