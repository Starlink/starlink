      SUBROUTINE GRP_OWN( IGRP1, IGRP2, STATUS )
*+
*  Name:
*     GRP_OWN

*  Purpose:
*     Returns the identifier of the group which owns the specified
*     group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_OWN( IGRP1, IGRP2, STATUS )

*  Description:
*     If the group identified by IGRP1 has had an "owner" group
*     established for it by a call to GRP_SOWN, then the identifier of
*     the owner group is returned in IGRP2. Otherwise, the value
*     GRP__NOID is returned (but no error is reported).

*  Arguments:
*     IGRP1 = INTEGER (Given)
*        An identifier for the slave group whose owner is to be
*        returned.
*     IGRP2 = INTEGER (Returned)
*        An identifier for the group which owns the group identified by
*        IGRP1. Returned equal to GRP__NOID if an error occurs.
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
*     {enter_changes_here}

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
*        CMN_OWNER( GRP__MAXG ) = INTEGER (Read)
*           The identifers of the owner of each group.

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

*  If OK, return the owner group.
      IF( STATUS .EQ. SAI__OK ) IGRP2 = CMN_OWNER( SLOT )

*  If an error has occurred give a context message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GRP_OWN_ERR2',
     :                 'GRP_OWN: Unable to get the owner of a group.',
     :                 STATUS )
      END IF

      END
