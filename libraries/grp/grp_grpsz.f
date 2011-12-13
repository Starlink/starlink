      SUBROUTINE GRP_GRPSZ( IGRP, SIZE, STATUS )
*+
*  Name:
*     GRP_GRPSZ

*  Purpose:
*     Returns the number of names in a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_GRPSZ( IGRP, SIZE, STATUS )

*  Description:
*     This routine returns the number of names in a group.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group.
*     SIZE = INTEGER (Returned)
*        The number of names in the group. Returned equal to one if an
*        error occurs.
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
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_GSIZE( GRP__MAXG ) = INTEGER (Read)
*           The index of the last entry in each group.

*  Arguments Given:
      INTEGER IGRP

*  Arguments Returned:
      INTEGER SIZE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.
*.

*  Ensure that SIZE is returned equal to one if an error condition
*  exists on entry.
      SIZE = 1

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP, SLOT, STATUS )

*  If OK, get the group size from the corresponding slot.
      IF ( STATUS .EQ. SAI__OK ) SIZE = CMN_GSIZE( SLOT )

*  If an error occurred, give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GRP_GRPSZ_ERR1',
     :      'GRP_GRPSZ: Unable to get the the current size of a group.',
     :                STATUS )
      END IF

      END
