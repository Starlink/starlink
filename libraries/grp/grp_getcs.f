      SUBROUTINE GRP_GETCS( IGRP, SENSIT, STATUS )
*+
*  Name:
*     GRP_GETCS

*  Purpose:
*     Determine the case sensitivity of a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_GETCS( IGRP, SENSIT, STATUS )

*  Description:
*     Checks whether a group is currently case sensitive, or case
*     insensitive (see routine GRP_SETCS).

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group.
*     SENSIT = LOGICAL (Returned)
*        Returned .TRUE. if the group is case sensitive and .FALSE.
*        otherwise.
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
*        CMN_UPPER( GRP__MAXG ) = LOGICAL (Read)
*           If true, then all names in the group should be converted
*           to upper case before being used. Otherwise, they are left
*           as they are.

*  Arguments Given:
      INTEGER IGRP

*  Arguments Returned:
      LOGICAL SENSIT

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

*  If OK, get the case sensitivity flag from the corresponding slot.
      IF ( STATUS .EQ. SAI__OK ) SENSIT = .NOT. CMN_UPPER( SLOT )

*  If an error occurred, give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GRP_GETCS_ERR1',
     :                'GRP_GETCS: Unable to get the case sensitivity '//
     :                'flag for a group.' , STATUS )
      END IF

      END
