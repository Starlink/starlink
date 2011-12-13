      SUBROUTINE GRP1_SLAVE( ISLOT, IGRP, SLAVE, STATUS )
*+
*  Name:
*     GRP1_SLAVE

*  Purpose:
*     See if a group is a slave of another group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_SLAVE( ISLOT, IGRP, SLAVE, STATUS )

*  Description:
*     A check is made to see if the group identified by ISLOT is a slave
*     of the group identified by IGRP, and SLAVE is returned true if
*     it is. The check is recursive, that is SLAVE is returned true if
*     IGRP is the owner of a group which is the owner of ISLOT (etc).

*  Arguments:
*     ISLOT = INTEGER (Given)
*        The slot number of the group to be checked.
*     IGRP = INTEGER (Given)
*        A GRP identifier for the owner group. Assumed valid.
*     SLAVE = LOGICAL (Returned)
*        Returned true if the group identified by ISLOT is a direct, or
*        indirect slave of the group identified by IGRP, or if IGRP and
*        ISLOT identify the same group. Returned false otherwise.
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
*        CMN_CHK( GRP__MAXG ) = INTEGER (Read)
*           The GRP identifier issued for each slot.
*        CMN_OWNER( GRP__MAXG ) = INTEGER (Read)
*           The GRP identifier of the group which owns each group.

*  Arguments Given:
      INTEGER ISLOT
      INTEGER IGRP

*  Arguments Returned:
      LOGICAL SLAVE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      INTEGER IGRPOW             ! The current owner group identifier.
      INTEGER ISLTOW             ! The current owner group slot number.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the owner group at the current level in the owner-slave
*  chain to be the supplied test group.
      ISLTOW = ISLOT
      IGRPOW = CMN_CHK( ISLOT )

*  Loop round checking the owner in sucessive levels in the owner-slave
*  chain, until either the supplied target group is found, or a free
*  group is found.
      DO WHILE( IGRPOW .NE. GRP__NOID .AND. IGRPOW. NE. IGRP )

*  Find the GRP identifier of the group which owns the group stored in
*  slot ISLTOW.
         IGRPOW = CMN_OWNER( ISLTOW )

*  Find the slot number at which this group is stored.
         CALL GRP1_ID2SL( IGRPOW, ISLTOW )

      END DO

*  If the supplied target group was encountered while climbing the
*  chain, then the test group is a slave of the target group, otherwise
*  it isn't.
      IF( IGRPOW .EQ. IGRP ) THEN
         SLAVE = .TRUE.
      ELSE
         SLAVE = .FALSE.
      END IF

      END
