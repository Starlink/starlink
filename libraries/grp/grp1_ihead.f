      SUBROUTINE GRP1_IHEAD( ISLOT, IGRP, STATUS )
*+
*  Name:
*     GRP1_IHEAD

*  Purpose:
*     Find the group at the head of a master-slave chain.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_IHEAD( ISLOT, IGRP, STATUS )

*  Description:
*     The identifier for the group at the head of the master-slave chain
*     containing the group with the supplied slot number is returned.
*     If the group is not part of a chain, then the identifier for the
*     supplied group is returned.

*  Arguments:
*     ISLOT = INTEGER (Given)
*        A slot number.
*     IGRP = INTEGER (Returned)
*        A GRP identifier for the group at the head of the chain.
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

*  Arguments Returned:
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      INTEGER ISLTOW             ! The current owner group slot number.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the identifier for the group which owns the supplied group.
      ISLTOW = ISLOT
      IGRP = CMN_OWNER( ISLTOW )

*  Loop round checking the owner in sucessive levels in the owner-slave
*  chain, until a free group is found.
      DO WHILE( IGRP .NE. GRP__NOID )

*  Find the slot number at which the owner group is stored.
         CALL GRP1_ID2SL( IGRP, ISLTOW )

*  Find the identifier of the group which owns the group stored in
*  slot ISLTOW.
         IGRP = CMN_OWNER( ISLTOW )

      END DO

*  Find the identifier issued for the group with slot number ISLTOW.
      IGRP = CMN_CHK( ISLTOW )

      END
