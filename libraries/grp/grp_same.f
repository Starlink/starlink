      SUBROUTINE GRP_SAME( IGRP1, IGRP2, SAME, STATUS )
*+
*  Name:
*     GRP_SAME

*  Purpose:
*     Determine if two GRP identifiers refer to the same group

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_SAME( IGRP1, IGRP2, SAME, STATUS )

*  Description:
*     The routines returns a flag indicating if the two supplied GRP
*     identifiers refer to the same group.

*  Arguments:
*     IGRP1 = INTEGER (Given)
*        The first GRP identifier.
*     IGRP2 = INTEGER (Given)
*        The second GRP identifier.
*     SAME = LOGICAL (Returned)
*        Returned set to .TRUE. if the two identifiers refer to the
*        same group, and .FALSE. otherwise. If both identifiers are
*        GRP__NOID, a value of .TRUE. will be returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - No error is reported if either of the identifiers is GRP__NOID.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     24-SEP-2012 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      INTEGER IGRP1
      INTEGER IGRP2

* Arguments Returned:
      LOGICAL SAME

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      INTEGER SLOT1              ! Common array slot for IGRP1
      INTEGER SLOT2              ! Common array slot for IGRP2
*.

*  Initialise returned value
      SAME = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If both identifiers are GRP__NOID, return true.
      IF( IGRP1 .EQ. GRP__NOID .AND. IGRP2 .EQ. GRP__NOID ) THEN
         SAME = .TRUE.

*  If neither identifier is GRP__NOID, test the slot numbers.
      ELSE IF( IGRP1 .NE. GRP__NOID .AND. IGRP2 .NE. GRP__NOID ) THEN
         CALL GRP1_IMPID( IGRP1, SLOT1, STATUS )
         CALL GRP1_IMPID( IGRP2, SLOT2, STATUS )

*  If succesful, return a flag indicating if the slots are the same.
         IF ( STATUS .EQ. SAI__OK ) SAME = ( SLOT1 .EQ. SLOT2 )

*  If one bit not both identifiers are GRP__NOID, return false.
      ELSE
         SAME = .FALSE.
      END IF

*  If an error occurred give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GRP_SAME_ERR2', 'GRP_SAME: Unable to '//
     :                 'determine if two GRP identifiers refer to '//
     :                 'the same group.', STATUS )
      END IF

      END
