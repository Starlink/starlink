      SUBROUTINE GRP_SHOW( IGRP, SLAVES, STATUS )
*+
*  Name:
*     GRP_SHOW

*  Purpose:
*     List contents of a group to the screen.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_SHOW( IGRP, SLAVES, STATUS )

*  Description:
*     The contents of the supplied group are listed to the screen, with
*     one name on each line. If the group has a non-blank type string, it
*     is dislayed first, prefixed with the groups current comment character
*     (see routine GRP_SETCC). If SLAVES is non-zero, then any groups
*     connected to the supplied group via and "owner-slave" relationship
*     (see GRP_OWN) are also displayed.

*  Arguments:
*     IGRP = INTEGER (Given)
*        The GRP identifier for the group to be displayed.
*     SLAVES = INTEGER (Given)
*        Controls the display of related owner or slave groups. If zero,
*        then only the supplied group is displayed. If 1 then the group
*        at the head of the "owner-slave" chain that contains the
*        supplied group is displayed first. Other groups in the chain are
*        then displayed, ending with the supplied group itself. If 2 then
*        the supplied group is displayed first, followed by any slave
*        groups that are owned by the supplied group, down to the foot
*        of the chain. If 3 then the entire chain containing the supplied
*        group, from the head to the foot is displayed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the group is case insensitive (as set up by a call to
*     routine GRP_SETCS) then the names are written out in upper case,
*     otherwise they are written out as supplied.

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
      INCLUDE 'GRP_ERR'          ! GRP error constants

*  Arguments Given:
      INTEGER IGRP
      INTEGER SLAVES

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER TYPE*(GRP__SZTYP)! The type string for the group
      INTEGER IGRP1              ! The next group to display
      INTEGER IGRP2              ! The last group to display
      INTEGER IGRP3              ! The slave group
      LOGICAL FIRST              ! Is this the first group?
      LOGICAL SAME               ! Do two id's refer to same group?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get identifiers for the first and last group to display.
      IF( SLAVES .EQ. 0 ) THEN
         IGRP1 = IGRP
         IGRP2 = IGRP

      ELSE IF( SLAVES .EQ. 1 ) THEN
         CALL GRP_HEAD( IGRP, IGRP1, STATUS )
         IGRP2 = IGRP

      ELSE IF( SLAVES .EQ. 2 ) THEN
         IGRP1 = IGRP
         IGRP2 = GRP__NOID

      ELSE IF( SLAVES .EQ. 3 ) THEN
         CALL GRP_HEAD( IGRP, IGRP1, STATUS )
         IGRP2 = GRP__NOID

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = GRP__BDCHN
         CALL MSG_SETI( 'S', SLAVES )
         CALL ERR_REP( ' ', 'GRP_SHOW: Bad value (^S) supplied '//
     :                 'for argument SLAVES (programming error).',
     :                 STATUS )
      END IF

*  Loop until we have displayed all required groups.
      FIRST = .TRUE.
      DO WHILE ( IGRP1 .NE. GRP__NOID .AND. STATUS .EQ. SAI__OK )

*  If we are not displaying the first group , write out a group sererator line.
         IF( FIRST ) THEN
            CALL MSG_BLANK( STATUS )
            CALL MSG_OUT( ' ', '--------------------------------------',
     :                    STATUS )
            FIRST = .FALSE.
         END IF

*  Get the group type string.
         CALL GRP_GTYPE( IGRP1, TYPE, STATUS )

*  List the contents of the group to the screen.
         CALL GRP1_LISTU( -1, 0, 0, TYPE, IGRP1, STATUS )

*  If we have just displayed the last required group, we set the next
*  group identifier to NOID to force the loop to exit.
         CALL GRP_SAME( IGRP1, IGRP2, SAME, STATUS )
         IF( SAME ) THEN
            IGRP1 = GRP__NOID

*  Otherwise, get the identifier for the slave group that is owned by the
*  previous group. */
         ELSE
            CALL GRP_SLAVE( IGRP1, IGRP3, STATUS )
            IGRP1 = IGRP3
         END IF

      END DO

*  If an error occurred, give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GRP_SHOW_ERR2', 'GRP_SHOW: Unable to display '//
     :                 'the contents of a group', STATUS )
      END IF

      END
