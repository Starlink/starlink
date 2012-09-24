      SUBROUTINE GRP_SHOW( IGRP, SLAVES, STATUS )
*+
*  Name:
*     GRP_SHOW

*  Purpose:
*     List contents ofa group to the screen.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_SHOW( IGRP, SLAVES, STATUS )

*  Description:
*     The contents of the supplied group are listed to the screen, with
*     one name on each line. If the group has a non-blank type string, it
*     is dislayed first, prefixed with the groups current comment character
*     (see routine GRP_SETCC). If SLAVES is .TRUE., then any
*     slave group owned by the supplied group is also displayed. This is
*     recursive - any slave group owned by a displayed slave group is also
*     displayed.

*  Arguments:
*     IGRP = INTEGER (Given)
*        The GRP identifier for the group to be displayed.
*     SLAVES = LOGICAL (Given)
*        If TRUE., then the entire chain of slaved groups owned by the
*        specified group are also displayed.
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

*  Arguments Given:
      INTEGER IGRP
      LOGICAL SLAVES

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER TYPE*(GRP__SZTYP)! The type string for the group
      INTEGER IGRP1              ! The group to display
      INTEGER IGRP2              ! The slave group
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Take a copy of the supplied group identifier so we do not modify it
*  below.
      IGRP1 = IGRP

*  Loop until we have displayed all required groups.
      DO WHILE ( IGRP1 .NE. GRP__NOID .AND. STATUS .EQ. SAI__OK )

*  If we are not displaying the supplied group ( i.e. if we are displaying
*  a slave), write out a group sererator line.
         IF( IGRP1 .NE. IGRP ) THEN
            CALL MSG_BLANK( STATUS )
            CALL MSG_OUT( ' ', '--------------------------------------',
     :                    STATUS )
         END IF

*  Get the group type string.
         CALL GRP_GTYPE( IGRP1, TYPE, STATUS )

*  List the contents of the group to the screen.
         CALL GRP1_LISTU( -1, 0, 0, TYPE, IGRP1, STATUS )

*  If required, get the identifier for the slvae group that is owned by
*  the displayed group. */
         IF( SLAVES ) THEN

*  Get the identifier for the slave group - if any.
            CALL GRP_SLAVE( IGRP1, IGRP2, STATUS )

*  Use the slave identifier in place of the old identifier.
            IGRP1 = IGRP2
         END IF

      END DO

*  If an error occurred, give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GRP_SHOW_ERR2', 'GRP_SHOW: Unable to display '//
     :                 'the contents of a group', STATUS )
      END IF

      END
