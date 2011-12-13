      SUBROUTINE CCD1_GRDEL( GID, STATUS )
*+
*  Name:
*     CCD1_GRDEL

*  Purpose:
*     Release resources associated with a group.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_GRDEL( GID, STATUS )

*  Description:
*     This routine may be called to release resources associated with
*     a GRP group.  If a valid group is identified by the GRP identifier
*     GID, then it is deleted using GRP_DELET.  If GID is not a valid
*     group identifier however, no such call will be made (it is an
*     error to call GRP_DELET on an invalid group identifier).
*     It is therefore safe to call this routine, unlike GRP_DELET,
*     on an identifier which may or may not refer to a valid group.
*
*     The routine will call GRP_DELET even if STATUS is bad on entry;
*     in this case no further error report will be made.

*  Arguments:
*     GID = INTEGER (Given and Returned)
*        If GID is a valid identifier for a GRP group on entry to the
*        routine, that group will be annulled.  In any case, this
*        argument will be returned as GRP__NOID.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-JUL-2000 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants

*  Arguments Given:
      INTEGER GID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL VALID              ! Does GID represent a valid group?

*.

*  Check inherited global status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If the status is good, then inquire whether the group is valid.
         CALL GRP_VALID( GID, VALID, STATUS )
      END IF

*  If either the inherited status is bad, or the group is valid, then
*  annul the group.
      IF ( STATUS .NE. SAI__OK .OR. VALID ) THEN
         CALL GRP_DELET( GID, STATUS )

*  If there was no valid group, then set the input value to GRP__NOID
*  by hand.
      ELSE
         GID = GRP__NOID
      END IF

      END
* $Id$
