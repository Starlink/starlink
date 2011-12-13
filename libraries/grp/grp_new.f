      SUBROUTINE GRP_NEW( TYPE, IGRP, STATUS )
*+
*  Name:
*     GRP_NEW

*  Purpose:
*     Create a new empty group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_NEW( TYPE, IGRP, STATUS )

*  Description:
*     A new empty group is created and an identifier to it is returned
*     in IGRP. The string supplied in TYPE is stored with the group,
*     and should be used to store a description of the contents of the
*     group (see also routines GRP_GTYPE and GRP_PTYPE).
*
*     The created group has the default control characters described in
*     routine GRP_SETCC, and has no "owner" group (see GRP_OWN).

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        A descriptive string to be associated with the group. The
*        maximum length for a TYPE string is given by GRP__SZTYP.
*        Supplied characters beyond this length are ignored.
*     IGRP = INTEGER (Returned)
*        An identifier for the created group. GRP__NOID is returned if
*        an error occurs.
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
      INCLUDE 'GRP_PAR'          ! GRP public constants.

*  Arguments Given:
      CHARACTER TYPE*(*)

*  Arguments Returned:
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables;
      INTEGER SLOT               ! Index within the common arrays at
                                 ! which information about the new group
                                 ! is stored.

*.

*  Set the GRP identifier to an invalid value before checking the
*  status.
      IGRP = GRP__NOID

*  Check inherited global status. If bad, return with an invalid GRP
*  identifier.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the index of the next free slot in the common arrays.
*  Information describing the global properties of the new group will
*  be stored in this slot. An array is created in temporary workspace
*  to hold the contents of the group.
      CALL GRP1_GTSLT( TYPE, SLOT, STATUS )

*  Create an encoded identifier for the new group.
      CALL GRP1_EXPID( SLOT, IGRP, STATUS )

*  If an error occurred, delete the group and give a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( IGRP, STATUS )
         CALL ERR_REP( 'GRP_NEW_ERR1',
     :                 'GRP_NEW: Unable to create a new group', STATUS )
      END IF

      END
