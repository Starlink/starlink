      SUBROUTINE ARD_GRPEX( DESC, IGRP1, IGRP2, FLAG, STATUS )
*+
*  Name:
*     ARD_GRPEX

*  Purpose:
*     Store an ARD description in a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD_GRPEX( DESC, IGRP1, IGRP2, FLAG, STATUS )

*  Description:
*     The supplied ARD description is appended to the group
*     identified by IGRP2. If the symbolic constant GRP__NOID is
*     supplied for IGRP2 then a new group is first created and its
*     identifier is returned in IGRP2.
*
*     If a GRP identifier for an existing group is supplied for IGRP1
*     then the group will be used as the basis for any modification
*     elements contained within the ARD description. No checks are
*     made for modification elements if the symbolic constant GRP__NOID
*     is supplied for IGRP1.

*  Arguments:
*     DESC = CHARACTER * ( * ) (Given)
*        The ARD description.
*     IGRP1 = INTEGER (Given)
*        GRP identifier for a group to be used as a basis for
*        modification elements.
*     IGRP2 = INTEGER (Given and Returned)
*        GRP identifier for the group holding the ARD description.
*     FLAG = LOGICAL (Returned)
*        Returned .TRUE. if the last non-blank character in the
*        supplied ARD description is a minus sign ("-").
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  No checks are made on the syntax of the ARD description.
*     -  The returned GRP identifier (IGRP2) should be deleted using
*     GRP_DELET when it is no longer needed.
*     -  The symbolic constant GRP__NOID is defined in the include file
*     GRP_PAR.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     29-APR-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants

*  Arguments Given:
      CHARACTER DESC*(*)
      INTEGER IGRP1

*  Arguments Returned:
      INTEGER IGRP2
      LOGICAL FLAG

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  ADDED,                   ! No. of elements added to group.
     :  SIZE                     ! Current size of group.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If no group was supplied, create a new empty group.
      IF( IGRP2 .EQ. GRP__NOID ) CALL GRP_NEW( 'ARD DESCRIPTION',
     :                                         IGRP2, STATUS )

*  Ensure that the groups control characters use ";" to delimit
*  elements, and no parenthesised "nests" are allowed.
      CALL GRP_SETCC( IGRP2, 'NUL,DEL,OPEN_N,CLOSE_N', '%;%%', STATUS )

*  Read elements from the group expression and append them to the group.
      CALL GRP_GRPEX( DESC, IGRP1, IGRP2, SIZE, ADDED, FLAG, STATUS )

*  If an error has occurred and add context information.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'DESC', DESC )
         CALL ERR_REP( 'ARD_GRPEX_ERR1', 'ARD_GRPEX: Error reading an'//
     :                 ' ARD description from the string ''^DESC''.',
     :                  STATUS )
      END IF

      END
