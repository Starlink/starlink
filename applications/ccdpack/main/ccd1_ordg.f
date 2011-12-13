      SUBROUTINE CCD1_ORDG( AGRP1, BGRP1, AGRP2, BGRP2, STATUS )
*+
*  Name:
*     CCD1_ORDG

*  Purpose:
*     Generates an ordered subgroup.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_ORDG( AGRP1, BGRP1, AGRP2, BGRP2, STATUS )

*  Description:
*     This routine creates a group from another one with members and
*     ordering corresponding to a given pair of groups.
*     Given GRP groups AGRP1 and BGRP1 which define the correspondence
*     between pairs of names, this routine will generate a new group
*     BGRP2 which corresponds in the same way to a given group AGRP2.
*     So if AGRP1 consists of the names AX, AY, AZ; BGRP1 consists
*     of the names BX, BY, BZ; and AGRP2 consists of the name AY,
*     a new group BGRP2 will be created consisting of the name BY.
*
*     All groups are treated as plain GRP groups (i.e. any NDG-type
*     supplemental information will be ignored).

*  Arguments:
*     AGRP1 = INTEGER (Given)
*        GRP identifier of the A-type template group.
*     BGRP1 = INTEGER (Given)
*        GRP identifier of the B-type template group.
*     AGRP2 = INTEGER (Given)
*        GRP identifier of the A-type actual group.
*     BGRP2 = INTEGER (Returned)
*        GRP identifier of the B-type actual group, constructed by
*        this routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     Use of this subroutine is not a particularly efficient way to
*     proceed; it would in principle be possible not to lose track of
*     the ordering of the output group in the first place.  However,
*     because of the small size of the lists involved, it's hardly
*     going to be a performance bottleneck, and it gives the calling
*     code fewer things to worry about.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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
*     12-FEB-2001 (MBT):
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
      INTEGER AGRP1
      INTEGER BGRP1
      INTEGER AGRP2

*  Arguments Returned:
      INTEGER BGRP2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER IGOT               ! Position of name in AGRP1
      INTEGER N2                 ! Size of the output group
      CHARACTER * ( GRP__SZNAM ) ANAME ! Name got from AGRP1
      CHARACTER * ( GRP__SZNAM ) BNAME ! Name got from BGRP1

*.

*  Initialise error status return value.
      BGRP2 = GRP__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the size of the output group.
      CALL GRP_GRPSZ( AGRP2, N2, STATUS )

*  Loop through each name in AGRP2 finding the corresponding one in AGRP1.
      DO I = 1, N2

*  Get the name in AGRP2.
         CALL GRP_GET( AGRP2, I, 1, ANAME, STATUS )

*  Find the corresponding position in AGRP1.
         CALL GRP_INDEX( ANAME, AGRP1, 1, IGOT, STATUS )
         IF ( IGOT .LE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_ORDG', 'CCD1_ORDG: Matching name '//
     :                    'not found in group (programming error?)',
     :                    STATUS )
            GO TO 99
         END IF

*  Copy the corresponding name from BGRP1 to BGRP2.  For the first
*  item only use GRP_COPY, since this should copy across all the
*  auxiliary data like case significance flags etc.
         IF ( I .EQ. 1 ) THEN
            CALL GRP_COPY( BGRP1, IGOT, IGOT, .FALSE., BGRP2, STATUS )
         ELSE
            CALL GRP_GET( BGRP1, IGOT, 1, BNAME, STATUS )
            CALL GRP_PUT( BGRP2, 1, BNAME, I, STATUS )
         END IF
      END DO

*  Error exit label.
 99   CONTINUE

      END
* $Id$
