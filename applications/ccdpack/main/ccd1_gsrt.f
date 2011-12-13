      SUBROUTINE CCD1_GSRT( IGRP, OGRP, STATUS )
*+
*  Name:
*     CCD1_GSRT

*  Purpose:
*     Sorts a GRP group.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_GSRT( IGRP, OGRP, STATUS )

*  Description:
*     This routine creates a new GRP group which is a copy of an old one
*     but with the names sorted.  The case sensitivity attribute of
*     the group is respected.
*
*     The algorithm currently used is an insertion sort, which is not
*     very efficient; if the routine is likely to be used for
*     large or repeated sorts it ought to be recoded.

*  Arguments:
*     IGRP = INTEGER (Given)
*        The GRP identfier of the group to be sorted.
*     OGRP = INTEGER (Returned)
*        The GRP identifier of the sorted copy of the group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     7-FEB-2001 (MBT):
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
      INTEGER IGRP

*  Arguments Returned:
      INTEGER OGRP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      INTEGER K                  ! Loop variable
      INTEGER NITEM              ! Group size
      LOGICAL CSFLAG             ! Is group case sensitive?
      CHARACTER * ( GRP__SZNAM ) CINAME ! Case-folded name from input group
      CHARACTER * ( GRP__SZNAM ) CONAME ! Case-folded name from output group
      CHARACTER * ( GRP__SZNAM ) INAME ! Name from input group
      CHARACTER * ( GRP__SZNAM ) ONAME ! Name from output group
      CHARACTER * ( GRP__SZNAM ) TNAME ! Temporary name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the size of the input group.
      CALL GRP_GRPSZ( IGRP, NITEM, STATUS )

*  Get the case sensitivity flag of the input group.
      CALL GRP_SETCS( IGRP, CSFLAG, STATUS )

*  Create a new group as a copy of the old one.  This copies over the
*  various associated data.  The first item is transferred across.
      CALL GRP_COPY( IGRP, 1, 1, .FALSE., OGRP, STATUS )

*  Copy the next ones across into the sorted positions.
      DO I = 2, NITEM

*  Get the name from the input group.
         CALL GRP_GET( IGRP, I, 1, INAME, STATUS )

*  Fold case if necessary.
         CINAME = INAME
         IF ( .NOT. CSFLAG ) CALL CHR_UCASE( CINAME )

*  Look through the output group to see where to insert it.
         DO J = 1, I - 1
            CALL GRP_GET( OGRP, J, 1, ONAME, STATUS )
            CONAME = ONAME
            IF ( .NOT. CSFLAG ) CALL CHR_UCASE( CONAME )

*  If we need to insert it here, shift the higher names up one and
*  slip it in to the gap.
            IF ( CINAME .LT. CONAME ) THEN
               DO K = I - 1, J, -1
                  CALL GRP_GET( OGRP, K, 1, TNAME, STATUS )
                  CALL GRP_PUT( OGRP, 1, TNAME, K + 1, STATUS )
               END DO
               CALL GRP_PUT( OGRP, 1, INAME, J, STATUS )
               GO TO 1
            END IF
         END DO

*  Not inserted in the middle of the group, so append it to the end.
         CALL GRP_PUT( OGRP, 1, INAME, 0, STATUS )
  1      CONTINUE
      END DO

      END
* $Id$
