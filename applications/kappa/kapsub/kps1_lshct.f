      SUBROUTINE KPS1_LSHCT( NPOS, IDS, FIRST, LAST, STEP, NDISP,
     :                       STATUS )
*+
*  Name:
*     KPS1_LSHCT

*  Purpose:
*     Count the number of positions to be displayed by LISTSHOW.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LSHCT( NPOS, IDS, FIRST, LAST, STEP, NDISP, STATUS )

*  Description:
*     This routine counts the number of positions identifiers between the
*     supplied limits.

*  Arguments:
*     NPOS = INTEGER (Given)
*        The number of supplied position identifiers.
*     IDS = INTEGER( NPOS ) (Given)
*        The supplied position identifiers.
*     FIRST = INTEGER (Given)
*        The lowest position identifier to be selected.
*     LAST = INTEGER (Given)
*        The highest position identifier to be selected.
*     STEP = INTEGER (Given)
*        The increment between position identifier to be selected.
*     NDISP = INTEGER (Returned)
*        The number of positions identifiers within IDS which are within
*        the range FIRST to LAST (inclusive).
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-SEP-1998 (DSB):
*        Original version.
*     18-FEB-2010 (DSB):
*        Added argument STEP.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NPOS
      INTEGER IDS( NPOS )
      INTEGER FIRST
      INTEGER LAST
      INTEGER STEP

*  Arguments Returned:
      INTEGER NDISP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count
*.

*  Initialise.
      NDISP = 0

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check each identifier.
      DO I = 1, NPOS
         IF( IDS( I ) .GE. FIRST .AND.
     :       IDS( I ) .LE. LAST .AND.
     :       MOD( IDS( I ) - FIRST, STEP ) .EQ. 0 ) NDISP = NDISP + 1
      END DO

      END
