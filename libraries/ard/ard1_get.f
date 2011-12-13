      SUBROUTINE ARD1_GET( IGRP, INDX, SIZE, NAMES, STATUS )
*+
*  Name:
*     ARD1_GET

*  Purpose:
*     Returns a set of names contained in a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_GET( IGRP, INDX, SIZE, NAMES, STATUS )

*  Description:
*     This is a wrapper for GRP_GET which removes any strings "<!!" or
*     "!!>" from the returned text.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group.
*     INDX = INTEGER (Given)
*        The lowest index for which the corresponding name is required.
*     SIZE = INTEGER (Given)
*        The number of names required.
*     NAMES( SIZE ) = CHARACTER * ( * ) (Returned)
*        The names held at the given positions in the group. The
*        corresponding character variables should have declared length
*        specified by the symbolic constant GRP__SZNAM. If the declared
*        length is shorter than this, the returned names may be
*        truncated, but no error is reported.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     30-AUG-2001 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER IGRP
      INTEGER INDX
      INTEGER SIZE

*  Arguments Returned:
      CHARACTER NAMES( SIZE )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Index into the group NAMES array
      INTEGER START              ! Inded of the start of the found string
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the text.
      CALL GRP_GET( IGRP, INDX, SIZE, NAMES, STATUS )

*  Loop round each returned name.
      DO I = 1, SIZE

*  Loop removing all occurences of the string "<!!".
         START = INDEX( NAMES( I ), '<!!' )
         DO WHILE( START .GT. 0 )
            NAMES( I )( START : ) = NAMES( I )( START + 3 : )
            START = INDEX( NAMES( I ), '<!!' )
         END DO

*  Loop removing all occurences of the string "!!>".
         START = INDEX( NAMES( I ), '!!>' )
         DO WHILE( START .GT. 0 )
            NAMES( I )( START : ) = NAMES( I )( START + 3 : )
            START = INDEX( NAMES( I ), '!!>' )
         END DO

      END DO

      END
