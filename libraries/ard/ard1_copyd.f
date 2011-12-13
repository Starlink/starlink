      SUBROUTINE ARD1_COPYD( N, IN, OUT, STATUS )
*+
*  Name:
*     ARD1_COPYD

*  Purpose:
*     Copy double precision values from one array to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_COPYD( N, IN, OUT, STATUS )

*  Description:
*     This routine copies double precision values from one array to another.

*  Arguments:
*     N = INTEGER (Given)
*        The number of values to copy.
*     IN( N ) = DOUBLE PRECISION (Given)
*        The input values.
*     OUT( N ) = DOUBLE PRECISION (Returned)
*        The output values.
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
*     27-JUN-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION IN( N )

*  Arguments Returned:
      DOUBLE PRECISION OUT( N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :        I                  ! Loop count
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do it.
      DO I = 1, N
         OUT( I ) = IN( I )
      END DO

      END
