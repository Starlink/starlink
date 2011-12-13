      INTEGER FUNCTION ARD1_CEIL( VALUE )
*+
*  Name:
*     ARD1_CEIL

*  Purpose:
*     Return the smallest integer larger than or equal to a supplied value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = ARD1_CEIL( VALUE )

*  Description:
*     This routine returns the smallest integer larger than or equal to
*     a supplied value.

*  Arguments:
*     VALUE = DOUBLE PRECISION (Given)
*        The value.

*  Function Value:
*     ARD1_CEIL = INTEGER
*        The smallest integer larger than or equal to the supplied value.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-AUG-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      DOUBLE PRECISION VALUE

*  Do it.
      IF( VALUE .GT. VAL__MAXI ) THEN
         ARD1_CEIL = VAL__MAXI

      ELSE IF( VALUE .LT. VAL__MINI ) THEN
         ARD1_CEIL = VAL__MINI

      ELSE
         ARD1_CEIL = INT( VALUE )
         IF( DBLE( ARD1_CEIL ) .LT. VALUE ) ARD1_CEIL = ARD1_CEIL + 1
      END IF

      END

