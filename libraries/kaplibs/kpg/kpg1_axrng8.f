      SUBROUTINE KPG1_AXRNG8( EL, CENTRE, WIDTH, ASTART, AEND, STATUS )
*+
*  Name:
*     KPG1_AXRNG8

*  Purpose:
*     Calculates the extent of an NDF along an axis.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_AXRNG8( EL, CENTRE, WIDTH, ASTART, AEND, STATUS )

*  Description:
*     This routine is equivalent to KPG1_AXRNG except that argument
*     EL is INTEGER*8 instead of INTEGER. See KPG1_AXRNG for more
*     information.

*  Arguments:
*     EL = INTEGER*8 (Given)
*        The number of elements in the axis arrays.
*     CENTRE( EL ) = DOUBLE PRECISION (Given)
*        The centres of the pixels on the axis.
*     WIDTH( EL ) = DOUBLE PRECISION (Given)
*        The widths of the pixels on the axis.
*     ASTART = DOUBLE PRECISION (Returned)
*        If the axis centre positions increase with NDF pixel index,
*        this argument returns the axis position of the edge of the
*        first pixel which has the lower co-ordinate. Otherwise it
*        returns the axis position of the edge with the higher
*        co-ordinate.
*     AEND = DOUBLE PRECISION (Returned)
*        If the axis centre positions increase with NDF pixel index,
*        this argument returns the axis position of the edge of the
*        last pixel which has the higher co-ordinate. Otherwise it
*        returns the axis position of the edge with the lower
*        co-ordinate.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     4-OCT-2019 (DSB):
*        Original version, copied from KPG1_AXRNG and changed to use
*        INTEGER*8 pixel counts.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER*8 EL
      DOUBLE PRECISION CENTRE( EL )
      DOUBLE PRECISION WIDTH ( EL )

*  Arguments Returned:
      DOUBLE PRECISION ASTART
      DOUBLE PRECISION AEND

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the axis centre values increase with pixel index, then calculate
*  the extent.
      IF ( CENTRE( 1 ) .LE. CENTRE( EL ) ) THEN
         ASTART = CENTRE( 1 ) - 0.5D0 * ABS( WIDTH ( 1 ) )
         AEND = CENTRE( EL ) + 0.5D0 * ABS( WIDTH( EL ) )

*  Otherwise calculate the extent appropriate to decreasing axis centre
*  values.
      ELSE
         ASTART = CENTRE( 1 ) + 0.5D0 * ABS( WIDTH ( 1 ) )
         AEND = CENTRE( EL ) - 0.5D0 * ABS( WIDTH( EL ) )
      END IF

      END
