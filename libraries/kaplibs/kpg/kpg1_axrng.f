      SUBROUTINE KPG1_AXRNG( EL, CENTRE, WIDTH, ASTART, AEND, STATUS )
*+
*  Name:
*     KPG1_AXRNG

*  Purpose:
*     Calculates the extent of an NDF along an axis.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_AXRNG( EL, CENTRE, WIDTH, ASTART, AEND, STATUS )

*  Description:
*     This routine calculates the starting and ending positions of an
*     NDF's pixels along an axis, taking account of the axis width
*     values.

*  Arguments:
*     EL = INTEGER (Given)
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
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
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
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-APR-1991 (RFWS):
*        Original version.
*     1993 January 4 (MJC):
*        Virually re-written, since previous method of passing the lower
*        and upper-bound centres and widths does not work, if there is
*        no width array and the centres are not incremented by one.
*        Uses completely different input arguments.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER EL
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
