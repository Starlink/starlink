      SUBROUTINE KPG1_PGSHT( HGT, STATUS )
*+
*  Name:
*     KPG1_PGSHT

*  Purpose:
*     Sets the PGPLOT character size in world co-ordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL  KPG1_PGSHT( HGT, STATUS )

*  Description:
*     This routine sets the PGPLOT character size to a specified value in
*     world co-ordinates. It mimics SGS_SHTX in so far as this is possible.
*
*     Note SGS and PGPLOT behave differently if the scales on the X and Y
*     axes are not equal. SGS keeps the character size constant in world
*     oordinates, so absolute character size will be different for vertical
*     and horizontal text. On the other hand, PGPLOT keeps the absolute
*     character size fixed, resulting in the characters size in world
*     co-ordinates varying for horizontal and vertical text. This routine
*     sets the size for horizontal text. If the axis scales are not equal,
*     vertical text will have have a different size (in world co-ordinates).

*  Arguments:
*     HGT = REAL (Given)
*        The required character height, in world co-ordinates.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-MAR-1998 (DSB):
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
      REAL HGT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL FACTOR                ! Dimensionless scale factor for text size
      REAL XCH                   ! Text size in X axis units
      REAL YCH                   ! Text size in Y axis units
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the current PGPLOT character heights in world co-ordinates.
      CALL PGQCS( 4, XCH, YCH )

*  Get the current scale factor for character size.
      CALL PGQCH( FACTOR )

*  Set the new scale factor to give the required height in world
*  co-ordinates (assuming horizontal text).
      IF( YCH .NE. 0.0 ) CALL PGSCH( HGT*FACTOR/YCH )

      END
