      SUBROUTINE IRQ1_COUNT( SIZE, DATA, NGOOD, NBAD, STATUS )
*+
*  Name:
*     IRQ1_COUNT

*  Purpose:
*     Count the good and bad pixels in a vector.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_COUNT( SIZE, DATA, NGOOD, NBAD, STATUS )

*  Description:
*     The number of good and bad pixels in the input data are returned.
*  Arguments:
*     SIZE = INTEGER*8 (Given)
*        Size of the data vector.
*     DATA( SIZE ) = REAL (Given)
*        Data vector.
*     NGOOD = INTEGER*8 (Returned)
*        Number of pixels in DATA not equal to VAL__BADR.
*     NBAD = INTEGER*8 (Returned)
*        Number of pixels in DATA equal to VAL__BADR.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     29-JUL-1991 (DSB):
*        Original version.
*     24-OCT-2019 (DSB):
*        Change to use 8-byte args.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.

*  Arguments Given:
      INTEGER*8 SIZE
      REAL DATA( SIZE )

*  Arguments Returned:
      INTEGER*8 NGOOD
      INTEGER*8 NBAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER*8 I                ! Loop count.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the counters.
      NGOOD = 0
      NBAD = 0

*  Loop round all the supplied data, counting good and bad pixels.
      DO I = 1, SIZE
         IF( DATA( I ) .EQ. VAL__BADR ) THEN
            NBAD = NBAD + 1
         ELSE
            NGOOD = NGOOD + 1
         END IF
      END DO

      END
