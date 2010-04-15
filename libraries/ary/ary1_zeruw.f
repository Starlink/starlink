      SUBROUTINE ARY1_ZERUW( N, ARGV, STATUS )
*+
*  Name:
*     ARY1_ZERUW

*  Purpose:
*     Set all elements of a vectorized UNSIGNED WORD array to zero.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_ZERUW( N, ARGV, STATUS )

*  Description:
*     The routine sets all the elements of the UNSIGNED WORD array supplied to
*     zero.

*  Arguments:
*     N = INTEGER (Given)
*        Number of elements in the array.
*     ARGV( N ) = INTEGER*2 (Returned)
*        The vectorised UNSIGNED WORD array to be set to zero.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Set each array element to zero with an assignment statement.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JUN-1989  (RFWS):
*        Original version.
*     30-AUG-1989 (RFWS):
*        Changed name to conform to prefix plus 5 character convention.
*     13-MAR-1990 (RFWS):
*        Renamed from VEC_ZERUW to ARY1_ZERUW.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      INTEGER N

*  Arguments Returned:
      INTEGER*2 ARGV( N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER*2 ZERO                ! Zero
      PARAMETER ( ZERO = 0  )

*  Local variables:
      INTEGER I                  ! Loop counter for array elements

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Assign the value ZERO to each array element.
      DO 1 I = 1, N
         ARGV( I ) = ZERO
1     CONTINUE

      END
