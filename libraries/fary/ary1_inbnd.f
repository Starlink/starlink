      SUBROUTINE ARY1_INBND( NDIM1, LBND1, UBND1, NDIM2, LBND2, UBND2,
     :                       INSIDE, STATUS )
*+
*  Name:
*     ARY1_INBND

*  Purpose:
*     Test if the bounds of one array lie inside those of another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_INBND( NDIM1, LBND1, UBND1, NDIM2, LBND2, UBND2, INSIDE,
*     STATUS )

*  Description:
*     The routine checks to see if the second set of array bounds
*     supplied lie inside the first set. For this purpose, "inside"
*     means that there are no pixels in the second array which are not
*     present in the first. If the arrays are of different
*     dimensionality, then the bounds of the array with lower
*     dimensionality are padded with 1's before testing them. The array
*     bounds information supplied is not checked for validity.

*  Arguments:
*     NDIM1 = INTEGER (Given)
*        Number of dimensions for the first array.
*     LBND1( NDIM1 ) = INTEGER (Given)
*        Lower bounds of the first array.
*     UBND1( NDIM1 ) = INTEGER (Given)
*        Upper bounds of the first array.
*     NDIM2 = INTEGER (Given)
*        Number of dimensions for the second array.
*     LBND2( NDIM2 ) = INTEGER (Given)
*        Lower bounds of the second array.
*     UBND2( NDIM2 ) = INTEGER (Given)
*        Upper bounds of the second array.
*     INSIDE = LOGICAL (Returned)
*        Whether the second array lies inside the first one.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Test each relevant dimension in turn.
*     -  Obtain the bounds information for both arrays in each
*     dimension, padding with 1's if necessary.
*     -  Check to see if the second array's extent in each dimension
*     lies inside that of the first array; return with INSIDE set to
*     .FALSE. if this is not true for any dimension.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     9-JUN-1989  (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      INTEGER NDIM1
      INTEGER LBND1( NDIM1 )
      INTEGER UBND1( NDIM1 )
      INTEGER NDIM2
      INTEGER LBND2( NDIM2 )
      INTEGER UBND2( NDIM2 )

*  Arguments Returned:
      LOGICAL INSIDE

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER L1                 ! Lower bound of first array
      INTEGER L2                 ! Lower bound of second array
      INTEGER U1                 ! Upper bound of first array
      INTEGER U2                 ! Upper bound of second array

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      INSIDE = .TRUE.

*  Loop to test each relevant dimension.
      DO 1 I = 1, MAX( NDIM1, NDIM2 )

*  Obtain the bounds of the first array in each dimension, padding with
*  1's if necessary.
         IF ( I .LE. NDIM1 ) THEN
            L1 = LBND1 ( I )
            U1 = UBND1 ( I )
         ELSE
            L1 = 1
            U1 = 1
         END IF

*  Similarly, obtain the bounds of the second array.
         IF ( I .LE. NDIM2 ) THEN
            L2 = LBND2( I )
            U2 = UBND2( I )
         ELSE
            L2 = 1
            U2 = 1
         END IF

*  Test to see if the extent of the second array lies inside that of the
*  first array. Return with INSIDE set to .FALSE. if this is not true
*  for any dimension.
         IF ( ( L1 .GT. L2 ) .OR. ( U1 .LT. U2 ) ) THEN
            INSIDE = .FALSE.
            GO TO 2
         END IF
1     CONTINUE
2     CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_INBND', STATUS )

      END
