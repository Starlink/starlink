      SUBROUTINE ARY1_XSBND( NDIM1, LBND1, UBND1, NDIM2, LBND2, UBND2,
     :                       NDIM, LBND, UBND, EXIST, STATUS )
*+
*  Name:
*     ARY1_XSBND

*  Purpose:
*     Calculate bounds of the intersection set between two arrays.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_XSBND( NDIM1, LBND1, UBND1, NDIM2, LBND2, UBND2, NDIM,
*     LBND, UBND, EXIST, STATUS )

*  Description:
*     The routine calculates the bounds of the region in common between
*     two arrays, whose lower and upper bounds are supplied. If the
*     dimensionalities of the two arrays differ, then the bounds of the
*     array with lower dimensionality are padded with 1's before
*     calculating the intersection region. The resulting bounds are
*     returned up to the dimensionality limit set by the size of the
*     output arrays; if these require more values than are available,
*     then they are padded with 1's.  If no intersection region exists,
*     then the logical EXIST flag is set to .FALSE. to indicate this.
*     No checks are performed to ensure that the array bounds supplied
*     are valid.

*  Arguments:
*     NDIM1 = INTEGER (Given)
*        Number of dimensions for first array.
*     LBND1( NDIM1 ) = INTEGER (Given)
*        Lower bounds of first array.
*     UBND1( NDIM1 ) = INTEGER (Given)
*        Upper bounds of first array.
*     NDIM2 = INTEGER (Given)
*        Number of dimensions for second array.
*     LBND2( NDIM2 ) = INTEGER (Given)
*        Lower bounds of second array.
*     UBND2( NDIM2 ) = INTEGER (Given)
*        Upper bounds of second array.
*     NDIM = INTEGER (Given)
*        Number of elements in output arrays.
*     LBND( NDIM ) = INTEGER (Returned)
*        Lower bounds of the intersection region (if it exists).
*     UBND( NDIM ) = INTEGER (Returned)
*        Upper bounds of the intersection region (if it exists).
*     EXIST = LOGICAL (Returned)
*        Whether the intersection region exists.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Find the lower and upper bounds of each array in each relevant
*     dimension, padding the bounds information with 1's if necessary.
*     -  Calculate the intersection bounds for each dimension.
*     -  Return with EXIST set to .FALSE. if the array bounds in any
*     dimension do not include an overlap region.

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
*     8-JUN-1989  (RFWS):
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
      INTEGER NDIM

*  Arguments Returned:
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      LOGICAL EXIST

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER L                  ! Lower bound of intersection region
      INTEGER L1                 ! Lower bound of first array
      INTEGER L2                 ! Lower bound of second array
      INTEGER U                  ! Upper bound of intersection region
      INTEGER U1                 ! Upper bound of first array
      INTEGER U2                 ! Upper bound of second array

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      EXIST = .TRUE.

*  Loop to process each relevant dimension.
      DO 1 I = 1, MAX( NDIM1, NDIM2, NDIM )

*  Obtain the bounds of the first array in this dimension, padding with
*  1's if necessary.
         IF ( I .LE. NDIM1 ) THEN
            L1 = LBND1( I )
            U1 = UBND1( I )
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

*  Calculate the bounds of the overlap region.
         L = MAX( L1, L2 )
         U = MIN( U1, U2 )

*  If there is no overlap, then return with EXIST set to .FALSE..
         IF ( L .GT. U ) THEN
            EXIST = .FALSE.
            GO TO 2

*  Put the bounds of the overlap region into the output arrays, if
*  there is room.
         ELSE IF ( I .LE. NDIM ) THEN
            LBND( I ) = L
            UBND( I ) = U
         END IF
1     CONTINUE
2     CONTINUE
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_XSBND', STATUS )

      END
