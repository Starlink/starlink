      SUBROUTINE ARY1_NEL( NDIM, LBND, UBND, EL, STATUS )
*+
*  Name:
*     ARY1_NEL

*  Purpose:
*     Calculate the number of elements in an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_NEL( NDIM, LBND, UBND, EL, STATUS )

*  Description:
*     The routine calculates the number of elements in a
*     multi-dimensional array from the lower and upper bounds
*     information. The bounds information is not checked for validity.

*  Arguments:
*     NDIM = INTEGER (Given)
*        Number of array dimensions.
*     LBND( NDIM ) = INTEGER (Given)
*        Lower array bounds.
*     UBND( NDIM ) = INTEGER (Given)
*        Upper array bounds.
*     EL = INTEGER (Returned)
*        Number of elements in the array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Multiply together the array extents in each dimension.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-JUN-1989  (RFWS):
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
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )

*  Arguments Returned:
      INTEGER EL

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER I                  ! Loop counter for dimensions

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      EL = 1

*  Multiply together the array extents in each dimension.
      DO 1 I = 1, NDIM
         EL = EL * ( UBND( I ) - LBND( I ) + 1 )
1     CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_NEL', STATUS )

      END
