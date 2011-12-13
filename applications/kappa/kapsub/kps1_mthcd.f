      SUBROUTINE KPS1_MTHCD( AEL, AXIS, STRIDE, EL, ARRAY, STATUS )
*+
*  Name:
*     KPS1_MTHCx

*  Purpose:
*     Fills a vectorised n-d array with co-ordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MTHCx( AEL, AXIS, STRIDE, EL, ARRAY, STATUS )

*  Description:
*     This routine fills a vectorised n-dimensional array with data or
*     pixel co-ordinates for processing co-ordinates in MATHS.  The
*     filled array should have the dimensions of the output array
*     created by MATHS.

*  Arguments:
*     AEL = INTEGER (Given)
*        The number of elements in the AXIS array.
*     AXIS( AEL ) = ? (Given)
*        The array of co-ordinates to be used to fill the array.
*     STRIDE = INTEGER (Given)
*        The number of elements to stride in the ARRAY before
*        incrementing the index of the AXIS value being copied to the
*        ARRAY..
*     EL = INTEGER (Given)
*        The number of elements in the ARRAY array.
*     ARRAY( EL ) = ? (Returned)
*        The array to fill with co-ordinates from AXIS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  There is a routine for the double precision and real data
*     types: replace "x" in the routine name by D or R as appropriate.
*     The AXIS and ARRAY arguments must have the data type specified.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 September 18 (MJC):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER AEL
      DOUBLE PRECISION AXIS( AEL )
      INTEGER STRIDE
      INTEGER EL

*  Arguments Returned:
      DOUBLE PRECISION ARRAY( AEL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Index to ARRAY element
      INTEGER J                  ! Loop counter through the stride
      INTEGER K                  ! Loop counter through the axis
                                 ! elements                                 ! stride

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop for each element of the array in strides.
      DO K = 1, EL / STRIDE
         DO J = 1, STRIDE

*  Generate the index to array element.
            I = ( K - 1 ) * STRIDE + J

*  Fill the array with axis values.  Mod of the axis element is needed
*  for the first axis, as the STRIDE will be 1.
            ARRAY( I ) = AXIS( MOD( K - 1, AEL ) + 1 )
         END DO
      END DO

      END
