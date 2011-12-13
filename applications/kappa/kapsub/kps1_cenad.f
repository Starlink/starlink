      SUBROUTINE KPS1_CENAD( NDIMS, SLBND, SUBND, DIN, VIN,
     :                         VLBND, VUBND, OUT, STATUS )
*+
*  Name:
*     KPS1_CENAD

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CENAD( NDIMS, SLBND, SUBND, DIN, VIN, VLBND, VUBND,
*                      OUT, STATUS )

*  Description:
*     This routine copies a specified section of the supplied data array
*     (DIN) to an output array (OUT), and then adds Gaussian noise to the
*     output values. The variance of the noise to add at each pixel is
*     specified by the VIN array.

*  Arguments:
*     NDIMS = INTEGER (Given)
*        The number of axes.
*     SLBND( NDIMS ) = INTEGER (Given)
*        The lower pixel index bounds of the DIN and VIN arrays.
*     SUBND( NDIMS ) = INTEGER (Given)
*        The upper pixel index bounds of the DIN and VIN arrays.
*     DIN( * ) = DOUBLE PRECISION (Given)
*        The data values. Bounds given by SLBND and SUBND.
*     VIN( * ) = DOUBLE PRECISION (Given)
*        The variance values. Bounds given by SLBND and SUBND.
*     VLBND( NDIMS ) = INTEGER (Given)
*        The lower pixel index bounds of the area of the DIN to be copied
*        to OUT. These are also the lower pixel index bounds of the OUT
*        array.
*     VUBND( NDIMS ) = INTEGER (Given)
*        The upper pixel index bounds of the area of the DIN to be copied
*        to OUT.
*     OUT( * ) = DOUBLE PRECISION (Given)
*        The output data values with added noise. Bounds given by VLBND
*        and VUBND.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the output array is not completely contained within the input
*     array, then the sections of the output array which fall outside the
*     input arrays will be filled with bad values.

*  Copyright:
*     Copyright (C) 1999, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     30-JUN-1999 (DSB):
*        Original version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDIMS
      INTEGER SLBND( NDIMS )
      INTEGER SUBND( NDIMS )
      DOUBLE PRECISION DIN( * )
      DOUBLE PRECISION VIN( * )
      INTEGER VLBND( NDIMS )
      INTEGER VUBND( NDIMS )

*  Arguments Returned:
      DOUBLE PRECISION OUT( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER EL                 ! The number of elements in OUT
      INTEGER IPW                ! Pointer to work array
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the N-dimensional section from the input data array to the output.
      CALL KPG1_CPNDD( NDIMS, SLBND, SUBND, DIN, VLBND, VUBND, OUT,
     :                   EL, STATUS )

*  Allocate work array to hold a section copied from the input variance
*  array.
      CALL PSX_CALLOC( EL, '_DOUBLE', IPW, STATUS )

*  Copy the N-dimensional section from the input variance array to the
*  work array.
      CALL KPG1_CPNDD( NDIMS, SLBND, SUBND, VIN, VLBND, VUBND,
     :                   %VAL( CNF_PVAL( IPW ) ), EL, STATUS )

*  Add Gaussian noise to the returned data section.
      CALL KPG1_NOISD( .TRUE., EL, %VAL( CNF_PVAL( IPW ) ),
     :                 OUT, STATUS )

*  Free the work array.
      CALL PSX_FREE( IPW, STATUS )

      END
