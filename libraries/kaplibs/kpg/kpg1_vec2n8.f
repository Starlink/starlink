      SUBROUTINE KPG1_VEC2N8( NVEC, VEC, NDIM, LBND, UBND, IDIM,
     :                        STATUS )
*+
*  Name:
*     KPG1_VEC2N8

*  Purpose:
*     Converts vectorised array indices into n-dimensional form.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_VEC2N8( NVEC, VEC, NDIM, LBND, UBND, IDIM, STATUS )

*  Description:
*     This routine converts pixel indices which refer to a pixel in a
*     vectorised array into sets of indices which identify the same
*     pixel when the array is regarded as n-dimensional, with specified
*     lower and upper pixel-index bounds for each dimension.

*  Arguments:
*     NVEC = INTEGER (Given)
*        Number of vectorised array indices to convert.
*     VEC( NVEC ) = INTEGER*8 (Given)
*        Array of vectorised pixel indices to be converted.
*     NDIM = INTEGER (Given)
*        Number of array dimensions.
*     LBND( NDIM ) = INTEGER*8 (Given)
*        Lower pixel-index bounds for each array dimension.
*     UBND( NDIM ) = INTEGER*8 (Given)
*        Upper pixel-index bounds for each array dimension.
*     IDIM( NDIM, NVEC ) = INTEGER*8 (Returned)
*        Returns a set of NDIM pixel-indices for each vectorised index
*        supplied.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The maximum number of dimensions which can be handled by this
*     routine is equal to the symbolic constant NDF__MXDIM.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     18-MAR-1991 (RFWS):
*        Original version.
*     5-DEC-2019 (DSB):
*        Support huge files.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Arguments Given:
      INTEGER NVEC
      INTEGER*8 VEC( NVEC )
      INTEGER NDIM
      INTEGER*8 LBND( NDIM )
      INTEGER*8 UBND( NDIM )

*  Arguments Returned:
      INTEGER*8 IDIM( NDIM, NVEC )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER*8 IVEC             ! Variable for address calculation
      INTEGER J                  ! Loop counter for input values
      INTEGER*8 STRIDE( NDF__MXDIM ) ! Stride of each dimension

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop to find the stride for each dimension of the n-dimensional
*  array.
      STRIDE( 1 ) = 1
      DO 1 I = 1, NDIM - 1
         STRIDE( I + 1 ) = STRIDE( I ) * ( UBND( I ) - LBND( I ) + 1 )
 1    CONTINUE

*  Loop through all the vectorised indices to be converted.
      DO 3 J = 1, NVEC

*  Calculate the zero-based index in each dimension, updating IVEC as
*  the calculation proceeds.
         IVEC = VEC( J ) - 1
         DO 2 I = NDIM, 1, -1
            IDIM( I, J ) = IVEC / STRIDE( I )
            IVEC = IVEC - IDIM( I, J ) * STRIDE( I )

*  Add the lower bound of the n-dimensional array to each result.
            IDIM( I, J ) = IDIM ( I, J ) + LBND( I )
 2       CONTINUE
 3    CONTINUE

      END
