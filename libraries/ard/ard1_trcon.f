      SUBROUTINE ARD1_TRCON(NDIM, C1, C2, D, STATUS )
*+
*  Name:
*     ARD1_TRCON

*  Purpose:
*     Concatenate two N-dimensional linear mappings

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_TRCON(NDIM, C1, C2, D, STATUS )

*  Description:
*     The co-efficients of a linear mapping are returned in D which
*     represents the total mapping obtained by applying the mapping
*     given by C1 followed by the mapping given by C2.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of dimensions.
*     C1( 0:NDIM, NDIM ) = REAL (Given)
*        The co-efficients of the first linear mapping. Column zero
*        holds the offset vector.
*     C2( 0:NDIM, NDIM ) = REAL (Given)
*        The co-efficients of the second linear mapping.
*     D( 0:NDIM, NDIM ) = REAL (Returned)
*        The co-efficients of the resulting linear mapping.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     28-APR-1994 (DSB):
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
      INTEGER NDIM
      REAL C1( 0:NDIM, NDIM )
      REAL C2( 0:NDIM, NDIM )

*  Arguments Returned:
      REAL D( 0:NDIM, NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I,J,K              ! Loop counts.

      REAL
     :  MAT,                     ! An inner product
     :  OFF                      ! An offset

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round each row of the output matrix
      DO J = 1, NDIM

*  Initialise the offset term.
         OFF = C2( 0, J )

*  Loop round each column of the output matrix (excluding column zero
*  which holds the offset vector and is dealt with separately).
         DO I = 1, NDIM

*  Increment the offset term.
            OFF = OFF + C1( 0, I )*C2( I, J )

*  Initialise the matrix inner product term.
            MAT = 0.0

*  Loop round each term in the inner product.
            DO K = 1, NDIM

*  Update the inner product.
               MAT = MAT + C1( I, K )*C2( K, J )

            END DO

*  Store this inner product in the output matrix.
            D( I, J ) = MAT

         END DO

*  Store this offset term in the output matrix.
         D( 0, J ) = OFF

      END DO

      END
