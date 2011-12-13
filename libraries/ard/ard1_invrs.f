      SUBROUTINE ARD1_INVRS( NDIM, C, D, STATUS )
*+
*  Name:
*     ARD1_INVRS

*  Purpose:
*     Invert a linear mapping

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_INVRS( NDIM, C, D, STATUS )

*  Description:
*     The arrays representing linear mappings have NDIM "rows" and
*     NDIM+1 "columns". Column 0 is a vector giving the translational
*     offset to add into the transformed position. The remaining NDIM
*     columns give the multiplicative matrix relating input and output
*     positions. An SLALIB routine is used to invert this matrix, and
*     the inverted matrix is used to find the appropriate offset vector
*     for the returned array of co-efficients.

*  Arguments:
*     NDIM = INTEGER (Given)
*        No. of dimensions
*     C( 0:NDIM, NDIM ) = DOUBLE PRECISION (Given)
*        The supplied mapping.
*     D( 0:NDIM, NDIM ) = DOUBLE PRECISION (Given)
*        The inverse mapping.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     16-MAY-1994 (DSB):
*        Original version.
*     18-JUL-2001 (DSB):
*        Modified for ARD version 2.0.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants
      INCLUDE 'ARD_CONST'        ! ARD_ private constants

*  Arguments Given:
      INTEGER NDIM
      DOUBLE PRECISION C( 0:NDIM, NDIM )

*  Arguments Returned:
      DOUBLE PRECISION D( 0:NDIM, NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  EL,                      ! Vector index into matrix array
     :  I,J,                     ! Row and column counters
     :  IW( ARD__MXDIM ),        ! Work space
     :  SING                     ! Singularity flag

      DOUBLE PRECISION
     :  DET,                     ! Determinant
     :  MAT( ARD__MXDIM*(1+ARD__MXDIM) ),! The vectorised matrix
     :  RES( ARD__MXDIM ),       ! The results and solution vectors
     :  S                        ! Inner product

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the supplied matrix (excluding the leading vector which gives
*  the translational offset) into a local array. Set up a vector to be
*  used as the results vector when inverting the matrix.
      EL = 1

      DO J = 1, NDIM
         RES( J ) = 1.0D0
         DO I = 1, NDIM
            MAT( EL ) = C( I, J )
            EL = EL + 1
         END DO
      END DO

*  Invert the supplied matrix.
      CALL SLA_DMAT( NDIM, MAT, RES, DET, SING, IW )

*  If the inverse could not be found, report an error and abort.
      IF( SING .NE. 0 ) THEN
         STATUS = ARD__INDET
         CALL ERR_REP( 'ARD1_INVRS_ERR1', 'Singular transformation'//
     :                 ' supplied within an ARD description.',
     :                 STATUS )
         GO TO 999
      END IF

*  Calculate the translational offset vector for the inverse
*  transformation and store it in the returned array of co-efficients.
*  Also store th inverse matrix in the retuned array.
      EL = 1
      DO J = 1, NDIM
         S = 0.0D0

         DO I = 1, NDIM
            S = S + MAT( EL )*C( 0, I )
            D( I, J ) = MAT( EL )
            EL = EL + 1
         END DO

         D( 0, J ) = -S

      END DO

 999  CONTINUE

      END
