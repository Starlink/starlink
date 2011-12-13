      SUBROUTINE CCD1_ALDIF( D1, N1, D2, N2, DIF, DIFMAX, DIFMIN,
     :                       STATUS )
*+
*  Name:
*     CCD1_ALDIF

*  Purpose:
*     Forms an array of all possible differences between two arrays.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_ALDIF( D1, N1, D2, N2, DIF, DIFMAX, DIFMIN, STATUS )

*  Description:
*     This routine performs all the possible intercomparisons between
*     two arrays of double precision data, forming the difference
*     between the first array and the second data data values. The
*     differences are returned together with the minimum and maximum
*     values.

*  Arguments:
*     D1( N1 ) = DOUBLE PRECISION (Given)
*        The first array of values.
*     N1 = INTEGER (Given)
*        Number of values in first array.
*     D2( N2 ) = DOUBLE PRECISION (Given)
*        The second array of values.
*     N2 = INTEGER (Given)
*        Number of values in second array.
*     DIF( * ) = DOUBLE PRECISION (Returned)
*        The differences in data values which result from the
*        intercomparison of D1 and D2. The array used to perform the
*        intercomparison is
*
*            index = 0
*            do i = 1, n1
*               do j = 1, n2
*                  index = index + 1
*                  dif( index ) = d1( i ) - d2( j )
*               end do
*            end do
*
*        so this mechanism should be used for referencing values.
*     DIFMAX = DOUBLE PRECISION (Returned)
*        Maximum difference.
*     DIFMIN = DOUBLE PRECISION (Returned)
*        Minimum difference.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-JAN-1993 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      INTEGER N1
      DOUBLE PRECISION D1( N1 )
      INTEGER N2
      DOUBLE PRECISION D2( N2 )

*  Arguments Returned:
      DOUBLE PRECISION DIF( * )
      DOUBLE PRECISION DIFMAX
      DOUBLE PRECISION DIFMIN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, J, N            ! Loop variables
      DOUBLE PRECISION VAL       ! Dummy variable for difference value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initial output array counter and maximum and minimum differencences.
      N = 0
      DIFMAX = VAL__MIND
      DIFMIN = VAL__MAXD

*  Now perform the intercomparison creating the differences.
      DO 1 I = 1, N1
         DO 2 J = 1, N2
            N = N + 1
            VAL = D1( I ) - D2( J )
            DIF( N ) = VAL
            DIFMAX = MAX( DIFMAX, VAL )
            DIFMIN = MIN( DIFMIN, VAL )
 2       CONTINUE
 1    CONTINUE

      END
* $Id$
