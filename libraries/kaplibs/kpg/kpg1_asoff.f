      SUBROUTINE KPG1_ASOFF( FRAME, DIM, NAX, POS, I1, I2, GEO, DIS,
     :                       OFFPOS, STATUS )
*+
*  Name:
*     KPG1_ASOFF

*  Purpose:
*     Finds a position offset by a given distance from one position
*     towards another position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASOFF( FRAME, DIM, NAX, POS, I1, I2, GEO, DIS, OFFPOS,
*                      STATUS )

*  Description:
*     This routine returns the co-ordinates of a position which is a given
*     distance along a curve joining one position to another position.
*     The curve can be either be the geodesic or Euclidean curve joining
*     the two points.

*  Arguments:
*     FRAME = INTEGER (Given)
*        An AST pointer to the Frame. Only accessed if GEO is .TRUE.
*     DIM = INTEGER (Given)
*        The size of the first dimension of the POS array.
*     NAX = INTEGER (Given)
*        The number of axes in the Frame.
*     POS( DIM, NAX ) = DOUBLE PRECISION (Given)
*        An array holding the co-ordinates at DIM positions within the
*        supplied Frame.
*     I1 = INTEGER (Given)
*        The index of the first position, in the range 1 to DIM.
*     I2 = INTEGER (Given)
*        The index of the second position, in the range 1 to DIM.
*     GEO = LOGICAL (Given)
*        Is the geodesic distance required?
*     DIS = DOUBLE PRECISION (GIVEN)
*        The distance to move away from position I1 towards position I2.
*     OFFPOS( NAX ) = DOUBLE PRECISION (Returned)
*        The returned position.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Arguments Given:
      INTEGER FRAME
      INTEGER DIM
      INTEGER NAX
      DOUBLE PRECISION POS( DIM, NAX )
      INTEGER I1
      INTEGER I2
      LOGICAL GEO
      DOUBLE PRECISION DIS

*  Arguments returned:
      DOUBLE PRECISION OFFPOS( NAX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION FRAC      ! Fraction of distance between two positions
      DOUBLE PRECISION L2        ! The sum of squared axis increments
      DOUBLE PRECISION P1( NDF__MXDIM )! The first position
      DOUBLE PRECISION P2( NDF__MXDIM )! The second position
      INTEGER J                  ! Axis index
      LOGICAL GOOD               ! Both positions good?
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the first position.
      DO J = 1, NAX
         P1( J ) = POS( I1, J )
      END DO

*  Store the second position.
      DO J = 1, NAX
         P2( J ) = POS( I2, J )
      END DO

*  If the geodesic distance is required, use AST_OFFSET.
      IF( GEO ) THEN
         CALL AST_OFFSET( FRAME, P1, P2, DIS, OFFPOS, STATUS )

*  Otherwise, just use the Euclidean offset.
      ELSE

*  Find the sum of the squared axis increments between the 2 positions.
         L2 = 0.0D0
         GOOD = .FALSE.

         DO J = 1, NAX
            IF( P2( J ) .NE. AST__BAD .AND. P1( J ) .NE. AST__BAD ) THEN
               L2 = L2 + ( P2( J ) - P1( J ) )**2
               GOOD = .TRUE.
            END IF
         END DO

*  If there were no good axis values, return bad positions.
         IF( .NOT. GOOD ) THEN
            DO J = 1, NAX
               OFFPOS( J ) = AST__BAD
            END DO

*  Otherwise, find the distance to offset as a fraction of the distance from
*  P1 to P2.
         ELSE IF( L2 .GT. 0.0 ) THEN
            FRAC = DIS/SQRT( L2 )

*  Find the required position.
            DO J = 1, NAX
               IF( P2( J ) .NE. AST__BAD .AND.
     :             P1( J ) .NE. AST__BAD ) THEN
                  OFFPOS( J ) = FRAC*( P2( J ) - P1( J ) ) + P1( J )
               ELSE
                  OFFPOS( J ) = AST__BAD
               END IF
            END DO

*  If the distance between the positions is zero, return the first
*  position.
         ELSE
            DO J = 1, NAX
               OFFPOS( J ) = P1( J )
            END DO

         END IF

      END IF

      END
