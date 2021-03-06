      SUBROUTINE ARD1_ORBX( NDIM, LBND1, UBND1, LBND2, UBND2, LB, UB,
     :                      STATUS )
*+
*  Name:
*     ARD1_ORBX

*  Purpose:
*     Find the union of two boxes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_ORBX( NDIM, LBND1, UBND1, LBND2, UBND2, LB, UB, STATUS )

*  Description:
*     The union region of the two supplied boxes is found.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The no. of dimensions.
*     LBND1( NDIM ) = INTEGER (Given)
*        The lower bounds of the first box. A value of VAL__MAXI for the
*        first element indicates an infinite sized box, and a value of
*        VAL__MINI indicates a zero sized box.
*     UBND1( NDIM ) = INTEGER (Given)
*        The upper bounds of the first box.
*     LBND2( NDIM ) = INTEGER (Given)
*        The lower bounds of the second box. A value of VAL__MAXI for
*        the first element indicates an infinite sized box, and a value
*        of VAL__MINI indicates a zero sized box.
*     UBND2( NDIM ) = INTEGER (Given)
*        The upper bounds of the first box.
*     LB( NDIM ) = INTEGER (Returned)
*        The lower bounds of the union of the two supplied boxes.
*     UB( NDIM ) = INTEGER (Returned)
*        The upper bounds of the union of the two supplied boxes.
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
*     25-FEB-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      INTEGER NDIM
      INTEGER LBND1( NDIM )
      INTEGER UBND1( NDIM )
      INTEGER LBND2( NDIM )
      INTEGER UBND2( NDIM )

*  Arguments Returned:
      INTEGER LB( NDIM )
      INTEGER UB( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Dimension counter
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If either of the supplied boxes has infinite extent then the result
*  has infinite extent.
      IF( LBND1( 1 ) .EQ. VAL__MAXI .OR.
     :    LBND2( 1 ) .EQ. VAL__MAXI ) THEN
         LB( 1 ) = VAL__MAXI

*  If box 2 has zero extent, then the result is just equal to box 1.
      ELSE IF( LBND2( 1 ) .EQ. VAL__MINI ) THEN

         DO I = 1, NDIM
            LB( I ) = LBND1( I )
            UB( I ) = UBND1( I )
         END DO

*  If box 1 has zero extent, then the result is just equal to box 2.
      ELSE IF( LBND1( 1 ) .EQ. VAL__MINI ) THEN

         DO I = 1, NDIM
            LB( I ) = LBND2( I )
            UB( I ) = UBND2( I )
         END DO

*  If neither box is of zero or infinite extent...
      ELSE

*  Loop round each dimension.
         DO I = 1, NDIM

*  Find the bounds of the union range on the current axis.
            LB( I ) = MIN( LBND1( I ), LBND2( I ) )
            UB( I ) = MAX( UBND1( I ), UBND2( I ) )

         END DO

      END IF

      END
