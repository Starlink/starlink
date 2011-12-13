      SUBROUTINE ARD1_BXFND( NDIM, LBND, UBND, NPAR, D, PAR, LBINTB,
     :                       UBINTB, STATUS )
*+
*  Name:
*     ARD1_BXFND

*  Purpose:
*     Find the pixel index bounds of a n-D box with sides parallel to
*     the user axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_BXFND( NDIM, LBND, UBND, NPAR, D, PAR, LBINTB, UBINTB,
*                      STATUS )

*  Description:
*     The user co-ordinates of each corner of the box are found. These
*     are transformed into pixel indices and the supplied bounds of the
*     internal bounding box are updated to include all the corners.

*  Arguments:
*     NDIM = INTEGER (Given)
*        No. of dimensions.
*     LBND( NDIM ) = INTEGER (Given)
*        Lower bounds of mask.
*     UBND( NDIM ) = INTEGER (Given)
*        Upper bounds of mask.
*     NPAR = INTEGER (Given)
*        No. of values in PAR.
*     D( * ) = DOUBLE PRECISION (Given)
*        The coefficients of the user->pixel mapping. There should be
*        NDIM*(NDIM+1) elements in the array. The mapping is:
*
*        P1 = D0 + D1*U1 + D2*U2 + ...  + Dn*Un
*        P2 = Dn+1 + Dn+2*U1 + Dn+3*U2 + ...  + D2n+1*Un
*        ...
*        Pn = ...
*     PAR( NPAR ) = DOUBLE PRECISION (Given)
*        Parameters; user coords of box centre, followed by length of each
*        side of the box (in user coords).
*     LBINTB( NDIM ) = INTEGER (Returned)
*        The lower bounds of the internal bounding box.
*     UBINTB( NDIM ) = INTEGER (Returned)
*        The upper bounds of the internal bounding box.
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
*     30-MAR-1994 (DSB):
*        Original version.
*     26-JUN-2001 (DSB):
*        Modified for ARD version 2.0.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'ARD_CONST'        ! ARD private constants

*  Arguments Given:
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER NPAR
      DOUBLE PRECISION D( * )
      DOUBLE PRECISION PAR( NPAR )

*  Arguments Returned:
      INTEGER LBINTB( NDIM )
      INTEGER UBINTB( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :     I,                    ! Dimension counter
     :     IC,                   ! Corner counter
     :     PINDEX                ! Pixel index value

      LOGICAL
     :     UPPER( ARD__MXDIM ),  ! At upper bound?
     :     CARRY                 ! Carry forward?

      DOUBLE PRECISION
     :     PIXCO( ARD__MXDIM ),  ! Pixel coordinates for current corner
     :     USERCO( ARD__MXDIM )  ! User coordinates for current corner

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the interior bounding box.
      DO I = 1, NDIM
         LBINTB( I ) = VAL__MAXI
         UBINTB( I ) = VAL__MINI
      END DO

*  Initialise a set of flags (one for each axis) indicating if the
*  corresponding co-ordinate is at its upper or lower bound. Set all
*  axes to their lower bounds.
      DO I = 1, NDIM
         UPPER( I ) = .FALSE.
         USERCO( I ) = PAR( I ) - 0.5*PAR( NDIM + I )
      END DO

*  Loop round each of the corners.
      DO IC = 1, 2**NDIM

*  Transform the current user position to pixel coordinates.
         CALL ARD1_LTRAN( NDIM, D, 1, USERCO, PIXCO, STATUS )

*  Convert the pixel co-ordinates to pixel indices and update the upper
*  and lower bounds of the internal bounding box.
         DO I = 1, NDIM
            PINDEX = DBLE( INT( PIXCO( I ) ) )
            IF( PINDEX .LT. PIXCO( I ) ) PINDEX = PINDEX + 1

            LBINTB( I ) = MIN( LBINTB( I ), PINDEX )
            UBINTB( I ) = MAX( UBINTB( I ), PINDEX )

         END DO

*  Change the flags to move on to another corner. Each flag is treated
*  as if it was one bit in a binary integer value (.TRUE. = 1,
*  .FALSE. = 0). This integer value is incremented by one on each pass
*  through the following code. This results in all possible combinations
*  of upper and lower bounds being used by the time IC=2**NDIM is
*  reached.
         I = 1
         CARRY = .TRUE.
         DO WHILE( CARRY .AND. I .LE. NDIM )

*  If this "bit" is set, clear it, and store the lower bound. Go on to
*  add the carry onto the next bit.
            IF( UPPER( I ) ) THEN
               UPPER( I ) = .FALSE.
               USERCO( I ) = PAR( I ) - 0.5*PAR( NDIM + I )
               I = I + 1

*  If this "bit" is clear, set it, and store the upper bound. Indicate
*  that no carry is required.
            ELSE
               UPPER( I ) = .TRUE.
               USERCO( I ) = PAR( I ) + 0.5*PAR( NDIM + I )
               CARRY = .FALSE.

            END IF

         END DO

      END DO

*  Ensure that the returned bounding box does not exceed the bounds of
*  the mask.
      DO I = 1, NDIM
         LBINTB( I ) = MAX( LBINTB( I ), LBND( I ) )
         UBINTB( I ) = MIN( UBINTB( I ), UBND( I ) )

*  If the lower bound is higher than the upper bound, return with a
*  null box
         IF( LBINTB( I ) .GT. UBINTB( I ) ) THEN
            LBINTB( 1 ) = VAL__MINI
            GO TO 999
         END IF

      END DO

 999  CONTINUE

      END
