      SUBROUTINE ARD1_SCALE( FRM, P, DIST, AXIS, SDIST, STATUS )
*+
*  Name:
*     ARD1_SCALE

*  Purpose:
*     Convert a distance given by the user into the equivalent distance
*     along a specified axis.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_SCALE( FRM, P, DIST, AXIS, SDIST, STATUS )

*  Description:
*     Distances supplied as arguments within keywords are usually given as
*     increments along axis 1 (unless the user coord Frame is a SkyFrame,
*     in which case they are given as increments along the latitude axis).
*     This routine converts a given increment to the equivalent increment
*     measured along a specified axis (which may not be the one along
*     which the increment was originaly specified). For instance, you can
*     use this routine to find the RA increment corresponding to a given
*     arc-distance, at a given position on the sky.

*  Arguments:
*     FRM = INTEGER (Given)
*        An AST pointer to a Frame describing user coordinates.
*     P( * ) = DOUBLE PRECISION (Given)
*        The position within FRM at which the increment is to be converted.
*     DIST = DOUBLE PRECISION (Given)
*        An increment along the axis used to measure distances within FRM
*        (as identified by ARD1_DSTAX).
*     AXIS = INTEGER (Given)
*        The index of the axis along which the equivalent increment is
*        required.
*     SDIST = DOUBLE PRECISION (Given)
*        The equivalent increment along axis AXIS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
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
*     24-AUG-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST_ constants
      INCLUDE 'ARD_CONST'        ! ARD_ private constants

*  Arguments Given:
      INTEGER FRM
      DOUBLE PRECISION P( * )
      DOUBLE PRECISION DIST
      INTEGER AXIS

*  Arguments Returned:
      DOUBLE PRECISION SDIST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION P2( ARD__MXDIM )! Arbitrary point along request axis
      DOUBLE PRECISION P3( ARD__MXDIM )! Required point along request axis
      INTEGER I                  ! Axis count
      INTEGER IAXIS              ! Axis corresponding to arc-distance
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the index of the axis along which distances in the keyword
*  argument list are supplied.
      CALL ARD1_DSTAX( FRM, IAXIS, STATUS )

*  If this is the same as the requested axis, just return the suppllied
*  increment unchanged.
      IF( IAXIS .EQ. AXIS ) THEN
         SDIST = DIST

*  Otherwise...
      ELSE

*  Set up the coordinates of a position which is offset away from the given
*  position along the requested axis, by a small amount.
         DO I = 1, AST_GETI( FRM, 'NAXES', STATUS )
            P2( I ) = P( I )
         END DO
         P2( AXIS ) = P2( AXIS ) + MAX( 1.0D-6, P2( AXIS )*0.01 )

*  Offset away from the supplied position, towards this second position by
*  the supplied arc-distance.
         CALL AST_OFFSET( FRM, P, P2, DIST, P3, STATUS )

*  Return the corresponding increment along the requested axis.
         SDIST = P3( AXIS ) - P( AXIS )

      END IF

      END
