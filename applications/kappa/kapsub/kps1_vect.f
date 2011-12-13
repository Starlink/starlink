      SUBROUTINE KPS1_VECT( INK, X, Y, JUST, VECLEN, VECANG, AHSIZE,
     :                      X1, X2, Y1, Y2, STATUS )
*+
*  Name:
*     KPS1_VECT

*  Purpose:
*     Draws a vector.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_VECT( INK, X, Y, JUST, VECLEN, VECANG, AHSIZE, X1, X2,
*                     Y1, Y2, STATUS )

*  Description:
*     This routine draws a vector, either with or without an arrowhead
*     at the end.  The vector may be drawn centred on the supplied
*     position, or starting at the supplied position. A supplied bounding
*     box is extended to include the drawn vector.

*  Arguments:
*     INK = LOGICAL (Given)
*        If .FALSE., nothing is draw but the bounding box is still
*        returned.
*     X = REAL (Given)
*        The x component of the vectors reference position, in the
*        current PGPLOT window.
*     Y = REAL (Given)
*        The y component of the vectors reference position, in the
*         current PGPLOT window.
*     JUST = CHARACTER * ( * ) (Given)
*        Specifies the disposition of the vector with respect to the
*        reference position given by X and Y.  If it has the value
*        'CENTRE' (case sensitive), then the vector is positioned so
*        that its centre coincides with the position given by X and Y.
*        If it has the value 'START' the vector is positioned so that
*        it starts at the position given by X and Y.  If it has the
*        value 'END' the vector is positioned so that it ends at the
*        position given by X and Y.
*     VECLEN = REAL (Given)
*        The length of the vector in the current PGPLOT window.
*     VECANG = REAL (Given)
*        The angle from the y axis to the vector, in radians.  Positive
*        angles are in the same sense as rotation from the x axis to
*        the y axis.
*     AHSIZE = REAL (Given)
*        The length of each stroke of the arrowhead to be placed at the
*        end of the vector, in the world co-ordinate system of the
*        current PGPLOT window.  No arrowhead is drawn if a zero or negative
*        value is supplied.
*     X1 = REAL (Given and Returned)
*        The lowest X value in the bounding box. Modified on exit to include
*        the drawn vector.
*     X2 = REAL (Given and Returned)
*        The highest X value in the bounding box. Modified on exit to include
*        the drawn vector.
*     Y1 = REAL (Given and Returned)
*        The lowest Y value in the bounding box. Modified on exit to include
*        the drawn vector.
*     Y2 = REAL (Given and Returned)
*        The highest Y value in the bounding box. Modified on exit to include
*        the drawn vector.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

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
*     {enter_new_authors_here}

*  History:
*     4-OCT-1999 (DSB):
*        Original version.
*     13-AUG-2002 (DSB):
*        Added args INK, X1, X2, Y1 and Y2.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL INK
      REAL X
      REAL Y
      CHARACTER * ( * ) JUST
      REAL VECLEN
      REAL VECANG
      REAL AHSIZE

*  Arguments Given and Returned:
      REAL X1
      REAL X2
      REAL Y1
      REAL Y2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL COSA      ! COS( half of opening angle of arrow )
      PARAMETER ( COSA = 0.866 )

      REAL SINA      ! SIN( half of opening angle of arrow )
      PARAMETER ( SINA = 0.5 )

*  Local Variables:
      REAL AX0        ! X co-ordinate of arrow end point
      REAL AX1        ! X co-ordinate of arrow start point
      REAL AX2        ! X co-ordinate of first arrow head point
      REAL AX3        ! X co-ordinate of second arrow head point
      REAL AY0        ! Y co-ordinate of arrow end point
      REAL AY1        ! Y co-ordinate of arrow start point
      REAL AY2        ! Y co-ordinate of first arrow head point
      REAL AY3        ! Y co-ordinate of second arrow head point
      REAL COSANG     ! COS of vector orientation
      REAL SINANG     ! SIN of vector orientation
      REAL VECX       ! X component of vector
      REAL VECY       ! Y component of vector
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the COS and SIN of the vector orientation.
      SINANG = SIN( VECANG )
      COSANG = COS( VECANG )

*  Find the x and y components of the vector.
      VECX = -VECLEN * SINANG
      VECY = VECLEN * COSANG

*  Note the position of the point of the arrowhead.
      IF ( JUST .EQ. 'CENTRE' ) THEN
         AX0 = X + 0.5 * VECX
         AY0 = Y + 0.5 * VECY
      ELSE IF ( JUST .EQ. 'START' ) THEN
         AX0 = X + VECX
         AY0 = Y + VECY
      ELSE
         AX0 = X
         AY0 = Y
      END IF

*  Now draw the line which represents the vector.
      AX1 = AX0 - VECX
      AY1 = AY0 - VECY
      CALL PGMOVE( AX0, AY0 )
      IF( INK ) CALL PGDRAW( AX1, AY1 )

*  Update the bounding box.
      X1 = MIN( X1, MIN( AX0, AX1 ) )
      Y1 = MIN( Y1, MIN( AY0, AY1 ) )
      X2 = MAX( X2, MAX( AX0, AX1 ) )
      Y2 = MAX( Y2, MAX( AY0, AY1 ) )

*  Now draw an arrowhead if required.
      IF ( AHSIZE .NE. 0.0 )  THEN

*  Calculate the co-ordinates of the end of the first arrow stroke by
*  multiplying by the rotation matrix.
         AX2 = AX0 + AHSIZE * ( SINANG * COSA - COSANG * SINA )
         AY2 = AY0 - AHSIZE * ( SINANG * SINA + COSANG * COSA )

*  Draw the first arrow stroke.
         CALL PGMOVE( AX0, AY0 )
         IF( INK ) CALL PGDRAW( AX2, AY2 )

*  Calculate the co-ordinates of the end of the second arrow stroke by
*  multiplying by the rotation matrix.
         AX3 = AX0 + AHSIZE * ( SINANG * COSA + COSANG * SINA )
         AY3 = AY0 - AHSIZE * ( -SINANG * SINA + COSANG * COSA )

*  Draw the second arrow stroke.
         CALL PGMOVE( AX0, AY0 )
         IF( INK ) CALL PGDRAW( AX3, AY3 )

*  Update the bounding box.
         X1 = MIN( X1, MIN( AX2, AX3 ) )
         Y1 = MIN( Y1, MIN( AY2, AY3 ) )
         X2 = MAX( X2, MAX( AX2, AX3 ) )
         Y2 = MAX( Y2, MAX( AY2, AY3 ) )

      END IF


      END
