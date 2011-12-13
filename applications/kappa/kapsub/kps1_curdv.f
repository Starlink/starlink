      SUBROUTINE KPS1_CURDV( MAP, XC, YC, NDIM, LBND, UBND, COMP, IN,
     :                       IAT, LINE, STATUS )
*+
*  Name:
*     KPS1_CURDV

*  Purpose:
*     Format a data value for CURSOR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CURDV( MAP, XC, YC, NDIM, LBND, UBND, COMP, IN,
*                      IAT, LINE, STATUS )

*  Description:
*     This routine formats the value of an array element for application
*     CURSOR. The position is supplied in GRAPHICS co-ordinates. It is mapped
*     into PIXEL Frame before being used.

*  Arguments:
*     MAP = INTEGER (Given)
*        A pointer to the Mapping from the supplied graphics position
*        (XC, YC) to the PIXEL Frame.
*     XC = REAL (Given)
*        The X GRAPHICS co-ordinate.
*     YC = REAL (Given)
*        The Y GRAPHICS co-ordinate.
*     NDIM = INTEGER (Given)
*        The number of pixel axes.
*     LBND( NDIM ) = INTEGER (Given)
*        The lower pixel index bounds of IN.
*     UBND( NDIM ) = INTEGER (Given)
*        The upper pixel index bounds of IN.
*     COMP = CHARACTER * ( * ) (Given)
*        The NDF array component being displayed.
*     IN( * ) = DOUBLE PRECISION (Given)
*        The data array.
*     IAT = INTEGER (Given and Returned)
*        The number of characters in LINE.
*     LINE = CHARACTER * ( * ) (Given and Returned)
*        The text.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JAN-2006 (DSB):
*        Original version.
*     1-FEB-2006 (DSB):
*        Correct conversion of pixels coords to pixel indices.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER MAP
      REAL XC
      REAL YC
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      CHARACTER COMP*(*)
      DOUBLE PRECISION IN( * )

*  Arguments Given and Returned:
      INTEGER IAT
      CHARACTER LINE*(*)

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      DOUBLE PRECISION GXY( 2 ) ! Graphics position
      DOUBLE PRECISION CXY( 2 ) ! PIXEL position
      INTEGER I                 ! Loop count
      INTEGER IV                ! Vectorised index into IN
      INTEGER IX                ! Pixel index
      INTEGER STEP              ! Step between adjacent pixels on current axis
      LOGICAL OUT               ! Is pixel outside bounds of IN?
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Transform the cursor position from GRAPHICS co-ordinates into the
*  PIXEL Frame.
      GXY( 1 ) = DBLE( XC )
      GXY( 2 ) = DBLE( YC )
      CALL AST_TRANN( MAP, 1, 2, 1, GXY, .TRUE., NDIM, 1, CXY, STATUS )

*  Get the corresponding pixel indices, and check they are within the
*  bounds of the array. Also find the the corresponding 1D index into
*  the vectorised data array.
      IV = 1
      STEP = 1
      OUT = .FALSE.

      DO I = 1, NDIM
         IX = INT( CXY( I ) )
         IF( CXY( I ) .GE. 0 ) IX = IX + 1

         IF( IX .LT. LBND( I ) .OR. IX .GT. UBND( I ) ) THEN
            OUT = .TRUE.
         ELSE
             IV = IV + ( IX - LBND( I ) )*STEP
         END IF
         STEP = STEP * ( UBND( I ) - LBND( I ) + 1 )
      END DO

*  If the position is outside the array bounds, say so.
      IF( OUT ) THEN
         CALL CHR_APPND( '(position is outside the NDF bounds)',
     :                   LINE, IAT )

*  Otherwise, format the data value, preceeding the value with the
*  component name.
      ELSE
         CALL CHR_APPND( COMP, LINE, IAT )
         CALL CHR_APPND( ' value =', LINE, IAT )
         IAT = IAT + 1
         IF( IN( IV ) .NE. VAL__BADD ) THEN
            CALL CHR_PUTD( IN( IV ), LINE, IAT )
         ELSE
            CALL CHR_APPND( '<bad>', LINE, IAT )
         END IF
      END IF

      END
