      SUBROUTINE ARD1_POI2( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR, PAR,
     :                     IWCS, B, LBEXTB, UBEXTB, LBINTB, UBINTB,
     :                     STATUS )
*+
*  Name:
*     ARD1_POI2

*  Purpose:
*     Initialise an array to hold a set of points, with non-linear user
*     coords.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_POI2( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR, PAR, IWCS, B,
*                    LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )

*  Description:
*     Interior values (equal to RINDEX) are assigned to the points
*     specified by the supplied parameters. The supplied parameters are
*     the user co-ordinates of the interior points.

*  Arguments:
*     RINDEX = INTEGER (Given)
*        The value to represent interior points.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the B array.
*     LBND( NDIM ) = INTEGER (Given)
*        The lower pixel index bounds of the B array.
*     UBND( NDIM ) = INTEGER (Given)
*        The upper pixel index bounds of the B array.
*     MSKSIZ = INTEGER (Given)
*        The total number of elements in the B array.
*     NPAR = INTEGER (Given)
*        The size of the PAR array.
*     PAR( NPAR ) = DOUBLE PRECISION (Given)
*        A list of user co-ordinates, in groups of NDIM.
*     IWCS = INTEGER (Given)
*        An identifer for an AST FrameSet. The Base Frame should be
*        PIXEL coordinates within the B array. The Current Frame should
*        be user coordinates.
*     B( MSKSIZ ) = INTEGER (Given and Returned)
*        The array.
*     LBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        exterior points in B. A value of VAL__MAXI for element 1 is
*        returned to indicate an "infinite" box. Other elements should
*        be ignored.
*     UBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        exterior points in B. The returned values should be ignored
*        since the box is "infinite".
*     LBINTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        interior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBINTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        interior points in B.
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
*     1-MAR-1994 (DSB):
*        Original version.
*     15-JUN-2001 (DSB):
*        Modified to be called from ARD1_KDRAW (includes adding arg IWCS).
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'AST_PAR'          ! AST_ constants and functions
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants

*  Arguments Given:
      INTEGER RINDEX
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER MSKSIZ
      INTEGER NPAR
      DOUBLE PRECISION PAR( NPAR )
      INTEGER IWCS

*  Arguments Given and Returned:
      INTEGER B( MSKSIZ )
      INTEGER LBEXTB( NDIM )
      INTEGER UBEXTB( NDIM )
      INTEGER LBINTB( NDIM )
      INTEGER UBINTB( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION           ! Pixel position
     :        GR( ARD__MXDIM )

      INTEGER
     :        I,                 ! Loop count
     :        IND,               ! Current Cartesian co-ord. value
     :        IPAR,              ! Index of current Cartesian co-ord.
     :        MAP,               ! AST identifier for user -> pixel Mapping
     :        NWCS,              ! No. of user axes
     :        VA                 ! Vector address eqv to Cartesian pnt.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the bounds of the interior bounding box.
      DO I = 1, NDIM
         LBINTB( I ) = VAL__MAXI
         UBINTB( I ) = VAL__MINI
      END DO

*  Get the user -> pixel Mapping from the FrameSet, and the number of
*  user axes.
      MAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, AST__CURRENT,
     :                                    AST__BASE, STATUS ),
     :                    STATUS )
      NWCS = AST_GETI( MAP, 'Nin', STATUS )

*  Loop round each set of co-ordinates.
      IPAR = 1
      DO WHILE( IPAR .LE. NPAR - NDIM + 1 )

*  Transform this position from user to pixel coords.
         CALL AST_TRANN( MAP, 1, NWCS, 1, PAR( IPAR ), .TRUE., NDIM,
     :                   1, GR, STATUS )

*  Find the vector address corresponding to these pixel co-ordinates.
         CALL ARD1_GTOV( NDIM, LBND, UBND, GR, VA, STATUS )

*  If the point is within the array, set it to an interior value
         IF( VA .GT. 0 ) THEN
            B( VA ) = RINDEX

*  Update the interior bounding box, and increment the pointer to the
*  next set of co-ordinates.
            DO I = 1, NDIM
               IND = NINT( GR( I ) )

               IF( IND .LT. LBINTB( I ) ) LBINTB( I ) = IND
               IF( IND .GT. UBINTB( I ) ) UBINTB( I ) = IND

            END DO

         END IF

*  Increment the pointer to the next set of co-ordinates.
         IPAR = IPAR + NDIM

      END DO

*  Annull the Mapping.
      CALL AST_ANNUL( MAP, STATUS )

*  Report an error if the number of supplied parameters is not a
*  multiple of NDIM.
      IF( IPAR .LT. NPAR .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETI( 'NP', NPAR )
         CALL MSG_SETI( 'ND', NDIM )
         CALL ERR_REP( 'ARD1_POI2_ERR1', 'Wrong no. of parameters '//
     :                 '(^NP) supplied for ^ND D region in ARD1_POI2 '//
     :                 '(programming error).', STATUS )
      END IF

      END
