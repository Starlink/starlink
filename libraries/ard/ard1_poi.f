      SUBROUTINE ARD1_POI( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR, D,
     :                     PAR, B, LBEXTB, UBEXTB, LBINTB, UBINTB,
     :                     STATUS )
*+
*  Name:
*     ARD1_POI

*  Purpose:
*     Initialise an array to hold a set of points.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_POI( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR, D, PAR, B,
*                    LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )

*  Description:
*     The array B is initialised by setting all values within the
*     supplied interior bounding box to the exterior value 0.
*     All points outside this box already hold exterior values.
*     Interior values (equal to RINDEX) are then assigned to the points
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
*     D( * ) = DOUBLE PRECISION (Given)
*        The coefficients of the user->pixel mapping. There should be
*        NDIM*(NDIM+1) elements in the array. The mapping is:
*
*        P1 = D0 + D1*U1 + D2*U2 + ...  + Dn*Un
*        P2 = Dn+1 + Dn+2*U1 + Dn+3*U2 + ...  + D2n+1*Un
*        ...
*        Pn = ...
*     PAR( NPAR ) = DOUBLE PRECISION (Given and Returned)
*        A list of user co-ordinates, in groups of NDIM. On exit they
*        are converted to pixel indices.
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
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants

*  Arguments Given:
      INTEGER RINDEX
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER MSKSIZ
      INTEGER NPAR
      DOUBLE PRECISION D( * )

*  Arguments Given and Returned:
      DOUBLE PRECISION PAR( NPAR )
      INTEGER B( MSKSIZ )
      INTEGER LBEXTB( NDIM )
      INTEGER UBEXTB( NDIM )
      INTEGER LBINTB( NDIM )
      INTEGER UBINTB( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER ARD1_CEIL          ! Smallest integer <= a given floating value

*  Local Variables:
      INTEGER
     :        I,                 ! Loop count
     :        IND,               ! Current Cartesian co-ord. value
     :        IPAR,              ! Index of current Cartesian co-ord.
     :        VA                 ! Vector address eqv to Cartesian pnt.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Reset all pixels within the interior bounding box so that they
*  hold exterior values. The pixels outside the interior bounding box
*  already hold exterior values.
      CALL ARD1_BXSET( NDIM, LBND, UBND, MSKSIZ, 0, LBINTB,
     :                 UBINTB, B, STATUS )

*  Initialise the bounds of the interior bounding box.
      DO I = 1, NDIM
         LBINTB( I ) = VAL__MAXI
         UBINTB( I ) = VAL__MINI
      END DO

*  Convert the supplied user coords to pixel coords.
      CALL ARD1_LTRAN( NDIM, D, NPAR/NDIM, PAR, PAR, STATUS )

*  Convert the pixel coords to pixel indices.
      DO IPAR = 1, NPAR
         PAR( IPAR ) = DBLE( ARD1_CEIL( PAR( IPAR ) ) )
      END DO

*  Loop round each set of co-ordinates.
      IPAR = 1
      DO WHILE( IPAR .LE. NPAR - NDIM + 1 )

*  Find the vector address corresponding to the supplied Cartesian
*  co-ordinates.
         CALL ARD1_CTOV( NDIM, LBND, UBND, PAR( IPAR ), VA, STATUS )

*  If the point is within the array, set it to an interior value
         IF( VA .GT. 0 ) THEN
            B( VA ) = RINDEX

*  Update the interior bounding box, and increment the pointer to the
*  next set of co-ordinates.
            DO I = 1, NDIM
               IND = PAR( IPAR )
               IPAR = IPAR + 1

               IF( IND .LT. LBINTB( I ) ) LBINTB( I ) = IND
               IF( IND .GT. UBINTB( I ) ) UBINTB( I ) = IND

            END DO

*  Otherwise, just increment the pointer to the next set of
*  co-ordinates.
         ELSE
            IPAR = IPAR + NDIM

         END IF

      END DO

*  Report an error if the number of supplied parameters is not a
*  multiple of NDIM.
      IF( IPAR .LT. NPAR ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETI( 'NP', NPAR )
         CALL MSG_SETI( 'ND', NDIM )
         CALL ERR_REP( 'ARD1_POI_ERR1', 'Wrong no. of parameters '//
     :                 '(^NP) supplied for ^ND D region in ARD1_POI '//
     :                 '(programming error).', STATUS )

*  Otherwise...
      ELSE

*  Ensure the the exterior bounding box is returned "infinite".
         LBEXTB( 1 ) = VAL__MAXI

*  Indicate if the interior bounding box is null.
         IF( LBINTB( 1 ) .EQ. VAL__MAXI ) LBINTB( 1 ) = VAL__MINI

      END IF

      END
