      SUBROUTINE ARD1_ROW( RINDEX, NDIM, D, LBND, UBND, MSKSIZ,
     :                     NPAR, PAR, B, LBEXTB, UBEXTB, LBINTB,
     :                     UBINTB, STATUS )
*+
*  Name:
*     ARD1_ROW

*  Purpose:
*     Initialise an array to hold a ROW region.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_ROW( RINDEX, NDIM, D, LBND, UBND, MSKSIZ,
*                    NPAR, PAR, B, LBEXTB, UBEXTB, LBINTB,
*                    UBINTB, STATUS )

*  Description:
*     The array B is initialised by setting all values within the
*     supplied interior bounding box to the exterior value 0.
*     All points outside this box already hold exterior values.
*     Interior values are then assigned to the points specified by the
*     supplied parameters.

*  Arguments:
*     RINDEX = INTEGER (Given)
*        The value to use to represent interior points.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the B array.
*     D( * ) = DOUBLE PRECISION (Given)
*        The coefficients of the user->pixel mapping. There should be
*        NDIM*(NDIM+1) elements in the array. The mapping is:
*
*        P1 = D0 + D1*U1 + D2*U2 + ...  + Dn*Un
*        P2 = Dn+1 + Dn+2*U1 + Dn+3*U2 + ...  + D2n+1*Un
*        ...
*        Pn = ...
*     LBND( NDIM ) = INTEGER (Given)
*        The lower pixel index bounds of the B array.
*     UBND( NDIM ) = INTEGER (Given)
*        The upper pixel index bounds of the B array.
*     MSKSIZ = INTEGER (Given)
*        The total number of elements in the B array.
*     NPAR = INTEGER (Given)
*        The size of the PAR array.
*     PAR( NPAR ) = DOUBLE PRECISION (Given)
*        Region parameters, as supplied in the ARD expression.
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
*     23-MAR-1994 (DSB):
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
      DOUBLE PRECISION D( * )
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER MSKSIZ
      INTEGER NPAR
      DOUBLE PRECISION PAR( NPAR )

*  Arguments Given and Returned:
      INTEGER B( MSKSIZ )
      INTEGER LBEXTB( NDIM )
      INTEGER UBEXTB( NDIM )
      INTEGER LBINTB( NDIM )
      INTEGER UBINTB( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :        IROW               ! Loop count

      DOUBLE PRECISION
     :        LPAR( 4 ),         ! Parameters for an extended line
     :        MODUL,             ! Modulus of supplied vector.
     :        MXDIST,            ! Max. distance from any mask edge
     :        VX,                ! X component of unit direction vector
     :        VY,                ! Y component of unit direction vector
     :        X,                 ! X value at axis crossing
     :        Y                  ! Y value at axis crossing

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if the number of dimensions is not 2.
      IF( NDIM .NE. 2 ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETI( 'ND', NDIM )
         CALL ERR_REP( 'ARD1_ROW_ERR1', 'Wrong no. of dimensions '//
     :                 '(^ND) supplied in ARD1_ROW (programming '//
     :                 'error).', STATUS )
         GO TO 999
      END IF

*  Reset all pixels within the interior bounding box so that they
*  hold exterior values. The pixels outside the interior bounding box
*  already hold exterior values.
      CALL ARD1_BXSET( NDIM, LBND, UBND, MSKSIZ, 0, LBINTB,
     :                 UBINTB, B, STATUS )

*  Initialise the interior bounding box.
      LBINTB( 1 ) = VAL__MAXI
      LBINTB( 2 ) = VAL__MAXI
      UBINTB( 1 ) = VAL__MINI
      UBINTB( 2 ) = VAL__MINI

*  Store the components of a unit direction vector along the row.
      VX = D( 2 )
      VY = D( 5 )
      MODUL = SQRT( MAX( 0.0D0, VX**2 + VY**2 ) )

      IF( MODUL .GT. 0.0 ) THEN
         VX = VX / MODUL
         VY = VY / MODUL

      ELSE
         STATUS = ARD__INTER
         CALL ERR_REP( 'ARD1_ROW_ERR3', 'Null direction vector '//
     :                 'supplied to ARD1_ROW (programming error).',
     :                 STATUS )
         GO TO 999

      END IF

*  Loop round each of the rows.
      DO IROW = 1, NPAR

*  Find the pixel co-ordinates corresponding to position (0,PAR).
         X = D( 1 ) + D( 3 )*PAR( IROW )
         Y = D( 4 ) + D( 6 )*PAR( IROW )

*  Find the maximum distance of the given position from any of the
*  mask boundaries.
         MXDIST = MAX( ABS( X - DBLE( LBND( 1 ) ) + 1.0D0 ),
     :            MAX( ABS( X - DBLE( UBND( 1 ) ) ),
     :            MAX( ABS( Y - DBLE( LBND( 2 ) ) + 1.0D0),
     :                 ABS( Y - DBLE( UBND( 2 ) ) ) ) ) )

*  Store the pixel co-ordinates of the ends of a line which is co-linear
*  with the current row and which extends beyond the boundaries of the
*  mask.
         LPAR( 1 ) = X + 4.0*MXDIST*VX
         LPAR( 2 ) = Y + 4.0*MXDIST*VY
         LPAR( 3 ) = X - 4.0*MXDIST*VX
         LPAR( 4 ) = Y - 4.0*MXDIST*VY

*  Call another routine to draw the line.
         CALL ARD1_LINFL( RINDEX, NDIM, LBND, UBND, MSKSIZ, 4, LPAR,
     :                    B, LBINTB, UBINTB, STATUS )

      END DO

*  If the interior bounding box is null, return the usual value
*  (VAL__MINI for LBINTB( 1 ) ).
      IF( LBINTB( 1 ) .GT. UBINTB( 1 ) .OR.
     :    LBINTB( 2 ) .GT. UBINTB( 2 ) ) LBINTB( 1 ) = VAL__MINI

*  Ensure the the exterior bounding box is returned "infinite".
      LBEXTB( 1 ) = VAL__MAXI

*  Jump to here if an error occurs.
 999  CONTINUE

      END
