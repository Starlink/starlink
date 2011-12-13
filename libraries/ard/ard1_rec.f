      SUBROUTINE ARD1_REC( RINDEX, NDIM, FRM, LBND, UBND, MSKSIZ, NPAR,
     :                     D, PAR, B, LBEXTB, UBEXTB, LBINTB, UBINTB,
     :                     STATUS )
*+
*  Name:
*     ARD1_REC

*  Purpose:
*     Initialise an array to hold a RECT region.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_REC( RINDEX, NDIM, FRM, LBND, UBND, MSKSIZ, NPAR, D, PAR,
*                    B, LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )

*  Description:
*     The supplied parameters are modified so that they look the same
*     as those for a BOX region. ARD1_BOX is then called to load the
*     region.

*  Arguments:
*     RINDEX = INTEGER (Given)
*        The value to use to represent interior points.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the B array.
*     FRM = INTEGER (Given)
*        An AST pointer to the user coord Frame.
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
*     PAR( NPAR ) = DOUBLE PRECISION (Given)
*        A list of user co-ordinates, in groups of NDIM.
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
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants
      INCLUDE 'AST_PAR'          ! AST_ constants and functions

*  Arguments Given:
      INTEGER RINDEX
      INTEGER NDIM
      INTEGER FRM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER MSKSIZ
      INTEGER NPAR
      DOUBLE PRECISION D( * )
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
     :        I                  ! Loop count

      DOUBLE PRECISION
     :        HW,                 ! Axis half width
     :        LPAR( ARD__MXDIM*2 )! Local parameters

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if the number of supplied parameters is wrong.
      IF( NPAR .NE. 2*NDIM ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETI( 'NP', NPAR )
         CALL MSG_SETI( 'ND', NDIM )
         CALL ERR_REP( 'ARD1_REC_ERR1', 'Wrong no. of parameters '//
     :                 '(^NP) supplied for ^ND D region in ARD1_REC '//
     :                 '(programming error).', STATUS )
         GO TO 999
      END IF

*  Loop round each dimension.
      DO I = 1, NDIM

*  Find the half width of the box on this axis.
         HW = 0.5*AST_AXDISTANCE( FRM, I, PAR( I ), PAR( I + NDIM ),
     :                            STATUS )

*  Find the centre of the box on this axis.
         LPAR( I ) = AST_AXOFFSET( FRM, I, PAR( I ), HW, STATUS )

*  Store the full width of the box on this axis.
         LPAR( I + NDIM ) = 2*ABS( HW )

      END DO

*  The parameters are now in the same format as those for a BOX region.
*  Call the subroutine used to load a BOX region.
      CALL ARD1_BOX( RINDEX, NDIM, FRM, LBND, UBND, MSKSIZ, NPAR, D,
     :               LPAR, B, LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )

*  Jump here if an error occurs.
 999  CONTINUE

      END
