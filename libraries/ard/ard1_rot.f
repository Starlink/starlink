      SUBROUTINE ARD1_ROT( FRM, RINDEX, LBND1, UBND1, LBND2, UBND2,
     :                     NPAR, D, PAR, B, LBEXTB, UBEXTB, LBINTB,
     :                     UBINTB, STATUS )
*+
*  Name:
*     ARD1_ROT

*  Purpose:
*     Initialise an array to hold a ROTBOX region.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_ROT( FRM, RINDEX, LBND1, UBND1, LBND2, UBND2, NPAR, D, PAR,
*                    B, LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )

*  Description:
*     The supplied parameters are modified so that they look the same
*     as those for a POLYGON region. ARD1_POL is then called to load the
*     region.
*
*     A polygon vertex is put at the middle of each side in case we are
*     dealing with spherical coords, in which case a polygon edge greater
*     than 180 arc-degrees could cause problems.

*  Arguments:
*     FRM = INTEGER (Given)
*        A pointer to the user coord Frame.
*     RINDEX = INTEGER (Given)
*        The value to use to represent interior points.
*     LBND1 = INTEGER (Given)
*        The lower pixel index bounds of the B array on the first axis.
*     UBND1 = INTEGER (Given)
*        The upper pixel index bounds of the B array on the first axis.
*     LBND2 = INTEGER (Given)
*        The lower pixel index bounds of the B array on the second axis.
*     UBND2 = INTEGER (Given)
*        The upper pixel index bounds of the B array on the second axis.
*     NPAR = INTEGER (Given)
*        The size of the PAR array.
*     D( 6 ) = DOUBLE PRECISION (Given)
*        The coefficients of the user->pixel mapping. The mapping is:
*        P1 = D0 + D1*U1 + D2*U2
*        P2 = D3 + D4*U1 + D5*U2
*     PAR( NPAR ) = DOUBLE PRECISION (Given and Returned)
*        Region parameters.
*     B( LBND1:UBND1, LBND2:UBND2 ) = INTEGER (Given and Returned)
*        The array.
*     LBEXTB( 2 ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        exterior points in B. A value of VAL__MAXI for element 1 is
*        returned to indicate an "infinite" box. Other elements should
*        be ignored.
*     UBEXTB( 2 ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        exterior points in B. The returned values should be ignored
*        since the box is "infinite".
*     LBINTB( 2 ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        interior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBINTB( 2 ) = INTEGER (Given and Returned)
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
*     6-JUN-2012 (DSB):
*        Handle the zero/2.PI discontinity in celestial longitude.
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
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Arguments Given:
      INTEGER FRM
      INTEGER RINDEX
      INTEGER LBND1
      INTEGER UBND1
      INTEGER LBND2
      INTEGER UBND2
      INTEGER NPAR
      DOUBLE PRECISION D( 6 )
      DOUBLE PRECISION PAR( NPAR )

*  Arguments Given and Returned:
      INTEGER B( LBND1:UBND1, LBND2:UBND2 )
      INTEGER LBEXTB( 2 )
      INTEGER UBEXTB( 2 )
      INTEGER LBINTB( 2 )
      INTEGER UBINTB( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION
     :    HL,                    ! Half the requested box length
     :    HW,                    ! Half the requested box width
     :    LPAR( 16 ),            ! Local parameters
     :    P0( 2 ),               ! The box centre
     :    PA0,                   ! Requested angle as a position angle
     :    PA1,                   ! The position angle at the end
     :    PA2                    ! The position angle at the end
      INTEGER
     :    I,                     ! Position index
     :    J                      ! Axis index
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if the number of supplied parameters is wrong.
      IF( NPAR .NE. 5 ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETI( 'NP', NPAR )
         CALL ERR_REP( 'ARD1_ROT_ERR1', 'Wrong no. of parameters '//
     :                 '(^NP) supplied for a ROTBOX region in '//
     :                 'ARD1_ROT (programming error).', STATUS )
         GO TO 999
      END IF

*  Offset away from the centre point along the first edge, at the
*  requested angle, going half the box length. Using AST caters for both
*  Cartesian and spherical user coords. Store points in the LPAR
*  array, in a suitable order to make the points a continuous curve.
      PA0 = ( 90.0 - PAR( 5 ) )*ARD__DTOR
      HL = 0.5*PAR( 3 )
      HW = 0.5*PAR( 4 )
      P0( 1 ) = PAR( 1 )
      P0( 2 ) = PAR( 2 )

      PA1 = AST_OFFSET2( FRM, P0, PA0, HL, LPAR, STATUS )

*  Now turn to the left by 90 degrees and offset up by half the box height.
      PA1 = PA1 - ARD__PIBY2
      PA2 = AST_OFFSET2( FRM, LPAR, PA1, HW, LPAR( 3 ), STATUS )

*  Now offset down by half the box height.
      PA2 = AST_OFFSET2( FRM, LPAR, PA1, -HW, LPAR( 15 ), STATUS )

*  Now offset up and down by half the box height, starting at the box
*  centre.
      PA1 = PA0 - ARD__PIBY2
      PA2 = AST_OFFSET2( FRM, PAR, PA1, HW, LPAR( 5 ), STATUS )
      PA2 = AST_OFFSET2( FRM, PAR, PA1, -HW, LPAR( 13 ), STATUS )

*  Offset away from the centre point along the first edge, away from the
*  requested angle, going half the box length.
      PA1 = AST_OFFSET2( FRM, PAR, PA0, -HL, LPAR( 9 ), STATUS )

*  Now turn to the left by 90 degrees and offset up by half the box height.
      PA1 = PA1 - ARD__PIBY2
      PA2 = AST_OFFSET2( FRM, LPAR( 9 ), PA1, HW, LPAR( 7 ), STATUS )

*  Now offset down by half the box height.
      PA2 = AST_OFFSET2( FRM, LPAR( 9 ), PA1, -HW, LPAR( 11 ), STATUS )

*  If the frame contains a skyframe, there is the possibility that the
*  celestial longitude may pass through the zero/2.PI discontinuity. Since
*  the supplied mapping is linear, it cannot handle this. So now ensure
*  that all points are within +/- PI of the box centre. if the Frame is
*  not a SkyFrame, the positions are left unchanged.
      DO I = 0, 14, 2
         DO J = 1, 2
            LPAR( I + J ) = P0( J ) + AST_AXDISTANCE( FRM, J, P0( J ),
     :                                                LPAR( I + J ),
     :                                                STATUS )
         END DO
      END DO

*  The parameters are now in the same format as those for a POLYGON region.
*  Call the subroutine used to load a POLYGON region.
      CALL ARD1_POL( RINDEX, LBND1, UBND1, LBND2, UBND2, 16,
     :               D, LPAR, B, LBEXTB, UBEXTB, LBINTB, UBINTB,
     :               STATUS )

*  Jump here if an error occurs.
 999  CONTINUE

      END
