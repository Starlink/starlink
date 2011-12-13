      SUBROUTINE ARD1_ELL( FRM, RINDEX, LBND1, UBND1, LBND2, UBND2,
     :                     NPAR, D, PAR, B, LBEXTB, UBEXTB, LBINTB,
     :                     UBINTB, STATUS )
*+
*  Name:
*     ARD1_ELL

*  Purpose:
*     Initialise an array to hold a ELLIPSE region.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_ELL( FRM, RINDEX, LBND1, UBND1, LBND2, UBND2, NPAR, D, PAR,
*                    B, LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )

*  Description:
*     The array B is initialised by setting all values within the
*     supplied interior bounding box to the exterior value 0.
*     All points outside this box already hold exterior values.
*     Interior values are then assigned to the points specified by the
*     supplied parameters. The supplied parameters are the user
*     co-ordinates of the ellipse centre, the lengths of the two ellipse
*     axes (in user co-ordinates), and the angle from the user X axis to
*     the first ellipse axis (in degrees, measured +ve from user +X to
*     user +Y).

*  Arguments:
*     FRM = INTEGER (Given)
*        A pointer to the user cordinate Frame.
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
*     PAR( NPAR ) = DOUBLE PRECISION (Given)
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
*     11-APR-1994 (DSB):
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
     :  AA1,                     ! First ellipse axis projected on Xp
     :  AA2,                     ! First ellipse axis projected on Yp
     :  ANG1,                    ! An angle
     :  ANG2,                    ! An angle
     :  BB1,                     ! Second ellipse axis projected on Xp
     :  BB2,                     ! Second ellipse axis projected on Yp
     :  COSA,                    ! Cosine of supplied orientation
     :  COSP,                    ! Cosine of central latitude
     :  D2,                      ! Modified coefficient
     :  D5,                      ! Modified coefficient
     :  F,                       ! Ratio of axis scales
     :  P( 2 ),                  ! Test point
     :  SDIST,                   ! Corresponding increment along the other axis
     :  SINA,                    ! Sine of supplied orientation
     :  T,                       ! Temporary real storage.
     :  X0,                      ! X pixel co-ordinate at ellipse centre
     :  X1                       ! X pixel coord at lower intersection

      REAL
     :  X2,                      ! X pixel coord at upper intersection
     :  XRANGE,                  ! Half range of X pixel co-ordinates
     :  Y,                       ! Y pixel coordinate at row centre
     :  Y0,                      ! Y pixel co-ordinate at ellipse centre
     :  YHI,                     ! High limit of Y on ellipse
     :  YLO,                     ! Low limit of Y on ellipse
     :  YRANGE                   ! Half range of Y pixel co-ordinates

      INTEGER
     :  AXIS,                    ! Index of axis used to measure distances
     :  IX,                      ! X pixel index of current column
     :  IX1,                     ! Lower X pixel index limit
     :  IX2,                     ! Upper X pixel index limit
     :  IY,                      ! Y pixel index of current row
     :  LBND( 2 ),               ! Mask lower bounds
     :  MSKSIZ,                  ! No. of elements in mask
     :  RL,                      ! Real lower bound used on 2nd axis
     :  RU,                      ! Real uper bound used on 2nd axis
     :  UBND( 2 )                ! Mask upper bounds

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if less than 5 parameters have been supplied.
      IF( NPAR .LT. 5 ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETI( 'NP', NPAR )
         CALL ERR_REP( 'ARD1_ELL_ERR1', 'Wrong no. of parameters '//
     :                 '(^NP) supplied for ARD1_ELL (programming '//
     :                 'error).', STATUS )
         GO TO 999
      END IF

*  Reset all pixels within the interior bounding box so that they
*  hold exterior values. The pixels outside the interior bounding box
*  already hold exterior values.
      LBND( 1 ) = LBND1
      UBND( 1 ) = UBND1
      LBND( 2 ) = LBND2
      UBND( 2 ) = UBND2
      MSKSIZ = ( UBND1 - LBND1 + 1 )*( UBND2 - LBND2 + 1 )

      CALL ARD1_BXSET( 2, LBND, UBND, MSKSIZ, 0, LBINTB,
     :                 UBINTB, B, STATUS )

*  Initialise the x-axis bounds of the interior bounding box.
      LBINTB( 1 ) = VAL__MAXI
      UBINTB( 1 ) = VAL__MINI

*  Find the pixel co-ordinates of the ellipse centre.
      X0 = D( 1 ) + D( 2 )*PAR( 1 ) + D( 3 )*PAR( 2 )
      Y0 = D( 4 ) + D( 5 )*PAR( 1 ) + D( 6 )*PAR( 2 )

*  We need to take account of any difference in the scales of the axes in
*  the user coordinate system (eg if users coords are RA/DEC, then a given
*  arc-distance will in general correspond to different increments along
*  the RA and DEC axes). Find the axis along which distances are specified
*  in keyword argument lists.
      CALL ARD1_DSTAX( FRM, AXIS, STATUS )

*  Find the increment along the other axis which corresponds to the
*  length of the first or second ellipse axis. The conversion is performed
*  at the ellipse centre. Find the ratio of the axis scales.
      IF( PAR( 3 ) .NE. 0.0 ) THEN
         CALL ARD1_SCALE( FRM, PAR, PAR( 3 ), 3 - AXIS, SDIST, STATUS )
         F = SDIST/PAR( 3 )
      ELSE
         CALL ARD1_SCALE( FRM, PAR, PAR( 4 ), 3 - AXIS, SDIST, STATUS )
         F = SDIST/PAR( 4 )
      END IF

*  Modify the D( 2 ) and D( 5 ) terms to describe a coord system in which
*  the first axis is on the same scale as the second axis.
      D2 = D( 2 )*F
      D5 = D( 5 )*F

*  Store the sine and cosine of the orientation of the ellipse in user
*  co-ordinates.
      SINA = SIN( PAR( 5 )*ARD__DTOR )
      COSA = COS( PAR( 5 )*ARD__DTOR )

*  Find the half-ranges of pixel co-ordinates encompassed by the
*  ellipse.
      AA1 = PAR( 3 )*( D2*COSA + D( 3 )*SINA )
      BB1 = PAR( 4 )*( D( 3 )*COSA - D2*SINA )
      XRANGE = SQRT( MAX( 0.0D0, AA1**2 + BB1**2 ) )

      AA2 = PAR( 3 )*( D5*COSA + D( 6 )*SINA )
      BB2 = PAR( 4 )*( D( 6 )*COSA - D5*SINA )
      YRANGE = SQRT( MAX( 0.0D0, AA2**2 + BB2**2 ) )

*  Abort if the ellipse is actually a line.
      IF( XRANGE .LE. 0.0D0 .OR. YRANGE .LE. 0.0D0 ) THEN
         STATUS = ARD__INTER
         CALL ERR_REP( 'ARD1_ELL_ERR2', 'Null ELLIPSE region '//
     :                 'encountered in ARD description.', STATUS )
         GO TO 999
      END IF

*  Find an angle needed later.
      ANG2  = ATAN2( AA1, BB1 ) - ATAN2( AA2, BB2 )

*  Find the maximum and minimum Y pixel co-ordinates covered by the
*  ellipse.
      YHI = Y0 + YRANGE
      YLO = Y0 - YRANGE

*  Convert these values to pixel indices.
      LBINTB( 2 ) = INT( YLO ) - 1
      UBINTB( 2 ) = INT( YHI ) + 1

*  Limit them to the bounds of the array.
      LBINTB( 2 ) = MAX( LBND2, LBINTB( 2 ) )
      UBINTB( 2 ) = MIN( UBND2, UBINTB( 2 ) )

*  Initialize the real bounds used by the second axis.
      RL = VAL__MAXI
      RU = VAL__MINI

*  Loop round the range of rows covered by the ellipse.
      DO IY = LBINTB( 2 ), UBINTB( 2 )

*  Find the displacement from the ellipse centre to the centre of the
*  current row.
         Y = DBLE( IY ) - 0.5 - Y0

*  See if this row intersects the ellipse.
         IF( ABS( Y ) .LE. YRANGE ) THEN

*  If so, find the X pixel co-ordinate values at which the row
*  intersects the ellipse.
            ANG1 = ASIN( Y/YRANGE )
            X1 = XRANGE*SIN( ANG1 + ANG2 ) + X0
            X2 = XRANGE*SIN( ANG1 - ANG2 ) + X0

*  Ensure that X2 is not less than X1.
            IF( X2 .LT. X1 ) THEN
               T = X2
               X2 = X1
               X1 = T
            END IF

*  Find the corresponding pixel indices. IX1 is the lower pixel index
*  bound and IX2 is the upper pixel index bound.
            T = X1 + 0.5
            IX1 = INT( T )
            IF( T .GT. 0.0D0 .AND. DBLE( IX1 ) .NE. T ) IX1 = IX1 + 1

            T = X2 + 0.5
            IX2 = INT( T )
            IF( T .LT. 0.0D0 .AND. DBLE( IX2 ) .NE. T ) IX2 = IX2 - 1

*  Limit them to the bounds of the mask.
            IX1 = MAX( IX1, LBND1 )
            IX2 = MIN( IX2, UBND1 )

*  Update the X bounds of the internal bounding box.
            LBINTB( 1 ) = MIN( LBINTB( 1 ), IX1 )
            UBINTB( 1 ) = MAX( UBINTB( 1 ), IX2 )

*  Assign the supplied value to all pixels on the current row between
*  the pixel index bounds just found.
            DO IX = IX1, IX2
               B( IX, IY ) = RINDEX
            END DO

*  Update the bounds of the real bounding box on the second axis.
            RL = MIN( RL, IY )
            RU = MAX( RU, IY )

         END IF

      END DO

*  If the interior bounding box is null, return the usual value
*  (VAL__MINI for LBINTB( 1 ) ).
      IF( RL .EQ. VAL__MAXI ) THEN
         LBINTB( 1 ) = VAL__MINI
      ELSE
         LBINTB( 2 ) = RL
         UBINTB( 2 ) = RU
      END IF

*  Ensure the the exterior bounding box is returned "infinite".
      LBEXTB( 1 ) = VAL__MAXI

*  Jump to here if an error occurs.
 999  CONTINUE

      END
