      SUBROUTINE ARD1_FRA( RINDEX, LBND1, UBND1, LBND2, UBND2, D, PAR,
     :                     B, LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )
*+
*  Name:
*     ARD1_FRA

*  Purpose:
*     Initialise an array to hold a FRAME region.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_FRA( RINDEX, LBND1, UBND1, LBND2, UBND2, D, PAR, B,
*                    LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )

*  Description:
*     The array B is initialised by setting all values within the
*     supplied interior bounding box to the exterior value 0.
*     All points outside this box already hold exterior values.
*     Interior values are then assigned to the points specified by the
*     supplied parameter. The supplied parameter is the border width in
*     user coords.

*  Arguments:
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
*     D( 6 ) = DOUBLE PRECISION (Given)
*        The coefficients of the user->pixel mapping. The mapping is:
*        P1 = D0 + D1*U1 + D2*U2
*        P2 = D3 + D4*U1 + D5*U2
*     PAR = DOUBLE PRECISION (Given and Returned)
*        The border width, supplied in user coords, returned in pixels.
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
*     15-APR-1994 (DSB):
*        Original version.
*     26-JUN-2001 (DSB):
*        Modified for ARD version 2.0.
*     5-DEC-2007 (DSB):
*        Correct test for shear.
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
      INTEGER LBND1
      INTEGER UBND1
      INTEGER LBND2
      INTEGER UBND2
      DOUBLE PRECISION D( 6 )

*  Arguments Given and Returned:
      DOUBLE PRECISION PAR
      INTEGER B( LBND1:UBND1, LBND2:UBND2 )
      INTEGER LBEXTB( 2 )
      INTEGER UBEXTB( 2 )
      INTEGER LBINTB( 2 )
      INTEGER UBINTB( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  I,                       ! X pixel index of current column
     :  IXHI,                    ! High bound of pixel x index
     :  IXLO,                    ! Low bound of pixel x index
     :  IYHI,                    ! High bound of pixel y index
     :  IYLO,                    ! Low bound of pixel y index
     :  J,                       ! Y pixel index of current row
     :  LBND( 2 ),               ! Mask lower bounds
     :  MSKSIZ,                  ! No. of elements in mask
     :  UBND( 2 )                ! Mask upper bounds

      DOUBLE PRECISION
     :  T,                       ! Temporary real storage
     :  XHI,                     ! High bound of pixel x coord
     :  XLO,                     ! Low bound of pixel x coord
     :  XSCA,                    ! X scale factor
     :  YHI,                     ! High bound of pixel y coord
     :  YLO,                     ! Low bound of pixel y coord
     :  YSCA                     ! Y scale factor

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the X and Y scale factors between user co-ordinates and
*  pixel co-ordinates.
      XSCA = SQRT( D( 2 )**2 + D( 5 )**2 )
      YSCA = SQRT( D( 3 )**2 + D( 6 )**2 )

*  Report an error and abort if the two scale factors are not equal.
      IF( ABS( XSCA - YSCA ) .GE. ABS( XSCA*VAL__EPSD ) .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = ARD__SCALE
         CALL ERR_REP( 'ARD1_FRA_ERR1', 'Current ARD user '//
     :                 'co-ordinate system has different '//
     :                 'scales in the X and Y directions.',
     :                 STATUS )
         CALL ERR_REP( 'ARD1_FRA_ERR2', 'Illegal FRAME keyword '//
     :                 'found in ARD expression. FRAME keywords '//
     :                 'can only be used if axis scales are equal.',
     :                 STATUS )
         GO TO 999
      END IF

*  Report an error and abort if the there is any shear.
      IF( ABS( D( 3 )*D( 6 ) + D( 2 )*D( 5 ) ) .GE.
     :    100*( XSCA + YSCA )*VAL__EPSD .AND.
     :    STATUS .EQ. SAI__OK ) THEN

         STATUS = ARD__SCALE
         CALL ERR_REP( 'ARD1_FRA_ERR3', 'Current ARD user '//
     :                 'co-ordinate system has sheared '//
     :                 'axes.',
     :                 STATUS )
         CALL ERR_REP( 'ARD1_FRA_ERR4', 'Illegal FRAME keyword '//
     :                 'found in ARD expression. FRAME keywords '//
     :                 'can only be used if axis are not sheared.',
     :                 STATUS )
         GO TO 999
      END IF

*  Convert the supplied border width from user to pixel coords.
      PAR = PAR*XSCA

*  Find the pixel co-ordinate bounds at the frame edge.
      XLO = DBLE( LBND1 ) - 1.0 + PAR
      XHI = DBLE( UBND1 ) - PAR
      YLO = DBLE( LBND2 ) - 1.0 + PAR
      YHI = DBLE( UBND2 ) - PAR

*  Convert these to pixel indices.
      T = XLO + 0.5
      IXLO = INT( T )
      IF( T .GT. 0.0 .AND. DBLE( IXLO ) .NE. T ) IXLO = IXLO + 1

      T = XHI + 0.5
      IXHI = INT( T )
      IF( T .LT. 0.0 .AND. DBLE( IXHI ) .NE. T ) IXHI = IXHI - 1

      T = YLO + 0.5
      IYLO = INT( T )
      IF( T .GT. 0.0 .AND. DBLE( IYLO ) .NE. T ) IYLO = IYLO + 1

      T = YHI + 0.5
      IYHI = INT( T )
      IF( T .LT. 0.0 .AND. DBLE( IYHI ) .NE. T ) IYHI = IYHI - 1

*  Limit to the bounds of the array.
      IXLO = MAX( IXLO, LBND1 )
      IXHI = MIN( IXHI, UBND1 )
      IYLO = MAX( IYLO, LBND2 )
      IYHI = MIN( IYHI, UBND2 )

*  If the border is not of zero width...
      IF( IXLO .GT. LBND1 .OR. IXHI .LT. UBND1 .OR.
     :    IYLO .GT. LBND2 .OR. IYHI .LT. UBND2 ) THEN

*  ... reset all pixels within the interior bounding box so that they
*  hold exterior values. The pixels outside the interior bounding box
*  already hold exterior values.
         LBND( 1 ) = LBND1
         UBND( 1 ) = UBND1
         LBND( 2 ) = LBND2
         UBND( 2 ) = UBND2
         MSKSIZ = ( UBND1 - LBND1 + 1 )*( UBND2 - LBND2 + 1 )

         CALL ARD1_BXSET( 2, LBND, UBND, MSKSIZ, 0, LBINTB,
     :                    UBINTB, B, STATUS )

*  Set up the bounds of the new interior bounding box (i.e. the whole
*  mask excluding the border).
         LBINTB( 1 ) = IXLO
         UBINTB( 1 ) = IXHI
         LBINTB( 2 ) = IYLO
         UBINTB( 2 ) = IYHI

*  Assign the supplied value to all points within the new interior
*  bounding box.
         DO J = LBINTB( 2 ), UBINTB( 2 )
            DO I = LBINTB( 1 ), UBINTB( 1 )
               B( I, J ) = RINDEX
            END DO
         END DO

*  If the interior bounding box is null (because the border was wider
*  than half the mask size), return the usual value (VAL__MINI for
*  LBINTB( 1 ) ).
         IF( LBINTB( 1 ) .GT. UBINTB( 1 ) .OR.
     :       LBINTB( 2 ) .GT. UBINTB( 2 ) ) LBINTB( 1 ) = VAL__MINI

*  Ensure the the exterior bounding box is returned "infinite".
         LBEXTB( 1 ) = VAL__MAXI

*  If the border has zero width...
      ELSE

*  ...return an "infinite" interior bounding box.
         LBINTB( 1 ) = VAL__MAXI

*  Fill the entire mask with interior values.
         DO J = LBND2, UBND2
            DO I = LBND1, UBND1
               B( I, J ) = RINDEX
            END DO
         END DO

*  Ensure the the exterior bounding box is returned "null".
         LBEXTB( 1 ) = VAL__MINI

      END IF

 999  CONTINUE

      END
