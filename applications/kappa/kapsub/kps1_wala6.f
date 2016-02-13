      SUBROUTINE KPS1_WALA6( MAP, LBNDX, UBNDX, LBNDY, UBNDY, IB1, IB2,
     :                       JB1, JB2, XAMAP, YAMAP, XA, YA, XB, YB, NK,
     :                       ICOL, JROW, NBAD, STATUS )
*+
*  Name:
*     KPS1_WALA6

*  Purpose:
*     Find the input and output image coordinates of a grid of nine
*     test points for WCSALIGN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_WALA6( MAP, LBNDX, UBNDX, LBNDY, UBNDY, IB1, IB2, JB1, JB2,
*                      XAMAP, YAMAP, XA, YA, XB, YB, NK, ICOL, JROW, NBAD,
*                      STATUS )

*  Description:
*     This routine sets up a grid of nine test points (3 rows of 3
*     points) evenly spaced in the section (IB1:IB2,JB1:JB2) of the
*     output image. It uses the supplied AST Mapping to calculate
*     the corresponding input pixel coordinates (unless they have alrady
*     benn calculated and stored in XAMAP and YAMAP), and stores them in
*     XAMAP and YAMAP. It also returns the coordinates of the test
*     points in XA, YA, XB and YB. Depending on the projections
*     involved, some of the test points may not correspond to valid
*     positions in the input pixel grid. Such test points are returned
*     with bad coordinates. If the size of the section is too small
*     some of the test points may be coincident. Each distinct test
*     point is only included once in the output arrays. NK returns the
*     number of distinct test points calculated.

*  Arguments:
*     MAP = INTEGER (Given)
*        AST Mapping from input pixel co-ordinates to reference (i.e.
*        output) pixel co-ordinates.
*     LBNDX = INTEGER (Given)
*        The lower X bound of the output image.
*     UBNDX = INTEGER (Given)
*        The upper X bound of the output image.
*     LBNDY = INTEGER (Given)
*        The lower Y bound of the output image.
*     UBNDY = INTEGER (Given)
*        The upper Y bound of the output image.
*     IB1 = INTEGER (Given)
*        The lower X bound of the section to be filled.
*     IB2 = INTEGER (Given)
*        The upper X bound of the section to be filled.
*     JB1 = INTEGER (Given)
*        The lower Y bound of the section to be filled.
*     JB2 = INTEGER (Given)
*        The upper Y bound of the section to be filled.
*     XAMAP( LBNDX:UBNDX, LBNDY:UBNDY ) = DOUBLE PRECISION (Returned)
*        Holds the input X pixel coordinate corresponding to the centre
*        of each output pixel.
*     YAMAP( LBNDX:UBNDX, LBNDY:UBNDY ) = DOUBLE PRECISION (Returned)
*        Holds the input Y pixel coordinate corresponding to the centre
*        of each output pixel.
*     XA( 9 ) = DOUBLE PRECISION( Returned)
*        The input X pixel coordinate of each test point.
*     YA( 9 ) = DOUBLE PRECISION( Returned)
*        The input Y pixel coordinate of each test point.
*     XB( 9 ) = DOUBLE PRECISION( Returned)
*        The output X pixel coordinate of each test point.
*     YB( 9 ) = DOUBLE PRECISION( Returned)
*        The output Y pixel coordinate of each test point.
*     NK = INTEGER (Returned)
*        The actual no. of test points returned in XA, YA, XB and YB.
*     ICOL( 3 ) = INTEGER (Returned)
*        The output column numbers at which the three columns of test
*        points are placed. ICOL( 1 ) and ICOL( 2 ) may be equal.
*     JROW( 3 ) = INTEGER (Returned)
*        The output row numbers at which the three rows of test
*        points are placed. JROW( 1 ) and JROW( 2 ) may be equal.
*     NBAD = INTEGER (Returned)
*        The number of test points with bad input pixel coordinates.
*        These are included in the NK values returned in XA, YA, etc.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1998 (DSB):
*        Original version, based on IRAS90:SALIA6.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER MAP
      INTEGER LBNDX
      INTEGER UBNDX
      INTEGER LBNDY
      INTEGER UBNDY
      INTEGER IB1
      INTEGER IB2
      INTEGER JB1
      INTEGER JB2

*  Arguments Given and Returned:
      DOUBLE PRECISION XAMAP( LBNDX:UBNDX, LBNDY:UBNDY )
      DOUBLE PRECISION YAMAP( LBNDX:UBNDX, LBNDY:UBNDY )

*  Arguments Returned:
      DOUBLE PRECISION XA( 9 )
      DOUBLE PRECISION YA( 9 )
      DOUBLE PRECISION XB( 9 )
      DOUBLE PRECISION YB( 9 )
      INTEGER NK
      INTEGER ICOL( 3 )
      INTEGER JROW( 3 )
      INTEGER NBAD

*  Status:
      INTEGER STATUS          ! Global status

*  Local Variables:
      DOUBLE PRECISION XX( 9 )! X pixel coordinate of test points
      DOUBLE PRECISION YY( 9 )! Y pixel coordinate of test points
      INTEGER COL             ! Current column of test points
      INTEGER DX              ! No. of columns between test points
      INTEGER DY              ! No. of rows between test points
      INTEGER I(9)            ! Output X pixel index of each test point
      INTEGER II              ! Output X pixel index
      INTEGER J(9)            ! Output Y pixel index of each test point
      INTEGER JJ              ! Output Y pixel index
      INTEGER K               ! Index of current test point
      INTEGER KK(9)           ! Index of test points to be transformed
      INTEGER L               ! Index of current transformed test point
      INTEGER NL              ! No. of test points not yet transformed
      INTEGER ROW             ! Current row of test points
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the number of columns of pixels between the left hand and
*  middle columns of test points.
      DX = ( IB2 - IB1 )/2

*  Set up the number of rows of pixels between the bottom and
*  middle rows of test points.
      DY = ( JB2 - JB1 )/2

*  Store the indices of the three columns of test points. Note, the left
*  hand and middle columns will be coincident if IB1 and IB2 are
*  adjacent.
      ICOL( 1 ) = IB1
      ICOL( 2 ) = IB1 + DX
      ICOL( 3 ) = IB2

*  Store the indices of the three rows of test points. Note, the bottom
*  and middle rows will be coincident if JB1 and JB2 are adjacent.
      JROW( 1 ) = JB1
      JROW( 2 ) = JB1 + DY
      JROW( 3 ) = JB2

*  Initialise the number of distinct test points to zero.
      NK = 0

*  Loop round each row of test points.
      DO ROW = 1, 3

*  Skip over the middle row if it coincident with the bottom row.
         IF( ROW .NE. 2 .OR. DY .NE. 0 ) THEN

*  Loop round each column of test points.
            DO COL = 1, 3

*  Skip over the middle column if it coincident with the left hand
*  column.
               IF( COL .NE. 2 .OR. DX .NE. 0 ) THEN

*  Increment the number of distinct test points, and store the X and Y
*  indices of the current test point.
                  NK = NK + 1
                  I( NK ) = ICOL( COL )
                  J( NK ) = JROW( ROW )

               END IF

            END DO

         END IF

      END DO

*  We now have lists of the pixel indices of the distinct test points.
*  See if any of these points have already been transformed. If they
*  have, return the coordinates previously stored in the X and Y maps.
*  Construct a list of test points for which no transformed coordinates
*  are yet available. First initialise the number of such test points to
*  zero.
      NL = 0

*  Loop round the distinct test points.
      DO K = 1, NK

*  Store the pixel indices of this test point.
         II = I( K )
         JJ = J( K )

*  Store the pixel coordinates of this test point. These refer to the
*  output pixel grid.
         XB( K ) = DBLE( II ) - 0.5D0
         YB( K ) = DBLE( JJ ) - 0.5D0

*  If the X and Y maps contain good values at this test point, store the
*  values as the input pixel coordinates of the test point.
         IF( XAMAP( II, JJ ) .NE. AST__BAD .AND.
     :       YAMAP( II, JJ ) .NE. AST__BAD ) THEN
            XA( K ) = XAMAP( II, JJ )
            YA( K ) = YAMAP( II, JJ )

*  If the input coordinates corresponding to the test point have not yet
*  been calculated, add the test point to the list of those to be
*  transformed.
         ELSE
            NL = NL + 1
            XX( NL ) = DBLE( XB( K ) )
            YY( NL ) = DBLE( YB( K ) )
            KK( NL ) = K
         END IF

      END DO

*  If the input pixel coordinates of any of the test points have not yet
*  been calculated, do it now using the full projection mappings.
      IF( NL .GT. 0 ) THEN

*  Convert output pixel coordinates to input pixel co-ordinates.
         CALL AST_TRAN2( MAP, NL, XX, YY, .FALSE., XX, YY, STATUS )

*  Now store the input pixel coordinates in the returned arrays.
         NBAD = 0

         DO L = 1, NL
            K = KK( L )

            IF( XX( L ) .NE. AST__BAD .AND. YY( L ) .NE. AST__BAD ) THEN
               XA( K ) = DBLE( XX( L ) )
               YA( K ) = DBLE( YY( L ) )
               XAMAP( I( K ), J( K ) ) = DBLE( XX( L ) )
               YAMAP( I( K ), J( K ) ) = DBLE( YY( L ) )

            ELSE
               NBAD = NBAD + 1
               XA( K ) = AST__BAD
               YA( K ) = AST__BAD
               XAMAP( I( K ), J( K ) ) = AST__BAD
               YAMAP( I( K ), J( K ) ) = AST__BAD

            END IF

         END DO

      END IF

      END
