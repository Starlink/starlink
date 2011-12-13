      SUBROUTINE KPS1_DNRTH( INK, IPLOT, LENS, ARROW, GC0, SIZE,
     :                       X1, X2, Y1, Y2, STATUS )
*+
*  Name:
*     KPS1_DNRTH

*  Purpose:
*     Draws direction arrows for DRAWNORTH.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_DNRTH( INK, IPLOT, LENS, ARROW, GC0, SIZE, X1, X2, Y1,
*                      Y2, STATUS )

*  Description:
*     The arrows and labels defined by the supplied arguments are drawn,
*     and the bounding box containing them is returned. If INK is .FALSE.
*     then "invisible ink" is used (i.e. nothing is actually drawn), but
*     the bounding box is still returned.

*  Arguments:
*     INK = LOGICAL (Given)
*        If .FALSE, do all the calculations but do not actually draw anything.
*     IPLOT = INTEGER (Given)
*        The Plot. The current Frame should eb the one for which
*        direction arrows are required. This Frame should have exactly 2 axes.
*     LENS( 2 ) = REAL (Given)
*        The lengths of the arrows (in millimetres).
*     ARROW = REAL (Given)
*        The size of the arrow heads (in millimetres).
*     GC0( 2 ) = DOUBLE PRECISION (Given)
*        The GRAPHICS coordinates at which the arrows are to be drawn.
*     SIZE = REAL (Given)
*        The minimum dimension of the underlying DATA picture, in millimetres.
*     X1 = REAL (Returned)
*        The GRAPHICS X value at the left edge of the box just enclosing
*        the labels and arrows.
*     X2 = REAL (Returned)
*        The GRAPHICS X value at the right edge of the box just enclosing
*        the labels and arrows.
*     Y1 = REAL (Returned)
*        The GRAPHICS Y value at the bottom edge of the box just enclosing
*        the labels and arrows.
*     Y2 = REAL (Returned)
*        The GRAPHICS Y value at the top edge of the box just enclosing
*        the labels and arrows.

*  Copyright:
*     Copyright (C) 2002 Central Laboratory of the Research Councils

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
*     13-AUG-2002 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'AST_PAR'          ! AST_ constants

*  Arguments Given:
      LOGICAL INK
      INTEGER IPLOT
      REAL LENS( 2 )
      REAL ARROW
      DOUBLE PRECISION GC0( 2 )
      REAL SIZE

*  Arguments Returned:
      REAL X1
      REAL X2
      REAL Y1
      REAL Y2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION PIBY2     ! pi/2
      PARAMETER ( PIBY2 = 1.57079632679 )

      REAL COSPI8                ! Cosine of pi/8
      PARAMETER ( COSPI8 = 0.9238795 )

      REAL SINPI8                ! Sine of pi/8
      PARAMETER ( SINPI8 = 0.3826834 )

*  Local Variables:
      CHARACTER HJ*1             ! Horizontal justification letter
      CHARACTER LABEL*20         ! Text label
      CHARACTER VJ*1             ! Vertical justification letter
      DOUBLE PRECISION ANG       ! Arrow position angle in radians
      DOUBLE PRECISION ATTR( 20 )! Saved graphics attribute values
      DOUBLE PRECISION CC0( 2 )  ! Current Frame coords at origin
      DOUBLE PRECISION CC1( 2 )  ! Current Frame coords
      DOUBLE PRECISION CC2( 2 )  ! Current Frame coords
      DOUBLE PRECISION D         ! A small distance in current frame
      DOUBLE PRECISION GAP       ! Gap between arrow and label
      DOUBLE PRECISION GC1( 2 )  ! Graphics coords
      DOUBLE PRECISION GC2( 2 )  ! Graphics coords
      INTEGER BFRM               ! Pointer to Base Frame
      INTEGER IBASE              ! Index of Base Frame
      INTEGER ICURR              ! Index of original current Frame
      INTEGER MAP                ! Pointer to Base->required Mapping
      INTEGER RFRM               ! Pointer to required Frame
      REAL BXLBN( 2 )            ! Lower limits of bounding box for text
      REAL BXUBN( 2 )            ! Upper limits of bounding box for text
      REAL COSA                  ! Cosine of axis angle
      REAL RATS( 5 )             ! The original graphics attributes
      REAL SINA                  ! Sine of axis angle
      REAL UP( 2 )               ! The up vector

      DATA UP /0.0D0, 1.0D0 /
*.

*  Initialize the returned bounding box.
      X1 = VAL__MAXR
      X2 = VAL__MINR
      Y1 = VAL__MAXR
      Y2 = VAL__MINR

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make AST Plot methods use invisible ink, if requested.
      CALL AST_SETL( IPLOT, 'Invisible', .NOT. INK, STATUS )

*  Node the index of the Current (required) Frame.
      ICURR = AST_GETI( IPLOT, 'Current', STATUS )

*  Node the index of the Base (GRAPHICS) Frame.
      IBASE = AST_GETI( IPLOT, 'Base', STATUS )

*  Get a pointer to the Current (required) Frame.
      RFRM = AST_GETFRAME( IPLOT, AST__CURRENT, STATUS )

*  Get a pointer to the Base (GRAPHICS) Frame.
      BFRM = AST_GETFRAME( IPLOT, AST__BASE, STATUS )

*  Get the Mapping from the Base (GRAPHICS) Frame to the required Frame.
      MAP = AST_GETMAPPING( IPLOT, AST__BASE, AST__CURRENT, STATUS )

*  Map the origin into the required Frame.
      CALL AST_TRAN2( MAP, 1, GC0( 1 ), GC0( 2 ), .TRUE., CC0( 1 ),
     :                CC0( 2 ), STATUS )

*  Abort if the origin is bad in the required Frame.
      IF( CC0( 1 ) .EQ. AST__BAD .OR. CC0( 2 ) .EQ. AST__BAD ) GO TO 999

*  Increase the graphics X value at the origin by 1 mm and map into
*  the required Frame.
      GC1( 1 ) = GC0( 1 ) + 1.0D0
      GC1( 2 ) = GC0( 2 )
      CALL AST_TRAN2( MAP, 1, GC1( 1 ), GC1( 2 ), .TRUE., CC1( 1 ),
     :                CC1( 2 ), STATUS )

*  Find the arc distance between these two points in the required Frame.
*  This is our "small distance" which we will use to calculate the
*  direction of the axis at the origin.
      D = AST_DISTANCE( RFRM, CC0, CC1, STATUS )

*  Draw the arrow and label for the first axis if the arrow has non-zero
*  length.
      IF( LENS( 1 ) .GT. 0.0 ) THEN

*  Offset away from the origin along the first axis by a "small distance".
         CC2( 1 ) = CC0( 1 ) + D
         CC2( 2 ) = CC0( 2 )
         CALL AST_OFFSET( RFRM, CC0, CC2, D, CC1, STATUS )

*  Convert this point back to GRAPHICS coords.
         CALL AST_TRAN2( MAP, 1, CC1( 1 ), CC1( 2 ), .FALSE.,
     :                   GC1( 1 ), GC1( 2 ), STATUS )

*  Find the angle between the 2nd GRAPHICS axis and the line from the
*  origin to the point found above.
         ANG = AST_AXANGLE( BFRM, GC0, GC1, 1, STATUS )

*  Abort if this angle is bad.
         IF( ANG .EQ. AST__BAD ) GO TO 999

*  Convert to the sort of angle needed by KPS1_VECT.
         ANG = - ( ANG + PIBY2 )

*  Set up the correct PGPLOT attributes for this arrow.
         CALL KPG1_PGSTY( IPLOT, 'AXIS1', .TRUE., ATTR, STATUS )

*  Draw the vector.
         CALL KPS1_VECT( INK, REAL( GC0( 1 ) ), REAL( GC0( 2 ) ),
     :                   'START', LENS( 1 ), REAL( ANG ), ARROW, X1,
     :                   X2, Y1, Y2, STATUS )

*  Re-instate the original PGPLOT attributes.
         CALL KPG1_PGSTY( IPLOT, 'AXIS1', .FALSE., ATTR, STATUS )

*  Draw the label if required.
         IF( AST_GETL( IPLOT, 'TextLab(1)', STATUS ) ) THEN

*  Get the required gap between the end of the arrow and the label, in
*  millimetres.
            GAP = SIZE*AST_GETR( IPLOT, 'TextLabGap(1)', STATUS )

*  Find the corresponding graphics position.
            CALL AST_OFFSET( BFRM, GC0, GC1, LENS( 1 ) + GAP, GC2,
     :                       STATUS )

*  Get the text for the label.
            LABEL = AST_GETC( RFRM, 'Symbol(1)', STATUS )

*  Choose the justification for the string.
            COSA = COS( ANG )
            IF( COSA .GT. COSPI8 ) THEN
               VJ = 'B'
            ELSE IF( COSA .LT. -COSPI8 ) THEN
               VJ = 'T'
            ELSE
               VJ = 'C'
            END IF

            SINA = SIN( ANG )
            IF( SINA .GT. SINPI8 ) THEN
               HJ = 'R'
            ELSE IF( SINA .LT. -SINPI8 ) THEN
               HJ = 'L'
            ELSE
               HJ = 'C'
            END IF

*  Make the AST_TEXT method draw strings using the TextLab1 attributes.
            CALL KPG1_CPSTY( IPLOT, 'TextLab1', 'Strings', RATS,
     :                       STATUS )

*  Draw the text string, temporarily making the GRAPHICS Frame the
*  current Frame.
            CALL AST_SETI( IPLOT, 'Current', IBASE, STATUS )

            CALL AST_TEXT( IPLOT, LABEL, GC2, UP, VJ//HJ, STATUS )
            CALL AST_SETI( IPLOT, 'Current', ICURR, STATUS )

*  Find the bounding box enclosing the text produced by the above call to
*  AST_TEXT and update the total bounding box to include it.
            CALL AST_BOUNDINGBOX( IPLOT, BXLBN, BXUBN, STATUS )
            X1 = MIN( X1, BXLBN( 1 ) )
            X2 = MAX( X2, BXUBN( 1 ) )
            Y1 = MIN( Y1, BXLBN( 2 ) )
            Y2 = MAX( Y2, BXUBN( 2 ) )

         END IF

      END IF

*  Draw the arrow and label for the second axis if the arrow has non-zero
*  length.
      IF( LENS( 2 ) .GT. 0.0 ) THEN

*  Offset away from the origin along the second axis by a "small distance".
         CC2( 1 ) = CC0( 1 )
         CC2( 2 ) = CC0( 2 ) + D
         CALL AST_OFFSET( RFRM, CC0, CC2, D, CC1, STATUS )

*  Convert this point back to GRAPHICS coords.
         CALL AST_TRAN2( MAP, 1, CC1( 1 ), CC1( 2 ), .FALSE.,
     :                   GC1( 1 ), GC1( 2 ), STATUS )

*  Find the angle between the 2nd GRAPHICS axis and the line from the
*  origin to the point found above.
         ANG = AST_AXANGLE( BFRM, GC0, GC1, 1, STATUS )

*  Abort if this angle is bad.
         IF( ANG .EQ. AST__BAD ) GO TO 999

*  Convert to the sort of angle needed by KPS1_VECT.
         ANG = - ( ANG + PIBY2 )

*  Set up the correct PGPLOT attributes for this arrow.
         CALL KPG1_PGSTY( IPLOT, 'AXIS2', .TRUE., ATTR, STATUS )

*  Draw the vector.
         CALL KPS1_VECT( INK, REAL( GC0( 1 ) ), REAL( GC0( 2 ) ),
     :                   'START', LENS( 2 ), REAL( ANG ), ARROW, X1,
     :                   X2, Y1, Y2, STATUS )

*  Re-instate the original PGPLOT attributes.
         CALL KPG1_PGSTY( IPLOT, 'AXIS2', .FALSE., ATTR, STATUS )

*  Draw the label if required.
         IF( AST_GETL( IPLOT, 'TextLab(2)', STATUS ) ) THEN

*  Get the required gap between the end of the arrow and the label, in
*  millimetres.
            GAP = SIZE*AST_GETR( IPLOT, 'TextLabGap(2)', STATUS )

*  Find the corresponding graphics position.
            CALL AST_OFFSET( BFRM, GC0, GC1, LENS( 2 ) + GAP, GC2,
     :                       STATUS )

*  Get the text for the label.
            LABEL = AST_GETC( RFRM, 'Symbol(2)', STATUS )

*  Choose the justification for the string.
            COSA = COS( ANG )
            IF( COSA .GT. COSPI8 ) THEN
               VJ = 'B'
            ELSE IF( COSA .LT. -COSPI8 ) THEN
               VJ = 'T'
            ELSE
               VJ = 'C'
            END IF

            SINA = SIN( ANG )
            IF( SINA .GT. SINPI8 ) THEN
               HJ = 'R'
            ELSE IF( SINA .LT. -SINPI8 ) THEN
               HJ = 'L'
            ELSE
               HJ = 'C'
            END IF

*  Make the AST_TEXT method draw strings using the TextLab2 attributes.
            CALL KPG1_CPSTY( IPLOT, 'TextLab2', 'Strings', RATS,
     :                       STATUS )

*  Draw the text string, temporarily making the GRAPHICS Frame the
*  current Frame.
            CALL AST_SETI( IPLOT, 'Current', IBASE, STATUS )
            CALL AST_TEXT( IPLOT, LABEL, GC2, UP, VJ//HJ, STATUS )
            CALL AST_SETI( IPLOT, 'Current', ICURR, STATUS )

*  Find the bounding box enclosing the text produced by the above call to
*  AST_TEXT and update the total bounding box to include it.
            CALL AST_BOUNDINGBOX( IPLOT, BXLBN, BXUBN, STATUS )
            X1 = MIN( X1, BXLBN( 1 ) )
            X2 = MAX( X2, BXUBN( 1 ) )
            Y1 = MIN( Y1, BXLBN( 2 ) )
            Y2 = MAX( Y2, BXUBN( 2 ) )

         END IF

      END IF

 999  CONTINUE

*  Make AST Plot methods use visible ink.
      CALL AST_SETL( IPLOT, 'Invisible', .FALSE., STATUS )

      END
