      SUBROUTINE KPG1_GDARE( JUST, FRACT, ASPECT, X1, X2, Y1, Y2,
     :                       STATUS )
*+
*  Name:
*     KPG1_GDARE

*  Purpose:
*     Defines a region within the current PGPLOT window.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL KPG1_GDARE( JUST, FRACT, ASPECT, X1, X2, Y1, Y2, STATUS )

*  Description:
*     This subroutine defines a two-dimensional region in the current
*     PGPLOT window.  The region is defined by the given position
*     justification, and the linear fraction along each axis of the
*     current PGPLOT window; and an aspect ratio.  The linear fraction is
*     applied first followed by the aspect-ratio constraint.

*  Arguments:
*     JUST = CHARACTER * ( * ) (Given)
*        Justification of the new region specified by a two-character
*        code.  The first character controls the vertical location, and
*        may be T, B, or C to create the new region at the top, bottom,
*        or in the centre respectively.  The second defines the
*        horizontal situation, and may be L, R, or C to define a new
*        region to the left, right, or in the centre respectively.
*        Thus a code of BR will make a new region in the bottom-right
*        corner.  The justification code need not be in uppercase.
*     FRACT( 2 ) = REAL (Given)
*        The fractional size of the new region applied along each axis.
*        So values of 0.5,0.5 would create a picture 0.25 the area of
*        the current window.
*     ASPECT  = REAL (Given)
*        The aspect ratio of the new region (x/y).  If the value is
*        negative, no aspect-ratio constraint is applied to define the
*        new region.
*     X1 = REAL (Returned)
*        Lower x world co-ordinate in the current window of the new
*        region.
*     X2 = REAL (Returned)
*        Upper x world co-ordinate in the current window of the new
*        region.
*     Y1 = REAL (Returned)
*        Lower y world co-ordinate in the current window of the new
*        region.
*     Y2 = REAL (Returned)
*        Upper y world co-ordinate in the current window of the new
*        region.
*     STATUS  = INTEGER (Given and Returned)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-AUG-1998 (DSB):
*       Original version. Based on KPS1_FRARE by MJC.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! SSE definitions

*  Status:
      INTEGER STATUS            ! Global Status

*  Arguments Given:
      CHARACTER * ( * ) JUST
      REAL FRACT( 2 )
      REAL ASPECT

*  Arguments Returned:
      REAL X1
      REAL X2
      REAL Y1
      REAL Y2

*  Local Variables:
      REAL XCEN                 ! X centre of the current window
      REAL XHIGH                ! X upper bound of the current window
      REAL XLOW                 ! X lower bound of the current window
      REAL XM                   ! Frame window size in x (dummy)
      REAL XRANGE               ! X size of the current window in world
      REAL YCEN                 ! Y centre of the current window
      REAL YHIGH                ! Y upper bound of the current window
      REAL YLOW                 ! Y lower bound of the current window
      REAL YM                   ! Frame window size in y (dummy)
      REAL YRANGE               ! Y size of the current window in world
*.

*  Check the inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Get the bounds of the current window in millimetres, and convert to metres.
      CALL PGQVP( 2, XLOW, XHIGH, YLOW, YHIGH )
      XM = ( XHIGH - XLOW )/1000.0
      YM = ( YHIGH - YLOW )/1000.0

*  Get its bounds in world co-ordinates.
      CALL PGQWIN( XLOW, XHIGH, YLOW, YHIGH )

*  Find the co-ordinates of the centre and the height and width of the
*  window.
      XCEN = 0.5 * ( XLOW + XHIGH )
      YCEN = 0.5 * ( YLOW + YHIGH )
      XRANGE = XHIGH - XLOW
      YRANGE = YHIGH - YLOW

*  Get the horizontal limits depending on the justification code.  L
*  for left, R for right, and C for centre.
      IF( JUST( 2:2 ) .EQ. 'L' .OR. JUST( 2:2 ) .EQ. 'l' ) THEN
         X1 = XLOW
         X2 = XLOW + FRACT( 1 ) * XRANGE

      ELSE IF( JUST( 2:2 ) .EQ. 'R' .OR. JUST( 2:2 ) .EQ. 'r' ) THEN
         X1 = XHIGH - FRACT( 1 ) * XRANGE
         X2 = XHIGH

      ELSE IF( JUST( 2:2 ) .EQ. 'C' .OR. JUST( 2:2 ) .EQ. 'c' ) THEN
         X1 = XCEN - 0.5 * FRACT( 1 ) * XRANGE
         X2 = XCEN + 0.5 * FRACT( 1 ) * XRANGE

*  Report an error when the code is not recognised.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'JUST', JUST )
         CALL ERR_REP( 'KPG1_GDARE_INVCD', 'Probable programming '//
     :                 'error: invalid justification code ''^JUST'' '//
     :                 'used to define a new region.', STATUS )
         GO TO 999
      END IF

*  Get the vertical limits depending on the justification code.  B for
*  bottom, T for top, and C for centre.
      IF( JUST( 1:1 ) .EQ. 'B' .OR. JUST( 1:1 ) .EQ. 'b' ) THEN
         Y1 = YLOW
         Y2 = YLOW + FRACT( 2 ) * YRANGE

      ELSE IF( JUST( 1:1 ) .EQ. 'T' .OR. JUST( 1:1 ) .EQ. 't' ) THEN
         Y1 = YHIGH - FRACT( 2 ) * YRANGE
         Y2 = YHIGH

      ELSE IF( JUST( 1:1 ) .EQ. 'C' .OR. JUST( 1:1 ) .EQ. 'c' ) THEN
         Y1 = YCEN - 0.5 * FRACT( 2 ) * YRANGE
         Y2 = YCEN + 0.5 * FRACT( 2 ) * YRANGE

*  Report an error when the code is not recognised.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'JUST', JUST )
         CALL ERR_REP( 'KPG1_GDARE_INVCD', 'Probable programming '//
     :                 'error: invalid justification code ''^JUST'' '//
     :                 'used to define a new region.', STATUS )
      END IF

*  Check to prevent picture-out-of-bounds errors.
      X1 = MIN( MAX( XLOW, X1 ), XHIGH )
      Y1 = MIN( MAX( YLOW, Y1 ), YHIGH )
      X2 = MIN( MAX( XLOW, X2 ), XHIGH )
      Y2 = MIN( MAX( YLOW, Y2 ), YHIGH )

*  Decide whether to apply an aspect-ratio constraint.
      IF( ASPECT .GT. 0.0 ) THEN

*  For tall windows, reduce the width of the window specified by FRACTION
*  alone.
         IF( ASPECT .LT. 1.0 ) THEN

*  Get the required width.
            XRANGE = ( Y2 - Y1 ) * ASPECT

*  Adjust the upper and/or lower X bound, depending on the justification.
            IF( JUST( 2:2 ) .EQ. 'L' .OR.
     :          JUST( 2:2 ) .EQ. 'l' ) THEN
               X2 = X1 + XRANGE

            ELSE IF( JUST( 2:2 ) .EQ. 'R' .OR.
     :               JUST( 2:2 ) .EQ. 'r' ) THEN
               X1 = X2 - XRANGE

            ELSE
               XCEN = 0.5*( X1 + X2 )
               X1 = XCEN - 0.5 * XRANGE
               X2 = XCEN + 0.5 * XRANGE
            END IF

*  For wide windows, reduce the height of the window specified by FRACTION
*  alone.
         ELSE

*  Get the required height.
            YRANGE = ( X2 - X1 ) / ASPECT

*  Adjust the upper and/or lower Y bound, depending on the justification.
            IF( JUST( 1:1 ) .EQ. 'B' .OR.
     :          JUST( 1:1 ) .EQ. 'b' ) THEN
               Y2 = Y1 + YRANGE

            ELSE IF( JUST( 1:1 ) .EQ. 'T' .OR.
     :               JUST( 1:1 ) .EQ. 't' ) THEN
               Y1 = Y2 - YRANGE

            ELSE
               YCEN = 0.5*( Y1 + Y2 )
               Y1 = YCEN - 0.5 * YRANGE
               Y2 = YCEN + 0.5 * YRANGE
            END IF

         END IF

      END IF

*  Double check to prevent picture-out-of-bounds errors.
      X1 = MIN( MAX( XLOW, X1 ), XHIGH )
      Y1 = MIN( MAX( YLOW, Y1 ), YHIGH )
      X2 = MIN( MAX( XLOW, X2 ), XHIGH )
      Y2 = MIN( MAX( YLOW, Y2 ), YHIGH )

 999  CONTINUE

      END
