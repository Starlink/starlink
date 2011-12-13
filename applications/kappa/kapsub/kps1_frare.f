      SUBROUTINE KPS1_FRARE( JUST, FRACT, ASPECT, X1, X2, Y1, Y2,
     :                       STATUS )
*+
*  Name:
*     KPS1_FRARE

*  Purpose:
*     Defines a region within the current SGS zone.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL KPS1_FRARE( JUST, FRACT, ASPECT, X1, X2, Y1, Y2, STATUS )

*  Description:
*     This subroutine defines a two-dimensional region in the current
*     SGS zone.  The region is defined by the given position
*     justification, and the linear fraction along each axis of the
*     current SGS zone; and an aspect ratio.  The linear fraction is
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
*        the current zone.
*     ASPECT  = REAL (Given)
*        The aspect ratio of the new region (x/y).  If the value is
*        negative, no aspect-ratio constraint is applied to define the
*        new region.
*     X1 = REAL (Returned)
*        Lower x world co-ordinate in the current zone of the new
*        region.
*     X2 = REAL (Returned)
*        Upper x world co-ordinate in the current zone of the new
*        region.
*     Y1 = REAL (Returned)
*        Lower y world co-ordinate in the current zone of the new
*        region.
*     Y2 = REAL (Returned)
*        Upper y world co-ordinate in the current zone of the new
*        region.
*     STATUS  = INTEGER (Given and Returned)
*        Global status value.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     Get bounds of the zone, the position of its centre and its size
*     Get the vertical limit from the code using the zone bounds,
*       reporting an error should the code be invalid
*     Get the horizontal limit from the code using the zone bounds,
*       reporting an error should the code be invalid
*     End

*  Prior Requiremnts:
*     There must be an SGS workstation open.

*  Copyright:
*     Copyright (C) 1989, 1993 Science & Engineering Research Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1989 June 25 (MJC):
*       Original version.
*     1989 November 27 (MJC):
*       More clamping on the picture limits.
*     1993 August 19 (MJC):
*        Renamed from FRARE to KPS1_FRARE.  Added ASPECT argument and
*        made FRACT a two-element vector.
*     {enter_further_changes_here}

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
      INTEGER IZONE             ! Input SGS zone
      REAL XCEN                 ! X centre of the current zone
      REAL XHIGH                ! X upper bound of the current zone
      REAL XLOW                 ! X lower bound of the current zone
      REAL XM                   ! Frame zone size in x (dummy)
      REAL XRANGE               ! X size of the current zone in world
      REAL XS1                  ! X lower bound of zone with specified
                                ! shape
      REAL XS2                  ! X upper bound of zone with specified
                                ! shape
      REAL YCEN                 ! Y centre of the current zone
      REAL YHIGH                ! Y upper bound of the current zone
      REAL YLOW                 ! Y lower bound of the current zone
                                ! co-ordinates
      REAL YM                   ! Frame zone size in y (dummy)
      REAL YRANGE               ! Y size of the current zone in world
                                ! co-ordinates
      REAL YS1                  ! Y lower bound of zone with specified
                                ! shape
      REAL YS2                  ! Y upper bound of zone with specified
                                ! shape
      INTEGER ZONE1             ! Temporary SGS zone (after fractional
                                ! size applied)
      INTEGER ZONE2             ! Temporary SGS zone (after aspect ratio
                                ! applied)

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the bounds of the current zone.
      CALL SGS_IZONE( XLOW, XHIGH, YLOW, YHIGH, XM, YM )

*  Find the co-ordinates of the centre and the height and width of the
*  zone.
      XCEN = 0.5 * ( XLOW + XHIGH )
      YCEN = 0.5 * ( YLOW + YHIGH )
      XRANGE = XHIGH - XLOW
      YRANGE = YHIGH - YLOW

*  Get the horizontal limits depending on the justification code.  L
*  for left, R for right, and C for centre.
      IF ( JUST( 2:2 ) .EQ. 'L' .OR. JUST( 2:2 ) .EQ. 'l' ) THEN
         X1 = XLOW
         X2 = XLOW + FRACT( 1 ) * XRANGE

      ELSE IF ( JUST( 2:2 ) .EQ. 'R' .OR. JUST( 2:2 ) .EQ. 'r' ) THEN
         X1 = XHIGH - FRACT( 1 ) * XRANGE
         X2 = XHIGH

      ELSE IF ( JUST( 2:2 ) .EQ. 'C' .OR. JUST( 2:2 ) .EQ. 'c' ) THEN
         X1 = XCEN - 0.5 * FRACT( 1 ) * XRANGE
         X2 = XCEN + 0.5 * FRACT( 1 ) * XRANGE

*  Report an error when the code is not recognised.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'JUST', JUST )
         CALL ERR_REP( 'KPS1_FRARE_INVCD',
     :     'Probable programming error: invalid justification code '/
     :     /'^JUST used to define a new region.', STATUS )
         GOTO 999
      END IF

*  Get the vertical limits depending on the justification code.  B for
*  bottom, T for top, and C for centre.
      IF ( JUST( 1:1 ) .EQ. 'B' .OR. JUST( 1:1 ) .EQ. 'b' ) THEN
         Y1 = YLOW
         Y2 = YLOW + FRACT( 2 ) * YRANGE

      ELSE IF ( JUST( 1:1 ) .EQ. 'T' .OR. JUST( 1:1 ) .EQ. 't' ) THEN
         Y1 = YHIGH - FRACT( 2 ) * YRANGE
         Y2 = YHIGH

      ELSE IF ( JUST( 1:1 ) .EQ. 'C' .OR. JUST( 1:1 ) .EQ. 'c' ) THEN
         Y1 = YCEN - 0.5 * FRACT( 2 ) * YRANGE
         Y2 = YCEN + 0.5 * FRACT( 2 ) * YRANGE

*  Report an error when the code is not recognised.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'JUST', JUST )
         CALL ERR_REP( 'KPS1_FRARE_INVCD',
     :     'Probable programming error: invalid justification code '/
     :     /'^JUST used to define a new region.', STATUS )
      END IF

*  Check to prevent picture-out-of-bounds errors.
      X1 = MIN( MAX( XLOW, X1 ), XHIGH )
      Y1 = MIN( MAX( YLOW, Y1 ), YHIGH )
      X2 = MIN( MAX( XLOW, X2 ), XHIGH )
      Y2 = MIN( MAX( YLOW, Y2 ), YHIGH )

*  Decide whether to apply an aspect-ratio constraint.
      IF ( ASPECT .GT. 0.0 ) THEN

*  Remember the current zone.
         CALL SGS_ICURZ( IZONE )

*  Define a new temporary zone to encompass the fraction region.
         CALL SGS_ZONE( X1, X2, Y1, Y2, ZONE1, STATUS )

*  Define a new temporary zone of the desired aspect ratio justified
*  as defined.
         CALL SGS_ZSHAP( ASPECT, JUST, ZONE2, STATUS )

*  Get the bounds of this zone (they should be 0.0 to 1.0 along both
*  axes) so this call is defensive.
         CALL SGS_IZONE( XS1, XS2, YS1, YS2, XM, YM )

*  Convert the bounds of the zone of the appropriate shape into
*  co-ordinates of the original zone.
         CALL SGS_TPZ( ZONE2, XS1, YS1, IZONE, X1, Y1, STATUS )
         CALL SGS_TPZ( ZONE2, XS2, YS2, IZONE, X2, Y2, STATUS )

*  Restore the original current zone.
         CALL SGS_SELZ( IZONE, STATUS )

*  Tidy the temporary zones.
         CALL SGS_RELZ( ZONE1 )
         CALL SGS_RELZ( ZONE2 )

*  Double check to prevent picture-out-of-bounds errors.
         X1 = MIN( MAX( XLOW, X1 ), XHIGH )
         Y1 = MIN( MAX( YLOW, Y1 ), YHIGH )
         X2 = MIN( MAX( XLOW, X2 ), XHIGH )
         Y2 = MIN( MAX( YLOW, Y2 ), YHIGH )

      END IF

 999  CONTINUE

      END
