      SUBROUTINE KPG1_SCALE( IWCS, LBND, UBND, SCALE, VALUE, UNIT,
     :                       STATUS )
*+
*  Name:
*     KPG1_SCALE

*  Purpose:
*     Determines nominal WCS axis scales in an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_SCALE( IWCS, LBND, UBND, SCALE, VALUE, UNIT, STATUS )

*  Description:
*     This routine determines (and formats) the nominal scales of the WCS
*     axes in the current Frame of the supplied FrameSet. The scales are
*     determined at a selection of points within the supplied base Frame
*     bounds, and the median scales are returned. At each point, the scale
*     for a specific WCS axis is the WCS axis increment produced by moving
*     a unit distance within the base Frame away from the point, along the
*     WCS axis.

*  Arguments:
*     IWCS = INTEGER (Given)
*        The FrameSet.
*     LBND( * ) = DOUBLE PRECISION (Given)
*        The lower bounds of the region within the base Frame of the
*        supplied FrameSet in which the WCS scales are to be evaluated.
*        The array should have one element for each base Frame axis.
*     UBND( * ) = DOUBLE PRECISION (Given)
*        The upper bounds of the region within the base Frame of the
*        supplied FrameSet in which the WCS scales are to be evaluated.
*        The array should have one element for each base Frame axis.
*     SCALE( * ) = DOUBLE PRECISION (Returned)
*        The returned WCS scales. Note, the scales for both celestial
*        longitude and latitude axes are returned as an arc-distance in
*        radians. The array should have one element for each WCS axis.
*     VALUE( * ) = CHARACTER( * ) (Returned)
*        The formatted WCS scales. Celestial axes are formatted as
*        arc-seconds using a "G15.6" format. Time values are also formatted
*        using G15.6 (the Format attribute in the current WCS Frame is
*        ignored, since it may produce a calendar date), in what ever
*        units are indicated in the current Frame. Other types of
*        axes (including spectral axes) are formatted using the axis Format
*        attribute in the current WCS Frame. The array should have one
*        element for each WCS axis. Each element of the array should be at
*        least 15 characters long. The returned text is left justified.
*     UNIT( * ) = CHARACTER( * ) (Returned)
*        Units strings that describe the values returned in VALUES. The
*        array should have one element for each WCS axis.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-MAR-2011 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER IWCS
      DOUBLE PRECISION LBND( * )
      DOUBLE PRECISION UBND( * )

*  Arguments Returned:
      DOUBLE PRECISION SCALE( * )
      CHARACTER VALUE( * )*( * )
      CHARACTER UNIT( * )*( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXPNT
      PARAMETER ( MAXPNT = 3**NDF__MXDIM )

*  Local Variables:
      CHARACTER ATTR*20
      CHARACTER DOM*30
      DOUBLE PRECISION STEP( NDF__MXDIM )
      DOUBLE PRECISION CEN( NDF__MXDIM )
      DOUBLE PRECISION AT( NDF__MXDIM )
      DOUBLE PRECISION TSCALE( MAXPNT, NDF__MXDIM )
      DOUBLE PRECISION PXSCL( NDF__MXDIM )
      INTEGER FCURR
      INTEGER I
      INTEGER IAT
      INTEGER IPNT
      INTEGER NBASE
      INTEGER NCURR
      INTEGER NELUSE
      INTEGER NPNT
      INTEGER OFF( NDF__MXDIM )
      LOGICAL INCR
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a pointer to the current (WCS) Frame.
      FCURR = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Get the number of base Frame axes.
      NBASE = AST_GETI( IWCS, 'Nin', STATUS )

*  Get the number of current Frame (WCS) axes.
      NCURR = AST_GETI( IWCS, 'Nout', STATUS )

*  Find the central position in the supplied region, and the steps to
*  offset by along each axis. Using a step of half the array width would
*  put the test points on the edge of the array, which may be dangerous
*  and/or unrepresentative. So use steps of 0.3 of the array width.
      DO I = 1, NBASE
         CEN( I ) = 0.5*( UBND( I ) + LBND( I ) )
         STEP( I ) = 0.3*( UBND( I ) - LBND( I ) )
      END DO

*  For each pixel axis, the OFF array holds the offset of the current
*  test point from the CEN point. Initialise the OFF array so that the
*  current test point is the CEN point.
      DO I = 1, NBASE
         OFF( I ) = 0
      END DO

*  Get the total number of test points. Each axis has 3 test offsets, +STEP,
*  0 and -STEP.
      NPNT = 3**NBASE

*  Loop round all test points.
      DO IPNT = 1, NPNT

*  Get the base Frame coords at the next test point. Set the INCR flag so
*  that the offset for axis 1 is always incremented.
         INCR = .TRUE.
         DO I = 1, NBASE
            AT( I ) = CEN( I ) + OFF( I )*STEP( I )

*  If required, increment the pixel offset for the current axis. These
*  offsets are used in the order zero, +1, -1.
            IF( INCR ) THEN
               OFF( I ) = OFF( I ) + 1

*  Assume we wont need to increment any other offsets.
               INCR = .FALSE.

*  When we reach an offset of +2, change it to -1.
               IF( OFF( I ) .EQ. 2 ) THEN
                  OFF( I ) = -1

*  When we come back to zero offset, it is time to increment the offset on
*  the next axis.
               ELSE IF( OFF( I ) .EQ. 0 ) THEN
                  INCR = .TRUE.
               END IF

            END IF

         END DO

*  Evaluate the pixel scales at the current test point.
         CALL KPG1_PXSCL( IWCS, AT, PXSCL, STATUS )

*  Store in an array in the correct order for use by KPG1_MEDUD.
         DO I = 1, NCURR
            TSCALE( IPNT, I ) = PXSCL( I )
         END DO

      END DO

*  Find and store the returned values for each WCS axis.
      DO I = 1, NCURR

*  Initialise the returned strings.
         VALUE( I ) = '<undefined>'
         UNIT( I ) = ' '

*  Find the median scale.
         CALL KPG1_MEDUD( .TRUE., NPNT, TSCALE( 1, I ), SCALE( I ),
     :                    NELUSE, STATUS )

*  If the scale is defined, format it.
         IF( SCALE( I ) .NE. AST__BAD ) THEN

*  Get the Domain of the primary Frame defining the current axis.
            ATTR = 'Domain('
            IAT = 7
            CALL CHR_PUTI( I, ATTR, IAT )
            CALL CHR_APPND( ')', ATTR, IAT )
            DOM = AST_GETC( FCURR, ATTR( : IAT ), STATUS )

*  If this is a celestial axis, we convert from radians to arc-seconds
*  and format with a fixed format specifier.
            IF( DOM .EQ. 'SKY' ) THEN
               WRITE( VALUE( I ), '(G15.6)' ) SCALE( I )*AST__DR2D
     :                                        *3600.0
               CALL CHR_LDBLK( VALUE( I ) )

               UNIT( I ) = 'arc-sec'

*  If this is time, we ignore the Format since it may be set to format as
*  a  calendar date.
            ELSE IF( DOM .EQ. 'TIME' ) THEN
               WRITE( VALUE( I ), '(G15.6)' ) SCALE( I )
               CALL CHR_LDBLK( VALUE( I ) )

               ATTR = 'Unit('
               IAT = 5
               CALL CHR_PUTI( I, ATTR, IAT )
               CALL CHR_APPND( ')', ATTR, IAT )
               UNIT( I ) = AST_GETC( FCURR, ATTR( : IAT ), STATUS )

*  All other axes are formatted using their own Format attribute.
            ELSE
               VALUE( I ) = AST_FORMAT( FCURR, I, SCALE( I ), STATUS )

               ATTR = 'Unit('
               IAT = 5
               CALL CHR_PUTI( I, ATTR, IAT )
               CALL CHR_APPND( ')', ATTR, IAT )
               UNIT( I ) = AST_GETC( FCURR, ATTR( : IAT ), STATUS )
            END IF
         END IF
      END DO

*  Free resources
      CALL AST_ANNUL( FCURR, STATUS )

      END
