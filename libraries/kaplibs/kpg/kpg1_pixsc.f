      SUBROUTINE KPG1_PIXSC( IWCS, AT, PIXSC, VALUE, UNIT, STATUS )
*+
*  Name:
*     KPG1_PIXSC

*  Purpose:
*     Determines formatted pixel scales at a given grid position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PIXSC( IWCS, AT, PIXSC, VALUE, UNIT, STATUS )

*  Description:
*     This routine determines the scales of the WCS axes in the current
*     Frame of the supplied FrameSet, and formated them into a string.
*     For a specified WCS axis, the returned scale is the WCS axis
*     increment produced by moving a distance of one grid pixel away
*     from the supplied "AT" position, along the WCS axis.

*  Arguments:
*     IWCS = INTEGER (Given)
*        The FrameSet.
*     AT( * ) = DOUBLE PRECISION (Given)
*        The position in GRID co-ordinates at which the pixel scales are
*        to be determined. Note, the pixel scales may vary across the data
*        array if the WCS Mappings are non-linear. The array should have
*        one element for each GRID axis.
*     PIXSC( * ) = DOUBLE PRECISION (Returned)
*        The returned pixel scales. Note, the pixel scale for both celestial
*        longitude and latitude axes are returned as an arc-distance in
*        radians. The array should have one element for each WCS axis.
*     VALUE( * ) = CHARACTER( * ) (Returned)
*        The formatted pixel scales. Celestial axes are formatted as
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
*     Copyright (C) 2007-2011 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAY-2007 (DSB):
*        Original version.
*     5-MAY-2007 (DSB):
*        Check both transformations are available.
*     8-MAY-2007 (DSB):
*        - Correct initialisation of SKYFRAME.
*        - Format celestial axes as arc-seconds, and time axes as seconds.
*     15-MAR-2011 (DSB):
*        Fix bug that caused a scale of zero to be returned for celestial
*        axes inclined at exactly 45 degrees to the X axis.
*     16-MAR-2011 (DSB):
*        Re-written to use KPG1_PXSCL.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER IWCS
      DOUBLE PRECISION AT( * )

*  Arguments Returned:
      DOUBLE PRECISION PIXSC( * )
      CHARACTER VALUE( * )*( * )
      CHARACTER UNIT( * )*( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ATTR*20
      CHARACTER DOM*30
      INTEGER FWCS
      INTEGER I
      INTEGER IAT
      INTEGER NWCS
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the numerical pixel scales.
      CALL KPG1_PXSCL( IWCS, AT, PIXSC, STATUS )

*  Get a pointer to the current (i.e. WCS) Frame, and get the number of
*  WCS axes.
      FWCS = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
      NWCS = AST_GETI( FWCS, 'Naxes', STATUS )

*  Now loop round each WCS axis.
      DO I = 1, NWCS

*  If the pixel scale for the current WCS axis was found, format it. */
         IF( PIXSC( I ) .NE. AST__BAD ) THEN

*  Get the Domain of the primary Frame defining the current axis.
            ATTR = 'Domain('
            IAT = 7
            CALL CHR_PUTI( I, ATTR, IAT )
            CALL CHR_APPND( ')', ATTR, IAT )
            DOM = AST_GETC( FWCS, ATTR( : IAT ), STATUS )

*  If this is a celestial axis, we convert from radians to arc-seconds
*  and format with a fixed format specifier.
            IF( DOM .EQ. 'SKY' ) THEN
               WRITE( VALUE( I ), '(G15.6)' ) PIXSC( I )*
     :                                        AST__DR2D*3600.0
               CALL CHR_LDBLK( VALUE( I ) )
               UNIT( I ) = 'arc-sec'

*  If this is time, we ignore the Format since it may be set to format as
*  a  calendar date.
            ELSE IF( DOM .EQ. 'TIME' ) THEN
               WRITE( VALUE( I ), '(G15.6)' ) PIXSC( I )
               CALL CHR_LDBLK( VALUE( I ) )

               ATTR = 'Unit('
               IAT = 5
               CALL CHR_PUTI( I, ATTR, IAT )
               CALL CHR_APPND( ')', ATTR, IAT )
               UNIT( I ) = AST_GETC( FWCS, ATTR( : IAT ), STATUS )

*  All other axes are formatted using their own Format attribute.
            ELSE
               VALUE( I ) = AST_FORMAT( FWCS, I, PIXSC( I ), STATUS )

               ATTR = 'Unit('
               IAT = 5
               CALL CHR_PUTI( I, ATTR, IAT )
               CALL CHR_APPND( ')', ATTR, IAT )
               UNIT( I ) = AST_GETC( FWCS, ATTR( : IAT ), STATUS )
            END IF

         ELSE
            VALUE( I ) = '<undefined>'
            UNIT( I ) = ' '
         END IF
      END DO

*  Free resources
      CALL AST_ANNUL( FWCS, STATUS )

      END
