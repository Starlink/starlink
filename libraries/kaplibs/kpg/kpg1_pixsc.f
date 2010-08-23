      SUBROUTINE KPG1_PIXSC( IWCS, AT, PIXSC, VALUE, UNIT, STATUS )
*+
*  Name:
*     KPG1_PIXSC

*  Purpose:
*     Determines pixel scales at a given grid position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PIXSC( IWCS, AT, PIXSC, VALUE, UNIT, STATUS )

*  Description:
*     This routine determines the scales of the WCS axes in the current
*     Frame of the supplied FrameSet. For a specified WCS axis, the
*     returned scale is the WCS axis increment produced by moving a
*     distance of one grid pixel away from the supplied "AT" position,
*     along the WCS axis.

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
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
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
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL

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
      DOUBLE PRECISION ATWCS( NDF__MXDIM )
      DOUBLE PRECISION DPIX
      DOUBLE PRECISION DWCS
      DOUBLE PRECISION IN( 2, NDF__MXDIM )
      DOUBLE PRECISION OUT( 2, NDF__MXDIM )
      DOUBLE PRECISION Q( NDF__MXDIM )
      DOUBLE PRECISION QGRID( NDF__MXDIM )
      DOUBLE PRECISION XIN( 2 )
      DOUBLE PRECISION XOUT( 2 )
      INTEGER FGRID
      INTEGER FWCS
      INTEGER I
      INTEGER IAT
      INTEGER MAP
      INTEGER NPIX
      INTEGER NWCS
      INTEGER OUTAX( NDF__MXDIM )
      INTEGER OMAP
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of grid axes.
      NPIX = AST_GETI( IWCS, 'Nin', STATUS )

*  Get the number of WCS axes.
      NWCS = AST_GETI( IWCS, 'Nout', STATUS )

*  Get pointers to the base and current Frames, and the base to current
*  Mapping.
      FGRID = AST_GETFRAME( IWCS, AST__BASE, STATUS )
      FWCS = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
      MAP = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT, STATUS )

*  Check the Mapping has an inverse transformation.
      IF( AST_GETL( MAP, 'TranInverse', STATUS ) ) THEN

*  Store the supplied AT position and another point that is 1 pixel away
*  from AT along each grid axis.
         DO I = 1, NPIX
            IN( 1, I ) = AT( I )
            IN( 2, I ) = AT( I ) + 1.0
         END DO

*  Transform them into WCS coords.
         CALL AST_TRANN( MAP, 2, NPIX, 2, IN, .TRUE., NWCS, 2, OUT,
     :                   STATUS )

*  Save a copy of the WCS co-ordinates at the AT point. Initialise Q to
*  be the same as the AT point.
         DO I = 1, NWCS
            ATWCS( I ) = OUT( 1, I )
            Q( I ) = ATWCS( I )
         END DO

*  Now loop round each WCS axis.
         DO I = 1, NWCS

*  Get the co-ordinates of a point that is a small distance away from AT
*  along the current WCS axis.
            Q( I ) = OUT( 2, I )

*  Find the geodesic distance in the WCS frame between this point and the
*  AT point.
            DWCS = AST_DISTANCE( FWCS, ATWCS, Q, STATUS )

*  Transform the outlying WCS positions back into GRID coords.
            CALL AST_TRANN( MAP, 1, NWCS, 1, Q, .FALSE., NPIX, 1, QGRID,
     :                      STATUS )

*  Find the distance between the two points in the GRID frame.
            DPIX = AST_DISTANCE( FGRID, AT, QGRID, STATUS )

*  Find and return the axis scale.
            IF( DWCS .NE. AST__BAD .AND. DPIX .NE. AST__BAD .AND.
     :          DPIX .NE. 0.0 ) THEN
               PIXSC( I ) = DWCS/DPIX

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
     :                                           AST__DR2D*3600.0
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
               PIXSC( I ) = AST__BAD
               VALUE( I ) = '<undefined>'
               UNIT( I ) = ' '
            END IF

*  Reset Q back to the AT point.
            Q( I ) = ATWCS( I )

         END DO

*  If the Mapping is missing an inverse transformation, we may still be
*  able to deal with WCS axes that correspond with a single pixel axis.
      ELSE

         DO I = 1, NWCS

*  Initialise the scale to unknwon.
            PIXSC( I ) = AST__BAD
            VALUE( I ) = '<undefined>'
            UNIT( I ) = ' '

*  Attempt to split off the current axis form the Mapping
            CALL AST_MAPSPLIT( MAP, 1, I, OUTAX, OMAP, STATUS )
            IF( OMAP .NE. AST__NULL ) then
               IF( AST_GETI( OMAP, 'Nout', STATUS ) .EQ. 1 ) THEN

*  Transform a one pixel gap into WCS coords.
                  XIN( 1 ) = AT( 1 )
                  XIN( 2 ) = AT( I ) + 1.0
                  CALL AST_TRAN1( OMAP, 2, XIN, .TRUE., XOUT, STATUS )

*  Find and return the axis scale.
                  IF( XOUT( 1 ) .NE. AST__BAD .AND.
     :                XOUT( 2 ) .NE. AST__BAD ) THEN
                     PIXSC( I ) = ABS( XOUT( 2 ) - XOUT( 1 ) )


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
     :                                           AST__DR2D*3600.0
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
                        UNIT( I ) = AST_GETC( FWCS, ATTR( : IAT ),
     :                                        STATUS )

*  All other axes are formatted using their own Format attribute.
                     ELSE
                         VALUE( I ) = AST_FORMAT( FWCS, I, PIXSC( I ),
     :                                            STATUS )

                        ATTR = 'Unit('
                        IAT = 5
                        CALL CHR_PUTI( I, ATTR, IAT )
                        CALL CHR_APPND( ')', ATTR, IAT )
                        UNIT( I ) = AST_GETC( FWCS, ATTR( : IAT ),
     :                                        STATUS )
                     END IF

                  END IF
              END IF
              CALL AST_ANNUL( OMAP, STATUS )
           END IF
        END DO

      END IF

*  Free resources
      CALL AST_ANNUL( FGRID, STATUS )
      CALL AST_ANNUL( FWCS, STATUS )
      CALL AST_ANNUL( MAP, STATUS )

      END
