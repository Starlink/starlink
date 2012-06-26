      SUBROUTINE KPG1_PXSCL( IWCS, AT, PXSCL, STATUS )
*+
*  Name:
*     KPG1_PXSCL

*  Purpose:
*     Determines pixel scales at a given grid position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PXSCL( IWCS, AT, PXSCL, STATUS )

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
*     PXSCL( * ) = DOUBLE PRECISION (Returned)
*        The returned pixel scales. Note, the pixel scale for both celestial
*        longitude and latitude axes are returned as an arc-distance in
*        radians. The array should have one element for each WCS axis.
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
*        Original version, based on earlier verion of KPG1_PIXSC.
*     26-JUN-2012 (DSB):
*        If the grid->wcs Mapping has a PermMap in it, transforming AT
*        into WCS and then back to GRID may not result in the original
*        AT values.  Take account of this by comparing the mapped WCS
*        values with the round-tripped AT values rather than the original
*        AT values when calculating "DPIX".
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
      DOUBLE PRECISION PXSCL( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXPNT
      PARAMETER ( MAXPNT = 3**NDF__MXDIM )

*  Local Variables:
      DOUBLE PRECISION ATGRID( NDF__MXDIM )
      DOUBLE PRECISION ATWCS( NDF__MXDIM )
      DOUBLE PRECISION DAX
      DOUBLE PRECISION DAXMAX
      DOUBLE PRECISION DPIX
      DOUBLE PRECISION DWCS
      DOUBLE PRECISION IN( MAXPNT, NDF__MXDIM )
      DOUBLE PRECISION OFF( NDF__MXDIM )
      DOUBLE PRECISION OUT( MAXPNT, NDF__MXDIM )
      DOUBLE PRECISION Q( NDF__MXDIM )
      DOUBLE PRECISION QGRID( NDF__MXDIM )
      DOUBLE PRECISION XIN( 2 )
      DOUBLE PRECISION XOUT( 2 )
      INTEGER FGRID
      INTEGER FWCS
      INTEGER I
      INTEGER IMAX( NDF__MXDIM )
      INTEGER IPNT
      INTEGER MAP
      INTEGER NPIX
      INTEGER NPNT
      INTEGER NWCS
      INTEGER OMAP
      INTEGER OUTAX( NDF__MXDIM )
      LOGICAL INCR
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of grid axes.
      NPIX = AST_GETI( IWCS, 'Nin', STATUS )

*  Get the number of WCS axes.
      NWCS = AST_GETI( IWCS, 'Nout', STATUS )

*  Initialise the returned scales.
      DO I = 1, NWCS
         PXSCL( I ) = AST__BAD
      END DO

*  Get pointers to the base and current Frames, and the base to current
*  Mapping.
      FGRID = AST_GETFRAME( IWCS, AST__BASE, STATUS )
      FWCS = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
      MAP = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT, STATUS )

*  Check the Mapping has an inverse transformation.
      IF( AST_GETL( MAP, 'TranInverse', STATUS ) ) THEN

*  Store the supplied AT position and a set of neighbouring test points
*  that are all one pixel away from AT along different directions. For
*  each pixel axis, the OFF array holds the pixel offset of the current
*  test point from the AT point. Initialise the OFF array so that teh
*  current test point is the AT point.
         DO I = 1, NPIX
            OFF( I ) = 0.0D0
         END DO

*  Get the total number of test points. Each axis has 3 test offsets, +1,
*  0 and -1.
         NPNT = 3**NPIX

*  Loop round all test points.
         DO IPNT = 1, NPNT

*  For each axis , store the pixel coord at the current test point.
*  Set the INCR flag so that the offset for axis 1 is always incremented.
            INCR = .TRUE.
            DO I = 1, NPIX
               IN( IPNT, I ) = AT( I ) + OFF( I )

*  If required, increment the pixel offset for the current axis. These
*  offsets are used in the order zero, +1, -1.
               IF( INCR ) THEN
                  OFF( I ) = OFF( I ) + 1.0D0

*  Assume we wont need to increment any other offsets.
                  INCR = .FALSE.

*  When we reach an offset of +2, change it to -1.
                  IF( OFF( I ) .EQ. 2.0D0 ) THEN
                     OFF( I ) = -1.0D0

*  When we come back to zero offset, it is time to increment the offset on
*  the next axis.
                  ELSE IF( OFF( I ) .EQ. 0.0D0 ) THEN
                     INCR = .TRUE.
                  END IF

               END IF

            END DO

         END DO

*  Transform them into WCS coords.
         CALL AST_TRANN( MAP, NPNT, NPIX, MAXPNT, IN, .TRUE., NWCS,
     :                   MAXPNT, OUT, STATUS )

*  For each WCS axis, find the point which is furthest away from the AT
*  point.
         DO I = 1, NWCS
            DAXMAX = -1.0D0
            IMAX( I ) = 0

            DO IPNT = 2, NPNT
               DAX = AST_AXDISTANCE( FWCS, I, OUT( 1, I ),
     :                               OUT( IPNT, I ), STATUS )
               IF( DAX .NE. AST__BAD ) THEN
                  DAX = ABS( DAX )
                  IF( DAX .GT. DAXMAX ) THEN
                     DAXMAX = DAX
                     IMAX( I ) = IPNT
                  END IF
               END IF
            END DO

         END DO

*  Save a copy of the WCS co-ordinates at the AT point. Initialise Q to
*  be the same as the AT point.
         DO I = 1, NWCS
            ATWCS( I ) = OUT( 1, I )
            Q( I ) = ATWCS( I )
         END DO

*  Transform the AT position back into GRID coords. This may not be the
*  same as the original AT if the there are any non-reversible PermMaps in
*  the Mapping.
         CALL AST_TRANN( MAP, 1, NWCS, 1, ATWCS, .FALSE., NPIX, 1,
     :                   ATGRID, STATUS )

*  Now loop round each WCS axis.
         DO I = 1, NWCS

*  Get the co-ordinates of a point that is a small distance away from AT
*  along the current WCS axis.
            Q( I ) = OUT( IMAX( I ), I )

*  Find the geodesic distance in the WCS frame between this point and the
*  AT point.
            DWCS = AST_DISTANCE( FWCS, ATWCS, Q, STATUS )

*  Transform the outlying WCS positions back into GRID coords.
            CALL AST_TRANN( MAP, 1, NWCS, 1, Q, .FALSE., NPIX, 1, QGRID,
     :                      STATUS )

*  Find the distance between the two points in the GRID frame.
            DPIX = AST_DISTANCE( FGRID, ATGRID, QGRID, STATUS )

*  Find and return the axis scale.
            IF( DWCS .NE. AST__BAD .AND. DPIX .NE. AST__BAD .AND.
     :          DPIX .NE. 0.0 ) THEN
               PXSCL( I ) = DWCS/DPIX
            END IF

*  Reset Q back to the AT point.
            Q( I ) = ATWCS( I )

         END DO

*  If the Mapping is missing an inverse transformation, we may still be
*  able to deal with WCS axes that correspond with a single pixel axis.
      ELSE

*  Check each WCS axis in turn.
         DO I = 1, NWCS

*  Attempt to split off the current axis from the Mapping
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
                     PXSCL( I ) = ABS( XOUT( 2 ) - XOUT( 1 ) )
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
