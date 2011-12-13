      SUBROUTINE KPS1_PSWCS( INDF, LBND, UBND, X, Y, STATUS )
*+
*  Name:
*     KPS1_PSWCS

*  Purpose:
*     Add an OFFSET Frame to the WCS component in the output NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PSWCS( INDF, LBND, UBND, X, Y, STATUS )

*  Description:
*     This routine adds an OFFSET Frame into the WCS component in the NDF
*     holding the model PSF (INDF). It is assumed that the centre of the
*     model PSF is at the centre of the output image.
*
*     The Frame added has Domain OFFSET, and becomes the Current Frame.
*     It measures geodesic distance from the centre of the PSF in the
*     direction of the two pixel axes. The output image scale is the
*     same as the input image scale at the supplied star position (PX,PY).

*  Arguments:
*     INDF = INTEGER (Given)
*        Identifier for output NDF.
*     LBND( 2 ) = INTEGER (Given)
*        Lower pixel index bounds in output NDF.
*     UBND( 2 ) = INTEGER (Given)
*        Upper pixel index bounds in output NDF.
*     X( * ) = DOUBLE PRECISION (Given and Returned)
*        Work space. Must have at least
*        MAX( LBND( 1 ) - LBND( 1 ) + 3, LBND( 2 ) - LBND( 2 ) + 3 ) elements.
*     Y( * ) = DOUBLE PRECISION (Given and Returned)
*        Work space. Must have at least
*        MAX( LBND( 1 ) - LBND( 1 ) + 3, LBND( 2 ) - LBND( 2 ) + 3 ) elements.
*     STATUS = INTEGER (Given)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1999 (DSB):
*        Original version.
*     4-MAR-2010 (DSB):
*        Cater for NDFs that contain extra insignificant pixel axes.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER INDF
      INTEGER LBND( 2 )
      INTEGER UBND( 2 )

*  Arguments Given and Returned:
      DOUBLE PRECISION X( * )
      DOUBLE PRECISION Y( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      DOUBLE PRECISION RTOAS     ! Radians to arc-seconds conversion factor
      PARAMETER ( RTOAS = 57.295779513082320876798*3600.0 )

*  Local Variables:
      CHARACTER UNIT*20          ! UNITS string
      DOUBLE PRECISION C( 2 )    ! Cur. Frame co-ords at centre of output image
      DOUBLE PRECISION COUT( 2 ) ! GRID co-ords at centre of output image
      DOUBLE PRECISION D( 2 )    ! Cur. Frame co-ords at first pixel in LutMap
      DOUBLE PRECISION DIST0     ! Distance from first pixel to centre
      DOUBLE PRECISION E( 2 )    ! Cur. Frame co-ords at a pixel in LutMap
      INTEGER CMAP               ! Final combined Mapping
      INTEGER DIMS( NDF__MXDIM ) ! Dimensions of all pixel axes in NDF
      INTEGER FRM                ! Current Frame
      INTEGER I                  ! Index count
      INTEGER INPERM( NDF__MXDIM )! Permutation array for Permmap inputs
      INTEGER IWCS               ! Output WCS FrameSet
      INTEGER LMAP1              ! LutMap for axis 1
      INTEGER LMAP2              ! LutMap for axis 2
      INTEGER MAP                ! Base to Current Mapping
      INTEGER NDIM               ! Total number of pixel axes in NDF
      INTEGER NEWFRM             ! Current Frame for output NDF
      INTEGER NP                 ! Number of points in each LutMap
      INTEGER SDIM( 2 )          ! Significant axis indices
      INTEGER SLBND( 2 )         ! Significant lower bounds for output NDF
      INTEGER SUBND( 2 )         ! Significant upper bounds for output NDF
      LOGICAL SKYFRM             ! Is input Current Frame a SkyFrame?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the current WCS FrameSet from the output NDF. This will have been
*  propagated from the input NDF.
      CALL KPG1_ASGET( INDF, 2, .TRUE., .TRUE., .TRUE., SDIM,
     :                 SLBND, SUBND, IWCS, STATUS )

*  Get the mapping from Base (GRID) Frame to Current Frame.
      MAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT,
     :                                   STATUS ), STATUS )

*  Get a pointer to the Current Frame.
      FRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Find the GRID co-ordinates at the centre of the output NDF (assumed to
*  correspond to the centre of the model PSF).
      COUT( 1 ) = 0.5*DBLE( UBND( 1 ) - LBND( 1 ) + 1 ) + 0.5
      COUT( 2 ) = 0.5*DBLE( UBND( 2 ) - LBND( 2 ) + 1 ) + 0.5

*  Find the corresponding Current Frame position.
      CALL AST_TRAN2( MAP, 1, COUT( 1 ), COUT( 2 ), .TRUE., C( 1 ),
     :                C( 2 ), STATUS )

*  Store the GRID co-ordinates along a uniformly spaced row of points
*  passing through the centre of the output NDF, and parallel to axis 1.
*  The points are 1 pixel apart, and the row extends 1 pixel beyond the
*  edges of the output image. The central point is co-incident with the
*  centre of the output image.
      NP = UBND( 1 ) - LBND( 1 ) + 3
      DO I = 1, NP
         X( I ) = DBLE( I ) - 1.0
         Y( I ) = COUT( 2 )
      END DO

*  Map these input GRID positions into the current Frame.
      CALL AST_TRAN2( MAP, NP, X, Y, .TRUE., X, Y, STATUS )

*  Find the geodesic distance from the centre of the first output pixel in
*  this row to the centre of the image.
      D( 1 ) = X( 1 )
      D( 2 ) = Y( 1 )
      DIST0 = AST_DISTANCE( FRM, D, C, STATUS )

*  Find the geodesic distance from the centre of the output image to each
*  pixel in this row, and store it in array Y.
      DO I = 1, NP
         E( 1 ) = X( I )
         E( 2 ) = Y( I )
         Y( I ) = AST_DISTANCE( FRM, D, E, STATUS ) - DIST0
      END DO

*  If the current Frame is a SkyFrame, the geodesic distances found above
*  will be in radians. We want them in arc-seconds so scale them.
      IF( AST_ISASKYFRAME( FRM, STATUS ) ) THEN
         SKYFRM = .TRUE.

         DO I = 1, NP
            Y( I ) = RTOAS*Y( I )
         END DO

      ELSE
         SKYFRM = .FALSE.
      END IF

*  Create a LutMap giving geodesic distance from the centre as a function of
*  output Grid position for the first axis.
      LMAP1 = AST_LUTMAP( NP, Y, 0.0D0, 1.0D0, ' ', STATUS )

*  Do the same for the second axis...

*  Store the GRID co-ordinates along a column of points passing
*  through the centre of the output NDF. Each point is at the boundary
*  between output pixels, and the points extend across the entire
*  height of the output NDF.
      NP = UBND( 2 ) - LBND( 2 ) + 3
      DO I = 1, NP
         X( I ) = COUT( 1 )
         Y( I ) = DBLE( I ) - 1.0
      END DO

*  Map these input GRID positions into the current Frame.
      CALL AST_TRAN2( MAP, NP, X, Y, .TRUE., X, Y, STATUS )

*  Find the geodesic distance from the centre of the first output pixel in
*  this column to the centre of the image.
      D( 1 ) = X( 1 )
      D( 2 ) = Y( 1 )
      DIST0 = AST_DISTANCE( FRM, D, C, STATUS )

*  Find the geodesic distance from the centre of the output image to each
*  pixel in this column, and store it in array Y.
      DO I = 1, NP
         E( 1 ) = X( I )
         E( 2 ) = Y( I )
         Y( I ) = AST_DISTANCE( FRM, D, E, STATUS ) - DIST0
      END DO

*  If the current Frame is a SkyFrame, the geodesic distances found above
*  will be in radians. We want them in arc-seconds so scale them.
      IF( SKYFRM ) THEN
         DO I = 1, NP
            Y( I ) = RTOAS*Y( I )
         END DO
      END IF

*  Create a LutMap giving geodesic distance from the centre as a function of
*  output Grid position for the second axis.
      LMAP2 = AST_LUTMAP( NP, Y, 0.0D0, 1.0D0, ' ', STATUS )

*  Combine the two 1D LutMaps into a 2D Mapping, from output GRID Frame
*  to geodesic offset from the image centre within the current Frame.
      CMAP = AST_CMPMAP( LMAP1, LMAP2, .FALSE., ' ', STATUS )

*  We now construct a suitable current Frame for the output NDF.
      NEWFRM = AST_FRAME( 2, ' ', STATUS )
      IF( SKYFRM ) THEN
         CALL AST_SETC( NEWFRM, 'UNIT(1)', 'arc-seconds', STATUS )
         CALL AST_SETC( NEWFRM, 'UNIT(2)', 'arc-seconds', STATUS )

      ELSE
         UNIT = AST_GETC( FRM, 'UNIT(1)', STATUS )
         IF( UNIT .NE. ' ' ) THEN
            CALL AST_SETC( NEWFRM, 'UNIT(1)', UNIT( : CHR_LEN( UNIT ) ),
     :                     STATUS )
         END IF

         UNIT = AST_GETC( FRM, 'UNIT(2)', STATUS )
         IF( UNIT .NE. ' ' ) THEN
            CALL AST_SETC( NEWFRM, 'UNIT(2)', UNIT( : CHR_LEN( UNIT ) ),
     :                     STATUS )
         END IF

      END IF

      CALL AST_SETC( NEWFRM, 'LABEL(1)', 'Offset on axis 1', STATUS )
      CALL AST_SETC( NEWFRM, 'LABEL(2)', 'Offset on axis 2', STATUS )
      CALL AST_SETC( NEWFRM, 'DOMAIN', 'OFFSET', STATUS )

*  Determine the total number of pixel axes in the NDF.
      CALL NDF_DIM(  INDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  The NDF may contain more than 2 pixel axes (i.e. there may be some
*  additional insignificant pixel axes that were ignored by KPG1_ASGET).
*  If so we need to modify the Mapping created above to include the
*  ignored pixel axes.
      IF( NDIM .GT. 2 ) THEN

*  Create a PermMap that transforms NDIM-dimensional GRID coords within
*  the NDF into the 2-dimensional GRID coords described by the base Frame
*  in IWCS. Combine this PermMap in series with the Mapping created above.
         DO I = 1, NDIM
            INPERM( I ) = -1
         END DO

         INPERM( SDIM( 1 ) ) = 1
         INPERM( SDIM( 2 ) ) = 2

         CMAP = AST_CMPMAP( AST_PERMMAP( NDIM, INPERM, 2, SDIM, 1.0D0,
     :                                   ' ', STATUS ),
     :                      CMAP, .TRUE., ' ', STATUS )

*  Get the full WCS FrameSet from the NDF, retaining all pixel axes.
         CALL KPG1_GTWCS( INDF, IWCS, STATUS )

      END IF

*  Add the OFFSET Frame into the FrameSet using the Mapping found above.
      CALL AST_ADDFRAME( IWCS, AST__BASE, CMAP, NEWFRM, STATUS )

*  Save the modified FrameSet in the output NDF.
      CALL NDF_PTWCS( IWCS, INDF, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
