      SUBROUTINE ARD1_KDRAW( RINDEX, NDIM, LBND, UBND, MSKSIZ, TYPE,
     :                       IWCS, NPAR, PAR, IPB, LBEXTB, UBEXTB,
     :                       LBINTB, UBINTB, STATUS )
*+
*  Name:
*     ARD1_KDRAW

*  Purpose:
*     Initialise an array to hold an interior value at all pixels
*     through which a specified curve passes, and the exterior value
*     at all other points.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_KDRAW( RINDEX, NDIM, LBND, UBND, MSKSIZ, TYPE, IWCS, NPAR,
*                      PAR, IPB, LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )

*  Description:
*     The array B is initialised by setting all values within the
*     supplied interior bounding box to the exterior value 0.
*     All points outside this box already hold exterior values.
*     Interior values are then assigned to all pixels through which the
*     curve specified by the supplied region passes.
*
*     This routine will only handle regions with zero area/volume.

*  Arguments:
*     RINDEX = INTEGER (Given)
*        The value to use to represent interior points.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the B array.
*     LBND( NDIM ) = INTEGER (Given)
*        The lower pixel index bounds of the B array.
*     UBND( NDIM ) = INTEGER (Given)
*        The upper pixel index bounds of the B array.
*     MSKSIZ = INTEGER (Given)
*        The total number of elements in the B array.
*     TYPE = INTEGER (Given)
*        The symbolic value representing the region type.
*     IWCS = INTEGER (Given)
*        An identifer for an AST FrameSet. The Base Frame should be
*        PIXEL coordinates within the B array. The Current Frame should
*        be user coordinates.
*     NPAR = INTEGER (Given)
*        The size of the PAR array.
*     PAR( NPAR ) = DOUBLE PRECISION (Given)
*        The parameters defining the region.
*     IPB = INTEGER (Given)
*        A pointer to the B array, which should be an array of MSKSIZ
*        integers.
*     LBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        exterior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        exterior points in B.
*     LBINTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        interior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBINTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        interior points in B.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
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
*     15-JUN-2001 (DSB):
*        Original version.
*     15-OCT-2011 (DSB):
*        Register a GRF "capabilities" function with AST before plotting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'AST_PAR'          ! AST_ constants and functions
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants

*  Global Variables:
      INCLUDE 'ARD_COM'          ! ARD common blocks
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
*        CMN_MSKSC = INTEGER (Write)
*           The MSKSIZ value passed to the ARD "drawing" routines.
*        CMN_RNDXC = INTEGER (Write)
*           The RINDEX value passed to the ARD "drawing" routines.
*        CMN_IPBC = INTEGER (Write)
*           Pointer to the B array passed to the ARD "drawing" routines.
*        CMN_LBNDC( 2 ) = INTEGER (Write)
*           The lower bounds of the B array passed to the ARD "drawing"
*           routines.
*        CMN_UBNDC( 2 ) = INTEGER (Write)
*           The upper bounds of the B array passed to the ARD "drawing"
*           routines.
*        CMN_LBIBC( 2 ) = INTEGER (Write)
*           The lower bounds of the interior bounding box passed to the
*           ARD "drawing" routines.
*        CMN_UBIBC( 2 ) = INTEGER (Write)
*           The upper bounds of the interior bounding box passed to the
*           ARD "drawing" routines.

*  Arguments Given:
      INTEGER RINDEX
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER MSKSIZ
      INTEGER NPAR
      DOUBLE PRECISION PAR( NPAR )
      INTEGER TYPE
      INTEGER IWCS
      INTEGER IPB

*  Arguments Given and Returned:
      INTEGER LBEXTB( NDIM )
      INTEGER UBEXTB( NDIM )
      INTEGER LBINTB( NDIM )
      INTEGER UBINTB( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ARD1_GLINE
      EXTERNAL ARD1_GATTR
      EXTERNAL ARD1_GCAP

*  Local Constants:
      INTEGER NLP                ! No. of subdivisions for non-2D LINE
      PARAMETER ( NLP = 500 )

*  Local Variables:
      DOUBLE PRECISION
     :        BBOX( 4 ),         ! The PIXEL bounds of B
     :        START( 2 )         ! Start position for row/col

      INTEGER
     :        I,                 ! Loop count
     :        IPLOT,             ! AST Plot identifier
     :        NWCS,              ! No. of user coord axes
     :        IPW1               ! Pointer to work space

      REAL
     :        GBOX( 4 )          ! The PIXEL bounds of B

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the number of WCS axes.
      NWCS = AST_GETI( IWCS, 'Naxes', STATUS )

*  POINT/PIXEL keywords are easy. Just transform them from user to pixel and
*  mark the pixel in which they fall.
      IF( TYPE .EQ. ARD__POI .OR. TYPE .EQ. ARD__PIX ) THEN
         CALL ARD1_POI2( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR, PAR,
     :                   IWCS, %VAL( CNF_PVAL( IPB ) ),
     :                   LBEXTB, UBEXTB, LBINTB,
     :                   UBINTB, STATUS )

*  Non-2D LINE keywords cannot be handled using AST (see below), so
*  just transform a set of evenly spaced points from user coords to pixel
*  coords and join them up with straight lines. Not nice, but probably no
*  one uses 3D lines anyway.
      ELSE IF( TYPE .EQ. ARD__LIN .AND. ( NDIM .NE. 2 .OR.
     :                                    NWCS .NE. 2 ) ) THEN
         CALL PSX_CALLOC( NDIM*NLP, '_DOUBLE', IPW1, STATUS )
         CALL ARD1_LIN2( RINDEX, NDIM, NWCS, LBND, UBND, MSKSIZ, NPAR,
     :                   PAR, NLP, IWCS, %VAL( CNF_PVAL( IPB ) ),
     :                   LBEXTB, UBEXTB, LBINTB,
     :                   UBINTB, %VAL( CNF_PVAL( IPW1 ) ), STATUS )
         CALL PSX_FREE( IPW1, STATUS )

*  All other cases are handled by the graphics facilities of the AST Plot
*  class (they are all 2D cases). We use special "graphics" functions which
*  "draw" into the B array by assigning RINDEX to the pixels through which
*  the drawn curve passes. Doing it this way means we pick up all the
*  sophisticated adaptive facilities of the Plot class for handling
*  non-linearities and discontinuities in the user->pixel transformation.
      ELSE IF( NDIM .EQ. 2 .AND. NWCS .EQ. 2 ) THEN

*  Initialize the interior bounding box passed in common to the "drawing"
*  routines.
         DO I = 1, 2
            CMN_LBIBC( I ) = VAL__MAXI
            CMN_UBIBC( I ) = VAL__MINI
         END DO

*  Store other items required by the drawing routines.
         CMN_MSKSC = MSKSIZ
         CMN_RNDXC = RINDEX
         CMN_IPBC = IPB
         DO I = 1, 2
            CMN_LBNDC( I ) = LBND( I )
            CMN_UBNDC( I ) = UBND( I )
         END DO

*  A straight line in user coords could conceivably correspond to a curve
*  in pixel coords, potentially with discontinuities. The AST Plot class
*  has facilities for transforming curves from one Frame to another,
*  taking account of any non-linearities and discontinuities. We create
*  a Plot in which "graphics" coordinates corresponds to PIXEL coordinates
*  in the grid, and use ARD1_GLINE as the routine for "plotting" lines.
*  In fact ARD1_GLINE "draws" by storing interior values within pixels in
*  the grid. First store the bounds of the B array in PIXEL coords.

         GBOX( 1 ) = REAL( LBND( 1 ) ) - 1.0
         GBOX( 2 ) = REAL( LBND( 2 ) ) - 1.0
         GBOX( 3 ) = REAL( UBND( 1 ) )
         GBOX( 4 ) = REAL( UBND( 2 ) )

         BBOX( 1 ) = DBLE( GBOX( 1 ) )
         BBOX( 2 ) = DBLE( GBOX( 2 ) )
         BBOX( 3 ) = DBLE( GBOX( 3 ) )
         BBOX( 4 ) = DBLE( GBOX( 4 ) )

*  Create the plot. indicating that graphics are to "drawn" using routines
*  registered by AST_GRFSET.
         IPLOT = AST_PLOT( IWCS, GBOX, BBOX, 'GRF=1', STATUS )

*  Indicate that curves should be drawn to an accuracy of about 0.2 of
*  a pixel.
         CALL AST_SETD( IPLOT, 'TOL', 0.2D0/DBLE(
     :    MIN( UBND( 1 ) - LBND( 1 ) + 1, UBND( 2 ) - LBND( 2 ) + 1 ) ),
     :                  STATUS )

*  Indicate that the Plot class should use ARD1_GCAP to determine the
*  capabilities of the ARD grf functions.
      CALL AST_GRFSET( IPLOT, 'CAP', ARD1_GCAP, STATUS )

*  Indicate that the Plot class should use ARD1_GLINE to "draw" lines
*  into the B array.
         CALL AST_GRFSET( IPLOT, 'LINE', ARD1_GLINE, STATUS )

*  Indicate that the Plot class should use ARD1_GATTR to incquire
*  drawing attributes. In fact, there are no attributes, but AST objects
*  if no routine is supplied.
         CALL AST_GRFSET( IPLOT, 'ATTR', ARD1_GATTR, STATUS )

*  LINE keywords...
         IF( TYPE .EQ. ARD__LIN ) THEN
            CALL AST_CURVE( IPLOT, PAR( 1 ), PAR( 3 ), STATUS )

*  ROW and COLUMN keywords...
         ELSE IF( TYPE .EQ. ARD__ROW .OR. TYPE .EQ. ARD__COL ) THEN

*  We use the AST_GRID method rather than AST_GRIDLINE since AST_GRIDLINE
*  requires us to specify a starting position and length for each row/col,
*  whereas AST_GRID works these out for itself. AST_GRID produces a full
*  annotated grid by default, so first set attribute values which tell
*  AST_GRID not to draw any of the components of the grid.
            CALL AST_SETC( IPLOT, 'LABELLING', 'INTERIOR', STATUS )
            CALL AST_SETL( IPLOT, 'BORDER', .FALSE., STATUS )
            CALL AST_SETL( IPLOT, 'DRAWAXES', .FALSE., STATUS )
            CALL AST_SETL( IPLOT, 'DRAWTITLE', .FALSE., STATUS )
            CALL AST_SETL( IPLOT, 'GRID', .FALSE., STATUS )
            CALL AST_SETL( IPLOT, 'NUMLAB', .FALSE., STATUS )
            CALL AST_SETL( IPLOT, 'TEXTLAB', .FALSE., STATUS )
            CALL AST_SETR( IPLOT, 'MAJTICKLEN', 0.0, STATUS )
            CALL AST_SETR( IPLOT, 'MINTICKLEN', 0.0, STATUS )

*  Now indicate that we want to draw the relevant axis.
            IF( TYPE .EQ. ARD__ROW ) THEN
               CALL AST_SETL( IPLOT, 'DRAWAXES(1)', .TRUE., STATUS )
            ELSE
               CALL AST_SETL( IPLOT, 'DRAWAXES(2)', .TRUE., STATUS )
            END IF

*  Loop round each row/col.
            DO I = 1, NPAR

*  Indicate where the row/col is to be drawn.
               IF( TYPE .EQ. ARD__ROW ) THEN
                  CALL AST_SETD( IPLOT, 'LABELAT(1)', PAR(I), STATUS )
               ELSE
                  CALL AST_SETD( IPLOT, 'LABELAT(2)', PAR(I), STATUS )
               END IF

*  Draw it.
               CALL AST_GRID( IPLOT, STATUS )

            END DO

*  Report an error and abort for any other keyword.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = ARD__INTER
            CALL MSG_SETI( 'TYPE', TYPE )
            CALL ERR_REP( 'ARD1_KDRAW_ERR1', 'Illegal keyword '//
     :                    'identifier (^TYPE) encountered in routine '//
     :                    'ARD1_KDRAW (programming error).', STATUS )
         END IF

*  Update the returned interior bounding box.
         DO I = 1, 2
            LBINTB( I ) = CMN_LBIBC( I )
            UBINTB( I ) = CMN_UBIBC( I )
         END DO

*  Annul the Plot.
         CALL AST_ANNUL( IPLOT, STATUS )

      END IF

*  Indicate that the exterior bounding box is infinite.
      LBEXTB( 1 ) = VAL__MAXI

*  Indicate if the interior bounding box is null.
      IF( LBINTB( 1 ) .EQ. VAL__MAXI ) THEN
         LBINTB( 1 ) = VAL__MINI
      ELSE
         DO I = 1, NDIM
            IF( LBINTB( I ) .GT. UBINTB( I ) ) LBINTB( 1 ) = VAL__MINI
         END DO
      END IF

      END
