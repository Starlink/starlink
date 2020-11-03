      SUBROUTINE ARD1_DRAW2( RINDEX, LBND, UBND, MSKSIZ, TYPE,
     :                       IWCS, NPAR, PAR, IPB, LBINTB, UBINTB,
     :                       STATUS )
*+
*  Name:
*     ARD1_DRAW2

*  Purpose:
*     Initialise a 2D array to hold an interior value at all pixels
*     through which a specified curve passes, and the exterior value
*     at all other points.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_DRAW2( RINDEX, LBND, UBND, MSKSIZ, TYPE, IWCS, NPAR,
*                      PAR, IPB, LBINTB, UBINTB, STATUS )

*  Description:
*     The 2D array B is initialised by setting all values within the
*     supplied interior bounding box to the exterior value 0.
*     All points outside this box already hold exterior values.
*     Interior values are then assigned to all pixels through which the
*     curve specified by the supplied region passes.
*
*     This routine will only handle 2D regions with non-zero volume/area.
*
*     We use the graphics facilities of the AST Plot class. We use
*     special "graphics" functions which "draw" into the B array by
*     assigning RINDEX to the pixels through which the drawn curve
*     passes. Doing it this way means we pick up all the sophisticated
*     adaptive facilities of the Plot class for handling non-linearities
*     and discontinuities in the user->pixel transformation.

*  Arguments:
*     RINDEX = INTEGER (Given)
*        The value to use to represent interior points.
*     LBND( 2 ) = INTEGER (Given)
*        The lower pixel index bounds of the B array.
*     UBND( 2 ) = INTEGER (Given)
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
*     LBINTB( 2 ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        interior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBINTB( 2 ) = INTEGER (Given and Returned)
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
*     3-NOV-2020 (DSB):
*        Test the central pixel in the array to differentiate between
*        regions that enclose the whole array and regions that do not
*        intersect the array. Previously, it was always assumed that the
*        region did not intersect the array.
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
*        CMN_IPPAR = INTEGER (Write)
*           Pointer to na array holding parameter values.
*        CMN_NPARC = INTEGER (Write)
*           The number of parameters in CMN_IPPAR.
*        CMN_FRMC = INTEGER (Write)
*           The user coord Frame.
*        CMN_TYPEC = INTEGER (Write)
*           The region type identifier.
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
      INTEGER LBND( 2 )
      INTEGER UBND( 2 )
      INTEGER MSKSIZ
      INTEGER NPAR
      DOUBLE PRECISION PAR( NPAR )
      INTEGER TYPE
      INTEGER IWCS
      INTEGER IPB

*  Arguments Given and Returned:
      INTEGER LBINTB( 2 )
      INTEGER UBINTB( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL ARD1_INTR          ! Is the point an interior point?
      EXTERNAL ARD1_INTRA
      EXTERNAL ARD1_GLINE
      EXTERNAL ARD1_GATTR
      EXTERNAL ARD1_GCAP

*  Local Variables:
      DOUBLE PRECISION
     :        BBOX( 4 ),         ! The PIXEL bounds of B
     :        PC( 2 ),           ! PIXEL coords at test point
     :        UC( 2 )            ! User coords at test point

      INTEGER
     :        I,                 ! Loop count
     :        IMAP,              ! AST IntraMap identifier
     :        IPLOT,             ! AST Plot identifier
     :        IPPAR              ! Pointer to memory holding parameters

      REAL
     :        GBOX( 4 )          ! The PIXEL bounds of B

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Reset all pixels within the interior bounding box so that they
*  hold exterior values. The pixels outside the interior bounding box
*  already hold exterior values.
      CALL ARD1_BXSET( 2, LBND, UBND, MSKSIZ, 0, LBINTB,
     :                 UBINTB, %VAL( CNF_PVAL( IPB ) ), STATUS )

*  We create an AST IntraMap defining the Mapping from distance along a
*  curve to user coordinates. This IntrMap needs to know how all about
*  the region being drawn. Allocate memory for a copy of the supplied
*  region parameters, and copy them.
      CALL PSX_CALLOC( NPAR, '_DOUBLE', IPPAR, STATUS )
      CALL ARD1_COPYD( NPAR, PAR, %VAL( CNF_PVAL( IPPAR ) ), STATUS )

*  Store information needed by the IntraMap transformation routine
*  in common .
      CMN_TYPEC = TYPE
      CMN_NPARC = NPAR
      CMN_IPPAR = IPPAR
      CMN_FRMC = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Register the IntraMap transformation routine.
      CALL AST_INTRAREG( 'ARDDRAW', 1, 2, ARD1_INTRA, AST__NOINV, ' ',
     :                   ' ', ' ', STATUS )

*  Create the IntraMap.
      IMAP = AST_INTRAMAP( 'ARDDRAW', 1, 2, ' ', STATUS )

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
     :               STATUS )

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

*  Now draw the curve.
      CALL AST_GENCURVE( IPLOT, IMAP, STATUS )

*  If the curve did not intersect the array, it may be either because the
*  array is completely inside the region, or completely outside the region.
*  To distinguish between these two cases, we test the central pixel of
*  the array to see if it is inside the region.
      IF( CMN_LBIBC( 1 ) .EQ. VAL__MAXI .OR.
     :    CMN_LBIBC( 1 ) .EQ. VAL__MINI ) THEN

* Get the PIXEL coords at the centre of the B array.
        PC( 1 ) = 0.5*( LBND( 1 ) - 1 + UBND( 1 ) )
        PC( 2 ) = 0.5*( LBND( 2 ) - 1 + UBND( 2 ) )

*  Transform to user coords.
        CALL AST_TRAN2( IWCS, 1, PC( 1 ), PC( 2 ), .TRUE., UC( 1 ),
     :                  UC( 2 ), STATUS )

*  Test if this position is inside the region. If so, set the returned
*  interior bounding box to be infinite. Otherwise, set it to be null.
        IF( ARD1_INTR( CMN_FRMC, TYPE, 2, NPAR, PAR, UC, .FALSE.,
     :                 STATUS ) ) THEN
           LBINTB( 1 ) = VAL__MAXI
        ELSE
           LBINTB( 1 ) = VAL__MINI
        END IF

*  Otherwise, update the returned interior bounding box.
      ELSE
         DO I = 1, 2
            LBINTB( I ) = CMN_LBIBC( I )
            UBINTB( I ) = CMN_UBIBC( I )
         END DO
      END IF

*  Annul the Plot.
      CALL AST_ANNUL( IPLOT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Free memory holding the copy of the supplied region parameters.
      CALL PSX_FREE( IPPAR, STATUS )

      END
