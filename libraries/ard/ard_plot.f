      SUBROUTINE ARD_PLOT( IGRP, IPLOT, GBOX, REGVAL, STATUS )
*+
*  Name:
*     ARD_PLOT

*  Purpose:
*     Plot the boundary of an ARD description.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD_PLOT( IGRP, IPLOT, GBOX, REGVAL, STATUS )

*  Description:
*     This routine draws a curve marking the boundary of the ARD
*     description supplied within group IGRP. It can also draw a boundary
*     round a given sub-region by supplying a positive value for REGVAL.
*     The ARD description must be 2-dimensional.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group holding the 2-dimensional ARD 
*        description.
*     IPLOT = INTEGER (Given)
*        An AST pointer to a Plot which will be used to draw the boundary.
*        The Plot and the ARD description will be aligned in a suitable
*        common coordinate Frame, present in both the Plot and the WCS
*        FrameSet implied by the ARD description. If no such common Frame 
*        is available, an error is reported.
*     GBOX( 4 ) = REAL (Given)
*        An array giving the position and extent of the plotting area
*        (on the plotting surface of the underlying graphics system)
*        in which graphical output is to appear. This must be specified 
*        in the base (i.e. GRAPHICS) Frame of the supplied Plot. This can
*        be smaller than the area covered by the supplied Plot, in which 
*        case the graphics will be truncated.
*
*        The first pair of values should give the coordinates of the
*        bottom left corner of the plotting area and the second pair
*        should give the coordinates of the top right corner. The
*        coordinate on the horizontal axis should be given first in
*        each pair. 
*     REGVAL = INTEGER (Given and Returned)
*        The index of the region within the ARD description to be outlined.
*        If the value zero is supplied, the entire ARD description is
*        outlined. If a positive value is supplied, then only the region
*        with the specified index is outlined. If a negative value is
*        supplied, then regions with indices greater than or equal to the
*        absolute value are outlined. If the supplied value is not zero, 
*        then REGVAL is modified on return to hold one more than the largest 
*        value used to represent any of the keywords in the ARD description.
*        The supplied value is left unchanged if it zero.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-SEP-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'PRM_PAR'          ! VAL__ constants 
      INCLUDE 'ARD_CONST'        ! ARD private constants 

*  Global Constants:
      INCLUDE 'ARD_COM'          ! ARD common blocks
*        CMN_AWCS = INTEGER (Read and Write)
*           A pointer to the application FrameSet.
*        CMN_ADOM = CHARACTER*40 (Read and Write)
*           The Domain name associated with pixel
*           coordinates in the mask array.

*  Arguments Given:
      INTEGER IGRP
      INTEGER IPLOT
      REAL GBOX( 4 )

*  Arguments Given and Returned:
      INTEGER REGVAL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXPTS             ! Maximum number of positions in each
      PARAMETER ( MAXPTS = 10000 ) ! axis that define the locus of a contour

      INTEGER N                  ! Fixed size of mask array
      PARAMETER( N = 500 )  

*  Local Variables:
      CHARACTER*40 ADOM0         ! Orig. pixel Domain name 
      DOUBLE PRECISION INA( 2 )  ! Input coords of window corner A
      DOUBLE PRECISION INB( 2 )  ! Input coords of window corner B
      DOUBLE PRECISION OUTA( 2 ) ! Output coords of window corner A
      DOUBLE PRECISION OUTB( 2 ) ! Output coords of window corner B
      INTEGER AWCS0              ! Pointer to original application FrameSet.
      INTEGER DIM( 2 )           ! Dimensions of mask array 
      INTEGER GFRM               ! AST pointer to mask grid coords Frame 
      INTEGER ICURR              ! Index of original current Frame
      INTEGER IGRID              ! Index of mask grid coords Frame within JPLOT
      INTEGER IPDONE             ! Pointer to contouring work array
      INTEGER IPMASK             ! Pointer to mask array
      INTEGER IPXY               ! Pointer to contour locus work array
      INTEGER JPLOT              ! AST pointer to modified Plot
      INTEGER LBND( 2 )          ! Lower bounds for mask array 
      INTEGER LBNDE( 2 )         ! Lower bounds of exterior bounding box
      INTEGER LBNDI( 2 )         ! Lower bounds of interior bounding box
      INTEGER PFRM               ! AST pointer to mask pixel coords Frame 
      INTEGER RV                 ! The returned value of REGVAL
      INTEGER UBND( 2 )          ! Lower bounds for mask array 
      INTEGER UBNDE( 2 )         ! Upper bounds of exterior bounding box
      INTEGER UBNDI( 2 )         ! Upper bounds of interior bounding box
      INTEGER WINMAP             ! AST pointer to a WinMap Mapping
      INTEGER XSIZE              ! X dimension of interior box
      INTEGER YSIZE              ! Y dimension of interior box

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Create a work array over which the ARD description will be evaluated. 
*  coordinates are equivalent. This array will be mapped onto the
*  plotting area specified by GBOX.
      LBND( 1 ) = 1
      LBND( 2 ) = 1
      UBND( 1 ) = N
      UBND( 2 ) = N
      DIM( 1 ) = UBND( 1 ) - LBND( 1 ) + 1
      DIM( 2 ) = UBND( 2 ) - LBND( 2 ) + 1
      CALL PSX_CALLOC( DIM( 1 )*DIM( 2 ), '_INTEGER', IPMASK, STATUS )

*  Take a copy of the supplied Plot so that the original is not modified.
      JPLOT = AST_COPY( IPLOT, STATUS )

*  Note the index of the original current Frame in the Plot.
      ICURR = AST_GETI( JPLOT, 'CURRENT', STATUS )       

*  Create a new Frame representing grid coords within the above work
*  array. Give it the Domain ARDGRIDCO so that it can be distinguished
*  from any GRID Frame already in the Plot.
      GFRM = AST_FRAME( 2, 'DOMAIN=ARDGRIDCO', STATUS )

*  Add this Frame into the Plot, connecting it to the base (GRAPHICS)
*  Frame using a WinMap which results in the array covering the specified
*  box within the plot area. It becomes the current Frame.
      INA( 1 ) = DBLE( GBOX( 1 ) )
      INA( 2 ) = DBLE( GBOX( 2 ) )
      INB( 1 ) = DBLE( GBOX( 3 ) )
      INB( 2 ) = DBLE( GBOX( 4 ) )
      OUTA( 1 ) = 0.5D0
      OUTA( 2 ) = 0.5D0
      OUTB( 1 ) = DBLE( DIM( 1 ) ) + 0.5D0
      OUTB( 2 ) = DBLE( DIM( 2 ) ) + 0.5D0
      WINMAP = AST_WINMAP( 2, INA, INB, OUTA, OUTB, ' ', STATUS )
      CALL AST_ADDFRAME( JPLOT, AST__BASE, WINMAP, GFRM, STATUS )

*  Note the index of the ARDGRIDCO Frame. 
      IGRID = AST_GETI( JPLOT, 'CURRENT', STATUS )       

*  Create a new Frame representing pixel coords within the work array. 
*  Give it the Domain ARDPIXCO so that it can be distinguished from 
*  any PIXEL Frame already in the Plot.
      PFRM = AST_FRAME( 2, 'DOMAIN=ARDPIXCO', STATUS )

*  Add this Frame into the Plot, connecting it to the ARDGRIDCO Frame 
*  using a WinMap which performs the required pixel shift.
      INA( 1 ) = 0.5D0
      INA( 2 ) = 0.5D0
      INB( 1 ) = 1.5D0
      INB( 2 ) = 1.5D0
      OUTA( 1 ) = DBLE( LBND( 1 ) - 1 )
      OUTA( 2 ) = DBLE( LBND( 2 ) - 1 )
      OUTB( 1 ) = OUTA( 1 ) + 1.0D0
      OUTB( 2 ) = OUTA( 2 ) + 1.0D0
      WINMAP = AST_WINMAP( 2, INA, INB, OUTA, OUTB, ' ', STATUS )
      CALL AST_ADDFRAME( JPLOT, AST__CURRENT, WINMAP, PFRM, STATUS )

*  Reinstate the original current Frame.
      CALL AST_SETI( JPLOT, 'CURRENT', ICURR, STATUS )

*  Save the original application FrameSet and Domain, and use the modified 
*  Plot as the application FrameSet within ARD_WORK.
      AWCS0 = CMN_AWCS
      CMN_AWCS = JPLOT
      ADOM0 = CMN_ADOM
      CMN_ADOM = 'ARDPIXCO'

*  Fill the work array with integers representing the ARD description.
      RV = 2
      CALL ARD_WORK( IGRP, 2, LBND, UBND, VAL__BADR, .FALSE., 
     :               RV, %VAL( IPMASK ), LBNDI, UBNDI, LBNDE, UBNDE,
     :               STATUS )

*  Re-instate the original application FrameSet and Domain.
      CMN_AWCS = AWCS0
      CMN_ADOM = ADOM0

*  Make the ARDGRIDCO Frame the current Frame, as required by ARD1_CNTDR.
      CALL AST_SETI( JPLOT, 'CURRENT', IGRID, STATUS )

*  If we are contouring below or above the range of region indicies used
*  by the ARD description, skip the contouring since there is nothing to 
*  contour.
      IF( ABS( REGVAL ) .LT. RV ) THEN 

*  Allocate work space.
         XSIZE = UBNDI( 1 ) - LBNDI( 1 ) + 1                          
         YSIZE = UBNDI( 2 ) - LBNDI( 2 ) + 1                          
         CALL PSX_CALLOC( ( XSIZE + 1 ) * ( YSIZE + 1 ), '_LOGICAL', 
     :                    IPDONE, STATUS )
         CALL PSX_CALLOC( 2*MAXPTS, '_DOUBLE', IPXY, STATUS )

*  Contour this array at the specified integer value.
         CALL ARD1_CNTDR( JPLOT, DIM( 1 ), DIM( 2 ), %VAL( IPMASK ), 
     :                    LBNDI( 1 ), LBNDI( 2 ), XSIZE, YSIZE, 
     :                    REGVAL, MAXPTS, %VAL( IPXY ), %VAL( IPDONE ), 
     :                    STATUS )

* Free the work space.
         CALL PSX_FREE( IPXY, STATUS )
         CALL PSX_FREE( IPDONE, STATUS )
      END IF

*  Store the returned region index if required.
      IF( REGVAL .NE. 0 ) REGVAL = RV

*  Release the mask work space.
      CALL PSX_FREE( IPMASK, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
