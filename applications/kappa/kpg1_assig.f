      SUBROUTINE KPG1_ASSIG( IWCS, NDIM, LBND, UBND, STATUS )
*+
*  Name:
*     KPG1_ASSIG

*  Purpose:
*     Ensure that the Current Frame from an NDF WCS FrameSet has no 
*     insignificant axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASSIG( IWCS, NDIM, LBND, UBND, STATUS )

*  Description:
*     This routine looks for insignificant axes in the Current Frame of
*     the supplied WCS FrameSet (an axis is insignificant if all pixels
*     within the NDF pixel array has the same position on the axis). If
*     any insignificant axes are found, a new Frame is added to the
*     FrameSet containing only the significant axes from the Current Frame.
*     This new Frame is added into the FrameSet and becomes the new
*     Current Frame. The PermMap which connects it to the original
*     Current Frame assigns the correct constant values to the
*     insignificant axes whn used in the inverse direction.

*  Arguments:
*     IWCS = INTEGER (Given)
*        The WCS FrameSet from the NDF, as returned by KPG1_GTWCS.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the NDF, as returned by NDF_BOUND.
*     LBND( NDIM ) = INTEGER (Returned)
*        The lower pixel index bounds of the NDF, as returned by NDF_BOUND.
*     UBND( NDIM ) = INTEGER (Returned)
*        The upper pixel index bounds of the NDF, as returned by NDF_BOUND.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JUL-1999 (DSB):
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
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER IWCS
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      DOUBLE PRECISION EPS       ! Max range for insignificant axes
      PARAMETER ( EPS = 100*VAL__EPSD )

*  Local Variables:
      CHARACTER TTL*80           ! Title
      DOUBLE PRECISION AXVAL( NDF__MXDIM )! Constant axis values
      DOUBLE PRECISION CLBND     ! Lower Current Frame bounds
      DOUBLE PRECISION CUBND     ! Upper Current Frame bounds
      DOUBLE PRECISION GLBND( NDF__MXDIM )! Lower GRID bounds
      DOUBLE PRECISION GUBND( NDF__MXDIM )! Upper GRID bounds
      DOUBLE PRECISION XL( NDF__MXDIM )! GRID coords at CLBND
      DOUBLE PRECISION XU( NDF__MXDIM )! GRID coords at CUBND
      INTEGER DUMMY              ! Unused PermMap 
      INTEGER INPRM( NDF__MXDIM )! Input axis permutation array
      INTEGER J                  ! Axis index
      INTEGER LTTL               ! Used length of TTL
      INTEGER MAP                ! GRID -> Current Frame Mapping
      INTEGER NAXC               ! No. of axes in original Current Frame
      INTEGER NEWCUR             ! Pointer to new Current Frame
      INTEGER NINSIG             ! No. of insignificant axes in Current Frame
      INTEGER NSIG               ! No. of significant axes in Current Frame
      INTEGER OUTPRM( NDF__MXDIM )! Output axis permutation array
      INTEGER PMAP               ! PermMap connecting original and new Frames
      LOGICAL VERB               ! Display extra messages?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  See if we are running in verbose mode.
      CALL KPG1_VERB( VERB, STATUS )

*  Store the bounds of the box in GRID co-ordinates. The box extends
*  between the centre of the corner pixels, and so will cover a range of zero
*  on an insignificant pixel axis (i.e. an axis spanning only a single
*  pixel).
      DO J = 1, NDIM
         GLBND( J ) = 1.0      
         GUBND( J ) = DBLE( UBND( J ) - LBND( J ) + 1 )
      END DO

*  Get the the Mapping from GRID (Base) Frame to the NDFs Current Frame, 
*  and simplify it.
      MAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT,
     :                                    STATUS ), STATUS )

*  Store the number of outputs produced by this Mapping (this equals the
*  number of axes in the current Frame of the FrameSet).
      NAXC = AST_GETI( MAP, 'NOUT', STATUS )

*  Initialise the number of significant and insignificant axes found so
*  far to zero.
      NSIG = 0
      NINSIG = 0

*  Find the bounding box which encloses the pixel grid in the Current
*  Frame. Do one axis at a time.
      DO J = 1, NAXC
         CALL AST_MAPBOX( MAP, GLBND, GUBND, .TRUE., J, CLBND, CUBND, 
     :                    XL, XU, STATUS ) 

*  If this axis covers a zero range (i.e. if it is insignificant)...
         IF( ABS( CUBND - CLBND ) .LE. 
     :       EPS*MAX( ABS( CUBND ), ABS( CLBND ) ) ) THEN 

*  Increment the number of insignificant axes found so far.
            NINSIG = NINSIG + 1

*  Indicate that the PermMap input axis which connects to axis J of 
*  the NDFs Current Frame should be fed a constant value when the PermMap
*  is used in the inverse direction. Store the (negative) index of the
*  constant, and store the constant itself in AXVAL.
            INPRM( J ) = -NINSIG
            AXVAL( NINSIG ) = 0.5*( CUBND + CLBND )

*  Warn the user about insignificant axes in verbose mode.
            IF( VERB ) THEN
               CALL MSG_SETI( 'J', J )
               CALL MSG_OUT( 'KPG1_ASSIG_MSG1', 'Ignoring axis ^J of '//
     :                       'the Current co-ordinate Frame.', STATUS )
            END IF

*  If the axis is significant, increment the number of significant axes
*  found so far, and set up a direct connection between the corresponding
*  input and output axes of the PermMap.
         ELSE
            NSIG = NSIG + 1
            INPRM( J ) = NSIG
            OUTPRM( NSIG ) = J
         END IF            

      END DO

*  If any insignificant axes were found...
      IF( NINSIG .GT. 0 ) THEN

*  Create a PermMap which connects the full Current Frame (with NAXC axes)
*  to the cut-down Frame (with NSIG axes).
         PMAP = AST_PERMMAP( NAXC, INPRM, NSIG, OUTPRM, AXVAL, ' ', 
     :                       STATUS )

*  Create a new Frame by picking the significant axes from the original
*  Current Frame. We do not use the returned PermMap (DUMMY) because it
*  assigned AST__BAD values to the unconnected axes, instead of the
*  correct constant axis values.
         NEWCUR = AST_PICKAXES( IWCS, NSIG, OUTPRM, DUMMY, STATUS )

*  If the original Current Frame is a CmpFrame, the Frame created from
*  the above call to AST_PICKAXES may not have inherited its Title. If
*  the Frame created above has no Title, but the original Frame had, then
*  copy the original Frame's Title to the new Frame.
         IF( AST_TEST( IWCS, 'TITLE', STATUS ) .AND.
     :       .NOT. AST_TEST( NEWCUR, 'TITLE', STATUS ) ) THEN
            TTL = AST_GETC( IWCS, 'TITLE', STATUS )
            LTTL = MAX( 1, CHR_LEN( TTL ) )
            CALL AST_SETC( NEWCUR, 'TITLE', TTL( : LTTL ), STATUS )
         END IF

*  Add this new Frame into the FrameSet. It becomes the Current Frame.
         CALL AST_ADDFRAME( IWCS, AST__CURRENT, PMAP, NEWCUR, STATUS )

      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
