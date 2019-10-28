      SUBROUTINE KPG1_ASSIG8( IWCS, NDIM, LBND, UBND, STATUS )
*+
*  Name:
*     KPG1_ASSIG8

*  Purpose:
*     Ensures that the Current Frame from an NDF WCS FrameSet has no
*     insignificant axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASSIG8( IWCS, NDIM, LBND, UBND, STATUS )

*  Description:
*     This routine is equivalent to KPG1_ASSIG except that arguments
*     LBND and UBND are INTEGER*8 instead of INTEGER. See KPG1_ASSIG
*     for more information.

*  Arguments:
*     IWCS = INTEGER (Given)
*        The WCS FrameSet from the NDF, as returned by KPG1_GTWCS.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the NDF, as returned by NDF_BOUND.
*     LBND( NDIM ) = INTEGER*8 (Returned)
*        The lower pixel index bounds of the NDF, as returned by NDF_BOUND.
*     UBND( NDIM ) = INTEGER*8 (Returned)
*        The upper pixel index bounds of the NDF, as returned by NDF_BOUND.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory
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
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     4-OCT-2019 (DSB):
*        Original version, copied from KPG1_ASSIG and changed to use
*        INTEGER*8 bounds and dimensions.
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
      INTEGER*8 LBND( NDIM )
      INTEGER*8 UBND( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      DOUBLE PRECISION EPS       ! Max range for insignificant axes
      PARAMETER ( EPS = 100*VAL__EPSD )

*  Local Variables:
      CHARACTER ATT*20           ! Attribute name
      CHARACTER TTL*80           ! Title
      DOUBLE PRECISION AXVAL( NDF__MXDIM )! Constant axis values
      DOUBLE PRECISION CLBND     ! Lower Current Frame bounds
      DOUBLE PRECISION CUBND     ! Upper Current Frame bounds
      DOUBLE PRECISION GLBND( NDF__MXDIM )! Lower GRID bounds
      DOUBLE PRECISION GUBND( NDF__MXDIM )! Upper GRID bounds
      DOUBLE PRECISION XL( NDF__MXDIM )! GRID coords at CLBND
      DOUBLE PRECISION XU( NDF__MXDIM )! GRID coords at CUBND
      INTEGER DUMMY              ! Unused PermMap
      INTEGER IAT                ! Used length of ATT
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
      CALL KPG1_VERB( VERB, 'KAPPA', STATUS )

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

*  Abort if an error occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Now try to find the bounding box.
         CALL AST_MAPBOX( MAP, GLBND, GUBND, .TRUE., J, CLBND, CUBND,
     :                    XL, XU, STATUS )

*  Report a more informative error if anything went wrong.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )

            ATT = 'Label('
            IAT = 6
            CALL CHR_PUTI( J, ATT, IAT )
            CALL CHR_APPND( ')', ATT, IAT )

            CALL MSG_SETC( 'AX',
     :                     AST_GETC( IWCS, ATT( : IAT ), STATUS ) )

            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPG1_ASSIG8_ERR1', 'Unable to find limits '//
     :                    'for ^AX over the pixel grid. Try using '//
     :                    'WCSFRAME to set the current co-ordinate '//
     :                    'Frame to PIXEL.', STATUS )
            GO TO 999
         END IF

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
               CALL MSG_OUT( 'KPG1_ASSIG8_MSG1', 'Ignoring axis ^J of'//
     :                       ' the Current co-ordinate Frame.', STATUS )
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

*  Report an error if no significant axes were found.
      IF( NSIG .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_ASSIG8_ERR2', 'No significant axes found'//
     :                 ' in the current co-ordinate Frame.', STATUS )

*  If any insignificant axes were found...
      ELSE IF( NINSIG .GT. 0 ) THEN

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

*  Arrive here if an error occurs.
 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

      END
