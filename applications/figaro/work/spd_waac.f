      SUBROUTINE SPD_WAAC( AXES, TICK, NUML, MAJOR, MINOR,
     :   CHIGHT, TEXT, VIEW, LABSPC, LABEL, STATUS )
*+
*  Name:
*     SPD_WAAC

*  Purpose:
*     Plot a sophisticated box.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WAAC( AXES, TICK, NUML, MAJOR, MINOR,
*        CHIGHT, TEXT, LABSPC, LABEL, STATUS )

*  Description:
*     This is a wrap around PGBOX and PGMTXT to draw a more
*     sophisticated box and labels than single calls to PGBOX and
*     PGLABEL can provide.

*  Arguments:
*     AXES( 4 ) = INTEGER (Given)
*        0 or 1, telling whether the axis is to be drawn. 1 for yes.
*     TICK( 4 ) = INTEGER (Given)
*        -1, 0, or +1, telling whether ticks are to be drawn at all and
*        whether they are inside or outside the box. +1 for outside.
*     NUML( 4 ) = INTEGER (Given)
*        Telling whether numeric labels are to be drawn. 0 for no. The
*        first and third element can be 1 for yes. The second and fourth
*        element can be +1 for horizontal and -1 for vertical labels.
*     MAJOR( 2 ) = REAL (Given)
*        Distance between major tick marks for horizontal and vertical
*        directions. Used only if OVER is zero (false).
*     MINOR( 2 ) = INTEGER (Given)
*        Number of minor tick intervals per major tick interval for
*        horizontal and vertical directions. Used only if OVER is zero
*        (false).
*     CHIGHT = REAL (Given)
*        Character height in PGPLOT units. This is used here to measure
*        the position of text labels inwards from the edge of the
*        labelling area.
*     TEXT( 4 ) = INTEGER (Given)
*        For each axis (bottom, left, top, right), telling whether a
*        text label is to be drawn.
*     VIEW( 4 ) = REAL (Given)
*        The view port for coordinate space.
*     LABSPC( 4 ) = REAL (Given)
*        The fraction of the view surface to be reserved for labels. The
*        elements apply to the bottom, left, top, right in that order.
*        The labelling area is outside the current view port and extends
*        by LABSPC beyond it. The resulting area must be within the
*        current view surface. Text labels will be positioned inwards
*        from the outer edge of the labelling area, not outwards from
*        the view port.
*     LABEL( 4 ) = CHARACTER * ( * ) (Given)
*        The given plot label strings for the bottom, left, top, and
*        right.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     It is not possible to request ticks without drawing the axis
*     itself.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     22 Sep 1994 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER AXES( 4 ), TICK( 4 ), NUML( 4 )
      REAL MAJOR( 2 )
      INTEGER MINOR( 2 )
      REAL CHIGHT
      INTEGER TEXT( 4 )
      REAL VIEW( 4 )
      REAL LABSPC( 4 )
      CHARACTER * ( * ) LABEL( 4 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Volatile Variables:
      REAL RTEMP(  4 )           ! Temporary floating point numbers
      REAL XWIDTH, YWIDTH        ! View surface in millimetre
      REAL ASPECT                ! Width divided by height
      CHARACTER * ( 6 ) BOXOPT   ! Option string for PGBOX

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Plot the box and numeric labels according to box layout
*  requests, separately for each side of the box.
      BOXOPT = '      '
      IF ( AXES(1) .EQ. 1 ) BOXOPT(1:1) = 'B'
      IF ( TICK(1) .NE. 0 ) BOXOPT(2:3) = 'TS'
      IF ( TICK(1) .EQ. 1 ) BOXOPT(4:4) = 'I'
      IF ( NUML(1) .EQ. 1 ) BOXOPT(5:5) = 'N'
      IF ( BOXOPT .NE. ' ' )
     :   CALL PGBOX( BOXOPT, MAJOR(1), MINOR(1),
     :      ' ', MAJOR(2), MINOR(2) )
      BOXOPT = '      '
      IF ( AXES(2) .EQ. 1 ) BOXOPT(1:1) = 'B'
      IF ( TICK(2) .NE. 0 ) BOXOPT(2:3) = 'TS'
      IF ( TICK(2) .EQ. 1 ) BOXOPT(4:4) = 'I'
      IF ( NUML(2) .NE. 0 ) BOXOPT(5:5) = 'N'
      IF ( NUML(2) .EQ. 1 ) BOXOPT(6:6) = 'V'
      IF ( BOXOPT .NE. ' ' )
     :   CALL PGBOX( ' ', MAJOR(1), MINOR(1),
     :      BOXOPT, MAJOR(2), MINOR(2) )
      BOXOPT = '      '
      IF ( AXES(3) .EQ. 1 ) BOXOPT(1:1) = 'C'
      IF ( TICK(3) .NE. 0 ) BOXOPT(2:3) = 'TS'
      IF ( TICK(3) .EQ. 1 ) BOXOPT(4:4) = 'I'
      IF ( NUML(3) .EQ. 1 ) BOXOPT(5:5) = 'M'
      IF ( BOXOPT .NE. ' ' )
     :   CALL PGBOX( BOXOPT, MAJOR(1), MINOR(1),
     :      ' ', MAJOR(2), MINOR(2) )
      BOXOPT = '      '
      IF ( AXES(4) .EQ. 1 ) BOXOPT(1:1) = 'C'
      IF ( TICK(4) .NE. 0 ) BOXOPT(2:3) = 'TS'
      IF ( TICK(4) .EQ. 1 ) BOXOPT(4:4) = 'I'
      IF ( NUML(4) .NE. 0 ) BOXOPT(5:5) = 'M'
      IF ( NUML(4) .EQ. 1 ) BOXOPT(6:6) = 'V'
      IF ( BOXOPT .NE. ' ' )
     :   CALL PGBOX( ' ', MAJOR(1), MINOR(1),
     :      BOXOPT, MAJOR(2), MINOR(2) )

*  Plot the text labels.
*  The position must be specified in units of character height
*  and counts from the viewport edge. We would like the label to
*  be one character height inwards from the view surface.
*  At the moment we know only via LABSPC the fraction of the view
*  surface between the viewport and the view surface.
*  On the other hand the character height is 1/40 of the view
*  surface height.

*  Aspect ratio of view surface (width/height).
*  First get the view port in millimetres.
*  Then transform it to view surface size in millimetres.
*  Then divide width by height.
      CALL PGQVP( 2, RTEMP(1), RTEMP(2), RTEMP(3), RTEMP(4) )
      XWIDTH = ( RTEMP(2) - RTEMP(1) )
      XWIDTH = XWIDTH / ( VIEW(2) - VIEW(1) )
      YWIDTH = ( RTEMP(4) - RTEMP(3) )
      YWIDTH = YWIDTH / ( VIEW(4) - VIEW(3) )
      ASPECT = XWIDTH / YWIDTH

*  Draw each label.
      IF ( TEXT(1) .EQ. 1 )
     :   CALL PGMTXT('B',LABSPC(1)*40.       /CHIGHT-1.,.5,.5,LABEL(1))
      IF ( TEXT(2) .EQ. 1 )
     :   CALL PGMTXT('L',LABSPC(2)*40.*ASPECT/CHIGHT-1.,.5,.5,LABEL(2))
      IF ( TEXT(3) .EQ. 1 )
     :   CALL PGMTXT('T',LABSPC(3)*40.       /CHIGHT-1.,.5,.5,LABEL(3))
      IF ( TEXT(4) .EQ. 1 )
     :   CALL PGMTXT('R',LABSPC(2)*40.*ASPECT/CHIGHT-1.,.5,.5,LABEL(4))

*  Return.
      END

