      SUBROUTINE KPS1_NCUCO( XLOW, XHIGH, YLOW, YHIGH, STATUS )
*+
*  Name:
*     KPS1_NCUCO

*  Purpose:
*     Defines the user co-ordinates of an NCAR grid without plotting
*     axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_NCUCO( XLOW, XHIGH, YLOW, YHIGH, STATUS )

*  Description:
*     This routine defines the limiting user co-ordinates of an NCAR
*     grid region.  This is needed to prepare the NCAR environment so
*     that text in fancy founts, and dashed and annotated curves may be
*     drawn in the desired co-ordinate system without having axes
*     plotted.  It defines new bounds and user co-ordinates of the grid
*     region.  The current SGS zone is used.  The default grid limits
*     are used unless changed externally.
*
*     It is recommmended that the input co-ordinates are world
*     co-ordinates, i.e. increase from left to right and bottom to top.

*  Arguments:
*     XLOW = REAL (Given)
*        The lower user co-ordinate of the x axis.
*     XHIGH = REAL (Given)
*        The upper user co-ordinate of the x axis.
*     YLOW = REAL (Given)
*        The lower user co-ordinate of the y axis.
*     YHIGH = REAL (Given)
*        The upper user co-ordinate of the y axis.
*     STATUS = INTEGER (Given)
*        The global status.

*  Prior Requirements:
*     -  An SGS device must be opened, and an SGS zone selected or
*     created.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 October 22 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL
     :  XLOW,
     :  XHIGH,
     :  YLOW,
     :  YHIGH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL
     :  CNTROL( 4 ),           ! Input AUTOGRAPH grid parameters
     :  LIMITS( 4 ),           ! Input x,y axis maxima and minima
     :  XD( 2 ),               ! Dummy x co-ordinates
     :  YD( 2 )                ! Dummy y co-ordinates

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get AUTOGRAPH to use current SGS zone.

      CALL SNX_AGWV

*    Store current extrema of axis co-ordinates.

      CALL AGGETF( 'X/MINIMUM.', LIMITS(1) )
      CALL AGGETF( 'X/MAXIMUM.', LIMITS(2) )
      CALL AGGETF( 'Y/MINIMUM.', LIMITS(3) )
      CALL AGGETF( 'Y/MAXIMUM.', LIMITS(4) )

*    Set axis limits because a dummy point is to be plotted.  Allow for
*    inverted axes.

      CALL AGSETF( 'X/MINIMUM.', MIN( XLOW, XHIGH ) )
      CALL AGSETF( 'X/MAXIMUM.', MAX( XLOW, XHIGH ) )
      CALL AGSETF( 'Y/MINIMUM.', MIN( YLOW, YHIGH ) )
      CALL AGSETF( 'Y/MAXIMUM.', MAX( YLOW, YHIGH ) )

*    Store the current axis control parameters.

      CALL AGGETF( 'AXIS/LEFT/CONTROL.', CNTROL(2) )
      CALL AGGETF( 'AXIS/BOTTOM/CONTROL.', CNTROL(1) )
      CALL AGGETF( 'AXIS/RIGHT/CONTROL.', CNTROL(4) )
      CALL AGGETF( 'AXIS/TOP/CONTROL.', CNTROL(3) )

*    Plot a dummy background (invisible axes and labels).

      CALL AGSETF( 'AXIS/LEFT/CONTROL.', 0. )
      CALL AGSETF( 'AXIS/BOTTOM/CONTROL.', 0. )
      CALL AGSETF( 'AXIS/RIGHT/CONTROL.', 0. )
      CALL AGSETF( 'AXIS/TOP/CONTROL.', 0. )
      CALL SNX_EZRXY( XD, YD, 1, ' ', ' ', ' ' )

*    Restore the axes co-ordinate limits to their input values.

      CALL AGSETF( 'X/MINIMUM.', LIMITS(1) )
      CALL AGSETF( 'X/MAXIMUM.', LIMITS(2) )
      CALL AGSETF( 'Y/MINIMUM.', LIMITS(3) )
      CALL AGSETF( 'Y/MAXIMUM.', LIMITS(4) )

*    Restore changed axis control parameters to their input values.

      CALL AGSETF( 'AXIS/LEFT/CONTROL.', CNTROL(2) )
      CALL AGSETF( 'AXIS/BOTTOM/CONTROL.', CNTROL(1) )
      CALL AGSETF( 'AXIS/RIGHT/CONTROL.', CNTROL(4) )
      CALL AGSETF( 'AXIS/TOP/CONTROL.', CNTROL(3) )

      END
