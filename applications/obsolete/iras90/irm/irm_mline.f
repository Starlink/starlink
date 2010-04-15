      SUBROUTINE IRM_MLINE( NX, NLINE, ONEXDT, XDAT, YDAT, XORDER,
     :                      YORDER, CURPEN, SOLID, X1, X2, Y1, Y2,
     :                      AXSPEN, TITLE, TITPEN, XLAB, YLAB, LBSZ,
     :                      ALBPEN, INLAB, INLBPS, INLPEN, LMTX, LMTY,
     :                      XLMT, YLMT, LOGX, LOGY, XAXS, YAXS, MAJTIC,
     :                      MINTIC, TCKLN, OUTIC, TCKPEN, NLBPEN,
     :                      BAD, STATUS )
*+
*  Name:
*     IRM_MLINE

*  Purpose:
*     Draw multi-curve plot with annotated and labelled axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_MLINE( NX, NLINE, ONEXDT, XDAT, YDAT, XORDER,
*                     YORDER, CURPEN, SOLID, X1, X2, Y1, Y2,
*                     AXSPEN, TITLE, TITPEN, XLAB, YLAB, LBSZ,
*                     ALBPEN, INLAB, INLBPS, INLPEN, LMTX, LMTY,
*                     XLMT, YLMT, LOGX, LOGY, XAXS, YAXS, MAJTIC,
*                     MINTIC, TCKLN, OUTIC, TCKPEN, NLBPEN,
*                     BAD, STATUS )

*  Description:
*     This subroutine uses NCAR to draw multi-curve plot with
*     annotated and labelled axes within the current SGS zone. In the
*     plot, each curve is a solid line interrupted at a specified
*     position by a specified in-line label. The current SGS zone is
*     the NCAR graph window (in which a graph including labels, is to
*     be drawn ). The position and the size of NCAR grid window (along
*     the edges of which the axes are to be drawn and within which
*     curves are to be drawn ) are determined by the user. The axes are
*     started and ended at the start and end of the data rather than at
*     major tick marks as NCAR's default setting. In addition, the user
*     also has following controlling options towards the plot.
*
*     1) Control the extents of the x and y axes, rather than letting
*        NCAR decide;
*     2) Specify the number of minor tick marks, and approximate
*        number of major tick marks. The actual number of major tick
*        marks will be between 2*maj and 5*maj/2+4, where the maj is
*        the specified number.
*     3) Plot the tick marks outside the grid;
*     4) have either or both axes logarithmic;
*     5) Select either bottom or top edge as X axis;
*     6) Select either left or right edge as Y axis;
*
*     An edge which is not selected as an axis will only be a
*     straight line and have no tick marks and numeric label on it.
*
*     Before using this routine an SGS device must be opened. A call
*     should be made to SNX_AGWV which marched the NCAR graph window
*     with the current SGS zone.
*
*     Warning:
*     After using this routine the world coordinates of the SGS zone
*     will be AUTOGRAPH grid coordinates, that is, the grid window has
*     bounds ( 0.0, 1.0, 0.0, 1.0 ).
*     And after this routine the NCAR setting is changed to present
*     setting.
*
*  Arguments:
*     NX = INTEGER (Given)
*        The number of x data.
*     NLINE = INTEGER (Given)
*        The total number of curves .
*     ONEXDT = LOGICAL (Given)
*        When it is true, all the curves to be plotted will have the
*        same x data at the same horizontal grid, otherwise, the
*        curves have different x data at the same horizontal grid.
*     XDAT( NX, * ) = REAL (Given)
*        The x data of the curves. When ONEXDT is true, it should be
*        a vector (or a 2-dimension array with the 2 dimension having
*        size 1 ) giving the x data for all curves. When ONEXDT is
*        false, it should be a 2-dimensional array with its second
*        dimension having size NLINE. Each of its row gives the x
*        data of the corresponding curve.
*     YDAT( NX, NLINE ) = REAL (Given)
*        The y data of NLINE curves.
*     XORDER = INTEGER (Given)
*        When it is 0, the values of user x coordinates mapped to the
*        horizontal axis of the grid window should increase from left to
*        right. When it is 1, the values of user x coordinates mapped to
*        the horizontal axis of the grid window should decrease from
*        left to right.
*     YORDER = INTEGER (Given)
*        When it is 0, the values of user y coordinates mapped to the
*        vertical axis of the grid window should increase from bottom to
*        top. When it is 0, the values of user y coordinates mapped to
*        the vertical axis of the grid window should decrease from
*        bottom to top.
*     CURPEN( NLINE ) = INTEGER (Given)
*        The SGS pen number used to draw each curve.
*     SOLID = LOGICAL (Given)
*        The flag to show whether to draw the curves in the solid line
*        regardless of the pen number of the curves.  If it is true the
*        curves in the plot will all be solid regardless of their pen
*        number and graphic device, otherwise, the line types of the
*        the curves depends on both their pen number and the graphic
*        device type.
*     X1, X2 = REAL (Given)
*        The X extent of the NCAR grid window, given in the fraction
*        of the graph-window (current SGS zone ) width.
*     Y1, Y2 = REAL (Given)
*        The Y extent of the NCAR grid window, given in the fraction
*        of the graph-window (current SGS zone ) height.
*     AXSPEN = INTEGER (Given)
*        The SGS pen number used to draw four axes.
*     TITLE = CHARACTER*(*) (Given)
*        The title of the plot.
*     TITPEN = INTEGER (Given)
*        The SGS pen number used to write the title of the plot.
*     XLAB = CHARACTER*(*) (Given)
*        The label of x axis.
*     YLAB = CHARACTER*(*) (Given)
*        The label of y axis.
*     LBSZ = REAL (Given)
*        The text height of the x and y labels given as a fraction of
*        the smaller dimension of the grid window. The value less than
*        or equal to zero means using NCAR default setting. Title of
*        the display will have the text height of 1.2*LBSZ.
*     ALBPEN = INTEGER (Given)
*        The SGS pen number used to write the axis labels.
*     INLAB( NLINE ) = CHARACTER*(*) (Given)
*        The in-line label of each curve.
*     INLBPS( NLINE ) = REAL (Given)
*        The position of in-line label on each curve given in fraction
*        of the smaller one of XSIZE and YSIXE from the left. A
*        negative value or a value greater than 1.0 means that the
*        corresponding line will be drawn with solid line.
*     INLPEN( NLINE ) = REAL(Given)
*        The SGS pen number used to write each in-line label.
*     LMTX = INTEGER (Given)
*        Its value determines how the lower and upper limits of the X
*        axis is set. It can take following values:
*
*          -1 :The values given in XLMT( 1 ) and XLMT( 2 ) will be
*              used as lower and upper limits of X axis, respectively.
*
*           0 :The graph package will examine the user's x coordinate
*              data and find the minimum and maximum value as the axis
*              lower and upper limits.
*
*           1 :The values given in XLMT( 1 ) will be used as the lower
*              limit of the axis. The graph package uses the maximum
*              value of user's x coordinate as the axis upper limit.
*
*           2 :The values given in XLMT( 2 ) will be uses as the upper
*              limit of the axis.  The graph package uses the minimum
*              value of user's x coordinate as the axis lower limit.
*
*     LMTY = INTEGER (Given)
*        Its value determines how the lower and upper limits of the Y
*        axis is set. It can take following values:
*
*          -1 :The values given in YLMT( 1 ) and YLMT( 2 ) will be
*              used as lower and upper limits of Y axis, respectively.
*
*           0 :The graph package will examine the user's Y coordinate
*              data and find the minimum and maximum value as the axis
*              lower and upper limits.
*
*           1 :The values given in YLMT( 1 ) will be used as the lower
*              limit of the axis. The graph package uses the maximum
*              value of user's Y coordinate as the axis upper limit.
*
*           2 :The values given in YLMT( 2 ) will be uses as the upper
*              limit of the axis.  The graph package uses the minimum
*              value of user's Y coordinate as the axis lower limit.
*
*     XLMT( 2 ) = REAL (Given)
*        The user specified lower limit and higher limit of x axis,
*        respectively.
*     YLMT( 2 ) = REAL (Given)
*        The user specified lower limit and higher limit of y axis,
*        respectively.
*     LOGX = LOGICAL (Given)
*        If it is true, the x axis will be set to logarithmic,
*        otherwise, to linear.
*     LOGY = LOGICAL (Given)
*        If it is true, the y axis will be set to logarithmic,
*        otherwise, to linear.
*     XAXS = LOGICAL (Given)
*        If it is true, the bottom edge of grid window is to be selected
*        as x axis. Otherwise, top edge as x axis.
*     YAXS = LOGICAL (Given)
*        If it is true, the left edge of grid window is to be selected
*        as y axis. Otherwise, top edge as y axis.
*     MAJTIC( 2 ) = REAL (Given)
*        The number of major ticks on X and Y axes, respectively. Number
*        used is between MAJTIC*2 AND 5*MAJTIC/2+4. A negative value
*        forces the NCAR to compute appropriate values.
*     MINTIC( 2 ) = REAL (Given)
*        The number of minor tick marks between each major tick mark for
*        the x and y axes. A negative value forces the NCAR to compute
*        appropriate values. A value less than 1.0 but greater than 0.0
*        will suppresses minor tick completely.
*     TCKLN = REAL (Given)
*        The length of the major tick marks given in the fraction of the
*        small dimension of the NCAR grid window. Its value should be
*        within range ( 0.0, 0.05 ). A value outside this range means
*        NCAR default. The minor tick marks will have the length
*        0.66 * TCKLN.
*     OUTTIC = LOGICAL (Given)
*        If it is true, the axis tick marks are drawn outside the grid
*        window. Otherwise, inside grid window.
*     TCKPEN = INTEGER (Given)
*        The SGS pen number used to draw the tick marks.
*     NLBPEN = INTEGER (Given)
*        The SGS pen number used to draw the numeric labels.
*     BAD = LOGICAL (Given)
*        If it is true, the bad input data are flag by ADAM magic value.
*        Otherwise, no bad input data or the bad input data is flag by
*        NCAR null value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     21-JAN-1991 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Magic-value definitions

*  Arguments Given:
      INTEGER NX
      INTEGER NLINE
      LOGICAL ONEXDT
      REAL XDAT( NX, * )
      REAL YDAT( NX, NLINE )
      INTEGER XORDER
      INTEGER YORDER
      INTEGER CURPEN( NLINE )
      LOGICAL SOLID
      REAL X1, X2
      REAL Y1, Y2
      INTEGER AXSPEN
      CHARACTER*(*) TITLE
      INTEGER TITPEN
      CHARACTER*(*) XLAB
      CHARACTER*(*) YLAB
      REAL LBSZ
      INTEGER ALBPEN
      CHARACTER*(*) INLAB( NLINE )
      REAL INLBPS( NLINE )
      INTEGER INLPEN( NLINE )
      INTEGER LMTX
      INTEGER LMTY
      REAL XLMT( 2 )
      REAL YLMT( 2 )
      LOGICAL LOGX
      LOGICAL LOGY
      LOGICAL XAXS
      LOGICAL YAXS
      REAL MAJTIC( 2 )
      REAL MINTIC( 2 )
      REAL TCKLN
      LOGICAL OUTIC
      INTEGER TCKPEN
      INTEGER NLBPEN
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NPNT               ! Number of points to be drawn

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If input data contain ADAM magic values convert them to NCAR null
*  value.
      IF( BAD ) THEN
         CALL IRM_STNUL( NX*NLINE, .TRUE., YDAT( 1, 1 ), STATUS )
         IF( ONEXDT ) THEN
            CALL IRM_STNUL( NX, .TRUE., XDAT( 1, 1 ), STATUS )
         ELSE
            CALL IRM_STNUL( NX*NLINE, .TRUE., XDAT( 1, 1 ), STATUS )
         END IF
      END IF

*  If all NLINE curves have the same x data, ...
      IF ( ONEXDT ) THEN
         CALL AGSETF( 'ROW.', 1.0 )

*  Otherwise each curve has its own x data, ...
      ELSE
         CALL AGSETF( 'ROW.', 2.0 )
      END IF

*  Set pen number.
      CALL IRM_STPEN( NLINE, CURPEN, SOLID, INLPEN, AXSPEN,
     :                TCKPEN, NLBPEN, ALBPEN, TITPEN, STATUS )

*  Set NCAR axes attributes.
      CALL IRM_STAXS( XAXS, YAXS, XLAB, YLAB, LBSZ, MAJTIC,
     :                MINTIC, TCKLN, OUTIC, STATUS )

*  Set the mapping order of the coordinates to the axes.
      CALL IRM_STMAP( XORDER, YORDER, STATUS )

*  Set the position and size of the NCAR grid window.
      CALL IRM_STGRD( X1, X2, Y1, Y2, STATUS )

*  Set the limits of axes.
      CALL IRM_STLMT( LMTX, LMTY, XLMT, YLMT, STATUS )

*  Set axes to logarithmic where required.
      CALL IRM_STLOG( LOGX, LOGY, STATUS )

*  Set the phase of curves.
      CALL IRM_STPHA( INLAB, NLINE, INLBPS, STATUS )

*  If no error happens so far, draw the plot.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Draw the curve.
         NPNT = NX
         CALL EZMXY( XDAT, YDAT, NX, NLINE, NPNT, TITLE )
         CALL SGS_FLUSH

*  Make the world coordinate of the SGS zone matches the grid coordinates.
         CALL SNX_AGCS

*  Otherwise, give an error message
      ELSE
         CALL ERR_REP( 'IRM_MLINE_ERR1',
     :                 'IRM_MLINE: Error setting NCAR parameters',
     :                 STATUS )
      END IF

      END
