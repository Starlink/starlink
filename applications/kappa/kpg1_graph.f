      SUBROUTINE KPG1_GRAPH( N, X, Y, NSIGMA, YSIGMA, XLAB, YLAB, TTL,
     :                       XSYM, YSYM, MODE, NULL, XL, XR, YB, YT, 
     :                       APP, QUIET, IPLOT, STATUS )
*+
*  Name:
*     KPG1_GRAPH

*  Purpose:
*     Draws a line graph.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GRAPH( N, X, Y, NSIGMA, YSIGMA, XLAB, YLAB, TTL, XSYM,
*                      YSYM, MODE, NULL, XL, XR, YB, YT, APP, QUIET, 
*                      IPLOT, STATUS )

*  Description:
*     Opens a graphics device and draws a graph displaying a supplied 
*     set of points. Each point is defined by an X and Y value, plus an 
*     optional error bar. An AST Plot is returned so that the calling 
*     application can add further graphics to the plot if needed. When 
*     complete, the calling application should annul the Plot, and close 
*     the workstation:
*
*       CALL AST_ANNUL( IPLOT, STATUS )
*       CALL AGP_DEASS( 'DEVICE', .FALSE., STATUS )

*  Environment Parameters:
*     The following envirnment parameter names are used by this routine,
*     to encourage uniformity in parameter naming, and behaviour:
*
*        AXES = _LOGICAL (Read)
*           TRUE if annotated axes are to be produced.
*        CLEAR = _LOGICAL (Read)
*           TRUE if the graphics device is to be cleared on opening. 
*        DEVICE = DEVICE (Read)
*           The plotting device. 
*        MARGIN( 4 ) = _REAL (Read)
*           The widths of the margins to leave for axis annotation, given 
*           as fractions of the corresponding dimension of the DATA picture. 
*           Four values may be given, in the order - bottom, right, top, left. 
*           If less than four values are given, extra values are used equal to 
*           the first supplied value. If these margins are too narrow any axis 
*           annotation may be clipped. The dynamic default is 0.18 (for all 
*           edges) if either annotated axes or a key are produced, and zero 
*           otherwise. 
*        MARKER = INTEGER )Read)
*           The PGPLOT marker type to use. Only accessed if MODE is 3 or 5.
*        STYLE = GROUP (Read)
*           A description of the plotting style required. The following 
*           synonyms for graphical elements may be used: 
*           "Err(Bars)" - Specifies colour, etc for error bars. Size(errbars)
*                         scales the size of the serifs (i.e. a size value of 
*                         1.0 produces a default size).
*           "Sym(bols)" - Specifies colour, etc for markers (used in modes 3
*                         and 5).
*           "Lin(es)"   - Specifies colour, etc for lines (used in modes 1, 2
*                         and 5).
*        XLEFT = LITERAL (Read)
*           The axis value to place at the left hand end of the horizontal
*           axis. The dynamic default is specified by argument XL. The value 
*           supplied may be greater than or less than the value supplied for 
*           XRIGHT. 
*        XRIGHT = LITERAL (Read)
*           The axis value to place at the right hand end of the horizontal
*           axis. The dynamic default is specified by argument XR. The value 
*           supplied may be greater than or less than the value supplied for 
*           XLEFT. 
*        YBOT = LITERAL (Read)
*           The axis value to place at the bottom end of the vertical 
*           axis. The dynamic default is specified by argument YB. The value 
*           supplied may be greater than or less than the value supplied for 
*           YTOP. 
*        YTOP = LITERAL (Read)
*           The axis value to place at the top end of the vertical axis. 
*           The dynamic default is specified by argument YT. The value 
*           supplied may be greater than or less than the value supplied 
*           for YBOT. 

*  Arguments:
*     N = INTEGER (Given)
*        No. of points
*     X( N ) = REAL (Given)
*        X value at each point.
*     Y( N ) = REAL (Given)
*        Y value at each point.
*     NSIGMA = REAL (Given)
*        Controls the length of the vertical error bars. A value of zero
*        suppresses error bars. Otherwise error bars are drawn which extend 
*        by from Y - NSIGMA*YSIGMA to Y + NSIGMA*YSIGMA.
*     YSIGMA( N ) = REAL (Given)
*        The standard deviation associated with each Y value.
*     XLAB = CHARACTER * ( * ) (Given)
*        A default label for the X axis. Only used if the user does not
*        supply an alternative. Trailing spaces are ignored.
*     YLAB = CHARACTER * ( * ) (Given)
*        A default label for the Y axis. Only used if the user does not
*        supply an alternative. Trailing spaces are ignored.
*     TTL = CHARACTER * ( * ) (Given)
*        A default title for the plot. Only used if the user does not
*        supply an alternative.
*     XSYM = CHARACTER * ( * ) (Given)
*        The default symbol for the horizontal axis. Only used if the user 
*        does not supply an alternative. This is not displayed on the
*        screen, but will be stored with the Plot in the AGI database and 
*        (for instance) used by CURSOR as axis symbols when displaying the 
*        cursor positions on the screen.
*     YSYM = CHARACTER * ( * ) (Given)
*        The default symbol for the vertical axis. Only used if the user 
*        does not supply an alternative. This is not displayed on the
*        screen, but will be stored with the Plot in the AGI database and 
*        (for instance) used by CURSOR as axis symbols when displaying the 
*        cursor positions on the screen.
*     MODE = INTEGER (Given)
*        Determines the way in which the data points are represented:
*            1 - A "staircase" histogram, in which each horizontal line is
*                centred on the X position.
*            2 - The points are joined by straight lines.
*            3 - A marker is placed at each point.
*            4 - (not used)
*            5 - A "chain" in which each point is marker by a marker and also 
*                join by straight lines to its neighbouring points.
*     NULL = LOGICAL (Given)
*        If .TRUE., then the user may supply a null (!) value for most of the
*        parameters accessed by this routine to indicate that nothing is to
*        be plotted. In this case, no error is returned. Otherwise, a
*        PAR__NULL error status is returned if a null value is supplied.
*     XL = REAL (Given)
*        The default value for the XLEFT parameter. If VAL__BADR is
*        supplied, the minimum of the X values is used (plus a small
*        margin).
*     XR = REAL (Given)
*        The default value for the XRIGHT parameter. If VAL__BADR is
*        supplied, the maximum of the X values is used (plus a small
*        margin).
*     YB = REAL (Given)
*        The default value for the YBOT parameter. If VAL__BADR is
*        supplied, the minimum of the low end of the Y error bars is 
*        used (plus a small margin).
*     YT = REAL (Given)
*        The default value for the YTOP parameter. If VAL__BADR is
*        supplied, the maximum of the high end of the Y error bars is 
*        used (plus a small margin).
*     APP = CHARACTER * ( * ) (Given)
*        The name of the application in the form "<package>_<application>".
*        E.g. "KAPPA_NORMALIZE".
*     QUIET = LOGICAL (Given)
*        If .FALSE., a message is displayed indicating the number of
*        points which were plotted. If .TRUE., nothing is displayed on
*        the alpha screen.
*     IPLOT = INTEGER (Returned)
*        The AST Plot used to do the drawing.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - If an error occurs, or if no graphics is produced because the
*     user supplied a null value for a parameter, IPLOT is returned equal
*     to AST__NULL, and PGPLOT is shut down.

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JUN-1999 (DSB):
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
      INTEGER N
      REAL X( N )
      REAL Y( N )
      REAL NSIGMA
      REAL YSIGMA( N )
      CHARACTER XLAB*(*)
      CHARACTER YLAB*(*)
      CHARACTER TTL*(*)
      CHARACTER XSYM*(*)
      CHARACTER YSYM*(*)
      INTEGER MODE
      LOGICAL NULL
      REAL XL
      REAL XR
      REAL YB
      REAL YT
      CHARACTER APP*(*)
      LOGICAL QUIET

*  Arguments Returned:
      INTEGER IPLOT

*  Status:
      INTEGER STATUS          ! Global status

*  Local Variables:
      INTEGER IPW1
      INTEGER IPW2
      INTEGER IPW3
*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Allocate the required work space.
      CALL PSX_CALLOC( N, '_DOUBLE', IPW1, STATUS )
      CALL PSX_CALLOC( N, '_DOUBLE', IPW2, STATUS )

      IF( NSIGMA .GT. 0 ) THEN
         CALL PSX_CALLOC( 2*N, '_DOUBLE', IPW3, STATUS )
      ELSE
         IPW3 = IPW1 
      END IF

*  Draw the graph.
      CALL KPG1_GRPHW( N, X, Y, NSIGMA, YSIGMA, XLAB, YLAB, TTL,
     :                 XSYM, YSYM, MODE, NULL, XL, XR, YB, YT, APP, 
     :                 QUIET, %VAL( IPW1 ), %VAL( IPW2 ), %VAL( IPW3 ), 
     :                 IPLOT, STATUS )

*  Free the work space.
      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPW2, STATUS )
      IF( NSIGMA .GT. 0 ) CALL PSX_FREE( IPW3, STATUS )

      END
