      SUBROUTINE PIDEAX( STATUS )
*+
*  Name:
*     SUBROUTINE PIDEAX

*  Purpose:
*     Design image display axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PIDEAX( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     The current information is used to design the graph axes.
*     The axes are designed using calls to NCAR.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       AT4 version.
*     05-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*       Conversion to GKS 7.2 graphics.
*     09-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     25-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMGRAF'
      INCLUDE 'CMCOLR'
      INCLUDE 'CMLINR'
      INCLUDE 'CMPAL'

*  Local Constants:
      INTEGER MAXLABEL  ! Maximum length of label.
      INTEGER MAXTITLE  ! Maximum length of title.
      PARAMETER ( MAXLABEL = 40, MAXTITLE = 80 )

*  Status:
      INTEGER STATUS    ! Global status.

*  Local Variables:
      REAL ARDAT        ! Data aspect ratio.
      REAL ARDEV        ! Worstation aspect ratio.
      REAL XGAP         ! 5% gap between data and axes.
      REAL XLEFT        ! X grid left.
      REAL XM           ! SGS zone size.
      REAL XOFSET       ! X-axis offset.
      REAL XRANGE       ! Range of X-axis data.
      REAL XRIGHT       ! X grid right.
      REAL X1           ! XLIM(1 ) and SGS zone limit.
      REAL X2           ! XLIM(2) and SGS zone limit.
      REAL YBOTTM       ! Y grid bottom.
      REAL YGAP         ! 5% gap between axes and data.
      REAL YM           ! SGS zone size.
      REAL YOFSET       ! X-axis offset.
      REAL YRANGE       ! range of Y-axis data.
      REAL YTOP         ! Y grid top.
      REAL Y1           ! YLIM(1 ) and SGS zone limit.
      REAL Y2           ! YLIM(2) and SGS zone limit.

      BYTE LABELS( MAXLABEL ) ! Label string.

      CHARACTER GLABEL*( MAXTITLE ) ! Graph title.
      CHARACTER XLABEL*( MAXLABEL ) ! X-axis label.
      CHARACTER YLABEL*( MAXLABEL ) ! Y-axis label.

      INTEGER NCHAR     ! Character count.
      INTEGER NGL       ! Length of graph title.
      INTEGER NXL       ! Length of X-axis label.
      INTEGER NYL       ! Length of Y-axis label.
*.

*   Attempt to draw axes only if they are not already drawn
      IF ( .NOT. DRAWN ) THEN

*      Force a plot that reproduces the aspect ratio of the data
         CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*      Determine aspect ratios of both data and grphics workstation
         ARDAT = DBLE( ( XLIM( 2 ) - XLIM( 1 ) ) /
     :           ( YLIM( 2 ) - YLIM( 1 ) ) )
         ARDEV = DBLE( ( X2 - X1 ) / ( Y2 - Y1 ) )

*      Branch to calculate plot bounds
         IF ( ARDAT .GT. ARDEV ) THEN

*         For plot limited by X-axis extent of workstation
            XRANGE = ( X2 - X1 ) / ARDEV
            XOFSET = 0.1 * XRANGE
            XLEFT = X1 / ARDEV + XOFSET
            XRIGHT = X2 / ARDEV - XOFSET

            YRANGE = ( XRIGHT - XLEFT ) / ARDAT * ARDEV
            YOFSET = ( Y2 - Y1 - YRANGE ) / 2.0
            YBOTTM = YOFSET
            YTOP = ( YRANGE + YOFSET )

         ELSE

*         For plot limited by Y-axis extent of workstation
            YRANGE = Y2 - Y1
            YOFSET = 0.1 * YRANGE
            YBOTTM = Y1 + YOFSET
            YTOP = Y2 - YOFSET

            XRANGE = ( YTOP - YBOTTM ) * ARDAT
            XOFSET = ( X2 - X1 - XRANGE ) / 2.0
            XLEFT = XOFSET / ARDEV
            XRIGHT = ( XRANGE + XOFSET ) / ARDEV
         END IF

*      Set text colour index
         CALL GSTXCI( TILUT( TITEXT ) )

*      Switch from SGS to NCAR graphics
         CALL SNX_AGWV

*      Set NCAR WINDOW
         CALL AGSETI( 'WINDOW.', 1 )

*      Set GRID area within GRAPH window
         CALL AGSETF( 'GRID/LEFT.', XLEFT )
         CALL AGSETF( 'GRID/RIGHT.', XRIGHT )
         CALL AGSETF( 'GRID/BOTTOM.', YBOTTM )
         CALL AGSETF( 'GRID/TOP.', YTOP)
         CALL AGSETF( 'GRID/SHAPE.', 0.0)

*      All axes numbering enabled but not rotated and not shrunk
         CALL AGSETI( 'AXIS/LEFT/CONTROL.', 1 )
         CALL AGSETI( 'AXIS/RIGHT/CONTROL.', 1 )
         CALL AGSETI( 'AXIS/TOP/CONTROL.', 1 )
         CALL AGSETI( 'AXIS/BOTTOM/CONTROL.', 1 )

*      Restrict major tick marks
         CALL AGSETI( 'AXIS/LEFT/TICKS/MAJOR/SPACING/COUNT.', 4 )
         CALL AGSETI( 'AXIS/RIGHT/TICKS/MAJOR/SPACING/COUNT.', 4 )
         CALL AGSETI( 'AXIS/TOP/TICKS/MAJOR/SPACING/COUNT.', 4 )
         CALL AGSETI( 'AXIS/BOTTOM/TICKS/MAJOR/SPACING/COUNT.', 4 )

*      Restrict minor tick marks
         CALL AGSETI( 'AXIS/LEFT/TICKS/MINOR/SPACING.', 1 )
         CALL AGSETI( 'AXIS/RIGHT/TICKS/MINOR/SPACING.', 1 )
         CALL AGSETI( 'AXIS/TOP/TICKS/MINOR/SPACING.', 1 )
         CALL AGSETI( 'AXIS/BOTTOM/TICKS/MINOR/SPACING.', 1 )

*      All labels enabled but not shrunk
         CALL AGSETI( 'LABEL/CONTROL.', 1 )

*      X-axis data range plus a bit extra
         XGAP = 0.05 * ( XLIM( 2 ) - XLIM( 1 ) )
         X1 = XLIM( 1 ) - XGAP
         X2 = XLIM( 2 ) + XGAP
         CALL AGSETF( 'X/MINIMUM.', X1 )
         CALL AGSETF( 'X/MAXIMUM.', X2 )

         IF ( XJST ) THEN
            CALL AGSETI( 'X/NICE.', -1 )

         ELSE
            CALL AGSETI( 'X/NICE.', 0 )
         END IF

*      X-axis reversal
         IF ( XREV .EQ. SAI__OK ) THEN
            CALL AGSETI( 'X/ORDER.', 0 )
         ELSE
            CALL AGSETI( 'X/ORDER.', 1 )
         END IF

*      Y-axis data range plus a bit extra
         YGAP = 0.05 * ( YLIM( 2) - YLIM( 1 ))
         Y1 = YLIM( 1 ) - YGAP
         Y2 = YLIM( 2 ) + YGAP
         CALL AGSETF( 'Y/MINIMUM.', Y1 )
         CALL AGSETF( 'Y/MAXIMUM.', Y2 )

         IF ( YJST ) THEN
            CALL AGSETI( 'Y/NICE.', -1 )

         ELSE
            CALL AGSETI( 'Y/NICE.', 0 )
         END IF

*      Y-axis reversal
         IF ( YREV .EQ. SAI__OK ) THEN
            CALL AGSETI( 'Y/ORDER.', 0 )

         ELSE
            CALL AGSETI( 'Y/ORDER.', 1 )
         END IF

*      X-axis default: no label
         XLABEL = ' '

*      X-axis label
         CALL STR_MOVE( XLAB, MAXLABEL, LABELS )
         CALL STR_APPND( '     \\', MAXLABEL, LABELS )
         CALL STR_APPND( XUN, MAXLABEL, LABELS )

         CALL GEN_STOC( LABELS, MAXLABEL, XLABEL, NCHAR )
         NXL = NCHAR

         CALL AGSETC( 'LABEL/NAME.', 'B' )
         CALL AGSETI( 'LABEL/CENTERING.', 0 )
         CALL AGSETI( 'LINE/NUMBER.', -100 )
         CALL AGSETF( 'LINE/CHARACTER-WIDTH.', 0.02 )
         CALL AGSETI( 'LINE/MAXIMUM.', NXL )
         CALL AGSETC( 'LINE/TEXT.', XLABEL( : NXL ) )

*      Y-axis default: no label
         YLABEL = ' '

*      Y-axis label
         CALL STR_MOVE( YLAB, MAXLABEL, LABELS )
         CALL STR_APPND( '     \\', MAXLABEL, LABELS )
         CALL STR_APPND( YUN, MAXLABEL, LABELS )
         CALL GEN_STOC(LABELS, MAXLABEL, YLABEL, NCHAR )
         NYL = NCHAR

         CALL AGSETC( 'LABEL/NAME.', 'L' )
         CALL AGSETI( 'LABEL/CENTERING.', 0 )
         CALL AGSETI( 'LINE/NUMBER.', 100 )
         CALL AGSETF( 'LINE/CHARACTER-WIDTH.', 0.02 )
         CALL AGSETI( 'LINE/MAXIMUM.', NYL )
         CALL AGSETC( 'LINE/TEXT.', YLABEL( : NYL ) )

*      Graph title default: no title
         GLABEL = ' '

*      Graph titles
         CALL GEN_STOC( GTITLE, MAXTITLE, GLABEL, NCHAR )
         NGL = NCHAR

*      Delete previous graph labels
         CALL AGSETC( 'LABEL/NAME.', 'T' )
         CALL AGSETI( 'LABEL/DEFINITION/SUPPRESSION.', -1 )

*      No supplementary label for image
         CALL AGSETC( 'LABEL/NAME.', 'T' )
         CALL AGSETI( 'LINE/NUMBER.', 100 )
         CALL AGSETF( 'LINE/CHARACTER-WIDTH.', 0.03 )
         CALL AGSETI( 'LINE/MAXIMUM.', NGL )
         CALL AGSETC( 'LINE/TEXT.', GLABEL( : NGL ) )

*      Plot the axes
         CALL AGSTUP( XLIM, 1, 1, 2, 1, YLIM, 1, 1, 2, 1 )
         CALL AGBACK
         CALL PLOTIT ( 0, 0, 2 )

*      Register the axes as drawn
         IDRAWN = .TRUE.
         ERASED = .FALSE.
      END IF

*   Switch to SGS coordinates
      CALL SNX_AGCS

      END
