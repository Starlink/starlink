      SUBROUTINE GRF_AXES( X1, X2, Y1, Y2, YGAP, SLABEL, NSL )
*+
*  Name:
*     SUBROUTINE GRF_AXES

*  Purpose:
*     Draw plot axes on display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRF_AXES( X1, X2, Y1, Y2, YGAP, SLABEL, NSL )

*  Arguments:
*     SLABEL = CHARACTER*( * ) (Given)
*        Supplementary label for graph.
*     NSL = INTEGER (Given)
*        Length of supplementary label.

*  Authors:
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     28-JAN-95 (MJC):
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
      INCLUDE 'CMPAL'

*  Arguments Given:
      CHARACTER*( * ) SLABEL    ! Supplementary graph label.

      INTEGER NSL     ! Length of supplementary graph label.

*  Arguments Returned:
      REAL X1         ! XLIM( 1 ).
      REAL X2         ! XLIM( 2 ).
      REAL Y1         ! YLIM( 1 ).
      REAL Y2         ! YLIM( 2 ).
      REAL YGAP       ! 5% gap between Y-axis data and axes.

*  Local Constants:
      INTEGER MAXLABEL              ! Maximum length of label.
      INTEGER MAXTITLE              ! Maximum length of title.
      PARAMETER ( MAXLABEL = 40, MAXTITLE = 80 )

*  Local Variables:
      BYTE LABELS( MAXLABEL )       ! Label string.

      CHARACTER GLABEL*( MAXTITLE ) ! Graph title.
      CHARACTER XLABEL*( MAXLABEL ) ! X-axis label.
      CHARACTER YLABEL*( MAXLABEL ) ! Y-axis label.

      INTEGER NCHAR   ! Number of characters in a label.
*.

*   Attempt to draw axes only if they are not already drawn.
      IF ( .NOT. DRAWN ) THEN

*      Set text colour index.
         CALL GSTXCI( TILUT( TITEXT ) )

*      Set font type.
         CALL SNX_CHSET( 2 )

*      Switch from SGS to NCAR graphics.
         CALL SNX_AGWV

*      Set NCAR WINDOW.
         CALL AGSETI( 'WINDOW.', 1 )

*      Set GRID area within GRAPH window, depends on zone selection.
         IF ( ZONE .EQ. 0 ) THEN
            CALL AGSETF( 'GRID/LEFT.', 0.10 )
            CALL AGSETF( 'GRID/RIGHT.', 0.95 )
            CALL AGSETF( 'GRID/BOTTOM.', 0.10 )
            CALL AGSETF( 'GRID/TOP.', 0.85 )
            CALL AGSETF( 'GRID/SHAPE.', 0.0 )

         ELSE IF ( ZONE.EQ.1 .OR. ZONE.EQ.2 .OR.
     :             ZONE.EQ.3 .OR. ZONE.EQ.4 ) THEN
            CALL AGSETF( 'GRID/LEFT.', 0.20 )
            CALL AGSETF( 'GRID/RIGHT.', 0.95 )
            CALL AGSETF( 'GRID/BOTTOM.', 0.15 )
            CALL AGSETF( 'GRID/TOP.', 0.80 )
            CALL AGSETF( 'GRID/SHAPE.', 0.0 )

         ELSE IF ( ZONE.EQ.5 .OR. ZONE.EQ.6 ) THEN
            CALL AGSETF( 'GRID/LEFT.', 0.10 )
            CALL AGSETF( 'GRID/RIGHT.', 0.95 )
            CALL AGSETF( 'GRID/BOTTOM.', 0.12 )
            CALL AGSETF( 'GRID/TOP.', 0.85 )
            CALL AGSETF( 'GRID/SHAPE.', 0.0 )

         ELSE IF ( ZONE.EQ.7 .OR. ZONE.EQ.8 ) THEN
            CALL AGSETF( 'GRID/LEFT.', 0.20 )
            CALL AGSETF( 'GRID/RIGHT.', 0.95 )
            CALL AGSETF( 'GRID/BOTTOM.', 0.10 )
            CALL AGSETF( 'GRID/TOP.', 0.85 )
            CALL AGSETF( 'GRID/SHAPE.', 0.0 )
         END IF

*      All axes numbering enabled but not rotated and not shrunk.
         CALL AGSETI( 'AXIS/LEFT/CONTROL.', 1 )
         CALL AGSETI( 'AXIS/RIGHT/CONTROL.', 1 )
         CALL AGSETI( 'AXIS/TOP/CONTROL.', 1 )
         CALL AGSETI( 'AXIS/BOTTOM/CONTROL.', 1 )

*      Restrict major tick marks.
         CALL AGSETI( 'AXIS/LEFT/TICKS/MAJOR/SPACING/COUNT.', 4 )
         CALL AGSETI( 'AXIS/RIGHT/TICKS/MAJOR/SPACING/COUNT.', 4 )
         CALL AGSETI( 'AXIS/TOP/TICKS/MAJOR/SPACING/COUNT.', 4 )
         CALL AGSETI( 'AXIS/BOTTOM/TICKS/MAJOR/SPACING/COUNT.', 4 )

*      Restrict minor tick marks.
         CALL AGSETI( 'AXIS/LEFT/TICKS/MINOR/SPACING.', 1 )
         CALL AGSETI( 'AXIS/RIGHT/TICKS/MINOR/SPACING.', 1 )
         CALL AGSETI( 'AXIS/TOP/TICKS/MINOR/SPACING.', 1 )
         CALL AGSETI( 'AXIS/BOTTOM/TICKS/MINOR/SPACING.', 1 )

*      All labels enabled but not shrunk.
         CALL AGSETI( 'LABEL/CONTROL.', 1 )

*      X-axis data range.
         X1 = XLIM( 1 )
         X2 = XLIM( 2 )
         CALL AGSETF( 'X/MINIMUM.', X1 )
         CALL AGSETF( 'X/MAXIMUM.', X2 )

         IF ( XJST ) THEN
            CALL AGSETI( 'X/NICE.', -1 )

         ELSE
            CALL AGSETI( 'X/NICE.', 0 )
         END IF

*      X-axis reversal.
         IF ( XREV .EQ. SAI__OK ) THEN
            CALL AGSETI( 'X/ORDER.', 0 )

         ELSE
            CALL AGSETI( 'X/ORDER.', 1 )
         END IF

*      Y-axis data range.
         YGAP = ( YLIM( 2 ) - YLIM( 1 ) ) * 0.05
         Y1 = YLIM( 1 ) - YGAP
         Y2 = YLIM( 2 ) + YGAP
         CALL AGSETF( 'Y/MINIMUM.', Y1 )
         CALL AGSETF( 'Y/MAXIMUM.', Y2 )

         IF ( YJST ) THEN
            CALL AGSETI( 'Y/NICE.', -1 )

         ELSE
            CALL AGSETI( 'Y/NICE.', 0 )
         END IF

*      Y-axis reversal.
         IF ( YREV .EQ. SAI__OK ) THEN
            CALL AGSETI( 'Y/ORDER.', 0 )

         ELSE
            CALL AGSETI( 'Y/ORDER.', 1 )
         END IF

*      X-axis default: no label.
         XLABEL = ' '

*      X-axis label.
         CALL STR_MOVE( XLAB, MAXLABEL, LABELS )
         CALL STR_APPND( '    \\', MAXLABEL, LABELS )
         CALL STR_APPND( XUN, MAXLABEL, LABELS )
         CALL GEN_STOC( LABELS, MAXLABEL, XLABEL, NCHAR )

         CALL AGSETC( 'LABEL/NAME.', 'B' )
         CALL AGSETI( 'LABEL/CENTERING.', 0 )
         CALL AGSETI( 'LINE/NUMBER.', -100 )
         CALL AGSETF( 'LINE/CHARACTER-WIDTH.', 0.02 )
         CALL AGSETI( 'LINE/MAXIMUM.', NCHAR )
         CALL AGSETC( 'LINE/TEXT.', XLABEL( : NCHAR ) )

*      Y-axis default: no label.
         YLABEL = ' '

*      Y-axis label.
         CALL STR_MOVE( YLAB, MAXLABEL, LABELS )
         CALL STR_APPND( '    \\', MAXLABEL, LABELS )
         CALL STR_APPND( YUN, MAXLABEL, LABELS )
         CALL GEN_STOC( LABELS, MAXLABEL, YLABEL, NCHAR )

         CALL AGSETC( 'LABEL/NAME.', 'L' )
         CALL AGSETI( 'LABEL/CENTERING.', 0 )
         CALL AGSETI( 'LINE/NUMBER.', 100 )
         CALL AGSETF( 'LINE/CHARACTER-WIDTH.', 0.02 )
         CALL AGSETI( 'LINE/MAXIMUM.', NCHAR )
         CALL AGSETC( 'LINE/TEXT.', YLABEL(  : NCHAR ) )

*      Graph title default: no title.
         GLABEL = ' '

*      Graph titles.
         CALL GEN_STOC( GTITLE, MAXTITLE, GLABEL, NCHAR )

*      Delete previous graph labels.
         CALL AGSETC( 'LABEL/NAME.', 'T' )
         CALL AGSETI( 'LABEL/DEFINITION/SUPPRESSION.', -1 )

*      Load graph labels.
         IF ( NSL .EQ. 0 ) THEN

*         For no supplementary label.
            CALL AGSETC( 'LABEL/NAME.', 'T' )
            CALL AGSETI( 'LINE/NUMBER.', 100 )
            CALL AGSETF( 'LINE/CHARACTER-WIDTH.', 0.03 )
            CALL AGSETI( 'LINE/MAXIMUM.', NCHAR )
            CALL AGSETC( 'LINE/TEXT.', GLABEL( : NCHAR ) )
         ELSE

*         For supplementary labelling.
            CALL AGSETC( 'LABEL/NAME.', 'T' )
            CALL AGSETI( 'LINE/NUMBER.', 200 )
            CALL AGSETF( 'LINE/CHARACTER-WIDTH.', 0.03 )
            CALL AGSETI( 'LINE/MAXIMUM.', NCHAR )
            CALL AGSETC( 'LINE/TEXT.', GLABEL( : NCHAR ) )

            CALL AGSETC( 'LABEL/NAME.', 'T' )
            CALL AGSETI( 'LINE/NUMBER.', 100 )
            CALL AGSETF( 'LINE/CHARACTER-WIDTH.', 0.03 )
            CALL AGSETI( 'LINE/MAXIMUM.', NSL )
            CALL AGSETC( 'LINE/TEXT.', SLABEL( : NSL ) )
         END IF

*      Draw axes.
         CALL AGSTUP( XLIM, 1, 1, 2, 1, YLIM, 1, 1, 2, 1 )
         CALL AGBACK

*      Register the axes as drawn.
         DRAWN = .TRUE.
         ERASED = .FALSE.
      ELSE

*      Switch from SGS to NCAR graphics.
         CALL SNX_AGWV
      END IF

      END
