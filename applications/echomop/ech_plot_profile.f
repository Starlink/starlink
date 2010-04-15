      SUBROUTINE ECH_PLOT_PROFILE(
     :           NY,
     :           PROFILING_MODE,
     :           N_ORDERS,
     :           ORDER_NUMBER,
     :           PFL_SUBSAMPLES,
     :           SUBSTEPS,
     :           MAX_SKY_PIXELS,
     :           SUBSAMPLED_PROFILE,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           OBJ_MASK,
     :           SKY_MASK,
     :           NEEDS_REFRESH,
     :           ALL_ORDERS,
     :           STATUS
     :          )
*+
* Name:
*    ECHOMOP - ECH_PLOT_PROFILE

* Purpose:
*    Plots the average spatial profile.

* Invocation:
*    CALL ECH_PLOT_PROFILE(
*   :     NY,
*   :     PROFILING_MODE,
*   :     N_ORDERS,
*   :     ORDER_NUMBER,
*   :     PFL_SUBSAMPLES,
*   :     SUBSTEPS,
*   :     MAX_SKY_PIXELS,
*   :     SUBSAMPLED_PROFILE,
*   :     DEK_BELOW,
*   :     DEK_ABOVE,
*   :     OBJ_MASK,
*   :     SKY_MASK,
*   :     NEEDS_REFRESH,
*   :     ALL_ORDERS,
*   :     STATUS
*   :    )
*

*  Arguments:
*     NY = INTEGER (Given)
*        Number of rows in input frame.
*     N_ORDERS = INTEGER (Given)
*        Number of orders.
*     ORDER_NUMBER = INTEGER (Given)
*        Order whose masks are to be used.
*     PROFILING_MODE = CHAR (Given)
*        Mode selector : D-dekker limits, O-Object profile.
*     DEK_BELOW = INTEGER (Given and Returned)
*        Lower dekker limit in pixels from trace.
*     DEK_ABOVE = INTEGER (Given and Returned)
*        Upper dekker limit in pixels from trace.
*     MAX_SKY_PIXELS = INTEGER (Given and Returned)
*        Size of mask arrays.
*     OBJ_MASK = INTEGER (Given and Returned)
*        Non-zero values denoting object presence.
*     SKY_MASK = INTEGER (Given and Returned)
*        Non-zero values denoting object presence.
*     PFL_SUBSAMPLES = INTEGER (Given and Returned)
*        Number of subsamples to use for profiles.
*     SUBSAMPLED_PROFILE = REAL (Given and Returned)
*        Raw averaged profile intensities.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.
*     SUBSTEPS = INTEGER (Given and Returned)
*        Subsamples per pixel to use for profiles.
*     NEEDS_REFRESH = LOGICAL (Given)
*        TRUE if plot needs refreshing.
*     ALL_ORDERS = LOGICAL (Given)
*        Wether the title is to reflect an all-order edit.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     16-JUL-1996 (MJC):
*       Support for ALL_ORDERS situation.
*     26-OCT-1996 (MJC):
*       New labels on slit-definition plots.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_GRAPHICS.INC'

*  Arguments Given:
      INTEGER NY
      INTEGER N_ORDERS
      CHARACTER*( * ) PROFILING_MODE
      INTEGER ORDER_NUMBER
      INTEGER DEK_BELOW( N_ORDERS )
      INTEGER DEK_ABOVE( N_ORDERS )
      INTEGER MAX_SKY_PIXELS
      INTEGER PFL_SUBSAMPLES
      LOGICAL NEEDS_REFRESH
      LOGICAL ALL_ORDERS

*  Arguments Returned:
      INTEGER OBJ_MASK( -MAX_SKY_PIXELS/2 : MAX_SKY_PIXELS/2, N_ORDERS )
*          ! Non-zero where object pixels are.
      INTEGER SKY_MASK( -MAX_SKY_PIXELS/2 : MAX_SKY_PIXELS/2, N_ORDERS )
*          ! Non-zero where sky pixels are.
      REAL SUBSAMPLED_PROFILE( -PFL_SUBSAMPLES/2 : PFL_SUBSAMPLES / 2 )
*          ! Subsampled average profile intensities.

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER PROF_HWID
      PARAMETER ( PROF_HWID = 1024 )

      INTEGER TYPE_DEKKER
      PARAMETER ( TYPE_DEKKER = 1 )

      INTEGER TYPE_BOTH
      PARAMETER ( TYPE_BOTH = 2 )

      INTEGER TYPE_SKY
      PARAMETER ( TYPE_SKY = 3 )

      INTEGER TYPE_OBJECT
      PARAMETER ( TYPE_OBJECT = 4 )

      INTEGER TYPE_UNKNOWN
      PARAMETER ( TYPE_UNKNOWN = 5 )

*  Local Variables:
      REAL TOTAL( -PROF_HWID : PROF_HWID )
      REAL XM
      REAL XH
      REAL YM
      REAL YH
      REAL YBKG             ! Y-coordinate for OBJECT label.
      REAL YOBJ             ! Y-coordinate for BACKGROUND label.
      REAL XNOT             ! X-coordinate for labels.
      REAL BOROST
      REAL BOROEN

      INTEGER INC_TYPE( -PROF_HWID : PROF_HWID )
      INTEGER I
      INTEGER COUNT
      INTEGER ORDER_SIZE
      INTEGER DISP_SCALE
      INTEGER SUBSTEPS
      INTEGER DUMSTEPS
      INTEGER PIXEL
      INTEGER OPTIONS
      INTEGER ISTART
      INTEGER CURR_SKY
      INTEGER CURR_OBJ
      INTEGER CURR_TYPE
      INTEGER NCHAR1

      CHARACTER*80 TITLE
      CHARACTER*8 REF_STR1
      CHARACTER*8 PLOT_COL
      CHARACTER*8 PLOT_STY
      CHARACTER*8 PLOT_LIN

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      INTEGER CHR_LEN
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Estimate the spatial extent in pixels and appropriate subsampling.
      CALL ECH_CALC_PROFSAMP( NY, N_ORDERS, PFL_SUBSAMPLES, DEK_BELOW,
     :     DEK_ABOVE, ORDER_SIZE, DUMSTEPS, STATUS )
      DISP_SCALE = MIN( ORDER_SIZE * SUBSTEPS, PFL_SUBSAMPLES / 2  )
      DISP_SCALE = MAX( DISP_SCALE, 3 * SUBSTEPS *
     :      MAX( ABS( DEK_BELOW( ORDER_NUMBER ) ),
     :      ABS( DEK_ABOVE( ORDER_NUMBER ) ) ) / 2 )
      DISP_SCALE = MIN( DISP_SCALE,
     :      MAX_SKY_PIXELS / 2 * SUBSTEPS, PFL_SUBSAMPLES / 2 )
      DISP_SCALE = MIN( PROF_HWID, DISP_SCALE )

*  If title and axes need plotting then.
      IF ( NEEDS_REFRESH ) THEN

*     Select appropriate title for graph.
         CALL CHR_UCASE( PROFILING_MODE )
         IF ( PROFILING_MODE( : 1 ) .EQ. 'D' ) THEN
            CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
            TITLE = ' Profile of order ' // REF_STR1( :NCHAR1 )
            NCHAR1 = CHR_LEN( TITLE )
            IF ( ALL_ORDERS ) THEN
               TITLE = TITLE( :NCHAR1 ) //
     :               ' for all-order dekker determination'

            ELSE
               TITLE = TITLE( :NCHAR1 ) // ' for dekker determination'
            END IF

         ELSE IF ( PROFILING_MODE( : 1 ) .EQ. 'O' .OR.
     :             PROFILING_MODE( : 1 ) .EQ. 'S' ) THEN
            CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
            TITLE = ' Profile of order ' // REF_STR1( :NCHAR1 )
            NCHAR1 = CHR_LEN( TITLE )
            IF ( ALL_ORDERS ) THEN
               TITLE = TITLE( :NCHAR1 ) //
     :               ' for all-order Object/Sky determination'

            ELSE
               TITLE = TITLE( :NCHAR1 ) //
     :         ' for Object/Sky determination'
            END IF
         END IF

*     Calculate min/max for Y-axis and fill X-axis array with scale.
         YH = 1.0
         YM = 1.0E20
         XH = FLOAT( DISP_SCALE ) / FLOAT( SUBSTEPS )
         XM = - XH
         DO I = -DISP_SCALE, DISP_SCALE
            TOTAL( I ) = FLOAT( I ) / FLOAT( SUBSTEPS )
            IF ( YM .GT. SUBSAMPLED_PROFILE( I ) )
     :         YM = SUBSAMPLED_PROFILE( I )
            IF ( YH .LT. SUBSAMPLED_PROFILE( I ) )
     :         YH = SUBSAMPLED_PROFILE( I )
         END DO
         YH = YH * 1.1
         YM = YM - 0.05 * ( YH - YM )

*     Plot outline for graph of observed average profile.
         OPTIONS = 0
         CALL ECH_PLOT_GRAPH( 1, TOTAL,
     :        SUBSAMPLED_PROFILE( -DISP_SCALE ),
     :        XM, XH, YM, YH, 'Y offset in pixels', 'Average',
     :        TITLE, 0.0, 0.0, OPTIONS, 'LINES', STATUS )
      END IF

*  Loop through subsamples of profile determining the 'type' at each subsample.
      DO I = -DISP_SCALE, DISP_SCALE

*     Determine nearest whole pixel value.
         IF ( I .GE. 0 ) THEN
            PIXEL = I + SUBSTEPS / 2

         ELSE
            PIXEL = I - SUBSTEPS / 2
         END IF
         PIXEL = PIXEL / SUBSTEPS

*     If pixel is outside dekker limits the set its type to DEKKER.
         IF ( PIXEL .LT. DEK_BELOW( ORDER_NUMBER ) .OR.
     :        PIXEL .GT. DEK_ABOVE( ORDER_NUMBER ) ) THEN
            INC_TYPE( I ) = TYPE_DEKKER

*     If we are in dekker mode then set its type to OBJECT-ONLY.
         ELSE IF ( PROFILING_MODE( : 1 ) .EQ. 'D' ) THEN
            INC_TYPE( I ) = TYPE_OBJECT
         ELSE

*        Get current pixels' object/sky mask values.
            CURR_SKY = SKY_MASK( PIXEL, ORDER_NUMBER )
            CURR_OBJ = OBJ_MASK( PIXEL, ORDER_NUMBER )

*        If marked as both Object and Sky then set type to BOTH.
            IF ( CURR_SKY .GT. 0 .AND. CURR_OBJ .GT. 0 ) THEN
               INC_TYPE( I ) = TYPE_BOTH

*        If marked as Sky then set type to SKY-ONLY.
            ELSE IF ( CURR_SKY .GT. 0 ) THEN
               INC_TYPE( I ) = TYPE_SKY

*        If marked as Object then set type to OBJECT-ONLY.
            ELSE IF ( CURR_OBJ .GT. 0 ) THEN
               INC_TYPE( I ) = TYPE_OBJECT

*        Otherwise mark as UNUSED.
            ELSE
               INC_TYPE( I ) = TYPE_UNKNOWN
            END IF
         END IF
      END DO

*  The following loops plot a subsampled profile in the following
*  colour scheme:
*
*     GREEN  -  Outside dekker
*     BLUE   -  Pixel is used to determine sky
*     RED    -  Pixel is used to determine object
*     YELLOW -  Pixel is used to determine sky and object
*     BLACK  -  Not used

      OPTIONS = GRPH_OVERLAY + GRPH_SET_COLOUR + GRPH_SET_LINE_STYLE

*  Find positions for labels.
      IF ( PROFILING_MODE( : 1 ) .EQ. 'O' .OR.
     :     PROFILING_MODE( : 1 ) .EQ. 'S' ) THEN
         XNOT = XM + 0.87 * ( XH - XM )
         YOBJ = YM + 0.95 * ( YH - YM )
         YBKG = YM + 0.10 * ( YH - YM )
      END IF

*  Loop through subsamples of profile plotting consecutive sequences of
*  subsamples of identical type.
      I = -DISP_SCALE + 1
      DO WHILE ( I .LT. DISP_SCALE - 1 )

*     Record starting subsample index and type.
         ISTART = I
         BOROST = TOTAL( I )
         CURR_TYPE = INC_TYPE( I )

*     Loop through subsamples until type changes, or subsamples exhausted.
         DO WHILE ( INC_TYPE( I ) .EQ. CURR_TYPE .AND.
     :              I .LT. DISP_SCALE - 1 )
            I = I + 1
         END DO

*     Calculate number of subsamples to plot.
         COUNT = MAX( 2, I - ISTART + 1 )
         BOROEN = TOTAL( I )

*     Plot next section of graph in the appropriate colour and style.
         IF ( CURR_TYPE .EQ. TYPE_DEKKER ) THEN
            PLOT_COL = 'GREEN'
            PLOT_STY = 'DOTD'
            PLOT_LIN = 'LINES'

         ELSE IF ( CURR_TYPE .EQ. TYPE_BOTH ) THEN
            PLOT_COL = 'YELLOW'
            PLOT_STY = ' '
            PLOT_LIN = '+'

         ELSE IF ( CURR_TYPE .EQ. TYPE_SKY ) THEN
            PLOT_COL = 'BLUE'
            PLOT_STY = 'DASH'
            PLOT_LIN = 'LINES'

         ELSE IF ( CURR_TYPE .EQ. TYPE_OBJECT ) THEN
            PLOT_COL = 'RED'
            PLOT_STY = 'FULL'
            PLOT_LIN = 'LINES'

         ELSE
            PLOT_COL = 'BLACK'
            PLOT_STY = 'FULL'
            PLOT_LIN = 'LINES'
         END IF

*     Overplot IRAF-style markers
         IF ( ( PROFILING_MODE( :1 ) .EQ. 'O' .OR.
     :        PROFILING_MODE( :1 ) .EQ. 'S' ) .AND.
     :        GRAPHICS_SETUP ) THEN
            CALL ECH_GR_SET_LINESTYLE( STY_FULL )
            IF ( CURR_TYPE .EQ. TYPE_SKY .OR.
     :           CURR_TYPE .EQ. TYPE_BOTH ) THEN
               CALL ECH_GR_SET_COLOUR( COL_BLUE )
               CALL PGMOVE( BOROST, YBKG )
               CALL PGDRAW( BOROST, YBKG + 0.02 * ( YH - YM ) )
               CALL PGMOVE( BOROST, YBKG )
               CALL PGDRAW( BOROEN, YBKG )
               CALL PGDRAW( BOROEN, YBKG + 0.02 * ( YH - YM ) )
            END IF
            IF ( CURR_TYPE .EQ. TYPE_OBJECT .OR.
     :           CURR_TYPE .EQ. TYPE_BOTH ) THEN
               CALL ECH_GR_SET_COLOUR( COL_RED )
               CALL PGMOVE( BOROST, YOBJ )
               CALL PGDRAW( BOROST, YOBJ - 0.02 * ( YH - YM ) )
               CALL PGMOVE( BOROST, YOBJ )
               CALL PGDRAW( BOROEN, YOBJ )
               CALL PGDRAW( BOROEN, YOBJ - 0.02 * ( YH - YM ) )
            END IF
         END IF
         CALL ECH_PLOT_GRAPH( COUNT, TOTAL( ISTART ),
     :        SUBSAMPLED_PROFILE( ISTART ), 0., 0., 0., 0.,
     :        PLOT_COL, PLOT_STY, TITLE, 0.0, 0.0,
     :        OPTIONS, PLOT_LIN, STATUS )
      END DO

*  Plot labels.
      IF ( ( PROFILING_MODE( :1 ) .EQ. 'O' .OR.
     :     PROFILING_MODE( :1 ) .EQ. 'S' ) .AND.
     :     GRAPHICS_SETUP ) THEN
         CALL ECH_GR_SET_COLOUR( COL_BLUE )
         CALL PGPTXT( XNOT, YBKG, 0.0, 0.0, 'Background' )
         YOBJ = YM + 0.93 * ( YH - YM )
         CALL ECH_GR_SET_COLOUR( COL_RED )
         CALL PGPTXT( XNOT, YOBJ, 0.0, 0.0, 'Object' )
      END IF

*  Reset plot colour to black.
      CALL ECH_PLOT_GRAPH( 1, TOTAL( ISTART ),
     :     SUBSAMPLED_PROFILE( ISTART ), 0., 0., 0., 0.,
     :     'BLACK', 'FULL', TITLE, 0.0, 0.0,
     :     OPTIONS, 'LINES', STATUS )

      END
