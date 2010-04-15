*+  P4_LIST_NB - List contents of P4_NB to screen
      SUBROUTINE P4_LIST_NB( STATUS )
*    Description :
*     This routine defines the contents of the P4_NB noticeboard.
*    Invocation :
*     CALL P4_LIST_NB( STATUS )
*    Authors :
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*      4-Aug-1994: Original version (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'NBS_ERR'
      INCLUDE 'PRM_PAR'
*    Status :
      INTEGER STATUS             ! Global status
*    Global variables :
      INCLUDE 'P4COM.INC'        ! P4 common block
*    Local variables :
      CHARACTER*(NBS_FLEN) CJUNK ! Temporary character variable
      INTEGER IJUNK              ! Temporary integer variable
      LOGICAL LJUNK              ! Temporary logical variable
      REAL RJUNK                 ! Temporary real variable
      INTEGER
     :  ACTBYTES,                ! Actual number of bytes read from NBS
     :  PORT,                    ! Port number
     :  NSTART,                  ! Start of DO-loop value
     :  NEND,                    ! End of DO-loop value
     :  I,                       ! Counter for Do-loop
     :  ERR_STAT                 ! An error status
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   If 0 <= PORT <= MAXPORT, update just that port
      CALL PAR_GET0I( 'PORT', PORT, STATUS )
      IF ( PORT.GE.0 .AND. PORT.LE.MAXPORT ) THEN
        NSTART = PORT
        NEND   = PORT
*   Else update them all
      ELSE
        NSTART = 0
        NEND   = MAXPORT
      ENDIF

*   Write out an info message
      CALL MSG_SETI( 'NSTART', NSTART )
      CALL MSG_SETI( 'NEND', NEND )
      CALL MSG_OUT( ' ', 'Reading NBS values (Port ^NSTART - ^NEND)', STATUS )

*   Read the parameters from the common block
      DO I = NSTART, NEND, 1

        IJUNK = 0
        CALL NBS_GET_VALUE( CONTOUR_LEVELS_ID(I), 0, VAL__NBI, IJUNK, ACTBYTES, STATUS )
        CALL MSG_SETI( 'IJUNK', IJUNK )
        CALL MSG_OUT( ' ', 'Number of contour levels = ^IJUNK', STATUS )
        CALL NBS_GET_VALUE( HISTOGRAM_BINS_ID(I), 0, VAL__NBI, IJUNK, ACTBYTES, STATUS )
        CALL MSG_SETI( 'IJUNK', IJUNK )
        CALL MSG_OUT( ' ', 'Number of histogram bins = ^IJUNK', STATUS )
        CALL NBS_GET_VALUE( HISTOGRAM_XSTEP_ID(I), 0, VAL__NBI, IJUNK, ACTBYTES, STATUS )
        CALL MSG_SETI( 'IJUNK', IJUNK )
        CALL MSG_OUT( ' ', 'Histogram X-step = ^IJUNK', STATUS )
        CALL NBS_GET_VALUE( HISTOGRAM_YSTEP_ID(I), 0, VAL__NBI, IJUNK, ACTBYTES, STATUS )
        CALL MSG_SETI( 'IJUNK', IJUNK )
        CALL MSG_OUT( ' ', 'Histogram Y-step = ^IJUNK', STATUS )
        CALL NBS_GET_VALUE( HIST_SMOOTH_ID(I), 0, VAL__NBI, IJUNK, ACTBYTES, STATUS )
        CALL MSG_SETI( 'IJUNK', IJUNK )
        CALL MSG_OUT( ' ', 'Histogram smoothing step = ^IJUNK', STATUS )
        CALL NBS_GET_VALUE( TOOSMALL_ID(I), 0, VAL__NBI, IJUNK, ACTBYTES, STATUS )
        CALL MSG_SETI( 'IJUNK', IJUNK )
        CALL MSG_OUT( ' ', 'Number of points too small = ^IJUNK', STATUS )
        CALL NBS_GET_VALUE( TOOLARGE_ID(I), 0, VAL__NBI, IJUNK, ACTBYTES, STATUS )
        CALL MSG_SETI( 'IJUNK', IJUNK )
        CALL MSG_OUT( ' ', 'Number of points too large  = ^IJUNK', STATUS )
        CALL NBS_GET_VALUE( ISTART_ID(I), 0, VAL__NBI, IJUNK, ACTBYTES, STATUS )
        CALL MSG_SETI( 'IJUNK', IJUNK )
        CALL MSG_OUT( ' ', 'I-start = ^IJUNK', STATUS )
        CALL NBS_GET_VALUE( IEND_ID(I), 0, VAL__NBI, IJUNK, ACTBYTES, STATUS )
        CALL MSG_SETI( 'IJUNK', IJUNK )
        CALL MSG_OUT( ' ', 'I-end = ^IJUNK', STATUS )
        CALL NBS_GET_VALUE( JSTART_ID(I), 0, VAL__NBI, IJUNK, ACTBYTES, STATUS )
        CALL MSG_SETI( 'IJUNK', IJUNK )
        CALL MSG_OUT( ' ', 'J-start = ^IJUNK', STATUS )
        CALL NBS_GET_VALUE( JEND_ID(I), 0, VAL__NBI, IJUNK, ACTBYTES, STATUS )
        CALL MSG_SETI( 'IJUNK', IJUNK )
        CALL MSG_OUT( ' ', 'J-end = ^IJUNK', STATUS )

        LJUNK = .FALSE.
        CALL NBS_GET_VALUE( PLOT_AXES_ID(I), 0, VAL__NBI, LJUNK, ACTBYTES, STATUS )
        CALL MSG_SETL( 'LJUNK', LJUNK )
        CALL MSG_OUT( ' ', 'Plot axes = ^LJUNK', STATUS )
        CALL NBS_GET_VALUE( PLOT_ERRORS_ID(I), 0, VAL__NBI, LJUNK, ACTBYTES, STATUS )
        CALL MSG_SETL( 'LJUNK', LJUNK )
        CALL MSG_OUT( ' ', 'Plot errors = ^LJUNK', STATUS )
        CALL NBS_GET_VALUE( PLOT_WHOLE_ID(I), 0, VAL__NBI, LJUNK, ACTBYTES, STATUS )
        CALL MSG_SETL( 'LJUNK', LJUNK )
        CALL MSG_OUT( ' ', 'Plot whole = ^LJUNK', STATUS )
        CALL NBS_GET_VALUE( PRE_ERASE_PLOT_ID(I), 0, VAL__NBI, LJUNK, ACTBYTES, STATUS )
        CALL MSG_SETL( 'LJUNK', LJUNK )
        CALL MSG_OUT( ' ', 'Pre-erase plot = ^LJUNK', STATUS )
        CALL NBS_GET_VALUE( AUTOSCALE_ID(I), 0, VAL__NBI, LJUNK, ACTBYTES, STATUS )
        CALL MSG_SETL( 'LJUNK', LJUNK )
        CALL MSG_OUT( ' ', 'Autoscale = ^LJUNK', STATUS )
        CALL NBS_GET_VALUE( PORT_OK_ID(I), 0, VAL__NBI, LJUNK, ACTBYTES, STATUS )
        CALL MSG_SETL( 'LJUNK', LJUNK )
        CALL MSG_OUT( ' ', 'Port OK = ^LJUNK', STATUS )
        CALL NBS_GET_VALUE( PLOT_OK_ID(I), 0, VAL__NBI, LJUNK, ACTBYTES, STATUS )
        CALL MSG_SETL( 'LJUNK', LJUNK )
        CALL MSG_OUT( ' ', 'Plot OK = ^LJUNK', STATUS )

        RJUNK = 0.0
        CALL NBS_GET_VALUE( VXSTART_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'VX-start = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( VXEND_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'VX-end = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( VYSTART_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'VY-start = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( VYEND_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'VY-end = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( AXSTART_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'AX-start = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( AXEND_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'AX-end = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( AYSTART_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'AY-start = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( AYEND_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'AY-end = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( XSTART_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'X-start = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( XEND_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'X-end = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( YSTART_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'Y-start = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( YEND_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'Y-end = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( MODE_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'Mode = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( MEAN_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'Mean = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( SIGMA_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'Sigma = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( LOW_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'Low = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( HIGH_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'High = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( FMIN_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'Minimum frequency = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( FMAX_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'Maximum frequency = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( SLICE_START_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'Slice start = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( SLICE_END_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'Slice end = ^RJUNK', STATUS )
        CALL NBS_GET_VALUE( CHAR_HEIGHT_ID(I), 0, VAL__NBR, RJUNK, ACTBYTES, STATUS )
        CALL MSG_SETR( 'RJUNK', RJUNK )
        CALL MSG_OUT( ' ', 'Character height = ^RJUNK', STATUS )

        CALL CHR_FILL( ' ', CJUNK )
        CALL NBS_GET_CVALUE( BG_COLOUR_ID(I), 0, CJUNK, ACTBYTES, STATUS )
        CALL MSG_SETC( 'CJUNK', CJUNK )
        CALL MSG_OUT( ' ', 'Background colour = ^CJUNK', STATUS )
        CALL CHR_FILL( ' ', CJUNK )
        CALL NBS_GET_CVALUE( COLOUR_STYLE_ID(I), 0, CJUNK, ACTBYTES, STATUS )
        CALL MSG_SETC( 'CJUNK', CJUNK )
        CALL MSG_OUT( ' ', 'Colour style = ^CJUNK', STATUS )
        CALL CHR_FILL( ' ', CJUNK )
        CALL NBS_GET_CVALUE( CONTOUR_TYPE_ID(I), 0, CJUNK, ACTBYTES, STATUS )
        CALL MSG_SETC( 'CJUNK', CJUNK )
        CALL MSG_OUT( ' ', 'Contour type = ^CJUNK', STATUS )
        CALL CHR_FILL( ' ', CJUNK )
        CALL NBS_GET_CVALUE( CUT_DIRECTION_ID(I), 0, CJUNK, ACTBYTES, STATUS )
        CALL MSG_SETC( 'CJUNK', CJUNK )
        CALL MSG_OUT( ' ', 'Cut direction = ^CJUNK', STATUS )
        CALL CHR_FILL( ' ', CJUNK )
        CALL NBS_GET_CVALUE( DEVICE_LUT_ID(I), 0, CJUNK, ACTBYTES, STATUS )
        CALL MSG_SETC( 'CJUNK', CJUNK )
        CALL MSG_OUT( ' ', 'Device LUT = ^CJUNK', STATUS )
        CALL CHR_FILL( ' ', CJUNK )
        CALL NBS_GET_CVALUE( DEVICE_NAME_ID(I), 0, CJUNK, ACTBYTES, STATUS )
        CALL MSG_SETC( 'CJUNK', CJUNK )
        CALL MSG_OUT( ' ', 'Device name = ^CJUNK', STATUS )
        CALL CHR_FILL( ' ', CJUNK )
        CALL NBS_GET_CVALUE( DEVICE_XOPT_ID(I), 0, CJUNK, ACTBYTES, STATUS )
        CALL MSG_SETC( 'CJUNK', CJUNK )
        CALL MSG_OUT( ' ', 'Device X-options = ^CJUNK', STATUS )
        CALL CHR_FILL( ' ', CJUNK )
        CALL NBS_GET_CVALUE( DEVICE_YOPT_ID(I), 0, CJUNK, ACTBYTES, STATUS )
        CALL MSG_SETC( 'CJUNK', CJUNK )
        CALL MSG_OUT( ' ', 'Device Y-options = ^CJUNK', STATUS )
        CALL CHR_FILL( ' ', CJUNK )
        CALL NBS_GET_CVALUE( DISPLAY_DATA_ID(I), 0, CJUNK, ACTBYTES, STATUS )
        CALL MSG_SETC( 'CJUNK', CJUNK )
        CALL MSG_OUT( ' ', 'Display data = ^CJUNK', STATUS )
        CALL CHR_FILL( ' ', CJUNK )
        CALL NBS_GET_CVALUE( DISPLAY_PLANE_ID(I), 0, CJUNK, ACTBYTES, STATUS )
        CALL MSG_SETC( 'CJUNK', CJUNK )
        CALL MSG_OUT( ' ', 'Display plane = ^CJUNK', STATUS )
        CALL CHR_FILL( ' ', CJUNK )
        CALL NBS_GET_CVALUE( DISPLAY_TYPE_ID(I), 0, CJUNK, ACTBYTES, STATUS )
        CALL MSG_SETC( 'CJUNK', CJUNK )
        CALL MSG_OUT( ' ', 'Display type = ^CJUNK', STATUS )
        CALL CHR_FILL( ' ', CJUNK )
        CALL NBS_GET_CVALUE( FG_COLOUR_ID(I), 0, CJUNK, ACTBYTES, STATUS )
        CALL MSG_SETC( 'CJUNK', CJUNK )
        CALL MSG_OUT( ' ', 'Foreground colour = ^CJUNK', STATUS )
        CALL CHR_FILL( ' ', CJUNK )
        CALL NBS_GET_CVALUE( LAST_TYPE_ID(I), 0, CJUNK, ACTBYTES, STATUS )
        CALL MSG_SETC( 'CJUNK', CJUNK )
        CALL MSG_OUT( ' ', 'Last type = ^CJUNK', STATUS )
        CALL CHR_FILL( ' ', CJUNK )
        CALL NBS_GET_CVALUE( OVERCOLOUR_ID(I), 0, CJUNK, ACTBYTES, STATUS )
        CALL MSG_SETC( 'CJUNK', CJUNK )
        CALL MSG_OUT( ' ', 'Overcolour = ^CJUNK', STATUS )
        CALL CHR_FILL( ' ', CJUNK )
        CALL NBS_GET_CVALUE( TITLE_ID(I), 0, CJUNK, ACTBYTES, STATUS )
        CALL MSG_SETC( 'CJUNK', CJUNK )
        CALL MSG_OUT( ' ', 'Title = ^CJUNK', STATUS )
        CALL MSG_SYNC( STATUS )
      ENDDO

      IF ( STATUS .NE. SAI__OK ) THEN
        IF ( STATUS .EQ. NBS__NILID ) THEN
          CALL MSG_OUT( ' ', 'Noticeboard not yet defined', STATUS )
        ELSE
          ERR_STAT = STATUS
          STATUS = SAI__ERROR
          CALL MSG_SETI( 'ES', ERR_STAT )
          CALL ERR_REP( ' ', 'P4_LIST_NB: '/
     :      /'Failed to read noticeboard values, Status = ^ES', STATUS )
        ENDIF
      ELSE
        CALL MSG_OUT( ' ', 'Read noticeboard values OK', STATUS )
      ENDIF


      END
