*+  P4COM.INC - INCLUDE file for P4 plotting D-task
*    Description :
*     This is the INCLUDE file which contains the common blocks for P4
*    Authors :
*     P N Daly   (JACH::PND)
*    History :
*     10-Aug-1994: Extensively re-formatted for Unix port       (PND)
*    endhistory
*-

*    Global constants :
      INTEGER MAXPORT                           ! Maximum port number
      PARAMETER ( MAXPORT = 8 )
      INTEGER PG__OK                            ! PGPLOT success status
      PARAMETER ( PG__OK = 1 )
      INTEGER NGUNS                             ! Number of colour guns
      PARAMETER ( NGUNS = 3 )
      INTEGER NLEVELS                           ! Number of CT levels
      PARAMETER ( NLEVELS = 256 )
      INTEGER NBS_FLEN                          ! Length of NBS items
      PARAMETER ( NBS_FLEN = 80 )
      INTEGER TRANSIZE                          ! Size of TRAN array
      PARAMETER ( TRANSIZE = 6 )

*    Miscellaneous integer common block variables :
      INTEGER
     :  NB_TOPID,                        ! Top-level of noticeboard
     :  TASK_BUSY_ID,                    ! Task busy flag
     :  PORT_ID( 0:MAXPORT ),            ! Port identifier
     :  CI1( 0:MAXPORT ),                ! Lowest available colour index
     :  CI2( 0:MAXPORT ),                ! Highest available colour index
     :  ISLICE_START,                    ! Pixel start of slice
     :  ISLICE_END,                      ! Pixel end of slice
     :  AXIS1_PTR, AXIS1_SLT,            ! Pointer and slot for X-axis
     :  AXIS2_PTR, AXIS2_SLT             ! Pointer and slot for Y-axis

      COMMON /P4INT/ 
     :  NB_TOPID, TASK_BUSY_ID, PORT_ID, CI1, CI2, ISLICE_START, ISLICE_END,
     :  AXIS1_PTR, AXIS1_SLT, AXIS2_PTR, AXIS2_SLT
      SAVE /P4INT/

*    Miscellaneous character common block variables :
      CHARACTER*( NBS_FLEN )
     :  DEFTITLE( 0:MAXPORT ),           ! Default title for plot
     :  SUBTITLE( 0:MAXPORT ),           ! Subtitle
     :  XLABEL( 0:MAXPORT ),             ! X-axis label
     :  YLABEL( 0:MAXPORT ),             ! Y-axis label
     :  RH1LABEL( 0:MAXPORT ),           ! RH edge label
     :  RH2LABEL( 0:MAXPORT ),           ! RH edge label
     :  RH3LABEL( 0:MAXPORT ),           ! RH edge label
     :  SYSNAME,                         ! System name 
     :  CURRENT_DEVICE,                  ! Current device open
     :  NOTICEBOARD                      ! Name of noticeboard
      CHARACTER*1
     :  PREFIX,                          ! Prefix for filenames
     :  SEPARATOR                        ! File separator

      COMMON /P4CHAR/ 
     :  DEFTITLE, SUBTITLE, XLABEL, YLABEL, RH1LABEL, RH2LABEL, 
     :  RH3LABEL, SYSNAME, CURRENT_DEVICE, NOTICEBOARD,
     :  PREFIX, SEPARATOR
      SAVE /P4CHAR/

      CHARACTER*( NBS_FLEN )
     :  P4_HOME,                          ! Home directory
     :  P4_MASKS,                         ! Masks directory
     :  P4_CT,                            ! Colour tables directory
     :  P4_CONFIG,                        ! Configuration directory
     :  P4_DATA,                          ! Data directory (home)
     :  P4_DATE,                          ! Root date
     :  RGDIR,                            ! Reduced group directory
     :  RODIR,                            ! Reduced obs directory
     :  RIDIR,                            ! Reduced int directory
     :  ODIR,                             ! Raw observation directory
     :  IDIR                              ! Raw integration directory

      COMMON /P4DIR/
     :  P4_HOME, P4_MASKS, P4_CT, P4_CONFIG, P4_DATA,
     :  P4_DATE, RGDIR, RODIR, RIDIR, ODIR, IDIR
      SAVE /P4DIR/

*     Miscellaneous real comon block variables :
      REAL
     :  TMATRIX( TRANSIZE ),             ! Transformation matrix
     :  ARRAY_CONTOURS( NLEVELS )        ! Array of contour levels

      COMMON /P4REAL/ TMATRIX, ARRAY_CONTOURS
      SAVE /P4REAL/

*    Define character items in NBS
      CHARACTER*( NBS_FLEN )
     :  DISPLAY_DATA( 0:MAXPORT ),       ! Name of file to display
     :  DEVICE_LUT( 0:MAXPORT ),         ! Colour table
     :  TITLE( 0:MAXPORT ),              ! User specified title
     :  DEVICE_NAME( 0:MAXPORT ),        ! Name of graphics device 
     :  DEVICE_XOPT( 0:MAXPORT ),        ! X labelling options
     :  DEVICE_YOPT( 0:MAXPORT ),        ! Y labelling options
     :  DISPLAY_TYPE( 0:MAXPORT ),       ! Type of plot
     :  DISPLAY_PLANE( 0:MAXPORT ),      ! Data, Quality or Errors
     :  CONTOUR_TYPE( 0:MAXPORT ),       ! Type of contour plot
     :  OVERCOLOUR( 0:MAXPORT ),         ! Colour of overgraph line
     :  COLOUR_STYLE( 0:MAXPORT ),       ! Monochrome or Colour
     :  FG_COLOUR( 0:MAXPORT ),          ! Foreground colour
     :  BG_COLOUR( 0:MAXPORT ),          ! Background colour
     :  CUT_DIRECTION( 0:MAXPORT ),      ! Direction of cut
     :  LAST_TYPE( 0:MAXPORT )           ! Last display type

      COMMON /P4NBS2/ 
     :  DISPLAY_DATA, DEVICE_LUT, TITLE, DEVICE_NAME, DEVICE_XOPT, 
     :  DEVICE_YOPT, DISPLAY_TYPE, DISPLAY_PLANE, CONTOUR_TYPE, 
     :  OVERCOLOUR, COLOUR_STYLE,  FG_COLOUR, BG_COLOUR, 
     :  CUT_DIRECTION, LAST_TYPE
      SAVE /P4NBS2/

*    Define character item IDs in NBS
      INTEGER
     :  DISPLAY_DATA_ID( 0:MAXPORT ),    ! 
     :  DEVICE_LUT_ID( 0:MAXPORT ),      ! 
     :  TITLE_ID( 0:MAXPORT ),           !
     :  DEVICE_NAME_ID( 0:MAXPORT ),     ! 
     :  DEVICE_XOPT_ID( 0:MAXPORT ),     ! 
     :  DEVICE_YOPT_ID( 0:MAXPORT ),     ! 
     :  DISPLAY_TYPE_ID( 0:MAXPORT ),    ! 
     :  DISPLAY_PLANE_ID( 0:MAXPORT ),   ! 
     :  CONTOUR_TYPE_ID( 0:MAXPORT ),    ! 
     :  OVERCOLOUR_ID( 0:MAXPORT ),      ! 
     :  COLOUR_STYLE_ID( 0:MAXPORT ),    ! 
     :  FG_COLOUR_ID( 0:MAXPORT ),       ! 
     :  BG_COLOUR_ID( 0:MAXPORT ),       ! 
     :  CUT_DIRECTION_ID( 0:MAXPORT ),   ! 
     :  LAST_TYPE_ID( 0:MAXPORT )        ! 

      COMMON /P4NBS3/
     :  DISPLAY_DATA_ID, DEVICE_LUT_ID, TITLE_ID, DEVICE_NAME_ID, 
     :  DEVICE_XOPT_ID, DEVICE_YOPT_ID, DISPLAY_TYPE_ID,
     :  DISPLAY_PLANE_ID, CONTOUR_TYPE_ID, OVERCOLOUR_ID,
     :  COLOUR_STYLE_ID, FG_COLOUR_ID, BG_COLOUR_ID, 
     :  CUT_DIRECTION_ID, LAST_TYPE_ID
      SAVE /P4NBS3/
      
*    Define logical items in NBS
      LOGICAL
     :  PLOT_AXES( 0:MAXPORT ),          ! Plot axes?
     :  PLOT_ERRORS( 0:MAXPORT ),        ! Plot error bars?
     :  PLOT_WHOLE( 0:MAXPORT ),         ! Plot whole array?
     :  PRE_ERASE_PLOT( 0:MAXPORT ),     ! Pre erase the display?
     :  AUTOSCALE( 0:MAXPORT ),          ! Autoscale the display?
     :  PORT_OK( 0:MAXPORT ),            ! T if port setup OK
     :  PLOT_OK( 0:MAXPORT ),            ! T if plot was OK
     :  VERBOSE                          ! T for verbose output

      COMMON /P4NBS4/
     :  PLOT_AXES, PLOT_ERRORS, PLOT_WHOLE, PRE_ERASE_PLOT,
     :  AUTOSCALE, PORT_OK, PLOT_OK, VERBOSE
      SAVE /P4NBS4/

*    Define logical item IDs in NBS
      INTEGER
     :  PLOT_AXES_ID( 0:MAXPORT ),       ! 
     :  PLOT_ERRORS_ID( 0:MAXPORT ),     ! 
     :  PLOT_WHOLE_ID( 0:MAXPORT ),      ! 
     :  PRE_ERASE_PLOT_ID( 0:MAXPORT ),  ! 
     :  AUTOSCALE_ID( 0:MAXPORT ),       ! 
     :  PORT_OK_ID( 0:MAXPORT ),         ! 
     :  PLOT_OK_ID( 0:MAXPORT )          ! 

      COMMON /P4NBS5/
     :  PLOT_AXES_ID, PLOT_ERRORS_ID, PLOT_WHOLE_ID, PRE_ERASE_PLOT_ID, 
     :  AUTOSCALE_ID, PORT_OK_ID, PLOT_OK_ID
      SAVE /P4NBS5/

*    Define integer items in NBS
      INTEGER
     :  CONTOUR_LEVELS( 0:MAXPORT ),     ! Number of contour levels
     :  HISTOGRAM_BINS( 0:MAXPORT ),     ! Number of bins in histogram
     :  HISTOGRAM_XSTEP( 0:MAXPORT ),    ! X increment for histogramming
     :  HISTOGRAM_YSTEP( 0:MAXPORT ),    ! Y increment for histogramming
     :  HIST_SMOOTH( 0:MAXPORT ),        ! Smooth value for histogram
     :  TOOSMALL( 0:MAXPORT ),           ! Number of values too small
     :  TOOLARGE( 0:MAXPORT ),           ! Number of values too large
     :  ISTART( 0:MAXPORT ),             ! Pixel Xstart
     :  IEND( 0:MAXPORT ),               ! Pixel Xend
     :  JSTART( 0:MAXPORT ),             ! Pixel Ystart
     :  JEND( 0:MAXPORT )                ! Pixel Yend

      COMMON /P4NBS6/
     :  CONTOUR_LEVELS, HISTOGRAM_BINS, HISTOGRAM_XSTEP, 
     :  HISTOGRAM_YSTEP, HIST_SMOOTH, TOOSMALL, TOOLARGE, ISTART, 
     :  IEND, JSTART, JEND
      SAVE /P4NBS6/

*    Define integer item IDs in NBS
      INTEGER
     :  CONTOUR_LEVELS_ID( 0:MAXPORT ),  ! 
     :  HISTOGRAM_BINS_ID( 0:MAXPORT ),  ! 
     :  HISTOGRAM_XSTEP_ID( 0:MAXPORT ), ! 
     :  HISTOGRAM_YSTEP_ID( 0:MAXPORT ), ! 
     :  HIST_SMOOTH_ID( 0:MAXPORT ),     ! 
     :  TOOSMALL_ID( 0:MAXPORT ),        ! 
     :  TOOLARGE_ID( 0:MAXPORT ),        ! 
     :  ISTART_ID( 0:MAXPORT ),          ! 
     :  IEND_ID( 0:MAXPORT ),            ! 
     :  JSTART_ID( 0:MAXPORT ),          ! 
     :  JEND_ID( 0:MAXPORT )             ! 

      COMMON /P4NBS7/
     :  CONTOUR_LEVELS_ID, HISTOGRAM_BINS_ID, HISTOGRAM_XSTEP_ID, 
     :  HISTOGRAM_YSTEP_ID, HIST_SMOOTH_ID, TOOSMALL_ID, TOOLARGE_ID, 
     :  ISTART_ID, IEND_ID, JSTART_ID, JEND_ID
      SAVE /P4NBS7/

*    Define real items in NBS
      REAL
     :  VXSTART( 0:MAXPORT ),            ! Viewport Xstart
     :  VXEND( 0:MAXPORT ),              ! Viewport Xend
     :  VYSTART( 0:MAXPORT ),            ! Viewport Ystart
     :  VYEND( 0:MAXPORT ),              ! Viewport Yend
     :  AXSTART( 0:MAXPORT ),            ! Actual (viewport) Xstart
     :  AXEND( 0:MAXPORT ),              ! Actual (viewport) Xend
     :  AYSTART( 0:MAXPORT ),            ! Actual (viewport) Ystart
     :  AYEND( 0:MAXPORT ),              ! Actual (viewport) Yend
     :  XSTART( 0:MAXPORT ),             ! Data Xstart
     :  XEND( 0:MAXPORT ),               ! Data Xend
     :  YSTART( 0:MAXPORT ),             ! Data Ystart
     :  YEND( 0:MAXPORT )                ! Data Yend

      COMMON /P4NBS8/
     :  VXSTART, VXEND, VYSTART, VYEND, AXSTART, AXEND, 
     :  AYSTART, AYEND, XSTART, XEND, YSTART, YEND
      SAVE /P4NBS8/

      REAL
     :  MODE( 0:MAXPORT ),               ! Data mode value
     :  MEAN( 0:MAXPORT ),               ! Data mean value
     :  SIGMA( 0:MAXPORT ),              ! Data sigma value
     :  LOW( 0:MAXPORT ),                ! Data low value
     :  HIGH( 0:MAXPORT ),               ! Data high value
     :  FMIN( 0:MAXPORT ),               ! Data lowest frequency
     :  FMAX( 0:MAXPORT ),               ! Data highest frequency
     :  SLICE_START( 0:MAXPORT ),        ! Start of slice
     :  SLICE_END( 0:MAXPORT ),          ! End of slice
     :  CHAR_HEIGHT( 0:MAXPORT )         ! Height of characters

      COMMON /P4NBS9/
     :  MODE, MEAN, SIGMA, LOW, HIGH, FMIN, FMAX,
     :  SLICE_START, SLICE_END, CHAR_HEIGHT
      SAVE /P4NBS9/

*    Define real item IDs in NBS
      INTEGER
     :  VXSTART_ID( 0:MAXPORT ),            ! 
     :  VXEND_ID( 0:MAXPORT ),              ! 
     :  VYSTART_ID( 0:MAXPORT ),            ! 
     :  VYEND_ID( 0:MAXPORT ),              ! 
     :  AXSTART_ID( 0:MAXPORT ),            ! 
     :  AXEND_ID( 0:MAXPORT ),              ! 
     :  AYSTART_ID( 0:MAXPORT ),            ! 
     :  AYEND_ID( 0:MAXPORT ),              ! 
     :  XSTART_ID( 0:MAXPORT ),             ! 
     :  XEND_ID( 0:MAXPORT ),               ! 
     :  YSTART_ID( 0:MAXPORT ),             ! 
     :  YEND_ID( 0:MAXPORT )                ! 

      COMMON /P4NBSA/
     :  VXSTART_ID, VXEND_ID, VYSTART_ID, VYEND_ID, AXSTART_ID, 
     :  AXEND_ID, AYSTART_ID, AYEND_ID, XSTART_ID, XEND_ID,
     :  YSTART_ID, YEND_ID
      SAVE /P4NBSA/

      INTEGER
     :  MODE_ID( 0:MAXPORT ),               ! 
     :  MEAN_ID( 0:MAXPORT ),               ! 
     :  SIGMA_ID( 0:MAXPORT ),              ! 
     :  LOW_ID( 0:MAXPORT ),                ! 
     :  HIGH_ID( 0:MAXPORT ),               ! 
     :  FMIN_ID( 0:MAXPORT ),               ! 
     :  FMAX_ID( 0:MAXPORT ),               ! 
     :  SLICE_START_ID( 0:MAXPORT ),        ! 
     :  SLICE_END_ID( 0:MAXPORT ),          ! 
     :  CHAR_HEIGHT_ID( 0:MAXPORT )         ! 

      COMMON /P4NBSB/
     :  MODE_ID, MEAN_ID, SIGMA_ID, LOW_ID, HIGH_ID, FMIN_ID,
     :  FMAX_ID, SLICE_START_ID, SLICE_END_ID, CHAR_HEIGHT_ID
      SAVE /P4NBSB/

*    End of common block

