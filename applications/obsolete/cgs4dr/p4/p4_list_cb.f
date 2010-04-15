*+  P4_LIST_CB - List the common block values
      SUBROUTINE P4_LIST_CB( STATUS )
*    Invocation :
*     CALL P4_LIST_CB( STATUS )
*    Authors :
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*      5-Aug-1994: Original version.                            (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS                        ! Global status
*    Global variables :
      INCLUDE 'P4COM.INC'                   ! P4 common block
*    Local variables :
      INTEGER
     :  NSTART,                             ! Loop start
     :  NEND,                               ! Loop end
     :  I,                                  ! Loop index
     :  PORT                                ! Port number
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   If 0 <= PORT <= MAXPORT update just that port
      CALL PAR_GET0I( 'PORT', PORT, STATUS )
      IF ( ( PORT .GE. 0 ) .AND. ( PORT .LE. MAXPORT ) ) THEN
        NSTART = PORT
        NEND   = PORT
*   Else update all ports
      ELSE
        NSTART = 0
        NEND   = MAXPORT
      ENDIF

*   Set the common block values
      DO I = NSTART, NEND, 1

        CALL MSG_BLANK( STATUS )
        CALL MSG_SETI( 'I', I )
        CALL MSG_OUT( ' ', 'Common block values for port ^I are: ', STATUS )
        CALL MSG_BLANK( STATUS )

        CALL MSG_SETC( 'OUTPUT', DEVICE_NAME(I) )
        CALL MSG_OUT( ' ', 'Device name       = ^OUTPUT', STATUS )
        CALL MSG_SETC( 'OUTPUT', DEVICE_XOPT(I) )
        CALL MSG_OUT( ' ', 'X axis options    = ^OUTPUT', STATUS )
        CALL MSG_SETC( 'OUTPUT', DEVICE_YOPT(I) )
        CALL MSG_OUT( ' ', 'Y axis ptions     = ^OUTPUT', STATUS )
        CALL MSG_SETC( 'OUTPUT', DEVICE_LUT(I) )
        CALL MSG_OUT( ' ', 'Colour table      = ^OUTPUT', STATUS )
        CALL MSG_SETC( 'OUTPUT', DISPLAY_DATA(I) )
        CALL MSG_OUT( ' ', 'Datafile          = ^OUTPUT', STATUS )
        CALL MSG_SETC( 'OUTPUT', TITLE(I) )
        CALL MSG_OUT( ' ', 'Default title     = ^OUTPUT', STATUS )
        CALL MSG_SETC( 'OUTPUT', DISPLAY_TYPE(I) )
        CALL MSG_OUT( ' ', 'Plot type         = ^OUTPUT', STATUS )
        CALL MSG_SETC( 'OUTPUT', DISPLAY_PLANE(I) )
        CALL MSG_OUT( ' ', 'Display plane     = ^OUTPUT', STATUS )
        CALL MSG_SETC( 'OUTPUT', CONTOUR_TYPE(I) )
        CALL MSG_OUT( ' ', 'Contour type      = ^OUTPUT', STATUS )
        CALL MSG_SETC( 'OUTPUT', OVERCOLOUR(I) )
        CALL MSG_OUT( ' ', 'Overgraph colour  = ^OUTPUT', STATUS )
        CALL MSG_SETC( 'OUTPUT', COLOUR_STYLE(I) )
        CALL MSG_OUT( ' ', 'Colour style      = ^OUTPUT', STATUS )
        CALL MSG_SETC( 'OUTPUT', FG_COLOUR(I) )
        CALL MSG_OUT( ' ', 'Foreground        = ^OUTPUT', STATUS )
        CALL MSG_SETC( 'OUTPUT', BG_COLOUR(I) )
        CALL MSG_OUT( ' ', 'Background        = ^OUTPUT', STATUS )
        CALL MSG_SETC( 'OUTPUT', CUT_DIRECTION(I) )
        CALL MSG_OUT( ' ', 'Direction of cut  = ^OUTPUT', STATUS )
        CALL MSG_SETC( 'OUTPUT', LAST_TYPE(I) )
        CALL MSG_OUT( ' ', 'Last plot type    = ^OUTPUT', STATUS )
        CALL MSG_SETL( 'OUTPUT', PLOT_AXES(I) )
        CALL MSG_OUT( ' ', 'Plot axes         = ^OUTPUT', STATUS )
        CALL MSG_SETL( 'OUTPUT', PLOT_ERRORS(I) )
        CALL MSG_OUT( ' ', 'Plot error bars   = ^OUTPUT', STATUS )
        CALL MSG_SETL( 'OUTPUT', PLOT_WHOLE(I) )
        CALL MSG_OUT( ' ', 'Plot whole array  = ^OUTPUT', STATUS )
        CALL MSG_SETL( 'OUTPUT', PRE_ERASE_PLOT(I) )
        CALL MSG_OUT( ' ', 'Pre erase plot    = ^OUTPUT', STATUS )
        CALL MSG_SETL( 'OUTPUT', AUTOSCALE(I) )
        CALL MSG_OUT( ' ', 'Autoscale         = ^OUTPUT', STATUS )
        CALL MSG_SETL( 'OUTPUT', PORT_OK(I) )
        CALL MSG_OUT( ' ', 'Port_OK           = ^OUTPUT', STATUS )
        CALL MSG_SETL( 'OUTPUT', PLOT_OK(I) )
        CALL MSG_OUT( ' ', 'Plot_OK           = ^OUTPUT', STATUS )
        CALL MSG_SETI( 'OUTPUT', CONTOUR_LEVELS(I) )
        CALL MSG_OUT( ' ', 'Contour levels    = ^OUTPUT', STATUS )
        CALL MSG_SETI( 'OUTPUT', HISTOGRAM_BINS(I) )
        CALL MSG_OUT( ' ', 'Histogram bins    = ^OUTPUT', STATUS )
        CALL MSG_SETI( 'OUTPUT', HISTOGRAM_XSTEP(I) )
        CALL MSG_OUT( ' ', 'Histogram xstep   = ^OUTPUT', STATUS )
        CALL MSG_SETI( 'OUTPUT', HISTOGRAM_YSTEP(I) )
        CALL MSG_OUT( ' ', 'Histogram ystep   = ^OUTPUT', STATUS )
        CALL MSG_SETI( 'OUTPUT', HIST_SMOOTH(I) )
        CALL MSG_OUT( ' ', 'Histogram smooth  = ^OUTPUT', STATUS )
        CALL MSG_SETI( 'OUTPUT', TOOSMALL(I) )
        CALL MSG_OUT( ' ', 'Toosmall          = ^OUTPUT', STATUS )
        CALL MSG_SETI( 'OUTPUT', TOOLARGE(I) )
        CALL MSG_OUT( ' ', 'Toolarge          = ^OUTPUT', STATUS )
        CALL MSG_SETI( 'OUTPUT', ISTART(I) )
        CALL MSG_OUT( ' ', 'Pixel xstart      = ^OUTPUT', STATUS )
        CALL MSG_SETI( 'OUTPUT', IEND(I) )
        CALL MSG_OUT( ' ', 'Pixel xend        = ^OUTPUT', STATUS )
        CALL MSG_SETI( 'OUTPUT', JSTART(I) )
        CALL MSG_OUT( ' ', 'Pixel ystart      = ^OUTPUT', STATUS )
        CALL MSG_SETI( 'OUTPUT', JEND(I) )
        CALL MSG_OUT( ' ', 'Pixel yend        = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', VXSTART(I) )
        CALL MSG_OUT( ' ', 'Viewport xstart   = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', VXEND(I) )
        CALL MSG_OUT( ' ', 'Viewport xend     = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', VYSTART(I) )
        CALL MSG_OUT( ' ', 'Viewport ystart   = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', VYEND(I) )
        CALL MSG_OUT( ' ', 'Viewport yend     = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', AXSTART(I) )
        CALL MSG_OUT( ' ', 'Real port xstart  = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', AXEND(I) )
        CALL MSG_OUT( ' ', 'Real port xend    = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', AYSTART(I) )
        CALL MSG_OUT( ' ', 'Real port ystart  = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', AYEND(I) )
        CALL MSG_OUT( ' ', 'Real port ynd     = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', XSTART(I) )
        CALL MSG_OUT( ' ', 'Data xstart       = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', XEND(I) )
        CALL MSG_OUT( ' ', 'Data xend         = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', YSTART(I) )
        CALL MSG_OUT( ' ', 'Data ystart       = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', YEND(I) )
        CALL MSG_OUT( ' ', 'Data yend         = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', MEAN(I) )
        CALL MSG_OUT( ' ', 'Data mean         = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', MODE(I) )
        CALL MSG_OUT( ' ', 'Data mode         = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', SIGMA(I) )
        CALL MSG_OUT( ' ', 'Data sigma        = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', LOW(I) )
        CALL MSG_OUT( ' ', 'Data low value    = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', HIGH(I) )
        CALL MSG_OUT( ' ', 'Data high value   = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', FMIN(I) )
        CALL MSG_OUT( ' ', 'Minimum frequency = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', FMAX(I) )
        CALL MSG_OUT( ' ', 'Maximum frequency = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', SLICE_START(I) )
        CALL MSG_OUT( ' ', 'Start of slice    = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', SLICE_END(I) )
        CALL MSG_OUT( ' ', 'End of slice      = ^OUTPUT', STATUS )
        CALL MSG_SETR( 'OUTPUT', CHAR_HEIGHT(I) )
        CALL MSG_OUT( ' ', 'Character height  = ^OUTPUT', STATUS )
        CALL MSG_SYNC( STATUS )
      ENDDO

*    Report the current device, logical names etc
      CALL MSG_SETC( 'OUTPUT', CURRENT_DEVICE )
      CALL MSG_OUT( ' ', 'Current device is ^OUTPUT', STATUS )
      CALL MSG_SETC( 'P4_HOME', P4_HOME )
      CALL MSG_OUT( ' ', 'P4_HOME directory   = ^P4_HOME', STATUS )
      CALL MSG_SETC( 'P4_CT', P4_CT )
      CALL MSG_OUT( ' ', 'P4_CT directory     = ^P4_CT', STATUS )
      CALL MSG_SETC( 'P4_CONFIG', P4_CONFIG )
      CALL MSG_OUT( ' ', 'P4_CONFIG directory = ^P4_CONFIG', STATUS )
      CALL MSG_SETC( 'P4_DATA', P4_DATA )
      CALL MSG_OUT( ' ', 'P4_DATA directory   = ^P4_DATA', STATUS )
      CALL MSG_SETC( 'P4_DATE', P4_DATE)
      CALL MSG_OUT( ' ', 'Root UT-date        = ^P4_DATE', STATUS )
      CALL MSG_SETC( 'RGDIR', RGDIR )
      CALL MSG_OUT( ' ', 'RGDIR directory     = ^RGDIR', STATUS )
      CALL MSG_SETC( 'RODIR', RODIR )
      CALL MSG_OUT( ' ', 'RODIR directory     = ^RODIR', STATUS )
      CALL MSG_SETC( 'RIDIR', RIDIR )
      CALL MSG_OUT( ' ', 'RIDIR directory     = ^RIDIR', STATUS )
      CALL MSG_SETC( 'IDIR', IDIR )
      CALL MSG_OUT( ' ', 'IDIR directory      = ^IDIR', STATUS )
      CALL MSG_SETC( 'ODIR', ODIR )
      CALL MSG_OUT( ' ', 'ODIR directory      = ^ODIR', STATUS )
      END
