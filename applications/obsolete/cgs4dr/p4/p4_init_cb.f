*+  P4_INIT_CB - Initialise the common block values to pre-defined state
      SUBROUTINE P4_INIT_CB( PORT, STATUS )
*    Invocation :
*     CALL P4_INIT_CB( PORT, STATUS )
*    Authors :
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*      5-Aug-1994: Original version.                            (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import:
      INTEGER PORT
*    Status :
      INTEGER STATUS                        ! Global status
*    Global variables :
      INCLUDE 'P4COM.INC'                   ! P4 common block
*    Local variables :
      INTEGER NSTART, NEND, I               ! Loop variables
*    Local data :
      REAL VPXSTART( 0:MAXPORT )
      DATA VPXSTART /0.2,0.1,0.6,0.1,0.6,0.1,0.1,0.1,0.6/
      REAL VPXEND( 0:MAXPORT )
      DATA VPXEND   /0.8,0.4,0.9,0.4,0.9,0.9,0.9,0.4,0.9/
      REAL VPYSTART( 0:MAXPORT )
      DATA VPYSTART /0.2,0.6,0.6,0.1,0.1,0.6,0.1,0.1,0.1/
      REAL VPYEND( 0:MAXPORT )
      DATA VPYEND   /0.8,0.9,0.9,0.4,0.4,0.9,0.4,0.9,0.9/
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   If 0 <= PORT <= MAXPORT update just that port
      IF ( PORT.GE.0 .AND. PORT.LE.MAXPORT ) THEN
        NSTART = PORT
        NEND   = PORT
*   Else update all ports
      ELSE
        NSTART = 0
        NEND   = MAXPORT
      ENDIF

*   Set the common block values
      DO I = NSTART, NEND, 1

        IF ( VERBOSE ) THEN
           CALL MSG_SETI( 'I', I )
           CALL MSG_OUT( ' ',
     :       'Initialising common block for port ^I', STATUS )
        ENDIF

        CALL CHR_FILL( ' ', DEVICE_NAME(I) )
        DEVICE_NAME(I) = 'xwindows'
        CALL CHR_FILL( ' ', DEVICE_XOPT(I) )
        DEVICE_XOPT(I) = 'BCNTSI'
        CALL CHR_FILL( ' ', DEVICE_YOPT(I) )
        DEVICE_YOPT(I) = 'BCNTSI'
        CALL CHR_FILL( ' ', DEVICE_LUT(I) )
        DEVICE_LUT(I) = 'default'
        CALL CHR_FILL( ' ', DISPLAY_DATA(I) )
        DISPLAY_DATA(I) = 'Name_of_DSA_data_structure'
        CALL CHR_FILL( ' ', TITLE(I) )
        TITLE(I) = 'A_U_T_O'
        CALL CHR_FILL( ' ', DISPLAY_TYPE(I) )
        DISPLAY_TYPE(I) = 'IMAGE'
        CALL CHR_FILL( ' ', DISPLAY_PLANE(I) )
        DISPLAY_PLANE(I) = 'DATA'
        CALL CHR_FILL( ' ', CONTOUR_TYPE(I) )
        CONTOUR_TYPE(I) = 'LIN'
        CALL CHR_FILL( ' ', OVERCOLOUR(I) )
        OVERCOLOUR(I) = 'RED'
        CALL CHR_FILL( ' ', COLOUR_STYLE(I) )
        COLOUR_STYLE(I) = 'COLOUR'
        CALL CHR_FILL( ' ', FG_COLOUR(I) )
        FG_COLOUR(I) = 'BLACK'
        CALL CHR_FILL( ' ', BG_COLOUR(I) )
        BG_COLOUR(I) = 'WHITE'
        CALL CHR_FILL( ' ', CUT_DIRECTION(I) )
        CUT_DIRECTION(I) = 'X'
        CALL CHR_FILL( ' ', LAST_TYPE(I) )
        LAST_TYPE(I) = 'NONE'

        PLOT_AXES(I) = .TRUE.
        PLOT_ERRORS(I) = .FALSE.
        PLOT_WHOLE(I) = .TRUE.
        PRE_ERASE_PLOT(I) = .TRUE.
        AUTOSCALE(I) = .TRUE.
        PORT_OK(I) = .FALSE.
        PLOT_OK(I) = .FALSE.

        CONTOUR_LEVELS(I) = 10
        HISTOGRAM_BINS(I) = 50
        HISTOGRAM_XSTEP(I) = 1
        HISTOGRAM_YSTEP(I) = 1
        HIST_SMOOTH(I) = 1
        TOOSMALL(I) = 0
        TOOLARGE(I) = 0
        ISTART(I) = -1
        IEND(I) = -1
        JSTART(I) = -1
        JEND(I) = -1

        VXSTART(I) = VPXSTART(I)
        VXEND(I) = VPXEND(I)
        VYSTART(I) = VPYSTART(I)
        VYEND(I) = VPYEND(I)
        AXSTART(I) = VPXSTART(I)
        AXEND(I) = VPXEND(I)
        AYSTART(I) = VPYSTART(I)
        AYEND(I) = VPYEND(I)
        XSTART(I) = 0.0
        XEND(I) = 256.0
        YSTART(I) = 0.0
        YEND(I) = 256.0
        MODE(I) = 0.0
        MEAN(I) = 0.0
        SIGMA(I) = 0.0
        LOW(I) = 0.0
        HIGH(I) = 1000.0
        FMIN(I) = 0.0
        FMAX(I) = 3536.0
        SLICE_START(I) = 20.0
        SLICE_END(I) = 40.0
        CHAR_HEIGHT(I) = 1.0
      ENDDO

*    Set the current device status
      CALL CHR_FILL( ' ', CURRENT_DEVICE )
      CURRENT_DEVICE = 'NONE/CLOSED'

      END
