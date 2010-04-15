*+  P4_HISTOGRAM - Display a dataset as a HISTOGRAM
      SUBROUTINE P4_HISTOGRAM( PORT, STATUS )
*    Description :
*     This routine displays a dataset as a HISTOGRAM
*    Invocation :
*     CALL P4_HISTOGRAM( PORT, STATUS )
*    Authors :
*     P. N. Daly ( JACH::PND )
*    History :
*     16-Aug-1994: Original Unix version (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Import :
      INTEGER PORT                            ! Number of viewport for plot
*    External references :
      INTEGER CHR_LEN                         ! String length function
      INTEGER DSA_TYPESIZE
*    Global variables :
      INCLUDE 'P4COM.INC'                     ! P4 common block
      INCLUDE 'COLOURS.INC'                   ! Colours common block
*    Local Constants :
      INTEGER MAXDIM                          ! Max number of dimensions
      PARAMETER ( MAXDIM = 3 )
*    Local variables :
      LOGICAL SAME_UNITS
      LOGICAL ERRORS                          ! T if want to plot errors
      LOGICAL QUALITY                         ! T if want to plot quality
      INTEGER BYTES                           ! Number of bytes required
      INTEGER SLEN                            ! Length of sub-title string
      INTEGER TLEN                            ! Length of title string
      INTEGER XLEN                            ! Length of X-axis string
      INTEGER YLEN                            ! Length of Y-axis string
      INTEGER CPOS                            ! Length of general string
      INTEGER NDIM                            ! Info on size of data array
      INTEGER DIMS( MAXDIM )                  !              "
      INTEGER NELM                            !              "
      INTEGER DATA_SLOT                       ! for data array
      INTEGER DATA_PTR                        !              "
      INTEGER QUAL_SLOT                       ! for quality array
      INTEGER QUAL_PTR                        !              "
      INTEGER BIN_SLOT                        ! slot for bin co-ordinates
      INTEGER BIN_PTR                         ! pointer for bin co-ordinates
      INTEGER FREQ_SLOT                       ! slot for frequencies workspace
      INTEGER FREQ_PTR                        ! pointer for frequencies workspace
      INTEGER WORK_SLOT                       ! slot for smooth work array
      INTEGER WORK_PTR                        ! pointer for smooth work array
*-

*    Return if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check that there is a device open
      CALL P4_CHECK_PORT( PORT, STATUS )

*    Open the dataset
      CALL P4_CHECK_INPUT( DISPLAY_DATA( PORT ), STATUS )
      CALL DSA_OPEN( STATUS )
      CALL DSA_NAMED_INPUT( 'DATA', DISPLAY_DATA( PORT ), STATUS )
      CALL DSA_USE_QUALITY( 'DATA', STATUS )
      CALL DSA_DATA_SIZE( 'DATA', MAXDIM, NDIM, DIMS, NELM, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( NDIM .GT. 2 ) THEN
            CALL MSG_SETI( 'NDIM', NDIM )
            CALL MSG_OUT( ' ', 'NB: The data is ^NDIM-D '/
     :        /'so only the first 2-D plane will be plotted', STATUS )
         ELSE IF ( NDIM .EQ. 1 ) THEN
            DIMS( 2 ) = 1
         ENDIF
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'P4_HISTOGRAM: '/
     :     /'Error while opening DSA and data structure', STATUS )
         GOTO 500
      ENDIF

*    Does the display data have errors and quality?
      CALL DSA_SEEK_ERRORS( 'DATA', ERRORS, STATUS )
      CALL DSA_SEEK_QUALITY( 'DATA', QUALITY, STATUS )

*    Map the data
      IF ( DISPLAY_PLANE( PORT ) .EQ. 'DATA' ) THEN
         CALL DSA_MAP_DATA( 'DATA', 'READ', 'FLOAT',
     :      DATA_PTR, DATA_SLOT, STATUS )

*    Or map the errors
      ELSE IF ( DISPLAY_PLANE( PORT ) .EQ. 'ERRORS' ) THEN
         IF ( .NOT. ERRORS ) THEN
            STATUS = SAI__ERROR
              CALL ERR_REP( ' ', 'P4_HISTOGRAM: '/
     :          /'The structure does not have an error array', STATUS )
            GOTO 500
         ELSE
            CALL DSA_MAP_ERRORS( 'DATA', 'READ', 'FLOAT',
     :         DATA_PTR, DATA_SLOT, STATUS )
         ENDIF

*    Or map the quality as a floating point array
      ELSE IF ( DISPLAY_PLANE( PORT ) .EQ. 'QUALITY' ) THEN
         IF ( .NOT. QUALITY ) THEN
            STATUS = SAI__ERROR
              CALL ERR_REP( ' ', 'P4_HISTOGRAM: '/
     :          /'The structure does not have a quality array', STATUS )
            GOTO 500
         ELSE
            CALL DSA_MAP_QUALITY ('DATA', 'READ', 'FLOAT', DATA_PTR,
     :         DATA_SLOT, STATUS)
         ENDIF
      ENDIF

*    Map the quality as a byte array
      IF ( QUALITY ) THEN
         CALL DSA_MAP_QUALITY( 'DATA', 'READ', 'BYTE',
     :      QUAL_PTR, QUAL_SLOT, STATUS )
      ELSE
         QUAL_PTR = 0
      ENDIF

*    Get the axis info
      CALL P4_GET_AXLIM( PORT, DIMS(1), DIMS(2), STATUS )

*    Get the maximum, minimum, mean and sigma
      MEAN( PORT ) = 0.0
      SIGMA( PORT ) = 0.0
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL P4_GET_STATS( DIMS(1), DIMS(2), %val(DATA_PTR),
     :     QUALITY, %val(QUAL_PTR), PLOT_WHOLE( PORT ), ISTART( PORT ),
     :     IEND( PORT ), HISTOGRAM_XSTEP( PORT ), JSTART( PORT ),
     :     JEND( PORT ), HISTOGRAM_YSTEP( PORT ), AUTOSCALE( PORT ),
     :     HIGH( PORT ), LOW( PORT ), MEAN( PORT ), SIGMA( PORT ), STATUS )
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'P4_HISTOGRAM: '/
     :      /'Error while mapping data or error and quality arrays', STATUS )
         GOTO 500
      ENDIF

*    Check values of LOW( PORT ) and HIGH( PORT ) are not equal
      IF ( DISPLAY_PLANE( PORT ) .EQ. 'QUALITY' ) THEN
         LOW( PORT ) = 0.0
         HIGH( PORT ) = 1.001
      ENDIF

      IF ( LOW( PORT ) .EQ. HIGH( PORT ) ) THEN
         IF ( LOW( PORT ) .EQ. 0.0 ) THEN
            LOW( PORT ) = -0.01
            HIGH( PORT ) = 0.01
         ELSE
            HIGH( PORT ) = ABS( LOW( PORT ) ) * 1.1
            LOW( PORT ) = ABS( LOW( PORT ) ) * 0.9
         ENDIF
      ENDIF

*    Obtain some workspace to hold the histogram frequencies, the
*    bin co-ordinates and the smooth work array.
      BYTES = HISTOGRAM_BINS( PORT ) * DSA_TYPESIZE( 'FLOAT', STATUS )
      CALL DSA_GET_WORKSPACE( BYTES, BIN_PTR, BIN_SLOT, STATUS )
      CALL DSA_GET_WORKSPACE( BYTES, FREQ_PTR, FREQ_SLOT, STATUS )
      CALL DSA_GET_WORKSPACE( BYTES, WORK_PTR, WORK_SLOT, STATUS )

*    Bin the data
      MODE( PORT ) = 0.0
      FMIN( PORT ) = 0.0
      FMAX( PORT ) = 0.0
      TOOSMALL( PORT ) = 0
      TOOLARGE( PORT ) = 0
      CALL P4_BIN( DIMS( 1 ), DIMS( 2 ), %val( DATA_PTR ),
     :   %val( QUAL_PTR ), QUALITY, ISTART( PORT ), IEND( PORT ),
     :   HISTOGRAM_XSTEP( PORT ), JSTART( PORT ), JEND( PORT ),
     :   HISTOGRAM_YSTEP( PORT ), LOW( PORT ), HIGH( PORT ),
     :   HIST_SMOOTH( PORT ), HISTOGRAM_BINS( PORT ), %val( BIN_PTR ),
     :   %val( FREQ_PTR ), %val( WORK_PTR ), TOOSMALL( PORT ),
     :   TOOLARGE( PORT ), FMIN( PORT ), FMAX( PORT ), MODE( PORT ), STATUS )

*    Ensure that FMAX is never zero. (PGWINDOW will crash if it is).
      IF ( FMAX( PORT ) .LT. 1.0 ) THEN
         FMAX( PORT ) = 1.0
      END IF

*    Get labels
      SAME_UNITS = .FALSE.
      CALL P4_GET_AXLAB( PORT, SAME_UNITS, STATUS )

*    Clear display for new HISTOGRAM, if desired and if possible
      IF ( PRE_ERASE_PLOT( PORT ) ) CALL P4_CLEARPORT( PORT, STATUS )

*    First select basic viewport
      PLOT_OK( PORT ) = .FALSE.
      CALL P4_SELPORT( PORT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'P4_HISTOGRAM: '/
     :      /'Unable to select desired viewport', STATUS )
         GOTO 500
      ENDIF

*    Set up the window and remember these in the common block.
*    (Note that the lower Y window edge is always set to 0.0 and not FMIN).
*     XSTART( PORT ) = LOW( PORT )
*     XEND( PORT )   = HIGH( PORT )
*     YSTART( PORT ) = 0.0
*     YEND( PORT )   = FMAX( PORT )

      CALL PGWINDOW( LOW( PORT ), HIGH( PORT ),
     :   0.0, FMAX( PORT ) )

*    Remember the viewport limits in the common block (for cursoring).
      AXSTART( PORT ) = VXSTART( PORT )
      AXEND( PORT )   = VXEND( PORT )
      AYSTART( PORT ) = VYSTART( PORT )
      AYEND( PORT )   = VYEND( PORT )

*   Draw the histogram
      CALL PGBIN( HISTOGRAM_BINS( PORT ), %val( BIN_PTR ),
     :   %val( FREQ_PTR ), .FALSE. )

*    Plot the ancillary bits if need be
      IF ( PLOT_AXES( PORT ) ) THEN

         CALL PGBOX( DEVICE_XOPT( PORT ), 0.0, 0,
     :      DEVICE_YOPT( PORT ), 0.0, 0)

         XLEN = CHR_LEN( XLABEL(PORT) )
         YLEN = CHR_LEN( YLABEL(PORT) )
         IF ( TITLE( PORT ) .EQ. 'A_U_T_O' ) THEN
           TLEN = CHR_LEN( DEFTITLE(PORT) )
           SLEN = CHR_LEN( SUBTITLE(PORT) )
           CALL PGLABEL( XLABEL(PORT)(1:XLEN),
     :       YLABEL(PORT)(1:YLEN), DEFTITLE(PORT)(1:TLEN) )
           CALL PGMTEXT( 'T', 0.5, 0.5, 0.5, SUBTITLE(PORT)(1:SLEN) )
         ELSE
           TLEN = CHR_LEN( TITLE(PORT) )
           CALL PGLABEL( ' ', ' ', TITLE(PORT)(1:TLEN) )
         ENDIF

*       Write RH labels
         CPOS = CHR_LEN( RH1LABEL(PORT) )
         CALL PGMTEXT('R', 2.2, 0.5, 0.5, RH1LABEL(PORT)(1:CPOS))
         CPOS = CHR_LEN( RH2LABEL(PORT) )
         CALL PGMTEXT('R', 3.7, 0.5, 0.5, RH2LABEL(PORT)(1:CPOS))
      ENDIF

*    If OK, then set C_PLOT_OK to say that there is a valid plot in this port
      IF ( STATUS .EQ. SAI__OK ) THEN
         PLOT_OK( PORT ) = .TRUE.
         LAST_TYPE( PORT ) = 'HISTOGRAM'
      ELSE
         PLOT_OK (PORT) = .FALSE.
         LAST_TYPE( PORT ) = 'UNKNOWN'
      ENDIF

*    Close DSA and update the noticeboard
500   CONTINUE
      CALL DSA_CLOSE( STATUS )

      END
