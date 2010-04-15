*+  P4_IMAGE - Display a dataset as an image
      SUBROUTINE P4_IMAGE( PORT, STATUS )
*    Description :
*     This routine displays a dataset as a colour image
*    Invocation :
*     CALL P4_IMAGE( PORT, STATUS )
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
*    Global variables :
      INCLUDE 'P4COM.INC'                     ! P4 common block
      INCLUDE 'COLOURS.INC'                   ! Colours common block
*    Local Constants :
      INTEGER NINFO                           ! Maximum number of info items
      PARAMETER ( NINFO = 2 )
      INTEGER MAXDIM                          ! max number of dimensions
      PARAMETER ( MAXDIM = 3 )
      REAL TOLER                              ! Tolerance for testing
      PARAMETER ( TOLER = 1.0E-10 )
*    Local variables :
      LOGICAL SAME_UNITS                      ! T if units are same on both axes
      LOGICAL ERRORS                          ! T if want to plot errors
      LOGICAL QUALITY                         ! T if want to plot quality
      INTEGER XLEN, YLEN, SLEN, TLEN          ! Label lengths
      INTEGER NDIM                            ! Info on size of data array
      INTEGER DIMS( MAXDIM )                  !              "
      INTEGER NELM                            !              "
      INTEGER DATA_SLOT                       ! for data array
      INTEGER DATA_PTR                        !              "
      INTEGER IDATA_SLOT                      ! for integer data array
      INTEGER IDATA_PTR                       !              "
      INTEGER QUAL_SLOT                       ! for quality array
      INTEGER QUAL_PTR                        !              "
      INTEGER I, J                            ! Counters
      REAL RATIO, AMT, TEMP, DIFF             ! Colour scaling
      REAL XSIZE, YSIZE, ASPECT               ! Size of plot surface
      REAL VPCENTRE_X, VPCENTRE_Y             ! Fudge factors
      REAL VPWIDTH_X, VPWIDTH_Y               ! Fudge factors
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
         CALL ERR_REP( ' ', 'P4_IMAGE: '/
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
              CALL ERR_REP( ' ', 'P4_IMAGE: '/
     :          /'The structure does not have an error array', STATUS )
            GOTO 500
         ELSE
            CALL DSA_MAP_ERRORS( 'DATA', 'READ', 'FLOAT',
     :         DATA_PTR, DATA_SLOT, STATUS )
         ENDIF

*    Or map the quality as a flaoting point array
      ELSE IF ( DISPLAY_PLANE( PORT ) .EQ. 'QUALITY' ) THEN
         IF ( .NOT. QUALITY ) THEN
            STATUS = SAI__ERROR
              CALL ERR_REP( ' ', 'P4_IMAGE: '/
     :          /'The structure does not have a quality array', STATUS )
            GOTO 500
         ELSE
            CALL DSA_MAP_QUALITY( 'DATA', 'READ', 'FLOAT',
     :         DATA_PTR, DATA_SLOT, STATUS )
         ENDIF
      ENDIF

*    Map the quality
      IF ( QUALITY ) THEN
         CALL DSA_MAP_QUALITY( 'DATA', 'READ', 'BYTE',
     :      QUAL_PTR, QUAL_SLOT, STATUS )
      ELSE
         QUAL_PTR = 0
      ENDIF

*    Get the axis info
      CALL P4_GET_AXLIM( PORT, DIMS(1), DIMS(2), STATUS )

*    Get the maximum, minimum, mean and sigma (using X/Y STEP of 1)
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL P4_GET_STATS( DIMS(1), DIMS(2), %val(DATA_PTR),
     :      QUALITY, %val(QUAL_PTR), PLOT_WHOLE(PORT), ISTART( PORT ),
     :      IEND( PORT ), 1, JSTART( PORT ), JEND( PORT ), 1 ,
     :      AUTOSCALE( PORT ), HIGH( PORT ), LOW( PORT ), MEAN( PORT ),
     :      SIGMA( PORT ), STATUS )
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'P4_IMAGE: '/
     :      /'Error while mapping data or error and quality arrays', STATUS )
         GOTO 500
      ENDIF

*    Get labels for plot
      SAME_UNITS = .FALSE.
      CALL P4_GET_AXLAB( PORT, SAME_UNITS, STATUS )

*    Get an integer work array
      CALL DSA_GET_WORK_ARRAY( DIMS(1)*DIMS(2), 'INT', IDATA_PTR,
     :   IDATA_SLOT, STATUS )

*    Check values of LOW( PORT ) and HIGH( PORT ) are not equal
      IF ( DISPLAY_PLANE( PORT ) .EQ. 'QUALITY' ) THEN
         LOW( PORT ) = 0.0
         HIGH( PORT ) = 1.0
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

*    Clear display for new image, if desired and if possible
      IF ( PRE_ERASE_PLOT( PORT ) ) CALL P4_CLEARPORT( PORT, STATUS )

*    First select basic viewport
      PLOT_OK( PORT ) = .FALSE.
      CALL P4_SELPORT( PORT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'P4_IMAGE: '/
     :      /'Unable to select desired viewport', STATUS )
         GOTO 500
      ENDIF

*    Determine the aspect ratio of the display port (ensuring it is always >1).
      XSIZE = ABS( VXEND( PORT ) - VXSTART( PORT ) )
      YSIZE = ABS( VYEND( PORT ) - VYSTART( PORT ) )
      ASPECT = MAX( XSIZE, YSIZE ) / MIN( XSIZE, YSIZE )
      IF ( VERBOSE ) THEN
        CALL MSG_SETR( 'ASPECT', ASPECT )
        CALL MSG_OUT( ' ', 'P4_IMAGE: Aspect ratio is ^ASPECT', STATUS )
      ENDIF

*    Set up the display window.
*    If the axis units are the same, and the aspect ratio of the display
*    port is less than 2:1, then set up the window so that a unit is
*    square. Otherwise fill the viewport with the window.
      IF ( SAME_UNITS .AND. ( ASPECT .LT. 2.0 ) ) THEN
         CALL PGWNAD( XSTART( PORT ), XEND( PORT ),
     :      YSTART( PORT ), YEND( PORT ) )
      ELSE
         CALL PGWINDOW( XSTART( PORT ), XEND( PORT ),
     :      YSTART( PORT ), YEND( PORT ) )
      ENDIF

*    Preserve what has been done in the common block
      CALL PGQVP( 0, AXSTART( PORT ), AXEND( PORT ),
     :   AYSTART( PORT ), AYEND( PORT ) )

*    >>> there is a bug in PGQVP which means the viewport returned
*    >>> has the correct size but is wedged down in the blc of the
*    >>> original viewport before PGWNAD was called, correct for
*    >>> this
      VPCENTRE_X = (VXSTART( PORT ) + VXEND( PORT )) / 2.0
      VPCENTRE_Y = (VYSTART( PORT ) + VYEND( PORT )) / 2.0
      VPWIDTH_X = AXEND( PORT ) - AXSTART( PORT )
      VPWIDTH_Y = AYEND( PORT ) - AYSTART( PORT )
      AXSTART( PORT ) = VPCENTRE_X - VPWIDTH_X / 2.0
      AXEND( PORT)    = VPCENTRE_X + VPWIDTH_X / 2.0
      AYSTART( PORT ) = VPCENTRE_Y - VPWIDTH_Y / 2.0
      AYEND( PORT )   = VPCENTRE_Y + VPWIDTH_Y / 2.0
*    >>> end of bodge

*    First inquire how many colours there are and set RGB values.
*    We should put this in an IF block with a flag, b/c if a new colour
*    table hasn't been asked for, this code is redundant
      RATIO = FLOAT( CI2( PORT ) ) / 256.0
      AMT = RATIO / 2.0
      DO I = 1,256

         TEMP = RATIO * I
         J = NINT(TEMP)
         DIFF = ABS(TEMP-J)

         IF ( DIFF .LT. AMT ) THEN

            IF ( J .EQ. 1 ) THEN

               RED(1) = FG_RD
               GREEN(1) = FG_GR
               BLUE(1) = FG_BL
            ENDIF

            CALL PGSCR( J, RED(I), GREEN(I), BLUE(I) )
         ENDIF
      ENDDO

*    Scale the data
      IF ( VERBOSE ) THEN
        CALL MSG_SETR( 'LO', LOW(PORT) )
        CALL MSG_SETR( 'HI', HIGH(PORT) )
        CALL MSG_SETI( 'MC', CI2(PORT) )
        CALL MSG_OUT( ' ', 'P4_IMAGE: Scaling data from ^LO to ^HI '/
     :   /'to lie in range 2 to ^MC', STATUS )
      ENDIF
      CALL P4_SCALE_REAL ( DIMS(1), DIMS(2), %val( DATA_PTR ),
     :   LOW( PORT ), HIGH( PORT ), 2, CI2( PORT ),
     :   %val( IDATA_PTR ), STATUS )

*    Plot the image
      IF ( VERBOSE ) THEN
        CALL MSG_SETI( 'DIM1', DIMS(1) )
        CALL MSG_SETI( 'DIM2', DIMS(2) )
        CALL MSG_SETI( 'PORT', PORT )
        CALL MSG_OUT( ' ',
     :    'P4_IMAGE: Data size is ^DIM1 x ^DIM2 on port ^PORT', STATUS )
        CALL MSG_SETI( 'I1', ISTART(PORT) )
        CALL MSG_SETI( 'I2', IEND(PORT) )
        CALL MSG_OUT( ' ', 'P4_IMAGE: I-range is ^I1 to ^I2', STATUS )
        CALL MSG_SETI( 'J1', JSTART(PORT) )
        CALL MSG_SETI( 'J2', JEND(PORT) )
        CALL MSG_OUT( ' ', 'P4_IMAGE: J-range is ^J1 to ^J2', STATUS )
        CALL MSG_SETR( 'X1', XSTART(PORT) )
        CALL MSG_SETR( 'X2', XEND(PORT) )
        CALL MSG_OUT( ' ', 'P4_IMAGE: X-range is ^X1 to ^X2', STATUS )
        CALL MSG_SETR( 'Y1', YSTART(PORT) )
        CALL MSG_SETR( 'Y2', YEND(PORT) )
        CALL MSG_OUT( ' ', 'P4_IMAGE: Y-range is ^Y1 to ^Y2', STATUS )
      ENDIF

*   Plot if status is OK
      IF ( STATUS .EQ. SAI__OK ) THEN
        CALL PGPIXL( %val( IDATA_PTR ), DIMS( 1 ), DIMS( 2 ),
     :     ISTART( PORT ), IEND( PORT ), JSTART( PORT ), JEND( PORT ),
     :     XSTART( PORT ), XEND( PORT ), YSTART( PORT ), YEND( PORT ) )
      ENDIF

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
      ENDIF

*    If OK, then set C_PLOT_OK to say that there is a valid plot in this port
      IF ( STATUS .EQ. SAI__OK ) THEN
         PLOT_OK( PORT ) = .TRUE.
         LAST_TYPE( PORT ) = 'IMAGE'
      ELSE
         PLOT_OK (PORT) = .FALSE.
         LAST_TYPE( PORT ) = 'UNKNOWN'
      ENDIF

*    Plot a colour bar
      IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :     ( PLOT_AXES( PORT ) ) ) CALL P4_PLOTBAR( PORT, STATUS )

*    Close DSA and update the noticeboard
500   CONTINUE
      CALL DSA_FREE_WORKSPACE( IDATA_SLOT, STATUS )
      CALL DSA_CLOSE( STATUS )

      END
