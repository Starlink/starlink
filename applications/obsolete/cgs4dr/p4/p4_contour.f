*+  P4_CONTOUR - Display a dataset as a contour
      SUBROUTINE P4_CONTOUR( PORT, STATUS )
*    Invocation :
*     CALL P4_CONTOUR( PORT STATUS )
*    Authors :
*     P.N.Daly (JACH::PND)
*    History :
*     02-Dec-1993: Original version                              (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Import :
      INTEGER PORT                        ! Number of viewport for plot
*    External references :
      INTEGER CHR_LEN                     ! String length function
*    Global variables :
      INCLUDE 'P4COM.INC'                 ! P4 common block
*    Local Constants :
      INTEGER MAXDIM
      PARAMETER ( MAXDIM = 3 )            ! Maximum number of dimensions
*    Local variables :
      CHARACTER*12 COLOUR
      LOGICAL SAME_UNITS                  ! T if axis units are the same
      LOGICAL ERRORS                      ! T if want to plot errors
      LOGICAL QUALITY                     ! T if want to plot quality
      INTEGER CVAL                        ! Colour value
      INTEGER CSLS                        ! Colour value
      INTEGER SLEN                        ! Length of string
      INTEGER TLEN                        ! Length of string
      INTEGER XLEN                        ! Length of string
      INTEGER YLEN                        ! Length of string
      INTEGER NDIM                        ! info on size of data array
      INTEGER DIMS( MAXDIM )              !              "
      INTEGER NELM                        !              "
      INTEGER DATA_SLOT                   ! for data array
      INTEGER DATA_PTR                    !              "
      INTEGER QUAL_SLOT                   ! for quality array
      INTEGER QUAL_PTR                    !              "
      INTEGER I                           ! Counters
      REAL XSIZE, YSIZE, ASPECT           ! Size of plot surface
      REAL VPCENTRE_X, VPCENTRE_Y         ! Fudge factors
      REAL VPWIDTH_X, VPWIDTH_Y           ! Fudge factors
      REAL RD, GR, BL                     ! RGB colours
*-

*    Return if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check that there is a device open
      CALL P4_CHECK_PORT( PORT, STATUS )

*    Open the data structure
      CALL P4_CHECK_INPUT( DISPLAY_DATA( PORT ), STATUS )
      CALL DSA_OPEN( STATUS )
      CALL DSA_NAMED_INPUT( 'DATA', DISPLAY_DATA( PORT ), STATUS )
      CALL DSA_USE_QUALITY ('DATA', STATUS)
      CALL DSA_DATA_SIZE( 'DATA', MAXDIM, NDIM, DIMS, NELM, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( NDIM .GT. 2 ) THEN
            CALL MSG_SETI( 'NDIM', NDIM )
            CALL MSG_OUT( ' ', 'NB: The data is ^NDIM-D so only '/
     :        /'the first 2-D plane will be plotted', STATUS )
         ELSE IF ( NDIM .EQ. 1 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'P4_CONTOUR: '/
     :         /'Unable to contour plot 1-D data', STATUS )
            GOTO 500
         ENDIF
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'P4_CONTOUR: '/
     :      /'Error while opening DSA and data structure', STATUS )
         GOTO 500
      ENDIF

*    Does the display data have quality and/or errors?
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
            CALL ERR_REP( ' ', 'P4_CONTOUR: '/
     :         /'The structure does not have an error array', STATUS )
            GOTO 500
         ELSE
            CALL DSA_MAP_ERRORS ('DATA', 'READ', 'FLOAT',
     :         DATA_PTR, DATA_SLOT, STATUS)
         ENDIF

*    Or signal an error if we want to contour the quality array
      ELSE IF ( DISPLAY_PLANE( PORT ) .EQ. 'QUALITY' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'P4_CONTOUR: '/
     :      /'A contour plot of a (binary) quality array '/
     :      /'is meaningless', STATUS )
         GOTO 500
      ENDIF

*    Map the quality array (for other pruposes)
      IF ( QUALITY ) THEN
         CALL DSA_MAP_QUALITY( 'DATA', 'READ', 'BYTE',
     :      QUAL_PTR, QUAL_SLOT, STATUS )
      ELSE
         QUAL_PTR = 0
      ENDIF

*    Get the axis info
      CALL P4_GET_AXLIM( PORT, DIMS(1), DIMS(2), STATUS )

*    Get the minimum, maximum, mean and sigma
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL P4_GET_STATS( DIMS(1), DIMS(2), %val(DATA_PTR),
     :   QUALITY, %val(QUAL_PTR), PLOT_WHOLE( PORT ), ISTART( PORT ),
     :   IEND( PORT ), 1, JSTART( PORT ), JEND( PORT ), 1,
     :   AUTOSCALE( PORT ), HIGH( PORT ), LOW( PORT ), MEAN( PORT ),
     :   SIGMA( PORT ), STATUS )
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'P4_CONTOUR: '/
     :      /'Error while mapping data and quality arrays', STATUS )
         GOTO 500
      ENDIF

*    Check values of LOW( PORT ) and HIGH( PORT ) are not equal
      IF ( LOW( PORT ) .EQ. HIGH( PORT ) ) THEN
         IF ( LOW( PORT ) .EQ. 0.0 ) THEN
            LOW( PORT ) = -0.01
            HIGH( PORT ) = 0.01
         ELSE
            HIGH( PORT ) = ABS( LOW( PORT ) ) * 1.1
            LOW( PORT ) = ABS( LOW( PORT ) ) * 0.9
         ENDIF
      ENDIF

*    Set the contour levels in the array
      CALL P4_SET_CONTOUR( PORT, STATUS )

*    Get axis labels
      SAME_UNITS = .FALSE.
      CALL P4_GET_AXLAB( PORT, SAME_UNITS, STATUS )

*    Clear display for new contour, if desired and if possible
      IF ( PRE_ERASE_PLOT( PORT ) ) CALL P4_CLEARPORT( PORT, STATUS )

*    First select basic viewport
      CALL P4_SELPORT( PORT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'P4_CONTOUR: '/
     :      /'Unable to select desired viewport', STATUS )
         GOTO 500
      ENDIF

*    Determine the aspect ratio of the display port (ensuring it is
*    always greater than 1).
      XSIZE = ABS( VXEND( PORT ) - VXSTART( PORT ) )
      YSIZE = ABS( VYEND( PORT ) - VYSTART( PORT ) )
      ASPECT = MAX( XSIZE, YSIZE ) / MIN( XSIZE, YSIZE )

*    Set up the display window.
*    If the axis units are the same, and the aspect ratio of the display
*    port is less than 2:1, then set up the window so that a unit is
*    square. Otherwise fill the viewport with the window.
      IF ( ( SAME_UNITS ) .AND. ( ASPECT .LT. 2.0 ) ) THEN
         CALL PGWNAD( XSTART( PORT ), XEND( PORT ),
     :      YSTART( PORT ), YEND( PORT ) )
      ELSE
         CALL PGWINDOW( XSTART( PORT ), XEND( PORT ),
     :      YSTART( PORT ), YEND( PORT ) )
      ENDIF

*    Preserve what has been done in the common block
      CALL PGQVP( 0, AXSTART( PORT ), AXEND( PORT ),
     :   AYSTART( PORT ),AYEND( PORT ) )

*    >>> there is a bug in PGQVP which means the viewport returned
*    >>> has the correct size but is wedged down in the blc of the
*    >>> original viewport before PGWNAD was called, correct for
*    >>> this
      VPCENTRE_X = ( VXSTART( PORT ) + VXEND( PORT ) ) / 2.0
      VPCENTRE_Y = ( VYSTART( PORT ) + VYEND( PORT ) ) / 2.0
      VPWIDTH_X = AXEND( PORT ) - AXSTART( PORT )
      VPWIDTH_Y = AYEND( PORT ) - AYSTART( PORT )
      AXSTART( PORT ) = VPCENTRE_X - VPWIDTH_X / 2.0
      AXEND( PORT ) = VPCENTRE_X + VPWIDTH_X / 2.0
      AYSTART( PORT ) = VPCENTRE_Y - VPWIDTH_Y / 2.0
      AYEND( PORT ) = VPCENTRE_Y + VPWIDTH_Y / 2.0
*    >>> end of bodge

      CVAL = 2
      CSLS = 1
      DO I = 1, CONTOUR_LEVELS( PORT ), 1

*       Set the colour
         CALL PGSLS( CSLS )
         IF ( COLOUR_STYLE( PORT ) .EQ. 'COLOUR' ) THEN
            CALL P4_NUMTOCOL( COLOUR, CVAL, RD, GR, BL, STATUS )
            CALL PGSCR( CVAL, RD, GR, BL )
            CALL PGSCI( CVAL )
         ELSE
            IF ( MOD( I, 2 ) .EQ. 0 ) THEN
               CALL P4_NUMTOCOL( COLOUR, 1, RD, GR, BL, STATUS )
               CALL PGSCR( 1, RD, GR, BL )
               CALL PGSCI( 1 )
            ELSE
               CALL P4_NUMTOCOL( COLOUR, 15, RD, GR, BL, STATUS )
               CALL PGSCR( 15, RD, GR, BL )
               CALL PGSCI( 15 )
            ENDIF
         ENDIF

*       Plot contours
         CALL PGCONT( %val( DATA_PTR ), DIMS(1), DIMS(2),
     :      ISTART( PORT ), IEND( PORT ), JSTART( PORT ),
     :      JEND( PORT ), ARRAY_CONTOURS(I), -1, TMATRIX )

         IF ( MOD( CSLS, 5 ) .EQ. 0 ) THEN
            CSLS = 1
         ELSE
            CSLS = CSLS + 1
         ENDIF

         IF ( MOD( I, 17 ) .EQ. 0 ) THEN
            CVAL = 2
         ELSE
            CVAL = CVAL + 1
         ENDIF
      ENDDO

      CALL PGSLS( 1 )
      CALL PGSCI( 1 )

*    Write the contour levels to a file
      CALL P4_WRITE_CONTOUR( PORT, STATUS )

*    Plot the ancillary bits if need be
      IF ( PLOT_AXES( PORT ) ) THEN

         CALL PGBOX( DEVICE_XOPT( PORT ), 0.0, 0,
     :      DEVICE_YOPT( PORT ), 0.0, 0 )

*       Write the title
         XLEN = CHR_LEN( XLABEL(PORT) )
         YLEN = CHR_LEN( YLABEL(PORT) )
         IF ( TITLE( PORT ).EQ. 'A_U_T_O' ) THEN
            TLEN = CHR_LEN( DEFTITLE(PORT) )
            SLEN = CHR_LEN( SUBTITLE(PORT) )
            CALL PGLABEL( XLABEL(PORT)(1:XLEN), YLABEL(PORT)(1:YLEN),
     :         DEFTITLE(PORT)(1:TLEN) )
            CALL PGMTEXT( 'T', 0.5, 0.5, 0.5, SUBTITLE(PORT)(1:SLEN) )
         ELSE
            TLEN = CHR_LEN( TITLE(PORT) )
            CALL PGLABEL( ' ', ' ', TITLE(PORT)(1:TLEN) )
         ENDIF
      ENDIF

*    If OK, then set C_PLOT_OK to say that there is a valid plot in this port
      IF (STATUS .EQ. SAI__OK) THEN
         PLOT_OK( PORT ) = .TRUE.
         LAST_TYPE( PORT ) = 'CONTOUR'
      ELSE
         PLOT_OK( PORT ) = .FALSE.
         LAST_TYPE( PORT ) = 'UNKNOWN'
      ENDIF

*    Close DSA and exit
 500  CONTINUE
      CALL DSA_CLOSE( STATUS )
      END
