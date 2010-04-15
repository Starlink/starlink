*+  P4_SURFACE - Display a dataset as an SURFACE
      SUBROUTINE P4_SURFACE( PORT, STATUS )
*    Description :
*     This routine displays a dataset as a colour SURFACE
*    Invocation :
*     CALL P4_SURFACE( PORT, STATUS )
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
      INTEGER MAXDIM                          ! max number of dimensions
      PARAMETER ( MAXDIM = 3 )
*    Local variables :
      CHARACTER*12 COLOUR
      LOGICAL SAME_UNITS                      ! T if axis units are the same
      LOGICAL ERRORS                          ! T if want to plot errors
      LOGICAL QUALITY                         ! T if want to plot quality
      INTEGER SLEN                            ! Length of string
      INTEGER TLEN                            ! Length of string
      INTEGER XLEN                            ! Length of string
      INTEGER YLEN                            ! Length of string
      INTEGER CPOS                            ! Length of string
      INTEGER NDIM                            ! Info on size of data array
      INTEGER DIMS( MAXDIM )                  !              "
      INTEGER NELM                            !              "
      INTEGER DS_PTR                          !              "
      INTEGER ES_PTR                          !              "
      INTEGER QS_PTR                          !              "
      INTEGER E_SLOT                          ! for error array
      INTEGER E_PTR                           !              "
      INTEGER DATA_SLOT                       ! for data array
      INTEGER DATA_PTR                        !              "
      INTEGER QUAL_SLOT                       ! for quality array
      INTEGER QUAL_PTR                        !              "
      INTEGER J                               ! Counters
      INTEGER CVAL, CSAVE                     ! Colour values
      REAL RD, GR, BL                         ! RGB colours
      REAL YINCREMENT, YSHIFT                 ! Space between lines
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
         CALL ERR_REP( ' ', 'P4_SURFACE: '/
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
              CALL ERR_REP( ' ', 'P4_SURFACE: '/
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
              CALL ERR_REP( ' ', 'P4_SURFACE: '/
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

*    Map the errors
      IF ( ERRORS ) THEN
         CALL DSA_MAP_ERRORS( 'DATA', 'READ', 'FLOAT',
     :      E_PTR, E_SLOT, STATUS )
      ELSE
         E_PTR = 0
      ENDIF

*    Get the axis range
      CALL P4_GET_AXLIM( PORT, DIMS(1), DIMS(2), STATUS )

*    Get the maximum, minimum, mean and sigma (resetting the X/YSTEP)
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL P4_GET_STATS( DIMS(1), DIMS(2), %val(DATA_PTR),
     :     QUALITY, %val(QUAL_PTR), PLOT_WHOLE( PORT ), ISTART( PORT ),
     :     IEND( PORT ), 1, JSTART( PORT ), JEND( PORT ), 1,
     :     AUTOSCALE( PORT ), HIGH( PORT ), LOW( PORT ), MEAN( PORT ),
     :     SIGMA( PORT ), STATUS )
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'P4_SURFACE: '/
     :      /'Error while mapping data or error and quality arrays', STATUS )
         GOTO 500
      ENDIF

*    Get axis labels
      SAME_UNITS = .FALSE.
      CALL P4_GET_AXLAB( PORT, SAME_UNITS, STATUS )

*    Check values of LOW( PORT ) and HIGH( PORT ) are not equal
      IF ( LOW( PORT ) .EQ. HIGH( PORT ) ) THEN
         IF ( LOW( PORT ) .EQ. 0.0 ) THEN
            HIGH( PORT ) = 0.01
         ELSE
            HIGH( PORT ) = ABS( LOW( PORT ) ) * 1.1
         ENDIF
      ENDIF

*    Clear display for new SURFACE, if desired and if possible
      IF ( PRE_ERASE_PLOT( PORT ) ) CALL P4_CLEARPORT( PORT, STATUS )

*    First select basic viewport
      PLOT_OK( PORT ) = .FALSE.
      CALL P4_SELPORT( PORT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'P4_SURFACE: '/
     :      /'Unable to select desired viewport', STATUS )
         GOTO 500
      ENDIF

*    Save the viewport limits
      AXSTART( PORT ) = VXSTART( PORT )
      AXEND( PORT)    = VXEND( PORT )
      AYSTART( PORT ) = VYSTART( PORT )
      AYEND( PORT )   = VYEND( PORT )

*    Setup the window
      YSTART( PORT ) = (HIGH(PORT)+LOW(PORT))*0.5 - (HIGH(PORT)-LOW(PORT))*1.5
      YEND( PORT )   = (HIGH(PORT)+LOW(PORT))*0.5 + (HIGH(PORT)-LOW(PORT))*1.5
      CALL PGWINDOW( XSTART( PORT ), XEND( PORT ),
     :   YSTART( PORT ), YEND( PORT ) )

*    Calculate offset between lines
      YINCREMENT = (HIGH(PORT)-LOW(PORT))*2.0 / FLOAT(JEND(PORT)-JSTART(PORT)+1)
      YSHIFT = YEND( PORT ) - HIGH( PORT )

*    Save the colour and set style
      CALL PGQCI( CSAVE )
      CVAL = 2
      DO J = JEND( PORT ), JSTART( PORT ), -1
        IF ( COLOUR_STYLE( PORT ) .EQ. 'COLOUR' ) THEN
           CALL P4_NUMTOCOL( COLOUR, CVAL, RD, GR, BL, STATUS )
           CALL PGSCR( CVAL, RD, GR, BL )
           CALL PGSCI( CVAL )
        ELSE
           IF ( MOD( J, 2 ) .EQ. 0 ) THEN
              CALL P4_NUMTOCOL( COLOUR, 1, RD, GR, BL, STATUS )
              CALL PGSCR( 1, RD, GR, BL )
              CALL PGSCI( 1 )
           ELSE
              CALL P4_NUMTOCOL( COLOUR, 15, RD, GR, BL, STATUS )
              CALL PGSCR( 15, RD, GR, BL )
              CALL PGSCI( 15 )
           ENDIF
        ENDIF

*       Calculate pointers for this slice
         DS_PTR = DATA_PTR + ( J-1 )*DIMS(1)*DSA_TYPESIZE( 'FLOAT', STATUS )
         IF ( ERRORS ) ES_PTR = E_PTR + ( J-1 )*DIMS(1)*DSA_TYPESIZE( 'FLOAT', STATUS )
         IF ( QUALITY ) QS_PTR = QUAL_PTR + ( J-1 )*DIMS(1)*DSA_TYPESIZE( 'BYTE', STATUS )

*       Plot the line, using errors and quality if present
*       NO xshift at prsent, though this would be easy to add
         CALL P4_LINE( DIMS( 1 ), ISTART( PORT ), IEND( PORT ),
     :      %val( AXIS1_PTR ), %val( DS_PTR ), %val( ES_PTR ),
     :      %val( QS_PTR ), ERRORS, QUALITY, 0.0, YSHIFT, STATUS )

*       Decrement YSHIFT and adjust colour value
         YSHIFT = YSHIFT - YINCREMENT
         CVAL = CVAL + 1
         IF ( CVAL .GT. 15 ) CVAL = 2
      ENDDO

*    Restore colour
      CALL PGSCI( CSAVE )

*    Plot the labels
      IF ( PLOT_AXES( PORT ) ) THEN

         CALL PGBOX( DEVICE_XOPT( PORT ), 0.0, 0, DEVICE_YOPT( PORT ), 0.0, 0 )

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

         CPOS = CHR_LEN( RH1LABEL(PORT) )
         CALL PGMTEXT( 'R', 2.2, 0.5, 0.5, RH1LABEL(PORT)(1:SLEN) )
      ENDIF

*    If OK, then set C_PLOT_OK to say that there is a valid plot in this port
      IF ( STATUS .EQ. SAI__OK ) THEN
         PLOT_OK( PORT ) = .TRUE.
         LAST_TYPE( PORT ) = 'SURFACE'
      ELSE
         PLOT_OK (PORT) = .FALSE.
         LAST_TYPE( PORT ) = 'UNKNOWN'
      ENDIF

*    Close DSA
500   CONTINUE
      CALL DSA_CLOSE( STATUS )

      END
