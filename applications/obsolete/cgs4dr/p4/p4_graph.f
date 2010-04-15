*+  P4_GRAPH - Display a dataset as an GRAPH
      SUBROUTINE P4_GRAPH( PORT, STATUS )
*    Description :
*     This routine displays a dataset as a colour GRAPH
*    Invocation :
*     CALL P4_GRAPH( PORT, STATUS )
*    Authors :
*     P. N. Daly ( JACH::PND )
*    History :
*     16-Aug-1994: Original Unix version (PND)
*      2-Dec-1994: Replace GEN_RANGEFV with standard GEN_QRANGEF
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
      LOGICAL SAME_UNITS                      ! T if axis units are the same
      LOGICAL ERRORS                          ! T if want to plot errors
      LOGICAL QUALITY                         ! T if want to plot quality
      INTEGER SLEN                            ! Length of string
      INTEGER TLEN                            ! Length of string
      INTEGER XLEN                            ! Length of string
      INTEGER YLEN                            ! Length of string
      INTEGER CPOS                            ! Length of string
      INTEGER NDIM                            ! Info on size of data array
      INTEGER DIM                             !              "
      INTEGER DIMS( MAXDIM )                  !              "
      INTEGER NELM                            !              "
      INTEGER CS_SLOT                         ! for data array
      INTEGER CS_PTR                          !              "
      INTEGER DS_SLOT                         ! for data array
      INTEGER DS_PTR                          !              "
      INTEGER ES_SLOT                         ! for error array
      INTEGER ES_PTR                          !              "
      INTEGER QS_SLOT                         ! for quality array
      INTEGER QS_PTR                          !              "
      INTEGER ERR_SLOT                        ! for error array
      INTEGER ERR_PTR                         !              "
      INTEGER DATA_SLOT                       ! for data array
      INTEGER DATA_PTR                        !              "
      INTEGER QUAL_SLOT                       ! for quality array
      INTEGER QUAL_PTR                        !              "
      INTEGER CVAL, CSAVE                     ! Colours
      INTEGER FLOATSIZE, BYTESIZE, INTSIZE    ! Size of data types
      INTEGER LABELPOS                        ! Counters, label position
      REAL RD, GR, BL, OFFSET                 ! Colour levels
      LOGICAL QDUMMY, EDUMMY                  ! T if dummy arrays are used
*    Local data :
      DATA LABELPOS /0/
      SAVE LABELPOS
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
         IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Opened data OK', STATUS )
         IF ( NDIM .GT. 2 ) THEN
            CALL MSG_SETI( 'NDIM', NDIM )
            CALL MSG_OUT( ' ', 'NB: The data is ^NDIM-D so only the first 2-D plane will be plotted', STATUS )
         ELSE IF ( NDIM .EQ. 1 ) THEN
            DIMS( 2 ) = 1
         ENDIF
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'P4_GRAPH: Error while opening DSA and data structure', STATUS )
         GOTO 600
      ENDIF

*   Get datatype sizes
      FLOATSIZE = DSA_TYPESIZE( 'FLOAT', STATUS )
      BYTESIZE  = DSA_TYPESIZE( 'BYTE', STATUS )
      INTSIZE   = DSA_TYPESIZE( 'INT', STATUS )

*    Does the display data have errors and quality?
      ERRORS = .FALSE.
      CALL DSA_SEEK_ERRORS( 'DATA', ERRORS, STATUS )
      QUALITY = .FALSE.
      CALL DSA_SEEK_QUALITY( 'DATA', QUALITY, STATUS )

*    Map the data
      DATA_PTR = 0
      DATA_SLOT = 0
      IF ( ( DISPLAY_PLANE( PORT ) .EQ. 'DATA' ) .AND. ( STATUS .EQ. SAI__OK ) ) THEN
         IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Mapping data into data array', STATUS )
         CALL DSA_MAP_DATA( 'DATA', 'READ', 'FLOAT', DATA_PTR, DATA_SLOT, STATUS )

*    Or map the errors
      ELSE IF ( ( DISPLAY_PLANE( PORT ) .EQ. 'ERRORS' ) .AND. ( STATUS .EQ. SAI__OK ) ) THEN
         IF ( .NOT. ERRORS ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'P4_GRAPH: The structure does not have an error array', STATUS )
            GOTO 600
         ELSE
            IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Mapping errors into data array', STATUS )
            CALL DSA_MAP_ERRORS( 'DATA', 'READ', 'FLOAT', DATA_PTR, DATA_SLOT, STATUS )
         ENDIF

*    Or map the quality as a floating point array (generates soft error)
      ELSE IF ( ( DISPLAY_PLANE( PORT ) .EQ. 'QUALITY' ) .AND. ( STATUS .EQ. SAI__OK ) ) THEN
         IF ( .NOT. QUALITY ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'P4_GRAPH: The structure does not have a quality array', STATUS )
            GOTO 600
         ELSE
            IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Mapping quality into data array', STATUS )
            CALL DSA_MAP_QUALITY( 'DATA', 'READ', 'FLOAT', DATA_PTR, DATA_SLOT, STATUS )
            CALL ERR_ANNUL( STATUS )
         ENDIF
      ENDIF

*    Map the quality
      QUAL_PTR = 0
      QUAL_SLOT = 0
      QDUMMY = .FALSE.
      IF ( ( QUALITY ) .AND. ( STATUS .EQ. SAI__OK ) ) THEN
         IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Mapping quality', STATUS )
         CALL DSA_MAP_QUALITY( 'DATA', 'READ', 'BYTE', QUAL_PTR, QUAL_SLOT, STATUS )
      ELSE
         CALL DSA_GET_WORKSPACE( BYTESIZE*NELM, QUAL_PTR, QUAL_SLOT, STATUS )
         IF ( STATUS .EQ. SAI__OK ) QDUMMY = .TRUE.
         CALL GEN_FILL( BYTESIZE*NELM, 0, %val( QUAL_PTR ) )
      ENDIF

*    Look for error information, if desired
      ERR_PTR = 0
      ERR_SLOT = 0
      EDUMMY = .FALSE.
      IF ( PLOT_ERRORS( PORT ) .AND. ( STATUS .EQ. SAI__OK ) ) THEN
         IF ( ERRORS ) THEN
            IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Mapping errors', STATUS )
            CALL DSA_MAP_ERRORS( 'DATA', 'READ', 'FLOAT', ERR_PTR, ERR_SLOT, STATUS )
         ELSE
            CALL DSA_GET_WORKSPACE( FLOATSIZE*NELM, ERR_PTR, ERR_SLOT, STATUS )
            IF ( STATUS .EQ. SAI__OK ) EDUMMY = .TRUE.
            CALL GEN_FILL( FLOATSIZE*NELM, 0.0, %val( ERR_PTR ) )
         ENDIF
      ENDIF

*    Get the axis limits etc
      IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Getting axis limits', STATUS )
      CALL P4_GET_AXLIM( PORT, DIMS(1), DIMS(2), STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'P4_GRAPH: Unable to get axis range', STATUS )
        GOTO 600
      ENDIF

*    Set the number of dimensions (assume 1-D and correct for >2-D case)
      DIM = DIMS(1)
      IF ( CUT_DIRECTION(PORT)(1:1) .NE. 'X' ) DIM = DIMS(2)

      IF ( VERBOSE ) THEN
        CALL MSG_SETC( 'DIR', CUT_DIRECTION(PORT) )
        CALL MSG_SETI( 'DIM', DIM )
        CALL MSG_OUT( ' ', 'Cut direction is ^DIR with ^DIM elements', STATUS )
      ENDIF

*    Initialise slots and pointers
      DS_PTR = 0
      ES_PTR = 0
      QS_PTR = 0
      CS_PTR = 0
      DS_SLOT = 0
      ES_SLOT = 0
      QS_SLOT = 0
      CS_SLOT = 0

*    For 1-D, use existing pointers
      IF ( NDIM .EQ. 1 ) THEN

        IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Data is one dimensional', STATUS )
        DS_PTR = DATA_PTR
        ES_PTR = ERR_PTR
        QS_PTR = QUAL_PTR

*      Extract the data and errors
      ELSE

*      Get some workspace for data and initialize it
        IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Getting workspace for data', STATUS )
        CALL DSA_GET_WORKSPACE( FLOATSIZE*DIM, DS_PTR, DS_SLOT, STATUS )
        CALL GEN_FILL( FLOATSIZE*DIM, 0.0, %val( DS_PTR ) )

*      Get some workspace for errors and initialize it
        IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Getting workspace for errors', STATUS )
        CALL DSA_GET_WORKSPACE( FLOATSIZE*DIM, ES_PTR, ES_SLOT, STATUS )
        CALL GEN_FILL( FLOATSIZE*DIM, 0.0, %val( ES_PTR ) )

*      Get some workspace for quality and initialize it
        IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Getting workspace for quality', STATUS )
        CALL DSA_GET_WORKSPACE( BYTESIZE*DIM, QS_PTR, QS_SLOT, STATUS )
        CALL GEN_FILL( BYTESIZE*DIM, 0, %val( QS_PTR ) )

*      Get some workspace for counters and initialize it
        IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Getting workspace for counters', STATUS )
        CALL DSA_GET_WORKSPACE( INTSIZE*DIM, CS_PTR, CS_SLOT, STATUS )
        CALL GEN_FILL( INTSIZE*DIM, 0, %val( CS_PTR ) )

*      Extract the data
        IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Data is multi dimensional', STATUS )
        IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Extracting data', STATUS )
        CALL P4_EXTRACT( DIMS(1), DIMS(2), %val(DATA_PTR), %val(QUAL_PTR),
     :    QUALITY, CUT_DIRECTION(PORT)(1:1), ISLICE_START, ISLICE_END,
     :    %val(CS_PTR), %val(DS_PTR), %val(QS_PTR), STATUS )

        IF ( PLOT_ERRORS( PORT ) .AND. ERRORS ) THEN
          IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Extracting errors', STATUS )
          CALL P4_EXTRACT( DIMS(1), DIMS(2), %val(ERR_PTR), %val(QUAL_PTR),
     :      QUALITY, CUT_DIRECTION(PORT)(1:1), ISLICE_START, ISLICE_END,
     :      %val(CS_PTR), %val(ES_PTR), %val(QS_PTR), STATUS )
        ENDIF
      ENDIF

*    Check the scaling (GRAPH only OVERGRAPH uses the same scale)
      IF ( DISPLAY_TYPE( PORT ) .EQ. 'GRAPH' ) THEN

*      Check values of LOW( PORT ) and HIGH( PORT ) are not equal
        IF ( .NOT. AUTOSCALE( PORT ) ) THEN
           YSTART( PORT ) = LOW( PORT )
           YEND( PORT ) = HIGH( PORT )
        ELSE

*      If autoscaling is enabled, rescale the window to the range
*      of the data, ensuring that YSTART( PORT ) and YEND( PORT ) cannot be same.
           IF (VERBOSE) CALL MSG_OUT( ' ', 'Calling GEN_QRANGEF', STATUS )
           CALL GEN_QRANGEF( %val( DS_PTR ), %val( QS_PTR ),
     :       1, DIM, YEND( PORT ), YSTART( PORT ) )
           IF ( YEND( PORT ) .EQ. YSTART( PORT ) ) THEN
              IF ( YSTART( PORT ) .EQ. 0.0 ) THEN
                 YEND( PORT ) = 0.01
                 YSTART( PORT ) = -0.01
              ELSE IF ( YSTART( PORT ) .LT. 0.0 ) THEN
                 YEND( PORT ) = YSTART( PORT ) * 0.9
                 YSTART( PORT ) = YSTART( PORT ) * 1.1
              ELSE
                 YEND( PORT ) = YSTART( PORT ) * 1.1
                 YSTART( PORT ) = YSTART( PORT ) * 0.9
              ENDIF
           ENDIF
        ENDIF
      ELSE

*    Get the limits from storage in common block
        XSTART( PORT ) = AXSTART( PORT )
        XEND( PORT ) = AXEND( PORT )
        YSTART( PORT ) = AYSTART( PORT )
        YEND( PORT ) = AYEND( PORT )
      ENDIF

*    Get labels
      IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Getting labels', STATUS )
      SAME_UNITS = .FALSE.
      CALL P4_GET_AXLAB( PORT, SAME_UNITS, STATUS )

*    Clear display for new GRAPH, if desired and if possible
      IF ( DISPLAY_TYPE( PORT ) .EQ. 'GRAPH' .AND.
     :     PRE_ERASE_PLOT( PORT ) ) CALL P4_CLEARPORT( PORT, STATUS )

*    First select basic viewport
      PLOT_OK( PORT ) = .FALSE.
      CALL P4_SELPORT( PORT, STATUS )

*    Set up window
      AXSTART( PORT ) = XSTART( PORT )
      AXEND( PORT ) = XEND( PORT )
      AYSTART( PORT ) = YSTART( PORT )
      AYEND( PORT ) = YEND( PORT )
      IF ( PORT_OK( PORT ) ) THEN

        CALL PGWINDOW( XSTART( PORT ), XEND( PORT ),
     :     YSTART( PORT ), YEND( PORT ) )

*      Change the pen colour for OVERGRAPH
        IF ( DISPLAY_TYPE( PORT ) .EQ. 'OVERGRAPH' ) THEN
          CALL P4_COLTONUM( OVERCOLOUR( PORT ), CVAL, RD, GR, BL, STATUS )
          CALL PGQCI( CSAVE )
          CALL PGSCR( CVAL, RD, GR, BL )
          CALL PGSCI( CVAL )
        ENDIF

*      Plot the points, with error bars if possible and no offset
        IF ( CUT_DIRECTION( PORT ) .EQ. 'X' ) THEN
           IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Plotting X cut', STATUS )
           CALL P4_LINE( DIM, ISTART( PORT ), IEND( PORT ), %val( AXIS1_PTR ),
     :        %val( DS_PTR ), %val( ES_PTR ), %val( QS_PTR ), PLOT_ERRORS(PORT),
     :        QUALITY, 0.0, 0.0, STATUS )
        ELSE IF ( CUT_DIRECTION( PORT ) .EQ. 'Y' ) THEN
           IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Plotting Y cut', STATUS )
           CALL P4_LINE( DIM, ISTART( PORT ), IEND( PORT ), %val( AXIS2_PTR ),
     :        %val( DS_PTR ), %val( ES_PTR ), %val( QS_PTR ), PLOT_ERRORS(PORT),
     :        QUALITY, 0.0, 0.0, STATUS )
        ENDIF

*      Plot the ancillary bits if need be
        IF ( PLOT_AXES( PORT ) ) THEN

          IF ( DISPLAY_TYPE( PORT ) .EQ. 'GRAPH' ) THEN
            CALL PGBOX( DEVICE_XOPT( PORT ), 0.0, 0,
     :         DEVICE_YOPT( PORT ), 0.0, 0)

            XLEN = CHR_LEN( XLABEL(PORT) )
            YLEN = CHR_LEN( YLABEL(PORT) )
            IF ( TITLE( PORT ) .EQ. 'A_U_T_O' ) THEN
              TLEN = CHR_LEN( DEFTITLE(PORT) )
              SLEN = CHR_LEN( SUBTITLE(PORT) )
              CALL PGLABEL( XLABEL(PORT)(1:XLEN),
     :          YLABEL(PORT)(1:YLEN), DEFTITLE(PORT)(1:TLEN) )
              CALL PGMTEXT( 'T', 0.5, 0.5, 0.5, SUBTITLE(PORT)(1:SLEN) )
            ELSE
              TLEN = CHR_LEN( TITLE(PORT) )
              CALL PGLABEL( ' ', ' ', TITLE(PORT)(1:TLEN) )
            ENDIF
          ELSE IF ( DISPLAY_TYPE( PORT ) .EQ. 'OVERGRAPH' ) THEN
            OFFSET = 1.2 + REAL( LABELPOS )
            CPOS = CHR_LEN( RH1LABEL(PORT) )
            CALL PGMTEXT( 'R', OFFSET, 0.5, 0.5, RH1LABEL(PORT)(1:SLEN) )
            LABELPOS = MOD( LABELPOS+1, 4 )
            CALL PGSCI( CSAVE )
          ENDIF
        ENDIF
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'P4_GRAPH: Port has not been selected', STATUS )
      ENDIF

*    If OK, then set PLOT_OK to say that there is a valid plot in this port
      IF ( STATUS .EQ. SAI__OK ) THEN
         PLOT_OK( PORT ) = .TRUE.
         LAST_TYPE( PORT ) = DISPLAY_TYPE(PORT)
      ELSE
         PLOT_OK (PORT) = .FALSE.
         LAST_TYPE( PORT ) = 'UNKNOWN'
      ENDIF

*    Close open structures and return
 500  CONTINUE
      IF ( NDIM .GT. 1 ) THEN
        CALL DSA_FREE_WORKSPACE( DS_SLOT, STATUS )
        CALL DSA_FREE_WORKSPACE( ES_SLOT, STATUS )
        CALL DSA_FREE_WORKSPACE( QS_SLOT, STATUS )
        CALL DSA_FREE_WORKSPACE( CS_SLOT, STATUS )
      ENDIF
 600  IF ( QDUMMY ) CALL DSA_FREE_WORKSPACE( QUAL_SLOT, STATUS )
      IF ( EDUMMY ) CALL DSA_FREE_WORKSPACE( ERR_SLOT, STATUS )
      CALL DSA_CLOSE( STATUS )

      END
