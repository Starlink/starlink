*+  P4_READ_CONFIG - Read a configuration from an ASCII file
      SUBROUTINE P4_READ_CONFIG( PORT, STATUS )
*    Description :
*     This routine reads an ASCII configuration file and sets the common block
*    Invocation :
*     CALL P4_READ_CONFIG( PORT, STATUS )
*    Authors :
*     P N Daly (JACH::PND)
*    History :
*      7-Aug-1994: Original version                               (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'
*    Import :
      INTEGER PORT
*    Status :
      INTEGER STATUS                   ! Global status
*    External references :
      INTEGER CHR_LEN                  ! Length of string routine
*    Global variables :
      INCLUDE 'P4COM.INC'              ! P4 common block
*    Local constants :
      INTEGER MAXWRD                   ! Maximum number of words in string
      PARAMETER ( MAXWRD = 80 )
*    Local variables :
      CHARACTER*( NBS_FLEN )
     :  CVAL,                          ! Character encoded string
     :  STRUCTURE,                     ! Structure name
     :  TMPSTR,                        ! Temporary string
     :  FILE,                          ! Name of config file
     :  WORDS( MAXWRD ),               ! Words returned in string
     :  RECORD,                        ! Record read from config file
     :  ITEM,                          ! Noticeboard item
     :  CTYPE                          ! Noticeboard WORDS(3)
      INTEGER
     :  CLEN, CPOS1, CPOS2,            ! Length in character string
     :  LUN,                           ! Fortran logical unit number
     :  POS,                           ! Position in string
     :  I,                             ! A counter
     :  RECLEN,                        ! Length of a record
     :  EOF,                           ! The end-of-file marker
     :  NWRD,                          ! Number of words found in string
     :  START( MAXWRD ),               !
     :  STOP( MAXWRD ),
     :  IVALUE,                        ! Decoded integer value
     :  PORTNUM                        ! Port number
      REAL
     :  RVALUE                         ! Decoded real value
      LOGICAL
     :  DECODED_OK,                    ! T if string decoded OK
     :  LVALUE                         ! Decoded logical value
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   If 0 <= PORT <= MAXPORT, update just that port
      IF ( PORT.GE.0 .AND. PORT.LE.MAXPORT ) THEN
        CALL CHR_ITOC( PORT, CVAL, CLEN )
        STRUCTURE = 'PORT_' // CVAL(1:CLEN)
*   Else do them all
      ELSE
        STRUCTURE = 'PORT'
      ENDIF

*   Obtain the name of the configuration file to be read
      CALL CHR_FILL( ' ', FILE )
      CALL PAR_GET0C( 'FILE', FILE, STATUS )
      CPOS1 = INDEX( FILE, 'P4_CONFIG' )
      CPOS2 = INDEX( FILE, SEPARATOR )
      IF ( CPOS1.EQ.0 .AND. CPOS2.EQ.0 ) THEN
        FILE = PREFIX // 'P4_CONFIG' // SEPARATOR // FILE(1:CHR_LEN(FILE))
        CALL CHR_RMBLK( FILE )
      ENDIF
      IF ( INDEX( FILE, '.p4' ).EQ.0 ) FILE = FILE(1:CHR_LEN(FILE)) // '.p4'
      CALL P4_CHECK_INPUT( FILE, STATUS )

      IF ( VERBOSE ) THEN
        CALL MSG_SETC( 'FILE', FILE )
        CALL MSG_OUT( ' ', 'Reading configuration from file ^FILE', STATUS )
      ENDIF

*   Open the configuration file
      CALL FIO_OPEN( FILE(1:CHR_LEN(FILE)), 'READ', 'LIST', 0, LUN, STATUS )

*    Recursively read the contents of the file
      EOF = .FALSE.
      DO WHILE ( ( .NOT. EOF ) .AND. ( STATUS .EQ. SAI__OK ) )

*      Read the next record ignoring comment lines and blank lines
        CALL FIO_READ( LUN, RECORD, RECLEN, STATUS )

*      Test the status return for EOF
          IF ( STATUS .EQ. FIO__EOF ) THEN

            EOF = .TRUE.
            CALL ERR_ANNUL( STATUS )

*      Test the status return for an error
          ELSE IF ( STATUS .NE. SAI__OK ) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'P4_READ_CONFIG: '/
     :        /'Error reading configuration file', STATUS )

*      Ignore completely blank lines
          ELSE IF ( RECORD(1:RECLEN) .EQ. ' ' ) THEN
            IF ( VERBOSE ) THEN
              CALL MSG_OUT( ' ', 'Ignoring blank lines', STATUS )
            ENDIF

*      Ignore comment lines beginning with *
          ELSE IF ( INDEX( RECORD(1:RECLEN), '*' ) .GT. 0 ) THEN
            IF ( VERBOSE ) THEN
              CALL MSG_OUT( ' ', 'Ignoring record beginning with *', STATUS )
            ENDIF

*      Ignore comment lines beginning with !
          ELSE IF ( INDEX( RECORD(1:RECLEN), '!' ) .GT. 0 ) THEN
            IF ( VERBOSE ) THEN
              CALL MSG_OUT( ' ', 'Ignoring record beginning with !', STATUS )
            ENDIF

*      Ignore comment lines beginning with {
          ELSE IF ( INDEX( RECORD(1:RECLEN), '{' ) .GT. 0 ) THEN
            IF ( VERBOSE ) THEN
              CALL MSG_OUT( ' ', 'Ignoring record beginning with {', STATUS )
            ENDIF

*      Ignore comment lines beginning with #
          ELSE IF ( INDEX( RECORD(1:RECLEN), '#' ) .GT. 0 ) THEN
            IF ( VERBOSE ) THEN
              CALL MSG_OUT( ' ', 'Ignoring record beginning with #', STATUS )
            ENDIF

*      Ignore lines not containing the required port number
          ELSE IF ( INDEX( RECORD(1:RECLEN), STRUCTURE(1:CHR_LEN(STRUCTURE)) ) .EQ. 0 ) THEN
            IF ( VERBOSE ) THEN
              CALL MSG_OUT( ' ', 'Ignoring line not referring to required port', STATUS )
            ENDIF

*      Decode the string
          ELSE
            CALL CHR_DCWRD( RECORD, MAXWRD, NWRD, START, STOP, WORDS, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
              STATUS = SAI__ERROR
              CALL MSG_SETC( 'RECORD', RECORD )
              CALL ERR_REP( ' ', 'P4_READ_CONFIG: '/
     :          /'Failed to decode ^RECORD into constituent words', STATUS )
            END IF

*        The first word must be PUTNBS
            CALL CHR_UCASE( WORDS(1) )
            IF ( INDEX( WORDS(1), 'PUTNBS' ) .GT. 0 ) THEN
              DECODED_OK = .TRUE.
            ELSE
              DECODED_OK = .FALSE.
            END IF

*       Populate the common block item
            IF ( DECODED_OK ) THEN

              CALL CHR_RMBLK( WORDS(2) )
              CALL CHR_UCASE( WORDS(2) )
              CALL CHR_RMBLK( WORDS(3) )
              CALL CHR_FILL( ' ', TMPSTR )
              TMPSTR = WORDS(3)
*             CALL CHR_UCASE( WORDS(3) )

*        First get the item name and port number, from words(2)
              POS = 0
              POS = INDEX( WORDS(2), 'PORT_' )
              ITEM = WORDS(2)(POS+7:CHR_LEN(WORDS(2))-2)

              CVAL = WORDS(2)(POS+5:POS+5)
              CALL CHR_CTOI( CVAL, PORTNUM, STATUS )
              IF ( PORTNUM.LT.0 .AND. PORTNUM.GT.MAXPORT ) THEN
                STATUS = SAI__ERROR
                CALL ERR_REP( ' ', 'P4_READ_CONFIG: '/
     :            /'Failed to get port number for translation', STATUS )
              ENDIF

*        If it contains "", assume a character string
              IF ( INDEX( WORDS(3), '"') .GT. 0 ) THEN

*              Remove " from start of string
                DO I = 1, CHR_LEN( WORDS(3) ), 1
                  IF ( WORDS(3)( I:I ) .EQ. '"' ) WORDS(3)( I:I ) = ' '
                END DO
                CALL CHR_RMBLK( WORDS(3) )
                CTYPE = WORDS(3)

*              Remove " from end of string
                DO I = 1, CHR_LEN( WORDS(NWRD) ), 1
                  IF ( WORDS(NWRD)( I:I ) .EQ. '"' ) WORDS(NWRD)( I:I ) = ' '
                END DO
                CALL CHR_RMBLK( WORDS(NWRD) )
                CTYPE = WORDS(NWRD)

*              Reconstruct the TITLE in a temporary string since CHR_DCWRD destroys it
                DO I = 1, CHR_LEN( TMPSTR ), 1
                  IF ( TMPSTR( I:I ) .EQ. '"' ) TMPSTR( I:I ) = ' '
                END DO
                CALL CHR_RMBLK( TMPSTR )
                IF ( NWRD .GE. 4 ) THEN
                  DO I = 4, NWRD, 1
                    TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // ' ' // WORDS(I)(1:CHR_LEN(WORDS(I)))
                  ENDDO
                ENDIF

*              Now populate the common block
                IF ( ITEM.EQ.'DEVICE_NAME' ) THEN
                  CALL CHR_FILL( ' ', DEVICE_NAME(PORTNUM) )
                  DEVICE_NAME(PORTNUM) = CTYPE(1:CHR_LEN(CTYPE))
                ELSE IF ( ITEM.EQ.'DEVICE_XOPT' ) THEN
                  CALL CHR_FILL( ' ', DEVICE_XOPT(PORTNUM) )
                  DEVICE_XOPT(PORTNUM) = CTYPE(1:CHR_LEN(CTYPE))
                ELSE IF ( ITEM.EQ.'DEVICE_YOPT' ) THEN
                  CALL CHR_FILL( ' ', DEVICE_YOPT(PORTNUM) )
                  DEVICE_YOPT(PORTNUM) = CTYPE(1:CHR_LEN(CTYPE))
                ELSE IF ( ITEM.EQ.'DEVICE_LUT' ) THEN
                  CALL CHR_FILL( ' ', DEVICE_LUT(PORTNUM) )
                  DEVICE_LUT(PORTNUM) = CTYPE(1:CHR_LEN(CTYPE))
                ELSE IF ( ITEM.EQ.'DISPLAY_DATA' ) THEN
                  CALL CHR_FILL( ' ', DISPLAY_DATA(PORTNUM) )
                  DISPLAY_DATA(PORTNUM) = CTYPE(1:CHR_LEN(CTYPE))
                ELSE IF ( ITEM.EQ.'TITLE' ) THEN
                  CALL CHR_FILL( ' ', TITLE(PORTNUM) )
                  TITLE(PORTNUM) = TMPSTR(1:CHR_LEN(TMPSTR))
                ELSE IF ( ITEM.EQ.'DISPLAY_TYPE' ) THEN
                  CALL CHR_FILL( ' ', DISPLAY_TYPE(PORTNUM) )
                  DISPLAY_TYPE(PORTNUM) = CTYPE(1:CHR_LEN(CTYPE))
                ELSE IF ( ITEM.EQ.'DISPLAY_PLANE' ) THEN
                  CALL CHR_FILL( ' ', DISPLAY_PLANE(PORTNUM) )
                  DISPLAY_PLANE(PORTNUM) = CTYPE(1:CHR_LEN(CTYPE))
                ELSE IF ( ITEM.EQ.'CONTOUR_TYPE' ) THEN
                  CALL CHR_FILL( ' ', CONTOUR_TYPE(PORTNUM) )
                  CONTOUR_TYPE(PORTNUM) = CTYPE(1:CHR_LEN(CTYPE))
                ELSE IF ( ITEM.EQ.'OVERCOLOUR' ) THEN
                  CALL CHR_FILL( ' ', OVERCOLOUR(PORTNUM) )
                  OVERCOLOUR(PORTNUM) = CTYPE(1:CHR_LEN(CTYPE))
                ELSE IF ( ITEM.EQ.'COLOUR_STYLE' ) THEN
                  CALL CHR_FILL( ' ', COLOUR_STYLE(PORTNUM) )
                  COLOUR_STYLE(PORTNUM) = CTYPE(1:CHR_LEN(CTYPE))
                ELSE IF ( ITEM.EQ.'FG_COLOUR' ) THEN
                  CALL CHR_FILL( ' ', FG_COLOUR(PORTNUM) )
                  FG_COLOUR(PORTNUM) = CTYPE(1:CHR_LEN(CTYPE))
                ELSE IF ( ITEM.EQ.'BG_COLOUR' ) THEN
                  CALL CHR_FILL( ' ', BG_COLOUR(PORTNUM) )
                  BG_COLOUR(PORTNUM) = CTYPE(1:CHR_LEN(CTYPE))
                ELSE IF ( ITEM.EQ.'CUT_DIRECTION' ) THEN
                  CALL CHR_FILL( ' ', CUT_DIRECTION(PORTNUM) )
                  CUT_DIRECTION(PORTNUM) = CTYPE(1:CHR_LEN(CTYPE))
                ELSE IF ( ITEM.EQ.'LAST_TYPE' ) THEN
                  CALL CHR_FILL( ' ', LAST_TYPE(PORTNUM) )
                  LAST_TYPE(PORTNUM) = CTYPE(1:CHR_LEN(CTYPE))
                ELSE
                  CALL MSG_SETC( 'VAL', ITEM )
                  CALL MSG_OUT( ' ', 'Unable to restore char item ^VAL', STATUS )
                ENDIF

*        If it says TRUE or FALSE, assume a logical string
              ELSE IF ( ( INDEX( WORDS(3), 'TRUE' ).GT.0 )   .OR.
     :                  ( INDEX( WORDS(3), 'FALSE' ).GT.0 ) ) THEN

                CALL CHR_CTOL( WORDS(3), LVALUE, STATUS )

                IF ( ITEM.EQ.'PLOT_AXES' ) THEN
                  PLOT_AXES(PORTNUM) = LVALUE
                ELSE IF ( ITEM.EQ.'PLOT_ERRORS' ) THEN
                  PLOT_ERRORS(PORTNUM) = LVALUE
                ELSE IF ( ITEM.EQ.'PLOT_WHOLE' ) THEN
                  PLOT_WHOLE(PORTNUM) = LVALUE
                ELSE IF ( ITEM.EQ.'PRE_ERASE_PLOT' ) THEN
                  PRE_ERASE_PLOT(PORTNUM) = LVALUE
                ELSE IF ( ITEM.EQ.'AUTOSCALE' ) THEN
                  AUTOSCALE(PORTNUM) = LVALUE
                ELSE IF ( ITEM.EQ.'PORT_OK' ) THEN
                  PORT_OK(PORTNUM) = LVALUE
                ELSE IF ( ITEM.EQ.'PLOT_OK' ) THEN
                  PLOT_OK(PORTNUM) = LVALUE
                ELSE
                  CALL MSG_SETC( 'VAL', ITEM )
                  CALL MSG_OUT( ' ', 'Unable to restore log item ^VAL', STATUS )
                ENDIF

*        If it contains ., assume a real string
              ELSE IF ( INDEX( WORDS(3), '.' ).GT.0 ) THEN

                CALL CHR_CTOR( WORDS(3), RVALUE, STATUS )

                IF ( ITEM.EQ.'VXSTART' ) THEN
                  VXSTART(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'VXEND' ) THEN
                  VXEND(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'VYSTART' ) THEN
                  VYSTART(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'VYEND' ) THEN
                  VYEND(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'AXSTART' ) THEN
                  AXSTART(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'AXEND' ) THEN
                  AXEND(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'AYSTART' ) THEN
                  AYSTART(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'AYEND' ) THEN
                  AYEND(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'XSTART' ) THEN
                  XSTART(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'XEND' ) THEN
                  XEND(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'YSTART' ) THEN
                  YSTART(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'YEND' ) THEN
                  YEND(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'MODE' ) THEN
                  MODE(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'MEAN' ) THEN
                  MEAN(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'SIGMA' ) THEN
                  SIGMA(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'LOW' ) THEN
                  LOW(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'HIGH' ) THEN
                  HIGH(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'FMIN' ) THEN
                  FMIN(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'FMAX' ) THEN
                  FMAX(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'SLICE_START' ) THEN
                  SLICE_START(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'SLICE_END' ) THEN
                  SLICE_END(PORTNUM) = RVALUE
                ELSE IF ( ITEM.EQ.'CHAR_HEIGHT' ) THEN
                  CHAR_HEIGHT(PORTNUM) = RVALUE
                ELSE
                  CALL MSG_SETC( 'VAL', ITEM )
                  CALL MSG_OUT( ' ', 'Unable to restore real item ^VAL', STATUS )
                ENDIF

*        Otherwise assume it is an integer string
              ELSE
                CALL CHR_CTOI( WORDS(3), IVALUE, STATUS )

                IF ( ITEM.EQ.'CONTOUR_LEVELS' ) THEN
                  CONTOUR_LEVELS(PORTNUM) = IVALUE
                ELSE IF ( ITEM.EQ.'HISTOGRAM_BINS' ) THEN
                  HISTOGRAM_BINS(PORTNUM) = IVALUE
                ELSE IF ( ITEM.EQ.'HISTOGRAM_XSTEP' ) THEN
                  HISTOGRAM_XSTEP(PORTNUM) = IVALUE
                ELSE IF ( ITEM.EQ.'HISTOGRAM_YSTEP' ) THEN
                  HISTOGRAM_YSTEP(PORTNUM) = IVALUE
                ELSE IF ( ITEM.EQ.'HIST_SMOOTH' ) THEN
                  HIST_SMOOTH(PORTNUM) = IVALUE
                ELSE IF ( ITEM.EQ.'TOOSMALL' ) THEN
                  TOOSMALL(PORTNUM) = IVALUE
                ELSE IF ( ITEM.EQ.'TOOLARGE' ) THEN
                  TOOLARGE(PORTNUM) = IVALUE
                ELSE IF ( ITEM.EQ.'ISTART' ) THEN
                  ISTART(PORTNUM) = IVALUE
                ELSE IF ( ITEM.EQ.'IEND' ) THEN
                  IEND(PORTNUM) = IVALUE
                ELSE IF ( ITEM.EQ.'JSTART' ) THEN
                  JSTART(PORTNUM) = IVALUE
                ELSE IF ( ITEM.EQ.'JEND' ) THEN
                  JEND(PORTNUM) = IVALUE
                ELSE
                  CALL MSG_SETC( 'VAL', ITEM )
                  CALL MSG_OUT( ' ', 'Unable to restore int item ^VAL', STATUS )
                ENDIF
              ENDIF
            ELSE
              STATUS = SAI__ERROR
              CALL ERR_REP( ' ', 'P4_READ_CONFIG: '/
     :          /'Unknown file format detected', STATUS )
            ENDIF
          ENDIF
        ENDDO

*    Close the file
       CALL FIO_CLOSE( LUN, STATUS )
       END
