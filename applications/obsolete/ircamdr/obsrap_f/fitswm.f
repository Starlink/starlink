        SUBROUTINE FITSWM( STATUS)

*+ FITSWM - write images to tape in fits format with head information.
*
*    Description : Images are written to a tape in fits format using MT
*                  commands. Takes input from either IRCAM observation HDS
*                  container files or stand-alone processed HDS images or
*                  both. Stand-alone image names read from free-format AscII
*                  file created with edit or DCL dir output. The image names
*                  should NOT contain the .SDF file extension as HDS does not
*                  like that ...
*
*    Invocation : CALL FITSWM( STATUS)
*
*    Parameters :
*
*    Method :
*
*    Bugs :
*
*    Authors :
*
*     Colin Aspin (ROE/UKIRT) : 7th July 1987
*
*    History :
*
*     07-07-1987 : First implementation
*     24-JUN-1994  Changed LIB$ to FIO_, STR$ to CHR_,
*                  MSG_OUT on error to ERR_REP (SKL@JACH)
*     07-SEP-1994  Reduced continuation lines for UNIX compiler (SKL@JACH)
*     11-AUG-2004  Use FIO for open (otherwsie non-portable)
*
*    Type definitions :

        IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

        INCLUDE  'SAE_PAR'          ! SSE global definitions
        INCLUDE  'DAT_PAR'          ! Necessary for non-VMS
        INCLUDE  'FIO_PAR'
        INCLUDE  'CHR_ERR'

*    Status :

        INTEGER  STATUS             ! global status parameter

*    Local constants :

        INTEGER MTCHAN              ! Channel for mag tape deck writing
        PARAMETER (MTCHAN = 42)

*    Local variables :

        INTEGER
     :    CONTEND( 100),            ! end image in container file
     :    CONTSTART( 100),          ! start image in container file
     :    COUNTER,                  ! loop counter
     :    DATA_FLAG,                ! flag to tell which data buffers full
     :    DIMS( 2 ),                ! dimensions of input DATA_ARRAY
     :    IMCODE,                   ! code for image parameter
     :    IMCODE2,                  ! code for image parameter
     :    J,                        ! loop counter
     :    JTAPE,                    ! return status for tape writing
     :    K                         ! loop variable
      INTEGER
     :    KL,                       ! length of observation number variable
     :    LEN,                      ! length of character variables
     :    LUN,                      ! logical unit number for input file
     :    LUNO,                     ! logical unit number for output disk
     :    MTWR,                     ! function for writing to tape
     :    L1                        ! used for length of characters
      INTEGER
     :    NDIMS,                    ! input image dimensionality
     :    NUMCONT,                  ! number of container files specified
     :    PNTRI,                    ! pointer to input DATA_ARRAY component
     :    STARTNUM,                 ! start number of fits disk files
     :    STARTOBS,                 ! start observation element in struc
     :    ENDOBS                    ! end observation element in struc

      REAL
     :    AIRMASS,                  ! airmass
     :    BIAS,                     ! bias voltage level
     :    BSCALE,                   ! scaling factor
     :    BZERO,                    ! zero offset factor
     :    COADDS,                   ! number of coadds
     :    DEC( 3),                  ! telescope declination
     :    EQUINOX,                  ! equinox of telescope coordinates
     :    EVENMEAN,                 ! even channel mean from statistics
     :    EVENSTD,                  ! even channel standard deviation from stats
     :    EXPO,                     ! exposure time on chip
     :    FPX,                      ! FP X position
     :    FPY,                      ! FP Y position
     :    FPZ,                      ! FP Z position
     :    HEIGHT,                   ! height of telescope above sea level
     :    HA( 3)                    ! Hour Angle
      REAL
     :    GAIN,                     ! system gain
     :    GATE,                     ! gate voltage level
     :    LAT( 3),                  ! latitude of telescope
     :    LONG( 3),                 ! longitude of telescope
     :    MAXXY,                    ! maximum in data image
     :    MAXVAL,                   ! maximum calculated in program
     :    MINNY,                    ! minimum in data image
     :    MINVAL,                   ! minimum calculated in program
     :    ODDMEAN,                  ! odd channel mean from statistics
     :    ODDSTD,                   ! odd channel standard deviation from stats
     :    OFFSETDEC,                ! DEC offset of telescope
     :    OFFSETRA,                 ! RA offset of telescope
     :    PLATESCALE,               ! plate scale in image
     :    RA( 3),                   ! ra of telescope
     :    READRATE                  ! readout rate of array
      REAL
     :    ST( 3),                   ! Siderial Time
     :    TEMP,                     ! temperature of array
     :    UT( 4),                   ! UT of observation
     :    XHEAD_ARCSECPMM,          ! Xhead arcsec/mm scale
     :    XHEAD_DEC_ZERO,           ! Xhead DEC zero point
     :    XHEAD_RA_ZERO,            ! Xhead RA zero point
     :    XHEAD_DEC,                ! Xhead DEC position
     :    XHEAD_RA                  ! Xhead RA position

      CHARACTER*(DAT__SZLOC)      ! locator for :
     :    LOCI,                     ! input data structure
     :    LOCOBS,                   ! observation structure
     :    LOCCELL,                  ! observation cell
     :    LOCGEN,                   ! general structure
     :    LOCINST,                  ! instrument structure
     :    LOCID,                    ! id structure
     :    LOCTEL,                   ! telescope structure
     :    LOCBUFF,                  ! data buffer
     :    LOCDA                     ! data array buffer

      CHARACTER
     :    CCOUNTER*80,              ! character version of counter
     :    COMMENTS( 5)*80,          ! comments from data file
     :    DISKFITS*80,              ! name of disk fits file
     :    FFILE*80,                 ! name of free-format file with names
     :    KC*10,                    ! character version of observation number
     :    IMAGENAME*80,             ! name of image
     :    SOURCE*80,                ! source of image input C, F or B
     :    CONTNAME( 100)*80,        ! name of container files C
     :    MODE*80,                  ! KTC mode of observation, ON/OFF
     :    CONF*80,                  ! type od observation, STARE/CHOP
     :    ARRAY*80,                 ! array type used
     :    INST*80,                  ! instrument used
     :    OBSERVERS*80,             ! names of observers
     :    ORIGIN*80                 ! origin of observers
      CHARACTER
     :    SOFTWARE*80,              ! writer of software (me)
     :    TELESCOPE*80,             ! telescope used
     :    FILTER*80,                ! filter used
     :    LOCALTIME*80,             ! local time of observation
     :    OBJECT*80,                ! object name
     :    TITLE*80,                 ! title of observation
     :    TAPEORDISK*80             ! tape of disk write

        LOGICAL
     :    MORE,                     ! another input variable
     :    SWAPBYTES                 ! swap bytes on tape
*-
*      check status on entry - return if not o.k.
        IF ( STATUS .NE. SAI__OK ) THEN
          RETURN
        END IF

*      Welcome the users to the FITSWM program
        CALL MSG_OUT( 'BLANK', ' ', STATUS)
        CALL MSG_OUT( 'MESSAGE',
     :    'IRCAM FITS disk-disk or disk-tape writing utility',
     :    STATUS)
        CALL MSG_OUT( 'BLANK', ' ', STATUS)

*      Write header for the output list file
        WRITE( 1, *) 'IRCAM FITSW list file'
        WRITE( 1, *) '*********************'
        WRITE( 1, *) ' '
        WRITE( 1, *) 'List of IRCAM images written in FITS format'
        WRITE( 1, *) ' '

*      Initialize the counting variable for number of image written to tape
        COUNTER = 0

*      get the code of the image interface file entry parameter
        CALL SUBPAR_FINDPAR( 'INPIC', IMCODE, STATUS)
        CALL SUBPAR_FINDPAR( 'INPICI', IMCODE2, STATUS)

*      get option to write to tape of to disk in fits format
        CALL MSG_OUT( 'BLANK', ' ', STATUS)
        CALL PAR_GET0C( 'TAPEORDISK', TAPEORDISK, STATUS)
        CALL CHR_UCASE( TAPEORDISK )
        IF( TAPEORDISK( 1:1) .NE. 'T' .AND.
     :      TAPEORDISK( 1:1) .NE. 'D') TAPEORDISK( 1:1) = 'T'

*      get the input source option from user
        CALL MSG_OUT( 'BLANK', ' ', STATUS)
        CALL MSG_OUT( 'MESSAGE',
     :'Data image input sources are C = IRCAM container files',
     :    STATUS)
        CALL MSG_OUT( 'MESSAGE',
     :'                          F = List processed images in free',
     :    STATUS)
        CALL MSG_OUT( 'MESSAGE',
     :    '                                 format list file',
     :    STATUS)
        CALL MSG_OUT( 'MESSAGE',
     :'                          B = Both the above input sources',
     :    STATUS)
        CALL MSG_OUT( 'MESSAGE',
     :'                          I = Separate IRCAM images by name',
     :    STATUS)
        CALL MSG_OUT( 'MESSAGE',
     :'                          S = IRCAM SNAPSHOT SDF files',
     :    STATUS)
        CALL MSG_OUT( 'BLANK', ' ', STATUS)
        CALL PAR_GET0C( 'SOURCE', SOURCE, STATUS)
        CALL CHR_UCASE( SOURCE )

*      test the input source option
        IF( SOURCE( 1:1) .EQ. 'C' .OR. SOURCE( 1:1) .EQ. 'B' .OR.
     :      SOURCE( 1:1) .EQ. 'S') THEN
          CALL MSG_OUT( 'BLANK', ' ', STATUS)
          CALL MSG_OUT( 'MESSAGE',
     :                  'Container file input section ...',
     :                  STATUS)
          CALL MSG_OUT( 'BLANK', ' ', STATUS)

*        get the number of container files to be written to tape
          CALL PAR_GET0I( 'NUMCONT', NUMCONT, STATUS)

*        loop to get all container filenames and start,end obs elements
          DO J = 1, NUMCONT
            CALL MSG_OUT( 'BLANK', ' ', STATUS)
            CALL PAR_GET0C( 'CONTNAME', CONTNAME( J), STATUS)
            CALL MSG_OUT( 'BLANK', ' ', STATUS)
            CALL PAR_GET0I( 'CONTSTART', CONTSTART( J), STATUS)
            CALL PAR_GET0I( 'CONTEND', CONTEND( J), STATUS)
            CALL MSG_OUT( 'BLANK', ' ', STATUS)
            CALL PAR_CANCL( 'CONTNAME', STATUS)
            CALL PAR_CANCL( 'CONTSTART', STATUS)
            CALL PAR_CANCL( 'CONTEND', STATUS)
          END DO
        END IF

*      Get the name of the free format file ...
        IF( SOURCE( 1:1) .EQ. 'F' .OR. SOURCE( 1:1) .EQ. 'B') THEN
          CALL MSG_OUT( 'BLANK', ' ', STATUS)
          CALL MSG_OUT( 'MESSAGE', 'Single file input section ...',
     :                  STATUS)
          CALL MSG_OUT( 'BLANK', ' ', STATUS)

*        get the name of the file containing the list of images
  400     CONTINUE
          CALL MSG_OUT( 'BLANK', ' ', STATUS)
          CALL PAR_GET0C( 'FILENAME', FFILE, STATUS)
          CALL PAR_CANCL( 'FILENAME', STATUS)
          CALL FIO_OPEN(FFILE,'READ','NONE',0,LUN,STATUS)
          IF (STATUS .EQ. SAI__OK) GO TO 300
          CALL MSG_SETC( 'FFILE', FFILE)
          CALL ERR_REP( 'MESSAGE',
     :      'Error, cannot find file ... ^FFILE',
     :      STATUS)
          CALL ERR_ANNUL( STATUS )
          GOTO 400
  300     CONTINUE
          CALL FIO_CLOSE( LUN, STATUS )
          CALL MSG_OUT( 'BLANK', ' ', STATUS)
        END IF

*      get the start number for the disk fits files if that is slected
        IF( TAPEORDISK( 1:1) .EQ. 'D') THEN
          CALL MSG_OUT( 'BLANK', ' ', STATUS)
          CALL PAR_GET0I( 'STARTNUM', STARTNUM, STATUS)
          CALL MSG_OUT( 'BLANK', ' ', STATUS)
        ELSE
          STARTNUM = 1
        END IF

*      get option to swap bytes on disk
        CALL PAR_GET0L( 'SWAPBYTES', SWAPBYTES, STATUS)
        CALL MSG_OUT( 'BLANK', ' ', STATUS)

*      initialize the counting variable for number on tape
        COUNTER = 0

*      Write the container file images to tape ...
        IF( SOURCE( 1:1) .EQ. 'C' .OR. SOURCE( 1:1) .EQ. 'B') THEN

*        loop to scan through all input container files and write them to tape
          DO J = 1, NUMCONT

*          set the parameter with the current container filename
            CALL SUBPAR_PUTNAME ( IMCODE, CONTNAME( J), STATUS)

*          put name to upper case and trim it to get length
            CALL CHR_UCASE( CONTNAME( J) )

*          get locator to the input IMAGE type data structure
            CALL DAT_EXIST( 'INPIC', 'READ', LOCI, STATUS)
            IF( STATUS .NE. SAI__OK) THEN
              STATUS = SAI__OK
              CALL MSG_SETC( 'CONT', CONTNAME( J))
              CALL MSG_OUT( 'MESSAGE',
     :         'ERROR, IRCAM observation file ^CONT NOT FOUND ...',
     :         STATUS)
              GOTO 500
            END IF
            CALL DAT_FIND( LOCI, 'OBS', LOCOBS, STATUS)
            CALL DAT_FIND( LOCI, 'GENERAL', LOCGEN, STATUS)
            CALL DAT_FIND( LOCGEN, 'INSTRUMENT', LOCINST, STATUS)
            CALL DAT_FIND( LOCGEN, 'ID', LOCID, STATUS)
            CALL DAT_FIND( LOCGEN, 'TELESCOPE', LOCTEL, STATUS)

*          get size of OBS structure and setup number elements
            CALL DAT_SHAPE( LOCOBS, 2, DIMS, NDIMS, STATUS)
            STARTOBS = MIN( CONTSTART( J), DIMS( 1))
            ENDOBS = MIN( CONTEND( J), DIMS( 1))
            CALL MSG_SETI( 'ST', STARTOBS)
            CALL MSG_SETI( 'EN', ENDOBS)
            CALL MSG_OUT( 'MESS',
     :       'Start observation = ^ST, End observation = ^EN', STATUS)

*          loop for all images from container file specified by user
            DO K = STARTOBS, ENDOBS

*            get the parameters from the current container file observation
*            after getting locator to current obs cell elements
              NDIMS = 1
              DIMS( 1) = K
              CALL DAT_CELL( LOCOBS, NDIMS, DIMS, LOCCELL, STATUS)

*            get the primitive parameters defining observation for header
              CALL FITSWGET( LOCINST, LOCID, LOCTEL, LOCCELL, ARRAY,
     :                       INST, PLATESCALE, OBSERVERS, ORIGIN,
     :                       SOFTWARE, HEIGHT, LAT, LONG,
     :                       TELESCOPE, BIAS, CONF, MAXXY,
     :                       MINNY, DEC, AIRMASS, UT,
     :                       EQUINOX, EVENMEAN, EVENSTD, EXPO,
     :                       FPX, FPY, FPZ, FILTER, GAIN, GATE,
     :                       HA, LOCALTIME, MODE, COADDS, OBJECT,
     :                       ODDMEAN, ODDSTD, OFFSETDEC,
     :                       OFFSETRA, RA, READRATE, ST, TEMP,
     :                       TITLE, XHEAD_ARCSECPMM,
     :                       XHEAD_DEC_ZERO, XHEAD_RA_ZERO,
     :                       XHEAD_DEC, XHEAD_RA, COMMENTS, STATUS )

*            test the status to see if the find on the primitives in the file
*            was successful, if not then tell user and release this obs element
              IF( STATUS .NE. SAI__OK) THEN
                STATUS = SAI__OK
                CALL MSG_SETI( 'NUMO', K)
                CALL MSG_SETC( 'CON', CONTNAME( J))
                CALL MSG_OUT( 'MESSAGE',
     :'IRCAM Container file ^CON, Obs number ^NUMO NOT FOUND ...',
     :           STATUS)
                GOTO 200
              END IF

*            set number of dimensions to zero for scalar primitives
              NDIMS = 0

*            put the mode and configuration to upper case for testing
              CALL CHR_UCASE( MODE )
              CALL CHR_UCASE( CONF )

*            test if the configuration is legal i.e. either STARE or CHOP and
*            mode is legal i.e. KTC ON or KTC OFF. Set the flag for the buffers
*            filled
              IF( CONF .EQ. 'STARE' .AND. MODE .EQ. 'KTC OFF') THEN
                DATA_FLAG = 1
              ELSE IF( CONF .EQ. 'STARE' .AND. MODE .EQ. 'KTC ON')
     :         THEN
                DATA_FLAG = 2
              ELSE IF( CONF .EQ. 'CHOP' .AND. MODE .EQ. 'KTC OFF')
     :         THEN
                DATA_FLAG = 3
              ELSE IF( CONF .EQ. 'CHOP' .AND. MODE .EQ. 'KTC ON')
     :         THEN
                DATA_FLAG = 4
              ELSE
                DATA_FLAG = 0
              END IF

*            write the correct data buffers to tape in fits format
              IF( DATA_FLAG .EQ. 1 .OR.
     :            DATA_FLAG .EQ. 2 .OR.
     :            DATA_FLAG .EQ. 3 .OR.
     :            DATA_FLAG .EQ. 4 ) THEN

*              for any data flag other than 0 get buffer 1, PHASEA
                CALL DAT_FIND( LOCCELL, 'PHASEA', LOCBUFF, STATUS)
                CALL DAT_FIND( LOCBUFF, 'DATA_ARRAY', LOCDA, STATUS)
                CALL DAT_SHAPE( LOCDA, 2, DIMS, NDIMS, STATUS)

*              test the status to see if the find on the data_array etc
*              was successful, if not then tell user and release this element

                IF( STATUS .NE. SAI__OK) THEN
                  STATUS = SAI__OK
                  CALL MSG_SETI( 'NUMO', K)
                  CALL MSG_SETC( 'CON', CONTNAME( J))
                  CALL MSG_OUT( 'MESSAGE',
     :'IRCAM Container file ^CON, Obs number ^NUMO NOT FOUND ...',
     :             STATUS)
                  CALL DAT_ANNUL( LOCDA, STATUS)
                  CALL DAT_ANNUL( LOCBUFF, STATUS)
                  STATUS = SAI__OK
                  GOTO 200
                END IF

*              map the data array component ...
                CALL DAT_MAPR( LOCDA, 'READ', NDIMS, DIMS, PNTRI,
     :                         STATUS)

*              increment counter for number of images written to tape
                COUNTER = COUNTER + 1

*              call subroutine to calculate max,min in data and BZERO, BSCALE
                CALL FITSWCALB( DIMS( 1), DIMS( 2), %VAL( PNTRI),
     :                          MAXVAL, MINVAL, BZERO, BSCALE)

*              tell user of the max,min and b values calculated
                CALL MSG_SETI( 'NUM', K)
                CALL MSG_SETC( 'CON', CONTNAME( J))
                CALL MSG_OUT( 'MESSAGE',
     :'IRCAM Container file ^CON, Obs number ^NUM, PHASE A data ...',
     :            STATUS)
                CALL MSG_SETR( 'MAX', MAXVAL)
                CALL MSG_SETR( 'MIN', MINVAL)
                CALL MSG_OUT( 'MESSAGE',
     :            'Maximum = ^MAX, Minimum = ^MIN',
     :            STATUS)
                CALL MSG_SETR( 'BZE', BZERO)
                CALL MSG_SETR( 'BSC', BSCALE)
                CALL MSG_OUT( 'MESSAGE',
     :            'Bzero   = ^BZE, Bscale  = ^BSC',
     :            STATUS)

*              form the image name string
                CALL CHR_ITOC( K, KC, KL )
                CALL CHR_CLEAN( CONTNAME( J) )
                LEN = 0
                CALL CHR_APPND( CONTNAME( J), CONTNAME( J), LEN )
                IMAGENAME =
     :           CONTNAME( J)( 1:LEN)//'.OBS('//KC(1:KL)//').PHASEA'

*              write header to the text file
                WRITE( 1, '(1X,I5,2X,A50)') COUNTER, IMAGENAME

*              get lun for output disk fits if requested
                IF( TAPEORDISK( 1:1) .EQ. 'D') THEN
                  CALL CHR_ITOC(STARTNUM, CCOUNTER, L1)
                  STARTNUM = STARTNUM + 1
                  CALL CHR_CLEAN( CCOUNTER)
                  L1 = 0
                  CALL CHR_APPND( CCOUNTER, CCOUNTER, L1)
                  DISKFITS = 'FITS'//CCOUNTER( 1:L1)//'.FITS'
                  CALL RIO_OPEN(DISKFITS,'WRITE','UNFORMATTED',
     :                 2880, LUNO, STATUS )
                  IF (STATUS .NE. SAI__OK) GO TO 996
                  CALL MSG_SETC( 'FI', DISKFITS )
                  CALL MSG_OUT( 'MESS', 'Disk FITS file = ^FI',
     :                           STATUS)
                END IF

*              call subroutine to write image to tape in fits format
                CALL FITSWSUB( TAPEORDISK, MTCHAN, LUNO,
     :                         DIMS( 1), DIMS( 2),
     :                         %VAL( PNTRI), BZERO, BSCALE,
     :                         ARRAY, INST, PLATESCALE,
     :                         OBSERVERS, ORIGIN, SOFTWARE,
     :                         HEIGHT, LAT, LONG,
     :                         TELESCOPE, BIAS, CONF,
     :                         MAXVAL, MINVAL, DEC,
     :                         AIRMASS, EQUINOX, EVENMEAN,
     :                         EVENSTD, EXPO, FILTER,
     :                         GAIN, GATE, LOCALTIME,
     :                         MODE, COADDS, OBJECT,
     :                         ODDMEAN, ODDSTD, OFFSETDEC,
     :                         OFFSETRA, RA, READRATE,
     :                         TEMP, TITLE, IMAGENAME,
     :                         UT, HA, ST, FPX, FPY, FPZ,
     :                         XHEAD_ARCSECPMM,
     :                         XHEAD_DEC_ZERO, XHEAD_RA_ZERO,
     :                         XHEAD_DEC, XHEAD_RA,
     :                         COMMENTS, SWAPBYTES)

*              release lun for output disk fits file
                IF( TAPEORDISK( 1:1) .EQ. 'D') THEN
                   CALL RIO_CLOSE( LUNO, STATUS )
                END IF

*              annul locators for the phaseA data buffer components
                CALL DAT_ANNUL( LOCDA, STATUS)
                CALL DAT_ANNUL( LOCBUFF, STATUS)
              END IF
              IF( DATA_FLAG .EQ. 3 .OR.
     :            DATA_FLAG .EQ. 4) THEN

*              if CHOP then get buffer 2, PHASEB
                CALL DAT_FIND( LOCCELL, 'PHASEB', LOCBUFF, STATUS)
                CALL DAT_FIND( LOCBUFF, 'DATA_ARRAY', LOCDA, STATUS)
                CALL DAT_SHAPE( LOCDA, 2, DIMS, NDIMS, STATUS)

*              test the status to see if the find on the data_array etc
*              was successful, if not then tell user and release this element
                IF( STATUS .NE. SAI__OK) THEN
                  STATUS = SAI__OK
                  CALL MSG_SETI( 'NUMO', K)
                  CALL MSG_SETC( 'CON', CONTNAME( J))
                  CALL MSG_OUT( 'MESSAGE',
     :'IRCAM Container file ^CON, Obs number ^NUMO NOT FOUND ...',
     :             STATUS)
                  CALL DAT_ANNUL( LOCDA, STATUS)
                  CALL DAT_ANNUL( LOCBUFF, STATUS)
                  STATUS = SAI__OK
                  GOTO 200
                END IF

*              map the data array component ...
                CALL DAT_MAPR( LOCDA, 'READ', NDIMS, DIMS, PNTRI,
     :                         STATUS)

*              increment counter for number of images written to tape
                COUNTER = COUNTER + 1

*              call subroutine to calculate max,min in data and BZERO, BSCALE
                CALL FITSWCALB( DIMS( 1), DIMS( 2), %VAL( PNTRI),
     :                          MAXVAL, MINVAL, BZERO, BSCALE)

*              tell user of the max,min and b values calculated
                CALL MSG_SETI( 'NUM', K)
                CALL MSG_SETC( 'CON', CONTNAME( J))
                CALL MSG_OUT( 'MESSAGE',
     :'IRCAM Container file ^CON, Obs number ^NUM, PHASE B data ...',
     :            STATUS)
                CALL MSG_SETR( 'MAX', MAXVAL)
                CALL MSG_SETR( 'MIN', MINVAL)
                CALL MSG_OUT( 'MESSAGE',
     :            'Maximum = ^MAX, Minimum = ^MIN',
     :            STATUS)
                CALL MSG_SETR( 'BZE', BZERO)
                CALL MSG_SETR( 'BSC', BSCALE)
                CALL MSG_OUT( 'MESSAGE',
     :            'Bzero   = ^BZE, Bscale  = ^BSC',
     :            STATUS)

*              form the image name string
                CALL CHR_ITOC( K, KC, KL )
                CALL CHR_CLEAN( CONTNAME( J) )
                LEN = 0
                CALL CHR_APPND( CONTNAME( J), CONTNAME( J), LEN)
                IMAGENAME =
     :            CONTNAME( J)( 1:LEN)//'.OBS('//KC(1:KL)//').PHASEB'

*              write header to the text file
                WRITE( 1, '(1X,I5,2X,A50)') COUNTER, IMAGENAME

*              get lun for output disk fits if requested
                IF( TAPEORDISK( 1:1) .EQ. 'D') THEN
                  CALL CHR_ITOC( STARTNUM, CCOUNTER, L1 )
                  STARTNUM = STARTNUM + 1
                  CALL CHR_CLEAN( CCOUNTER )
                  L1 = 0
                  CALL CHR_APPND( CCOUNTER, CCOUNTER, L1)
                  DISKFITS = 'FITS'//CCOUNTER( 1:L1)//'.FITS'
                  CALL RIO_OPEN(DISKFITS,'WRITE','UNFORMATTED',
     :                 2880, LUNO, STATUS )
                  IF (STATUS .NE. SAI__OK) GO TO 996
                  CALL MSG_SETC( 'FI', DISKFITS )
                  CALL MSG_OUT( 'MESS', 'Disk FITS file = ^FI',
     :                           STATUS)
                END IF

*              call subroutine to write image to tape in fits format
                CALL FITSWSUB( TAPEORDISK, MTCHAN, LUNO,
     :                         DIMS( 1), DIMS( 2), %VAL( PNTRI),
     :                         BZERO, BSCALE, ARRAY, INST,
     :                         PLATESCALE, OBSERVERS, ORIGIN,
     :                         SOFTWARE, HEIGHT, LAT, LONG,
     :                         TELESCOPE, BIAS, CONF,
     :                         MAXVAL, MINVAL, DEC, AIRMASS,
     :                         EQUINOX, EVENMEAN, EVENSTD,
     :                         EXPO, FILTER, GAIN, GATE,
     :                         LOCALTIME, MODE, COADDS,
     :                         OBJECT, ODDMEAN, ODDSTD,
     :                         OFFSETDEC, OFFSETRA, RA,
     :                         READRATE, TEMP, TITLE, IMAGENAME,
     :                         UT, HA, ST, FPX, FPY, FPZ,
     :                         XHEAD_ARCSECPMM, XHEAD_DEC_ZERO,
     :                         XHEAD_RA_ZERO, XHEAD_DEC,
     :                         XHEAD_RA, COMMENTS, SWAPBYTES)

*              release lun for output disk fits file
                IF( TAPEORDISK( 1:1) .EQ. 'D') THEN
                   CALL RIO_CLOSE( LUNO, STATUS )
                END IF

*              annul locators for the phaseA data buffer components
                CALL DAT_ANNUL( LOCDA, STATUS)
                CALL DAT_ANNUL( LOCBUFF, STATUS)
              END IF
!             IF( DATA_FLAG .EQ. 2 .OR.
!     :           DATA_FLAG .EQ. 4) THEN

*              if STARE + KTC ON or CHOP + KTC ON then get buffer 3, KTCA
!               CALL DAT_FIND( LOCCELL, 'KTCA', LOCBUFF, STATUS)
!               CALL DAT_FIND( LOCBUFF, 'DATA_ARRAY', LOCDA, STATUS)
!               CALL DAT_SHAPE( LOCDA, 2, DIMS, NDIMS, STATUS)

*              test the status to see if the find on the data_array etc
*              was successful, if not then tell user and release this element
!               IF( STATUS .NE. SAI__OK) THEN
!                 STATUS = SAI__OK
!                 CALL MSG_SETI( 'NUMO', K)
!                 CALL MSG_SETC( 'CON', CONTNAME( J))
!                 CALL MSG_OUT( 'MESSAGE',
!     :'IRCAM Container file ^CON, Observation number ^NUMO NOT FOUND ',
!     :            STATUS)
!                 CALL DAT_ANNUL( LOCDA, STATUS)
!                 CALL DAT_ANNUL( LOCBUFF, STATUS)
!                 STATUS = SAI__OK
!                 GOTO 200
!               END IF

*              map the data array component ...
!               CALL DAT_MAPR( LOCDA, 'READ', NDIMS, DIMS, PNTRI, STATUS)

*              increment counter for number of images written to tape
!               COUNTER = COUNTER + 1

*              call subroutine to calculate max,min in data and BZERO, BSCALE
!               CALL FITSWCALB( DIMS( 1), DIMS( 2), %VAL( PNTRI), MAXVAL,
!     :                         MINVAL, BZERO, BSCALE)

*              tell user of the max,min and b values calculated
!               CALL MSG_SETI( 'NUM', K)
!               CALL MSG_SETC( 'CON', CONTNAME( J))
!               CALL MSG_OUT( 'MESSAGE',
!     :'IRCAM Container file ^CON, Observation number ^NUM, KTC A data ...',
!     :           STATUS)
!               CALL MSG_SETR( 'MAX', MAXVAL)
!               CALL MSG_SETR( 'MIN', MINVAL)
!               CALL MSG_OUT( 'MESSAGE',
!     :           'Maximum = ^MAX, Minimum = ^MIN',
!     :           STATUS)
!               CALL MSG_SETR( 'BZE', BZERO)
!               CALL MSG_SETR( 'BSC', BSCALE)
!               CALL MSG_OUT( 'MESSAGE',
!     :           'Bzero   = ^BZE, Bscale  = ^BSC',
!     :           STATUS)

*              form the image name string
!               CALL LIB$CVT_DX_DX( %DESCR( K), %DESCR( KC), %REF( KL))
!               CALL STR$TRIM( CONTNAME( J), CONTNAME( J), LEN)
!               IMAGENAME = CONTNAME( J)( 1:LEN)//'.OBS('//KC(1:KL)//').KTCA'

*              write header to the text file
!               WRITE( 1, '(1X,I5,2X,A50)') COUNTER, IMAGENAME

*              get lun for output disk fits if requested
!               IF( TAPEORDISK( 1:1) .EQ. 'D') THEN
!                 CALL LIB$CVT_DX_DX( %DESCR( STARTNUM), %DESCR( CCOUNTER))
!                 STARTNUM = STARTNUM + 1
!                 CALL STR$TRIM( CCOUNTER, CCOUNTER, L1)
!                 DISKFITS = 'FITS'//CCOUNTER( 1:L1)//'.FITS'
!                 CALL LIB$GET_LUN( LUNO)
!                 OPEN( UNIT=LUNO, FILE=DISKFITS, STATUS='UNKNOWN',
!     :                 FORM='UNFORMATTED', RECORDTYPE='FIXED',
!     :                 RECL=2880/4, ERR=996)
!                 CALL MSG_SETC( 'FI', DISKFITS)
!                 CALL MSG_OUT( 'MESS', 'Disk FITS file = ^FI', STATUS)
!               END IF

*              call subroutine to write image to tape in fits format
!               CALL FITSWSUB( TAPEORDISK,
!     :                        MTCHAN,
!     :                        LUNO,
!     :                        DIMS( 1),
!     :                        DIMS( 2),
!     :                        %VAL( PNTRI),
!     :                        BZERO,
!     :                        BSCALE,
!     :                        ARRAY,
!     :                        INST,
!     :                        PLATESCALE,
!     :                        OBSERVERS,
!     :                        ORIGIN,
!     :                        SOFTWARE,
!     :                        HEIGHT,
!     :                        LAT,
!     :                        LONG,
!     :                        TELESCOPE,
!     :                        BIAS,
!     :                        CONF,
!     :                        MAXVAL,
!     :                        MINVAL,
!     :                        DEC,
!     :                        AIRMASS,
!     :                        EQUINOX,
!     :                        EVENMEAN,
!     :                        EVENSTD,
!     :                        EXPO,
!     :                        FILTER,
!     :                        GAIN,
!     :                        GATE,
!     :                        LOCALTIME,
!     :                        MODE,
!     :                        COADDS,
!     :                        OBJECT,
!     :                        ODDMEAN,
!     :                        ODDSTD,
!     :                        OFFSETDEC,
!     :                        OFFSETRA,
!     :                        RA,
!     :                        READRATE,
!     :                        TEMP,
!     :                        TITLE,
!     :                        IMAGENAME,
!     :                        UT,
!     :                        HA,
!     :                        ST,
!     :                        FPX,
!     :                        FPY,
!     :                        FPZ,
!     :                        XHEAD_ARCSECPMM,
!     :                        XHEAD_DEC_ZERO,
!     :                        XHEAD_RA_ZERO,
!     :                        XHEAD_DEC,
!     :                        XHEAD_RA,
!     :                        COMMENTS,
!     :                        SWAPBYTES)

*              release lun for output disk fits file
!               IF( TAPEORDISK( 1:1) .EQ. 'D') THEN
!                 CLOSE( LUNO)
!                 CALL LIB$FREE_LUN( LUNO)
!               END IF

*              annul locators for the phaseA data buffer components
!               CALL DAT_ANNUL( LOCDA, STATUS)
!               CALL DAT_ANNUL( LOCBUFF, STATUS)
!             END IF
!             IF( DATA_FLAG .EQ. 4) THEN

*              if CHOP + KTC ON then get buffer 4, KTCB
!               CALL DAT_FIND( LOCCELL, 'KTCB', LOCBUFF, STATUS)
!               CALL DAT_FIND( LOCBUFF, 'DATA_ARRAY', LOCDA, STATUS)
!               CALL DAT_SHAPE( LOCDA, 2, DIMS, NDIMS, STATUS)

*              test the status to see if the find on the data_array etc
*              was successful, if not then tell user and release this element
!               IF( STATUS .NE. SAI__OK) THEN
!                 STATUS = SAI__OK
!                 CALL MSG_SETI( 'NUMO', K)
!                 CALL MSG_SETC( 'CON', CONTNAME( J))
!                 CALL MSG_OUT( 'MESSAGE',
!     :'IRCAM Container file ^CON, Observation number ^NUMO NOT FOUND ...',
!     :            STATUS)
!                 CALL DAT_ANNUL( LOCDA, STATUS)
!                 CALL DAT_ANNUL( LOCBUFF, STATUS)
!                 STATUS = SAI__OK
!                 GOTO 200
!               END IF

*              map the data array component ...
!               CALL DAT_MAPR( LOCDA, 'READ', NDIMS, DIMS, PNTRI, STATUS)

*              increment counter for number of images written to tape
!               COUNTER = COUNTER + 1

*              call subroutine to calculate max,min in data and BZERO, BSCALE
!               CALL FITSWCALB( DIMS( 1), DIMS( 2), %VAL( PNTRI), MAXVAL,
!     :                         MINVAL, BZERO, BSCALE)

*              tell user of the max,min and b values calculated
!               CALL MSG_SETI( 'NUM', K)
!               CALL MSG_SETC( 'CON', CONTNAME( J))
!               CALL MSG_OUT( 'MESSAGE',
!     :'IRCAM Container file ^CON, Observation number ^NUM, KTC B data ...',
!     :           STATUS)
!               CALL MSG_SETR( 'MAX', MAXVAL)
!               CALL MSG_SETR( 'MIN', MINVAL)
!               CALL MSG_OUT( 'MESSAGE',
!     :           'Maximum = ^MAX, Minimum = ^MIN',
!     :           STATUS)
!               CALL MSG_SETR( 'BZE', BZERO)
!               CALL MSG_SETR( 'BSC', BSCALE)
!               CALL MSG_OUT( 'MESSAGE',
!     :           'Bzero   = ^BZE, Bscale  = ^BSC',
!     :           STATUS)

*              form the image name string
!               CALL LIB$CVT_DX_DX( %DESCR( K), %DESCR( KC), %REF( KL))
!               CALL STR$TRIM( CONTNAME( J), CONTNAME( J), LEN)
!               IMAGENAME = CONTNAME( J)( 1:LEN)//'.OBS('//KC(1:KL)//').KTCB'

*              write header to the text file
!               WRITE( 1, '(1X,I5,2X,A50)') COUNTER, IMAGENAME

*              get lun for output disk fits if requested
!               IF( TAPEORDISK( 1:1) .EQ. 'D') THEN
!                 CALL LIB$CVT_DX_DX( %DESCR( STARTNUM), %DESCR( CCOUNTER))
!                 STARTNUM = STARTNUM + 1
!                 CALL STR$TRIM( CCOUNTER, CCOUNTER, L1)
!                 DISKFITS = 'FITS'//CCOUNTER( 1:L1)//'.FITS'
!                 CALL LIB$GET_LUN( LUNO)
!                 OPEN( UNIT=LUNO, FILE=DISKFITS, STATUS='UNKNOWN',
!     :                 FORM='UNFORMATTED', RECORDTYPE='FIXED',
!     :                 RECL=2880/4, ERR=996)
!                 CALL MSG_SETC( 'FI', DISKFITS)
!                 CALL MSG_OUT( 'MESS', 'Disk FITS file = ^FI', STATUS)
!               END IF

*              call subroutine to write image to tape in fits format
!               CALL FITSWSUB( TAPEORDISK,
!     :                        MTCHAN,
!     :                        LUNO,
!     :                        DIMS( 1),
!     :                        DIMS( 2),
!     :                        %VAL( PNTRI),
!     :                        BZERO,
!     :                        BSCALE,
!     :                        ARRAY,
!     :                        INST,
!     :                        PLATESCALE,
!     :                        OBSERVERS,
!     :                        ORIGIN,
!     :                        SOFTWARE,
!     :                        HEIGHT,
!     :                        LAT,
!     :                        LONG,
!     :                        TELESCOPE,
!     :                        BIAS,
!     :                        CONF,
!     :                        MAXVAL,
!     :                        MINVAL,
!     :                        DEC,
!     :                        AIRMASS,
!     :                        EQUINOX,
!     :                        EVENMEAN,
!     :                        EVENSTD,
!     :                        EXPO,
!     :                        FILTER,
!     :                        GAIN,
!     :                        GATE,
!     :                        LOCALTIME,
!     :                        MODE,
!     :                        COADDS,
!     :                        OBJECT,
!     :                        ODDMEAN,
!     :                        ODDSTD,
!     :                        OFFSETDEC,
!     :                        OFFSETRA,
!     :                        RA,
!     :                        READRATE,
!     :                        TEMP,
!     :                        TITLE,
!     :                        IMAGENAME,
!     :                        UT,
!     :                        HA,
!     :                        ST,
!     :                        FPX,
!     :                        FPY,
!     :                        FPZ,
!     :                        XHEAD_ARCSECPMM,
!     :                        XHEAD_DEC_ZERO,
!     :                        XHEAD_RA_ZERO,
!     :                        XHEAD_DEC,
!     :                        XHEAD_RA,
!     :                        COMMENTS,
!     :                        SWAPBYTES)

*              release lun for output disk fits file
!               IF( TAPEORDISK( 1:1) .EQ. 'D') THEN
!                 CLOSE( LUNO)
!                 CALL LIB$FREE_LUN( LUNO)
!               END IF

*              annul locators for the phaseA data buffer components
!               CALL DAT_ANNUL( LOCDA, STATUS)
!               CALL DAT_ANNUL( LOCBUFF, STATUS)
!             END IF
  200         CONTINUE

*            tidy up the input data structure
              CALL DAT_ANNUL( LOCCELL, STATUS)
              STATUS = SAI__OK
            END DO

*         tidy up the input general structure
            CALL DAT_ANNUL( LOCTEL, STATUS)
            CALL DAT_ANNUL( LOCID, STATUS)
            CALL DAT_ANNUL( LOCINST, STATUS)
            CALL DAT_ANNUL( LOCGEN, STATUS)
  500       CONTINUE
          END DO
        END IF

*      Here to write images from free-format file to tape/disk
        IF( SOURCE( 1:1) .EQ. 'F' .OR. SOURCE( 1:1) .EQ. 'B') THEN
          CALL MSG_OUT( 'BLANK', ' ', STATUS)

*        try to open the file
          CALL FIO_OPEN(FFILE, 'READ', 'NONE', 0, LUN, STATUS)
          IF (STATUS .NE. SAI__OK) GO TO 999

*        dump one image frame at a time
          MORE = .TRUE.
          DO WHILE (MORE)

*          read the image name from the file and set the associated parameter
            READ( LUN, '(A)', END=100, ERR=998) IMAGENAME
            CALL SUBPAR_PUTNAME ( IMCODE, IMAGENAME, STATUS)

*          put name to upper case and trim it to get length
            CALL CHR_UCASE( IMAGENAME )

*          write the naeme of the file to the output file on channel 1
            COUNTER = COUNTER + 1

*          get locator to the input IMAGE type data structure
            CALL DAT_EXIST( 'INPIC', 'READ', LOCI, STATUS )
            IF( STATUS .NE. SAI__OK) THEN
              STATUS = SAI__OK
              COUNTER = COUNTER - 1
              CALL MSG_SETC( 'IMAGE', IMAGENAME)
              CALL MSG_OUT( 'MESSAGE',
     :         'Image ^IMAGE NOT FOUND ...',
     :         STATUS)
              GOTO 600
            END IF

*            get the primitive parameters defining observation for header
              CALL FITSWGET( LOCI, LOCI, LOCI, LOCI, ARRAY, INST,
     :                       PLATESCALE, OBSERVERS, ORIGIN,
     :                       SOFTWARE, HEIGHT, LAT, LONG,
     :                       TELESCOPE, BIAS, CONF, MAXXY,
     :                       MINNY, DEC, AIRMASS, UT, EQUINOX,
     :                       EVENMEAN, EVENSTD,  EXPO, FPX,
     :                       FPY, FPZ, FILTER, GAIN, GATE,
     :                       HA, LOCALTIME, MODE, COADDS, OBJECT,
     :                       ODDMEAN,
     :                       ODDSTD, OFFSETDEC, OFFSETRA, RA,
     :                       READRATE, ST, TEMP, TITLE,
     :                       XHEAD_ARCSECPMM, XHEAD_DEC_ZERO,
     :                       XHEAD_RA_ZERO, XHEAD_DEC, XHEAD_RA,
     :                       COMMENTS, STATUS)

*          map in its DATA_ARRAY component
            NDIMS = 2
            CALL CMP_MAPN( LOCI, 'DATA_ARRAY', '_REAL', 'READ',
     :                     NDIMS, PNTRI, DIMS, STATUS )
            IF( STATUS .NE. SAI__OK) THEN
              STATUS = SAI__OK
              COUNTER = COUNTER - 1
              CALL MSG_SETC( 'IMAGE', IMAGENAME)
              CALL MSG_OUT( 'MESSAGE',
     :         'Image ^IMAGE DATA_ARRAY mapping error ...',
     :         STATUS)
              GOTO 600
            END IF

*          output the dimensions of the image to the user
            CALL MSG_SETC( 'NAME', IMAGENAME)
            CALL MSG_SETI( 'XDIM', DIMS( 1 ) )
            CALL MSG_SETI( 'YDIM', DIMS( 2 ) )
            CALL MSG_SETI( 'COUN', COUNTER)
            CALL MSG_OUT( 'INPUT_DIMS',
     : 'Image ^COUN = ^NAME, ^XDIM by ^YDIM pixels', STATUS )

*          call subroutine to calculate max,min in data and BZERO, BSCALE
            CALL FITSWCALB( DIMS( 1), DIMS( 2), %VAL( PNTRI), MAXVAL,
     :                      MINVAL, BZERO, BSCALE)

*          tell user of the max,min and b values calculated
            CALL MSG_SETR( 'MAX', MAXVAL)
            CALL MSG_SETR( 'MIN', MINVAL)
            CALL MSG_OUT( 'MESSAGE', 'Maximum = ^MAX, Minimum = ^MIN',
     :                     STATUS)
            CALL MSG_SETR( 'BZE', BZERO)
            CALL MSG_SETR( 'BSC', BSCALE)
            CALL MSG_OUT( 'MESSAGE', 'Bzero   = ^BZE, Bscale  = ^BSC',
     :                     STATUS)

*          write the header to the output text file for image written
            WRITE( 1, '(1X,I5,2X,A50)') COUNTER, IMAGENAME

*          get lun for output disk fits if requested
            IF( TAPEORDISK( 1:1) .EQ. 'D') THEN
              CALL CHR_ITOC( STARTNUM, CCOUNTER, L1 )
              STARTNUM = STARTNUM + 1
              CALL CHR_CLEAN( CCOUNTER )
              L1 = 0
              CALL CHR_APPND( CCOUNTER, CCOUNTER, L1)
              DISKFITS = 'FITS'//CCOUNTER( 1:L1)//'.FITS'
              CALL RIO_OPEN(DISKFITS,'WRITE','UNFORMATTED',
     :             2880, LUNO, STATUS )
              IF (STATUS .NE. SAI__OK) GO TO 996
              CALL MSG_SETC( 'FI', DISKFITS )
              CALL MSG_OUT( 'MESS', 'Disk FITS file = ^FI', STATUS)
            END IF

*          call subroutine to write image to tape in fits format
            CALL FITSWSUB( TAPEORDISK, MTCHAN, LUNO, DIMS( 1),
     :                     DIMS( 2), %VAL( PNTRI), BZERO,
     :                     BSCALE, ARRAY, INST, PLATESCALE,
     :                     OBSERVERS, ORIGIN, SOFTWARE,
     :                     HEIGHT, LAT, LONG, TELESCOPE,
     :                     BIAS, CONF, MAXVAL, MINVAL,
     :                     DEC, AIRMASS, EQUINOX, EVENMEAN,
     :                     EVENSTD, EXPO, FILTER, GAIN,
     :                     GATE, LOCALTIME, MODE, COADDS,
     :                     OBJECT, ODDMEAN, ODDSTD,
     :                     OFFSETDEC, OFFSETRA, RA,
     :                     READRATE, TEMP, TITLE, IMAGENAME,
     :                     UT, HA, ST, FPX, FPY,FPZ,
     :                     XHEAD_ARCSECPMM, XHEAD_DEC_ZERO,
     :                     XHEAD_RA_ZERO, XHEAD_DEC,
     :                     XHEAD_RA, COMMENTS, SWAPBYTES)

*          release lun for output disk fits file
            IF( TAPEORDISK( 1:1) .EQ. 'D') THEN
               CALL RIO_CLOSE( LUNO, STATUS )
            END IF

*          tidy up the input data structure
            CALL CMP_UNMAP( LOCI, 'DATA_ARRAY', STATUS )
            CALL DAT_ANNUL( LOCI, STATUS )
            CALL PAR_CANCL( 'INPIC', STATUS)
  600       CONTINUE
          END DO
  100     CONTINUE
        END IF

*      Here to write images from separate Images to tape/disk
        IF( SOURCE( 1:1) .EQ. 'I') THEN
          CALL MSG_OUT( 'BLANK', ' ', STATUS)

*        dump one image frame at a time
          MORE = .TRUE.
          DO WHILE ( MORE)

*          write the name of the file to the output file
            COUNTER = COUNTER + 1

*          get locator to the input IMAGE type data structure
            CALL GETINP( 'INPICI', LOCI, STATUS )
            IF( STATUS .NE. SAI__OK) THEN
              STATUS = SAI__OK
              COUNTER = COUNTER - 1
              CALL SUBPAR_GETNAME ( IMCODE2, IMAGENAME, STATUS)
              CALL MSG_SETC( 'IMAGE', IMAGENAME)
              CALL MSG_OUT( 'MESSAGE',
     :         'Image ^IMAGE NOT FOUND ...',
     :         STATUS)
              GOTO 601
            END IF
            CALL SUBPAR_GETNAME ( IMCODE2, IMAGENAME, STATUS)

*            get the primitive parameters defining observation for header
              CALL FITSWGET( LOCI, LOCI, LOCI, LOCI, ARRAY,
     :                       INST, PLATESCALE, OBSERVERS,
     :                       ORIGIN, SOFTWARE, HEIGHT, LAT,
     :                       LONG, TELESCOPE, BIAS, CONF,
     :                       MAXXY, MINNY, DEC,  AIRMASS,
     :                       UT, EQUINOX, EVENMEAN, EVENSTD,
     :                       EXPO, FPX, FPY, FPZ, FILTER,
     :                       GAIN, GATE, HA, LOCALTIME,
     :                       MODE, COADDS, OBJECT, ODDMEAN,
     :                       ODDSTD, OFFSETDEC, OFFSETRA,
     :                       RA, READRATE, ST, TEMP, TITLE,
     :                       XHEAD_ARCSECPMM, XHEAD_DEC_ZERO,
     :                       XHEAD_RA_ZERO, XHEAD_DEC,
     :                       XHEAD_RA, COMMENTS, STATUS)

*          map in its DATA_ARRAY component
            NDIMS = 2
            CALL CMP_MAPN( LOCI, 'DATA_ARRAY', '_REAL', 'READ',
     :                     NDIMS, PNTRI, DIMS, STATUS )
            IF( STATUS .NE. SAI__OK) THEN
              STATUS = SAI__OK
              COUNTER = COUNTER - 1
              CALL MSG_OUT( 'MESSAGE',
     :         'DATA_ARRAY mapping error ...',
     :         STATUS)
              GOTO 601
            END IF

*          output the dimensions of the image to the user
            CALL MSG_SETC( 'NAME', IMAGENAME)
            CALL MSG_SETI( 'XDIM', DIMS( 1 ) )
            CALL MSG_SETI( 'YDIM', DIMS( 2 ) )
            CALL MSG_SETI( 'COUN', COUNTER)
            CALL MSG_OUT( 'INPUT_DIMS',
     :      'Image ^COUN = ^NAME, ^XDIM by ^YDIM pixels', STATUS )

*          call subroutine to calculate max,min in data and BZERO, BSCALE
            CALL FITSWCALB( DIMS( 1), DIMS( 2), %VAL( PNTRI),
     :                      MAXVAL, MINVAL, BZERO, BSCALE)

*          tell user of the max,min and b values calculated
            CALL MSG_SETR( 'MAX', MAXVAL)
            CALL MSG_SETR( 'MIN', MINVAL)
            CALL MSG_OUT( 'MESSAGE', 'Maximum = ^MAX, Minimum = ^MIN',
     :                     STATUS)
            CALL MSG_SETR( 'BZE', BZERO)
            CALL MSG_SETR( 'BSC', BSCALE)
            CALL MSG_OUT( 'MESSAGE', 'Bzero   = ^BZE, Bscale  = ^BSC',
     :                    STATUS)

*          write the header to the output text file for image written
            WRITE( 1, '(1X,I5,2X,A50)') COUNTER, IMAGENAME

*          get lun for output disk fits if requested
            IF( TAPEORDISK( 1:1) .EQ. 'D') THEN
              CALL CHR_ITOC( STARTNUM, CCOUNTER, L1 )
              STARTNUM = STARTNUM + 1
              CALL CHR_CLEAN( CCOUNTER )
              L1 = 0
              CALL CHR_APPND( CCOUNTER, CCOUNTER, L1)
              DISKFITS = 'FITS'//CCOUNTER( 1:L1)//'.FITS'
              CALL RIO_OPEN(DISKFITS,'WRITE','UNFORMATTED',
     :             2880, LUNO, STATUS )
              IF (STATUS .NE. SAI__OK) GO TO 996
              CALL MSG_SETC( 'FI', DISKFITS)
              CALL MSG_OUT( 'MESS', 'Disk FITS file = ^FI', STATUS)
            END IF

*          call subroutine to write image to tape in fits format
            CALL FITSWSUB( TAPEORDISK, MTCHAN, LUNO, DIMS( 1),
     :                     DIMS( 2), %VAL( PNTRI), BZERO,  BSCALE,
     :                     ARRAY, INST, PLATESCALE, OBSERVERS,
     :                     ORIGIN, SOFTWARE, HEIGHT, LAT,
     :                     LONG, TELESCOPE, BIAS, CONF,
     :                     MAXVAL, MINVAL, DEC,  AIRMASS,
     :                     EQUINOX, EVENMEAN, EVENSTD,
     :                     EXPO, FILTER, GAIN, GATE,
     :                     LOCALTIME, MODE,  COADDS,
     :                     OBJECT, ODDMEAN,  ODDSTD,
     :                     OFFSETDEC, OFFSETRA, RA, READRATE,
     :                     TEMP, TITLE, IMAGENAME,  UT, HA,
     :                     ST, FPX, FPY, FPZ,
     :                     XHEAD_ARCSECPMM, XHEAD_DEC_ZERO,
     :                     XHEAD_RA_ZERO, XHEAD_DEC,
     :                     XHEAD_RA, COMMENTS, SWAPBYTES)

*          release lun for output disk fits file
            IF( TAPEORDISK( 1:1) .EQ. 'D') THEN
               CALL RIO_CLOSE( LUNO, STATUS )
            END IF

*          tidy up the input data structure
            CALL CMP_UNMAP( LOCI, 'DATA_ARRAY', STATUS )
            CALL DAT_ANNUL( LOCI, STATUS )
            CALL PAR_CANCL( 'INPICI', STATUS)

*          ask user if want another image written out in FITS format
  601       CONTINUE
            CALL MSG_OUT( 'BLANK', ' ', STATUS)
            MORE = .FALSE.
            call par_cancl( 'another', status)
            CALL PAR_GET0L( 'ANOTHER', MORE, STATUS)
            CALL MSG_OUT( 'BLANK', ' ', STATUS)
          END DO
        END IF

*      Write the SNAPSHOT container file images to tape ...
        IF( SOURCE( 1:1) .EQ. 'S') THEN

*        loop to scan through all input container files and write them to tape
          DO J = 1, NUMCONT

*          set the parameter with the current container filename
            CALL SUBPAR_PUTNAME ( IMCODE, CONTNAME( J), STATUS)

*          put name to upper case and trim it to get length
            CALL CHR_UCASE( CONTNAME( J) )

*          get locator to the input IMAGE type data structure
            CALL DAT_EXIST( 'INPIC', 'READ', LOCI, STATUS)
            IF( STATUS .NE. SAI__OK) THEN
              STATUS = SAI__OK
              CALL MSG_SETC( 'CONT', CONTNAME( J))
              CALL MSG_OUT( 'MESSAGE',
     :        'ERROR, SNAPSHOT observation file ^CONT NOT FOUND ...',
     :         STATUS)
              GOTO 501
            END IF
            CALL DAT_FIND( LOCI, 'OBS', LOCOBS, STATUS)

*          get size of OBS structure and setup number elements
            CALL DAT_SHAPE( LOCOBS, 2, DIMS, NDIMS, STATUS)
            STARTOBS = MIN( CONTSTART( J), DIMS( 1))
            ENDOBS = MIN( CONTEND( J), DIMS( 1))
            CALL MSG_SETI( 'ST', STARTOBS)
            CALL MSG_SETI( 'EN', ENDOBS)
            CALL MSG_OUT( 'MESS',
     :       'Start observation = ^ST, End observation = ^EN', STATUS)

*          loop for all images from container file specified by user
            DO K = STARTOBS, ENDOBS

*            get the parameters from the current container file observation
*            after getting locator to current obs cell elements
              NDIMS = 1
              DIMS( 1) = K
              CALL DAT_CELL( LOCOBS, NDIMS, DIMS, LOCCELL, STATUS)

*            test the status to see if the find on the primitives in the file
*            was successful, if not then tell user and release this obs element
              IF( STATUS .NE. SAI__OK) THEN
                STATUS = SAI__OK
                CALL MSG_SETI( 'NUMO', K)
                CALL MSG_SETC( 'CON', CONTNAME( J))
                CALL MSG_OUT( 'MESSAGE',
     :'IRCAM Container file ^CON, Obs number ^NUMO NOT FOUND ...',
     :           STATUS)
                GOTO 201
              END IF

*            get the UT from the observation
              CALL FITSWGET2( LOCCELL, UT, STATUS)

*            set number of dimensions to zero for scalar primitives
              NDIMS = 0

*            for any data flag other than 0 get buffer 1, PHASEA
              CALL DAT_FIND( LOCCELL, 'PHASEA', LOCBUFF, STATUS)
              CALL DAT_FIND( LOCBUFF, 'DATA_ARRAY', LOCDA, STATUS)
              CALL DAT_SHAPE( LOCDA, 2, DIMS, NDIMS, STATUS)

*            test the status to see if the find on the data_array etc
*            was successful, if not then tell user and release this element
              IF( STATUS .NE. SAI__OK) THEN
                STATUS = SAI__OK
                CALL MSG_SETI( 'NUMO', K)
                CALL MSG_SETC( 'CON', CONTNAME( J))
                CALL MSG_OUT( 'MESSAGE',
     :'IRCAM SNAPSHOT file ^CON, Obs number ^NUMO NOT FOUND ...',
     :          STATUS)
                CALL DAT_ANNUL( LOCDA, STATUS)
                CALL DAT_ANNUL( LOCBUFF, STATUS)
                STATUS = SAI__OK
                GOTO 201
              END IF

*            map the data array component ...
              CALL DAT_MAPR( LOCDA, 'READ', NDIMS, DIMS, PNTRI, STATUS)

*            increment counter for number of images written to tape
              COUNTER = COUNTER + 1

*            call subroutine to calculate max,min in data and BZERO, BSCALE
              CALL FITSWCALB( DIMS( 1), DIMS( 2), %VAL( PNTRI),
     :                        MAXVAL, MINVAL, BZERO, BSCALE)

*            tell user of the max,min and b values calculated
              CALL MSG_SETI( 'NUM', K)
              CALL MSG_SETC( 'CON', CONTNAME( J))
              CALL MSG_OUT( 'MESSAGE',
     :'IRCAM SNAPSHOT file ^CON, Obs number ^NUM, PHASE A data ...',
     :          STATUS)
              CALL MSG_SETR( 'MAX', MAXVAL)
              CALL MSG_SETR( 'MIN', MINVAL)
              CALL MSG_OUT( 'MESSAGE',
     :          'Maximum = ^MAX, Minimum = ^MIN',
     :          STATUS)
              CALL MSG_SETR( 'BZE', BZERO)
              CALL MSG_SETR( 'BSC', BSCALE)
              CALL MSG_OUT( 'MESSAGE',
     :          'Bzero   = ^BZE, Bscale  = ^BSC',
     :          STATUS)

*            form the image name string
              CALL CHR_ITOC( K, KC, KL )
              CALL CHR_CLEAN( CONTNAME( J))
              LEN = 0
              CALL CHR_APPND( CONTNAME( J), CONTNAME( J), LEN)
              IMAGENAME =
     :            CONTNAME( J)( 1:LEN)//'.OBS('//KC(1:KL)//').PHASEA'

*            write header to the text file
              WRITE( 1, '(1X,I5,2X,A50)') COUNTER, IMAGENAME

*            get lun for output disk fits if requested
              IF( TAPEORDISK( 1:1) .EQ. 'D') THEN
                CALL CHR_ITOC( STARTNUM, CCOUNTER, L1 )
                STARTNUM = STARTNUM + 1
                CALL CHR_CLEAN( CCOUNTER )
                L1 = 0
                CALL CHR_APPND( CCOUNTER, CCOUNTER, L1)
                DISKFITS = 'FITS'//CCOUNTER( 1:L1)//'.FITS'
                CALL RIO_OPEN(DISKFITS,'WRITE','UNFORMATTED',
     :               2880, LUNO, STATUS )
                IF (STATUS .NE. SAI__OK) GO TO 996
                CALL MSG_SETC( 'FI', DISKFITS)
                CALL MSG_OUT( 'MESS', 'Disk FITS file = ^FI',
     :                STATUS)
              END IF

*            call subroutine to write image to tape in fits format
              CALL FITSWSUB( TAPEORDISK, MTCHAN,  LUNO,
     :                       DIMS( 1), DIMS( 2), %VAL( PNTRI),
     :                       BZERO, BSCALE, ARRAY, INST,
     :                       PLATESCALE, OBSERVERS, ORIGIN,
     :                       SOFTWARE, HEIGHT, LAT, LONG,
     :                       TELESCOPE, BIAS, CONF, MAXVAL,
     :                       MINVAL, DEC, AIRMASS, EQUINOX,
     :                       EVENMEAN, EVENSTD, EXPO,
     :                       FILTER, GAIN,  GATE,  LOCALTIME,
     :                       MODE, COADDS, OBJECT, ODDMEAN,
     :                       ODDSTD, OFFSETDEC, OFFSETRA,
     :                       RA, READRATE, TEMP, TITLE,
     :                       IMAGENAME, UT, HA, ST, FPX,
     :                       FPY, FPZ, XHEAD_ARCSECPMM,
     :                       XHEAD_DEC_ZERO, XHEAD_RA_ZERO,
     :                       XHEAD_DEC, XHEAD_RA, COMMENTS,
     :                       SWAPBYTES)

*            release lun for output disk fits file
              IF( TAPEORDISK( 1:1) .EQ. 'D') THEN
                 CALL RIO_CLOSE( LUNO, STATUS )
              END IF

*            annul locators for the phaseA data buffer components
              CALL DAT_ANNUL( LOCDA, STATUS)
              CALL DAT_ANNUL( LOCBUFF, STATUS)
  201         CONTINUE

*            tidy up the input data structure
              CALL DAT_ANNUL( LOCCELL, STATUS)
              STATUS = SAI__OK
            END DO
  501       CONTINUE
          END DO
        END IF

*      Write final tapemark
        IF( TAPEORDISK( 1:1) .EQ. 'T') THEN
          JTAPE = MTWR( MTCHAN, DIMS, 0, 1)
        END IF
        RETURN
  999   CONTINUE
        CALL FIO_CLOSE( LUN, STATUS )
        CALL ERR_OUT( 'MESSAGE', 'Error, cannot find image list file',
     :                STATUS)
        RETURN
  998   CONTINUE
        CALL FIO_CLOSE( LUN, STATUS )
        CALL ERR_REP( 'MESSAGE',
     :                'Error, cannot read from specified file',
     :                STATUS)
        RETURN
  996   CONTINUE
        CALL RIO_CLOSE( LUNO, STATUS)
        CALL ERR_REP( 'MESSAGE',
     :                'Error, cannot write disk fits file',
     :                STATUS)
        END
