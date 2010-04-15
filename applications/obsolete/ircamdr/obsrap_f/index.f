*+  INDEX - reads selected parameters from a specified series of observation

	SUBROUTINE INDEX ( STATUS )

* Description :
*
*     This routine opens a specified container file and reads selected
*     parameters from the information stored and writes them to the user
*     either in the form of a terminal dump of a disk LIST file
*
* Invocation :
*
*     CALL INDEX ( STATUS )
*
* Parameters :
*
*     CONTAINER = IMAGE( READ )
*	   Input container file, the source of the information
*     OUTPUT    = CHARACTER( READ )
*          The place where output listing go, TERMINAL or FILE
*     STARTOBS  = INTEGER( READ )
*	   The start observation number of the inspection
*     ENDOBS    = INTEGER( READ )
*	   The end observation number of the inspection
*
* Method :
*
* Bugs :
*
*     None known.
*
* Authors :
*
*     Colin Aspin ROE ( REVA::CAA )
*
* History :
*
*     22-07-1986 : First implementation (REVA::CAA)
*     24-11-1986 : added time file creation (HILO::CAA)
*     04-03-1987 : added list filename input (HILO::CAA)
*     23-Jun-1994  Changed LIB$ and STR$ to FIO_, CHR_,
*                  TYPE to ERR_REP     (SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE			! no default typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'		! SSE global definitions
        INCLUDE  'DAT_PAR'              ! Necessary for non-VMS
        INCLUDE  'FIO_PAR'
        INCLUDE  'CHR_ERR'

* Status :

	INTEGER  STATUS			! global status parameter

* Local Constants :

* Local variables :

	INTEGER CHARUP2			! character up to in string form
	INTEGER ENDOBS			! last obs element to be written
	INTEGER EXPTIME			! exposure time of obs element
	INTEGER IMCODE			! code of container file parameter
	INTEGER J			! loop counter for all obs elements
	INTEGER LEN			! length of container filename
	INTEGER LEN1			! length of string variable 1
	INTEGER LEN2			! length of string variable 2
	INTEGER LUN			! lun for write to file
	INTEGER LUN_T			! lun for write to file
	INTEGER NUMCOADDS		! number coadds in an obs element
	INTEGER NUMCOLS			! number columns in array
	INTEGER NUMROWS			! number rows in array
	INTEGER STARTOBS		! start obs element in write
	INTEGER STAT			! status for dat_annuls
	INTEGER STRLEN			! length of string output variable
	INTEGER SUBS( 1)		! subscripts for obs cell find

	REAL PIXELPITCH			! pitch of pixel in array
	REAL PLATESCALE			! plate scale of observations

	CHARACTER*20 ARRAYTYPE		! array type
	CHARACTER*20 CONFIGURATION	! configuration parameter
	CHARACTER*80 CONTNAME		! name of container file specified
	CHARACTER*132 DASHING		! dashed line string
	CHARACTER*20 DATETIME		! date/time string
	CHARACTER*20 FILTER		! filter string
	CHARACTER*80 FNAME		! name of list file created
	CHARACTER*20 INSTRUMENT		! instrument string
	CHARACTER*20 MODE		! mode string
	CHARACTER*80 OBJECTNAME		! object name string
	CHARACTER*80 OBSERVERS		! observers names
	CHARACTER*80 ORIGIN		! origin of observers
	CHARACTER*20 OUTPUT_OPTION	! option to write to terminal or file
	CHARACTER*20 SOFTWARE		! author of software used
	CHARACTER*132 STRING		! string variable for write
	CHARACTER*20 STRVAL1		! string variable in conversions
	CHARACTER*20 STRVAL2		! string variable in conversions
	CHARACTER*20 TELESCOPE		! telescope used in observation
	CHARACTER*20 TIMEFILE		! create a time/obsnumber file
	CHARACTER*80 TIMESTRING		! string for time line

	CHARACTER*(DAT__SZLOC)		! locators for :
     :    LOCTOP,			! input container file structure
     :    LOCGEN,			! general structure
     :	  LOCINST,			! instrument structure
     :    LOCID,			! id structure
     :    LOCOBS,			! obs structure
     :	  LOCTELE,			! telescope structure
     :    LOCCELL			! cell locator

*-
*      check status on entry - return if not o.k.

	IF ( STATUS .NE. SAI__OK ) THEN
	   CALL ERR_REP('ERR', 'Error on INDEX entry', STATUS )
	   RETURN

	END IF

*      get the input container file name in character format

	CALL PAR_GET0C( 'CONTNAME', CONTNAME, STATUS )

*      if no error then continue

	IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP('ERR', 'Error after par_get0c filename',
     :                   STATUS )
	  RETURN

	END IF

*      create the name of the output file for the list

*	CALL CHR_UCASE( CONTNAME )
        CALL CHR_CLEAN( CONTNAME )
        LEN = 0
	CALL CHR_APPND( CONTNAME, CONTNAME, LEN)

	FNAME = CONTNAME( 1:LEN) // '.list'

*      set the container file structure name

	CALL SUBPAR_FINDPAR( 'CONTAINER', IMCODE, STATUS)

	CALL SUBPAR_PUTNAME ( IMCODE, CONTNAME( 1:LEN), STATUS)

*      associate container file

	CALL DAT_ASSOC( 'CONTAINER', 'READ', LOCTOP, STATUS )

*      if no error then continue

	IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP('ERR', 'Error after dat_assoc ...', STATUS )
	  RETURN
	END IF

*      get option to write to terminal or to file and start, end obs element

	CALL PAR_GET0C( 'OUTPUT', OUTPUT_OPTION, STATUS)

	CALL PAR_GET0I( 'STARTOBS', STARTOBS, STATUS)

	CALL PAR_GET0I( 'ENDOBS', ENDOBS, STATUS)

*      get the option to create a time file for batch processing

	CALL PAR_GET0C( 'TIMEFILE', TIMEFILE, STATUS)

*      if no error then continue

	IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP('ERR', 'Error after par calls ...', STATUS )
	  RETURN
	END IF

*      test output type and act on it

	IF( OUTPUT_OPTION .EQ. 'FILE') THEN

*        get a LUN from the system and open output file for writing

          CALL FIO_GUNIT( LUN, STATUS )
	  OPEN( UNIT=LUN, FILE=FNAME, STATUS='UNKNOWN', ERR=999)

	  CALL MSG_SETC( 'FNAME', FNAME)
	  CALL MSG_OUT( 'MESSAGE', 'Output data file ^FNAME opened ...',
     :	                STATUS)

*        test timefile type and act on it

	  IF( TIMEFILE .EQ. 'YES' .OR. TIMEFILE .EQ. 'Y') THEN

*          get a LUN from the system and open output file for writing

            CALL FIO_GUNIT( LUN_T, STATUS )
	    OPEN( UNIT=LUN_T, FILE='IMAGEDIR:TIME.LIS', STATUS='UNKNOWN',
     :            ERR=999)

	  END IF
	END IF

*      find the top level locators for the GENERAL, ID and OBS structures

	CALL DAT_FIND( LOCTOP, 'GENERAL', LOCGEN, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after find GENERAL ...', STATUS )
	  return
	end if

	CALL DAT_FIND( LOCGEN, 'INSTRUMENT', LOCINST, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after find INSTRUMENT ...',
     :                         STATUS )
	  return
	end if

	CALL DAT_FIND( LOCGEN, 'ID', LOCID, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after find ID ...', STATUS )
	  return
	end if

	CALL DAT_FIND( LOCGEN, 'TELESCOPE', LOCTELE, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after find TELESCOPE ...', STATUS )
	  return
	end if

	CALL DAT_FIND( LOCTOP, 'OBS', LOCOBS, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after find OBS ...', STATUS )
	  return
	end if

*      test status to see if components found

	IF( STATUS .NE. SAI__OK) THEN

	  STAT = SAI__OK
	  CALL DAT_ANNUL( LOCTOP, STAT)

          CALL ERR_REP('ERR', 'Ho hum ... bit of a problem guys ...',
     :                 STATUS )

	  RETURN

	END IF

*      read the GENERAL and ID info required

	CALL CMP_GET0C( LOCINST, 'ARRAY_TYPE', ARRAYTYPE, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after ARRAY_TYPE ...', STATUS )
          CALL ERR_FLUSH( STATUS )
          CALL ERR_ANNUL( STATUS )
	  STATUS = SAI__OK
!	  return
	end if

	CALL CMP_GET0C( LOCINST, 'INSTRUMENT', INSTRUMENT, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after INSTRUMENT ...', STATUS )
          CALL ERR_FLUSH( STATUS )
          CALL ERR_ANNUL( STATUS )
	  STATUS = SAI__OK
!	  return
	end if

	CALL CMP_GET0I( LOCINST, 'NUMBER_COLUMNS', NUMCOLS, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after NUMBER_COLUMNS ...', STATUS )
          CALL ERR_FLUSH( STATUS )
          CALL ERR_ANNUL( STATUS )
	  STATUS = SAI__OK
!	  return
	end if

	CALL CMP_GET0I( LOCINST, 'NUMBER_ROWS', NUMROWS, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after NUMBER_ROWS ...', STATUS )
          CALL ERR_FLUSH( STATUS )
          CALL ERR_ANNUL( STATUS )
	  STATUS = SAI__OK
!	  return
	end if

	CALL CMP_GET0C( LOCID, 'OBSERVERS', OBSERVERS, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after OBSERVERS ...', STATUS )
          CALL ERR_FLUSH( STATUS )
          CALL ERR_ANNUL( STATUS )
	  STATUS = SAI__OK
!	  return
	end if

	CALL CMP_GET0C( LOCID, 'ORIGIN', ORIGIN, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after ORIGIN ...', STATUS )
          CALL ERR_FLUSH( STATUS )
          CALL ERR_ANNUL( STATUS )
	  STATUS = SAI__OK
!	  return
	end if

	CALL CMP_GET0R( LOCINST, 'PIXEL_PITCH', PIXELPITCH, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after PIXEL_PITCH ...', STATUS )
          CALL ERR_FLUSH( STATUS )
          CALL ERR_ANNUL( STATUS )
	  STATUS = SAI__OK
!	  return
	end if

	CALL CMP_GET0R( LOCINST, 'PLATE_SCALE', PLATESCALE, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after PLATE_SCALE ...', STATUS )
          CALL ERR_FLUSH( STATUS )
          CALL ERR_ANNUL( STATUS )
	  STATUS = SAI__OK
!	  return
	end if

	CALL CMP_GET0C( LOCID, 'SOFTWARE', SOFTWARE, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after SOFTWARE ...', STATUS )
          CALL ERR_FLUSH( STATUS )
          CALL ERR_ANNUL( STATUS )
	  STATUS = SAI__OK
!	  return
	end if

	CALL CMP_GET0C( LOCTELE, 'TELESCOPE', TELESCOPE, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after TELESCOPE ...', STATUS )
          CALL ERR_FLUSH( STATUS )
          CALL ERR_ANNUL( STATUS )
	  STATUS = SAI__OK
!	  return
	end if

*      test status to see if the values were read OK

	IF( STATUS .NE. SAI__OK) THEN

	  STAT = SAI__OK
	  CALL DAT_ANNUL( LOCOBS, STAT)
	  CALL DAT_ANNUL( LOCTELE, STAT)
	  CALL DAT_ANNUL( LOCID,  STAT)
	  CALL DAT_ANNUL( LOCINST, STAT)
	  CALL DAT_ANNUL( LOCGEN, STAT)
	  CALL DAT_ANNUL( LOCTOP, STAT)

          CALL ERR_REP('ERR',
     :                 'Bombed out after GENERAL info section ...',
     :                 STATUS )

	  RETURN

	END IF

*      write out the information just read

	IF( OUTPUT_OPTION .EQ. 'FILE') THEN

*        write out header information to disk file

	  WRITE( LUN, '(A)') ' '

*	  CALL CHR_UCASE( CONTNAME )
          CALL CHR_CLEAN( CONTNAME )
          LEN1 = 0
	  CALL CHR_APPND( CONTNAME, CONTNAME, LEN1)

	  STRING = 'IRCAM Observation Index  :  Container filename = ' //
     :	           CONTNAME( 1:LEN1)
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  STRING = '-----------------------'
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  WRITE( LUN, '(A)') ' '
	  WRITE( LUN, '(A)') ' '

	  STRING = 'Telescope used = '
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)

	  CALL CHR_UCASE( TELESCOPE )
          CALL CHR_CLEAN( TELESCOPE )
          LEN1 = 0
	  CALL CHR_APPND( TELESCOPE, TELESCOPE, LEN1)

	  STRING = STRING( 1:STRLEN) // ' ' // TELESCOPE( 1:LEN1)
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  WRITE( LUN, '(A)') ' '

*        form the instrument string and write it

	  CALL CHR_UCASE( INSTRUMENT)
          CALL CHR_CLEAN(  INSTRUMENT)
          LEN1 = 0
	  CALL CHR_APPND( INSTRUMENT,INSTRUMENT, LEN1)
	  STRING = 'Instrument used = ' // INSTRUMENT( 1:LEN1)
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  WRITE( LUN, '(A)') ' '

	  STRING = 'IRCAM Software : '
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  CALL CHR_UCASE( SOFTWARE)
          CALL CHR_CLEAN( SOFTWARE)
          LEN1 = 0
	  CALL CHR_APPND( SOFTWARE, SOFTWARE, LEN1)
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  STRING = STRING( 1:STRLEN) // ' ' // SOFTWARE( 1:LEN1)
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  WRITE( LUN, '(A)') ' '

*        dashed line separating sections

	  DO J = 1, 132
	    DASHING( J:J) = '*'
	  END DO

	  WRITE( LUN, '(A)') DASHING( 1:132)

	  WRITE( LUN, '(A)') ' '
	  WRITE( LUN, '(A)') ' '

	  STRING = '1) User/Instrument Information'
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  STRING = '------------------------------'
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  WRITE( LUN, '(A)') ' '

*        form the array type string

	  CALL CHR_UCASE( ARRAYTYPE )
          CALL CHR_CLEAN( ARRAYTYPE )
          LEN1 = 0
	  CALL CHR_APPND( ARRAYTYPE, ARRAYTYPE, LEN1)
	  STRING = 'IR Array = ' // ARRAYTYPE( 1:LEN1)
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)

*        form the array size string and write it

          CALL CHR_ITOC( NUMCOLS, STRVAL1, LEN1 )
          CALL CHR_ITOC( NUMROWS, STRVAL2, LEN2 )

	  STRING = STRING( 1:STRLEN) // '     :     Array size is ' //
     :             STRVAL1( 1:LEN1) // ' by ' // STRVAL2( 1:LEN2) //
     :             ' pixels'
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  WRITE( LUN, '(A)') ' '

*        form the pixel pitch and plate scale strings and write them out

          CALL CHR_RTOC( PIXELPITCH, STRVAL1, LEN1)
          CALL CHR_RTOC( PLATESCALE, STRVAL2, LEN2)

	  STRING = 'Plate Scale = ' // STRVAL2( 1:LEN2) // ' "/pixel' //
     :	           '     :     Pixel Pitch = ' // STRVAL1( 1:LEN1) //
     :             ' um.'
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  WRITE( LUN, '(A)') ' '

*        form the observers string and write it

	  CALL CHR_UCASE( OBSERVERS )
          CALL CHR_CLEAN( OBSERVERS )
          LEN1 = 0
	  CALL CHR_APPND( OBSERVERS, OBSERVERS, LEN1)
	  STRING = 'Observers : ' // OBSERVERS( 1:LEN1)
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)

*        form the origin string and write it

	  CALL CHR_UCASE( ORIGIN )
          CALL CHR_CLEAN( ORIGIN )
          LEN1 = 0
	  CALL CHR_APPND( ORIGIN, ORIGIN, LEN1)
	  STRING = STRING( 1:STRLEN) // ' from ' // ORIGIN( 1:LEN1)
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  WRITE( LUN, '(A)') ' '
	  WRITE( LUN, '(A)') ' '

	  WRITE( LUN, '(A)') DASHING( 1:132)

	  WRITE( LUN, '(A)') ' '
	  WRITE( LUN, '(A)') ' '

*        form the observation range strings and write them

          CALL CHR_ITOC( STARTOBS,  STRVAL1,  LEN1)
          CALL CHR_ITOC( ENDOBS, STRVAL2, LEN2)

	  STRING = '2) Observation Information'
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)
	  STRING = '--------------------------'
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  WRITE( LUN, '(A)') ' '

	  STRING = 'OBSERVATION listing requested from element number ' //
     :	           STRVAL1( 1:LEN1) // ' to ' // STRVAL2( 1:LEN2)
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  WRITE( LUN, '(A)') ' '
	  WRITE( LUN, '(A)') ' '

*      write out header for list of observation values

	  STRING = 'Number    '
	  STRING = STRING( 1:11) // 'Object                '
	  STRING = STRING( 1:31) // 'Date/Time          '
	  STRING = STRING( 1:56) // 'Filter                '
	  STRING = STRING( 1:77) // 'Observation Mode       '
	  STRING = STRING( 1:99) // 'Exp-Time   '
	  STRING = STRING( 1:110) // 'No. Coadds '

          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  STRING = '------    '
	  STRING = STRING( 1:11) // '------                '
	  STRING = STRING( 1:31) // '---------          '
	  STRING = STRING( 1:56) // '------                '
	  STRING = STRING( 1:77) // '----------------       '
	  STRING = STRING( 1:99) // '--------   '
	  STRING = STRING( 1:110) // '---------- '

          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  WRITE( LUN, '(A)') ' '

*      loop to get locator to each of the specified obs cells and get the info
*      to be written to the user

!	  call lib$init_timer

	  DO J = STARTOBS, ENDOBS

	    OBJECTNAME = ' '
	    DATETIME = ' '
	    FILTER = ' '
	    CONFIGURATION = ' '
	    MODE = ' '
	    EXPTIME = 0
	    NUMCOADDS = 0

	    IF( IFIX( J/10.0+0.5)*10 .EQ. J) THEN

	      CALL MSG_SETI( 'NUM', J)
	      CALL MSG_OUT( 'MESSAGE', 'Processing OBSERVATION ^NUM ...',
     :	                    STATUS)

	    END IF

	    STRING = ' '

*          locator the current obs cell

	    SUBS( 1) = J

	    CALL DAT_CELL( LOCOBS, 1, SUBS, LOCCELL, STATUS)

*          get the primitive values from the observation

	    CALL CMP_GET0C( LOCCELL, 'OBJECT_NAME', OBJECTNAME, STATUS)
	    STATUS = SAI__OK

	    CALL CMP_GET0C( LOCCELL, 'LOCAL_DATETIME', DATETIME, STATUS)
	    STATUS = SAI__OK

	    CALL CMP_GET0C( LOCCELL, 'FILTER', FILTER, STATUS)
	    STATUS = SAI__OK

	    CALL CMP_GET0C( LOCCELL, 'CONFIGURATION', CONFIGURATION, STATUS)
	    STATUS = SAI__OK

	    CALL CMP_GET0C( LOCCELL, 'MODE', MODE, STATUS)
	    STATUS = SAI__OK

	    CALL CMP_GET0I( LOCCELL, 'EXPOSURE_TIME', EXPTIME, STATUS)
	    STATUS = SAI__OK

	    CALL CMP_GET0I( LOCCELL, 'NUMBER_COADDS', NUMCOADDS, STATUS)
	    STATUS = SAI__OK

*          trim strings and form the string to be written out and write it

	    CHARUP2 = 1

            CALL CHR_ITOC(  J, STRVAL1, LEN1)

	    STRING = STRVAL1( 1:10)

	    TIMESTRING = STRVAL1( 1:LEN1)

	    CHARUP2 = 11

	    IF( OBJECTNAME( 1:1) .EQ. CHAR(0)) OBJECTNAME = '?'

	    STRING = STRING( 1:CHARUP2) // OBJECTNAME( 1:20) // ' '

	    CHARUP2 = 31

	    IF( DATETIME( 1:1) .EQ. CHAR(0)) DATETIME = '?'

	    STRING = STRING( 1:CHARUP2) // DATETIME( 1:20) // '     '

	    TIMESTRING = TIMESTRING( 1:LEN1) // ' ' // DATETIME( 1:20)

	    CHARUP2 = 56

	    IF( FILTER( 1:1) .EQ. CHAR(0)) FILTER = '?'

	    STRING = STRING( 1:CHARUP2) // FILTER( 1:20) // ' '

	    CHARUP2 = 77

	    IF( CONFIGURATION( 1:1) .EQ. CHAR(0)) CONFIGURATION = '?'

	    STRING = STRING( 1:CHARUP2) // CONFIGURATION( 1:10) // ' '

	    CHARUP2 = 88

	    IF( MODE( 1:1) .EQ. CHAR(0)) MODE = '?'

	    STRING = STRING( 1:CHARUP2) // MODE( 1:10) // ' '

	    CHARUP2 = 99

            CALL CHR_ITOC( EXPTIME, STRVAL1, LEN1)

	    STRING = STRING( 1:CHARUP2) // STRVAL1( 1:10) // ' '

	    CHARUP2 = 110

            CALL CHR_ITOC( NUMCOADDS,  STRVAL1, LEN1 )

	    STRING = STRING( 1:CHARUP2) // STRVAL1( 1:10) // ' '

	    CHARUP2 = 121

            CALL CHR_CLEAN( STRING )
            STRLEN = 0
            CALL CHR_APPND( STRING, STRING, STRLEN)
	    WRITE( LUN, '(A)') STRING( 1:STRLEN)

	    IF( TIMEFILE .EQ. 'YES' .OR. TIMEFILE .EQ. 'Y') THEN

	      WRITE( LUN_T, '(A)') TIMESTRING( 1:60)

	    END IF

*          annul the cell locator for the next one

	    STAT = SAI__OK
	    CALL DAT_ANNUL( LOCCELL, STAT)

	  END DO

!	  call lib$show_timer

	ELSE IF( OUTPUT_OPTION .EQ. 'TERMINAL') THEN

*        write info to the terminal

	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)

	  STRING = 'IRCAM Observation Index'
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  STRING = '-----------------------'
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)
	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)

	  STRING = 'Telescope = '
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
          CALL CHR_CLEAN( TELESCOPE )
          LEN1 = 0
	  CALL CHR_APPND( TELESCOPE, TELESCOPE, LEN1)
	  STRING = STRING( 1:STRLEN) // ' ' // TELESCOPE( 1:LEN1)
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)

	  STRING = 'IRCAM Software '
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
          CALL CHR_CLEAN( SOFTWARE )
          LEN1 = 0
	  CALL CHR_APPND(  SOFTWARE, SOFTWARE, LEN1)
	  STRING = STRING( 1:STRLEN) // ' ' // SOFTWARE( 1:LEN1)
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)

	  DO J = 1, 132
	    DASHING( J:J) = '*'
	  END DO
	  WRITE( LUN, '(A)') DASHING( 1:132)

	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)
	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)

	  STRING = '1) User/Instrument Information'
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)
	  STRING = '------------------------------'
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)

*        form the instrument string and write it

          CALL CHR_CLEAN( INSTRUMENT )
          LEN1 = 0
	  CALL CHR_APPND( INSTRUMENT, INSTRUMENT, LEN1)
	  STRING = 'Instrument = ' // INSTRUMENT( 1:LEN1)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)

*        form the array type string

          CALL CHR_CLEAN( ARRAYTYPE )
          LEN1 = 0
	  CALL CHR_APPND( ARRAYTYPE, ARRAYTYPE, LEN1)
	  STRING = 'IR Array = ' // ARRAYTYPE( 1:LEN1)
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)

*        form the array size string and write it

          CALL CHR_ITOC( NUMCOLS,  STRVAL1, LEN1)
          CALL CHR_ITOC( NUMROWS,  STRVAL2, LEN2)

	  STRING = STRING( 1:STRLEN) // '     :     Array size is ' //
     :             STRVAL1( 1:LEN1) // ' by ' // STRVAL2( 1:LEN2) //
     :             ' pixels'
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)

*        form the pixel pitch and plate scale strings and write them out

          CALL CHR_RTOC( PIXELPITCH, STRVAL1, LEN1)
          CALL CHR_RTOC( PLATESCALE, STRVAL2, LEN2)

	  STRING = 'Plate Scale = ' // STRVAL2( 1:LEN2) // ' "/pixel' //
     :	           '     :     Pixel Pitch = ' // STRVAL1( 1:LEN1) //
     :             ' um.'
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)

*        form the observers string and write it

          CALL CHR_CLEAN( OBSERVERS )
          LEN1 = 0
	  CALL CHR_APPND( OBSERVERS, OBSERVERS, LEN1)
	  STRING = 'Observers : ' // OBSERVERS( 1:LEN1)
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)

*        form the origin string and write it

          CALL CHR_CLEAN( ORIGIN )
          LEN1 = 0
	  CALL CHR_APPND( ORIGIN, ORIGIN, LEN1)
	  STRING = STRING( 1:STRLEN) // ' from ' // ORIGIN( 1:LEN1)
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)
	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)

	  WRITE( LUN, '(A)') DASHING( 1:132)

	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)
	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)

*        form the observation range strings and write them

          CALL CHR_ITOC( STARTOBS, STRVAL1, LEN1)
          CALL CHR_ITOC( ENDOBS, STRVAL2, LEN2)

	  STRING = '2) Observation Information'
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)
	  STRING = '--------------------------'
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)

	  STRING = 'OBSERVATION listing requested from element number ' //
     :	           STRVAL1( 1:LEN1) // ' to ' // STRVAL2( 1:LEN2)
          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)
	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)

*      write out header for list of observation values

	  STRING = 'Number    '
	  STRING = STRING( 1:11) // 'Object                '
	  STRING = STRING( 1:31) // 'Date/Time          '
	  STRING = STRING( 1:56) // 'Filter                '
	  STRING = STRING( 1:77) // 'Observation Mode       '
	  STRING = STRING( 1:99) // 'Exp-Time   '
	  STRING = STRING( 1:110) // 'No. Coadds '

          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  STRING = '------    '
	  STRING = STRING( 1:11) // '------                '
	  STRING = STRING( 1:31) // '---------          '
	  STRING = STRING( 1:56) // '------                '
	  STRING = STRING( 1:77) // '----------------       '
	  STRING = STRING( 1:99) // '--------   '
	  STRING = STRING( 1:110) // '---------- '

          CALL CHR_CLEAN( STRING )
          STRLEN = 0
	  CALL CHR_APPND( STRING, STRING, STRLEN)
	  WRITE( LUN, '(A)') STRING( 1:STRLEN)

	  CALL MSG_OUT ( 'BLANK', ' ', STATUS)

*      loop to get locator to each of the specified obs cells and get the info
*      to be written to the user

!	  call lib$init_timer

	  DO J = STARTOBS, ENDOBS

	    STRING = ' '

*          locator the current obs cell

	    SUBS( 1) = J

	    CALL DAT_CELL( LOCOBS, 1, SUBS, LOCCELL, STATUS)

*          get the primitive values from the observation

	    CALL CMP_GET0C( LOCCELL, 'OBJECT_NAME', OBJECTNAME, STATUS)

	    CALL CMP_GET0C( LOCCELL, 'LOCAL_DATETIME', DATETIME, STATUS)

	    CALL CMP_GET0C( LOCCELL, 'FILTER', FILTER, STATUS)

	    CALL CMP_GET0C( LOCCELL, 'CONFIGURATION', CONFIGURATION, STATUS)

	    CALL CMP_GET0C( LOCCELL, 'MODE', MODE, STATUS)

	    CALL CMP_GET0I( LOCCELL, 'EXPOSURE_TIME', EXPTIME, STATUS)

	    CALL CMP_GET0I( LOCCELL, 'NUMBER_COADDS', NUMCOADDS, STATUS)

*          trim strings and form the string to be written out and write it

	    CHARUP2 = 1

            CALL CHR_ITOC( J, STRVAL1, LEN1 )

	    STRING = STRVAL1( 1:10)

	    CHARUP2 = 11

	    IF( OBJECTNAME( 1:1) .EQ. CHAR(0)) OBJECTNAME = '?'

	    STRING = STRING( 1:CHARUP2) // OBJECTNAME( 1:20) // ' '

	    CHARUP2 = 31

	    IF( DATETIME( 1:1) .EQ. CHAR(0)) DATETIME = '?'

	    STRING = STRING( 1:CHARUP2) // DATETIME( 1:20) // '     '

	    CHARUP2 = 56

	    IF( FILTER( 1:1) .EQ. CHAR(0)) FILTER = '?'

	    STRING = STRING( 1:CHARUP2) // FILTER( 1:20) // ' '

	    CHARUP2 = 77

	    IF( CONFIGURATION( 1:1) .EQ. CHAR(0)) CONFIGURATION = '?'

	    STRING = STRING( 1:CHARUP2) // CONFIGURATION( 1:10) // ' '

	    CHARUP2 = 88

	    IF( MODE( 1:1) .EQ. CHAR(0)) MODE = '?'

	    STRING = STRING( 1:CHARUP2) // MODE( 1:10) // ' '

	    CHARUP2 = 99

            CALL CHR_ITOC( EXPTIME, STRVAL1, LEN1)

	    STRING = STRING( 1:CHARUP2) // STRVAL1( 1:10) // ' '

	    CHARUP2 = 110

            CALL CHR_ITOC( NUMCOADDS, STRVAL1, LEN1)

	    STRING = STRING( 1:CHARUP2) // STRVAL1( 1:10) // ' '

	    CHARUP2 = 121

            CALL CHR_CLEAN( STRING )
            STRLEN = 0
            CALL CHR_APPND( STRING, STRING, STRLEN)
	    WRITE( LUN, '(A)') STRING( 1:STRLEN)

*          annul the cell locator for the next one

	    STAT = SAI__OK
	    CALL DAT_ANNUL( LOCCELL, STAT)

	    STATUS = SAI__OK

	  END DO

!	  call lib$show_timer

	END IF

*      closes output file if one open

  999	IF( OUTPUT_OPTION .EQ. 'FILE') THEN

	  CLOSE( LUN)

          CALL FIO_PUNIT( LUN, STATUS )

	  IF( TIMEFILE .EQ. 'YES' .OR. TIMEFILE .EQ. 'Y') THEN

	    CLOSE( LUN_T)

            CALL FIO_PUNIT( LUN_T, STATUS )

	  END IF
	END IF

*      tidy up the input container file


	STAT = SAI__OK
	CALL DAT_ANNUL( LOCOBS, STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCTELE, STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCID,  STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCGEN, STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCINST, STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCTOP, STAT)

	END
