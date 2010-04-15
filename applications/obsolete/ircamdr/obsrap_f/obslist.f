*+  OBSLIST - reads selected parameter from a specified series of observation

	SUBROUTINE OBSLIST ( STATUS )

* Description :
*
*     This routine opens a specified container file and reads selected
*     parameter from the information stored and writes them to the user
*     either in the form of a terminal dump or a disk LIST file
*
* Invocation :
*
*     CALL OBSLIST ( STATUS )
*
* Parameters :
*
*     CONTAINER = IMAGE( READ )
*	   Input container file, the source of the information
*     WHAT      = CHARACTER( READ )
*          What component from file listed
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
*     Colin Aspin ROE ( UKTH::CAA )
*
* History :
*
*     22-07-1986 : First implementation (REVA::CAA)
*     03-12-1987 : created this program from INDEX (UKTH::CAA)
*     29-JUN-1994  Changed type statements to ERR_REP, STR$ and
*                  LIB$ to CHR_, FIO_ (SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE			! no default typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'		! SSE global definitions
        INCLUDE  'DAT_PAR'              ! Necessary for non-VMS
        INCLUDE  'CHR_ERR'
        INCLUDE  'FIO_PAR'

* Status :

	INTEGER  STATUS			! global status parameter

* Local Constants :

* Local variables :

	INTEGER ENDOBS
	INTEGER INTVAR
	INTEGER J			! loop counter for all obs elements
	INTEGER LEN			! length of container filename
	INTEGER LUN			! lun for write to file
	INTEGER STARTOBS		! start obs element in write
	INTEGER STAT			! status for dat_annuls
	INTEGER SUBS( 1)		! subscripts for obs cell find

	REAL REALVAR			! real value from file

	CHARACTER*80 CHARVAR
	CHARACTER*80 CONTNAME		! name of container file specified
	CHARACTER*80 FNAME		! name of list file created
	CHARACTER*20 OUTPUT_OPTION	! option to write to terminal or file
	CHARACTER*20 PTYPE
	CHARACTER*20 WHAT

	CHARACTER*(DAT__SZLOC)		! locators for :
     :    LOCTOP,			! input container file structure
     :    LOCOBS,			! obs structure
     :    LOCCELL,			! cell locator
     :	  LOCPAR                        ! locator for parameter

*-
*      check status on entry - return if not o.k.

	IF ( STATUS .NE. SAI__OK ) THEN

           CALL ERR_REP('ERR', 'Error on OBSLIST open', STATUS )

	   RETURN

	END IF

*      try to get the first input image structure

	CALL GETINP( 'CONTAINER', LOCTOP, STATUS )

*      if no error then continue

	IF ( STATUS .NE. SAI__OK ) THEN

          CALL ERR_REP('ERR', 'Error on getting input', STATUS )

	  RETURN

	END IF

*      pick up the name of the container file from the parameter system

!	CALL SUBPAR_FINDPAR( 'CONTAINER', IMCODE, STATUS)

!	CALL SUBPAR_GETNAME ( IMCODE, CONTNAME, STATUS)
	contname = 'IRCAM_OBSLIST'

*      create the name of the output file for the list

	CALL CHR_UCASE( CONTNAME )
        CALL CHR_CLEAN(  CONTNAME )
        LEN = 0
	CALL CHR_APPND( CONTNAME, CONTNAME, LEN)

	FNAME = CONTNAME( 1:LEN) // '.LIST'

*      get the name of the element from the container file to be read

	CALL PAR_GET0C( 'WHAT', WHAT, STATUS)

*      get option to write to terminal or to file and start, end obs element

	CALL PAR_GET0C( 'OUTPUT', OUTPUT_OPTION, STATUS)

	CALL PAR_GET0I( 'STARTOBS', STARTOBS, STATUS)

	CALL PAR_GET0I( 'ENDOBS', ENDOBS, STATUS)

*      test output type and act on it

	IF( OUTPUT_OPTION .EQ. 'FILE') THEN

*        get a LUN from the system and open output file for writing

	  CALL FIO_GUNIT( LUN, STATUS )

	  OPEN( UNIT=LUN, FILE=FNAME, STATUS='UNKNOWN', ERR=999)

	  CALL MSG_SETC( 'FNAME', FNAME)
	  CALL MSG_OUT( 'MESSAGE', 'Output data file ^FNAME opened ...',
     :	                STATUS)

	END IF

*      find the top level locators for the OBS structure

	CALL DAT_FIND( LOCTOP, 'OBS', LOCOBS, STATUS)

	IF( STATUS .NE. SAI__OK) THEN

	  CALL DAT_ANNUL( LOCTOP, STATUS)

          CALL ERR_REP( 'ERR', 'ERROR, AFTER FIND obs ...', STATUS )

	  IF( OUTPUT_OPTION .EQ. 'FILE') THEN

	    CLOSE( LUN)

	    CALL FIO_PUNIT( LUN, STATUS )

	  END IF

	  RETURN

	END IF

*      loop to get locator to each of the specified obs cells and get the info
*      to be written to the user

	DO J = STARTOBS, ENDOBS

*        locator the current obs cell

	  SUBS( 1) = J

	  CALL DAT_CELL( LOCOBS, 1, SUBS, LOCCELL, STATUS)

	  CALL DAT_FIND( LOCCELL, WHAT, LOCPAR, STATUS)

	  CALL DAT_TYPE( LOCPAR, PTYPE, STATUS)

	  CALL CHR_UCASE( PTYPE )

*        get the primitive values from the observation

	  IF( PTYPE( 1:5) .EQ. '_CHAR') THEN

	    CALL CMP_GET0C( LOCCELL, WHAT, CHARVAR, STATUS)

	    STATUS = SAI__OK

	  ELSE IF( PTYPE( 1:2) .EQ. '_R') THEN

	    CALL CMP_GET0R( LOCCELL, WHAT, REALVAR, STATUS)

	    STATUS = SAI__OK

	  ELSE IF( PTYPE( 1:2) .EQ. '_I') THEN

	    CALL CMP_GET0I( LOCCELL, WHAT, INTVAR, STATUS)

	    STATUS = SAI__OK

	  END IF

*        annul the cell locator for the next one

	  STAT = SAI__OK
	  CALL DAT_ANNUL( LOCPAR, STAT)

	  STAT = SAI__OK
	  CALL DAT_ANNUL( LOCCELL, STAT)

*        write the value to the output source

	  IF( OUTPUT_OPTION .EQ. 'FILE') THEN

	    IF( PTYPE( 1:5) .EQ. '_CHAR') THEN

	      WRITE( LUN, '(I4,5X,A30)') J, CHARVAR

	    ELSE IF( PTYPE( 1:2) .EQ. '_R') THEN

	      WRITE( LUN, *) J, REALVAR

	    ELSE IF( PTYPE( 1:2) .EQ. '_I') THEN

	      WRITE( LUN, *) J, INTVAR

	    END IF

	  ELSE

	    IF( PTYPE( 1:5) .EQ. '_CHAR') THEN

	      CALL MSG_SETI( 'NUM', J)
	      CALL MSG_SETC( 'CVAL', CHARVAR)
	      CALL MSG_OUT( 'MESSAGE', 'Observation ^NUM, Value = ^CVAL',
     :	        STATUS)

	    ELSE IF( PTYPE( 1:2) .EQ. '_R') THEN

	      CALL MSG_SETI( 'NUM', J)
	      CALL MSG_SETR( 'RVAL', REALVAR)
	      CALL MSG_OUT( 'MESSAGE', 'Observation ^NUM, Value = ^RVAL',
     :	        STATUS)

	    ELSE IF( PTYPE( 1:2) .EQ. '_I') THEN

	      CALL MSG_SETI( 'NUM', J)
	      CALL MSG_SETI( 'IVAL', INTVAR)
	      CALL MSG_OUT( 'MESSAGE', 'Observation ^NUM, Value = ^IVAL',
     :	        STATUS)

	    END IF

	  END IF

	END DO

*      closes output file if one open

  999	IF( OUTPUT_OPTION .EQ. 'FILE') THEN

	  CLOSE( LUN)

	  CALL FIO_PUNIT( LUN, STATUS )

	END IF

*      tidy up the input container file

	STAT = SAI__OK
	CALL DAT_ANNUL( LOCOBS, STAT)

	STAT = SAI__OK
	CALL DAT_ANNUL( LOCTOP, STAT)

	END
