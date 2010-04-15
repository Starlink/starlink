
*+  OBSEXT - Extracts a series of observation DATA_ARRAYs to separate files

	SUBROUTINE OBSEXT ( STATUS )

* Description :
*
*     This routine opens a specified HDS container file containing
*     observations and reads the specified DATA_ARRAY components,
*     creating new single observation HDS files with the DATA_ARRAY
*     values
*
* Invocation :
*
*     CALL OBSEXT ( STATUS )
*
* Parameters :
*
*     CONTAINER = IMAGE( READ )
*	   Input container file, the source of the information
*     STARTOBS  = INTEGER( READ )
*	   The start observation number of the extraction
*     ENDOBS    = INTEGER( READ )
*	   The end observation number of the extraction
*     CHANNAME  = CHARACTER( READ )
*          The name of the DATA_ARRAY channel to be used
*     OUTPIC    = IMAGE( WRITE )
*          The single image output file created
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
*     29-07-1986 : First implementation (REVA::CAA)
*     06-06-1989 : added option to create full output structure (JACH::CAA)
*     29-JUN-1994  Changed MSG_OUT on error to ERR_REP, STR$ and LIB$
*                  to CHR_ (SKL@JACH)
*     08-Sep-1994  Unused variables identified by UNIX compile removed(SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE			! no default typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'		! SSE global definitions
        INCLUDE  'DAT_PAR'              ! Necessary for non_VMS
        INCLUDE  'CHR_ERR'

* Status :

	INTEGER  STATUS			! global status parameter

* Local Constants :

* Local variables :

	INTEGER
     :	  ENDOBS,			! last obs element to be written
     :	  IMCODE,			! image code for subpar calls
     :	  J,				! loop counter for all obs elements
     :	  K,				! loop counter for all obs elements
     :	  LEN1,				! length of trimmed string
     :	  LEN2,				! length of trimmed string
     :	  NCOMP,                        ! number of components
     :	  STARTOBS,			! start obs element in write
     :	  SUBS( 2)			! subscript for the observation

	CHARACTER
     :	  CHANNAME*20,			! name of data_array channel to be used
     :	  FILENAME*80,			! output filename created
     :	  JUNK*20,			! string for start obs number
     :	  PREFIX*20,			! prefix for output filename
     :	  CNAME*80                      ! name of component

	CHARACTER*(DAT__SZLOC)		! locators for :
     :    LOCTOP,			! input container file structure
     :    LOCOBS,			! obs structure
     :    LOCCELL,			! cell observation
     :    LOC2,	 			! another locator
     :    LOCCHAN,			! data channel
     :	  LOCDA,                        ! data array in selected channel
     :	  LOCOUT			! output single image

       LOGICAL
     :	  MORESTUFF
*-
*      check status on entry - return if not o.k.

	IF ( STATUS .NE. SAI__OK ) THEN
	  RETURN
	END IF

*      find the index for the parameter system output image

        CALL SUBPAR_FINDPAR( 'OUTPIC', IMCODE, STATUS )

*      try to get the first input image structure

	CALL GETINP( 'CONTAINER', LOCTOP, STATUS )

!	print *, 'after getinp'
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL ERR_REP( 'ERR', 'error after GETINP ...', STATUS)
	  CALL DAT_ANNUL( LOCTOP, STATUS)
	  RETURN
	END IF

*      find the top level locators for the OBS structure

!	print *, 'before dat_find'
	CALL DAT_FIND( LOCTOP, 'OBS', LOCOBS, STATUS)

	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL ERR_REP( 'ERR', 'error after find OBS ...', STATUS)
	  CALL DAT_ANNUL( LOCOBS, STATUS)
	  CALL DAT_ANNUL( LOCTOP, STATUS)
	  RETURN
	END IF
!	print *, 'after dat_find'

*      get start, end obs element, the channel name to be used and the
*      prefix for the output filename sequence

  10	CALL PAR_GET0I( 'STARTOBS', STARTOBS, STATUS)
	CALL PAR_GET0I( 'ENDOBS', ENDOBS, STATUS)
	CALL PAR_GET0C( 'CHANNAME', CHANNAME, STATUS)
	CALL PAR_GET0C( 'PREFIX', PREFIX, STATUS)

	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL ERR_REP( 'ERR', 'error after PAR_GETs ...', STATUS)
	  CALL DAT_ANNUL( LOCOBS, STATUS)
	  CALL DAT_ANNUL( LOCTOP, STATUS)
	  RETURN
	END IF

*      create first filename to be used

        CALL CHR_CLEAN( PREFIX )
        LEN1 = 0
	CALL CHR_APPND( PREFIX, PREFIX, LEN1)
        CALL CHR_ITOC( STARTOBS-1, JUNK, LEN2 )
	IF( LEN2 .EQ. 1) THEN
	  FILENAME = PREFIX( 1:LEN1) // '000' // JUNK( 1:LEN2)
	ELSE IF( LEN2 .EQ. 2) THEN
	  FILENAME = PREFIX( 1:LEN1) // '00' // JUNK( 1:LEN2)
	ELSE IF( LEN2 .EQ. 3) THEN
	  FILENAME = PREFIX( 1:LEN1) // '0' // JUNK( 1:LEN2)
	ELSE IF( LEN2 .EQ. 4) THEN
	  FILENAME = PREFIX( 1:LEN1) // JUNK( 1:LEN2)
	ELSE
	  FILENAME = PREFIX( 1:LEN1) // '0000'
	END IF

*      loop to get locator to each of the specified obs cells and get the
*      DATA_ARRAY values to be written to the stand alone data file

	DO J = STARTOBS, ENDOBS

*        create the next name in the output sequence

          CALL NEXT_NAME( FILENAME, FILENAME, STATUS )

*        write out to user the name of current image being formed

	  CALL MSG_SETC( 'FNAME', FILENAME)
	  CALL MSG_SETI( 'OBSNUM', J)
	  CALL MSG_OUT( 'MESSAGE',
     :	                'Creating file ^FNAME from observation ^OBSNUM',
     :	                STATUS)

	  IF ( STATUS .NE. SAI__OK ) THEN
	    CALL ERR_REP( 'ERR', 'error after NEXT_NAME ...', STATUS)
	    CALL DAT_ANNUL( LOCOBS, STATUS)
	    CALL DAT_ANNUL( LOCTOP, STATUS)
	    RETURN
	  END IF

*        set the output filename

          CALL SUBPAR_PUTNAME( IMCODE, FILENAME, STATUS )

	  IF ( STATUS .NE. SAI__OK ) THEN
	    CALL ERR_REP( 'ERR', 'error after SUBPAR_PUTNAME ...', STATUS)
	    CALL DAT_ANNUL( LOCOBS, STATUS)
	    CALL DAT_ANNUL( LOCTOP, STATUS)
	    RETURN
	  END IF

*        locator the current obs cell

	  SUBS( 1) = J

	  CALL DAT_CELL( LOCOBS, 1, SUBS, LOCCELL, STATUS)

	  IF ( STATUS .NE. SAI__OK ) THEN
	    CALL ERR_REP( 'ERR', 'error after data CELL ...', STATUS)
            CALL ERR_FLUSH( STATUS )
            CALL ERR_ANNUL( STATUS )
	    CALL DAT_ANNUL( LOCCELL, STATUS)
	    GOTO 100
	  END IF

*        create NEW HDS container file for IRCAM images

	  CALL DAT_CREAT( 'OUTPIC', 'STRUCTURE', 0, 0, STATUS)

*        update disk file

	  CALL DAT_UPDAT( 'OUTPIC', STATUS)

*        associate container created with active HDS locator

	  CALL DAT_ASSOC( 'OUTPIC', 'WRITE', LOCOUT, STATUS)

	  IF ( STATUS .NE. SAI__OK ) THEN
	    CALL ERR_REP( 'ERR', 'error after DAT_CREAT ...', STATUS)
	    CALL DAT_ANNUL( LOCOUT, STATUS)
	    CALL DAT_ANNUL( LOCCELL, STATUS)
	    CALL DAT_ANNUL( LOCOBS, STATUS)
	    CALL DAT_ANNUL( LOCTOP, STATUS)
	    RETURN
	  END IF

*        get number of elements in input structure

	  CALL DAT_NCOMP( LOCCELL, NCOMP, STATUS)

*        loop for each component

	  DO K = 1, NCOMP

*          index into component lists

	    CALL DAT_INDEX( LOCCELL, K, LOC2, STATUS)

*          get name of input component

	    CALL DAT_NAME( LOC2, CNAME, STATUS)

*          recursively copy the element to the new output file

	    CALL DAT_RCOPY( LOC2, LOCOUT, CNAME, STATUS)

	    IF( STATUS .NE. SAI__OK) THEN
	      CALL DAT_ANNUL( LOCOUT, STATUS)
	      CALL DAT_ANNUL( LOCCELL, STATUS)
	      CALL DAT_ANNUL( LOC2, STATUS)
	      CALL DAT_ANNUL( LOCOBS, STATUS)
	      CALL DAT_ANNUL( LOCTOP, STATUS)
	      CALL ERR_REP( 'ERR', 'Error, after DAT_RCOPYs ...', STATUS)
	      RETURN
	    END IF

	  END DO

*        update output structure

	  CALL DAT_UPDAT( 'OUTPIC', STATUS)

*        find and move data array to top level of output structure

	  CALL DAT_FIND( LOCOUT, CHANNAME, LOCCHAN, STATUS)
	  CALL DAT_FIND( LOCCHAN, 'DATA_ARRAY', LOCDA, STATUS)

	  IF( STATUS .NE. SAI__OK) THEN
	    CALL DAT_ANNUL( LOCDA, STATUS)
	    CALL DAT_ANNUL( LOCCHAN, STATUS)
	    CALL DAT_ANNUL( LOCOUT, STATUS)
	    CALL DAT_ANNUL( LOCCELL, STATUS)
	    RETURN
	  END IF

	  CALL DAT_RCOPY( LOCDA, LOCOUT, 'DATA_ARRAY', STATUS)

*        delete old components

	  CALL DAT_ANNUL( LOCDA, STATUS)

	  CALL DAT_ERASE( LOCCHAN, 'DATA_ARRAY', STATUS)

	  CALL DAT_ANNUL( LOCCHAN, STATUS)

	  CALL DAT_ERASE( LOCOUT, CHANNAME, STATUS)

*        annul the channel and cell locators for the next one

	  CALL DAT_ANNUL( LOCOUT, STATUS)
	  CALL DAT_ANNUL( LOCCELL, STATUS)

*        cancel output image parameter for next in sequence

          CALL PAR_CANCL( 'OUTPIC', STATUS )

  100	  CONTINUE

	END DO

	CALL PAR_GET0L( 'MORESTUFF', MORESTUFF, STATUS)

	IF( MORESTUFF) THEN
	  CALL PAR_CANCL( 'MORESTUFF', STATUS)
	  CALL PAR_CANCL( 'STARTOBS', STATUS)
	  CALL PAR_CANCL( 'ENDOBS', STATUS)
	  CALL PAR_CANCL( 'CHANNAME', STATUS)
	  CALL PAR_CANCL( 'PREFIX', STATUS)
	  CALL MSG_OUT( 'BLANK', ' ', STATUS)
	  GOTO 10
	END IF

*      tidy up the input container file

	CALL DAT_ANNUL( LOCOBS, STATUS)
	CALL DAT_ANNUL( LOCTOP, STATUS)

	END
