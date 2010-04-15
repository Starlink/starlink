      SUBROUTINE ASCIIFILE ( STATUS)

*    Description :
*     Opens an ascii file for writing, write a line to it and closes it


*    Invocation :
*     A-task

*    Parameters :

*    Authors :
*	Colin Aspin UKTH::CAA 27-Sep-1989

*    History :
*       24-JUN-1994 Changed LIB$ to FIO_, STR$ to CHR_ (SKL@JACH)
*    endhistory

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
	INCLUDE 'SAE_PAR'
	INCLUDE 'ADAM_DEFNS'
        INCLUDE 'FIO_PAR'
        INCLUDE 'CHR_ERR'

*    Status :
	INTEGER STATUS

*    Local variables :
	INTEGER LUN, LEN

	CHARACTER*80 FILENAME, ACTION
	CHARACTER*132 STRING

	LOGICAL FILE_OPEN

	SAVE FILE_OPEN
*-

	IF( STATUS .NE. SAI__OK) THEN
	  RETURN
	END IF

*      Get parameter defining action to be taken
	CALL PAR_GET0C( 'ACTION', ACTION, STATUS)

*      Put actionn name to upper case
	CALL CHR_UCASE( ACTION )

*      Look at action and act on it
	IF( ACTION .EQ. 'OPEN') THEN

*        If file already open then close output file and free lun
	  IF( FILE_OPEN) THEN
	    CLOSE( LUN, ERR=997)
	    CALL FIO_PUNIT( LUN, STATUS )
	    FILE_OPEN = .FALSE.
	  END IF

*        Get the parameter with name of ascii file to be opened
	  CALL PAR_GET0C( 'FILENAME', FILENAME, STATUS)

*        Get a LUN for output channel
	  CALL FIO_GUNIT( LUN, STATUS )

*        Open file for writing
	  OPEN( UNIT=LUN, FILE=FILENAME, STATUS = 'UNKNOWN', ERR=999)

*        Set flag to indicate file opened
	  FILE_OPEN = .TRUE.

	ELSE IF( ACTION .EQ. 'WRITE') THEN

*        Get the parameter with the character string to be written
	  CALL PAR_GET0C( 'STRING', STRING, STATUS)

*        Write line to output file
          CALL CHR_CLEAN( STRING )
          LEN = 0
	  CALL CHR_APPND( STRING, STRING, LEN)
	  WRITE( LUN, '(A)', ERR=998) STRING( 1:LEN)

	ELSE IF( ACTION .EQ. 'CLOSE') THEN

*        Close output file and free lun
	  CLOSE( LUN, ERR=997)
	  CALL FIO_PUNIT( LUN, STATUS )

*        Set flag to indicate file closed
	  FILE_OPEN = .FALSE.

	ELSE
	 CALL MSG_OUT( 'MESS', 'Incorrect action try OPEN,WRITE,CLOSE',
     :	               STATUS)
	END IF

*      Jump to end if all was ok
	GOTO 888

*      Here if an error has occurred
  999   CALL MSG_SETC( 'FIL', FILENAME)
	CALL ERR_REP( 'ERR', 'Error, cannot open file ^FIL ...', STATUS)
	CLOSE( LUN)
	CALL FIO_PUNIT( LUN, STATUS )
	GOTO 888

  998   CALL MSG_SETC( 'FIL', FILENAME)
	CALL ERR_REP( 'ERR', 'Error, cannot write to file ^FIL', STATUS)
	CLOSE( LUN)
	CALL FIO_PUNIT( LUN, STATUS )
	GOTO 888

  997   CALL MSG_SETC( 'FIL', FILENAME)
	CALL ERR_REP( 'ERR', 'Error, cannot close file ^FIL', STATUS)
	CLOSE( LUN)
	CALL FIO_PUNIT( LUN, STATUS )
	GOTO 888

  888	END
