*+  LOOKER - read all entries from the ADAM log file since a particular
*            date/time

	SUBROUTINE LOOKER( STATUS)

* Description : TASK to read records from the log file and send them to the
*               user interface.

* Invocation : CALL LOOKER ( STATUS )

* Parameters : STATUS=INTEGER

* Method : Loops, reading from the log file all records since time specified

* Deficiencies : <description of any deficiencies>

* Bugs : <description of any "bugs" which have not been fixed>

* Authors : B.D.Kelly (REVAD::BDK)
*           C.Aspin (REVA::CAA)

* History :
*   24.02.1986:  Original (REVAD::BDK)
*   03.03.1986:  Modified READER to get all since time T (REVA::CAA)
*   11.08.2004:  Use CHR_APPND (TIMJ@JACH)
* endhistory

* Type Definitions :
	IMPLICIT NONE

* Global constants :
	INCLUDE 'SAE_PAR'

* Status :
	INTEGER STATUS

* Local variables :

	INTEGER LENGTH		   ! string length after trimming
	INTEGER POS		   ! position in output string

	CHARACTER*16  NAME	   ! name in record
	CHARACTER*123 OUTPUT	   ! string sent to userface
	CHARACTER*23  SINCE	   ! want record later in time than this
	CHARACTER*116 STRING	   ! message in record
	CHARACTER*23  TIME	   ! time stamp in read record

	LOGICAL FINISHED           ! controller for reading loop
	LOGICAL WANT_NAME    	   ! option to miss out name
	LOGICAL WANT_STRING    	   ! option to miss out string
	LOGICAL WANT_TIME    	   ! option to miss out time

	INTEGER CHR_LEN
	EXTERNAL CHR_LEN
*-
*
* Check status on entry
*
	IF ( STATUS .NE. SAI__OK ) RETURN
*
* Initialise logging system
*
	CALL LOG_INIT ( 'LOOKER', STATUS )
*
* Initialize looping variable
*
	FINISHED = .FALSE.
*
* Get time for read log
*
	CALL PAR_GET0C( 'TIME', SINCE, STATUS)
*
* Get what user wants in output string
*
	CALL PAR_GET0L( 'WANT_TIME', WANT_TIME, STATUS)
	CALL PAR_GET0L( 'WANT_NAME', WANT_NAME, STATUS)
	CALL PAR_GET0L( 'WANT_STRING', WANT_STRING, STATUS)
*
* Loop trying to pick up new records from the log file
*
	DO WHILE ( .NOT. FINISHED )
*
* read log entry
*
	  CALL LOG_READ( SINCE, NAME, TIME, STRING, STATUS )
*
* test if read went OK
*
	  IF ( STATUS .EQ. SAI__OK ) THEN
*
* initialize position variable
*
	    POS = 0
*
* test if want time in output string
*
	    IF( WANT_TIME ) THEN
*
* if read Ok then trim string TIME and put into output buffer
*
	       CALL CHR_APPND( TIME, OUTPUT, POS)
*
* add space after time
*
	       CALL CHR_APPND(' ',OUTPUT, POS)
	    END IF
*
* test if user wants name in output string
*
	    IF( WANT_NAME ) THEN
*
* trim string NAME and put into output buffer
*
	       CALL CHR_APPND(NAME, OUTPUT, POS)
*
* add space after name
*
	       CALL CHR_APPND(' ',OUTPUT, POS)
	    END IF
*
* test if user wants string in output string
*
	    IF( WANT_STRING ) THEN
*
* trim string STRING and put into output buffer
*
	       CALL CHR_APPND(STRING, OUTPUT, POS)

	    END IF
*
* write the output buffer to the users terminal
*
	    IF( WANT_TIME .OR. WANT_STRING .OR. WANT_TIME ) THEN
	      CALL MSG_OUT ( ' ', OUTPUT, STATUS )
	    END IF
*
* increment time since looking at log started
*
	    SINCE = TIME
	  ELSE
*
* if read was not OK then finish tidily
*
	    STATUS = SAI__OK
	    FINISHED = .TRUE.
	  END IF
	END DO

	END
