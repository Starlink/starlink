*+  DELFILE - deletes a file using open and close with status delete

	SUBROUTINE DELFILE ( STATUS)

* Description :
*
* This routine ...
*
* Invocation : CALL DELFILE ( STATUS)
*
* Parameters :
*
* Method :
*
* Bugs :
*
* None known.
*
* Authors : Colin Aspin ROE ( REVA::CAA)
*
* History :
*
*  01-02-1987 :  First implementation (REVA::CAA)
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE		  ! no implicit typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'	    ! SSE global definitions

* Status :

	INTEGER  STATUS		 ! global status parameter

* Local constants :

* Local variables :

	INTEGER N, M

	CHARACTER*80 FNAME, FNAME2

	LOGICAL EXISTS
*-

*      check status on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK) THEN
	   RETURN
	END IF

*      get filename, open it and close with status delete
	CALL PAR_GET0C( 'DELFILE', FNAME, STATUS)
	CALL CHR_FANDL( FNAME, N, M)
	IF( FNAME( N:N) .EQ. '$') THEN
          CALL AB_TRANSLATE_ENV( FNAME, FNAME2, STATUS)
          FNAME = FNAME2
	END IF
        INQUIRE (FILE=FNAME, EXIST=EXISTS)
	IF ( EXISTS) THEN
	  OPEN( UNIT=142, FILE=FNAME, STATUS='OLD')
	  CLOSE( UNIT=142, STATUS='DELETE')
	ELSE
!	  CALL MSG_SETC( 'FNAM', FNAME)
!	  CALL MSG_OUT( 'MESS', 'File ^FNAM does not exist, sorry',
!     :	    STATUS)
	END IF

	END
