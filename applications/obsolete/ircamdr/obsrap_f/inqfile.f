*+  INWFILE - inquires whether a file exists

	SUBROUTINE INQFILE ( STATUS)

* Description :
*
* This routine ...
*
* Invocation : CALL INQFILE ( STATUS)
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
	CALL PAR_GET0C( 'INQFILE', FNAME, STATUS)
	CALL CHR_FANDL( FNAME, N, M)
	IF( FNAME( N:N) .EQ. '$') THEN
          CALL AB_TRANSLATE_ENV( FNAME, FNAME2, STATUS)
          FNAME = FNAME2
	END IF
        INQUIRE (FILE=FNAME, EXIST=EXISTS)
	IF ( EXISTS) THEN
	  CALL PAR_PUT0L( 'FILE_EXISTS', .TRUE., STATUS)
	ELSE
	  CALL PAR_PUT0L( 'FILE_EXISTS', .FALSE., STATUS)
	END IF

	END
