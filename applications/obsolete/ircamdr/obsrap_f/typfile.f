*+  TYPFILE - types a file to screen

	SUBROUTINE TYPFILE ( STATUS)

* Description :
*
* This routine ...
*
* Invocation : CALL TYPFILE ( STATUS)
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

	INTEGER N, M, LUN

	CHARACTER*80 FNAME, FNAME2, DLINE

	LOGICAL EXISTS, MORE
*-

*      check status on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK) THEN
	   RETURN
	END IF

*      get filename, open it and close with status delete
	CALL PAR_GET0C( 'TYPFILE', FNAME, STATUS)
	CALL CHR_FANDL( FNAME, N, M)
	IF( FNAME( N:N) .EQ. '$') THEN
          CALL AB_TRANSLATE_ENV( FNAME, FNAME2, STATUS)
          FNAME = FNAME2
	END IF
        INQUIRE (FILE=FNAME, EXIST=EXISTS)
	IF ( EXISTS) THEN
	  CALL FIO_OPEN( FNAME, 'READ', 'NONE', 0, LUN, STATUS)
	  MORE = .TRUE.
	  IF (STATUS .NE. SAI__OK) GO TO 100
	  DO WHILE ( MORE)
	    READ( 142, '(A)', END=100) DLINE
	    CALL CHR_FANDL( DLINE, N, M)
	    CALL MSG_SETC( 'MESS', DLINE( N:M))
	    CALL MSG_OUT( 'TXT', '^MESS', STATUS)
	  END DO
  100	  CONTINUE
	  CALL FIO_CLOSE( LUN, STATUS )
	END IF

	END
