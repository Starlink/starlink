*+  COPFILE - copies an ascii file to another named file

	SUBROUTINE COPFILE ( STATUS)

* Description :
*
* This routine ...
*
* Invocation : CALL COPFILE ( STATUS)
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

	INTEGER N1, M1, N2, M2, LUN1, LUN2

	CHARACTER*80 FNAME1, FNAME2
	CHARACTER*132 DLINE

	LOGICAL EXISTS1, MORE
*-

*      check status on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK) THEN
	   RETURN
	END IF

*      get filename, open it and close with status delete
	CALL PAR_GET0C( 'COPFILEI', FNAME1, STATUS)
	CALL CHR_FANDL( FNAME1, N1, M1)
	IF( FNAME1( N1:N1) .EQ. '$') THEN
          CALL AB_TRANSLATE_ENV( FNAME1, DLINE, STATUS)
          FNAME1 = DLINE
	END IF
        INQUIRE (FILE=FNAME1, EXIST=EXISTS1)
	IF ( EXISTS1) THEN
	  CALL PAR_GET0C( 'COPFILEO', FNAME2, STATUS)
	  CALL CHR_FANDL( FNAME2, N2, M2)
	  IF( FNAME2( N2:N2) .EQ. '$') THEN
            CALL AB_TRANSLATE_ENV( FNAME2, DLINE, STATUS)
            FNAME2 = DLINE
	  END IF
	  CALL FIO_OPEN( FNAME1, 'READ', 'NONE', 0, LUN1, STATUS)
	  CALL FIO_OPEN( FNAME1, 'WRITE', 'NONE', 0, LUN2, STATUS)
	  IF (STATUS .NE. SAI__OK) GO TO 100
	  MORE = .TRUE.
	  DO WHILE ( MORE)
	    READ( LUN1, '(A)', END=100) DLINE
	    CALL CHR_FANDL( DLINE, N1, M1)
	    WRITE( LUN2, '(A)') DLINE( 1:M1)
	  END DO
  100	  CONTINUE
	  CALL FIO_CLOSE( LUN1, STATUS )
	  CALL FIO_CLOSE( LUN2, STATUS )
	ELSE
	  CALL MSG_SETC( 'F', FNAME1)
	  CALL MSG_OUT( 'MESS', 'File ^F does not exist', STATUS)
	END IF

	END
