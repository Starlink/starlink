*+  LOWCASE - converts string to lower case

	SUBROUTINE LOWCASE ( STATUS)

* Description :
*
* This routine ...
*
* Invocation : CALL LOWCASE ( STATUS)
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

	CHARACTER*132 STRING1
*-

*      check status on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK) THEN
	   RETURN
	END IF

*      get string and convert to lower case and put into parameter system
	CALL PAR_GET0C( 'STRING', STRING1, STATUS)
	CALL CHR_FANDL( STRING1, N, M)
	CALL CHR_LCASE( STRING1)
	CALL PAR_PUT0C( 'STRING', STRING1( N:M), STATUS)

	END
