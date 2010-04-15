*+  WELCOME_POLRAP - write up welcome information got from interface

	SUBROUTINE WELCOME_POLRAP ( STATUS )

* Description :
*
* Invocation :
*
*     CALL WELCOME_POLRAP ( STATUS )
*
* Parameters :
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
*
* Type definitions :

	IMPLICIT  NONE			! no default typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'		! SSE global definitions

* Status :

	INTEGER  STATUS			! global status parameter

* Local Constants :

* Local variables :

	CHARACTER
     :	  INFO*80

*-
*      check status on entry - return if not o.k.

	IF ( STATUS .NE. SAI__OK ) THEN

	   RETURN

	END IF

*      get the welcome info from the user interface

	CALL PAR_GET0C( 'INFO', INFO, STATUS)

*      write the welcome info to the user

	CALL MSG_SETC( 'INF', INFO)
	CALL MSG_OUT( 'MESSAGE', '^INF', STATUS)

	END
