	SUBROUTINE CLEAR_SCREEN(  STATUS)

* Description : Routine to clear screen without loss of colour table

* =======================================================================

* Invocation : Invoked by PLT2D only at present

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA - 1stFeb85

* History :
* 25-Jul-1994 Changed error reporting to ERR calls, removed VALUE (SKL@JACH)
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAMDEFNS'
	INCLUDE 'ADAMERRS'

	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

* Status :

	INTEGER STATUS

* Import :

* Import-Export :

* Export :

* External references :

* Global variables

	INCLUDE 'PLT2DCOM'

* Local Constants :

* Local variables :

* Internal References :

* ======================================================================

* check status on entry

	IF( STATUS. NE. ADAM__OK)THEN

          CALL ERR_REP('ERR', 'CLEAR_SCREEN : On entry', STATUS )

	  RETURN

	END IF

* clear workstation

	CALL SGS_CLRZ

	CALL SGS_FLUSH

	END
