	SUBROUTINE INT_TO_STR( INT_VALUE, STRING)

* Description : To convert an integer to a string using LIB$CVT_DX_DX
* system subroutine.

* ============================================================================

* Invocation : Invoked by anything...

* Use : Calls LIB$CVT_DX_DX system subroutine

* Parameters : none

* Authors : C.Aspin (UOE) 6thMar85

* History :
*   24-JUL-1994 Changed LIB$ to CHR_ (SKL@JACH)
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

        INCLUDE 'SAE_PAR'
        INCLUDE 'CHR_ERR'

* Import :

	INTEGER STATUS

* Import-Export :

* Export :

* External references :

* Local Constants :
* <local constants defined by PARAMETER>

* Local variables :

	INTEGER INT_VALUE
        INTEGER NCHAR

	CHARACTER*80 STRING

* Internal References :
* <declarations for internal functions>

* Local data :

* ============================================================================

* initialize status to OK status

	STATUS = SAI__OK

* convert INT_VALUE integer value to character variable STRING

        CALL CHR_ITOC(INT_VALUE, STRING, NCHAR )

	RETURN
	END
