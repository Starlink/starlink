	SUBROUTINE SET_PENCOL2( STATUS)

* Description : Routine to change colour of one pen of colour table

* =====================================================================

* Authors : C.Aspin (UOE)
* History
*   22-Jul-1994 Changed STR$ calls to CHR_ (SKL@JACH)
*   26-JUL-1994 Changed error reporting to use ERR_, removed VALUE (SKL@JACH)
*
* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'
        INCLUDE 'SAE_PAR'
	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'
        INCLUDE 'CHR_ERR'

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

	INTEGER LEN1
	INTEGER PEN_NUMBER

	CHARACTER*20 PEN_COLOUR

* Internal References :

* =====================================================================

* test if status ok
	IF( STATUS .NE. SAI__OK) THEN
	  RETURN
	END IF

* get pen variables from parameter system
	CALL PAR_GET0I( 'PEN_NUMBER', PEN_NUMBER, STATUS)
	CALL PAR_GET0C( 'PEN_COLOUR', PEN_COLOUR, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP( 'ERR', 'Error : SET_PENCOL : after PAR_GETS',
     :                  STATUS )
	  RETURN
	END IF

* set pen colour code to upper case
	CALL CHR_UCASE(  PEN_COLOUR )
        CALL CHR_CLEAN(  PEN_COLOUR )
        LEN1 = 0
	CALL CHR_APPND( PEN_COLOUR, PEN_COLOUR, LEN1)

* call subroutine to set colour
	CALL SET_COLOUR2( PEN_NUMBER, PEN_COLOUR( 1:LEN1))
	CALL SGS_FLUSH

	END
