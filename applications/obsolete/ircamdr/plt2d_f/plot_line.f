	SUBROUTINE PLOT_LINE( STATUS)

* Description : Routine to PLOT a line on workstation

* =====================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*   27-JUL-1994 Changed error reporting to use ERR_, removed VALUE
*               changed IFIX to INT (SKL@JACH)
* 26-Oct-1994 Changed MAGNIF from INT to REAL (SKL@JACH)
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'
        INCLUDE 'SAE_PAR'
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

	REAL MAGNIF
	INTEGER PEN_NUMBER
	INTEGER XLS
	INTEGER XLE
	INTEGER YLS
	INTEGER YLE

	REAL X1
	REAL X2
	REAL Y1
	REAL Y2

	CHARACTER COLOUR_CODE*1

* Internal References :

* =====================================================================

* check status on entry

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR', 'Error : PLOT_LINE : On entry', STATUS )
	  RETURN
	END IF

* get colour of LINE and start,end points from parameter aystem

	CALL PAR_GET0I( 'LINE_PEN', PEN_NUMBER, STATUS)
	CALL PAR_GET0C( 'LINE_COLOUR', COLOUR_CODE, STATUS)

	CALL PAR_GET0I( 'LINE_XST', XLS, STATUS)
	CALL PAR_GET0I( 'LINE_YST', YLS, STATUS)
	CALL PAR_GET0I( 'LINE_XEN', XLE, STATUS)
	CALL PAR_GET0I( 'LINE_YEN', YLE, STATUS)

	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR', 'Error : PLOT_LINE : after PAR_GETS',
     :                  STATUS )
	  RETURN
	END IF

* calculate magnification factor for current image

	MAGNIF = ( IM_XEN - IM_XST) / REAL(NX)

* set colour of LINE

	CALL SET_COLOUR( PEN_NUMBER, COLOUR_CODE)

* calculate coordinates of line start and end

	X1 = IM_XST + XLS*MAGNIF - 0.5*MAGNIF
	Y1 = IM_YST + YLS*MAGNIF - 0.5*MAGNIF
	X2 = IM_XST + XLE*MAGNIF - 0.5*MAGNIF
	Y2 = IM_YST + YLE*MAGNIF - 0.5*MAGNIF

* draw line

	CALL SGS_LINE( X1, Y1, X2, Y2)

	CALL SGS_FLUSH

	END
