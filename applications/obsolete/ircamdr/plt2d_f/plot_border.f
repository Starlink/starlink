	SUBROUTINE PLOT_BORDER( STATUS)

* Description : Routine to plot border around current image

* ================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*  27-Jul-1994 Changed error reporting to use ERR_, removed VALUE (SKL@JACH)
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

	INTEGER BORDER_WIDTH
	INTEGER J
	INTEGER PEN_NUMBER

	REAL XEN
	REAL XST
	REAL YEN
	REAL YST

	CHARACTER COLOUR_CODE*1

* Internal References :

* ================================================================

* check status on entry
	IF( STATUS. NE. SAI__OK)THEN
          CALL ERR_REP('ERR', 'Error : PLOT_BORDER on entry', STATUS )
	  RETURN
	END IF

* get border parameters
	CALL PAR_GET0I( 'BORDER_WIDTH', BORDER_WIDTH, STATUS)
	CALL PAR_GET0I( 'BORDER_PEN', PEN_NUMBER, STATUS)
	CALL PAR_GET0C( 'BORDER_COLOUR', COLOUR_CODE, STATUS)
	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR', 'Error : PLOT_BORDER : after PAR_GETs',
     :                  STATUS )
	  RETURN
	END IF

* set colour of border
	CALL SET_COLOUR( PEN_NUMBER, COLOUR_CODE)

* initialize width positional information
	    XST = IM_XST - 1
	    YST = IM_YST - 1
	    XEN = IM_XEN + 1
	    YEN = IM_YEN + 1

* plot border around image
	DO J = 1, BORDER_WIDTH

* set up border around image not on it
	  IF( J .GT. 1) THEN
	    XST = XST + 1
	    YST = YST + 1
	    XEN = XEN - 1
	    YEN = YEN - 1
	  END IF
	  CALL SGS_BOX( XST, XEN, YST, YEN)
	END DO

* flush buffer of residual output
	CALL SGS_FLUSH

	END
