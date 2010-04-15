	SUBROUTINE PLOT_CURCIRCLE( STATUS)

* Description : Routine to plot CIRCLE

* ================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*  27TH-JUL-94 Changed error reporting to use ERR_, removed VALUE,
*              changed IFIX to INT (SKL@JACH)
* 26th-Oct-1994 Changed MAGNIF from INT to REAL (SKL@JACH)
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

	INTEGER CIRCLE_DIAM
	REAL MAGNIF
	INTEGER PEN_NUMBER

	REAL ARCSEC_PIXEL
	REAL CIRCLE_XCEN
	REAL CIRCLE_YCEN

	CHARACTER COLOUR_CODE*1

* Internal References :

* ================================================================

* check status on entry

	IF( STATUS. NE. SAI__OK)THEN
          CALL ERR_REP('ERR', 'Error : PLOT_CURCIRCLE on entry',
     :                  STATUS )
	  RETURN
	END IF

* get circle parameters

	CALL PAR_GET0I( 'CIRCLE_DIAM', CIRCLE_DIAM, STATUS)

	CALL PAR_GET0R( 'ARCSEC_PIXEL', ARCSEC_PIXEL, STATUS)

	CALL PAR_GET0I( 'CIRCLE_PEN', PEN_NUMBER, STATUS)
	CALL PAR_GET0C( 'CIRCLE_COLOUR', COLOUR_CODE, STATUS)

	IF( STATUS. NE. SAI__OK)THEN
          CALL ERR_REP('ERR', 'Error : PLOT_CURCIRCLE after PAR_GETS',
     :                  STATUS )
	  RETURN
	END IF

* display cursor

	CALL CURSOR_DISPLAY( STATUS)

	IF( STATUS. NE. SAI__OK) THEN
	  RETURN
	END IF

* get centre position for circle

	CALL PAR_GET0R( 'X_CUR_REAL', CIRCLE_XCEN, STATUS)
	CALL PAR_GET0R( 'Y_CUR_REAL', CIRCLE_YCEN, STATUS)

	IF( STATUS. NE. SAI__OK)THEN
          CALL ERR_REP('ERR',
     :           'Error : PLOT_CURCIRCLE after PAR_GET cursor position',
     :                  STATUS )
	  RETURN
	END IF

* calculate magnification from start,end values and image size

	MAGNIF = (IM_XEN - IM_XST) / REAL(NX)

* set colour of CIRCLE

	CALL SET_COLOUR( PEN_NUMBER, COLOUR_CODE)

* plot CIRCLE

	CALL SGS_CIRCL( CIRCLE_XCEN, CIRCLE_YCEN,
     :	                0.5*CIRCLE_DIAM*MAGNIF/ARCSEC_PIXEL)

* flush buffer of residual output

	CALL SGS_FLUSH

	END
