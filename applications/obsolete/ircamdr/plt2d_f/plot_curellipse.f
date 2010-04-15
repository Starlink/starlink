	SUBROUTINE PLOT_CURELLIPSE( STATUS)

* Description : Routine to plot ELLIPSE

* ================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*   27th-Jul-94 Changed error reporting to use ERR_, removed VALUE,
*               changed IFIX to INT (SKL@JACH)
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

	INTEGER ELLIPSE_MAJOR
	INTEGER MAGNIF
	INTEGER PEN_NUMBER

	REAL ARCSEC_PIXEL
	REAL ELLIPSE_ECC
	REAL ELLIPSE_ANG
	REAL XPOS
	REAL YPOS

	CHARACTER COLOUR_CODE*1

* Internal References :

* ================================================================

* check status on entry

	IF( STATUS. NE. SAI__OK)THEN
	  RETURN
	END IF

* display cursor

	CALL CURSOR_DISPLAY( STATUS)

	IF( STATUS. NE. SAI__OK) THEN
	  RETURN
	END IF

* get centre position for circle

	CALL PAR_GET0R( 'X_CUR_REAL', XPOS, STATUS)
	CALL PAR_GET0R( 'Y_CUR_REAL', YPOS, STATUS)

	IF( STATUS. NE. SAI__OK)THEN
          CALL ERR_REP('ERR',
     :       'Error : PLOT_CURCIRCLE after PAR_GET cursor position',
     :                  STATUS )
	  RETURN
	END IF

* get ELLIPSE parameters

	CALL PAR_GET0I( 'ELLIPSE_MAJOR', ELLIPSE_MAJOR, STATUS)
	CALL PAR_GET0R( 'ELLIPSE_ECC', ELLIPSE_ECC, STATUS)
	IF( ELLIPSE_ECC .LT. 0.0) ELLIPSE_ECC=0.0
	IF( ELLIPSE_ECC .GT. 1.0) ELLIPSE_ECC=1.0
	CALL PAR_GET0R( 'ELLIPSE_ANG', ELLIPSE_ANG, STATUS)

	CALL PAR_GET0R( 'ARCSEC_PIXEL', ARCSEC_PIXEL, STATUS)

	CALL PAR_GET0I( 'ELLIPSE_PEN', PEN_NUMBER, STATUS)
	CALL PAR_GET0C( 'ELLIPSE_COLOUR', COLOUR_CODE, STATUS)

	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :       'Error : PLOT_CURCIRCLE after PAR_GETS',
     :                  STATUS )
	  RETURN
	END IF

* calculate magnification from start,end values and image size

	MAGNIF =  (IM_XEN - IM_XST) / REAL(NX)

* set colour of ELLIPSE

	CALL SET_COLOUR( PEN_NUMBER, COLOUR_CODE)

* plot ELLIPSE

	CALL SGS_ELLIPS( XPOS, YPOS,
     :	                 ELLIPSE_MAJOR*MAGNIF/2.0/ARCSEC_PIXEL,
     :	                 ELLIPSE_ECC, ELLIPSE_ANG)

* flush buffer of residual output

	CALL SGS_FLUSH

	END
