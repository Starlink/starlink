	SUBROUTINE PLOT_CIRCLE( STATUS)

* Description : Routine to plot CIRCLE

* ================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*  27-Jul-94 Changed error reporting to use ERR_, removed VALUE (SKL@JACH)
* 26-oct-1994 Changed MAGNIF from INT to REAL (SKL@JACH)
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAMDEFNS'
	INCLUDE 'ADAMERRS'
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
	INTEGER CIRCLE_XCEN
	INTEGER CIRCLE_YCEN
	REAL MAGNIF
	INTEGER PEN_NUMBER

	REAL ARCSEC_PIXEL
	REAL XPOS
	REAL YPOS

	CHARACTER COLOUR_CODE*1

* Internal References :

* ================================================================

* check status on entry
	IF( STATUS. NE. ADAM__OK)THEN
	  RETURN
	END IF

* get circle parameters
	CALL PAR_GET0I( 'CIRCLE_DIAM', CIRCLE_DIAM, STATUS)
	CALL PAR_GET0I( 'CIRCLE_XCEN', CIRCLE_XCEN, STATUS)
	CALL PAR_GET0I( 'CIRCLE_YCEN', CIRCLE_YCEN, STATUS)
	CALL PAR_GET0R( 'ARCSEC_PIXEL', ARCSEC_PIXEL, STATUS)
	CALL PAR_GET0I( 'CIRCLE_PEN', PEN_NUMBER, STATUS)
	CALL PAR_GET0C( 'CIRCLE_COLOUR', COLOUR_CODE, STATUS)
	IF( STATUS. NE. ADAM__OK) THEN
          CALL ERR_REP('ERR', 'Error : PLOT_CIRCLE : after PAR_GETs',
     :                  STATUS )
	  RETURN
	END IF

* calculate magnification from start,end values and image size
	MAGNIF = (IM_XEN - IM_XST) / REAL(NX)

* set colour of CIRCLE
	CALL SET_COLOUR( PEN_NUMBER, COLOUR_CODE)

* calculate position info
	XPOS = IM_XST + CIRCLE_XCEN*MAGNIF - 0.5*MAGNIF
	YPOS = IM_YST + CIRCLE_YCEN*MAGNIF - 0.5*MAGNIF

* plot CIRCLE
	CALL SGS_CIRCL( XPOS, YPOS, CIRCLE_DIAM*MAGNIF/2.0/ARCSEC_PIXEL)

* flush buffer of residual output
	CALL SGS_FLUSH

	END
