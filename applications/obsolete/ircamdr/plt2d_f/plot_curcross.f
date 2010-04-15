	SUBROUTINE PLOT_CURCROSS( STATUS)

* Description : Routine to plot CROSS

* ================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*  27th-Jul-1994 Changed error reporting to use ERR_, removed VALUE,
*                changed IFIX to INT   (SKL@JACH)
* 26th-oct-1994 Changed MAGNIF from INT to REAL (SK@JACH)
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

	INTEGER CROSS_SIZE
	REAL MAGNIF
	INTEGER PEN_NUMBER

	REAL ARCSEC_PIXEL
	REAL CROSS_XCEN
	REAL CROSS_YCEN

	CHARACTER COLOUR_CODE*1

* Internal References :

* ================================================================

* check status on entry

	IF( STATUS. NE. SAI__OK)THEN

	  RETURN

	END IF

* get cross parameters

	CALL PAR_GET0I( 'CROSS_SIZE', CROSS_SIZE, STATUS)

	CALL PAR_GET0R( 'ARCSEC_PIXEL', ARCSEC_PIXEL, STATUS)

	CALL PAR_GET0I( 'CROSS_PEN', PEN_NUMBER, STATUS)
	CALL PAR_GET0C( 'CROSS_COLOUR', COLOUR_CODE, STATUS)

	IF( STATUS. NE. SAI__OK)THEN

          CALL ERR_REP('ERR', 'Error : PLOT_CURCROSS after PAR_GETS',
     :                  STATUS )
	  RETURN

	END IF

* display cursor

	CALL CURSOR_DISPLAY( STATUS)

	IF( STATUS. NE. SAI__OK) THEN

	  RETURN

	END IF

* get cursor real position

	CALL PAR_GET0R( 'X_CUR_REAL', CROSS_XCEN, STATUS)
	CALL PAR_GET0R( 'Y_CUR_REAL', CROSS_YCEN, STATUS)

	IF( STATUS. NE. SAI__OK)THEN

          CALL ERR_REP('ERR',
     :           'Error : PLOT_CURCROSS after PAR_GET cursor position',
     :                  STATUS )
	  RETURN

	END IF

* calculate magnification from start,end values and image size

	MAGNIF =  (IM_XEN - IM_XST) / REAL(NX)

* set colour of CROSS

	CALL SET_COLOUR( PEN_NUMBER, COLOUR_CODE)

* plot CROSS

	CALL SGS_LINE( (( CROSS_XCEN - 0.5) -
     :	                  0.5*CROSS_SIZE*MAGNIF/ARCSEC_PIXEL),
     :	               (  CROSS_YCEN - 0.5),
     :	               (( CROSS_XCEN - 0.5) +
     :	                  0.5*CROSS_SIZE*MAGNIF/ARCSEC_PIXEL),
     :	               (  CROSS_YCEN - 0.5))

	CALL SGS_LINE( (  CROSS_XCEN - 0.5),
     :	               (( CROSS_YCEN - 0.5) -
     :	                  0.5*CROSS_SIZE*MAGNIF/ARCSEC_PIXEL),
     :	               ( CROSS_XCEN - 0.5),
     :	               (( CROSS_YCEN - 0.5) +
     :	                  0.5*CROSS_SIZE*MAGNIF/ARCSEC_PIXEL))

* flush buffer of residual output

	CALL SGS_FLUSH

	RETURN
	END
