	SUBROUTINE PLOT_CROSS( STATUS)

* Description : Routine to plot CROSS

* ================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
* 27-JUL-94 Changed error reporting to use ERR_, removed VALUE,
*           changed IFIX to INT         (SKL@JACH)
* 26-Oct-1994 Changed MAGNIF from INT to REAL (SKL@JACH)
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

	INTEGER CROSS_SIZE
	INTEGER CROSS_XCEN
	INTEGER CROSS_YCEN
	REAL MAGNIF
	INTEGER PEN_NUMBER

	REAL ARCSEC_PIXEL
	REAL X1
	REAL X2
	REAL Y1
	REAL Y2

	CHARACTER COLOUR_CODE*1

* Internal References :

* ================================================================

* check status on entry

	IF( STATUS. NE. ADAM__OK)THEN

	  RETURN

	END IF

* get the position and size of cross and scale, cross colour

	CALL PAR_GET0I( 'CROSS_SIZE', CROSS_SIZE, STATUS)
	CALL PAR_GET0I( 'CROSS_XCEN', CROSS_XCEN, STATUS)
	CALL PAR_GET0I( 'CROSS_YCEN', CROSS_YCEN, STATUS)
	CALL PAR_GET0R( 'ARCSEC_PIXEL', ARCSEC_PIXEL, STATUS)
	CALL PAR_GET0C( 'CROSS_COLOUR', COLOUR_CODE, STATUS)
	CALL PAR_GET0I( 'CROSS_PEN', PEN_NUMBER, STATUS)

	IF( STATUS. NE. ADAM__OK) THEN

          CALL ERR_REP('ERR', 'Error : PLOT_CROSS : after PAR_GETs',
     :                  STATUS )
	  RETURN

	END IF

* calculate magnification from start,end values and image size

	MAGNIF = (IM_XEN - IM_XST) / REAL(NX)

* set colour of CROSS

	CALL SET_COLOUR( PEN_NUMBER, COLOUR_CODE)

* plot CROSS

	X1 = IM_XST + ( CROSS_XCEN - 0.5)*MAGNIF -
     :	                0.5*CROSS_SIZE*MAGNIF/ARCSEC_PIXEL
	X2 = IM_XST + ( CROSS_XCEN - 0.5)*MAGNIF +
     :	                0.5*CROSS_SIZE*MAGNIF/ARCSEC_PIXEL
	Y1 = IM_YST + ( CROSS_YCEN - 0.5)*MAGNIF
	Y2 = IM_YST + ( CROSS_YCEN - 0.5)*MAGNIF

	CALL SGS_LINE( X1, Y1, X2, Y2)

	X1 = IM_XST + ( CROSS_XCEN - 0.5)*MAGNIF
	X2 = IM_XST + ( CROSS_XCEN - 0.5)*MAGNIF
	Y1 = IM_YST + ( CROSS_YCEN - 0.5)*MAGNIF -
     :	                0.5*CROSS_SIZE*MAGNIF/ARCSEC_PIXEL
	Y2 = IM_YST + ( CROSS_YCEN - 0.5)*MAGNIF +
     :	                0.5*CROSS_SIZE*MAGNIF/ARCSEC_PIXEL

	CALL SGS_LINE( X1, Y1, X2, Y2)

* flush buffer of residual output

	CALL SGS_FLUSH

	END
