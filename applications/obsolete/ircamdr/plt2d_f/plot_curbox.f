	SUBROUTINE PLOT_CURBOX( STATUS)

* Description : Routine to plot BOX using cursor

* ================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
* 16-APR-86 : CAA : fixed the plotting of the box (it never worked before)
* 27-Jul-94   SKL   Changed error reporting to use ERR_, removed VALUE,
*                   changed IFIX to INT (SKL@JACH)
* 26-Oct-1994 SKL  Changed MAGNIF from INT to REAL
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

	INTEGER BOX_XSIZE
	INTEGER BOX_YSIZE
	REAL MAGNIF
	INTEGER PEN_NUMBER

	REAL ARCSEC_PIXEL
	REAL BOX_XCEN
	REAL BOX_YCEN
	REAL X1
	REAL X2
	REAL Y1
	REAL Y2

	CHARACTER COLOUR_CODE*1, BOX_POSITION*20

* Internal References :

* ================================================================

* check status on entry

	IF( STATUS. NE. SAI__OK)THEN
          CALL ERR_REP('ERR', 'Error : PLOT_CURBOX on entry', STATUS )
	  RETURN
	END IF

* get the box positioning parameter

        CALL PAR_GET0C( 'BOX_POSITION', BOX_POSITION, STATUS)
*        type *, 'box_position = ', box_position
        IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :                  'Error : PLOT_BOX : after PAR_GET BOX POSITION',
     :                  STATUS )
          RETURN
        END IF

* get the size of the box to be plotted

	CALL PAR_GET0I( 'BOX_XSIZE', BOX_XSIZE, STATUS)
	CALL PAR_GET0I( 'BOX_YSIZE', BOX_YSIZE, STATUS)

	IF( STATUS. NE. SAI__OK)THEN
          CALL ERR_REP('ERR',
     :               'Error : PLOT_CURBOX after PAR_GET BOX SIZE',
     :                 STATUS )
	  RETURN
	END IF

* get the number of arcsecs/pixel in the image

	CALL PAR_GET0R( 'ARCSEC_PIXEL', ARCSEC_PIXEL, STATUS)

	IF( STATUS. NE. SAI__OK)THEN
          CALL ERR_REP('ERR',
     :               'Error : PLOT_CURBOX after PAR_GET ARCSEC_PIXEL',
     :                 STATUS )
	  RETURN
	END IF

* get the colour of the box

	CALL PAR_GET0I( 'BOX_PEN', PEN_NUMBER, STATUS)
	CALL PAR_GET0C( 'BOX_COLOUR', COLOUR_CODE, STATUS)

	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :               'Error : PLOT_CURBOX after PAR_GET BOX COLOUR',
     :                 STATUS )
	  RETURN
	END IF

* set colour of box

	CALL SET_COLOUR( PEN_NUMBER, COLOUR_CODE)

* put up cursor

	CALL CURSOR_DISPLAY( STATUS)

	IF( STATUS .NE. SAI__OK) THEN
	  RETURN
	END IF

* get cursor position

	CALL PAR_GET0R( 'X_CUR_REAL', BOX_XCEN, STATUS)
	CALL PAR_GET0R( 'Y_CUR_REAL', BOX_YCEN, STATUS)

* calculate current image magnification factor

	MAGNIF = (IM_XEN - IM_XST) /REAL(NX)

* calculate extremes of BOX and plot it

        IF( BOX_POSITION( 1:1) .EQ. 'B') THEN
	  X1 = BOX_XCEN
	  X2 = BOX_XCEN + BOX_XSIZE*MAGNIF/ARCSEC_PIXEL
	  Y1 = BOX_YCEN
	  Y2 = BOX_YCEN + BOX_YSIZE*MAGNIF/ARCSEC_PIXEL
	ELSE
	  X1 = BOX_XCEN - 0.5*BOX_XSIZE*MAGNIF/ARCSEC_PIXEL
	  X2 = BOX_XCEN + 0.5*BOX_XSIZE*MAGNIF/ARCSEC_PIXEL
	  Y1 = BOX_YCEN - 0.5*BOX_YSIZE*MAGNIF/ARCSEC_PIXEL
	  Y2 = BOX_YCEN + 0.5*BOX_YSIZE*MAGNIF/ARCSEC_PIXEL
        END IF

	CALL SGS_BOX( X1, X2, Y1, Y2)

* flush buffer of residual output

	CALL SGS_FLUSH

	RETURN
	END
