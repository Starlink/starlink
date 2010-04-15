	SUBROUTINE PLOT_BOX( STATUS)

* Description : Routine to plot BOX

* ================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
* 17-APR-86 : CAA : made this work, it never worked at all ...
* 12-MAY-86 : CAA : added the option to centre box at BL
* 27-Jul-94   SKL changed error reporting touse ERR_, removed VALUE
* 26-Oct-94   SKL changed MAGNIF from INT to REAL
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

	INTEGER BOX_XPOS
	INTEGER BOX_XSIZE
	INTEGER BOX_YPOS
	INTEGER BOX_YSIZE
	REAL MAGNIF
	INTEGER PEN_NUMBER

	REAL ARCSEC_PIXEL
	REAL RBOX_XPOS
	REAL RBOX_XSIZE
	REAL RBOX_YPOS
	REAL RBOX_YSIZE
	REAL X1
	REAL X2
	REAL Y1
	REAL Y2

	CHARACTER BOX_POSITION*20
	CHARACTER COLOUR_CODE*1

* Internal References :

* ================================================================

* check status on entry

	IF( STATUS. NE. SAI__OK)THEN
          CALL ERR_REP('ERR', 'Error : PLOT_BOX on entry', STATUS )
	  RETURN
	END IF

* get the box size from the parameter system

	CALL PAR_GET0I( 'BOX_XSIZE', BOX_XSIZE, STATUS)
	CALL PAR_GET0I( 'BOX_YSIZE', BOX_YSIZE, STATUS)

	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :                  'Error : PLOT_BOX : after PAR_GET BOX SIZE',
     :                  STATUS )
	  RETURN
	END IF

* get the box positioning parameter

	CALL PAR_GET0C( 'BOX_POSITION', BOX_POSITION, STATUS)

	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :                  'Error : PLOT_BOX : after PAR_GET BOX POSITION',
     :                  STATUS )
	  RETURN
	END IF

* get the box position from the parameter system

	CALL PAR_GET0I( 'BOX_XCEN', BOX_XPOS, STATUS)
	CALL PAR_GET0I( 'BOX_YCEN', BOX_YPOS, STATUS)

	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :                  'Error : PLOT_BOX : after PAR_GET BOX POSITION',
     :                  STATUS )
	  RETURN
	END IF

* get the number of arcsecs/pixel from the parameter system

	CALL PAR_GET0R( 'ARCSEC_PIXEL', ARCSEC_PIXEL, STATUS)

	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :                  'Error : PLOT_BOX : after PAR_GET ARCSEC/PIXEL',
     :                  STATUS )
	  RETURN
	END IF

* get the colour of the box from the parameter system

	CALL PAR_GET0I( 'BOX_PEN', PEN_NUMBER, STATUS)
	CALL PAR_GET0C( 'BOX_COLOUR', COLOUR_CODE, STATUS)

	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :                  'Error : PLOT_BOX : after PAR_GET BOX COLOUR',
     :                  STATUS )
	  RETURN
	END IF

* set colour of box

	CALL SET_COLOUR( PEN_NUMBER, COLOUR_CODE)

* calculate magnification of image displayed

	MAGNIF =  REAL( IM_XEN - IM_XST) / REAL(NX)

* convert box position and box size to real units

	RBOX_XPOS = IM_XST + ( BOX_XPOS - 0.5)*MAGNIF
	RBOX_YPOS = IM_YST + ( BOX_YPOS - 0.5)*MAGNIF

	RBOX_XSIZE = BOX_XSIZE*MAGNIF/ARCSEC_PIXEL
	RBOX_YSIZE = BOX_YSIZE*MAGNIF/ARCSEC_PIXEL

* test whether box to be positioned at CENTRE or BOTTOM LEFT

	IF( BOX_POSITION .EQ. 'CENTRE') THEN

* define start and end pixels for BOX plot

	  X1 = RBOX_XPOS - 0.5*RBOX_XSIZE
	  Y1 = RBOX_YPOS - 0.5*RBOX_YSIZE
	  X2 = X1 + RBOX_XSIZE
	  Y2 = Y1 + RBOX_YSIZE

	ELSE

* define start and end of box for plot

	  X1 = RBOX_XPOS
	  Y1 = RBOX_YPOS
	  X2 = X1 + RBOX_XSIZE
	  Y2 = Y1 + RBOX_YSIZE

	END IF

* plot BOX

	IF( ( X2 - X1) .GE. 1.0 .AND. ( Y2 - Y1) .GE. 1.0) THEN

	  CALL SGS_BOX( X1, X2, Y1, Y2)

	END IF

* flush buffer of residual output

	CALL SGS_FLUSH

	RETURN
	END
