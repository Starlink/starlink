	SUBROUTINE PLOT_GRID( STATUS)

* Description : Routine to PLOT a grid of lines on image

* =====================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*  27-Jul-94 Changed error reporting to use ERR_, removed VALUE,
*            changed IFIX to INT  (SKL@JACH)
* 26-oct-1994 Changed MAGNIF from INT to REAL (SKL@JACH)
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

	INTEGER PEN_NUMBER

	REAL ARCSEC_PIXEL
	REAL GRID_X_INC
	REAL GRID_Y_INC
	REAL X1
	REAL X2
	REAL X_INCREMENT
	REAL Y1
	REAL Y2
	REAL Y_INCREMENT
	REAL MAGNIF

	CHARACTER COLOUR_CODE*1

* Internal References :

* =====================================================================

* check status on entry

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR', 'PLOT_GRID : On entry', STATUS )
	  RETURN
	END IF

* get grid step from parameter system

	CALL PAR_GET0R( 'GRID_X_INC', GRID_X_INC, STATUS)
	CALL PAR_GET0R( 'GRID_Y_INC', GRID_Y_INC, STATUS)

* get number of arcseconds per pixel in data

	CALL PAR_GET0R( 'ARCSEC_PIXEL', ARCSEC_PIXEL, STATUS)

* get colour of grid from parameter aystem

	CALL PAR_GET0I( 'GRID_PEN', PEN_NUMBER, STATUS)
	CALL PAR_GET0C( 'GRID_COLOUR', COLOUR_CODE, STATUS)

	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR', 'PLOT_GRID : after PAR_GETS', STATUS )
	END IF

* calculate magnification from start,end values and image size

	MAGNIF = REAL(IM_XEN - IM_XST) / REAL(NX)

* set colour of grid

	CALL SET_COLOUR( PEN_NUMBER, COLOUR_CODE)

* loop for both X axis grid sections

	X1 = IM_XST
	Y1 = IM_YST
	Y2 = IFIX( REAL(IM_YST) + REAL( NY)*MAGNIF + 0.5)

* define x position

	X_INCREMENT = IFIX( GRID_X_INC*MAGNIF/ARCSEC_PIXEL + 0.5)

* loop to plot grid lines vertically

	DO WHILE ( X1 .LE. IM_XEN)

* increment X position

	  X1 = X1 + X_INCREMENT

* plot line

	  IF( X1 .LE. IM_XEN) THEN
	    CALL SGS_LINE( X1, Y1, X1, Y2)
	  END IF
	END DO

* loop for both Y axis grid sections

	X1 = IM_XST
	X2 = IFIX( REAL(IM_XST) + FLOAT( NX)*MAGNIF + 0.5)
	Y1 = IM_YST

* define Y position

	Y_INCREMENT = GRID_Y_INC*MAGNIF/ARCSEC_PIXEL

* loop to plot all horizontal lines

	DO WHILE ( Y1 .LE. IM_YEN)

* increment Y position

	  Y1 = Y1 + Y_INCREMENT

* plot line

	  IF( Y1 .LE. IM_YEN) THEN
	    CALL SGS_LINE( X1, Y1, X2, Y1)
	  END IF
	END DO

* empty plot buffer of remaining graphics

	CALL SGS_FLUSH

	END
