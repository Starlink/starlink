	SUBROUTINE CONVERT_VALUES( STATUS)

* Description : gets pixel position and converts to real position

* =====================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
* 1) CAA, 01st-Jan-90 : added getting of image start and end points from IF
*    SKL  26th-Jul-94   changed error reporting so removed VALUE
*    SKL  26thOct1994   changed MAGNIF from INT to REAL, changed IFIX to INT
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'

	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

	INCLUDE 'SAE_PAR'

* Import :

	INTEGER STATUS

	INTEGER X_PIXEL
	INTEGER Y_PIXEL

	REAL XRPOS
	REAL YRPOS

* Import-Export :

* Export :

* External references :

* Local Constants :

	INCLUDE 'PLT2DCOM'

* Local variables :

	REAL MAGNIF
	REAL RIM_XST
	REAL RIM_XEN
	REAL RIM_YST
	REAL RIM_YEN

	CHARACTER*30 IMAGE_ORI

* Internal References :

* Local data :

* ============================================================================

* test status on entry

	IF( STATUS. NE. SAI__OK) THEN
	  RETURN
	END IF

* get the option that plots images flipped EW

	CALL PAR_GET0C( 'IMAGE_ORIENT', IMAGE_ORI, STATUS)

* get the image x and y start and end parameters from interface file

	CALL PAR_GET0R( 'IM_XST', RIM_XST, STATUS)
	CALL PAR_GET0R( 'IM_XEN', RIM_XEN, STATUS)
	CALL PAR_GET0R( 'IM_YST', RIM_YST, STATUS)
	CALL PAR_GET0R( 'IM_YEN', RIM_YEN, STATUS)

	IM_XST = INT( RIM_XST + 0.5)
	IM_XEN = INT( RIM_XEN + 0.5)
	IM_YST = INT( RIM_YST + 0.5)
	IM_YEN = INT( RIM_YEN + 0.5)

* calculate magnification from start,end values and image size

	MAGNIF = (IM_XEN - IM_XST) / REAL(NX)

* get the pixel value to be converted

	CALL PAR_GET0I( 'X_CUR_PIXEL', X_PIXEL, STATUS)
	CALL PAR_GET0I( 'Y_CUR_PIXEL', Y_PIXEL, STATUS)

* test if image orientation flipped for plotting

	IF( IMAGE_ORI .EQ. 'IRCAM') THEN

* calculate real position of pixel when image flipped EW

	  XRPOS = ( NX - X_PIXEL + 0.5)*MAGNIF + IM_XST
	  YRPOS = ( NY - Y_PIXEL + 0.5)*MAGNIF + IM_YST

	ELSE

* calculate real position of pixel

	  XRPOS = ( X_PIXEL - 0.5)*MAGNIF + IM_XST
	  YRPOS = ( Y_PIXEL - 0.5)*MAGNIF + IM_YST

	END IF

* put the real value after being converted

	CALL PAR_PUT0R( 'X_CUR_REAL', XRPOS, STATUS)
	CALL PAR_PUT0R( 'Y_CUR_REAL', YRPOS, STATUS)

	END
