	SUBROUTINE SURROUND_ANNOTATE( STATUS)

* Description : Routine to plot default anotation on an image
* makes the IR data look nice and neat for photographing the
* screen

* =====================================================================

* Invocation : Invoked by PLT2D

* Parameters :

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*   26-JUL-1994 Removed VALUE as error reporting changed (SKL@JACH)
*   02-Sep-1994 Removed unused variables flagged by UNIX compiler (SKL@JACH)
*  26-Oct-1994 Changed MAGNIF from INT to REAL (SKL@JACH)
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'

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

* Local variables :

	REAL MAGNIF
	INTEGER NF
	INTEGER NPR
	INTEGER J
	INTEGER TICKSPA
	INTEGER PEN_NUMBER

	REAL ANNOTATE_SIZE
	REAL AXRAT
	REAL AR
	REAL DIVISOR
	REAL HM
	REAL HT
	REAL SP
	REAL XTICKINT
	REAL YTICKINT
	REAL XU
	REAL YU

	CHARACTER*1 COLOUR_CODE
	CHARACTER*2 TXJ

* Local Constants :

* Internal References :

* =====================================================================

* check status on entry
	IF( STATUS .NE. SAI__OK)THEN
	  RETURN
	END IF

* get all current text attributes
	CALL SGS_IMTX( HM, NF, NPR, HT, AR, XU, YU, SP, TXJ)

* calculate magnification factor used in last image display
	MAGNIF = REAL( IM_XEN - IM_XST)/REAL( NX)

* get text size from parameter system
	CALL PAR_GET0R( 'ANNOTATE_SIZE', ANNOTATE_SIZE, STATUS)

* set text size
	CALL SGS_SHTX( ANNOTATE_SIZE*MAGNIF/16.0*REAL(NY)/64.0)

* set text orientation
	CALL SGS_SUPTX( 0.0, 1.0)

* get colour of ANNOTATE lines
	CALL PAR_GET0I( 'ANNOTATE_PEN', PEN_NUMBER, STATUS)
	CALL PAR_GET0C( 'ANNOTATE_COLOUR', COLOUR_CODE, STATUS)

* set annotation colour
	CALL SET_COLOUR( PEN_NUMBER, COLOUR_CODE)

* plot border lines
	CALL SGS_BOX( IM_XST, IM_XEN, IM_YST, IM_YEN)
	CALL SGS_BOX( IM_XST-1, IM_XEN+1, IM_YST-1, IM_YEN+1)

* get the tick interval from parameter system
	CALL PAR_GET0R( 'CONTOUR_TICKINT', XTICKINT, STATUS)
	CALL PAR_GET0R( 'CONTOUR_YTICKIN', YTICKINT, STATUS)
	CALL PAR_GET0R( 'CONTOUR_AXRAT', AXRAT, STATUS)
	CALL PAR_GET0I( 'CONTOUR_TICKSPA', TICKSPA, STATUS)

* call subroutine to plot tick marks
	CALL CONTOUR_TICKS( XTICKINT, YTICKINT, MAGNIF, AXRAT, STATUS)

* put in the intermediate tick marks as requested
	DIVISOR = 1.5
	DO J = 1, TICKSPA
	  CALL CONTOUR_TICKSEC( DIVISOR,  XTICKINT/(2**J), YTICKINT/(2**J),
     :	                        MAGNIF, AXRAT, STATUS)
	END DO

* call subroutine to plot numbers
	CALL CONTOUR_NUMBERS( XTICKINT, YTICKINT, MAGNIF, AXRAT, STATUS)

* reset text attributes to old values
	CALL SGS_SHTX( HT)
	CALL SGS_SUPTX( XU, YU)
	CALL SGS_STXJ( TXJ)
	CALL SGS_FLUSH
	STATUS = SAI__OK

	END
