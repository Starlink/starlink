	SUBROUTINE PLOT_CURLINE( STATUS)

* Description : Routine to PLOT a line on workstation using cursor input

* =====================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
* 27-Jul-94 Changed error reporting to use ERR_, removed VALUE (SKL@JACH)
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

	REAL X1
	REAL X2
	REAL Y1
	REAL Y2

	CHARACTER COLOUR_CODE*1

* Internal References :

* =====================================================================

* check status on entry

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR', 'Error : PLOT_CURLINE : On entry',
     :                  STATUS )
	  RETURN
	END IF

* get colour of LINE from parameter aystem

	CALL PAR_GET0I( 'LINE_PEN', PEN_NUMBER, STATUS)
	CALL PAR_GET0C( 'LINE_COLOUR', COLOUR_CODE, STATUS)

	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR', 'Error : PLOT_CURLINE : after PAR_GETS',
     :                  STATUS )
	  RETURN
	END IF

* puts up cursor for start point of line

	CALL CURSOR_DISPLAY( STATUS)

	IF( STATUS. NE. SAI__OK) THEN
	  RETURN
	END IF

* gets start of line from cursor real values

	CALL PAR_GET0R( 'X_CUR_REAL', X1, STATUS)
	CALL PAR_GET0R( 'Y_CUR_REAL', Y1, STATUS)

	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :        'Error : PLOT_CURLINE : after PAR_GETs cursor 1 values',
     :                  STATUS )
	  RETURN
	END IF

* puts up cursor for end point of line

	CALL CURSOR_DISPLAY( STATUS)

	IF( STATUS. NE. SAI__OK) THEN
	  RETURN
	END IF

* gets start of line from cursor real values

	CALL PAR_GET0R( 'X_CUR_REAL', X2, STATUS)
	CALL PAR_GET0R( 'Y_CUR_REAL', Y2, STATUS)

	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :        'Error : PLOT_CURLINE : after PAR_GETs cursor 2 values',
     :                  STATUS )
	  RETURN
	END IF

* set colour of LINE

	CALL SET_COLOUR( PEN_NUMBER, COLOUR_CODE)

* draw line

	CALL SGS_LINE( X1, Y1, X2, Y2)

	CALL SGS_FLUSH

	END
