	SUBROUTINE CURSOR_MARK( STATUS)

* Description : Routine to plot CROSS at the selected cursor position

* =======================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*  26th-Jul-94 Changed error reporting to use ERR_, removed VALUE (SKL@JACH)
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
	INTEGER PEN_NUMBER

	REAL CROSS_XCEN
	REAL CROSS_YCEN

	CHARACTER COLOUR_CODE*1

* Internal References :

* ================================================================

* check status on entry

	IF( STATUS. NE. SAI__OK)THEN
	  RETURN
	END IF

* get and set cross parameters

	CROSS_SIZE = 8

	CALL PAR_GET0C( 'CROSS_COLOUR', COLOUR_CODE, STATUS)
	CALL PAR_GET0I( 'CROSS_PEN', PEN_NUMBER, STATUS)

	IF( STATUS. NE. SAI__OK)THEN
          CALL ERR_REP('ERR', 'Error : CURSOR_MARK after PAR_GETS',
     :                  STATUS )
	  RETURN
	END IF

* get cursor real position

	CALL PAR_GET0R( 'X_CUR_REAL', CROSS_XCEN, STATUS)
	CALL PAR_GET0R( 'Y_CUR_REAL', CROSS_YCEN, STATUS)

	IF( STATUS. NE. SAI__OK)THEN
          CALL ERR_REP('ERR',
     :           'Error : CURSOR_MARK after PAR_GET cursor position',
     :                  STATUS )
	  RETURN
	END IF

* set colour of CROSS

	CALL SET_COLOUR( PEN_NUMBER, COLOUR_CODE)

* plot CROSS

	CALL SGS_LINE( (CROSS_XCEN - 0.5*CROSS_SIZE),
     :	               (CROSS_YCEN),
     :	               (CROSS_XCEN + 0.5*CROSS_SIZE),
     :	               (CROSS_YCEN))

	CALL SGS_LINE( (CROSS_XCEN),
     :	               (CROSS_YCEN - 0.5*CROSS_SIZE),
     :	               (CROSS_XCEN),
     :	               (CROSS_YCEN + 0.5*CROSS_SIZE))

* flush buffer of residual output

	CALL SGS_FLUSH

	END
