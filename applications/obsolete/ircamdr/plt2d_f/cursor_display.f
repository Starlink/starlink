	SUBROUTINE CURSOR_DISPLAY( STATUS)

* Description : Put up cursor on workstation and return position, value

* =====================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*   26th-Jul-94 Changed error reporting to use ERR_, removed VALUE (SKL@JACH)
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

* Import-Export :

* Export :

* External references :

* Local Constants :

	INCLUDE 'PLT2DCOM'

* Local variables :

	INTEGER DATSIZ
	INTEGER ERR
	INTEGER ESW
	INTEGER GSET /0/
	INTEGER MODE
	INTEGER PET
	INTEGER TNR
	INTEGER X_PIXEL
	INTEGER Y_PIXEL

	REAL CURSOR_VALUE
	REAL ECHVAL( 4)
	REAL RILPX
	REAL RILPY
	REAL XRPOS
	REAL YRPOS

	CHARACTER*20 CURSOR_CROSS
	CHARACTER*80 DATREC

* Internal References :

* Local data :

* ============================================================================

* test status on entry

	IF( STATUS. NE. SAI__OK) THEN
	  RETURN
	END IF

* get start position of cursor from parameter system

	CALL PAR_GET0R( 'X_CUR_REAL', XRPOS, STATUS)
	CALL PAR_GET0R( 'Y_CUR_REAL', YRPOS, STATUS)

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :         'CURSOR_DISPLAY : after PAR_GETs LAST CURSOR POSITION',
     :                  STATUS )
	  RETURN
	END IF

* inquire cursor parameters

	CALL GQLCS( 1, 1, GSET, 1, ERR, MODE, ESW, TNR, RILPX, RILPY,
     :	            PET, ECHVAL, DATSIZ, DATREC)

* put cursor on screen and get position and value

	IF( DEVICE_NAME .NE. 'T6134') THEN

	  CALL GINLC( 1, 1, 1, XRPOS, YRPOS, PET, ECHVAL( 1), ECHVAL( 2),
     :	              ECHVAL( 3), ECHVAL( 4), DATSIZ, DATREC)

	  CALL GRQLC( 1, 1, ERR, TNR, XRPOS, YRPOS)

	ELSE

	  CALL GINLC( 1, 3, 1, XRPOS, YRPOS, PET, ECHVAL( 1), ECHVAL( 2),
     :	              ECHVAL( 3), ECHVAL( 4), DATSIZ, DATREC)

	  CALL GRQLC( 1, 3, ERR, TNR, XRPOS, YRPOS)

	END IF

* call subroutine to calculate real position and value of cursor
* position in chosen image

	CALL CURSOR_VALUES( XRPOS, YRPOS, X_PIXEL, Y_PIXEL,
     :	                    CURSOR_VALUE, STATUS)

	IF( STATUS .NE. SAI__OK) THEN
	  RETURN
	END IF

* set the cursor real position to the integer nearest value real number

	XRPOS = FLOAT( IFIX( XRPOS + 0.5))
	YRPOS = FLOAT( IFIX( YRPOS + 0.5))

* put real position of cursor to parameter system

	CALL PAR_PUT0R( 'X_CUR_REAL', XRPOS, STATUS)
	CALL PAR_PUT0R( 'Y_CUR_REAL', YRPOS, STATUS)

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :         'CURSOR_DISPLAY : after PAR_PUTs CURSOR REAL POSITION',
     :                  STATUS )
	  RETURN
	END IF

* put pixel positions and value to parameter system

	CALL PAR_PUT0I( 'X_CUR_PIXEL', X_PIXEL, STATUS)
	CALL PAR_PUT0I( 'Y_CUR_PIXEL', Y_PIXEL, STATUS)

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :         'CURSOR_DISPLAY : after PAR_PUTs CURSOR PIXEL POSITION',
     :                  STATUS )
	  RETURN
	END IF

	CALL PAR_PUT0R( 'CURSOR_VALUE', CURSOR_VALUE, STATUS)

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :         'CURSOR_DISPLAY : after PAR_PUT CURSOR PIXEL VALUE',
     :                  STATUS )
	  RETURN
	END IF

* test if user wants a mark at cursor position

	CALL PAR_GET0C( 'CURSOR_CROSS', CURSOR_CROSS, STATUS)

	IF( CURSOR_CROSS .NE. 'NO') THEN

* call subroutine to mark cursor position

	  CALL CURSOR_MARK( STATUS)

	END IF

	END
