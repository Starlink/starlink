	SUBROUTINE CURSOR_POSITION( STATUS)

* Description : Put up cursor on workstation and return position ONLY

* =====================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
* 18-APR-86 : CAA : created this from CURSOR_DISPLAY subroutine
* 05-OCT-87 : CAA : changed to sgs v7.2 calls from gks6
* 26-Jul-94   SKL   changed error reporting to use ERR_, removed VALUE
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
     :       'CURSOR_POSITION : after PAR_GETs LAST CURSOR POSITION',
     :                 STATUS )

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

* set the cursor real position to the integer nearest value real number

	XRPOS = FLOAT( IFIX( XRPOS + 0.5))

	YRPOS = FLOAT( IFIX( YRPOS + 0.5))

* put real position of cursor to parameter system

	CALL PAR_PUT0R( 'X_CUR_REAL', XRPOS, STATUS)

	CALL PAR_PUT0R( 'Y_CUR_REAL', YRPOS, STATUS)


	IF( STATUS .NE. SAI__OK) THEN

          CALL ERR_REP('ERR',
     :       'CURSOR_POSITION : after PAR_GETs CURSOR REAL POSITION',
     :                  STATUS )

	  RETURN

	END IF

* test if user wants a mark at cursor position

	CALL PAR_GET0C( 'CURSOR_CROSS', CURSOR_CROSS, STATUS)

	IF( CURSOR_CROSS .EQ. 'NO') THEN

	ELSE

* put a cross where the cursor was

	  CALL CURSOR_MARK( STATUS)

	END IF

	END
