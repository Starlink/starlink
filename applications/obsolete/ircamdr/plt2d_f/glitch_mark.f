	SUBROUTINE GLITCH_MARK( STATUS)

* Description : Put up cursor on workstation and writes position to a
*               data file

* =====================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*  09-Aug-1988 : JACH::CAA : changed SGS cursor calls to GKS7.2
*  24-JUL-1994 : SKL@JACH Changed LIB$ calls to FIO_
*  26-Jul-1994   SKL@JACH Changed error reporting to use ERR_, removed VALUE,
*                         changed IFIX to INT
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'

	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

	INCLUDE 'SAE_PAR'
        INCLUDE 'FIO_ERR'

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
	INTEGER LUN
	INTEGER X_PIXEL
	INTEGER Y_PIXEL


	REAL ECHVAL( 4)
	REAL RILPX
	REAL RILPY
	REAL CURSOR_VALUE
	REAL XRPOS
	REAL YRPOS

	CHARACTER*80 DATREC
	CHARACTER*20 CURSOR_CROSS
	CHARACTER*80 DISKFILE

	LOGICAL BADPOS
	LOGICAL MORE

* Internal References :

* Local data :

* ============================================================================

*      test status on entry

	IF( STATUS. NE. SAI__OK) THEN
	  RETURN
	END IF

*      get the name of the disk file to contain the list of glitch positions

	CALL PAR_GET0C( 'DISK_FILENAME', DISKFILE, STATUS)

*      open disk file after getting a lun from system

	CALL FIO_GUNIT( LUN, STATUS )

	OPEN( UNIT=LUN, FILE=DISKFILE, STATUS='NEW', ERR=999)

*     write a title to the glitch file since GLITCH expects this ...

	WRITE( LUN, *)'PLT2D glitch marker - bad pixel positions'

*      initialise the bad position indicator BADPOS and the looping variable

	BADPOS = .FALSE.

	MORE = .TRUE.

*      get start position of cursor from parameter system

	CALL PAR_GET0R( 'X_CUR_REAL', XRPOS, STATUS)
	CALL PAR_GET0R( 'Y_CUR_REAL', YRPOS, STATUS)

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :            'GLITCH_MARK : after PAR_GETs LAST CURSOR POSITION',
     :                  STATUS )
	  RETURN
	END IF

*      test if user wants to mark the position with a cross

	CALL PAR_GET0C( 'CURSOR_CROSS', CURSOR_CROSS, STATUS)

*      loop to display cursor, get position and value and write to disk file
*      continues until an illegal position is entered

	DO WHILE( MORE)

* inquire cursor parameters

	  CALL GQLCS( 1, 1, GSET, 1, ERR, MODE, ESW, TNR, RILPX, RILPY,
     :	              PET, ECHVAL, DATSIZ, DATREC)

* put cursor on screen and get position and value

	  IF( DEVICE_NAME .NE. 'T6134') THEN

	    CALL GINLC( 1, 1, 1, XRPOS, YRPOS, PET, ECHVAL( 1), ECHVAL( 2),
     :	                ECHVAL( 3), ECHVAL( 4), DATSIZ, DATREC)

	    CALL GRQLC( 1, 1, ERR, TNR, XRPOS, YRPOS)

	  ELSE

	    CALL GINLC( 1, 3, 1, XRPOS, YRPOS, PET, ECHVAL( 1), ECHVAL( 2),
     :	                ECHVAL( 3), ECHVAL( 4), DATSIZ, DATREC)

	    CALL GRQLC( 1, 3, ERR, TNR, XRPOS, YRPOS)

	  END IF

*        call subroutine to calculate real position and value of cursor
*        position in chosen image

	  CALL GLITCH_VALUES( XRPOS, YRPOS, X_PIXEL, Y_PIXEL,
     :	                      CURSOR_VALUE, BADPOS, STATUS)

	  IF( STATUS .NE. SAI__OK) THEN

	    BADPOS = .TRUE.

	    MORE = .FALSE.

	  END IF

*        test if a valid position entered

	  IF( .NOT. BADPOS) THEN

*          set the cursor real position to the integer nearest value real number

	    XRPOS = FLOAT( INT( XRPOS + 0.5))
	    YRPOS = FLOAT( INT( YRPOS + 0.5))

*          put real position of cursor to parameter system

	    CALL PAR_PUT0R( 'X_CUR_REAL', XRPOS, STATUS)
	    CALL PAR_PUT0R( 'Y_CUR_REAL', YRPOS, STATUS)

*          put pixel positions and value to parameter system

	    CALL PAR_PUT0I( 'X_CUR_PIXEL', X_PIXEL, STATUS)
	    CALL PAR_PUT0I( 'Y_CUR_PIXEL', Y_PIXEL, STATUS)

	    CALL PAR_PUT0R( 'CURSOR_VALUE', CURSOR_VALUE, STATUS)

*          test whether user wants a cross

	    IF( CURSOR_CROSS .NE. 'NO') THEN

*            call subroutine to mark cursor position

	      CALL CURSOR_MARK( STATUS)

	    END IF

*          write the position (pixel) to the disk file

	    WRITE( LUN, *) X_PIXEL, Y_PIXEL

*          continue looping since a bad position not entered

	    MORE = .TRUE.

	  ELSE

*          stop looping since a bad position entered

	    MORE = .FALSE.

	  END IF

	END DO

*      close the disk file

	CLOSE( LUN)

*      free the lun used

	CALL FIO_PUNIT( LUN, STATUS )

	RETURN

  999   CALL ERR_REP('ERR', 'Error, cannot open disk file ...',
     :                STATUS )

	END
