	SUBROUTINE DEVICE_CODE( STATUS)

* Description : Translate device type into workstation identifier

* =====================================================================

* Invocation : Invoked by any D-Task/A-Task/C-Task

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*   26-JUL-94 Changed error reporting to use ERR_, removed VALUE (SKL@JACH)
*   04-Oct-94 Modified coded for SUn compatiblity (CAA@JACH)
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

	CHARACTER TEK_LINE*10, HOST*30

* Internal References :

* ================================================================

* test status on entry
	IF( STATUS .NE. SAI__OK) THEN
	  RETURN
	END IF

* do translation
	WKSTN_ID = '0,0'
	WKSTN_SEQ = 0

* Args device
	IF( DEVICE_NAME. EQ. 'ARGS') THEN
	  WKSTN_ID = '160,0'
	  WKSTN_SEQ = 160

* ARGS Overlays  device
	ELSE IF( DEVICE_NAME. EQ. 'ARGS_OVERLAY') THEN
	  WKSTN_ID = '161,0'
	  WKSTN_SEQ = 161

* Sigmex 6134 device
	ELSE IF( DEVICE_NAME. EQ. 'T6134') THEN
	  WKSTN_ID = '158,0'
	  WKSTN_SEQ = 158

* Digisolve Ikon device
	ELSE IF( DEVICE_NAME. EQ. 'IKON') THEN
	  WKSTN_ID = '3200,0'
	  WKSTN_SEQ = 3200

* Digisolve Ikon overlay planes device
	ELSE IF( DEVICE_NAME. EQ. 'IKON_OVERLAY') THEN
	  WKSTN_ID = '3201,0'
	  WKSTN_SEQ = 3201

* Vaxstation 8-plane device
	ELSE IF( DEVICE_NAME. EQ. 'VAXSTATION8') THEN
	  WKSTN_ID = '1742,0'
	  WKSTN_SEQ = 1742

* X-Windows devices
	ELSE IF( DEVICE_NAME. EQ. 'X-WINDOWS') THEN
	  WKSTN_ID = '3800,0'
	  WKSTN_SEQ = 3800
	ELSE IF( DEVICE_NAME. EQ. 'X-WINDOWS2') THEN
	  WKSTN_ID = '3801,0'
	  WKSTN_SEQ = 3801
	ELSE IF( DEVICE_NAME. EQ. 'X-WINDOWS3') THEN
	  WKSTN_ID = '3802,0'
	  WKSTN_SEQ = 3802
	ELSE IF( DEVICE_NAME. EQ. 'X-WINDOWS4') THEN
	  WKSTN_ID = '3803,0'
	  WKSTN_SEQ = 3803

* Vaxstation 4-plane device
	ELSE IF( DEVICE_NAME. EQ. 'VAXSTATION4') THEN
	  WKSTN_ID = '1741,0'
	  WKSTN_SEQ = 1741

* Vaxstation 2-plane device
	ELSE IF( DEVICE_NAME. EQ. 'VAXSTATION2') THEN
          WKSTN_ID = '1740,0'
          WKSTN_SEQ = 1740

* Tektronix T4010 device
	ELSE IF( DEVICE_NAME. EQ. 'T4010') THEN
	  CALL PAR_GET0C( 'TEK_LINE', TEK_LINE, STATUS)
	  IF( TEK_LINE .EQ. 'REMOTE') THEN
	    WKSTN_ID = '201,1'
	    WKSTN_SEQ = 201
	  ELSE
	    WKSTN_ID = '201,0'
	    WKSTN_SEQ = 201
	  END IF

* Tektronix T4014 device
	ELSE IF( DEVICE_NAME. EQ. 'T4014') THEN
	  CALL PAR_GET0C( 'TEK_LINE', TEK_LINE, STATUS)
	  IF( TEK_LINE .EQ. 'REMOTE') THEN
	    WKSTN_ID = '203,1'
	    WKSTN_SEQ = 203
	  ELSE
	    WKSTN_ID = '203,0'
	    WKSTN_SEQ = 203
	  END IF

* Versatek device
	ELSE IF( DEVICE_NAME .EQ. 'VERSATEK') THEN
	  WKSTN_ID = '1101,4'
	  WKSTN_SEQ = 1101

* Printronix/LP device
	ELSE IF( DEVICE_NAME. EQ. 'PRINTRONIX') THEN
	  WKSTN_ID = '1200,7'
	  WKSTN_SEQ = 1200

* Calcomp 81  device
	ELSE IF( DEVICE_NAME. EQ. 'CALCOMP') THEN
	  WKSTN_ID = '704,0'
	  WKSTN_SEQ = 705

* LN03 LOW RESOLUTION device
	ELSE IF( DEVICE_NAME. EQ. 'LN03_LO') THEN
	  WKSTN_ID = '3700,1'
	  WKSTN_SEQ = 3700

* LN03 HIGH RESOLUTION  device
	ELSE IF( DEVICE_NAME. EQ. 'LN03_HI') THEN
	  WKSTN_ID = '3701,1'
	  WKSTN_SEQ = 3701

* QMS Landscape device
	ELSE IF( DEVICE_NAME. EQ. 'QMS_LANDSCAPE') THEN
	  WKSTN_ID = '2011,1'
	  WKSTN_SEQ = 2011

* QMS Portrait device
	ELSE IF( DEVICE_NAME. EQ. 'QMS_PORTRAIT') THEN
	  WKSTN_ID = '2010,1'
	  WKSTN_SEQ = 2010

* POSTSCRIPT Portrait device
	ELSE IF( DEVICE_NAME. EQ. 'PS_PORTRAIT') THEN
!	  WKSTN_ID = '2704,1'   ! Old VAX numbers
!	  WKSTN_SEQ = 2704
	  WKSTN_ID = '2700,1'
	  WKSTN_SEQ = 2700

* POSTSCRIPT Landscape device
	ELSE IF( DEVICE_NAME. EQ. 'PS_LANDSCAPE') THEN
!	  WKSTN_ID = '2705,1'   ! Old VAX numbers
!	  WKSTN_SEQ = 2705
	  WKSTN_ID = '2701,1'
	  WKSTN_SEQ = 2701

* Encapsulated POSTSCRIPT Portrait device
	ELSE IF( DEVICE_NAME. EQ. 'EPSP') THEN
	  WKSTN_ID = '2702,1'
	  WKSTN_SEQ = 2702

* Encapsulated POSTSCRIPT Landscape device
	ELSE IF( DEVICE_NAME. EQ. 'EPSL') THEN
	  WKSTN_ID = '2703,1'
	  WKSTN_SEQ = 2703

* Colour POSTSCRIPT Portrait device
	ELSE IF( DEVICE_NAME. EQ. 'CPSP') THEN
	  CALL PAR_GET0C( 'HOST', HOST, STATUS)
	  CALL CHR_UCASE( HOST)
	  IF( HOST( 1:3) .EQ. 'VAX') THEN
	    WKSTN_ID = '2708,1'
	    WKSTN_SEQ = 2708
	  ELSE
	    WKSTN_ID = '2720,1'
	    WKSTN_SEQ = 2720
	  END IF

* Colour POSTSCRIPT Landscape device
	ELSE IF( DEVICE_NAME. EQ. 'CPSL') THEN
	  CALL PAR_GET0C( 'HOST', HOST, STATUS)
	  CALL CHR_UCASE( HOST)
	  IF( HOST( 1:3) .EQ. 'VAX') THEN
	    WKSTN_ID = '2709,1'
	    WKSTN_SEQ = 2709
	  ELSE
	    WKSTN_ID = '2721,1'
	    WKSTN_SEQ = 2721
	  END IF

* Encapsulated Colour POSTSCRIPT Portrait device
	ELSE IF( DEVICE_NAME. EQ. 'ECPSP') THEN
	  CALL PAR_GET0C( 'HOST', HOST, STATUS)
	  CALL CHR_UCASE( HOST)
	  IF( HOST( 1:3) .EQ. 'VAX') THEN
	    WKSTN_ID = '2708,1'
	    WKSTN_SEQ = 2708
	  ELSE
	    WKSTN_ID = '2722,1'
	    WKSTN_SEQ = 2722
	  END IF

* Encapsluated Colour POSTSCRIPT Landscape device
	ELSE IF( DEVICE_NAME. EQ. 'ECPSL') THEN
	  CALL PAR_GET0C( 'HOST', HOST, STATUS)
	  CALL CHR_UCASE( HOST)
	  IF( HOST( 1:3) .EQ. 'VAX') THEN
	    WKSTN_ID = '2709,1'
	    WKSTN_SEQ = 2709
	  ELSE
	    WKSTN_ID = '2723,1'
	    WKSTN_SEQ = 2723
	  END IF

* Canon laser printer devices
	ELSE IF( DEVICE_NAME. EQ. 'CANON_L') THEN
	  WKSTN_ID = '2600,0'
	  WKSTN_SEQ = 2600
	ELSE IF( DEVICE_NAME. EQ. 'CANON_P') THEN
	  WKSTN_ID = '2601,0'
	  WKSTN_SEQ = 2601

* Canon TeX laser printer devices
	ELSE IF( DEVICE_NAME. EQ. 'CANON_LTEX') THEN
	  WKSTN_ID = '2610,0'
	  WKSTN_SEQ = 2610
	ELSE IF( DEVICE_NAME. EQ. 'CANON_PTEX') THEN
	  WKSTN_ID = '2611,0'
	  WKSTN_SEQ = 2611

* ZETA8 device
	ELSE IF( DEVICE_NAME. EQ. 'ZETA') THEN
	  WKSTN_ID = '1000,9'
	  WKSTN_SEQ = 1000

* CIFER T4 device
	ELSE IF( DEVICE_NAME. EQ. 'CIFER_T4') THEN
	  WKSTN_ID = '801,0'
	  WKSTN_SEQ = 801

* Sigmex 5688 device
	ELSE IF( DEVICE_NAME. EQ. 'T5688') THEN
	  WKSTN_ID = '107,1'
	  WKSTN_SEQ = 107

* test WKSTN_ID identifier has been set to legal value
	ELSE
	  CALL MSG_SETC( 'NAM', DEVICE_NAME)
          CALL MSG_OUT( 'ERR',
     :	        'Error : DEVICE_CODE : Incorrect workstation name ^NAM',
     :	                STATUS )
	END IF

	END
