	SUBROUTINE DEVICE_CLOSE( BASE_ZONE, STATUS)

* subroutine to close SGS/GKS under specified workstation

* subroutines called :
*                      SGS_CLOSE	- ADAMGRAPH/OPT

* History
* 26-Jul-1994 Changed error reporting to use ERR_, removed VALUE (SKL@JACH)
*

	IMPLICIT NONE

	INCLUDE 'ADAM_DEFNS'
        INCLUDE 'SAE_PAR'
	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

	INCLUDE 'PLT2DCOM'

	INTEGER BASE_ZONE
	INTEGER STATUS

* CLOSE SGS/GKS for plotting on device WKSTN_ID

	CALL SGS_CLSWK( BASE_ZONE, STATUS)

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR', 'DEVICE_CLOSE : after SGS_CLSWK',
     :                  STATUS )
          RETURN
	END IF

	END
