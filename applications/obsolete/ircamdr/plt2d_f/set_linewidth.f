	SUBROUTINE SET_LINEWIDTH( STATUS)

* subroutine set the pen width to be used for specified device

	IMPLICIT NONE

	INCLUDE 'ADAMDEFNS'
	INCLUDE 'ADAMERRS'

	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

	INCLUDE 'GKS_PAR'

	INCLUDE 'PLT2DCOM'

	INTEGER STATUS

	REAL LINFAC

* =============================================================================

* get and set line width on workstation

!	IF( DEVICE_NAME .EQ. 'PS_PORTRAIT' .OR.
!     :	    DEVICE_NAME .EQ. 'PS_LANDSCAPE' .OR.
!     :	    DEVICE_NAME .EQ. 'QMS_PORTRAIT' .OR.
!     :	    DEVICE_NAME .EQ. 'QMS_LANDSCAPE' .OR.
!     :	    DEVICE_NAME .EQ. 'LN03_HI' .OR.
!     :	    DEVICE_NAME .EQ. 'LN03_LO' .OR.
!     :	    DEVICE_NAME .EQ. 'CANON') THEN

	  CALL PAR_GET0R( 'LINFAC', LINFAC, STATUS)
	  CALL GSLWSC( LINFAC)

!	END IF

	END
