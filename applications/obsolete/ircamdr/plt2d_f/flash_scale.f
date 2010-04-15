
	SUBROUTINE FLASH_SCALE( DATA_ARRAY, SCRATCH_ARRAY, STATUS)

* Description : Routine to put an image into SCRATCH_ARRAY and pass back to
*               subroutine IMAGE_DISPLAY for plotting to workstation

* ==========================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'
	INCLUDE 'SAE_PAR'
	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

* Global variables

	INCLUDE 'PLT2DCOM'

* Status :

	INTEGER STATUS

* Import :

	INTEGER SCRATCH_ARRAY( NX, NY)

	REAL DATA_ARRAY( NX, NY)

* Import-Export :

* Export :

* External references :

* Local Constants :

* Local variables :

	INTEGER J
	INTEGER K

	REAL VALUE_SCALED

	CHARACTER*30 IMAGE_ORI

* Internal References :

* =====================================================================

* check status on entry

	IF( STATUS. NE. SAI__OK)THEN

	  RETURN

	END IF

* get the image orientation parameter

	CALL PAR_GET0C( 'IMAGE_ORIENT', IMAGE_ORI, STATUS)

* loops to data into output array

	DO J = 1, NY

	  DO K = 1, NX

* specify scaled value

	    VALUE_SCALED = DATA_ARRAY( K, J)

* test the value of the scaled data array

	    IF( VALUE_SCALED .LT. 0.0) VALUE_SCALED = 0.0
	    IF( VALUE_SCALED .GT. MAXIMCOL) VALUE_SCALED = MAXIMCOL

* invert the canon and qms intensity scale

	    IF( DEVICE_NAME .EQ. 'CANON'. OR.
     :	        DEVICE_NAME .EQ. 'QMS_LANDSCAPE' .OR.
     :	        DEVICE_NAME .EQ. 'QMS_PORTRAIT' .OR.
     :	        DEVICE_NAME .EQ. 'PS_LANDSCAPE' .OR.
     :	        DEVICE_NAME .EQ. 'PS_PORTRAIT' .OR.
     :	        DEVICE_NAME .EQ. 'EPSL' .OR.
     :	        DEVICE_NAME .EQ. 'EPSP') THEN

	      VALUE_SCALED = MAXIMCOL - VALUE_SCALED

	    END IF

* put value into scratch array called SCRATCH_ARRAY

	    IF( DEVICE_NAME .EQ. 'T5688') THEN

* test if user wants image flipped EW when scaled

	      IF( IMAGE_ORI .EQ. 'IRCAM') THEN

	        SCRATCH_ARRAY( NX-K+1, J) = IFIX( VALUE_SCALED)

	      ELSE

	        SCRATCH_ARRAY( K, J) = IFIX( VALUE_SCALED)

	      END IF

	    ELSE

* test if user wants image flipped EW when scaled

	      IF( IMAGE_ORI .EQ. 'IRCAM') THEN

!	        SCRATCH_ARRAY( NX-K+1, NY-J+1) = IFIX( VALUE_SCALED)

	        SCRATCH_ARRAY( NX-K+1, J) = IFIX( VALUE_SCALED)

	      ELSE

	        SCRATCH_ARRAY( K, NY-J+1) = IFIX( VALUE_SCALED)

!	        SCRATCH_ARRAY( K, J) = IFIX( VALUE_SCALED)

	      END IF

	    END IF

	  END DO

	END DO

	END
