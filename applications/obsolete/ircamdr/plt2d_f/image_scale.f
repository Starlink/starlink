	SUBROUTINE IMAGE_SCALE( DATA_ARRAY, SCRATCH_ARRAY,
     :	                        XMAXIMUM, XMINIMUM, STATUS)

* Description : Routine to scale an image wrt XMINIMUM and SCALE
*               and pass temporary image created SCRATCH_ARRAY back to
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
	REAL SCALE
	REAL XMAXIMUM
	REAL XMINIMUM

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

* test if max,min are equal

	IF( ABS( ( XMAXIMUM - XMINIMUM)) .LT. 0.001) THEN

* if max,min equal then calculate max,min

	  CALL CALCULATE_MAXMIN( DATA_ARRAY, XMAXIMUM, XMINIMUM)

	END IF

* put calculated max,min values into parameter system

	CALL PAR_PUT0R( 'CALCULATED_MAX', XMAXIMUM, STATUS)
	CALL PAR_PUT0R( 'CALCULATED_MIN', XMINIMUM, STATUS)

* get the image orientation parameter

	CALL PAR_GET0C( 'IMAGE_ORIENT', IMAGE_ORI, STATUS)

* calculate scale factor from max,min

	SCALE = ( XMAXIMUM - XMINIMUM) / ( MAXIMCOL - MINIMCOL)

	IF( ABS( SCALE) .LT. 1.0E-10) THEN
	  SCALE = 1.0E-10
	END IF

* loops to scale array by scaling factor for plotting

	DO J = 1, NY

	  DO K = 1, NX

	    IF( DATA_ARRAY( K, J) .GT. 1.0E20) THEN

	      VALUE_SCALED = MINIMCOL

	    ELSE

	      VALUE_SCALED = MINIMCOL + ( DATA_ARRAY( K, J)- XMINIMUM)/ SCALE

	    END IF

* invert the canon and qms intensity scale

	    IF( DEVICE_NAME .EQ. 'CANON'. OR.
     :	        DEVICE_NAME .EQ. 'QMS_LANDSCAPE' .OR.
     :	        DEVICE_NAME .EQ. 'QMS_PORTRAIT' .OR.
     :	        DEVICE_NAME .EQ. 'PS_PORTRAIT' .OR.
     :	        DEVICE_NAME .EQ. 'PS_LANDSCAPE' .OR.
     :	        DEVICE_NAME .EQ. 'EPSP' .OR.
     :	        DEVICE_NAME .EQ. 'EPSL' .OR.
     :	        DEVICE_NAME .EQ. 'CPSP' .OR.
     :	        DEVICE_NAME .EQ. 'CPSL') THEN

* test value of number for values outside limits

	      IF( VALUE_SCALED .LT. MINIMCOL) VALUE_SCALED = MINIMCOL
	      IF( VALUE_SCALED .GT. MAXIMCOL-1) VALUE_SCALED = MAXIMCOL-1

	      VALUE_SCALED = MAXIMCOL - VALUE_SCALED

	    ELSE

* test value of number for values outside limits

	      IF( VALUE_SCALED .LT. MINIMCOL) VALUE_SCALED = MINIMCOL
	      IF( VALUE_SCALED .GT. MAXIMCOL) VALUE_SCALED = MAXIMCOL

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

	        SCRATCH_ARRAY( NX-K+1, NY-J+1) = IFIX( VALUE_SCALED)

	      ELSE

	        SCRATCH_ARRAY( K, NY-J+1) = IFIX( VALUE_SCALED)

	      END IF

	    END IF

	  END DO

	END DO

	END
