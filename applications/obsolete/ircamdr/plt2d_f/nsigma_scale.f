	SUBROUTINE NSIGMA_SCALE( DATA_ARRAY, SCRATCH_ARRAY, NSIGMA_SORT,
     :	                         SIGMA_LEVEL, SIGMA_UP, SIGMA_DOWN,
     :	                         STATUS)

* Description : Routine to scale an image wrt N sigma level and pass temporary
*               image created SCRATCH_ARRAY back to subroutine NSIGMA_DISPLAY
*               for plotting to workstation

* ==========================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*  27-Jul-1994 Changed error reporting to use ERR_, removed VALUE (SKL@JACH)
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
	REAL SIGMA_DOWN
	REAL SIGMA_LEVEL
	REAL SIGMA_UP

	CHARACTER*( *) NSIGMA_SORT

* Import-Export :

* Export :

* External references :

* Local Constants :

* Local variables :

	INTEGER J
	INTEGER K

	REAL VALUE_SCALED
	REAL MAXVAL
	REAL MINVAL
	REAL XMAXIMUM
	REAL XMINIMUM

	CHARACTER*30 IMAGE_ORI

* Internal References :

* =====================================================================

* check status on entry

	IF( STATUS. NE. SAI__OK)THEN
	  RETURN
	END IF

* initialize maximum and minimum value variables

	MAXVAL = -1E20
	MINVAL = 1E20

* get the image orientation parameter defining whether tyo flip image E-W
* when scaling

	CALL PAR_GET0C( 'IMAGE_ORIENT', IMAGE_ORI, STATUS)

* calculate max,min values for N sigma plot

	CALL NSIGMA_MAXMIN( DATA_ARRAY, NSIGMA_SORT, SIGMA_LEVEL,
     :	                    SIGMA_UP, SIGMA_DOWN, XMAXIMUM, XMINIMUM,
     :	                    STATUS)

	IF( STATUS .NE. SAI__OK) THEN
	  RETURN
	END IF

* put calculated max,min values into parameter system

	CALL PAR_PUT0R( 'CALCULATED_MAX', XMAXIMUM, STATUS)
	CALL PAR_PUT0R( 'CALCULATED_MIN', XMINIMUM, STATUS)

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :                  'Error : NSIGMA_SCALE : Illegal Max,Min values',
     :                  STATUS )
	  RETURN
	END IF

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

	      VALUE_SCALED = MINIMCOL + ( DATA_ARRAY( K, J) - XMINIMUM)/SCALE

	    END IF

* test value of number for values outside limits

	    IF( DEVICE_NAME .EQ. 'CANON'. OR.
     :	        DEVICE_NAME .EQ. 'QMS_LANDSCAPE' .OR.
     :	        DEVICE_NAME .EQ. 'QMS_PORTRAIT' .OR.
     :	        DEVICE_NAME .EQ. 'PS_LANDSCAPE' .OR.
     :	        DEVICE_NAME .EQ. 'PS_PORTRAIT' .OR.
     :	        DEVICE_NAME .EQ. 'EPSP' .OR.
     :	        DEVICE_NAME .EQ. 'EPSL' .OR.
     :	        DEVICE_NAME .EQ. 'CPSP' .OR.
     :	        DEVICE_NAME .EQ. 'CPSL') THEN

	      IF( VALUE_SCALED .LT. MINIMCOL) VALUE_SCALED = MINIMCOL
	      IF( VALUE_SCALED .GT. MAXIMCOL-1) VALUE_SCALED = MAXIMCOL-1

* invert the canon ps and qms intensity scale

	      VALUE_SCALED = MAXIMCOL - VALUE_SCALED

	    ELSE

	      IF( VALUE_SCALED .LT. MINIMCOL) VALUE_SCALED = MINIMCOL
	      IF( VALUE_SCALED .GT. MAXIMCOL) VALUE_SCALED = MAXIMCOL

	    END IF

* put value into scratch array called SCRATCH_ARRAY

	    IF( DEVICE_NAME .EQ. 'T5688') THEN

* test if users wants image flipped EW when scaling takes place

	      IF( IMAGE_ORI .EQ. 'IRCAM') THEN

	        SCRATCH_ARRAY( NX-K+1, J) = NINT( VALUE_SCALED)

	      ELSE

	        SCRATCH_ARRAY( K, J) = NINT( MAXIMCOL-VALUE_SCALED)

	      END IF

	    ELSE

* test if users wants image flipped EW when scaling takes place

	      IF( IMAGE_ORI .EQ. 'IRCAM') THEN

	        SCRATCH_ARRAY( NX-K+1, NY-J+1) = NINT( VALUE_SCALED)

	      ELSE

	        SCRATCH_ARRAY( K, NY-J+1) = NINT( VALUE_SCALED)

	      END IF

	    END IF

	    MAXVAL = MAX( MAXVAL, VALUE_SCALED)
	    MINVAL = MIN( MINVAL, VALUE_SCALED)

	  END DO

	END DO

	END
