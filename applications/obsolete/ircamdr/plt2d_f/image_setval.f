	SUBROUTINE IMAGE_SETVAL( NAXIS1, NAXIS2, SCRATCH_ARRAY, STATUS)

* Description : Routine to set the scratch image to ZERO

* ==========================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*   Changed NAXIS arguments for UNIX compiler (SKL@JACH)
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

	INTEGER NAXIS1
	INTEGER NAXIS2
	INTEGER SCRATCH_ARRAY( NAXIS1, NAXIS2)

* Import-Export :

* Export :

* External references :

* Local Constants :

* Local variables :

	INTEGER J
	INTEGER K

* Internal References :

* =====================================================================

* check status on entry

	IF( STATUS. NE. SAI__OK) THEN
	  RETURN
	END IF

* loops to set array to zero for plotting

	DO J = 1, NAXIS2

	  DO K = 1, NAXIS1

	    SCRATCH_ARRAY( K, J) = 0

	  END DO

	END DO

	END
