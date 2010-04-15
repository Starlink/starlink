	SUBROUTINE COLOUR_TABLE( STATUS)

* Description : Subroutine to plot colour table to workstation

* ========================================================================

* Invocation : invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
* 02-MAY-86 : CAA : added CMP_UNMAP,DAT_ANNUL AFTER BAD CMP_MAPN
* 17-FEB-94   SKL@JACH changed CMP and DAT routines to NDF
*25-JUL-1994  SKL@JACH Changed error reporting to ERR calls, removed VALUE
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'

	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'

	INCLUDE 'SAE_PAR'

* Import :

	INTEGER STATUS

* Import-Export :

* Export :

* External references :

* Local Constants :

* Local variables :

	INTEGER NAXIS( 2)
	INTEGER POINTER_CT
	INTEGER LOC_CT
        INTEGER NDIM
        INTEGER NELEMENTS

* Internal References :

* Local data :

* ===================================================================

* check status on entry

	IF( STATUS . NE. SAI__OK) THEN

	  RETURN

	END IF

* get colour table locator from parameter name 'COLOUR_TABLE'

	CALL NDF_ASSOC( 'CT_NAME', 'READ', LOC_CT, STATUS)

	IF( STATUS. NE. SAI__OK) THEN

          CALL ERR_REP('ERR', 'COLOUR_TABLE : after NDF_ASSOC',
     :                  STATUS )

	  RETURN

	END IF

* map colour table data in 'DATA_ARRAY'

	CALL NDF_MAP( LOC_CT, 'Data', '_REAL', 'READ',
     :	              POINTER_CT, NELEMENTS, STATUS)

	IF( STATUS. NE. SAI__OK)THEN

          CALL ERR_REP('ERR', 'COLOUR_TABLE : after NDF_MAP',
     :                  STATUS )

	  CALL NDF_ANNUL( LOC_CT, STATUS)

	  RETURN

	END IF

* check size of input colour table

        CALL NDF_DIM( LOC_CT, 2, NAXIS, NDIM, STATUS)

	IF( NAXIS( 1) .NE. 3 .OR. NAXIS( 2) .NE. 256) THEN

          CALL MSG_OUT('ERR',
     :                 'COLOUR_TABLE : Colour table incorrect size',
     :                  STATUS )

	  CALL NDF_ANNUL( LOC_CT, STATUS)

	  RETURN

	END IF

* call subroutine to write colour table to workstation

	CALL WRITE_CT( NAXIS( 1), NAXIS( 2), %VAL( POINTER_CT),
     :	               STATUS)

	IF( STATUS .NE. SAI__OK) THEN

	  CALL NDF_ANNUL( LOC_CT, STATUS)

	  RETURN

	END IF


* annul active locator and unmap data array if necessary

	CALL NDF_ANNUL( LOC_CT, STATUS)

	IF( STATUS .NE. SAI__OK) THEN

          CALL ERR_REP('ERR',
     :                  'COLOUR_TABLE : after NDF_ANNUL',
     :                  STATUS )

	  RETURN

	END IF

	END
