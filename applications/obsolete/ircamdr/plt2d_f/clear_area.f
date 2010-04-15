	SUBROUTINE CLEAR_AREA( STATUS)

* Description : Routine to clear an area on a workstation

* =====================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
* 17-FEB-94 SKL@JACH DAT calls replaced by NDF
* 25-Jul-1994 Changed Error reporting to ERR calls (SKL@JACH)
* 11-OCT-1994 Changed IMAGE_SETVAL arguments for UNIX compiler (SKL@JACH)
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'

	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

	INCLUDE 'SAE_PAR'

        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'

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

	INTEGER NAXIS( 2)
	INTEGER POINTER_SCRATCH
        INTEGER LOCSR
        INTEGER NELEMENTS
        INTEGER LBND(2)

        DATA LBND / 1, 1 /

	REAL IMXST
	REAL IMYST
	REAL IMXEN
	REAL IMYEN

* Internal References :

* =====================================================================

* check status on entry

	IF( STATUS .NE. SAI__OK) THEN
	  RETURN
	END IF

* get start,end in X and Y for image position

	CALL PAR_GET0R( 'IM_XST', IMXST, STATUS)
	CALL PAR_GET0R( 'IM_YST', IMYST, STATUS)
	CALL PAR_GET0R( 'IM_XEN', IMXEN, STATUS)
	CALL PAR_GET0R( 'IM_YEN', IMYEN, STATUS)
!	type *, 'im_xst etc = ', imxst, imyst, imxen, imyen

	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP( 'ERR',
     :                   'Error : IMAGE_CLEAR : During PAR_GETs',
     :                   STATUS )
	  RETURN
	END IF

	NAXIS( 1) = IFIX( IMXEN - IMXST + 0.5)
	NAXIS( 2) = IFIX( IMYEN - IMYST + 0.5)
!	type *, 'naxis = ', naxis( 1), naxis( 2)

* create scratch area for scaled image and map it to obtain
* scratch area memory pointer

	CALL NDF_ASSOC( 'SCRATCH_NAME', 'WRITE', LOCSR, STATUS)

	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP( 'ERR',
     :                 'Error : IMAGE_CLEAR : during NDF_CREP scratch',
     :                   STATUS )
	  CALL NDF_ANNUL( LOCSR, STATUS)
	  RETURN
	END IF

*      reset scratch file pixel boundaries
        CALL NDF_SBND( 2, LBND, NAXIS, LOCSR, STATUS )

	CALL NDF_MAP( LOCSR, 'DATA', '_INTEGER', 'WRITE',
     :	              POINTER_SCRATCH, NELEMENTS, STATUS)

	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP( 'ERR',
     :                   'Error : IMAGE_CLEAR : During NDF_MAP scratch',
     :                   STATUS )
	  CALL NDF_ANNUL( LOCSR, STATUS)
	  RETURN
	END IF

* call subroutine to set temp image to minimum value in last image plotted

	CALL IMAGE_SETVAL( NAXIS(1), NAXIS(2), %VAL( POINTER_SCRATCH),
     :                     STATUS)

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP( 'ERR',
     :                   'Error : AREA_CLEAR : after IMAGE_SETVAL',
     :                   STATUS )
	  RETURN
	END IF

* plot data image after scaling

!          CALL GCA( IM_XST,
!     :              IM_YEN,
!     :              IM_XEN,
!     :              IM_YST,
!     :              NAXIS( 1),
!     :              NAXIS( 2),
!     :              1, 1,
!     :              NAXIS( 1),  NAXIS( 2),
!     :              %VAL( POINTER_IMAGE))

	CALL GCA( IMXST, IMYEN, IMXEN, IMYST, NAXIS( 1), NAXIS( 2),
     :	          1, 1, NAXIS( 1), NAXIS( 2),
     :	          %VAL( POINTER_SCRATCH))

* flush buffer of residual output

	CALL SGS_FLUSH

* annul active scratch locator which also unmaps mapped data

	CALL NDF_ANNUL( LOCSR, STATUS)

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP( 'ERR',
     :                   'Error : AREA_CLEAR : after NDF_ANNUL',
     :                   STATUS )
	  RETURN
	END IF

	END
