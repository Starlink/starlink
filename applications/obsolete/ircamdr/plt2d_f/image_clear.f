	SUBROUTINE IMAGE_CLEAR( STATUS)

* Description : Routine to clear an image on a workstation

* =====================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
* 17-Feb-94 SKL@JACH Changed DAT routines to NDF
* 27-Jul-1994 SKL@JACH Changed error reporting to use ERR_, removed VALUE
* 11-Oct-1994 Changed IMAGE_SETVAL arguments for UNIX compiler (SKL@JACH)
* 26-Oct-1994 Changed MAGNIF from INT to REAL and start/end pixel logic
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'

        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'

	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

	INCLUDE 'SAE_PAR'

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

	REAL MAGNIF
	INTEGER NAXIS( 2)
	INTEGER POINTER_SCRATCH
	INTEGER XIMCEN
	INTEGER YIMCEN
	INTEGER LOCSR
        INTEGER NELEMENTS
        INTEGER LBND(2)
        DATA LBND / 1, 1 /

* Internal References :

* =====================================================================

* check status on entry

	IF( STATUS .NE. SAI__OK) THEN

	  RETURN

	END IF

* get start,end in X and Y for image position

	CALL PAR_GET0I( 'IM_XCEN', XIMCEN, STATUS)
	CALL PAR_GET0I( 'IM_YCEN', YIMCEN, STATUS)

	CALL PAR_GET0R( 'MAGNIFICATION', MAGNIF, STATUS)

* get last image size from parameter system

	CALL PAR_GET0I( 'IM_XSIZE', NX, STATUS)
	CALL PAR_GET0I( 'IM_YSIZE', NY, STATUS)

	IF( STATUS. NE. SAI__OK) THEN

          CALL ERR_REP('ERR', 'Error : IMAGE_CLEAR : During PAR_GETs',
     :                  STATUS )

	  RETURN

	END IF

* test if image centres are zero; if so then calculate centre of screen

	IF( XIMCEN .LE. 0 .OR. YIMCEN .LE. 0) THEN

	  IF( DEVICE_NAME .EQ. 'T5688') THEN

	    XIMCEN = MAX_X/4
	    YIMCEN = MAX_Y/4

	  ELSE

	    XIMCEN = MAX_X/2
	    YIMCEN = MAX_Y/2

	  END IF

	END IF

* create scratch area for scaled image and map it to obtain
* scratch area memory pointer

	NAXIS( 1) = NX
	NAXIS( 2) = NY

	CALL NDF_ASSOC( 'SCRATCH_NAME', 'WRITE', LOCSR, STATUS)

	IF( STATUS. NE. SAI__OK) THEN

          CALL ERR_REP('ERR',
     :                'Error : IMAGE_CLEAR : During NDF_ASSOC scratch',
     :                  STATUS )

	  RETURN

	END IF

*      reset pixel boundaries of scratch file
        CALL NDF_SBND( 2, LBND, NAXIS, LOCSR, STATUS)

	CALL NDF_MAP( LOCSR, 'Data', '_INTEGER', 'WRITE',
     :	              POINTER_SCRATCH, NELEMENTS, STATUS)

	IF( STATUS. NE. SAI__OK) THEN

          CALL ERR_REP('ERR',
     :                'Error : IMAGE_CLEAR : During NDF_MAP scratch',
     :                  STATUS )

	  RETURN

	END IF

* if magnification is 0 then need to auto scale to fill screen

	IF( MAGNIF .EQ. 0.0) THEN

* calculate magnification for maximum image dimension

	  IF( DEVICE_NAME .EQ. 'T5688') THEN

* set magnification for Sigma 5688 device working in quadrant  mode

	    IF( MAX_X .GE. MAX_Y) THEN

	      MAGNIF = REAL(MAX_Y)/2.0/REAL(NY)

	    ELSE

	      MAGNIF = REAL(MAX_X)/2.0/REAL(NX)

	    END IF

	  ELSE

* set magnification for other devices

	    IF( MAX_X .GE. MAX_Y) THEN

	      MAGNIF = REAL(MAX_Y)/REAL(NY)

	    ELSE

	      MAGNIF = REAL(MAX_X)/REAL(NX)

	    END IF

	  END IF

* test for above maximum value for magnification

	  IF( MAGNIF .GT. 25.0) THEN

	    MAGNIF = 25.0

	  END IF

	END IF

* call subroutine to set temp image to minimum value in last image plotted

	CALL IMAGE_SETVAL( NAXIS(1), NAXIS(2),
     :                     %VAL( POINTER_SCRATCH), STATUS)

	IF( STATUS .NE. SAI__OK) THEN

          CALL ERR_REP('ERR',
     :                'Error : IMAGE_CLEAR : after IMAGE_SETVAL',
     :                  STATUS )

	  RETURN

	END IF

* define start and end pixels for image plot

	IM_XST = REAL(XIMCEN) - (MAGNIF * (REAL(NX)/2.0)) + 1.0
	IM_YST = REAL(YIMCEN) - (MAGNIF * (REAL(NY)/2.0)) + 1.0
	IM_XEN = (IM_XST - 1) + MAGNIF * REAL(NX)
	IM_YEN = (IM_YST - 1) + MAGNIF * REAL(NY)

* plot data image after scaling

	CALL GCA( IM_XST,
     :	          IM_YST,
     :	          IM_XEN,
     :	          IM_YEN,
     :	          NAXIS( 1),
     :	          NAXIS( 2),
     :	          NAXIS( 1),
     :	          %VAL( POINTER_SCRATCH))

* flush buffer of residual output

	CALL SGS_FLUSH

* annul active scratch locator which also unmaps mapped data

	CALL NDF_ANNUL( LOCSR, STATUS)

	IF( STATUS .NE. SAI__OK) THEN

          CALL ERR_REP('ERR',
     :                'Error : IMAGE_CLEAR : after NDF_ANNULs',
     :                  STATUS )

	  RETURN

	END IF

	END
