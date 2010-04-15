
*+  STEPIM - steps an image and creates another image

	SUBROUTINE STEPIM ( STATUS )

*    Description :

*    Parameters :

*    Method :

*    Authors :

*    History :
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     11-AUG-1994  Changed STEPIMSUB input DIMS arguments (SKL@JACH)
*
*    Type Definitions :

	IMPLICIT NONE

*    Global constants :

	INCLUDE 'SAE_PAR'
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'

*    Status :

	INTEGER STATUS

*    Local constants :

	INTEGER NDIM

	PARAMETER ( NDIM = 2 ) ! dimensionality of input/output images

*    Local variables :

	INTEGER
     :    LOCI,         ! locator for input data structure
     :    LOCO,         ! locator for output data structure
     :    DIMS( NDIM ), ! dimensions of the input/output DATA_ARRAYs
     :    ACTDIM,       ! actual dimensions from NDF_DIM
     :    NELEMENTS,    ! number of elements mapped by NDF_MAP
     :    PNTRI,        ! pointer to : input DATA_ARRAY
     :    PNTRO,        !            : output DATA_ARRAY
     :	  STEPNUM       ! number of steps in data

	REAL
     :	  STEPBASE,     ! base level of step, below ?
     :	  STEPINT,      ! interval between steps, above ?
     :	  XMAX,         ! value for ABOVE TOP
     :	  XMIN          ! value for BELOW BASE

*-

*      get locator to input IMAGE type data structure

	CALL GETINP( 'INPIC', LOCI, STATUS )

*      check for error

	IF( STATUS .EQ. SAI__OK ) THEN

*        map input DATA_ARRAY component and get dimensions

          CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )
          CALL NDF_DIM( LOCI, NDIM, DIMS, ACTDIM, STATUS )

*        tell user image size

	  CALL MSG_SETI( 'NX', DIMS( 1))
	  CALL MSG_SETI( 'NY', DIMS( 2))
	  CALL MSG_OUT( 'MESS',
     :     'Image is ^NX by ^NY pixels in size', STATUS )

*        get step number, base and increment and max min

	  CALL PAR_GET0R( 'STEPNUM', STEPNUM, STATUS)
	  CALL PAR_GET0R( 'STEPBASE', STEPBASE, STATUS)
	  CALL PAR_GET0R( 'STEPINT', STEPINT, STATUS)
	  CALL PAR_GET0R( 'ABOVEMAX', XMAX, STATUS)
	  CALL PAR_GET0R( 'BELOWMIN', XMIN, STATUS)

*        create the output IMAGE structure and get a title for it

	  CALL CREOUT( 'OUTPIC', 'OTITLE', NDIM, DIMS, LOCO, STATUS )

*        check for error

	  IF( STATUS .EQ. SAI__OK ) THEN

*          map output DATA_ARRAY component

            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                    PNTRO, NELEMENTS, STATUS )

*          call subroutine to do work

	    CALL STEPIMSUB( DIMS(1), DIMS(2), %VAL( PNTRI),
     :	                    %VAL( PNTRO), STEPNUM, STEPBASE, STEPINT,
     :                      XMAX, XMIN, STATUS)

*          tidy up output structure and workspace

	    CALL NDF_ANNUL(  LOCO, STATUS )

	  END IF

*       tidy up input structure

	  CALL NDF_ANNUL(  LOCI, STATUS )

	END IF

	END
