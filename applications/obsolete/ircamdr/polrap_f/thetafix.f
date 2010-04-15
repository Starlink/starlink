*+  THETAFIX - fixes position angles so that between 0 and 180 only

	SUBROUTINE THETAFIX ( STATUS )

* Description :
*
* This routine ...
*
* Invocation : CALL THETAFIX ( STATUS )
*
* Parameters :
*
* Method :
*
* Bugs :
*
* None known.
*
* Authors : Colin Aspin ROE ( REVA::CAA )
*
* History :
*
*  18-05-1987 :  First implementation (REVA::CAA)
*  18-May-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*  11-AUG-1994  Changed input DIMS arguments to THETAFIXSUB (SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE		  ! no implicit typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'	    ! SSE global definitions
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'

* Status :

	INTEGER  STATUS		 ! global status parameter

* Local constants :

	INTEGER NDIMS		   ! input image dimensionality
	PARAMETER ( NDIMS = 2 )

* Local variables :

	INTEGER
     :  IDIMS( NDIMS ),  ! dimensions of input DATA_ARRAY
     :  ODIMS( NDIMS ),  ! dimensions of output DATA_ARRAY
     :  ACTDIM,               ! actual dimensions from NDF_DIM
     :  NELEMENTS,            ! number of elements mapped by NDF_MAP
     :  PNTRI,	    	! pointer to input DATA_ARRAY component
     :  PNTRO,	    	! pointer to output DATA_ARRAY component
     :  LOCI,           ! locator for input data structure
     :  LOCO            ! locator for output structure

*-
*    check status on entry - return if not o.k.
*
	IF ( STATUS .NE. SAI__OK ) THEN
	   RETURN
	END IF
*
* get locator to input IMAGE type data structure
*
	CALL GETINP( 'INPIC', LOCI, STATUS)
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL( LOCI, STATUS )
	  RETURN
	END IF
*
* map in its DATA_ARRAY component and get dimensions
*
        CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )
        CALL NDF_DIM( LOCI, NDIMS, IDIMS, ACTDIM, STATUS )

	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL( LOCI, STATUS )
	  RETURN
	END IF
*
* output the dimensions of the input image to the user
*
	CALL MSG_SETI( 'XDIM', IDIMS(1))
	CALL MSG_SETI( 'YDIM', IDIMS(2))

	CALL MSG_OUT( 'INPUT_DIMS',
     :                'Input image is ^XDIM by ^YDIM pixels',
     :	              STATUS)
*
* set output images dimensions
*
	ODIMS( 1) = IDIMS( 1)
	ODIMS( 2) = IDIMS( 2)
*
* create output image structures
*
	CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO,
     :                STATUS )
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL( LOCI, STATUS )
	  CALL NDF_ANNUL( LOCO, STATUS )
	  RETURN
	END IF
*
* map in a DATA_ARRAY components
*
        CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                  PNTRO, NELEMENTS, STATUS )

        IF ( STATUS .NE. SAI__OK ) THEN
	    CALL NDF_ANNUL( LOCI, STATUS )
	    CALL NDF_ANNUL( LOCO, STATUS )
	    RETURN
        END IF
*
* call subroutine to do the work
*
	CALL THETAFIXSUB( IDIMS(1), IDIMS(2), %VAL( PNTRI ), ODIMS(1),
     :	                  ODIMS(2), %VAL( PNTRO ), STATUS )
*
* tidy up the input/output structures
*
 	CALL NDF_ANNUL( LOCI, STATUS )
 	CALL NDF_ANNUL( LOCO, STATUS )

	END
