*+  ABCOM - creates an image from two image with channels A and B data

	SUBROUTINE ABCOM ( STATUS )

* Description :
*
* This routine ...
*
* Invocation : CALL ABCOM ( STATUS )
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
*  23-04-1986 :  First implementation (REVA::CAA)
*  20-Apr-1994   DAT and CMP calls changed to NDF (SKL@JACH)
*  11-Aug-1994   Changed input DIMS to COPYAB_COMB (SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE		  ! no implicit typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'	    ! SSE global definitions
        INCLUDE  'NDF_PAR'
        INCLUDE  'NDF_ERR'

* Status :

	INTEGER  STATUS		 ! global status parameter

* Local constants :

	INTEGER NDIMS		   ! input image dimensionality
	PARAMETER ( NDIMS = 2 )

* Local variables :

	INTEGER
     :  LOCIA,             ! locator for input data structure
     :  LOCIB,             ! locator for input data structure
     :  LOCO,              ! locator for output channel B data structure
     :  IDIMS_1( NDIMS ),  ! dimensions of input DATA_ARRAY
     :  IDIMS_2( NDIMS ),  ! dimensions of input DATA_ARRAY
     :  ODIMS( NDIMS ),    ! dimensions of output DATA_ARRAY
     :  PNTRIA,	     	   ! pointer to input DATA_ARRAY component
     :  PNTRIB,	    	   ! pointer to input DATA_ARRAY component
     :  PNTRO,    	   ! pointer to output DATA_ARRAY component
     :  NELEMENTS,         ! number of elements mapped by NDF_MAP
     :  ACTDIM             ! actual dimensions from NDF_DIM



*-
*    check status on entry - return if not o.k.
*
	IF ( STATUS .NE. SAI__OK ) THEN

	   RETURN

	END IF
*
* get locator to input IMAGE type data structure
*
	CALL GETINP( 'INPICA', LOCIA, STATUS)
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL( LOCIA, STATUS )
	  RETURN
	END IF

	CALL GETINP( 'INPICB', LOCIB, STATUS)
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL( LOCIA, STATUS )
	  CALL NDF_ANNUL( LOCIB, STATUS )
	  RETURN
	END IF
*
* map in its DATA_ARRAY component and get dimensions
*
	CALL NDF_MAP( LOCIA, 'DATA', '_REAL', 'READ',
     :		      PNTRIA, NELEMENTS, STATUS )

        CALL NDF_DIM( LOCIA, NDIMS, IDIMS_1, ACTDIM, STATUS)

	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL( LOCIA, STATUS )
	  CALL NDF_ANNUL( LOCIB, STATUS )
	  RETURN
	END IF

	CALL NDF_MAP( LOCIB, 'DATA', '_REAL', 'READ',
     :		      PNTRIB, NELEMENTS, STATUS )

        CALL NDF_DIM( LOCIB, NDIMS, IDIMS_2, ACTDIM, STATUS)

	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL( LOCIA, STATUS )
	  CALL NDF_ANNUL( LOCIB, STATUS )
	  RETURN
	END IF

*
* test that the input imagea are same size
*
	IF( IDIMS_1( 1) .NE. IDIMS_2( 1) .OR.
     :	    IDIMS_1( 2) .NE. IDIMS_2( 2)) THEN

	  CALL MSG_OUT( 'MESSAGE', 'Error, input images NOT same size ...',
     :	                STATUS)

	  CALL NDF_ANNUL( LOCIA, STATUS )
	  CALL NDF_ANNUL( LOCIB, STATUS )

	  RETURN

	END IF
*
* output the dimensions of the input image to the user
*
	CALL MSG_SETI( 'XDIM', IDIMS_1( 1))
	CALL MSG_SETI( 'YDIM', IDIMS_1( 2))

	CALL MSG_OUT( 'INPUT_DIMS', 'Input image is ^XDIM by ^YDIM pixels',
     :	              STATUS)
*
* set output images dimensions
*
	ODIMS( 1) = IDIMS_1( 1)*2
	ODIMS( 2) = IDIMS_1( 2)
*
* output the dimensions of the output image to the user
*
	CALL MSG_SETI( 'XDIM', ODIMS(1))
	CALL MSG_SETI( 'YDIM', ODIMS(2))

	CALL MSG_OUT( 'OUTPUT_DIMS', 'Output image is ^XDIM by ^YDIM pixels',
     :	              STATUS)

	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL( LOCIA, STATUS )
	  CALL NDF_ANNUL( LOCIB, STATUS )
	  RETURN
	END IF
*
* create output image structures
*
	CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO, STATUS )
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL( LOCIA, STATUS )
	  CALL NDF_ANNUL( LOCIB, STATUS )
	  CALL NDF_ANNUL( LOCO, STATUS )
	  RETURN
	END IF
*
* map in a DATA_ARRAY components
*
	CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                 PNTRO, NELEMENTS, STATUS )
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL( LOCIA, STATUS )
	  CALL NDF_ANNUL( LOCIB, STATUS )
	  CALL NDF_ANNUL( LOCO, STATUS )
	  RETURN
	END IF
*
* put channel A and channel B data into the output array
*
	CALL COPYAB_COMB( IDIMS_1(1), IDIMS_1(2), %VAL( PNTRIA),
     :                    %VAL( PNTRIB), ODIMS(1), ODIMS(2),
     :	                  %VAL( PNTRO), STATUS)
*
* tidy up the input/output structures
*
	CALL NDF_ANNUL( LOCIA, STATUS )
	CALL NDF_ANNUL( LOCIB, STATUS )
	CALL NDF_ANNUL( LOCO, STATUS )

	END
