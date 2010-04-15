*+  ABSEP - creates two images from one IRCAM image with channels A and B
*           data separated

	SUBROUTINE ABSEP ( STATUS )

* Description :
*
* This routine ...
*
* Invocation : CALL ABSEP ( STATUS )
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
*  20-Apr-1994   Changed DAT and CMP calls to NDF (SKL@JACH)
*  11-Aug-1994   Changed COPYAB input DIMS arguments (SKL@JACH)
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
     :  IDIMS( NDIMS ),  ! dimensions of input DATA_ARRAY
     :  ODIMS( NDIMS ),  ! dimensions of output DATA_ARRAY
     :  PNTRI,	    	 ! pointer to input DATA_ARRAY component
     :  PNTROA,	    	 ! pointer to output DATA_ARRAY component
     :  PNTROB,	         ! pointer to output DATA_ARRAY component
     :  LOCI,            ! locator for input data structure
     :  LOCOA,           ! locator for output channel A data structure
     :  LOCOB,           ! locator for output channel B data structure
     :  NELEMENTS,       ! number of elements mapped by NDF_MAP
     :  ACTDIM           ! actual dimensions from NDF_DIM

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
     :		       PNTRI, NELEMENTS, STATUS )
        CALL NDF_DIM( LOCI, NDIMS, IDIMS, ACTDIM, STATUS)
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL( LOCI, STATUS )
	  RETURN
	END IF
*
* output the dimensions of the input image to the user
*
	CALL MSG_SETI( 'XDIM', IDIMS(1))
	CALL MSG_SETI( 'YDIM', IDIMS(2))

	CALL MSG_OUT( 'INPUT_DIMS', 'Input image is ^XDIM by ^YDIM pixels',
     :	              STATUS)
*
* set output images dimensions
*
	ODIMS( 1) = IDIMS( 1)/2
	ODIMS( 2) = IDIMS( 2)
*
* output the dimensions of the output image to the user
*
	CALL MSG_SETI( 'XDIM', ODIMS(1))
	CALL MSG_SETI( 'YDIM', ODIMS(2))

	CALL MSG_OUT( 'OUTPUT_DIMS', 'Output image is ^XDIM by ^YDIM pixels',
     :	              STATUS)

	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL( LOCI, STATUS )
	  RETURN
	END IF
*
* create output image structures
*
	CALL CREOUT( 'OUTPICA', 'OTITLE', NDIMS, ODIMS, LOCOA, STATUS )
	CALL CREOUT( 'OUTPICB', 'OTITLE', NDIMS, ODIMS, LOCOB, STATUS )
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL( LOCI, STATUS )
	  CALL NDF_ANNUL( LOCOA, STATUS )
	  CALL NDF_ANNUL( LOCOB, STATUS )
	  RETURN
	END IF
*
* map in a DATA_ARRAY components
*
	  CALL NDF_MAP( LOCOA, 'DATA', '_REAL', 'WRITE',
     :                   PNTROA, NELEMENTS, STATUS )
	  CALL NDF_MAP( LOCOB, 'DATA', '_REAL', 'WRITE',
     :                   PNTROB, NELEMENTS, STATUS )
	  IF ( STATUS .NE. SAI__OK ) THEN
	    CALL NDF_ANNUL( LOCI, STATUS )
	    CALL NDF_ANNUL( LOCOA, STATUS )
	    CALL NDF_ANNUL( LOCOB, STATUS )
	    RETURN
	  END IF
*
* put channel A and channel B data into the output arrays
*
	CALL COPYAB( IDIMS(1), IDIMS(2), %VAL( PNTRI ),
     :               ODIMS(1), ODIMS(2), %VAL( PNTROA ),
     :	             %VAL( PNTROB), STATUS )
*
* tidy up the input/output structures
*
	CALL NDF_ANNUL( LOCI, STATUS )
	CALL NDF_ANNUL( LOCOA, STATUS )
	CALL NDF_ANNUL( LOCOB, STATUS )

	END
