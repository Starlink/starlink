	SUBROUTINE CLEAN( STATUS)

	IMPLICIT NONE

      INCLUDE 'SAE_PAR'       ! global SSE definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*     IMAGE (input)                The image
*     INTEGER*2 (NPIX,NLINES)

*     NPIX,NLINES (input)          The dimensions of the image
*     INTEGER

*     INVAL (input)                Invalid pixel flag for IMAGE
*     INTEGER

*     SCALE,ZERO (input)           Scale and zero level for IMAGE
*     REAL


	INTEGER STATUS          ! global status parameter
	INTEGER NDIMS           ! dimensionality of images
	PARAMETER ( NDIMS = 2 ) ! 2-d only
	INTEGER
     :  DIMS( NDIMS ),        ! dimensions of input/output DATA_ARRAYs
     :  ACTDIM,               ! actual dimensions from NDF_DIM
     :  NELEMENTS,            ! number of elements mapped by NDF_MAP
     :  PNTRO,                ! pointer to output DATA_ARRAY
     :  PNTRI,                !    "     " input      "
     :  LOCI,                 ! input IMAGE structure
     :  LOCO                  ! output IMAGE structure

	REAL SCALE, ZERO

*      check for error on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK ) THEN
	  RETURN
	END IF

*      get a locator to input IMAGE type data structure
	CALL GETINP( 'INPIC', LOCI, STATUS )

	IF( STATUS .EQ. SAI__OK ) THEN

	  CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )
          CALL NDF_DIM( LOCI, NDIMS, DIMS, ACTDIM, STATUS )

          CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, DIMS, LOCO, STATUS )

          IF( STATUS .EQ. SAI__OK ) THEN

            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                      PNTRO, NELEMENTS, STATUS )

	    SCALE = 1
	    ZERO = 0
	    CALL BUSINESS( %VAL( PNTRI), DIMS( 1), DIMS( 2), -1, SCALE,
     :	                     ZERO)

            CALL NDF_ANNUL( LOCO, STATUS )

          END IF

          CALL NDF_ANNUL(  LOCI, STATUS )

	END IF

	END

