*+  APPLYMASK - applies bad pixel mask by setting bad pixels to magic number

      SUBROUTINE APPLYMASK ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL APPLYMASK ( STATUS )
*
*    Parameters :
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get input image data structure
*     If no error so far then
*        Map the input DATA_ARRAY component
*        Create output mask image
*        Map the output DATA_ARRAY component
*        If no error so far then
*           Get sigma of cut for bad pixels
*           If no errors then
*              Scan through input image setting pixels outside sigma to bad
*           Endif
*        Endif
*        Tidy up input structure
*     Endif
*     End
*
*    Authors :
*
*     Colin Aspin (JACH::CAA)
*
*    History :
*
*     14/08/1989 : Original version  (JACH::CAA)
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE           ! no implicit typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'       ! global SSE definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*    Status :

      INTEGER STATUS          ! global status parameter

*    Local constants :

      INTEGER NDIMS           ! dimensionality of images
      PARAMETER ( NDIMS = 2 ) ! 2-d only

*    Local variables :

      INTEGER
     :  LOCI,                 ! locator for input IMAGE structure
     :  LOCI2,                ! locator for input IMAGE structure
     :  LOCO,                 ! locator for output IMAGE structure
     :  DIMS( NDIMS ),        ! dimensions of input DATA_ARRAYs
     :  DIMS2( NDIMS ),       ! dimensions of input DATA_ARRAYs
     :  ODIMS( NDIMS ),       ! dimensions of output DATA_ARRAYs
     :  ACTDIM,               ! actual dimensions from NDF_DIM
     :  NELEMENTS,            ! number of elements mapped by NDF_MAP
     :  PNTRI,                !    "     " input      "
     :  PNTRI2,               !    "     " input      "
     :  PNTRO,                !    "     " output     "
     :	NUMPIX

      REAL
     :	MAGICNO                 ! magic number

*-
*      check for error on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK ) THEN
	  RETURN
	END IF

*      get a locator to input IMAGE type data structure
	CALL GETINP( 'INPIC', LOCI, STATUS )
	IF( STATUS .NE. SAI__OK ) THEN
	  RETURN
	END IF

*      map input DATA_ARRAY component and get dimensions
        CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                PNTRI, NELEMENTS, STATUS )
        CALL NDF_DIM( LOCI, NDIMS, DIMS, ACTDIM, STATUS )

	IF( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL(  LOCI, STATUS )
	  RETURN
	END IF

*      get a locator to input IMAGE type data structure
	CALL GETINP( 'INPIC2', LOCI2, STATUS )
	IF( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL(  LOCI, STATUS )
	  RETURN
	END IF

*      map input DATA_ARRAY component
        CALL NDF_MAP( LOCI2, 'DATA', '_REAL', 'READ',
     :                PNTRI2, NELEMENTS, STATUS )
        CALL NDF_DIM( LOCI, NDIMS, DIMS2, ACTDIM, STATUS )

	IF( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL(  LOCI, STATUS )
	  CALL NDF_ANNUL(  LOCI2, STATUS )
	  RETURN
	END IF

	IF( ( DIMS( 1) .NE. DIMS2( 1)) .OR.
     :	    ( DIMS( 2) .NE. DIMS2( 2))) THEN
	  CALL MSG_OUT( 'ERR', 'ERROR, image and mask not same size',
     :	                STATUS)
	  CALL NDF_ANNUL(  LOCI, STATUS )
	  CALL NDF_ANNUL(  LOCI2, STATUS )
	END IF

*      set output image dimensions
	ODIMS( 1) = DIMS( 1)
	ODIMS( 2) = DIMS( 2)

*      create output array
	CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO, STATUS )

	IF( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL(  LOCI, STATUS )
	  CALL NDF_ANNUL(  LOCI2, STATUS )
	  RETURN
	END IF

*      map output DATA_ARRAY component
        CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                PNTRO, NELEMENTS, STATUS )

	IF( STATUS .NE. SAI__OK ) THEN
	  CALL NDF_ANNUL( LOCI, STATUS )
	  CALL NDF_ANNUL( LOCI2, STATUS )
	  CALL NDF_ANNUL( LOCO, STATUS )
	  RETURN
	END IF

*      get magic number
	CALL PAR_GET0R( 'MAGICNO', MAGICNO, STATUS)

*      pass everything to the work routine
	CALL APPLYMASKSUB( DIMS( 1), DIMS( 2), %VAL( PNTRI),
     :                     %VAL( PNTRI2), %VAL( PNTRO), MAGICNO,
     :                     NUMPIX, STATUS)

*      tell user the bad news...
	CALL MSG_SETR( 'MAG', MAGICNO)
	CALL MSG_SETI( 'NP', NUMPIX)
	CALL MSG_OUT( 'MESS',
     :	  'Number of pixels masked to ^MAG in image = ^NP', STATUS)

*      annul locators and unmap

        CALL NDF_ANNUL( LOCI, STATUS )
	CALL NDF_ANNUL( LOCI2, STATUS )
	CALL NDF_ANNUL( LOCO, STATUS )

	END
