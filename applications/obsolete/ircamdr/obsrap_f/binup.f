*+  BINUP - AVERAGES n by n pixels in image to increase S/N

	SUBROUTINE BINUP ( STATUS)

* Description :
*
* This routine ...
*
* Invocation : CALL BINUP ( STATUS)
*
* Parameters :
*
* Method :
*
* Bugs :
*
* None known.
*
* Authors : Colin Aspin ROE ( REVA::CAA)
*
* History :
*
*  01-02-1987 :  First implementation (REVA::CAA)
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
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
	PARAMETER ( NDIMS = 2)

* Local variables :

	INTEGER
     :    LOCI,	                ! locator for input data structure
     :    LOCO,	                ! locator for output data structure
     :    LOCO2,        	! locator for output data structure
     :	  BINX,			! number of pixels binned up in X
     :	  BINY,			! number of pixels binned up in Y
     :    IDIMS( NDIMS),	! dimensions of input DATA_ARRAY
     :    ODIMS( NDIMS),	! dimensions of output DATA_ARRAY
     :    ODIMS2( NDIMS),	! dimensions of output DATA_ARRAY
     :    ACTDIM,               ! actual dimensions from NDF_DIM
     :    NELEMENTS,            ! number of elements mapped by NDF_MAP
     :    PNTRI,		! pointer to input DATA_ARRAY component
     :    PNTRO,	    	! pointer to output DATA_ARRAY component
     :    PNTRO2,	    	! pointer to output DATA_ARRAY component
     :    LBND( 2 )             ! lower bounds output array

        DATA LBND / 1, 1 /


*-

*      check status on entry - return if not o.k.

	IF ( STATUS .NE. SAI__OK) THEN
	   RETURN
	END IF

*      get locator to input IMAGE type data structure

	CALL NDF_ASSOC( 'INPIC', 'READ', LOCI, STATUS)
	IF ( STATUS .NE. SAI__OK) THEN
	  CALL NDF_ANNUL( LOCI, STATUS)
	  RETURN
	END IF

*      map in its DATA_ARRAY component and get dimensions

        CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )
        CALL NDF_DIM( LOCI, NDIMS, IDIMS, ACTDIM, STATUS )

	IF ( STATUS .NE. SAI__OK) THEN
	  CALL NDF_ANNUL( LOCI, STATUS)
	  RETURN
	END IF

*      output the dimensions of the input image to the user

	CALL MSG_SETI( 'XDIM', IDIMS(1))
	CALL MSG_SETI( 'YDIM', IDIMS(2))

	CALL MSG_OUT( 'INPUT_DIMS', 'Input image is ^XDIM by ^YDIM pixels',
     :	STATUS)

*      get the X and Y binning factor from user

	CALL PAR_GET0I( 'BINX', BINX, STATUS)
	CALL PAR_GET0I( 'BINY', BINY, STATUS)
	IF ( STATUS .NE. SAI__OK) THEN
	  CALL NDF_ANNUL( LOCI, STATUS)
	  RETURN
	END IF

*      test the binning factors for legal values

	IF( BINX .GT. 0 .AND. BINX .LE. 20 .AND.
     :	    BINY .GT. 0 .AND. BINY .LE. 20) THEN
	ELSE

*        message to user telling of bad bin values input

	  CALL MSG_SETI( 'BINX', BINX)
	  CALL MSG_SETI( 'BINY', BINY)
	  CALL MSG_OUT( 'MESSAGE',
     :	    'Error, illegal binning factors entered ... ^BINX , ^BINY',
     :	    STATUS)

	  CALL NDF_ANNUL( LOCI, STATUS)
	  RETURN
	END IF

*      set output images dimensions

	ODIMS( 1) = IDIMS( 1)/BINX
	ODIMS( 2) = IDIMS( 2)/BINY
	ODIMS2( 1) = IDIMS( 1)/BINX
	ODIMS2( 2) = IDIMS( 2)/BINY

*      output the dimensions of the output image to the user

	CALL MSG_SETI( 'XDIM', ODIMS(1))
	CALL MSG_SETI( 'YDIM', ODIMS(2))
	CALL MSG_OUT( 'OUTPUT_DIMS',
     :                'Output images are ^XDIM by ^YDIM pixels',
     :	              STATUS)

	IF ( STATUS .NE. SAI__OK) THEN
	  CALL NDF_ANNUL( LOCI, STATUS)
	  RETURN
	END IF

*      create output image structures

	CALL NDF_CREAT( 'OUTPIC', '_REAL', 2, LBND, ODIMS, LOCO,
     :                   STATUS )
	IF ( STATUS .NE. SAI__OK) THEN
	  CALL NDF_ANNUL( LOCI, STATUS)
	  CALL NDF_ANNUL( LOCO, STATUS)
	  RETURN
	END IF

	CALL NDF_CREAT( 'OUTPIC2', '_REAL', 2, LBND, ODIMS2, LOCO2,
     :                   STATUS )
	IF ( STATUS .NE. SAI__OK) THEN
	  CALL NDF_ANNUL( LOCI, STATUS)
	  CALL NDF_ANNUL( LOCO2, STATUS)
	  CALL NDF_ANNUL( LOCO, STATUS)
	  RETURN
	END IF

*      map in DATA_ARRAY components

        CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                PNTRO, NELEMENTS, STATUS )
	IF( STATUS .NE. SAI__OK) THEN
	  CALL NDF_ANNUL( LOCO, STATUS)
	  CALL NDF_ANNUL( LOCI, STATUS)
	  RETURN
	END IF

        CALL NDF_MAP( LOCO2, 'DATA', '_REAL', 'WRITE',
     :                PNTRO2, NELEMENTS, STATUS )

	IF( STATUS .NE. SAI__OK) THEN
	  CALL NDF_ANNUL( LOCI, STATUS)
	  CALL NDF_ANNUL( LOCO, STATUS)
	  CALL NDF_ANNUL( LOCO2, STATUS)
	  RETURN
	END IF

*      call subroutine to do binning and fill output image

	CALL BINUP2D( IDIMS( 1), IDIMS( 2), %VAL( PNTRI), BINX, BINY,
     :	              ODIMS( 1), ODIMS( 2), %VAL( PNTRO), %VAL( PNTRO2),
     :	              STATUS)

*      tidy up the input/output structures

	CALL NDF_ANNUL( LOCI, STATUS)
	CALL NDF_ANNUL( LOCO, STATUS)
	CALL NDF_ANNUL( LOCO2, STATUS)

	END
