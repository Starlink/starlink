*+  HOTSHOT - marks hot pixels with magic number for later removal

	SUBROUTINE HOTSHOT ( STATUS)

* Description :
*
* This routine ...
*
* Invocation : CALL HOTSHOT ( STATUS)
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
*  01-02-1987 :  First implementation (REVA::CAA)
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*  24-JUN-1994   Changed STR$ CHR_ (SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE		  ! no implicit typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'	    ! SSE global definitions
        INCLUDE  'NDF_PAR'
        INCLUDE  'NDF_ERR'
        INCLUDE  'CHR_ERR'

* Status :

	INTEGER  STATUS		 ! global status parameter

* Local constants :

	INTEGER NDIMS		   ! input image dimensionality
	PARAMETER ( NDIMS = 2)

* Local variables :

	INTEGER
     :    LOCI,         	! locator for input data structure
     :    LOCO,         	! locator for output data structure
     :	  IXCEN,
     :	  IYCEN,
     :	  IXSZ,
     :	  IYSZ,
     :	  BINX,			! number of pixels binned up in X
     :	  BINY,			! number of pixels binned up in Y
     :	  NUMHOT,
     :    IDIMS( NDIMS),	! dimensions of input DATA_ARRAY
     :    ODIMS( NDIMS),	! dimensions of output DATA_ARRAY
     :    ACTDIM,               ! actual dimensions from NDF_DIM
     :    NELEMENTS,            ! number of elements mapped by NDF_MAP
     :    PNTRI,		! pointer to input DATA_ARRAY component
     :    PNTRO,	    	! pointer to output DATA_ARRAY component
     :    LBND( 2 )             ! lower bounds of output array

        DATA LBND  / 1, 1 /

	REAL
     :	  SIGMA,
     :	  THRESH,
     :	  BADVAL

	CHARACTER*20
     :	  SETWHAT

	LOGICAL
     :	  WHOLE
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

*      map in its DATA_ARRAY component

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

*      set output images dimensions
	ODIMS( 1) = IDIMS( 1)
	ODIMS( 2) = IDIMS( 2)

*      create output data image structures

	CALL NDF_CREAT( 'OUTPIC', '_REAL', NDIMS, LBND, ODIMS,
     :                  LOCO, STATUS)

	IF ( STATUS .NE. SAI__OK) THEN
	  CALL NDF_ANNUL( LOCI, STATUS)
	  CALL NDF_ANNUL( LOCO, STATUS)
	  RETURN
	END IF

*      get the X and Y search factor and sigma level
	CALL PAR_GET0L( 'WHOLE', WHOLE, STATUS)
	IF( .NOT. WHOLE ) THEN
	  CALL PAR_GET0I( 'XCEN', IXCEN, STATUS)
	  CALL PAR_GET0I( 'YCEN', IYCEN, STATUS)
	  CALL PAR_GET0I( 'XSIZ', IXSZ, STATUS)
	  CALL PAR_GET0I( 'YSIZ', IYSZ, STATUS)
	  IF( ( IXCEN+INT(IXSZ/2.0+0.5)) .GT. IDIMS( 1) .OR.
     :	      ( IYCEN+INT(IYSZ/2.0+0.5)) .GT. IDIMS( 2) ) THEN
	    CALL MSG_OUT( 'MESS',
     :	      'ERROR, your search box EXCEEDS image area', STATUS)
	    CALL NDF_ANNUL( LOCI, STATUS)
	    CALL NDF_ANNUL( LOCO, STATUS)
	    RETURN
	  END IF
	END IF
	CALL PAR_GET0I( 'BINX', BINX, STATUS)
	CALL PAR_GET0I( 'BINY', BINY, STATUS)
	CALL PAR_GET0R( 'SIGMA', SIGMA, STATUS)
	CALL PAR_GET0R( 'THRESH', THRESH, STATUS)
	CALL PAR_GET0C( 'SETWHAT', SETWHAT, STATUS)
	CALL CHR_UCASE( SETWHAT )
	IF( SETWHAT( 1:1) .EQ. 'B') THEN
	  CALL PAR_GET0R( 'BADVAL', BADVAL, STATUS)
	ELSE
	  BADVAL = -1.00000E-20
	END IF
	IF ( STATUS .NE. SAI__OK) THEN
	  CALL NDF_ANNUL( LOCI, STATUS)
	  CALL NDF_ANNUL( LOCO, STATUS)
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
     :	    'Error, illegal search factors entered ... ^BINX , ^BINY',
     :	    STATUS)
	  CALL NDF_ANNUL( LOCI, STATUS)
	  CALL NDF_ANNUL( LOCO, STATUS)
	  RETURN
	END IF

*      map in data DATA_ARRAY components

        CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                  PNTRO, NELEMENTS, STATUS )

	IF( STATUS .NE. SAI__OK) THEN
	  CALL NDF_ANNUL( LOCI, STATUS)
	  CALL NDF_ANNUL( LOCO, STATUS)
	  RETURN
	END IF

*      call subroutine to do work
	CALL HOTSHOTSUB(
     :        IDIMS( 1), IDIMS( 2), %VAL( PNTRI), WHOLE, IXCEN,
     :	      IYCEN, IXSZ, IYSZ, BINX, BINY, SIGMA, THRESH,
     :	      SETWHAT, BADVAL, %VAL( PNTRO), NUMHOT, STATUS)

	CALL MSG_SETI( 'HOT', NUMHOT)
	CALL MSG_OUT( 'MESS', 'Number of HOT PIXELS = ^HOT', STATUS)

*      tidy up the input/output structures
	CALL NDF_ANNUL( LOCI, STATUS)
	CALL NDF_ANNUL( LOCO, STATUS)

	END
