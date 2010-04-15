*+ MANYCOL - puts several colour tables in one table

	SUBROUTINE MANYCOL ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL MANYCOL ( STATUS )
*
*    Parameters :
*
*    Method :
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Colin Aspin UoE ( UKTH::CAA )
*
*    History :
*
*     18-12/1987 : First implementation (UKTH::CAA)
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*
*
*    Type definitions :

	IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

	INCLUDE  'SAE_PAR'          ! SSE global definitions
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'

*    Status :

	INTEGER  STATUS             ! global status parameter

*    Local constants :

	INTEGER NDIMS               ! input image dimensionality
	PARAMETER ( NDIMS = 2 )     ! 2-d images only

*    Local variables :

	INTEGER
     :    LOCI,           ! input data structure
     :    LOCO,           ! OUTPUT data structure
     :    DIMS( NDIMS ),  ! dimensions of input DATA_ARRAY
     :    ACTDIM,         ! actual dimensions from NDF_DIM
     :    NELEMENTS,      ! number of elements mapped by NDF_MAP
     :	  J,
     :    PNTRI,          ! pointer to input DATA_ARRAY component
     :    PNTRO,          ! pointer to output DATA_ARRAY component
     :    NCOLS           ! number of colour tables combined

	REAL
     :    INPVAL( 3, 256, 10)  ! colour values


* zzzz
*-
*      check status on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK ) THEN
	  RETURN
	END IF

*      get the number of input colour tables
	CALL PAR_GET0I( 'NUMCOL', NCOLS, STATUS)

*      loop to get input colour tables
	J = 1
	DO WHILE ( J .LE. NCOLS)

*        tell user which colour table is being asked for
	  CALL MSG_SETI( 'NUM', J)
	  CALL MSG_OUT( 'MESSAGE', 'Input COLOUR TABLE number ^NUM',
     :                   STATUS)

*        get locator to the input IMAGE type data structure
	  CALL GETINP( 'INPIC', LOCI, STATUS)

	  IF ( STATUS .EQ. SAI__OK ) THEN

*          map in its DATA_ARRAY component and get dimensions
           CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                   PNTRI, NELEMENTS, STATUS )
           CALL NDF_DIM( LOCI, NDIMS, DIMS, ACTDIM, STATUS )

	    IF ( STATUS .EQ. SAI__OK ) THEN

*            test size of input image for valid colour table
	      IF( DIMS( 1) .EQ. 3 .AND. DIMS( 2) .EQ. 256) THEN
                CALL MSG_SETI( 'XDIM', DIMS( 1 ) )
                CALL MSG_SETI( 'YDIM', DIMS( 2 ) )
                CALL MSG_OUT( 'INPUT_DIMS',
     :               'Image is ^XDIM by ^YDIM pixels', STATUS )
	      ELSE
                CALL MSG_SETI( 'XDIM', DIMS( 1 ) )
                CALL MSG_SETI( 'YDIM', DIMS( 2 ) )
	        CALL MSG_OUT( 'MESSAGE',
     :	          'Error, Input image dimensions invalid ^XDIM, ^YDIM',
     :             STATUS)
	        CALL MSG_OUT( 'MESSAGE',
     :	             'Colour table should be 3 by 256 in size ...',
     :               STATUS)
	        GOTO 100
	      END IF

*            put the current colour table into the work array
	      CALL MANYCOL_PUTCOL( NCOLS, J, %VAL( PNTRI), INPVAL)

*            increment counting variable j
	      J = J + 1

*            release the current input image
  100	      CONTINUE

	    END IF

            CALL NDF_ANNUL( LOCI, STATUS )

	  END IF

	  CALL PAR_CANCL( 'INPIC', STATUS)

	END DO

*      create output image
	CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, DIMS, LOCO, STATUS)

*      map in its DATA_ARRAY component
        CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                PNTRO, NELEMENTS, STATUS )

        IF ( STATUS .EQ. SAI__OK ) THEN

*        now call the subroutine to do the work
          CALL MANYCOLSUB( DIMS, INPVAL, %VAL( PNTRO), NCOLS, STATUS )

        END IF

*      tidy up the input data structure
        CALL NDF_ANNUL( LOCO, STATUS )

	END
