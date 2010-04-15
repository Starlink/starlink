*+ COLCYCLE - cycles a colour table n times

	SUBROUTINE COLCYCLE ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL COLCYCLE ( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*           Image to be analysed
*     NCYCLES =  INTEGER( READ )
*           number of cycles of input colour table
*     OUTPIC =  IMAGE( WRITE )
*           output image
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
*    Type definitions :

	IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

	INCLUDE  'SAE_PAR'          ! SSE global definitions
        INCLUDE  'NDF_PAR'
        INCLUDE  'NDF_ERR'

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
     :    PNTRI,          ! pointer to input DATA_ARRAY component
     :    PNTRO,          ! pointer to output DATA_ARRAY component
     :    NCYCLES         ! number of cycles


*-
*      check status on entry - return if not o.k.

	IF ( STATUS .NE. SAI__OK ) THEN

	  RETURN

	END IF

*      get locator to the input IMAGE type data structure

	CALL GETINP( 'INPIC', LOCI, STATUS )

*      if no error so far then continue

	IF ( STATUS .EQ. SAI__OK ) THEN

*        map in its DATA_ARRAY component and get dimensions

          CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )
          CALL NDF_DIM( LOCI, NDIMS, DIMS, ACTDIM, STATUS )

*        test size of input image for valid colour table

	  IF( DIMS( 1) .EQ. 3 .AND. DIMS( 2) .EQ. 256) THEN

*          output the dimensions of the image to the user

            CALL MSG_SETI( 'XDIM', DIMS( 1 ) )
            CALL MSG_SETI( 'YDIM', DIMS( 2 ) )

            CALL MSG_OUT( 'INPUT_DIMS',
     :             'Image is ^XDIM by ^YDIM pixels', STATUS )

	  ELSE

            CALL MSG_SETI( 'XDIM', DIMS( 1 ) )
            CALL MSG_SETI( 'YDIM', DIMS( 2 ) )

	    CALL MSG_OUT( 'MESSAGE',
     :	           'Error, Input image dimensions invalid ^XDIM, ^YDIM',
     :             STATUS)
	    CALL MSG_OUT( 'MESSAGE',
     :	           'Colour table should be 3 by 256 in size ...',
     :	           STATUS)

            CALL NDF_ANNUL( LOCI, STATUS )

	    RETURN

	  END IF

*        get the number of cycles required in output colour table

	  CALL PAR_GET0I( 'NCYCLES', NCYCLES, STATUS)

*        create output image

	  CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, DIMS, LOCO, STATUS)

*        map in its DATA_ARRAY component

          CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                  PNTRO, NELEMENTS, STATUS )

          IF ( STATUS .EQ. SAI__OK ) THEN

*          now call the subroutine to do the work

            CALL COLCYCLESUB( DIMS, %VAL( PNTRI ), %VAL( PNTRO),
     :	                      NCYCLES, STATUS )

*          end of if-no-error-then-call-subroutine check

          END IF

*        tidy up the input data structure

          CALL NDF_ANNUL( LOCI, STATUS )
          CALL NDF_ANNUL( LOCO, STATUS )

*        end of if-error-after-getting-input-image-locator check

	END IF

	END
