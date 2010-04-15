*+  STCOADD - coadds N images into 1 image from input data structure

	SUBROUTINE STCOADD ( STATUS)

* Description :
*
* Invocation :
*
*     CALL STCOADD ( STATUS)
*
* Parameters :
*
* Method :
*
* Bugs :
*
*     None known.
*
* Authors :
*
*     Colin Aspin ROE ( REVA::CAA )
*
* History :
*
*     29-01-1987 : First implementation (HILO::CAA)
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     23-Jun-1994  Changed TYPE statements to ERR_REP (SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE			! no default typing allowed

* Global constants :

	INCLUDE 'SAE_PAR'		! SSE global definitions
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'

* Status :

	INTEGER  STATUS			! global status parameter

* Local Constants :

* Local variables :

     	INTEGER LOCINP			! input container file
        INTEGER LOCO   			! output data_array component
	INTEGER DIMS( 2)		! input data_array dimensions array
	INTEGER DIMSO( 2)		! output array dimensions array
        INTEGER NELEMENTS               ! number of elements mapped by NDF_MAP
	INTEGER ENDRAN			! end of range input
	INTEGER J			! loop counter
	INTEGER MAX_IMAGES		! maximum number of images to be coadded
	PARAMETER( MAX_IMAGES = 2000)	! define maximum number of images
	INTEGER NDIMS			! number of dimensions variable
	INTEGER NUMBER_IMAGE( 2000)	! the number of each image to be coadded
	INTEGER NUMBER_IMAGES		! total number of images to be coadded
	INTEGER PNTRDA			! pointer for input data_array image
	INTEGER PNTRO			! pointer for output data_array image
	INTEGER STARTRAN		! start of range input

	LOGICAL MORE			! defines more looping for image nos.

	CHARACTER*80 INPOPT		! option to input a range of numbers
					! or individual ones


*-
*      check status on entry - return if not o.k.

	IF ( STATUS .NE. SAI__OK ) THEN

	   RETURN

	END IF

*      prompt user for input image and associate it

	CALL NDF_ASSOC( 'INPIC', 'READ', LOCINP, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after ndf_assoc ...', STATUS )
	  return
	end if

*      ask user for a range or numbers or individual ones

	CALL PAR_GET0C( 'INPOPT', INPOPT, STATUS)

*      test the value of the input option just got and act

	IF( INPOPT .EQ. 'RANGE') THEN

*        get the start and end of the range to be coadded

	  CALL PAR_GET0I( 'STARTRAN', STARTRAN, STATUS)
	  CALL PAR_GET0I( 'ENDRAN', ENDRAN, STATUS)

*        calculate the number of images to be coadded and the number themselves

	  NUMBER_IMAGES = ENDRAN - STARTRAN + 1

	  DO J = 1, NUMBER_IMAGES

	    NUMBER_IMAGE( J) = STARTRAN + J - 1

	  END DO

*      here if want to input individual number rather than a range of values

	ELSE

*        loop to get all the input image numbers to be coadded

	  MORE = .TRUE.
	  NUMBER_IMAGES = 0

	  DO WHILE ( MORE)

	    NUMBER_IMAGES = NUMBER_IMAGES + 1

*          read the latest image number to be coadded

	    CALL PAR_GET0I( 'NUMBER', NUMBER_IMAGE( NUMBER_IMAGES),
     :                       STATUS)

*          cancel the current association with this parameter

	    CALL PAR_CANCL( 'NUMBER', STATUS)

*          suggest a new default of the one after the last input obs number

	    CALL PAR_DEF0I( 'NUMBER', NUMBER_IMAGE( NUMBER_IMAGES)+1,
     :                       STATUS)

*          test if number of input images greater than image number array size

	    IF( NUMBER_IMAGES .GE. MAX_IMAGES) THEN

	      CALL MSG_SETI( 'NUM', MAX_IMAGES)
	      CALL MSG_OUT( 'MESSAGE',
     : 'Sorry, I cannot handle any more input images. ^NUM is my limit',
     :	      STATUS)

	      MORE = .FALSE.

	    ELSE

*            test if user input number 0 or less and stop if did

	      IF( NUMBER_IMAGE( NUMBER_IMAGES) .LT. 1) THEN

	        MORE = .FALSE.

	        NUMBER_IMAGES = NUMBER_IMAGES - 1

	      END IF

	    END IF

	    if( status .ne. sai__ok) then
              CALL ERR_REP('ERR', 'Error end of input loop ...',
     :                      STATUS )
	      return
	    end if

	  END DO

	END IF

*      message to user to tell how many input obs elements input

	CALL MSG_SETI( 'NUM', NUMBER_IMAGES)
	CALL MSG_OUT( 'MESSAGE',
     :	  'O.K. I am about to coadd ^NUM images ...',
     :	  STATUS)


*      loop to scan through all the specified input observations to be coadded

	DO J = 1, NUMBER_IMAGES

*        set the dimension parameter for the cell required

	  DIMS( 1) = NUMBER_IMAGE( J)
	  DIMS( 2) = 0

*        get the shape/size of the data_array element

	  CALL NDF_DIM( LOCINP, 2, DIMS, NDIMS, STATUS)
	  if( status .ne. sai__ok) then
            CALL ERR_REP('ERR', 'Error ndf_dim image ...', STATUS )
	    return
	  end if

*        set the size parameter for the output image from first set of data

	  IF( J .EQ. 1) THEN

	    CALL MSG_SETI( 'NX', DIMS( 1))
	    CALL MSG_SETI( 'NY', DIMS( 2))
	    CALL MSG_OUT( 'MESSAGE',
     :	      'Input images are of size ^NX by ^NY',
     :	      STATUS)

	    DIMSO( 1) = DIMS( 1)
	    DIMSO( 2) = DIMS( 2)

*          get the name of the output image to contain coadded data

	    CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, DIMSO, LOCO,
     :                    STATUS )
	    if( status .ne. sai__ok) then
              CALL ERR_REP('ERR', 'Error after creout ...', STATUS )
  	      return
	    end if

*          map the output image data_array component

            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                    PNTRO, NELEMENTS, STATUS )

	    if( status .ne. sai__ok) then
              CALL ERR_REP('ERR',
     :                     'Error after ndf_map output image ...',
     :                      STATUS )
	      return
	    end if

*          set the output array to all zeros

	    CALL STCOADD_ZERO( DIMS( 1), DIMS( 2), %VAL( PNTRO))

	  END IF

*        message to user telling which image being processed

	  CALL MSG_SETI( 'NUM', NUMBER_IMAGE( J))
	  CALL MSG_OUT( 'MESSAGE',
     :	    'Coadding observation image number ^NUM ...',
     :	    STATUS)

*        map in the data in this DATA_ARRAY element

          CALL NDF_MAP( LOCINP, 'DATA', '_REAL', 'READ',
     :                  PNTRDA, NELEMENTS, STATUS )

	  if( status .ne. sai__ok) then
            CALL ERR_REP('ERR',
     :                    'Error after ndf_map input image ...',
     :                    STATUS )
	    return
	  end if

*        call subroutine to coadd the data into the output image

	  CALL STCOADD_IMAGE( DIMS( 1), DIMS( 2), %VAL( PNTRDA),
     :                        %VAL( PNTRO))

	END DO


*      unmap the output data image

	CALL NDF_ANNUL( LOCO, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after ndf_annul output ...',
     :                  STATUS )
	  return
	end if

*      annul the locators still active

	CALL NDF_ANNUL( LOCINP, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR', 'Error after ndf_annul input ...',
     :                  STATUS )
	  return
	end if

	END
