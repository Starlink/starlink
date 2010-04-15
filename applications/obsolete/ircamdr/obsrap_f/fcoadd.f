*+  FCOADD - coadds N separate images into 1 image

	SUBROUTINE FCOADD ( STATUS)

* Description :
*
* Invocation :
*
*     CALL FCOADD ( STATUS)
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
*     03-02-1987 : First implementation (HILO::CAA)
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     23-Jun-1994  Changed TYPE to ERR_REP,
*                  STR$ and LIB$ to CHR_ (SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE			! no default typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'		! SSE global definitions
        INCLUDE  'NDF_PAR'
        INCLUDE  'NDF_ERR'
        INCLUDE  'CHR_ERR'

* Status :

	INTEGER  STATUS			! global status parameter

* Local Constants :

* Local variables :

        INTEGER LOCI      		! input container file
     	INTEGER LOCO			! output data_array component
	INTEGER CHARLEN			! length of character version of number
	INTEGER DIMS( 2)		! input data_array dimensions array
	INTEGER DIMSO( 2)		! output array dimensions array
        INTEGER ACTDIM                  ! actual dimensions from NDF_DIM
        INTEGER NELEMENTS               ! number of elements mapped by NDF_MAP
	INTEGER INCODE			! code of input image parameter
	INTEGER J			! loop counter
	INTEGER MAX_IMAGES		! maximum number of images to be coadded
	PARAMETER( MAX_IMAGES = 2000)	! define maximum number of images
	INTEGER NDIMS			! number of dimensions variable
	PARAMETER( NDIMS=2 )            ! two dimensions only
	INTEGER NUMBER_IMAGE( 2000)	! the number of each image to be coadded
	INTEGER NUMBER_IMAGES		! total number of images to be coadded
	INTEGER PNTRDA			! pointer for input data_array image
	INTEGER PNTRO			! pointer for output data_array image
	INTEGER PRELEN			! length of prefix name

	LOGICAL MORE			! defines more looping for image nos.

	CHARACTER*80 CHARNUM		! character version of filenumber
	CHARACTER*80 FILESORT		! sort of input requested by user
	CHARACTER*80 FNAME		! filename of image to be coadded
	CHARACTER*80 NAME( 2000)	! names of input files to be coadded
	CHARACTER*80 PREFIX		! prefix to image name


*-
*      check status on entry - return if not o.k.

	IF ( STATUS .NE. SAI__OK ) THEN

	   RETURN

	END IF

*      ask user if he wants to input a filename prefix and a list of numbers or
*      a list of random filenames

	CALL PAR_GET0C( 'FILESORT', FILESORT, STATUS)

*      convert the input to upper case

	CALL CHR_UCASE( FILESORT )

*      input a filename prefix and a list of numbers

	IF( FILESORT .EQ. 'A') THEN

*        prompt user for input image prefix

	  CALL PAR_GET0C( 'PREFIX', PREFIX, STATUS)

*        convert the input to upper case

*	  CALL CHR_UCASE( PREFIX )

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
     :                      STATUS)

*          test if number of input images greater than image number array size

	    IF( NUMBER_IMAGES .GE. MAX_IMAGES) THEN

	      CALL MSG_SETI( 'NUM', MAX_IMAGES)
	      CALL MSG_OUT( 'MESSAGE',
     :	      'I cannot handle any more input images. ^NUM is my limit',
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
	      CALL ERR_REP('ERR', 'Error after end of input loop ...',
     :                      STATUS )
	      return
	    end if

	  END DO

*        message to user to tell how many input obs elements input

	  CALL MSG_SETI( 'NUM', NUMBER_IMAGES)
	  CALL MSG_SETC( 'P', PREFIX)
	  CALL MSG_OUT( 'MESSAGE',
     :	  'I am about to coadd ^NUM images all starting with prefix ^P',
     :	    STATUS)

*        get the code for the input image parameter

	  CALL SUBPAR_FINDPAR( 'INPIC', INCODE, STATUS)

*        loop to scan through all the specified input filenames to be coadded

	  DO J = 1, NUMBER_IMAGES

*          create the name of the file to be coadded, convert number to
*          characters

            CALL CHR_ITOC( NUMBER_IMAGE( J), CHARNUM, CHARLEN )

*          concatenate the prefix with the number to form the full name

            CALL CHR_CLEAN( PREFIX )
            PRELEN = 0
	    CALL CHR_APPND( PREFIX, PREFIX, PRELEN)

	    FNAME = PREFIX( 1:PRELEN) // CHARNUM( 1:CHARLEN)
            CALL CHR_CLEAN( FNAME )
            PRELEN = 0
	    CALL CHR_APPND( FNAME, FNAME, PRELEN)

*	    suggest this name as the new default for the input image

	    CALL SUBPAR_PUTNAME( INCODE, FNAME( 1:PRELEN), STATUS)

*          associate the input image

	    CALL NDF_ASSOC( 'INPIC', 'READ', LOCI, STATUS)

*          get the shape/size of the data_array element

	    CALL NDF_DIM( LOCI, NDIMS, DIMS, ACTDIM, STATUS)

	    IF( STATUS .NE. SAI__OK ) THEN
	        RETURN
            END IF

*          set the size parameter for the output image from first set of data

	    IF( J .EQ. 1) THEN

	      CALL MSG_SETI( 'NX', DIMS( 1))
	      CALL MSG_SETI( 'NY', DIMS( 2))
	      CALL MSG_OUT( 'MESSAGE',
     :	        'Images are of size ^NX by ^NY',
     :	        STATUS)

	      DIMSO( 1) = DIMS( 1)
	      DIMSO( 2) = DIMS( 2)

*            get the name of the output image to contain coadded data

	      CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, DIMSO, LOCO,
     :                      STATUS )

	      if( status .ne. sai__ok) then
                CALL ERR_REP('ERR', 'Error after creout ...',
     :                        STATUS )
	        return
	      end if

*            map the output image data_array component

              CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                      PNTRO, NELEMENTS, STATUS )

	      if( status .ne. sai__ok) then
                CALL ERR_REP( 'ERR',
     :                        'Error after ndf_map output image ...',
     :                        STATUS )
	        return
	      end if

*            set the output array to all zeros

	      CALL STCOADD_ZERO( DIMS( 1), DIMS( 2), %VAL( PNTRO))

	    ELSE

	    END IF

*          message to user telling which image being processed

	    CALL MSG_SETC( 'NUM', FNAME( 1:PRELEN))
	    CALL MSG_OUT( 'MESSAGE',
     :	      'Coadding file ^NUM ...',
     :	      STATUS)

*          map in the data in this DATA_ARRAY element

            CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                      PNTRDA, NELEMENTS, STATUS )

	    if( status .ne. sai__ok) then
              CALL ERR_REP('ERR',
     :                     'Error after ndf_map image ...', STATUS )
	      return
	    end if

*          call subroutine to coadd the data into the output image

	    CALL STCOADD_IMAGE( DIMS( 1), DIMS( 2), %VAL( PNTRDA),
     :                          %VAL( PNTRO))

*          release the input structure image and annul locators

	    CALL NDF_ANNUL( LOCI, STATUS)
	    if( status .ne. sai__ok) then
              CALL ERR_REP('ERR', 'Error after ndf_annul input ...',
     :                     STATUS )
	      return
	    end if

	  END DO

*        call subroutine to average data in output image

	  CALL MSG_OUT( 'MESSAGE',
     :	    'Averaging final (coadded) image ...',
     :	    STATUS)

	  CALL STCOADD_AVER( DIMS( 1), DIMS( 2), NUMBER_IMAGES,
     :	                     %VAL( PNTRO))

*        unmap the output data image

	  CALL NDF_ANNUL( LOCO, STATUS)
	  if( status .ne. sai__ok) then
            CALL ERR_REP('ERR', 'Error after ndf_annul output ...',
     :                   STATUS )
	    return
	  end if

*      here to coadd a list of random filenames

	ELSE

*        loop to get all the input image numbers to be coadded

	  MORE = .TRUE.
	  NUMBER_IMAGES = 0

	  DO WHILE ( MORE)

	    NUMBER_IMAGES = NUMBER_IMAGES + 1

*          read the latest image name to be coadded

	    CALL PAR_GET0C( 'NAME', NAME( NUMBER_IMAGES), STATUS)

*          convert the input to upper case

*	    CALL CHR_UCASE( NAME( NUMBER_IMAGES) )

*          cancel the current association with this parameter

	    CALL PAR_CANCL( 'NAME', STATUS)

*          test if number of input images greater than image number array size

	    IF( NUMBER_IMAGES .GE. MAX_IMAGES) THEN

	      CALL MSG_SETI( 'NUM', MAX_IMAGES)
	      CALL MSG_OUT( 'MESSAGE',
     :	      'I cannot handle any more input images. ^NUM is my limit',
     :	      STATUS)

	      MORE = .FALSE.

	    ELSE

*            test if user input the name EXIT or QUIT, stop if did

              CALL CHR_CLEAN( NAME( NUMBER_IMAGES) )
              PRELEN = 0
	      CALL CHR_APPND( NAME( NUMBER_IMAGES),
     :	                     NAME( NUMBER_IMAGES),
     :	                     PRELEN)

	      IF( NAME( NUMBER_IMAGES)( 1:PRELEN) .EQ. 'EXIT' .OR.
     :	          NAME( NUMBER_IMAGES)( 1:PRELEN) .EQ. 'exit' .OR.
     :	          NAME( NUMBER_IMAGES)( 1:PRELEN) .EQ. 'QUIT' .OR.
     :	          NAME( NUMBER_IMAGES)( 1:PRELEN) .EQ. 'quit') THEN

	        MORE = .FALSE.

	        NUMBER_IMAGES = NUMBER_IMAGES - 1

	      END IF

	    END IF

	    if( status .ne. sai__ok) then
              CALL ERR_REP('ERR', 'Error after end of input loop ...',
     :                     STATUS )
	      return
	    end if

	  END DO

*        message to user to tell how many input filenames input

	    CALL MSG_SETI( 'NUM', NUMBER_IMAGES)
	    CALL MSG_SETC( 'P', PREFIX)
	    CALL MSG_OUT( 'MESSAGE',
     :	      'O.K. I am about to coadd ^NUM images ...',
     :	      STATUS)

*        get the code for the input image parameter

	  CALL SUBPAR_FINDPAR( 'INPIC', INCODE, STATUS)

*        loop to scan through all the specified input filenames to be coadded

	  DO J = 1, NUMBER_IMAGES

*          suggest this name as the new default for the input image

            CALL CHR_CLEAN( NAME( NUMBER_IMAGES) )
            PRELEN = 0
	    CALL CHR_APPND( NAME( NUMBER_IMAGES),
     :	                   NAME( NUMBER_IMAGES),
     :	                   PRELEN)

	    CALL SUBPAR_PUTNAME( INCODE,
     :	                         NAME( J)( 1:PRELEN),
     :	                         STATUS)

*          associate the input image

	    CALL NDF_ASSOC( 'INPIC', 'READ', LOCI, STATUS)
	    if( status .ne. sai__ok) then
              CALL ERR_REP('ERR', 'Error after ndf_assoc ...',
     :                     STATUS )
	      return
	    end if

*          get the shape/size of the data_array element

	    CALL NDF_DIM( LOCI, NDIMS, DIMS, ACTDIM, STATUS)
	    if( status .ne. sai__ok) then
              CALL ERR_REP('ERR', 'Error after ndf_dim image ...',
     :                     STATUS )
	      return
	    end if

*          set the size parameter for the output image from first set of data

	    IF( J .EQ. 1) THEN

	      CALL MSG_SETI( 'NX', DIMS( 1))
	      CALL MSG_SETI( 'NY', DIMS( 2))
	      CALL MSG_OUT( 'MESSAGE',
     :	        'Images are of size ^NX by ^NY',
     :	        STATUS)

	      DIMSO( 1) = DIMS( 1)
	      DIMSO( 2) = DIMS( 2)

*            get the name of the output image to contain coadded data

	      CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, DIMSO, LOCO,
     :                      STATUS )
	      if( status .ne. sai__ok) then
                CALL ERR_REP('ERR', 'Error after creout ...',
     :                        STATUS )
	        return
	      end if

*            map the output image data_array component

              CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                      PNTRO, NELEMENTS, STATUS )

	      if( status .ne. sai__ok) then
                CALL ERR_REP('ERR',
     :                        'Error after ndf_map output image ...',
     :                         STATUS )
	        return
	      end if

*            set the output array to all zeros

	      CALL STCOADD_ZERO( DIMS( 1), DIMS( 2), %VAL( PNTRO))

	    ELSE

	    END IF

*          message to user telling which image being processed

	    CALL MSG_SETC( 'NUM', NAME( J)( 1:PRELEN))
	    CALL MSG_OUT( 'MESSAGE',
     :	      'Coadding image ^NUM ...',
     :	      STATUS)

*          map in the data in this DATA_ARRAY element

            CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                      PNTRDA, NELEMENTS, STATUS )

	    if( status .ne. sai__ok) then
              CALL ERR_REP('ERR', 'Error after ndf_map input image ...',
     :                      STATUS )
	      return
	    end if

*          call subroutine to coadd the data into the output image

	    CALL STCOADD_IMAGE( DIMS( 1), DIMS( 2), %VAL( PNTRDA),
     :                          %VAL( PNTRO))

*          release the input structure image and annul locators

	    CALL NDF_ANNUL( LOCI, STATUS)
	    if( status .ne. sai__ok) then
              CALL ERR_REP('ERR', 'Error after ndf_annul input ...',
     :                      STATUS )
	      return
	    end if

	  END DO

*        call subroutine to average data in output image

	  CALL MSG_OUT( 'MESSAGE',
     :	    'Averaging final (coadded) image ...',
     :	    STATUS)

	  CALL STCOADD_AVER( DIMS( 1), DIMS( 2), NUMBER_IMAGES,
     :	                     %VAL( PNTRO))

*        unmap the output data image

	  CALL NDF_ANNUL( LOCO, STATUS)
	  if( status .ne. sai__ok) then
            CALL ERR_REP('ERR', 'Error after ndf_annul output ...',
     :                      STATUS )
	    return
	  end if

	END IF

	END
