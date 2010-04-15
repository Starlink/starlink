*+  LINCONT - linearizes an IRCAM container file by applying polynomial

	SUBROUTINE LINCONT( STATUS)

* Description : This routine linearizes the images in an IRCAM container file
*               by subtracting a bias image then applying a 5th order poly-
*               nomial correction to the data.
*
* Invocation :
*
*     CALL LINCONT( STATUS)
*
* Parameters :
*
*     CONTAINER = IMAGE( READ)
*	   Input container file, the source of the information
*
*     NEWCONT   = IMAGE( WRITE)
*	   Output container file, the destination of the information
*
*     COEFFICIENT_FILENAME  = CHARACTER( READ)
*          The name of the ASCII file containig the linearization coefficents
*
* Method :
*
* Bugs :
*
*     None known.
*
* Authors :
*
*     Colin Aspin ROE ( JACH::CAA )
*
* History :
*
*     16-08-1988 : First implementation (JACH::CAA)
*     29-JUNE-1994 Changed MSG_OUT on error to ERR_REP; STR$, LIB$ to
*                  FIO_, CHR_                         (SKL@JACH)
*     11-Aug-1994  Changed input DIM arguments for LINCONT_LIN (SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE			! no default typing allowed

* Global constants :

	INCLUDE 'SAE_PAR'		! SSE global definitions
        INCLUDE 'DAT_PAR'               ! Necessary for non-VMS
	INCLUDE 'DAT_ERR'		! DAT subroutines error messages
        INCLUDE 'FIO_PAR'
        INCLUDE 'CHR_ERR'

* Status :

	INTEGER  STATUS			! global status parameter

* Local Constants :

* Local variables :

	INTEGER DIMS( 2)		! dimensions array
	INTEGER DIMS_BIAS( 2)		! dimensions array for bias image
	INTEGER DIMS_IMAGE( 2)		! dimensions array for data image
	INTEGER INCODE			! code for input container file
	INTEGER J			! loop counter variable
	INTEGER LEN1			! length of upcase variables
	INTEGER LEN2			! length of upcase variables
	INTEGER NDIMS			! number of dimensions variable
	INTEGER BIAS_IMAGE              ! obs number of bias image
	INTEGER NUMBER_ELEMENTS		! number of elements in OBS structure
	INTEGER NUMBER_COADDS_BIAS	! number of coadds in bias image
	INTEGER NUMBER_COADDS_IMAGE	! number of coadds in data image
	INTEGER OUTCODE			! code for input output file
	INTEGER STAT			! status for annuls
	INTEGER NUMCOEFF                ! number of correction coefficients
	INTEGER LUN                     ! lun for coeff file
	INTEGER SUBS( 2)                ! subscripts for playing with OBS
	INTEGER PNTR_BIAS               ! pointer for bias image
	INTEGER PNTR_IMAGE              ! pointer for data image

	PARAMETER ( NUMCOEFF = 5)       ! default the number of coefficients

	REAL COEFFICIENT( NUMCOEFF)     ! coefficients for correction

	CHARACTER*80 INNAME		! name of input container file
	CHARACTER*81 OUTNAME		! name of output container file
	CHARACTER*80 COEFFICIENT_FILENAME ! file containing linearization coeffs
	CHARACTER*80 WHAT_FILLED	! defines what is filled in data
	CHARACTER*80 TITLE_LINE         ! title line for coefficient file

	LOGICAL CONFIRM			! confirms copy
	LOGICAL FILLED			! defines is an observation is filled

	CHARACTER*( DAT__SZLOC)		! locators for :
     :    LOCTOP_O,			! input container file structure
     :    LOCGEN_O,			! general structure
     :	  LOCINST_O,			! instrument structure
     :    LOCID_O,			! id structure
     :    LOCOBS_O,			! obs structure
     :	  LOCTELE_O,			! telescope structure
     :    LOCTOP_N,			! input container file structure
     :    LOCGEN_N,			! general structure
     :    LOCOBS_N,			! obs structure
     :    LOCCELL_N,			! cell locator
     :	  LOCPRIM_N,			! locator for primitive element
     :	  LOCTMP_N,			! temporary locator for data_array
     :    LOCCELL_BIAS,			! cell locator for bias
     :	  LOCPRIM_BIAS,			! locator for primitive element
     :	  LOCTMP_BIAS,			! temporary locator for data_array
     :	  LOCPRIM_COADDS		! locator for coadds in image
*-
*      check status on entry - return if not o.k.

	IF ( STATUS .NE. SAI__OK ) THEN

           CALL ERR_REP( 'ERR', 'Error on LINCONT entry', STATUS )

	   RETURN

	END IF

*      intro to user

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

	CALL MSG_OUT( 'MESSAGE',
     :'This routine LINEARIZES IRCAM observation files, by subtracting',
     :	              STATUS)

	CALL MSG_OUT( 'MESSAGE',
     :'a bias from each image and applying a polynomial correction ...',
     :	              STATUS)

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

*      get the input image name

	CALL PAR_GET0C( 'CONTNAME', INNAME, STATUS)

*      if no error then continue

	IF ( STATUS .NE. SAI__OK ) THEN

          CALL ERR_REP( 'ERR', 'Error on input entry', STATUS )

	  RETURN

	END IF

*      message to user again

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

	CALL CHR_UCASE( INNAME )
        CALL CHR_CLEAN( INNAME )
        LEN1 = 0
	CALL CHR_APPND( INNAME, INNAME, LEN1)

	CALL MSG_SETC( 'NAMEIT', INNAME( 1:LEN1))

	CALL MSG_OUT( 'MESSAGE',
     :	  'O.K. I am about to linearize IRCAM container file ^NAMEIT',
     :	              STATUS)

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

	CALL PAR_GET0L( 'CONFIRM', CONFIRM, STATUS)

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

*      test the confirm varaible for stop option

	IF( .NOT. CONFIRM) THEN

	  CALL MSG_OUT( 'MESSAGE', 'O.K. stopping ...', STATUS)
	  CALL MSG_OUT( 'BLANK', ' ', STATUS)

	  RETURN

	END IF

*      get the name of the coefficient file from parameter system

	CALL PAR_GET0C( 'COEFFILE', COEFFICIENT_FILENAME, STATUS)

*      open the default coefficient file in default directory
	CALL FIO_OPEN(COEFFICIENT_FILENAME, 'READ', 'NONE',
     :        0, LUN, STATUS)
	IF (STATUS .NE. SAI__OK) GO TO 999

*      read title line from LINCOEFF data file and tell user

	READ( LUN, '(A80)') TITLE_LINE

	CALL MSG_SETC( 'TITLE', TITLE_LINE)
	CALL MSG_OUT( 'MESSAGE', 'Title - ^TITLE', STATUS)

*      read the coefficients from the input file

	DO J = 1, NUMCOEFF

	  READ( LUN, *) COEFFICIENT( J)

	  CALL MSG_SETI( 'NUM', J)
	  CALL MSG_SETR( 'C', COEFFICIENT( J))
	  CALL MSG_OUT( 'MESSAGE',
     :                  'Linearization coefficient ^NUM = ^C',
     :	                STATUS)

	END DO

*      close coefficient file and release lun

	CALL FIO_CLOSE( LUN, STATUS )

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

*      get the observation number of the bias image to be subtracted before
*      linearization

	CALL PAR_GET0I( 'BIAS_IMAGE', BIAS_IMAGE, STATUS)

*      create the output container file name from the input + C

	OUTNAME = INNAME( 1:LEN1) // 'L'

        CALL CHR_CLEAN( OUTNAME )
        LEN2 = 0
	CALL CHR_APPND( OUTNAME, OUTNAME, LEN2)

*      set the container file structure name

	CALL SUBPAR_FINDPAR( 'CONTAINER', INCODE, STATUS)

	CALL SUBPAR_PUTNAME ( INCODE, INNAME( 1:LEN1), STATUS)

*      associate container file

	CALL DAT_ASSOC( 'CONTAINER', 'READ', LOCTOP_O, STATUS )

*      pick up code for output filename

	CALL SUBPAR_FINDPAR( 'NEWCONT', OUTCODE, STATUS)

	CALL SUBPAR_PUTNAME ( OUTCODE, OUTNAME( 1:LEN2), STATUS)

*      message to user again

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

	CALL MSG_SETC( 'NAMEIT', OUTNAME( 1:LEN2))

	CALL MSG_OUT( 'MESSAGE',
     :'O.K. Output (linearized) container file will be called ^NAMEIT',
     :	              STATUS)

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

*      find the top level locators for the GENERAL, ID and OBS structures

	CALL DAT_FIND( LOCTOP_O, 'GENERAL', LOCGEN_O, STATUS)

	IF( STATUS .NE. SAI__OK) THEN

	  CALL ERR_REP( 'ERROR',
     :                  'Error, after find LOCGEN_O ...', STATUS)

	  RETURN

	END IF

	CALL DAT_FIND( LOCGEN_O, 'INSTRUMENT', LOCINST_O, STATUS)

	IF( STATUS .NE. SAI__OK) THEN

	  CALL ERR_REP( 'ERROR',
     :                   'Error, after find LOCINST_O ...', STATUS)

	  RETURN

	END IF

	CALL DAT_FIND( LOCGEN_O, 'ID', LOCID_O, STATUS)

	IF( STATUS .NE. SAI__OK) THEN

	  CALL ERR_REP( 'ERROR',
     :                  'Error, after find LOCID_O ...', STATUS)

	  RETURN

	END IF

	CALL DAT_FIND( LOCGEN_O, 'TELESCOPE', LOCTELE_O, STATUS)

	IF( STATUS .NE. SAI__OK) THEN

	  CALL ERR_REP( 'ERROR', 'Error, after find LOCTELE_O ...',
     :                  STATUS)

	  RETURN

	END IF

	CALL DAT_FIND( LOCTOP_O, 'OBS', LOCOBS_O, STATUS)

	IF( STATUS .NE. SAI__OK) THEN

	  CALL ERR_REP( 'ERROR', 'Error, after find LOCOBS_O ...',
     :                  STATUS)

	  RETURN

	END IF

*      get the size of the OBS structure to be scanned

	CALL DAT_SHAPE( LOCOBS_O, 2, DIMS, NDIMS, STATUS)

	NUMBER_ELEMENTS = DIMS( 1)

*      put message out to user

	CALL MSG_SETI( 'OBSELE', NUMBER_ELEMENTS)

	CALL MSG_OUT( 'MESSAGE',
     :                'Observation structure has ^OBSELE elements',
     :	              STATUS)

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

*      create NEW HDS container file for IRCAM images

	CALL DAT_CREAT( 'NEWCONT', 'STRUCTURE', 0, 0, STATUS)

	IF( STATUS .NE. SAI__OK) THEN

	  CALL ERR_REP( 'ERROR',
     :             'Error, after DAT_CREAT new container ...', STATUS)

	  RETURN

	END IF

*      update disk file

	CALL DAT_UPDAT( 'NEWCONT', STATUS)

*      associate container created with active HDS locator

	CALL DAT_ASSOC( 'NEWCONT', 'WRITE', LOCTOP_N, STATUS)

	IF( STATUS .NE. SAI__OK) THEN

	  CALL ERR_REP( 'ERROR',
     :           'Error, after DAT_ASSOC new container ...', STATUS)

	  RETURN

	END IF

*      create top structures to new output container file

	DIMS( 1) = 0
	DIMS( 2) = 0

	CALL DAT_NEW( LOCTOP_N, 'GENERAL', 'STRUCTURE', 0, DIMS,
     :                STATUS)

	CALL DAT_FIND( LOCTOP_N, 'GENERAL', LOCGEN_N, STATUS)

	IF( STATUS .NE. SAI__OK) THEN

	  CALL ERR_REP( 'ERROR',
     :            'Error, after DAT_NEW/DAT_FIND LOCGEN_N ...',
     :             STATUS)

	  RETURN

	END IF

*      tell user whats happening man ...

	CALL MSG_OUT( 'MESSAGE',
     :	  'Copying input images to output data file ... please wait',
     :	  STATUS)

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

*      recursively copy the general structures to the new output file

	CALL DAT_RCOPY( LOCINST_O, LOCGEN_N, 'INSTRUMENT', STATUS)
	CALL DAT_RCOPY( LOCID_O, LOCGEN_N, 'ID', STATUS)
	CALL DAT_RCOPY( LOCTELE_O, LOCGEN_N, 'TELESCOPE', STATUS)

*      recursively copy the observation structures to the new output file

	CALL DAT_RCOPY( LOCOBS_O, LOCTOP_N, 'OBS', STATUS)

	IF( STATUS .NE. SAI__OK) THEN

	  CALL ERR_REP( 'ERROR', 'Error, after DAT_RCOPYs ...',
     :                   STATUS)

	  RETURN

	END IF

*      tidy up the input container file

	STAT = SAI__OK
	CALL DAT_ANNUL( LOCOBS_O, STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCTELE_O, STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCID_O, STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCGEN_O, STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCINST_O, STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCTOP_O, STAT)

*      tell user whats happening man ... AGAIN

	CALL MSG_OUT( 'MESSAGE',
     : 'Finished copying images to output file ... now linearizing',
     :	  STATUS)

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

*      update output disk file

	CALL DAT_UPDAT( 'NEWCONT', STATUS)

*      find bias image and get it's NUMBER OF COADDS
*      find locator to top level OBS structure

	CALL DAT_FIND( LOCTOP_N, 'OBS', LOCOBS_N, STATUS)

	IF( STATUS .NE. SAI__OK) THEN

	  CALL ERR_REP( 'ERROR',
     :                  'Error, after DAT_FIND LOCOBS_N ...', STATUS)

	  RETURN

	END IF

*      check if the BIAS data is defined

	CALL REDCONT_CHECKDATA( BIAS_IMAGE, LOCOBS_N, FILLED, STATUS)

	IF( STATUS .NE. SAI__OK) THEN

	  CALL ERR_REP( 'ERROR',
     :                   'Error, after REDCONT_CHECKDATA ...',
     :                    STATUS)

	  RETURN

	END IF

*      test what bias data is filled

	IF( FILLED) THEN

*        get NUMBER_COADDS in bias

	  SUBS( 1) = BIAS_IMAGE
	  SUBS( 2) =0

	  CALL DAT_CELL( LOCOBS_N, 1, SUBS, LOCCELL_BIAS, STATUS)

	  CALL DAT_FIND( LOCCELL_BIAS, 'NUMBER_COADDS', LOCPRIM_BIAS,
     :                   STATUS)

	  CALL DAT_GETI( LOCPRIM_BIAS, 0, DIMS, NUMBER_COADDS_BIAS,
     :                   STATUS)

	  CALL DAT_ANNUL( LOCPRIM_BIAS, STATUS)

	  CALL DAT_FIND( LOCCELL_BIAS, 'PHASEA', LOCTMP_BIAS, STATUS)

	  CALL DAT_FIND( LOCTMP_BIAS, 'DATA_ARRAY', LOCPRIM_BIAS,
     :                   STATUS)

	  CALL DAT_SHAPE( LOCPRIM_BIAS, 2, DIMS_BIAS, NDIMS, STATUS)

	  CALL DAT_MAPR( LOCPRIM_BIAS, 'UPDATE', NDIMS, DIMS_BIAS,
     :	                 PNTR_BIAS, STATUS)

*        tell user how many coadds in specified bias image

	  CALL MSG_SETI( 'BI', BIAS_IMAGE)
	  CALL MSG_SETI( 'NCB', NUMBER_COADDS_BIAS)
	  CALL MSG_OUT( 'MESSAGE',
     :	  'Number of coadds in BIAS IMAGE OBS(^BI) = ^NCB', STATUS)

	  CALL MSG_OUT( 'BLANK', ' ', STATUS)

	ELSE

	END IF

*      loop to linearize all OBS primitives in new output file

	DO J = 1, NUMBER_ELEMENTS

*        setup the subscripts for the OBS element being processed

	  SUBS( 1) = J
	  SUBS( 2) = 0

*        get a locator to the cell being processed

	  CALL DAT_CELL( LOCOBS_N, 1, SUBS, LOCCELL_N, STATUS)

	  IF( STATUS .NE. SAI__OK) THEN

	    CALL ERR_REP( 'ERROR',
     :                    'Error, after DAT_CELL LOCCELL_N ...',
     :                    STATUS)

	    RETURN

	  END IF

*        check if the observational data is set for this OBS number

	  CALL REDCONT_CHECKDATA( J, LOCOBS_N, FILLED, STATUS)

	  IF( STATUS .NE. SAI__OK) THEN

            CALL ERR_ANNUL( STATUS )

	    CALL MSG_SETI( 'J', J)
	    CALL MSG_OUT( 'ERROR',
     :        'Error, observation ^J not defined ...',
     :	      STATUS)

	    FILLED = .FALSE.

	  END IF

*        test what data in this OBS is filled

	  IF( FILLED) THEN

*          tell user which observation is currently being processed

	    CALL MSG_SETI( 'OBSNUM', J)
	    CALL MSG_OUT( 'MESSAGE',
     :	      'Linearizing OBSERVATION number ^OBSNUM ...',
     :	                  STATUS)

*          get NUMBER_COADDS in image

	    CALL DAT_FIND( LOCCELL_N, 'NUMBER_COADDS', LOCPRIM_COADDS,
     :                     STATUS)

	    CALL DAT_GETI( LOCPRIM_COADDS, 0, DIMS,
     :                     NUMBER_COADDS_IMAGE, STATUS)

	    CALL DAT_ANNUL( LOCPRIM_COADDS, STATUS)

*          find out what is set in the DATA_ARRAY components

	    CALL REDCONT_WHATSET( J, LOCOBS_N, WHAT_FILLED, STATUS)

	    IF( STATUS .NE. SAI__OK) THEN

	      CALL ERR_REP( 'ERROR',
     :                      'Error, after REDCONT_WHATSET ...',
     :                       STATUS)

	      RETURN

	    END IF

*          trim the what set character variable

            CALL CHR_CLEAN( WHAT_FILLED )
            LEN1 = 0
	    CALL CHR_APPND( WHAT_FILLED, WHAT_FILLED, LEN1)

*          message to user about what filled

	    CALL MSG_SETC( 'CETI', WHAT_FILLED( 1:LEN1))
	    CALL MSG_OUT( 'MESSAGE', '   Data defined = ^CETI',
     :                     STATUS)

*          linearize the DATA_ARRAY components that are defined

	    IF( WHAT_FILLED( 1:LEN1) .EQ. 'PHASEA' .OR.
     :	        WHAT_FILLED( 1:LEN1) .EQ. 'PHASEA+KTCA' .OR.
     :	        WHAT_FILLED( 1:LEN1) .EQ. 'PHASEA+PHASEB' .OR.
     :	        WHAT_FILLED( 1:LEN1) .EQ. 'PHASEA+PHASEB+KTCA+KTCB')
     :      THEN

*            find the DATA_ARRAY component, get it's shape and map it

	      CALL DAT_FIND( LOCCELL_N, 'PHASEA', LOCTMP_N, STATUS)
	      CALL DAT_FIND( LOCTMP_N, 'DATA_ARRAY', LOCPRIM_N,
     :                                  STATUS)

	      CALL DAT_SHAPE( LOCPRIM_N, 2, DIMS_IMAGE, NDIMS, STATUS)

	      CALL DAT_MAPR( LOCPRIM_N, 'UPDATE', NDIMS, DIMS_IMAGE,
     :	                     PNTR_IMAGE, STATUS)

	      IF( STATUS .NE. SAI__OK) THEN

	        CALL ERR_REP( 'ERROR',
     :                     'Error, after MAPR PHASEA image ...',
     :                      STATUS)

	        RETURN

	      END IF

*            call subroutine to scale bias/image, subtract bias, linearize
*            re-scale bias/image, add bias back in and store in output image

	      CALL LINCONT_LIN( DIMS_BIAS(1), DIMS_BIAS(2),
     :	                        %VAL( PNTR_BIAS), NUMBER_COADDS_BIAS,
     :	                        DIMS_IMAGE(1), DIMS_IMAGE(2),
     :                          %VAL( PNTR_IMAGE), NUMBER_COADDS_IMAGE,
     :	                        NUMCOEFF, COEFFICIENT)

*            release the image/annul locators

	      CALL DAT_ANNUL( LOCPRIM_N, STATUS)
	      CALL DAT_ANNUL( LOCTMP_N, STATUS)

	    END IF

	    IF( WHAT_FILLED( 1:LEN1) .EQ. 'PHASEB' .OR.
     :	        WHAT_FILLED( 1:LEN1) .EQ. 'PHASEB+KTCB' .OR.
     :	        WHAT_FILLED( 1:LEN1) .EQ. 'PHASEA+PHASEB' .OR.
     :	        WHAT_FILLED( 1:LEN1) .EQ. 'PHASEA+PHASEB+KTCA+KTCB')
     :      THEN

*            find the DATA_ARRAY component, get it's shape and map it

	      CALL DAT_FIND( LOCCELL_N, 'PHASEB', LOCTMP_N, STATUS)
	      CALL DAT_FIND( LOCTMP_N, 'DATA_ARRAY', LOCPRIM_N, STATUS)

	      CALL DAT_SHAPE( LOCPRIM_N, 2, DIMS_IMAGE, NDIMS, STATUS)

	      CALL DAT_MAPR( LOCPRIM_N, 'UPDATE', NDIMS, DIMS_IMAGE,
     :	                     PNTR_IMAGE, STATUS)

	      IF( STATUS .NE. SAI__OK) THEN

	        CALL ERR_REP( 'ERROR',
     :                      'Error, after MAPR PHASEB image ...',
     :                       STATUS)

	        RETURN

	      END IF

*            call subroutine to scale bias/image, subtract bias, linearize
*            re-scale bias/image, add bias back in and store in output image

	      CALL LINCONT_LIN( DIMS_BIAS(1), DIMS_BIAS(2),
     :	                        %VAL( PNTR_BIAS), NUMBER_COADDS_BIAS,
     :	                        DIMS_IMAGE(1), DIMS_IMAGE(2),
     :                          %VAL( PNTR_IMAGE), NUMBER_COADDS_IMAGE,
     :	                        NUMCOEFF, COEFFICIENT)

*            release the image/annul locators

	      CALL DAT_ANNUL( LOCPRIM_N, STATUS)
	      CALL DAT_ANNUL( LOCTMP_N, STATUS)

	    END IF

	    IF( WHAT_FILLED( 1:LEN1) .EQ. 'PHASEA+KTCA' .OR.
     :	        WHAT_FILLED( 1:LEN1) .EQ. 'PHASEA+PHASEB+KTCA+KTCB')
     :      THEN

*            find the DATA_ARRAY component, get it's shape and map it

	      CALL DAT_FIND( LOCCELL_N, 'KTCA', LOCTMP_N, STATUS)
	      CALL DAT_FIND( LOCTMP_N, 'DATA_ARRAY', LOCPRIM_N, STATUS)

	      CALL DAT_SHAPE( LOCPRIM_N, 2, DIMS_IMAGE, NDIMS, STATUS)

	      CALL DAT_MAPR( LOCPRIM_N, 'UPDATE', NDIMS, DIMS_IMAGE,
     :	                     PNTR_IMAGE, STATUS)

	      IF( STATUS .NE. SAI__OK) THEN

	        CALL ERR_REP( 'ERROR',
     :                        'Error, after MAPR KTCA image ...',
     :                         STATUS)

	        RETURN

	      END IF

*            call subroutine to scale bias/image, subtract bias, linearize
*            re-scale bias/image, add bias back in and store in output image

	      CALL LINCONT_LIN( DIMS_BIAS(1), DIMS_BIAS(2),
     :	                        %VAL( PNTR_BIAS), NUMBER_COADDS_BIAS,
     :	                        DIMS_IMAGE(1), DIMS_IMAGE(2),
     :                          %VAL( PNTR_IMAGE), NUMBER_COADDS_IMAGE,
     :	                        NUMCOEFF, COEFFICIENT)

*            release the image/annul locators

	      CALL DAT_ANNUL( LOCPRIM_N, STATUS)
	      CALL DAT_ANNUL( LOCTMP_N, STATUS)

	    END IF

	    IF( WHAT_FILLED( 1:LEN1) .EQ. 'PHASEB+KTCB' .OR.
     :	        WHAT_FILLED( 1:LEN1) .EQ. 'PHASEA+PHASEB+KTCA+KTCB')
     :      THEN

*            find the DATA_ARRAY component, get it's shape and map it

	      CALL DAT_FIND( LOCCELL_N, 'KTCB', LOCTMP_N, STATUS)
	      CALL DAT_FIND( LOCTMP_N, 'DATA_ARRAY', LOCPRIM_N, STATUS)

	      CALL DAT_SHAPE( LOCPRIM_N, 2, DIMS_IMAGE, NDIMS, STATUS)

	      CALL DAT_MAPR( LOCPRIM_N, 'UPDATE', NDIMS, DIMS_IMAGE,
     :	                     PNTR_IMAGE, STATUS)

	      IF( STATUS .NE. SAI__OK) THEN

	        CALL ERR_REP( 'ERROR',
     :                         'Error, after MAPR KTCB image ...',
     :                         STATUS)

	        RETURN

	      END IF

*            call subroutine to scale bias/image, subtract bias, linearize
*            re-scale bias/image, add bias back in and store in output image

	      CALL LINCONT_LIN( DIMS_BIAS(1), DIMS_BIAS(2),
     :	                        %VAL( PNTR_BIAS), NUMBER_COADDS_BIAS,
     :	                        DIMS_IMAGE(1), DIMS_IMAGE(2),
     :                          %VAL( PNTR_IMAGE), NUMBER_COADDS_IMAGE,
     :	                        NUMCOEFF, COEFFICIENT)

*            release the image/annul locators

	      CALL DAT_ANNUL( LOCPRIM_N, STATUS)
	      CALL DAT_ANNUL( LOCTMP_N, STATUS)

	    END IF

	  END IF

	END DO

*      tidy up the output container file

	STAT = SAI__OK
	CALL DAT_ANNUL( LOCTMP_BIAS, STATUS)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCPRIM_BIAS, STATUS)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCOBS_N, STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCGEN_N, STAT)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCTOP_N, STAT)

*      tell user that that is it ... all finished ... pau

	CALL MSG_OUT( 'MESSAGE',
     :                'All finished ... IRCAM file linearized',
     :	              STATUS)

	GOTO 888

  999	CALL MSG_SETC( 'C', COEFFICIENT_FILENAME)
	CALL ERR_REP( 'MESSAGE',
     :	  'Error, cannot find coefficient file ^C', STATUS)

  888	END
