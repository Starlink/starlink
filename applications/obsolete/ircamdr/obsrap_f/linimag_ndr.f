*+  LINIMAG_NDR - linearizes an IRCAM file by applying polynomial
*                 This version for non-destructive read mode with KTC

	SUBROUTINE LINIMAG_NDR( STATUS)

* Description : This routine linearizes an image
*               by subtracting a bias image then applying a 5th order poly-
*               nomial correction to the data.  In non-destructive read mode
*               the KTC image is added back onto the PhaseA image and
*               the resultant image is linearizes.  After this the KTC
*               images is re-subtracted from the PhaseA image.
*
* Invocation :
*
*     CALL LIIMAG_NDR( STATUS)
*
* Parameters :
*
*     CONTAINER = IMAGE( READ)
*	   Input container file, the source of the information
*
*     OUTPIC = IMAGE( WRITE)
*	   Output file, the destination of the information
*
*     COEFFICIENT_FILENAME  = CHARACTER( READ)
*          The name of the ASCII file containig the linearization coefficents
*
*     OBSNUM = INTEGER ( READ )
*          The number of the image in container file to be linearized
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
*     14-12-1989 : Created this version from LINCONT (JACH::CAA)
*     17-06-1991 : Created this version from LINCONT_NDR (JACH::CAA)
*     29-JUN-1994  Changed MSG_OUT on error to ERR_REP, STR$ and LIB$
*                  to FIO_, CHR_ (SKL@JACH)
*     07-Sept-1994 Removed unused variables identified by UNIX compiler(SKL@JACH)
* Type definitions :

	IMPLICIT  NONE			! no default typing allowed

* Global constants :

	INCLUDE 'SAE_PAR'		! SSE global definitions
        INCLUDE 'DAT_PAR'       ! Necessary for non-VMS
	INCLUDE 'DAT_ERR'	! DAT subroutines error messages
        INCLUDE 'CHR_ERR'
        INCLUDE 'FIO_PAR'

* Status :

	INTEGER  STATUS			! global status parameter

* Local Constants :

* Local variables :

	INTEGER DIMS( 2)		! dimensions array
	INTEGER DIMS_IMAGE( 2)		! dimensions array for data image
	INTEGER INCODE			! code for input container file
	INTEGER J			! loop counter variable
	INTEGER LEN1			! length of upcase variables
	INTEGER LEN2			! length of upcase variables
	INTEGER NDIMS			! number of dimensions variable
	INTEGER NUMBER_ELEMENTS		! number of elements in OBS structure
	INTEGER NUMBER_COADDS_IMAGE	! number of coadds in data image
	INTEGER OUTCODE			! code for input output file
	INTEGER STAT			! status for annuls
	INTEGER NUMCOEFF                ! number of correction coefficients
	INTEGER LUN                     ! lun for coeff file
	INTEGER SUBS( 2)                ! subscripts for playing with OBS
	INTEGER SPNTR                   ! pointer for data image
	INTEGER PNTR_IMAGE              ! pointer for data image
	INTEGER PNTR2_IMAGE             ! pointer for data image
	INTEGER OBSNUM                  ! Observation number to be linearized

	PARAMETER ( NUMCOEFF = 3)       ! default the number of coefficients

	REAL COEFFICIENT( NUMCOEFF)     ! coefficients for correction
	REAL BASE                       ! base value in data
	REAL TOP                        ! top value in data
	REAL READOUT_RATE               ! rr of image
	REAL EXP_TIME                   ! exposure time of image
	REAL RAT                        ! ratio of rr to exp_time

	CHARACTER*80 INNAME		! name of input container file
	CHARACTER*81 OUTNAME		! name of output container file
	CHARACTER*80 COEFFICIENT_FILENAME ! file containing linearization coeffs
	CHARACTER*80 WHAT_FILLED	! defines what is filled in data
	CHARACTER*80 TITLE_LINE         ! title line for coefficient file
	CHARACTER*80 TRANSLATION        ! temp variable for env conversion

	LOGICAL FILLED			! defines is an observation is filled

	CHARACTER*( DAT__SZLOC)		! locators for :
     :	  LOCDA,                        !
     :    LOCTOP_O,			! input container file structure
     :    LOCOBS_O,			! obs structure
     :    LOCCELL_O,			! cell locator
     :    LOCTOP_N,			! input container file structure
     :    LOCGEN_N,			! general structure
     :    LOCOBS_N,			! obs structure
     :	  LOCPRIM_O,			! locator for primitive element
     :	  LOCPRIM2_O,			! locator for primitive element
     :	  LOCTMP_O,			! temporary locator for data_array
     :	  LOCTMP2_O,			! temporary locator for data_array
     :	  LOCPRIM_COADDS,		! locator for coadds in image
     :	  LOCPRIM_RR,			! locator for RR in image
     :	  LOCPRIM_ET			! locator for exp-time in image
*-
*      check status on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK ) THEN
	  RETURN
	END IF

*      get the input image name
	CALL PAR_GET0C( 'CONTNAME', INNAME, STATUS)
*	CALL CHR_UCASE( INNAME )
        CALL CHR_CLEAN( INNAME )
        LEN1 = 0
	CALL CHR_APPND( INNAME, INNAME, LEN1)
	IF ( STATUS .NE. SAI__OK ) THEN
	  RETURN
	END IF

*      get the name of the coefficient file from parameter system
	CALL PAR_GET0C( 'COEFFILE', COEFFICIENT_FILENAME, STATUS)
*	CALL CHR_UCASE( COEFFICIENT_FILENAME )
        CALL AB_TRANSLATE_ENV( COEFFICIENT_FILENAME, TRANSLATION,
     :	  STATUS)
        COEFFICIENT_FILENAME = TRANSLATION

*      open the default coefficient file in default directory
        CALL FIO_OPEN(COEFFICIENT_FILENAME, 'READ', 'NONE',
     :        0, LUN, STATUS)

*      read title line from LINCOEFF data file and tell user
	READ( LUN, '(A80)') TITLE_LINE
	CALL MSG_SETC( 'TITLE', TITLE_LINE)
	CALL MSG_OUT( 'MESSAGE', '^TITLE', STATUS)

*      read the coefficients from the input file
	DO J = 1, NUMCOEFF
	  READ( LUN, *) COEFFICIENT( J)
	  CALL MSG_SETI( 'NUM', J)
	  CALL MSG_SETR( 'C', COEFFICIENT( J))
	  CALL MSG_OUT( 'MESSAGE', 'Linearization coefficient ^NUM = ^C',
     :	                STATUS)
	END DO

*      read the base and top values from input file
	READ( LUN, *) BASE
	READ( LUN, *) TOP
	CALL MSG_SETR( 'BASE', BASE)
	CALL MSG_SETR( 'TOP', TOP)
	CALL MSG_OUT( 'MESSAGE', 'Base = ^BASE, Top = ^TOP',
     :	              STATUS)

*      close coefficient file and release lun
	CALL FIO_CLOSE( LUN, STATUS )

*     get number of observation to linearize
	CALL PAR_GET0I( 'OBSNUM', OBSNUM, STATUS)

*      create the output container file name from the input + C
	CALL PAR_GET0C( 'OUTNAME', OUTNAME, STATUS)
*	CALL CHR_UCASE( OUTNAME )
        CALL CHR_CLEAN( OUTNAME )
        LEN2 = 0
	CALL CHR_APPND( OUTNAME, OUTNAME, LEN2)

*      set the container file structure name
	CALL SUBPAR_FINDPAR( 'CONTAINER', INCODE, STATUS)
	CALL SUBPAR_PUTNAME ( INCODE, INNAME( 1:LEN1), STATUS)

*      pick up code for output filename
	CALL SUBPAR_FINDPAR( 'OUTPIC', OUTCODE, STATUS)
	CALL SUBPAR_PUTNAME ( OUTCODE, OUTNAME( 1:LEN2), STATUS)

*      associate container file
	CALL DAT_ASSOC( 'CONTAINER', 'READ', LOCTOP_O, STATUS )
	CALL DAT_FIND( LOCTOP_O, 'OBS', LOCOBS_O, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'ERROR', 'Error, after find LOCOBS_O ...',
     :                   STATUS)
	  RETURN
	END IF

*      get the size of the OBS structure to be scanned
	CALL DAT_SHAPE( LOCOBS_O, 2, DIMS, NDIMS, STATUS)
	NUMBER_ELEMENTS = DIMS( 1)

*      create NEW HDS container file for IRCAM images
	CALL DAT_CREAT( 'OUTPIC', 'STRUCTURE', 0, 0, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'ERROR', 'Error, after DAT_CREAT new file ...',
     :                   STATUS)
	  RETURN
	END IF

*      update disk file
	CALL DAT_UPDAT( 'OUTPIC', STATUS)

*      associate container created with active HDS locator
	CALL DAT_ASSOC( 'OUTPIC', 'WRITE', LOCTOP_N, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'ERROR', 'Error, after DAT_ASSOC new file ...',
     :                   STAT)
	  RETURN
	END IF

*      setup the subscripts for the OBS element being processed
	SUBS( 1) = OBSNUM
	SUBS( 2) = 0

*      get a locator to the cell being processed
	CALL DAT_CELL( LOCOBS_O, 1, SUBS, LOCCELL_O, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'ERROR', 'Error, after DAT_CELL LOCCELL_O ...',
     :                   STATUS)
	  RETURN
	END IF

*      check if the observational data is set for this OBS number
	CALL REDCONT_CHECKDATA( OBSNUM, LOCOBS_O, FILLED, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_ANNUL( STATUS )
	  CALL MSG_SETI( 'J', OBSNUM)
	  CALL MSG_OUT( 'ERROR', 'Error, observation ^J not defined ...',
     :	    STATUS)
	  FILLED = .FALSE.
	END IF

*      find out what is set in the DATA_ARRAY components
	CALL REDCONT_WHATSET( OBSNUM, LOCOBS_O, WHAT_FILLED, STATUS)
        CALL CHR_CLEAN( WHAT_FILLED )
        LEN1 = 0
	CALL CHR_APPND( WHAT_FILLED, WHAT_FILLED, LEN1)
	IF( STATUS .NE. SAI__OK) THEN
	   CALL ERR_REP( 'ERROR', 'Error, after REDCONT_WHATSET ...',
     :                    STATUS)
	  RETURN
	END IF

*      message to user about what filled
	CALL MSG_SETC( 'CETI', WHAT_FILLED( 1:LEN1))
	CALL MSG_OUT( 'MESSAGE', '  Data defined = ^CETI', STATUS)

*      Create new element
	CALL DAT_FIND( LOCCELL_O, 'PHASEA', LOCTMP_O, STATUS)
	CALL DAT_FIND( LOCTMP_O, 'DATA_ARRAY', LOCPRIM_O, STATUS)
	CALL DAT_SHAPE( LOCPRIM_O, 2, DIMS, NDIMS, STATUS)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCPRIM_O, STAT)
	call err_annul( status)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCTMP_O, STAT)
	call err_annul( status)
	CALL DAT_NEW( LOCTOP_N, 'DATA_ARRAY', '_REAL', NDIMS, DIMS, STATUS)
	CALL DAT_FIND( LOCTOP_N, 'DATA_ARRAY', LOCDA, STATUS)
	CALL DAT_MAPR( LOCDA, 'WRITE', NDIMS, DIMS, SPNTR, STATUS)

*      test what data in this OBS is filled
	IF( FILLED) THEN

*        get NUMBER_COADDS in image
	  CALL DAT_FIND( LOCCELL_O, 'NUMBER_COADDS', LOCPRIM_COADDS, STATUS)
	  CALL DAT_GETI( LOCPRIM_COADDS, 0, DIMS, NUMBER_COADDS_IMAGE, STATUS)
	  CALL DAT_ANNUL( LOCPRIM_COADDS, STATUS)
	call err_annul( status)

*        get READOUT_RATE in image
	  CALL DAT_FIND( LOCCELL_O, 'READOUT_RATE', LOCPRIM_RR, STATUS)
	  CALL DAT_GETR( LOCPRIM_RR, 0, DIMS, READOUT_RATE, STATUS)
	  CALL DAT_ANNUL( LOCPRIM_RR, STATUS)
	call err_annul( status)

*        get EXP_TIME in image
	  CALL DAT_FIND( LOCCELL_O, 'EXPOSURE_TIME', LOCPRIM_ET, STATUS)
	  CALL DAT_GETR( LOCPRIM_ET, 0, DIMS, EXP_TIME, STATUS)
	  CALL DAT_ANNUL( LOCPRIM_ET, STATUS)
	call err_annul( status)

*        calculate ratio of rr to exp-time
	  IF( EXP_TIME .NE. 0.0) THEN
	    RAT = READOUT_RATE/EXP_TIME
	  ELSE
	    RAT = 1.0
	  END IF

*        tell user readout rate, exposure time and ratio found
	  CALL MSG_SETR( 'RR', READOUT_RATE)
	  CALL MSG_SETR( 'ET', EXP_TIME)
	  CALL MSG_SETR( 'RA', RAT)
	  CALL MSG_OUT( 'MESS',
     :	    '  Readout Rate = ^RR, Exposure Time = ^ET, Ratio = ^RA',
     :	    STATUS)

*        linearize the DATA_ARRAY components that are defined
	  IF( WHAT_FILLED( 1:LEN1) .EQ. 'PHASEA+KTCA' .OR.
     :	      WHAT_FILLED( 1:LEN1) .EQ. 'PHASEA+PHASEB+KTCA+KTCB') THEN

*          find the DATA_ARRAY components, get there shape and map'um
	    CALL DAT_FIND( LOCCELL_O, 'PHASEA', LOCTMP_O, STATUS)
	    CALL DAT_FIND( LOCTMP_O, 'DATA_ARRAY', LOCPRIM_O, STATUS)
	    CALL DAT_SHAPE( LOCPRIM_O, 2, DIMS_IMAGE, NDIMS, STATUS)
	    CALL DAT_MAPR( LOCPRIM_O, 'READ', NDIMS, DIMS_IMAGE,
     :	                     PNTR_IMAGE, STATUS)
	    CALL DAT_FIND( LOCCELL_O, 'KTCA', LOCTMP2_O, STATUS)
	    CALL DAT_FIND( LOCTMP2_O, 'DATA_ARRAY', LOCPRIM2_O, STATUS)
	    CALL DAT_SHAPE( LOCPRIM2_O, 2, DIMS_IMAGE, NDIMS, STATUS)
	    CALL DAT_MAPR( LOCPRIM2_O, 'READ', NDIMS, DIMS_IMAGE,
     :	                   PNTR2_IMAGE, STATUS)
            IF( STATUS .NE. SAI__OK) THEN
	      CALL ERR_REP( 'ERROR',
     :	        'Error, after MAPR PHASEA+KTCA image ...', STATUS)
	      RETURN
	    END IF

*          call subroutine to scale image, add on ktc image, linearize,
*          re-scale image, subtract ktc and store in output image
	    CALL LINIMAG_NDR_LIN( DIMS_IMAGE( 1), DIMS_IMAGE( 2),
     :	                          %VAL( PNTR_IMAGE), %VAL( PNTR2_IMAGE),
     :	                          %VAL( SPNTR), NUMBER_COADDS_IMAGE,
     :	                          NUMCOEFF, COEFFICIENT, BASE, TOP, RAT)

*          release the image/annul locators
	    CALL DAT_ANNUL( LOCPRIM_O, STATUS)
	call err_annul( status)
	    CALL DAT_ANNUL( LOCTMP_O, STATUS)
	call err_annul( status)
	    CALL DAT_ANNUL( LOCPRIM2_O, STATUS)
	call err_annul( status)
	    CALL DAT_ANNUL( LOCTMP2_O, STATUS)
	call err_annul( status)
	  END IF

	  IF( WHAT_FILLED( 1:LEN1) .EQ. 'PHASEB+KTCB' .OR.
     :	      WHAT_FILLED( 1:LEN1) .EQ. 'PHASEA+PHASEB+KTCA+KTCB') THEN

*          find the DATA_ARRAY component, get it's shape and map it
	    CALL DAT_FIND( LOCCELL_O, 'PHASEB', LOCTMP_O, STATUS)
	    CALL DAT_FIND( LOCTMP_O, 'DATA_ARRAY', LOCPRIM_O, STATUS)
	    CALL DAT_SHAPE( LOCPRIM_O, 2, DIMS_IMAGE, NDIMS, STATUS)
	    CALL DAT_MAPR( LOCPRIM_O, 'READ', NDIMS, DIMS_IMAGE,
     :	                   PNTR_IMAGE, STATUS)
	    CALL DAT_FIND( LOCCELL_O, 'KTCB', LOCTMP2_O, STATUS)
	    CALL DAT_FIND( LOCTMP2_O, 'DATA_ARRAY', LOCPRIM2_O, STATUS)
	    CALL DAT_SHAPE( LOCPRIM2_O, 2, DIMS_IMAGE, NDIMS, STATUS)
	    CALL DAT_MAPR( LOCPRIM2_O, 'READ', NDIMS, DIMS_IMAGE,
     :	                   PNTR2_IMAGE, STATUS)
	    IF( STATUS .NE. SAI__OK) THEN
	      CALL ERR_REP( 'ERROR',
     :	        'Error, after MAPR PHASEB+KTCB image ...', STATUS)
	      RETURN
	    END IF

*          call subroutine to scale images, add KTC, linearize
*          re-scale image, subtract ktc and store in output image
	    CALL LINIMAG_NDR_LIN( DIMS_IMAGE( 1), DIMS_IMAGE( 2),
     :	                          %VAL( PNTR_IMAGE), %VAL( PNTR2_IMAGE),
     :	                          %VAL( SPNTR), NUMBER_COADDS_IMAGE,
     :	                          NUMCOEFF, COEFFICIENT, BASE, TOP, RAT)

*          release the image/annul locators
	    CALL DAT_ANNUL( LOCPRIM_O, STATUS)
	call err_annul( status)
	    CALL DAT_ANNUL( LOCTMP_O, STATUS)
	call err_annul( status)
	    CALL DAT_ANNUL( LOCPRIM2_O, STATUS)
	call err_annul( status)
	    CALL DAT_ANNUL( LOCTMP2_O, STATUS)
	call err_annul( status)
	  END IF

	  IF( WHAT_FILLED( 1:LEN1) .EQ. 'PHASEA' .OR.
     :	      WHAT_FILLED( 1:LEN1) .EQ. 'PHASEB') THEN
	    CALL MSG_SETI( 'NUM', J)
	    CALL MSG_OUT( 'ERROR',
     :	      'Error, observation ^NUM is NOT a NDR observation...',
     :	      STATUS)
	  END IF
	END IF

*      update output disk file
	CALL DAT_UPDAT( 'OUTPIC', STATUS)

*      tidy up the output container file
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCOBS_O, STAT)
	call err_annul( status)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCTOP_O, STAT)
	call err_annul( status)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCOBS_N, STAT)
	call err_annul( status)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCGEN_N, STAT)
	call err_annul( status)
	STAT = SAI__OK
	CALL DAT_ANNUL( LOCTOP_N, STAT)
	call err_annul( status)

	GOTO 888

  999	CALL MSG_SETC( 'C', COEFFICIENT_FILENAME)
	CALL ERR_REP( 'MESSAGE',
     :	  'Error, cannot find coefficient file ^C', STATUS)

  888	END
