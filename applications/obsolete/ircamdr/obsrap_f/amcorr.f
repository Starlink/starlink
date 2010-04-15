*+  AMCORR - corrects intensity image to zero airmass

      SUBROUTINE AMCORR ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL AMCORR ( STATUS )
*
*    Parameters :
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get extinction file to be used as correction
*     If no error then get extinction/airmass for different passbands
*     Get input image data structure
*     If no error so far then
*        Get airmass to be used as correction
*        Map the input DATA_ARRAY component
*        Create output image data structure
*        If no error so far then
*           Map an output DATA_ARRAY component
*           If no errors then
*              call subroutine to do airmass correction
*           Endif
*           Tidy up output structure
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
*     17/01/1990 : Original version  (JACH::CAA)
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     24-JUN-1994  Changed STR$ to CHR_ and LIB$ to FIO_ (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE           ! no implicit typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'       ! global SSE definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'
      INCLUDE 'CHR_ERR'
      INCLUDE 'FIO_PAR'

*    Status :

      INTEGER STATUS          ! global status parameter

*    Local constants :

      INTEGER NDIMS           ! dimensionality of images
      PARAMETER ( NDIMS = 2 ) ! 2-d only

*    Local variables :

      INTEGER
     :  LOCI,                 ! locator for input IMAGE structure
     :  LOCO,                 ! locator for output IMAGE structure
     :  DIMS( NDIMS ),        ! dimensions of input DATA_ARRAYs
     :  ODIMS( NDIMS ),       ! dimensions of output DATA_ARRAYs
     :  PNTRO,                ! pointer to output DATA_ARRAY
     :  PNTRI,                !    "     " input      "
     :  ACTDIM,               ! actual dimensions from NDF_DIM
     :  NELEMENTS,            ! number of elements mapped by NDF_DIM
     :	LUN,
     :	NUM_FILTERS,
     :	COUNTER,
     :	POSEQ

      REAL
     :	AIRMASS,
     :	FACTOR,
     :	EXTVAL,
     :	MAGICNO,
     :	EXTVALS( 20)          ! extinction values for each filter

      CHARACTER
     :	FILTER*40,            ! filter that observations were taken in
     :	EXTFILE*80,           ! file with extinction values in
     :  TRANSLATION*80,
     :	FILTERS( 20)*80,      ! filters for extinction values
     :	LINE*80

      LOGICAL
     :	MORE

*-
*    check for error on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get magic number for bad pixels from interface
      CALL PAR_GET0R( 'MAGICNO', MAGICNO, STATUS)

*    get the file containing the extinction values/airmass
      CALL PAR_GET0C( 'EXTFILE', EXTFILE, STATUS)
      CALL AB_TRANSLATE_ENV( EXTFILE, TRANSLATION, STATUS)
      EXTFILE = TRANSLATION

*    tell user the name of the extinction file being used
      CALL MSG_OUT( 'BLANK', ' ', STATUS)
      CALL MSG_SETC( 'EF', EXTFILE)
      CALL MSG_OUT( 'MESS', 'Extinction file in use = ^EF', STATUS)
      CALL MSG_OUT( 'BLANK', ' ', STATUS)

*    get a lun and try to open input file with extinction values
      CALL FIO_GUNIT( LUN, STATUS )
      OPEN( UNIT=LUN, FILE=EXTFILE, STATUS='OLD')

      MORE = .TRUE.
      COUNTER = 0

*    loop to scan through the extinction file getting filters and values
      DO WHILE ( MORE)

*      read data line
        READ( LUN, '(A)', END=100, ERR=998) LINE

*      increment the number of data lines read
	COUNTER = COUNTER + 1

*      find the position of the = sign since this delineates the
*      filter from the extinction value
	POSEQ = INDEX( LINE, '=')

*      if an = sign was found, read the filter name and the
*      extinction value from the data line
	IF( POSEQ .GT. 0) THEN
	  READ( LINE( 1:POSEQ-1), '(A)') FILTERS( COUNTER)
	  READ( LINE( POSEQ+1:), *) EXTVALS( COUNTER)
	  CALL CHR_UCASE( FILTERS( COUNTER) )

*        tell the user the extinction values read in ...
	  CALL MSG_SETC( 'FN', FILTERS( COUNTER))
	  CALL MSG_SETR( 'EX', EXTVALS( COUNTER))
	  CALL MSG_OUT( 'MESSAGE',
     :	  'Filter = ^FN,		Extinction/am = ^EX', STATUS)
	END IF
      END DO
  100 CLOSE( LUN)
      CALL FIO_PUNIT( LUN, STATUS )

*    set number of filters/extinction value pairs found
      NUM_FILTERS = COUNTER
      CALL MSG_OUT( 'BLANK', ' ', STATUS)
      CALL MSG_SETI( 'NF', NUM_FILTERS)
      CALL MSG_OUT( 'MESS',
     :    'Number of extinction coefficients read in = ^NF', STATUS)
      CALL MSG_OUT( 'BLANK', ' ', STATUS)
      IF( NUM_FILTERS .LE. 0) THEN
	CALL MSG_OUT( 'MESS', 'Uuuummm, no extinction values found',
     :	  STATUS)
        CALL MSG_OUT( 'BLANK', ' ', STATUS)
	GOTO 200
      END IF

*    get a locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

*       map input DATA_ARRAY component
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS)

         CALL NDF_DIM( LOCI, NDIMS, DIMS, ACTDIM, STATUS )

*       set the output image dimensions
         ODIMS( 1) = DIMS( 1)
	 ODIMS( 2) = DIMS( 2)

*       create the output image and get a title for it
         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO, STATUS )
         CALL MSG_OUT( 'BLANK', ' ', STATUS)

*       check for error
         IF( STATUS .EQ. SAI__OK ) THEN

*          find and map output DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                    PNTRO, NELEMENTS, STATUS )

*          check for error before getting other values region and accessing
*          pointers
            IF( STATUS .EQ. SAI__OK ) THEN

*            ask user for filter used in observations
	      CALL PAR_GET0C( 'FILTER', FILTER, STATUS)
	      CALL CHR_UCASE( FILTER )

*            get the extinction value from list for that filter
              MORE = .TRUE.
	      EXTVAL = -999.999
              COUNTER = 1
	      DO WHILE( MORE .AND. COUNTER .LE. NUM_FILTERS)
                IF( FILTER .EQ. FILTERS( COUNTER)) THEN
	          MORE = .FALSE.
	          EXTVAL = EXTVALS( COUNTER)
                  CALL MSG_SETR( 'EXT', EXTVAL)
	          CALL MSG_OUT( 'MESS',
     :	           'Extinction value used = ^EXT', STATUS)
	        END IF
                COUNTER = COUNTER + 1
	      END DO
	      CALL MSG_OUT( 'BLANK', ' ', STATUS)

*            if no extinction value found for filter selected get one
	      IF( EXTVAL .LT. 0.0) THEN
	        CALL MSG_SETC( 'FI', FILTER)
	        CALL MSG_OUT( 'MESS',
     :	      'Error, NO extinction coefficient found for filter ^FI',
     :	          STATUS)
	        CALL MSG_OUT( 'BLANK', ' ', STATUS)
	        CALL MSG_OUT( 'MESS',
     :	          'Please input one now!', STATUS)
	        CALL MSG_OUT( 'BLANK', ' ', STATUS)
	        CALL PAR_GET0R( 'EXTVAL', EXTVAL, STATUS)
	        CALL MSG_OUT( 'BLANK', ' ', STATUS)
	      END IF

*            ask user for airmass to be corrected for
	      CALL PAR_GET0R( 'AIRMASS', AIRMASS, STATUS)

*            pass everything to the airmass correction routine
              CALL AMCORRSUB( DIMS( 1), DIMS( 2), %VAL( PNTRI),
     :	                      ODIMS( 1), ODIMS( 2), %VAL( PNTRO),
     :	                      EXTVAL, AIRMASS, MAGICNO, FACTOR,
     :                        STATUS)
            END IF

*          tell user what has happened
	    CALL MSG_OUT( 'BLANK', ' ', STATUS)
	    CALL MSG_SETR( 'FAC', FACTOR)
	    CALL MSG_SETR( 'AIR', AIRMASS)
	    CALL MSG_OUT( 'MESS',
     :	      'Multiplicative correction factor for airmass ^AIR = ^FAC',
     :	      STATUS)
	    CALL MSG_OUT( 'BLANK', ' ', STATUS)

*           release the ouput image
            CALL NDF_ANNUL( LOCO, STATUS )

*       end of if-no-error-after-getting-output check
         END IF

*       tidy up the input structure
         CALL NDF_ANNUL(  LOCI, STATUS )

*    end of if-no-error-after-getting-input check
      END IF
      GOTO 200

  999 CALL MSG_SETC( 'FIL', EXTFILE)
      CALL ERR_REP( 'MESS',
     : 'Cannot find extinction file ^FIL', STATUS)
      CALL FIO_PUNIT( LUN, STATUS )
      GOTO 200

  998 CALL ERR_REP( 'MESS',
     : 'Error reading from input file', STATUS)
      CLOSE( LUN)
      CALL FIO_PUNIT( LUN, STATUS )
      GOTO 200

  200 END
