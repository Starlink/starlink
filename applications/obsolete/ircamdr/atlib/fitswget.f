	SUBROUTINE FITSWGET( LOCINST, LOCID, LOCTEL, LOCCELL, ARRAY,
     :	                     INST, PLATESCALE, OBSERVERS, ORIGIN,
     :	                     SOFTWARE, HEIGHT, LAT, LONG, TELESCOPE,
     :                       BIAS, CONF, MAX, MIN, DEC, AIRMASS, UT,
     :	                     EQUINOX, EVENMEAN, EVENSTD, EXPO, FPX, FPY,
     :	                     FPZ, FILTER, GAIN, GATE, HA, LOCALTIME,
     :	                     MODE, COADDS, OBJECT, ODDMEAN, ODDSTD,
     :	                     OFFSETDEC, OFFSETRA, RA, READRATE, ST,
     :	                     TEMP, TITLE, XHEAD_ARCSECPMM,
     :	                     XHEAD_DEC_ZERO, XHEAD_RA_ZERO, XHEAD_DEC,
     :                       XHEAD_RA, COMMENTS, STATUS)

	IMPLICIT NONE

*       HISTORY
*       14-JUL-1994 Changed MSG_OUT on error to ERR_REP (SKL@JACH)

	INCLUDE 'SAE_PAR'
        INCLUDE 'DAT_PAR'       ! Necessary for non-VMS

	INTEGER STATUS, NDIMS, DIMS( 2), J

	REAL PLATESCALE, HEIGHT, LAT( 3), LONG( 3), BIAS, MAX, MIN,
     :	     DEC( 3), EQUINOX, EVENMEAN, EVENSTD, EXPO, GAIN, GATE,
     :	     COADDS, ODDMEAN, ODDSTD, RA( 3), READRATE, TEMP, AIRMASS,
     :       UT( 4), UTTEMP( 3), OFFSETDEC, OFFSETRA, FPX,
     :	     FPY, FPZ, HA( 3), ST( 3),
     :	     XHEAD_ARCSECPMM, XHEAD_DEC_ZERO, XHEAD_RA_ZERO,
     :	     XHEAD_DEC, XHEAD_RA

	CHARACTER*( DAT__SZLOC) LOCINST, LOCID, LOCTEL, LOCTEMP

	CHARACTER*( *) LOCCELL

	CHARACTER*( *) CONF, MODE, ARRAY, INST, OBSERVERS, ORIGIN,
     :	               SOFTWARE, TELESCOPE, FILTER, LOCALTIME, OBJECT,
     :                 TITLE

	CHARACTER*80 COMMENTS( 5)

*      test status on entry
	IF( STATUS .NE. SAI__OK) THEN
	  RETURN
	END IF

*      get the primitive parameters defining an observation
	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCINST, 'ARRAY_TYPE', LOCTEMP, STATUS)
	CALL DAT_GETC( LOCTEMP, NDIMS, DIMS, ARRAY, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading ARRAY_TYPE from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCINST, 'INSTRUMENT', LOCTEMP, STATUS)
	CALL DAT_GETC( LOCTEMP, NDIMS, DIMS, INST, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading INSTRUMENT from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCINST, 'PLATE_SCALE', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, PLATESCALE, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading PLATE_SCALE from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCID, 'OBSERVERS', LOCTEMP, STATUS)
	CALL DAT_GETC( LOCTEMP, NDIMS, DIMS, OBSERVERS, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading OBSERVERS from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCID, 'ORIGIN', LOCTEMP, STATUS)
	CALL DAT_GETC( LOCTEMP, NDIMS, DIMS, ORIGIN, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading ORIGIN from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCID, 'SOFTWARE', LOCTEMP, STATUS)
	CALL DAT_GETC( LOCTEMP, NDIMS, DIMS, SOFTWARE, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading SOFTWARE from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCTEL, 'HEIGHT', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, HEIGHT, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading HEIGHT from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 1
	DIMS( 1) = 3
	DIMS( 2) = 0
	CALL DAT_FIND( LOCTEL, 'LATITUDE', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, LAT, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading LATITUDE from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 1
	DIMS( 1) = 3
	DIMS( 2) = 0
	CALL DAT_FIND( LOCTEL, 'LONGITUDE', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, LONG, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading LONGITUDE from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCTEL, 'TELESCOPE', LOCTEMP, STATUS)
	CALL DAT_GETC( LOCTEMP, NDIMS, DIMS, TELESCOPE, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading TELESCOPE from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'BIAS_VOLTAGE', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, BIAS, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading BIAS_VOLTAGE from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'CONFIGURATION', LOCTEMP, STATUS)
	CALL DAT_GETC( LOCTEMP, NDIMS, DIMS, CONF, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading CONFIGURATION from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'AIRMASS', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, AIRMASS, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading AIRMASS from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'DATA_MAX', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, MAX, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading DATA_MAX from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'DATA_MIN', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, MIN, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading DATA_MIN from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 1
	DIMS( 1) = 3
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'DEC', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, DEC, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS', 'Error reading DEC from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	IF( ABS( DEC( 1)) .GT. 90.0) THEN
	  DEC( 1) = -999.0
	  DEC( 2) = 0.0
	  DEC( 3) = 0.0
	END IF

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'EQUINOX', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, EQUINOX, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading EQUINOX from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'EVEN_MEAN', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, EVENMEAN, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading EVEN_MEAN from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'EVEN_STD', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, EVENSTD, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading EVEN_STD from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'EXPOSURE_TIME', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, EXPO, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading EXPOSURE_TIME from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'FILTER', LOCTEMP, STATUS)
	CALL DAT_GETC( LOCTEMP, NDIMS, DIMS, FILTER, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading FILTER from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'GAIN', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, GAIN, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading GAIN from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'GATE_VOLTAGE', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, GATE, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading GATE_VOLTAGE from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'LOCAL_DATETIME', LOCTEMP, STATUS)
	CALL DAT_GETC( LOCTEMP, NDIMS, DIMS, LOCALTIME, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading LOCAL_DATETIME from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'MODE', LOCTEMP, STATUS)
	CALL DAT_GETC( LOCTEMP, NDIMS, DIMS, MODE, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS', 'Error reading MODE from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'NUMBER_COADDS', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, COADDS, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading NUMBER_COADDS from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'OBJECT_NAME', LOCTEMP, STATUS)
	CALL DAT_GETC( LOCTEMP, NDIMS, DIMS, OBJECT, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading OBJECT_NAME from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 1
	DIMS( 1) = 5
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'COMMENTS', LOCTEMP, STATUS)
	CALL DAT_GETC( LOCTEMP, NDIMS, DIMS, COMMENTS, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  DO J = 1, 5
	    COMMENTS( J) = 'NO COMMENTS'
	  END DO
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'ODD_MEAN', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, ODDMEAN, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading ODD_MEAN from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'ODD_STD', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, ODDSTD, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading ODD_STD from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 1
	DIMS( 1) = 3
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'RA', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, RA, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS', 'Error reading RA from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	IF( RA( 1) .LT. 0.0 .OR. RA( 1) .GT. 23.0) THEN
	  RA( 1) = -999.0
	  RA( 2) = 0.0
	  RA( 3) = 0.0
	END IF

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'READOUT_RATE', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, READRATE, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading READOUT_RATE from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'TEMPERATURE', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, TEMP, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading TEMPERATURE from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'TITLE', LOCTEMP, STATUS)
	CALL DAT_GETC( LOCTEMP, NDIMS, DIMS, TITLE, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS', 'Error reading TITLE from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 1
	DIMS( 1) = 3
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'UNIVERSAL_TIME', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, UTTEMP, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading UNIVERSAL_TIME from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	UT( 1) = UTTEMP( 1)
	UT( 2) = UTTEMP( 2)
	UT( 3) = IFIX( UTTEMP( 3))
	UT( 4) = IFIX( ( UTTEMP( 3) - UT( 3))*1000.0 + 0.5)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'OFFSET_RA', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, OFFSETRA, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                   'Error reading OFFSET_RA from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'OFFSET_DEC', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, OFFSETDEC, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading OFFSET_DEC from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'FPX', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, FPX, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS', 'Error reading FPX from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'FPY', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, FPY, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS', 'Error reading FPY from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'FPZ', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, FPZ, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS', 'Error reading FPZ from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 1
	DIMS( 1) = 3
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'HOUR_ANGLE', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, HA, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading HOUR_ANGLE from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	IF( HA( 1) .LT. -12.0 .OR. HA( 1) .GT. 12.0) THEN
	  HA( 1) = -999.0
	  HA( 2) = 0.0
	  HA( 3) = 0.0
	END IF

	NDIMS = 1
	DIMS( 1) = 3
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'SIDERIAL_TIME', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, ST, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading SIDERIAL_TIME from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	IF( ST( 1) .LT. 0.0 .OR. ST( 1) .GT. 23.0) THEN
	  ST( 1) = -999.0
	  ST( 2) = 0.0
	  ST( 3) = 0.0
	END IF

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'XHEAD_ARCSECPMM', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, XHEAD_ARCSECPMM, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading XHEAD_ARCSECPMM from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'XHEAD_DEC_ZERO', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, XHEAD_DEC_ZERO, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading XHEAD_DEC_ZERO from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'XHEAD_RA_ZERO', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, XHEAD_RA_ZERO, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading XHEAD_RA_ZERO from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'XHEAD_DEC', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, XHEAD_DEC, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading XHEAD_DEC from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	NDIMS = 0
	DIMS( 1) = 0
	DIMS( 2) = 0
	CALL DAT_FIND( LOCCELL, 'XHEAD_RA', LOCTEMP, STATUS)
	CALL DAT_GETR( LOCTEMP, NDIMS, DIMS, XHEAD_RA, STATUS)
	IF( STATUS .NE. SAI__OK) THEN
	  CALL ERR_REP( 'MESS',
     :                  'Error reading XHEAD_RA from data file',
     :	                STATUS)
          CALL ERR_FLUSH( STATUS )
	END IF
	STATUS = SAI__OK
	CALL DAT_ANNUL( LOCTEMP, STATUS)
	STATUS = SAI__OK
	call err_annul( status)

	END
