*+  SLICE - Lists values in a dataset within a given data range
      SUBROUTINE SLICE( STATUS )
*
*   Description :
*
*     Lists all values (with their item numbers) of a N-D array lying within
*     a user-specified range. The array name may be entered explicitly, or
*     the name of a component containing a DATA_ARRAY. Arrays are mapped so
*     there is no size limitation.
*
*   Parameters :
*
*     INP = UNIV(R)
*         Input data object
*     RANGE_MIN = REAL(R)
*         Minimum value for inclusion in slice
*     RANGE_MAX = REAL(R)
*         Maximum value for inclusion in slice
*     DEVICE = CHAR(R)
*         Output ascii device
*
*   Method :
*     Straightforward.
*   Bugs :
*   Authors :
*
*     Trevor Ponman  (BHVAD::TJP)
*     David Allan    (BHVAD::DJA)
*
*   History :
*
*     14 Apr 86 : Original
*     13 Apr 87 : V0.6-1 Quality mapped as integer (TJP)
*     22 Jul 88 : V1.0-0 New STARLINK standards - Asterix88 upgrade. (DJA)
*      6 Oct 88 : V1.0-1 Range selection tidied up. (DJA)
*      1 Mar 89 : V1.0-2 Removed looping over range parameters. (DJA)
*      9 Aug 93 : V1.7-0 Use UTIL_SPOOL to spool output file. Generalised
*                        to N-dimensions (DJA)
*     28 Jul 94 : V1.7-1 Output using AIO routines - DEV parameter to
*                        use standard system. (DJA)
*
*   Type Definitions :
*
      IMPLICIT NONE
*
*   Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'

*   Status :
      INTEGER			STATUS          ! Run-time status
*
*   Local Variables :
*
      CHARACTER*(DAT__SZLOC)  ILOC		! Input data object locator
      CHARACTER*(DAT__SZTYP)  OBTYPE		! Type of data object
      CHARACTER*80            UNITS             ! Units of data
      CHARACTER*200           PATH,FILE         ! Trace information
      CHARACTER*50		TBUF			! Output text buffer

      REAL		      	RMIN,RMAX		! Min & max values of slice
      REAL		      	D			! Data value
      REAL 		      DQMIN,DQMAX	! Data min & max with quality
      REAL 		      DMIN,DMAX		! Data min & max without quality

      INTEGER		      DIMSD(DAT__MXDIM) ! Sizes of data dimensions
      INTEGER		      DIMSQ(DAT__MXDIM) ! Sizes of quality dimensions
      INTEGER			DPTR		! Pointer to input data array
      INTEGER			I, K		! Loop counters
      INTEGER		      INDICES(DAT__MXDIM) ! Array indices of point
      INTEGER			OCI		! AIO channel id
      INTEGER			NELM		! No.of data points
      INTEGER			NDIMD		! Dimensionality of data
      INTEGER			NDIMDR		! Dimensionality of data - 1
      INTEGER			NDIMQ		! Dimensionality of quality
      INTEGER			NSLICE		! No.of good points in slice
      INTEGER			NBAD		! No.of bad points in slice
      INTEGER			QPTR		! Pointer to data quality
      INTEGER			WIDTH			! Width of o/p stream

      LOGICAL                   OK              ! General validity test
      LOGICAL			PRIM		! Input object primitive?
      LOGICAL			Q		! Quality value
      LOGICAL			QUAL_OK		! Quality array available?
      LOGICAL		        SOMEBAD         ! Any bad quality points?
*
*    Version id:
*
      CHARACTER*21		VERSION
	PARAMETER	        ( VERSION='SLICE Version 1.7-1' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version
      CALL MSG_PRNT( VERSION )

*    Initialise Asterix common blocks
      CALL AST_INIT()

*    Obtain data object, access and check it
      CALL USI_ASSOCI( 'INP', 'READ', ILOC, PRIM, STATUS )

      CALL USI_SHOW( 'Input dataset {INP}', STATUS )

      CALL DAT_TYPE( ILOC, OBTYPE, STATUS )
      CALL MSG_SETC( 'OTYPE', OBTYPE )
      CALL MSG_PRNT( 'Data object is of type ^OTYPE' )

*    Is the input object the array required?
      CALL BDA_CHKDATA( ILOC, OK, NDIMD, DIMSD, STATUS )
      IF ( .NOT. OK ) THEN
	 CALL MSG_PRNT( 'ERROR : No data object found.' )
         STATUS = SAI__ERROR

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map in data. and quality - find range
      CALL BDA_MAPDATA( ILOC, 'READ', DPTR, STATUS )
      CALL ARR_SUMDIM( NDIMD, DIMSD, NELM )

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      CALL BDA_CHKQUAL( ILOC, QUAL_OK, NDIMQ, DIMSQ, STATUS )

      IF ( QUAL_OK .AND. ( STATUS .EQ. SAI__OK ) ) THEN
	 CALL BDA_MAPLQUAL( ILOC, 'READ', SOMEBAD, QPTR, STATUS )
      END IF

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
	 QUAL_OK = .FALSE.
      END IF

      CALL ARR_RANG1R( NELM, %VAL(DPTR), DMIN, DMAX, STATUS )

      IF ( QUAL_OK .AND. SOMEBAD ) THEN
	 CALL ARR_RANG1RLQ( NELM, %VAL(DPTR), %VAL(QPTR), DQMIN, DQMAX,
     :                      STATUS )
      ELSE
         IF ( .NOT. QUAL_OK ) THEN
            CALL MSG_PRNT( 'No data quality available.' )
	 END IF
         QUAL_OK = .FALSE.
      END IF

*    Try to get data units
      UNITS = 'data units'
      IF (.NOT. PRIM ) THEN
         CALL BDA_GETUNITS( ILOC, UNITS, STATUS )
         IF ( (UNITS .GT. ' ') .OR. (STATUS.NE.SAI__OK)) THEN
            STATUS = SAI__OK
            UNITS = 'data units'
         END IF
      END IF

*    Inform user about length of data set and data range
      CALL MSG_SETI( 'NELM', NELM )
      CALL MSG_PRNT ( '^NELM data points entered' )
      CALL MSG_SETR( 'MIN', DMIN )
      CALL MSG_SETR( 'MAX', DMAX )
      CALL MSG_SETC( 'UNITS', UNITS )
      CALL MSG_PRNT( 'Data range is ^MIN to ^MAX ^UNITS' )

      IF ( QUAL_OK ) THEN
        CALL MSG_SETR( 'MIN', DQMIN )
        CALL MSG_SETR( 'MAX', DQMAX )
        CALL MSG_SETC( 'UNITS', UNITS )
        CALL MSG_PRNT( 'Range of good quality data is '//
     :                             '^MIN to ^MAX ^UNITS' )
        CALL PAR_DEF0R( 'RANGE_MIN', DQMIN, STATUS )
        CALL PAR_DEF0R( 'RANGE_MAX', DQMAX, STATUS )
      ELSE
        CALL PAR_DEF0R( 'RANGE_MIN', DMIN, STATUS )
        CALL PAR_DEF0R( 'RANGE_MAX', DMAX, STATUS )
      END IF

*    Get range of data
      CALL PAR_GET0R('RANGE_MIN',RMIN,STATUS)
      CALL PAR_GET0R('RANGE_MAX',RMAX,STATUS)

      IF ( STATUS .NE. SAI__OK ) GOTO 99

      IF ( RMIN .LT. RMAX ) THEN
         IF ( RMIN.LT.DMIN ) THEN
            RMIN = DMIN
            CALL MSG_SETR( 'TMIN', DMIN )
            CALL MSG_PRNT( 'WARNING: Min value too small '//
     :                              '- increased to ^TMIN' )
         END IF

         IF ( RMAX .GT. DMAX ) THEN
            RMAX = DMAX
            CALL MSG_SETR( 'TMAX', DMAX )
            CALL MSG_PRNT( 'WARNING: Max value too large '//
     :                              '- decreased to ^TMAX' )
         END IF

         IF ( QUAL_OK ) THEN
            IF ((RMIN.LT.DQMIN).OR.(RMAX.GT.DQMAX)) THEN
               CALL MSG_PRNT( 'WARNING: Range will include'
     :                        //'some bad quality points' )
            END IF
         END IF

      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'Lower bound must be less than upper'
     :                                      //' bound.', STATUS )
         GOTO 99

      END IF

*    Open the output device
      CALL AIO_ASSOCO( 'DEVICE', 'LIST', OCI, WIDTH, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Output header
      CALL AIO_TITLE( OCI, VERSION, STATUS )

*    Report file name
      CALL HDS_TRACE( ILOC, I, PATH, FILE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        CALL AIO_BLNK( OCI, STATUS )
        CALL MSG_SETC( 'FILE', PATH )
        CALL AIO_WRITE( OCI, 'Analysing data set ^FILE', STATUS )
      END IF
      CALL AIO_BLNK( OCI, STATUS )
      CALL MSG_FMTR( 'MIN', '1PG14.6', RMIN )
      CALL MSG_FMTR( 'MAX', '1PG14.6', RMAX )
      CALL AIO_WRITE( OCI, ' Slice limits : ^MIN to ^MAX', STATUS )

*    List points within slice
      NBAD   = 0
      NSLICE = 0

      CALL AIO_BLNK( OCI, STATUS )
      CALL AIO_BLNK( OCI, STATUS )
      CALL AIO_WRITE( OCI, '       Data value           Index', STATUS )
      CALL AIO_WRITE( OCI, '    **********************************'/
     :                /'**********', STATUS )

      NDIMDR = NDIMD - 1
      DO I = 1, NELM

*      Get data value
        CALL ARR_ELEM1R( DPTR, NELM, I, D, STATUS )

*      In user supplied range?
	IF ( (D .GE. RMIN) .AND. (D.LE.RMAX))THEN

*        Increment counter
	  NSLICE = NSLICE + 1

*        Is this a good quality point?
	  IF ( QUAL_OK ) THEN
            CALL ARR_ELEM1L( QPTR, NELM, I, Q, STATUS )
          ELSE
            Q = .TRUE.
	  END IF

*        If its a good point...
          IF ( Q ) THEN

*          Recover array indices
            CALL UTIL_INDEX( NDIMD, DIMSD, I, INDICES )

*          Write line containing value and indices
	    IF ( NDIMD .EQ. 1 ) THEN
	      WRITE( TBUF, 15 ) D, INDICES(1)
 15           FORMAT( 1PG14.6, 3X, I7 )
	    ELSE
              WRITE( TBUF, 20 ) D, INDICES(1), (INDICES(K),K=2,NDIMD)
 20           FORMAT( 1PG14.6, 3X, I7, <NDIMDR>(', ',I7) )
	    END IF
            CALL AIO_IWRITE( OCI, 6, TBUF, STATUS )

          ELSE
            NBAD = NBAD + 1

          END IF
	END IF
      END DO

*    Number of points
      CALL AIO_BLNK( OCI, STATUS )
      CALL AIO_BLNK( OCI, STATUS )
      CALL MSG_SETI( 'NP', NSLICE )
      CALL AIO_WRITE( OCI, 'Total of ^NP points', STATUS )

*    Number of bad points (if any)
      IF ( QUAL_OK ) THEN
        CALL MSG_SETI( 'NB', NBAD )
        CALL AIO_WRITE( OCI, '^NB bad quality points omitted from'/
     :                                          /' slice', STATUS )
      END IF

*    Close file and spool
      CALL AIO_CANCL( 'DEVICE', STATUS )

*    Tidy up & exit
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END
