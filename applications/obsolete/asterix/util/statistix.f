*+  STATISTIX - Calculates statistics for an N dimensional data object
      SUBROUTINE STATISTIX( STATUS )
*
*   Description :
*
*     The double precision NAg routine G01AAF is used to calculate the mean,
*     standard deviation, coefficients of skewness and kurtosis, and the
*     maximum and minimum values of an N dimensional primitive data object, or
*     DATA_ARRAY component of an NDF.
*
*     Features
*     --------
*         a) Will list values deviating from the mean by more than
*            n standard deviations, or n individual errors on the points,
*            whichever is appropriate. The statistics are recalculated
*            for the reduced sample.
*
*         b) If input is an NDF, then may select to use the
*            VARIANCE , &/or QUALITY components, if present.
*
*    Parameters :
*
*     INPUT       = UNIV(R)
*                   Input data object
*     USEERRORS   = _LOGICAL(R)
*                   Asks if data errors are to be used
*     USEQUALITY  = _LOGICAL(R)
*                   Asks if data quality is to be used
*     LOOP        = _LOGICAL(R)
*                   Loop over rejecting high sigma points
*     SIMPLE      = _LOGICAL(R)
*                   Don't calculate kurtosis and skewness
*
*    Method :
*     Calls NAg routine G01AAF.
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Phillip Andrews (BHVAD::PLA)
*     David Allan (BHVAD::DJA)
*     Simon Duck (BHVAD::SRD)
*
*    History :
*
*     10 Oct 88 : V1.0-1 Extra parameter added to select looping mode
*     24 Aug 89 : V1.0-2 Structure definitions removed to STATISTIX_STR.
*                        Alterations to enable compilation under Fortran
*                        V5.2 - removal of structure references in array
*                        declarations (DJA)
*     16 Feb 90 : V1.2-0 Extensive bug fixes and code tidying. Checks
*                        for zero variance values on structured input.
*                        Redundant code removed. (DJA)
*      1 Mar 90 : V1.2-1 Removed where chisquare was calculated  on 2nd and
*                        subsequent iterations when no variance present. (DJA)
*     28 Mar 90 : V1.2-2 Now uses BDA_MAPTVAR to map variance to double
*                        precision directly (DJA)
*      6 Jul 90 : V1.2-3 Prints out one decimal place in numbers (DJA)
*     24 Jul 90 : V1.2-4 Calculates equivalent normal Z (SRD)
*     16 Jan 91 : V1.3-0 Bug fixed in QUAL,NOVAR case (DJA)
*     25 Jul 91 : V1.5-0 And another in the QUAL and VAR case (DJA)
*     23 Apr 92 : V1.6-0 SIMPLE option removes skewness and kurtosis
*                        calculation and is 3 times faster (DJA)
*     10 Jul 92 : V1.6-1 Prints pixel indices of min and max pixels (DJA)
*     15 Sep 92 : V1.6-2 Warns if using slice. Bug fix printing min/max
*                        positions. Traps huge weights. (DJA)
*      4 May 94 : V1.7-0 Updated i/o to AIO (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Status :
*
      INTEGER                STATUS
*
*    Local variables :
*
      CHARACTER              ILOC*(DAT__SZLOC)  ! Input object
      CHARACTER*150          PATH               ! HDS path to input object
      CHARACTER*150          INPUTFILE          ! Name of input container file

      INTEGER                DIMS(DAT__MXDIM)   ! Input dimensions
      INTEGER                TDIMS(DAT__MXDIM)  ! Auxilliary input dimensions

      INTEGER                AXPTR(DAT__MXDIM)  ! Axis data ptr's
      INTEGER                IDPTR              ! Data array ptr
      INTEGER                IQPTR              ! Data quality ptr
      INTEGER                IVPTR              ! Data variance ptr
      INTEGER                IAX                ! Loop over axes
      INTEGER                NDIM               ! Input dimensionality
      INTEGER                NELM               ! Total number of data elements
      INTEGER                NUMLEVELS          ! Number of levels in HDS path.
      INTEGER                OCH                ! Output channel
      INTEGER                OUTWIDTH           ! 80 if terminal,132 otherwise.
      INTEGER                TNDIM              ! Aux dimensionality
      INTEGER                WPTR               ! Weights array ptr

      LOGICAL                BADQUAL            ! Any of quality flags set?
      LOGICAL                DATAOK, VAROK      ! Various input
      LOGICAL                IPRIM              ! Input object primitive?
      LOGICAL                QUALOK, AXOK       ! objects there?
      LOGICAL                LOOP               ! Loop with sigma rejection?
      LOGICAL                SIMPLE             ! Simple mode
*
*    Version id :
*
      CHARACTER*25           VERSION
        PARAMETER            ( VERSION = 'STATISTIX Version 1.8-0' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version anouncement
      CALL MSG_PRNT( VERSION )

*    Start ASTERIX
      CALL AST_INIT( STATUS )

*    Obtain name of data file, and get a locator to it.
      CALL USI_ASSOCI( 'INP', 'READ', ILOC, IPRIM, STATUS )
      CALL HDS_TRACE( ILOC, NUMLEVELS, PATH, INPUTFILE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Report object name
      CALL USI_SHOW( 'Input {INPUT}', STATUS )

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Determine output device.
      CALL AIO_ASSOCO( 'DEVICE', 'LIST', OCH, OUTWIDTH, STATUS )
      IF ( OUTWIDTH .EQ. 132 ) THEN
        CALL AIO_WRITE( OCH, 'Statistics of '//INPUTFILE, STATUS )
        CALL AIO_BLNK( OCH, STATUS )
      END IF

*    Simple mode?
      CALL USI_GET0L( 'SIMPLE', SIMPLE, STATUS )

*    Look for components.
      CALL BDA_CHKDATA( ILOC, DATAOK, NDIM, DIMS, STATUS )

      IF ( DATAOK ) THEN

        CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*      Look for quality - ask user if present
        CALL BDA_CHKQUAL( ILOC, QUALOK, TNDIM, TDIMS, STATUS )
        IF ( QUALOK ) THEN
          CALL USI_GET0L( 'USEQUALITY', QUALOK, STATUS )
        END IF

*      Look for variance - ask user if present
        CALL BDA_CHKVAR( ILOC, VAROK, TNDIM, TDIMS, STATUS )
        IF ( VAROK ) THEN
          CALL USI_GET0L( 'USEERRORS', VAROK, STATUS)
        END IF

*      Check axis values
        DO IAX = 1, NDIM
          CALL BDA_CHKAXIS( ILOC, IAX, AXOK, STATUS )
        END DO

*      Map the axis values
        IF ( AXOK ) THEN
          DO IAX = 1, NDIM
            CALL BDA_MAPAXVAL( ILOC, 'READ', IAX, AXPTR(IAX), STATUS )
          END DO
        END IF

      ELSE

        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unable to find numeric data in input',
     :                                                    STATUS )

      END IF

*    Find out if looping is required
      CALL USI_GET0L( 'LOOP', LOOP, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map data as _DOUBLE
      CALL BDA_MAPTDATA( ILOC, '_DOUBLE', 'READ', IDPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT ( 'ERROR : Unable to map input data' )
        GOTO 99
      END IF

*    Obtain dynamic memory for weight array
      CALL DYN_MAPD( 1, NELM, WPTR, STATUS )

*    Map variance
      IF ( VAROK ) THEN
        CALL BDA_MAPTVAR( ILOC, '_DOUBLE', 'READ', IVPTR, STATUS )
      END IF

*    Map QUALITY as a logical array.
      IF ( QUALOK ) THEN
        CALL BDA_MAPLQUAL (ILOC, 'READ', BADQUAL, IQPTR, STATUS )
        IF ( .NOT. BADQUAL ) THEN
          CALL BDA_UNMAPLQUAL( ILOC, STATUS )
          QUALOK = .FALSE.
        END IF
      END IF

*    Pad extra dimensions to 7D
      DO IAX = NDIM+1, DAT__MXDIM
        DIMS(IAX) = 1
      END DO

*    Do the statistics
      CALL STATISTIX_INT( NELM, %VAL(IDPTR), %VAL(WPTR), VAROK,
     :                  %VAL(IVPTR), QUALOK, %VAL(IQPTR), NDIM,
     :           DIMS, AXOK, AXPTR, SIMPLE, OCH, LOOP, STATUS )

*    Annul PAR__NULL in loop mode
      IF ( LOOP .AND. (STATUS.EQ.PAR__NULL) ) THEN
        CALL ERR_ANNUL( STATUS )
      END IF

*    Close output channel
      CALL AIO_CANCL( 'DEVICE', STATUS )

*    Release dataset
      CALL BDA_RELEASE( ILOC, STATUS )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  STATISTIX_INT - Internal routine to evaluate numeric statistics
      SUBROUTINE STATISTIX_INT( N, DATA, WEIGHTS, VAROK, VAR, QUALOK,
     :                          QUAL, NDIM, DIMS, AXOK, AXPTR,
     :                               SIMPLE, OCH, LOOP, STATUS )
*
*    Description :
*
*     Calls the NAg routine G01AAF to calculate statistics. Observed data
*     errors and data quality are used if supplied. If LOOP is specified,
*     the user is asked if data > n standard deviations from the mean are
*     to be ignored. Ignored points are then listed if required,  and new
*     statistics recalculated on the remaining sample.
*
*    Parameters :
*
*     SIGMA = _DOUBLE(R)
*             Point rejection threshold
*
*    Method :
*
*     Weighting is used, in order to allow exclusion of points > n standard
*     deviations from the mean, and handle data errors and quality.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David Allan (BHVAD::DJA)
*
*    History :
*
*      17 Feb 90 : Original (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Status :
*
      INTEGER                STATUS
*
*    Import :
*
      DOUBLE PRECISION       DATA(*)            ! Mapped DATA_ARRAY
      DOUBLE PRECISION       VAR(*)             ! Mapped VARIANCE
      DOUBLE PRECISION       WEIGHTS(*)         ! Mapped weights array
      LOGICAL                QUAL(*)            ! Mapped QUALITY

      INTEGER                AXPTR(*)           ! Ptrs to axis data
      INTEGER                NDIM               ! Object dimensionality
      INTEGER                DIMS(*)            ! Object shape
      INTEGER                N                  ! Number of data items
      INTEGER                OCH                ! Output channel

      LOGICAL                AXOK               ! Axis data ok?
      LOGICAL                LOOP               ! Loop over data?
      LOGICAL                QUALOK             ! Use QUALITY array ?
      LOGICAL                VAROK              ! Use VARIANCE array?
      LOGICAL                SIMPLE             ! Simple mode
*
*    Local Constants :
*
      DOUBLE PRECISION       IGNORE             ! Ignore weight flag
         PARAMETER           ( IGNORE = 0.0D0 )
      DOUBLE PRECISION       MAXWEIGHT
         PARAMETER           ( MAXWEIGHT = 1.3043818D19 )
*
*    Local variables :
*
      DOUBLE PRECISION       SIGMA              ! Criterion for ignoring data points.
      DOUBLE PRECISION       MEAN               ! Mean of data points.
      DOUBLE PRECISION       SUM                ! Sum of data points.
      DOUBLE PRECISION       STDDEV             ! Standard deviation.
      DOUBLE PRECISION       SKEWNESS           ! Coeff. of skewness.
      DOUBLE PRECISION       KURTOSIS           ! Coeff. of kurtosis.
      DOUBLE PRECISION       MINVALUE           ! Of valid data points.
      DOUBLE PRECISION       MAXVALUE           ! Of valid data points.
      DOUBLE PRECISION       WTSUM              ! Sum of weights.
      DOUBLE PRECISION       MEANERR            ! Uncertainty of the mean value.
      DOUBLE PRECISION       CHISQUARE          ! Reduced Chi squared for fit of data to mean.
      DOUBLE PRECISION       ENZ                ! Equivalent normal Z

      INTEGER                I                  ! DO loop dummy
      INTEGER                MINP, MAXP         ! Min and max pixels
      INTEGER                NBAD               ! Number of bad points found
      INTEGER                NBIGW              ! Number of big weights
      INTEGER                NVALID             ! # pts used by NAG routine.

      LOGICAL                DISPLAY            ! Display ignored points?
      LOGICAL                INPUT              ! Loop over input until valid.
      LOGICAL                USEWEIGHT          ! Use weights?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Default is to use weights
      USEWEIGHT = .TRUE.
      NBAD = 0

*    Set up weights
      NBIGW = 0
      IF ( VAROK .AND. QUALOK ) THEN

        DO I = 1, N
          IF ( QUAL(I) ) THEN
            IF ( VAR(I) .EQ. 0.0 ) THEN
              WEIGHTS(I) = IGNORE
            ELSE
              WEIGHTS(I) = 1.0D0 / VAR(I)
              IF ( WEIGHTS(I) .GT. MAXWEIGHT ) THEN
                WEIGHTS(I) = IGNORE
                NBIGW = NBIGW + 1
              END IF
            END IF
          ELSE
            WEIGHTS(I) = IGNORE
            NBAD = NBAD + 1
          END IF
        END DO

      ELSE IF ( VAROK ) THEN
        DO I = 1, N
          IF ( VAR(I) .EQ. 0.0 ) THEN
            WEIGHTS(I) = IGNORE
          ELSE
            WEIGHTS(I) = 1.0D0 / VAR(I)
            IF ( WEIGHTS(I) .GT. MAXWEIGHT ) THEN
              WEIGHTS(I) = IGNORE
              NBIGW = NBIGW + 1
            END IF
          END IF
        END DO

      ELSE IF ( QUALOK ) THEN
        DO I = 1, N
          IF ( QUAL(I) ) THEN
            WEIGHTS(I) = 1.0D0
          ELSE
            WEIGHTS(I) = IGNORE
            NBAD = NBAD + 1
          END IF
        END DO

      ELSE

*      Don't use weights
        USEWEIGHT = .FALSE.

      END IF

*    Output No. of bad data points.
      IF ( NBAD .GT. 0 ) THEN
        CALL AIO_BLNK( OCH, STATUS )
        CALL MSG_SETI( 'NB', NBAD )
        CALL AIO_WRITE( OCH, ' ^NB points excluded by bad data quality',
     :                  STATUS )
        CALL AIO_BLNK( OCH, STATUS )
      END IF

*    Any big weights
      IF ( NBIGW .GT. 0 ) THEN
        CALL AIO_BLNK( OCH, STATUS )
        CALL MSG_SETI( 'NB', NBIGW )
        CALL AIO_WRITE( OCH, ' ^NB points excluded as weights too '/
     :                  /'large to square', STATUS )
        CALL AIO_BLNK( OCH, STATUS )
      END IF
      NBAD = NBAD + NBIGW

*    Do statistics
      CALL STATISTIX_CALC( SIMPLE, N, DATA, VAROK, USEWEIGHT, WEIGHTS,
     :                     MEAN, SUM, STDDEV, MEANERR, SKEWNESS,
     :                     KURTOSIS, MINVALUE, MAXVALUE, MINP, MAXP,
     :                     WTSUM, NVALID, CHISQUARE, ENZ, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Display results
      CALL STATISTIX_DISPLAY( SIMPLE, NDIM, DIMS, N, USEWEIGHT, VAROK,
     :                        MEAN, SUM,
     :                        STDDEV, SKEWNESS, KURTOSIS, MINVALUE,
     :                        MAXVALUE, MINP, MAXP, MEANERR, NVALID,
     :                        CHISQUARE, ENZ, OCH, STATUS )

*    Abort if no looping
      IF ( .NOT. LOOP ) GOTO 99

      SIGMA = 100.0D0 !Any large value will do to enter while loop.

      DO WHILE ( (SIGMA.GT.0.0D0) .AND. (STATUS.EQ.SAI__OK) )

*      Ask user for value of sigma
        SIGMA = 0.0D0
        INPUT = .TRUE.
        DO WHILE ( INPUT )
          CALL USI_GET0D('SIGMA', SIGMA, STATUS )
          CALL USI_CANCL('SIGMA', STATUS )

          IF ((STATUS .EQ. PAR__ABORT) .OR.(STATUS .EQ. PAR__NULL)) THEN
            GOTO 99
          ELSE IF (STATUS .EQ. SAI__OK) THEN
            INPUT = .FALSE.
          ELSE
            STATUS = SAI__OK
          END IF
        END DO

*      Ask user if output of ignored values is required.
        INPUT = .TRUE.
        DO WHILE ( INPUT )
          CALL USI_GET0L( 'DISPLAY', DISPLAY, STATUS)
          IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__OK
            CALL USI_CANCL( 'DISPLAY', STATUS )
          ELSE
            INPUT = .FALSE.
          END IF
        END DO

*      Check that sigma is non zero, and status is good
        IF ((SIGMA .GT. 0.0D0) .AND. (STATUS .EQ. SAI__OK)) THEN

*        Output sigma to be ignored, and header for ignored list.
          CALL MSG_SETD( 'SIG', SIGMA )
          CALL AIO_WRITE( OCH, ' Ignoring points > ^SIG standard '/
     :                    /'deviations from the mean.', STATUS )

*        Weights already defined?
          IF ( USEWEIGHT ) THEN

*          Variance present
            IF ( VAROK ) THEN
              DO I = 1, N
                IF ( WEIGHTS(I) .GT. 0.0 ) THEN
                  IF ( ABS(DATA(I)-MEAN) .GE. SQRT(VAR(I))*SIGMA ) THEN
                    WEIGHTS(I) = IGNORE
                    IF ( DISPLAY ) THEN
                      CALL STATISTIX_IGNOREOUT( NDIM, DIMS, AXOK,
     :                           AXPTR, DATA(I), I, OCH, STATUS )
                    END IF
                  END IF
                END IF
              END DO

*          Weights must've been created because of quality
            ELSE
              DO I = 1, N
                IF ( WEIGHTS(I) .GT. 0.0 ) THEN
                  IF ( ABS(DATA(I)-MEAN) .GE. STDDEV*SIGMA ) THEN
                    WEIGHTS(I) = IGNORE
                    IF ( DISPLAY ) THEN
                      CALL STATISTIX_IGNOREOUT( NDIM, DIMS, AXOK,
     :                           AXPTR, DATA(I), I, OCH, STATUS )
                    END IF
                  END IF
                END IF
              END DO

            END IF

          ELSE

*          Use weights from now on
            USEWEIGHT = .TRUE.

*          Assign weight to zero or one depending on deviation from
*          from mean
            DO I = 1, N
              IF ( ABS(DATA(I)-MEAN) .GE. STDDEV*SIGMA ) THEN
                WEIGHTS(I) = IGNORE
                IF ( DISPLAY ) THEN
                  CALL STATISTIX_IGNOREOUT( NDIM, DIMS, AXOK,
     :                       AXPTR, DATA(I), I, OCH, STATUS )
                END IF
              ELSE
                WEIGHTS(I) = 1.0D0
              END IF
            END DO

          END IF

*        Do statistics - always use weights
          CALL STATISTIX_CALC( SIMPLE, N, DATA, VAROK, USEWEIGHT,
     :                 WEIGHTS, MEAN, SUM, STDDEV, MEANERR, SKEWNESS,
     :                 KURTOSIS, MINVALUE, MAXVALUE, MINP, MAXP,
     :                 WTSUM, NVALID, CHISQUARE, ENZ, STATUS )

*        ...and display
      CALL STATISTIX_DISPLAY( SIMPLE, NDIM, DIMS, N, USEWEIGHT, VAROK,
     :                        MEAN, SUM,
     :                  STDDEV, SKEWNESS, KURTOSIS, MINVALUE, MAXVALUE,
     :                   MINP, MAXP, MEANERR, NVALID, CHISQUARE, ENZ,
     :                                                    OCH, STATUS )

        END IF

      END DO

*    Abort point
 99   CONTINUE

      END



*+  STATISTIX_IGNOREOUT - Writes details of an ignored point to OCH
      SUBROUTINE STATISTIX_IGNOREOUT( NDIM, DIMS, AXOK, AXPTR,
     :                           VALUE, ELEMENT, OCH, STATUS )
*    Description :
*
*     Outputs details of the ELEMENT'th point in the vectorised array
*     of dimensions DIMS[NDIM]. Axis values are used if present, else
*     pixel numbers are used.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David Allan (BHVAD::DJA)
*
*    History :
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      DOUBLE PRECISION       VALUE              ! Point value in data array

      INTEGER                OCH                ! Output channel
      INTEGER                NDIM, DIMS(*)      ! Object shape
      INTEGER                AXPTR(*)           ! Ptrs to axis data
      INTEGER                ELEMENT            ! Indices in data array

      LOGICAL                AXOK               ! Axis data ok?
*
*    Status :
*
      INTEGER                STATUS
*
*    Local variables :
*
      CHARACTER*79		OBUF			! Output buffer

      REAL                   ORD                ! Ordinate value for element

      INTEGER                IAX                ! Loop over axes
      INTEGER                INDEX(DAT__MXDIM)  ! Specifies position of element
                                                ! within data array
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Calculate index values
      CALL UTIL_INDEX( NDIM, DIMS, ELEMENT, INDEX )

*    Display index/ coordinate values.
      WRITE( OBUF, '(1X,A7,1PG15.7)' ) 'Value = ', VALUE
      CALL AIO_WRITE( OCH, OBUF, STATUS )

*    Get axis value on each dimension if axes defined
      IF ( AXOK ) THEN
        DO IAX = 1, NDIM
          CALL ARR_ELEM1R( AXPTR(IAX), DIMS(IAX), INDEX(IAX), ORD,
     :                                                    STATUS )
          WRITE( OBUF, '(4X,A9,I1,A3,1PG15.7)' ) 'Ordinate(',IAX,
     :                                        ') =', ORD
          CALL AIO_WRITE( OCH, OBUF, STATUS )
        END DO
      ELSE
        DO IAX = 1, NDIM
          WRITE( OBUF, '(4X,A6,I1,A3,I10)' ) 'Index(',IAX,') =',
     :                                                    INDEX(IAX)
          CALL AIO_WRITE( OCH, OBUF, STATUS )
        END DO
      END IF

      CALL AIO_BLNK( OCH, STATUS )

      END



*+  STATISTIX_DISPLAY - writes the results to appropriate unit.
      SUBROUTINE STATISTIX_DISPLAY( SIMPLE, NDIM, DIMS, N, WTERR, VAROK,
     :                              MEAN, SUM, STDDEV, SKEWNESS,
     :                              KURTOSIS, MINVALUE, MAXVALUE, MINP,
     :                              MAXP,
     :               MEANERR, NVALID, CHISQUARE, ENZ, OCH, STATUS )
*    Description :
*     Displays the statistics.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Phillip Andrews (BHVAD::PLA)
*
*    History :
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      LOGICAL                SIMPLE             ! Simple mode?
      INTEGER                NDIM               ! Input dimensionality
      INTEGER                DIMS(DAT__MXDIM)   ! Input dimensions
      INTEGER                OCH                ! Output channel
      INTEGER                N                  ! # points in input
      INTEGER                NVALID             ! # points in statistics

      DOUBLE PRECISION       MEAN               ! Mean of data points
      DOUBLE PRECISION       SUM                ! Sum of data points
      DOUBLE PRECISION       STDDEV             ! Standard deviation
      DOUBLE PRECISION       SKEWNESS           ! Coeff. of skewness
      DOUBLE PRECISION       KURTOSIS           ! Coeff. of kurtosis
      DOUBLE PRECISION       MINVALUE           ! Of valid data points
      DOUBLE PRECISION       MAXVALUE           ! Of valid data points
      INTEGER                MINP,MAXP          ! Min and max pixels
      DOUBLE PRECISION       MEANERR            ! Error on the mean
      DOUBLE PRECISION       CHISQUARE          ! Reduced chi squared for fit of mean to data.
      DOUBLE PRECISION       ENZ                ! Equivalent normal Z

      LOGICAL                VAROK              ! Data errors present
      LOGICAL                WTERR              ! If true then calc error on weighted mean
*
*    Status :
*
      INTEGER                STATUS
*
*    Local variables :
*
      CHARACTER*79		OBUF			! Output buffer
      CHARACTER*24           	PSTR               	! Pixel index string

      DOUBLE PRECISION       	ERRORONSTDDEV      	! Error on std. dev.
*-

  5   FORMAT( 7X, '***********************************************',
     :                                          '****************' )
 10   FORMAT( 7X, '*', 61X, '*' )
      WRITE( OBUF, 5 )
      CALL AIO_WRITE( OCH, OBUF, STATUS )

      WRITE( OBUF, 10 )
      CALL AIO_WRITE( OCH, OBUF, STATUS )

      WRITE( OBUF, '(7X,A1,2X,I10,1X,A12,5X,I10,1X,A12,8X,A1)' )
     :         '*', N, 'data points.', NVALID, 'points used.','*'
      CALL AIO_WRITE( OCH, OBUF, STATUS )
      WRITE( OBUF, 10 )
      CALL AIO_WRITE( OCH, OBUF, STATUS )
      WRITE( OBUF, '(7x,a1,2x,a27,1Pg15.7,17x,a1)' )
     :        '*', 'Sum of valid data points = ', SUM, '*'
      CALL AIO_WRITE( OCH, OBUF, STATUS )

*    WRITE the appropriate message for the mean.
      IF ( WTERR .AND. VAROK ) THEN
         WRITE(OBUF,'(7x,a1,2x,a15,1x,1Pg15.7,1x,a3,1x,1Pg15.7,8x,a1)')
     :'*', 'Weighted mean =', MEAN, '+/-', MEANERR, '*'

      ELSE
         WRITE(OBUF,'(7x,a1,2x,a12,1x,1Pg15.7,1x,a3,1x,1Pg15.7,11x,a1)')
     :'*', 'Mean value =', MEAN, '+/-', MEANERR, '*'

      END IF
      CALL AIO_WRITE( OCH, OBUF, STATUS )
      WRITE( OBUF, 10 )
      CALL AIO_WRITE( OCH, OBUF, STATUS )

*    Calculate error on the standard deviation.
      IF ( NVALID .GT. 1 ) THEN
        ERRORONSTDDEV = STDDEV / SQRT( 2.0D0 * NVALID )
        WRITE( OBUF, '(7x,a1,2x,a20,1x,1Pg15.7,1x,a3,1x,1Pg15.7,
     :    3x,a1)' )
     :  '*', 'Standard deviation =', STDDEV, '+/-', ERRORONSTDDEV, '*'
        CALL AIO_WRITE( OCH, OBUF, STATUS )
        WRITE( OBUF, 10 )
        CALL AIO_WRITE( OCH, OBUF, STATUS )

        IF ( .NOT. SIMPLE ) THEN
 15       FORMAT( 7X, '*', 2X, A25, 1X, 1PG15.7, 18X, '*' )
          WRITE( OBUF, 15 ) 'Coefficient of skewness =', SKEWNESS
          CALL AIO_WRITE( OCH, OBUF, STATUS )
          WRITE( OBUF, 15 ) 'Coefficient of kurtosis =', KURTOSIS
          CALL AIO_WRITE( OCH, OBUF, STATUS )
          WRITE( OBUF, 10 )
          CALL AIO_WRITE( OCH, OBUF, STATUS )
        END IF
      END IF

*    Print min and max and pixel in which they occur
 20   FORMAT( 7X, '*', 2X, A15, 2X, 1PG15.7, ' @ ', A, '*' )

*    Convert pixel number to indices and convert to char string
      CALL STR_ELEMTOC( MINP, NDIM, DIMS, PSTR, STATUS )
      WRITE( OBUF, 20 ) 'Minimum value =', MINVALUE, PSTR
      CALL AIO_WRITE( OCH, OBUF, STATUS )
      CALL STR_ELEMTOC( MAXP, NDIM, DIMS, PSTR, STATUS )
      WRITE( OBUF, 20 ) 'Maximum value =', MAXVALUE, PSTR
      CALL AIO_WRITE( OCH, OBUF, STATUS )
      WRITE( OBUF, 10 )
      CALL AIO_WRITE( OCH, OBUF, STATUS )

 30   FORMAT( 7X, '*', 2X, A24, 1X, 1PG15.7, 19X, '*' )
      IF ( VAROK .AND. (NVALID.GT.1) ) THEN
        WRITE( OBUF, '(7x, a1, 2x, a46, 13x, a1)' )
     :'*', 'Goodness of fit of the mean value to the data:','*'
        CALL AIO_WRITE( OCH, OBUF, STATUS )
        WRITE( OBUF, 30 ) '   Reduced chi squared =', CHISQUARE
        CALL AIO_WRITE( OCH, OBUF, STATUS )
        WRITE( OBUF, 30 ) '   Equivalent normal Z =', ENZ
        CALL AIO_WRITE( OCH, OBUF, STATUS )
        WRITE( OBUF, 10 )
        CALL AIO_WRITE( OCH, OBUF, STATUS )
      END IF

      WRITE( OBUF, 5 )
      CALL AIO_WRITE( OCH, OBUF, STATUS )

      END


*+  STATISTIX_CALC - Perform statistical calculations
      SUBROUTINE STATISTIX_CALC( SIMPLE, N, DATA, VAROK,USE_WEIGHT,WGT,
     :                           MEAN, SUM, STDDEV, MEANERR, SKEWNESS,
     :                           KURTOSIS, MINVALUE, MAXVALUE,
     :                           MINP, MAXP,
     :                           WTSUM, NVALID, CHISQUARE,
     :                           ENZ, STATUS )
*
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     23 Mar 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      LOGICAL                    SIMPLE               ! Use simple mode?
      INTEGER                    N                    ! # data values
      DOUBLE PRECISION           DATA(N)              ! Data values
      LOGICAL                    VAROK                ! Weights represent errors
      LOGICAL                    USE_WEIGHT           ! Use weights
      DOUBLE PRECISION           WGT(N)               ! Data weights
*
*    Export :
*
      DOUBLE PRECISION           MEAN                 ! Mean of data points.
      DOUBLE PRECISION           SUM                  ! Sum of data points.
      DOUBLE PRECISION           STDDEV               ! Standard deviation.
      DOUBLE PRECISION           MEANERR              ! Error on mean
      DOUBLE PRECISION           SKEWNESS             ! Coeff. of skewness.
      DOUBLE PRECISION           KURTOSIS             ! Coeff. of kurtosis.
      DOUBLE PRECISION           MINVALUE             ! Of valid data points.
      DOUBLE PRECISION           MAXVALUE             ! Of valid data points.
      INTEGER                    MINP, MAXP           ! Min and max pixels
      DOUBLE PRECISION           WTSUM                ! Sum of weights.
      INTEGER                    NVALID               ! # valid points
      DOUBLE PRECISION           CHISQUARE            ! Chi-squared
      DOUBLE PRECISION           ENZ                  ! Equivalent normal z
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      DOUBLE PRECISION           AMEAN                ! Assumed mean
      DOUBLE PRECISION           D                    ! sum(w)-sum(w^2)/sum(w)
      DOUBLE PRECISION           DMEAN                ! AMEAN - MEAN
      DOUBLE PRECISION           WTSUM2               ! Sum of weights squared

      INTEGER                    I,J                  ! Loop over data
      INTEGER                    IFAIL                ! NAG status
      INTEGER                    USEWEIGHT            !
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Number of points
      NVALID = N

*    Initialise
      MEAN = 0.0D0
      STDDEV = 0.0D0
      SKEWNESS = 0.0D0
      KURTOSIS = 0.0D0
      WTSUM = 0.0D0
      WTSUM2 = 0.0D0
      SUM = 0.0D0

*    Simple mode?
      IF ( SIMPLE ) THEN

*      Switch on use of weights
        IF ( USE_WEIGHT ) THEN

*        Find first valid point
          I = 1
          DO WHILE ( (I.LE.N) .AND. (WGT(I).LE.0.0) )
            I = I + 1
          END DO
          IF ( I .GT. N ) GOTO 99
          NVALID = NVALID - (I-1)
          MINVALUE = DATA(I)
          MAXVALUE = DATA(I)
          AMEAN = DATA(I)
          J = I
          MINP = J
          MAXP = J
          DO I = J, N
            IF ( WGT(I) .GT. 0.0 ) THEN
              WTSUM = WTSUM + WGT(I)
              WTSUM2 = WTSUM2 + WGT(I)*WGT(I)
              MEAN = MEAN + DATA(I)*WGT(I)
              SUM = SUM + DATA(I)
              STDDEV = STDDEV + WGT(I)*((DATA(I)-AMEAN)**2)
              IF ( MAXVALUE .LT. DATA(I) ) THEN
                MAXVALUE = DATA(I)
                MAXP = I
              END IF
              IF ( MINVALUE .GT. DATA(I) ) THEN
                MINVALUE = DATA(I)
                MINP = I
              END IF
            ELSE
              NVALID = NVALID - 1
            END IF
          END DO

        ELSE
          MINVALUE = DATA(1)
          MAXVALUE = DATA(1)
          MINP = 1
          MAXP = 1
          AMEAN = DATA(1)
          DO I = 1, N
            IF ( MAXVALUE .LT. DATA(I) ) THEN
              MAXVALUE = DATA(I)
              MAXP = I
            END IF
            IF ( MINVALUE .GT. DATA(I) ) THEN
              MINVALUE = DATA(I)
              MINP = I
            END IF
            SUM = SUM + DATA(I)
            STDDEV = STDDEV + (DATA(I)-AMEAN)**2
          END DO
          MEAN = SUM
          WTSUM = DBLE(N)
          WTSUM2 = WTSUM
        END IF

*      Find values from sums
        MEAN = MEAN / WTSUM
        D = WTSUM - WTSUM2/WTSUM

*      Correct standard deviation for assumed mean
        IF ( NVALID .GT. 1 ) THEN
          DMEAN = AMEAN - MEAN
          STDDEV = STDDEV - WTSUM*DMEAN**2
          STDDEV = SQRT(STDDEV/D)
        END IF

      ELSE

*      Set integer weights flag
        IF ( USE_WEIGHT ) THEN
          USEWEIGHT = 1
        ELSE
          USEWEIGHT = 0
        END IF

*      Set NAG error reporting
        IFAIL = 1

*      Use NAG routine to do work
        CALL G01AAF( N, DATA, USEWEIGHT, WGT, MEAN, STDDEV, SKEWNESS,
     :                   KURTOSIS, MINVALUE, MAXVALUE, WTSUM, IFAIL )

*      Find sum
        IF ( USE_WEIGHT ) THEN
          DO I = N, 1, -1
            IF ( WGT(I) .GT. 0.0D0 ) THEN
              SUM = SUM + DATA(I)
              IF ( DATA(I) .EQ. MAXVALUE ) MAXP = I
              IF ( DATA(I) .EQ. MINVALUE ) MINP = I
            END IF
          END DO
        ELSE
          DO I = N, 1, -1
            SUM = SUM + DATA(I)
            IF ( DATA(I) .EQ. MAXVALUE ) MAXP = I
            IF ( DATA(I) .EQ. MINVALUE ) MINP = I
          END DO
        END IF

*      Create a double precision version of USEWEIGHT as this is now the
*      number of points used by the NAG routine.
        NVALID = USEWEIGHT

*      Trap its failure
        IF ( IFAIL .NE. 0 ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Error in NAG routine G01AAF', STATUS )
          GOTO 99
        END IF

      END IF

*    Find error on mean
      IF ( VAROK ) THEN
        MEANERR = 1.0D0 / SQRT(WTSUM)
      ELSE
        MEANERR = STDDEV / SQRT(DBLE(NVALID))
      END IF

*    Calculate reduced chi square for fit of data to mean.
      CHISQUARE = 0.0D0
      IF ( VAROK ) THEN
        DO I = 1, N
          IF ( WGT(I) .GT. 0.0D0 ) THEN
            CHISQUARE = CHISQUARE + ((DATA(I)-MEAN)**2 * WGT(I))
          END IF
        END DO
        CHISQUARE = CHISQUARE / ( DBLE(NVALID) - 1.0D0 )

*      Calculate Equivalent normal Z
        IF ( NVALID .GT. 0 ) THEN
          ENZ = SQRT(2*CHISQUARE*(DBLE(NVALID)-1.0D0))-
     :               SQRT(2*(DBLE(NVALID)-1.0D0)-1.0D0)
        END IF

      END IF

*    Abort point
 99   CONTINUE

      END
