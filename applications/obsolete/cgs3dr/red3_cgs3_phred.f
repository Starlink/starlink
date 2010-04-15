C+
      SUBROUTINE RED3_CGS3_PHRED (STATUS)
C
C     C G S 3 _ P H R E D
C
C     Writes statistics from a CGS3 file (e.g. 29JUN0015 as an ASCII
C     output file.
C
C     Command parameters -
C
C     SPECTRUM  (Character) The name of the CGS3 file to be analyzed
C     FILE      (Character) The name of the ASCII format file to be
C               created.
C
C     Command keywords -  None
C
C     Modified:
C
C      13-Jun-1995 : cloned from WASCII
C      28-Jun-1995 : changed from Figaro program to Atask
C      30-Jun-1995 : expand to 4-D data arrays
C      20-Jul-1995 : put in verbosity flag for more streamline output
C      01-Aug-1995 : access FIO calls for output file
C      03-Aug-1995 : make sure airmass is between 1.0 and 2.1
C      16-Aug-1995 : alert user if a run is single beam data
C      04-Dec-1995 : remove Vax directory ref for dynamic_memory
*      27-Feb-1996 : rename from red4_
C+
      IMPLICIT NONE
      INTEGER      STATUS       ! Running status for DSA_ routines
C
C     Functions
C
      LOGICAL GEN_CHKNSF
      INTEGER DYN_ELEMENT

C     Global constants

      INCLUDE 'SAE_PAR'

C
C     Local variables
C
      REAL         AMASS	! mean air mass
      REAL         AMEND	! end air mass
      REAL         AMSTART	! start air mass
      INTEGER      ADDRESS      ! Address of dynamic memory element
      CHARACTER    COMMENT*80   ! 'Name of object'
      REAL         EXP		! integration time
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      INTEGER      DPTR		! Dynamic-memory pointer to data array
      INTEGER      DSLOT        ! Map slot number of input data array
      INTEGER      ENDSCAN	! ending scan number
      INTEGER      EPTR		! Dynamic-memory pointer to data errors
      CHARACTER    ERROR*64     ! An error message
      INTEGER      ESLOT        ! Map slot number of input data errors
      LOGICAL      FAULT        ! TRUE if an erro occurs
      INTEGER      FD           ! pointer to output file
      CHARACTER*80 FILE         ! The output file name
      LOGICAL      FOPEN        ! TRUE if the output was opened OK
      LOGICAL      GOTLU        ! TRUE if output logical unit opened OK
      INTEGER      IGNORE       ! Used to pass ignorable status
      INTEGER      IEND		! end index for spectrum sum
      INTEGER      IST		! start index for spectrum sum
      INTEGER      NBEAMS	! number of beams (2)
      INTEGER      NCYC		! number of spectra (pairs) in this run
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      NX           ! Size of 1st dimension
      INTEGER      NXX          ! Size of x-axis data
      INTEGER      NWAVE	! number of channels (32)
      CHARACTER    OBJECT_NAME*64    ! e.g. 'Jupiter'
      LOGICAL      QEXIST       ! True if found data quality array
      INTEGER      QPTR		! Dynamic-memory pointer to data quality
      INTEGER      QSLOT        ! Map slot number of input data quality
      INTEGER      RUN		! run number
      REAL         SIGMA_LIM    ! limiting number of sigma for data inclusion
      INTEGER      SLOT
      CHARACTER*80 SPECT	! input file name
      INTEGER      STARTSCAN    ! starting scan number
      INTEGER      SUMPTR      ! sum pointer
      CHARACTER*8  UTSTART     ! start UT time of obss
      LOGICAL      VERBOSE      ! put out intermediate statistics?
      LOGICAL      XEXIST       ! TRUE if the input has an x-axis data array
      INTEGER      XPTR 	! Dynamic-memory pointer to x-axis data
      INTEGER      XSLOT        ! Map slot number of x-axis data
C
C     Dynamic memory support - defines DYNAMIC_MEM
C
      INCLUDE 'DYNAMIC_MEMORY'
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Initial values
C
      FAULT=.FALSE.
      FOPEN=.FALSE.
      GOTLU=.FALSE.
C
C     Get name of file to be converted and open it.
C
      CALL PAR_GET0C ('SPECT', SPECT, STATUS )
      CALL DSA_NAMED_INPUT ('SPECT', SPECT, STATUS )
      IF (STATUS.NE.0) GO TO 500
C
C     Check on dimensions of data
C
      CALL DSA_DATA_SIZE('SPECT',4,NDIM,DIMS,NX,STATUS)
      IF ( (NDIM .NE. 4) .OR. (DIMS(1) .GT. 1) .OR. (DIMS(3) .GT. 2))
     *    THEN
        CALL MSG_OUT ( ' ', 'Incorrect dimensions.  Probably not'//
     *     ' correct data or directory.', STATUS )
      ENDIF
      IF (STATUS.NE.0) GO TO 500
*      CALL MSG_SETI ( 'NX', NX )
*      CALL MSG_OUT ( ' ', 'NX = ^NX', STATUS )
      NWAVE  = DIMS(2)
      NBEAMS = DIMS(3)
      NCYC   = DIMS(4)
      IF ( NBEAMS .EQ. 1 ) THEN
        CALL MSG_OUT ( ' ', 'This is single beam data!', STATUS )
      ENDIF
*      CALL MSG_SETI ( 'NWAVE', NWAVE )
*      CALL MSG_SETI ( 'NBEAMS', NBEAMS )
*      CALL MSG_SETI ( 'NCYC', NCYC )
*      CALL MSG_OUT ( ' ',
*     * 'NWAVE = ^NWAVE, NBEAMS = ^NBEAMS, NCYC = ^NCYC', STATUS )

C    Get start and end scans
      CALL PAR_GET0I ('STARTSCAN',STARTSCAN,STATUS)
      IF (STARTSCAN .LT. 1) THEN
         CALL MSG_OUT (' ', 'STARTSCAN must be >= 1', STATUS)
         STATUS = SAI__ERROR
      END IF
      IF (STARTSCAN .GT. NCYC) THEN
         CALL MSG_SETI ('NC', NCYC)
         CALL MSG_OUT (' ','Illegal STARTSCAN, max. is ^NC', STATUS)
         STATUS = SAI__ERROR
      END IF
      CALL PAR_GET0I ('ENDSCAN',ENDSCAN,STATUS)
      IF (ENDSCAN .EQ. 0) ENDSCAN = NCYC
      IF (ENDSCAN .LT. STARTSCAN) THEN
         CALL MSG_OUT (' ', 'Illegal ENDSCAN, '/
     :    /'must be greater than STARTSCAN', STATUS)
         STATUS = SAI__ERROR
      END IF
      IF (ENDSCAN .GT. NCYC) THEN
         CALL MSG_SETI ('NC', NCYC)
         CALL MSG_OUT (' ','Illegal ENDSCAN, max. is ^NC', STATUS)
         STATUS = SAI__ERROR
      END IF
C
C     Map data array
C
      CALL DSA_MAP_DATA('SPECT','READ','FLOAT',ADDRESS,DSLOT,STATUS)
      DPTR=DYN_ELEMENT(ADDRESS)
      IF (STATUS.NE.0) GOTO 500

C
C     Map data errors
C
      CALL DSA_MAP_ERRORS('SPECT','READ','FLOAT',ADDRESS,ESLOT,STATUS)
      EPTR=DYN_ELEMENT(ADDRESS)
      IF (STATUS.NE.0) GOTO 500
C
C     See if there is a quality array and get them if they're there
C
      CALL DSA_SEEK_QUALITY ( 'SPECT', QEXIST, STATUS )
      IF ( QEXIST ) THEN
        CALL DSA_MAP_QUALITY ('SPECT','READ','BYTE',ADDRESS,
     *     QSLOT,STATUS)
        QPTR=DYN_ELEMENT(ADDRESS)
        IF (STATUS.NE.0) GOTO 500
*      ELSE
*        CALL PAR_WRUSER (
*     *   'Data quality array missing. Does not matter though!',
*     *         IGNORE )
      ENDIF

C     Parameters from FITS header
C
C     Get the object name
C
      CALL DSA_GET_FITS_C ('SPECT', 'OBJECT', 1, OBJECT_NAME,
     *    COMMENT, STATUS )
*      CALL MSG_SETC ('OBJ', OBJECT_NAME )
*      CALL MSG_OUT ( ' ', 'Object = ^OBJ', STATUS )
      IF ( STATUS .NE. 0 ) GOTO 500

C     UT start

      CALL DSA_GET_FITS_C ('SPECT', 'UTSTART', 1, UTSTART,
     *    COMMENT, STATUS )
      IF ( STATUS .NE. 0 ) GOTO 500

C     Run number

      CALL DSA_GET_FITS_I ('SPECT', 'RUN', 0, RUN, COMMENT,
     *  STATUS )
*      CALL MSG_SETI ( 'RUN', RUN )
*      CALL MSG_OUT ( ' ', 'Run = ^RUN', STATUS )
      IF ( STATUS .NE. 0 ) GOTO 500

C     Get integration time

      CALL DSA_GET_FITS_F ('SPECT', 'C3STRDWL', 0, EXP, COMMENT,
     *  STATUS )
*      CALL MSG_SETR ( 'EXP', EXP )
*      CALL MSG_OUT ( ' ', 'Integ time = ^EXP', STATUS )
      IF ( STATUS .NE. 0 ) GOTO 500

C     Start and end air mass

      CALL DSA_GET_FITS_F ('SPECT', 'AMSTART', 0, AMSTART, COMMENT,
     *  STATUS )
      IF ( STATUS .NE. 0 ) GOTO 500

      CALL DSA_GET_FITS_F ('SPECT', 'AMEND', 0, AMEND, COMMENT, STATUS )
      IF ( STATUS .NE. 0 ) GOTO 500
      AMASS = 0.5 * ( AMSTART + AMEND )
      IF ( AMASS .LT. 1.0 .OR. AMASS .GT. 2.1 ) THEN
        CALL MSG_SETR ( 'AMASS', AMASS )
        CALL MSG_OUT ( ' ',
     *       'Mean air amass = ^AMASS out of bounds', STATUS )
        CALL MSG_OUT (' ', 'It will be set to 1.0', STATUS )
        AMASS = 1.0
      ENDIF

C
C     Check that there is an X array.  ***
C
      CALL DSA_SEEK_AXIS('SPECT',1,XEXIST,STATUS)
      CALL DSA_MAP_AXIS_DATA('SPECT',2,'READ','FLOAT',ADDRESS,XSLOT,
     :                                                       STATUS)
      XPTR=DYN_ELEMENT(ADDRESS)
      IF (STATUS.NE.0) GOTO 500
      IF (XEXIST) THEN
C
C        Warn if it only contains pixel numbers.
C
         CALL DSA_AXIS_SIZE('SPECT',2,4,NDIM,DIMS,NXX,STATUS)
         IF ( DIMS(1) .NE. NWAVE ) THEN
            CALL PAR_WRUSER(
     :          'Wavelength array and data array have different sizes',
     :                                                          IGNORE)
            FAULT=.TRUE.
            GO TO 500
         END IF
         IF (GEN_CHKNSF(DYNAMIC_MEM(XPTR),NWAVE)) THEN
            CALL PAR_WRUSER(
     :        '***Warning*** Data is not wavelength calibrated',IGNORE)
         END IF
      ELSE
C
C        No X-axis array, but we'll just use channel numbers
C
         CALL PAR_WRUSER(
     :        '***Warning*** Data has no wavelength array',
     :                                                        IGNORE)
      END IF

C     Get indices for sum
      CALL PAR_GET0I ( 'IST', IST, STATUS )
      CALL PAR_GET0I ( 'IEND', IEND, STATUS )

C     Get limiting number of standard deviations

      CALL PAR_GET0R ( 'SIGMA_LIM', SIGMA_LIM, STATUS )

C     Get the name of the file to be created.

      CALL PAR_GET0C ( 'FILE', FILE, STATUS )

C     Put out intermediate statistics?

      CALL PAR_GET0L ( 'VERBOSE', VERBOSE, STATUS )
C
C     Create file
C
      CALL FIO_OPEN (FILE, 'APPEND', 'LIST', 0, FD, STATUS)
      IF (STATUS.NE.0) THEN
         CALL GEN_FORTERR(STATUS,.FALSE.,ERROR)
         CALL PAR_WRUSER('Unable to open output file',IGNORE)
         CALL PAR_WRUSER(ERROR,IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
      FOPEN=.TRUE.

C     Get work array for means of scans
      CALL DSA_GET_WORK_ARRAY (NCYC,'FLOAT',SUMPTR,SLOT,STATUS)
C
C     Now get data and do statistics on it.  This has be a subroutine call
C     because of the use of mapped arrays.
C

      CALL FIG_ASCOUT(FD,NWAVE,NBEAMS,NCYC,DYNAMIC_MEM(XPTR),
     :    DYNAMIC_MEM(DPTR), OBJECT_NAME, UTSTART,
     :    DYNAMIC_MEM(EPTR),QEXIST,DYNAMIC_MEM(QPTR),
     :    STARTSCAN,ENDSCAN,AMASS,RUN,EXP,IST,IEND,SIGMA_LIM,
     :    %VAL(SUMPTR),VERBOSE,STATUS)
      IF (STATUS.NE.0) THEN
         CALL GEN_FORTERR(STATUS,.FALSE.,ERROR)
         CALL PAR_WRUSER('Error writing data to output file',IGNORE)
         CALL PAR_WRUSER(ERROR,IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     Tidy up
C
  500 CONTINUE
C
C     Close output file
C
      IF (FOPEN) THEN
         CALL FIO_CLOSE ( FD, STATUS )
         IF (STATUS.NE.0) THEN
            CALL GEN_FORTERR(STATUS,.FALSE.,ERROR)
            CALL PAR_WRUSER('Error closing output file',IGNORE)
            CALL PAR_WRUSER(ERROR,IGNORE)
            FAULT=.TRUE.
         END IF
      END IF
C
      CALL DSA_CLOSE(STATUS)
C
      IF (FAULT) CALL FIG_SETERR
C
      END
C+
      SUBROUTINE FIG_ASCOUT(FD,NWAVE,NBEAMS,NCYC,XDATA,ZDATA,
     *    OBJECT_NAME,UTSTART,ZERR,QEXIST,ZQUAL,STARTSCAN,ENDSCAN,
     *    AMASS,RUN,EXP,IST,IEND,SIGMA_LIM,XBAR,VERBOSE,STATUS)
C
C     F I G _ A S C O U T
C
C     WASCII utility routine.  Writes out the data arrays to the
C     output file.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) FD     (Integer) The field descriptor for the output file
C     (>) NWAVE  (Integer) Number of elements in the data.
C     (>) NBEAMS (Integer) Number of beams (2)
C     (>) NCYC   (Integer) Number of spectra in the run
C     (>) XDATA  (Real array XDATA(NX)) The wavelength data array.
C     (>) ZDATA  (Real array ZDATA(NX)) The flux data array.
C     (>) OBJECT_NAME (Character) e.g. 'Jupiter'
C     (>) UTSTART (Character) start time UT
C     (>) ZERR   (Real array ZERR(NX)) The flux error array.
C     (>) QEXIST (Logical) flag = .true. if data quality array was found.
C     (>) ZQUAL  (BYTE array ZQUAL(NX)) The flux data quality array.
C     (>) STARTSCAN starting scan number to use
C     (>) ENDSCAN   ending scan number to use
C     (>) AMASS  mean air mass
C     (>) RUN    run number
C     (>0 EXP    integration time
C     (>) IST      start index
C     (>) IEND     end index
C     (>) SIGMA_LIM    limiting number of std devs for inclusion a scan mean
C     (>) XBAR    work array for means of each scan
C     (>) VERBOSE   put out intermediate statistics?
C     (<) STATUS (Integer) Status code.  0 => OK, non-zero values are
C                Fortran I/O error codes.
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C                                        KS / AAO 10th Oct 1986
C     Modified:
C
C     24th Oct 1986.  KS / AAO.  Format code 2 data now written one pair
C                     of values to a line.
C     7-Jun-1991  :   KK / JAC   Mod for output of CGS3/4 data
C     13-Jun-1995 :   KK   other parameters from FITS header
C     30-Jun-1995 :   KK   expand to 4-D array analysis
C     20-Jul-1995 :   KK   put in verbosity flag
C     01-Aug-1995 :   output via FIO calls
C     03-Aug-1995 :   some error checking, add UT start time
C     14-AUG-1995 :   provision for single beam data (arcs)
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL QEXIST, VERBOSE
      INTEGER FD, STATUS, IST, IEND, RUN
      INTEGER NWAVE, NBEAMS, NCYC, STARTSCAN, ENDSCAN
      REAL    XDATA(NWAVE), ZDATA(1,NWAVE,NBEAMS,NCYC)
      REAL ZERR(NWAVE), AMASS, EXP, SIGMA_LIM, XBAR(NCYC)
      BYTE ZQUAL(NWAVE)
      CHARACTER OBJECT_NAME*10, UTSTART*8
      CHARACTER*87  HEAD        ! header(s) for ASCII output file
      CHARACTER*87 OBSREC       ! for ASCII output file
C
C     Local variables
C
      INTEGER I,N,ISCAN,INDEX
      REAL SIGMA_XBAR,RN,SIGMA,SUM, SUM2, TEMP, TEST
      REAL GRAND_MEAN, SN

      IF (VERBOSE) THEN
        HEAD =  'Scan  airmass  st_wavel  end_wavel'//
     *   ' mean_flux      +/-        integ object'
        CALL FIO_WRITE (FD, HEAD(1:80), STATUS )
      ENDIF

C
C     Calculate statistics and write the data out
C

C     Note: IST (e.g. 1) is starting channel number and IEND (e.g. 32)
C     is ending channel number, but data AND wavelengths have been put
C     into the file backwards

      N = IEND - IST + 1
      RN = FLOAT(N)
      DO ISCAN = STARTSCAN,ENDSCAN
        SUM = 0.0
        SUM2 = 0.0
        DO I = IST,IEND
          INDEX = NWAVE - I + 1
          IF ( NBEAMS .EQ. 2 ) THEN
            TEMP = ( ZDATA(1,INDEX,1,ISCAN) -
     *               ZDATA(1,INDEX,2,ISCAN) ) / 2.0
          ELSE IF ( NBEAMS .EQ. 1 ) THEN
            TEMP = ZDATA(1,INDEX,1,ISCAN)
          ENDIF
          IF ( ABS(TEMP) .LT. 1.0E+10 ) THEN
            SUM = SUM +  TEMP
            SUM2 = SUM2 + TEMP * TEMP
          ELSE
            CALL MSG_OUT ( ' ', 'Data seems out of range', STATUS )
            CALL MSG_SETI ( 'RUN', RUN )
            CALL MSG_SETI ( 'SCAN', ISCAN )
            CALL MSG_SETI ( 'I', I )
            CALL MSG_SETR ( 'VAL', TEMP )
            CALL MSG_OUT ( ' ',
     *    'Run = ^RUN, scan = ^SCAN, point = ^I, val = ^VAL', STATUS )
          ENDIF

        ENDDO

        XBAR(ISCAN) = SUM / RN
        SIGMA = SQRT ( (SUM2 - RN*XBAR(ISCAN)*XBAR(ISCAN)) / (RN-1.0) )
        SIGMA_XBAR = SIGMA/SQRT(RN)

      IF (VERBOSE) THEN
         WRITE ( OBSREC, '(T1,I4,
     *                     T6,F6.4,
     *                     T14,F8.4,
     *                     T24,F8.4,
     *                     T34,G12.6,
     *                     T48,G12.6,
     *                     T61,F6.2,
     *                     T68,A10)')  ISCAN, AMASS,
     *   XDATA(NWAVE-IST+1),
     *   XDATA(NWAVE-IEND+1), XBAR(ISCAN), SIGMA_XBAR, EXP, OBJECT_NAME
        CALL FIO_WRITE ( FD, OBSREC(1:80), STATUS )
      ENDIF

      ENDDO

C     Now do statistics on the individual scan means

      N = ENDSCAN - STARTSCAN + 1
      RN = FLOAT(N)
      SUM = 0.0
      SUM2 = 0.0
      DO I = STARTSCAN,ENDSCAN
        TEMP = XBAR(I)
        SUM = SUM + TEMP
        SUM2 = SUM2 + TEMP * TEMP
      ENDDO

      IF ( N .EQ. 0 ) THEN
        GRAND_MEAN = 0.0
        SIGMA = 0.0
        SIGMA_XBAR = 0.0
      ELSE IF ( N .EQ. 1 ) THEN
        GRAND_MEAN = SUM
        SIGMA = 0.0
        SIGMA_XBAR = 0.0
      ELSE
        GRAND_MEAN = SUM / RN
        SIGMA = SQRT ( (SUM2 - RN*GRAND_MEAN*GRAND_MEAN) / (RN-1.0) )
        SIGMA_XBAR = SIGMA/SQRT(RN)
      ENDIF

C     Exclude discordant scans

C       Note that the relevant paramter here is sigma, not sigma_xbar
C       That's because we're dealing with the *distribution* of means
C       to see which one might not fit the others

      IF ( N .GT. 1 .AND. SIGMA .GT. 0.0 ) THEN
        DO I = STARTSCAN,ENDSCAN
          TEST = ABS ( XBAR(I) - GRAND_MEAN ) / SIGMA
          IF ( TEST .GT. SIGMA_LIM ) THEN
            CALL MSG_SETI ('I', I)
            CALL MSG_SETR ( 'MEAN', XBAR(I) )
            CALL MSG_SETR ( 'TEST', TEST )
            CALL MSG_OUT ( ' ',
     *        'Scan ^I = ^MEAN is ^TEST sigma out.  Excluded.', STATUS )
             IF ( VERBOSE ) THEN
               HEAD = 'Scan   value       N-sigma        (excluded):'
               CALL FIO_WRITE ( FD, HEAD(1:80), STATUS )
               WRITE ( OBSREC, '(T1,I4,T6,G12.6,T19,G12.6)') I, XBAR(I),
     *             TEST
               CALL FIO_WRITE ( FD, OBSREC(1:80), STATUS )
             ENDIF

            SUM = SUM - XBAR(I)
            SUM2 = SUM2 - XBAR(I) * XBAR(I)
            N = N - 1
          ENDIF
        ENDDO
      ENDIF

      RN = FLOAT(N)
      IF ( N .GT. 1 ) THEN

        GRAND_MEAN = SUM / RN
        SIGMA = SQRT ( (SUM2 - RN*GRAND_MEAN*GRAND_MEAN) / (RN-1.0) )
        SIGMA_XBAR = SIGMA/SQRT(RN)
        SN = GRAND_MEAN / SIGMA_XBAR
        IF ( SN .GT. 999.99 ) SN = 999.99  ! avoid format errors
        IF ( SN .LT. -99.99 ) SN = -99.99

      ELSE IF ( N .EQ. 1 ) THEN
        GRAND_MEAN = SUM
        SIGMA_XBAR = 0.0
        SN = 0.0

      ELSE

        CALL MSG_OUT ( ' ', 'All scans were excluded!', STATUS )
        GRAND_MEAN = 0.0
        SIGMA_XBAR = 0.0
        SN = 0.0

      ENDIF

      IF (VERBOSE) THEN
        HEAD = ' Run object     UTstart   airmass  st_wavel ' //
     *     'end_wavel Grand Mean       +/-          S/N'
        CALL FIO_WRITE ( FD, HEAD, STATUS )
      ENDIF

       WRITE ( OBSREC, '(T1,I4,
     *                   T6,A10,
     *                   T17,A8,
     *                   T26,F8.4,
     *                   T36,F8.4,
     *                   T46,F8.4,
     *                   T56,G12.6,
     *                   T69,G12.6,
     *                   T82,F6.2)' ) RUN, OBJECT_NAME, UTSTART,
     *   AMASS, XDATA(NWAVE-IST+1),
     *   XDATA(NWAVE-IEND+1), GRAND_MEAN, SIGMA_XBAR, SN

       CALL FIO_WRITE ( FD, OBSREC, STATUS )

       CALL MSG_SETI ('RUN', RUN)
       CALL MSG_OUT ( ' ', 'Run ^RUN processed', STATUS )

C
      END
