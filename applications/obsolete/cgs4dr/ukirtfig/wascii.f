C+
      SUBROUTINE WASCII
C
C     W A S C I I
C
C     Writes a copy of a CGS3/4 Figaro file as an ASCII output file,
C     containing the wavelength, flux, flux_error, and flux_quality.
C
C     Command parameters -
C
C     SPECTRUM  (Character) The name of the Figaro file to be converted.
C     FILE      (Character) The name of the ASCII format file to be
C               created.
C
C     Command keywords -  None
C
C                                               KS / AAO 10th Oct 1986
C     Modified:
C
C     24th Oct 1986.  KS / AAO.  Format code 2 data now written one pair
C                     of values to a line.
C     27th Aug 1987.  DJA/ AAO.  Revised DSA_ routines - some specs changed.
C                     Now uses DYN_ routines for dynamic memory handling
C      7th Jun 1991.  KLK / JAC. From WDIPSO
C     12th May 1993.  PND / JAC. If FITS object name doesn't exist, set it.
C     18th Nov 1993.  PND / JAC. Use DSA_SEEK_FITS to get OBJECT.
C     25-Aug-1994     KLK / JAC. Eliminate LIB$GET_LUN and LIB$FREE_LUN TO
C			port to unix
C+
      IMPLICIT NONE
C
C     Functions
C
      LOGICAL GEN_CHKNSF
      INTEGER DYN_ELEMENT
C
C     Local variables
C
      CHARACTER    ACCESS*1     ! Access mode
      LOGICAL      EXISTS       ! T if fits item exists
      INTEGER      ELEMENTS     ! Number of elements in fits item
      INTEGER      STRLEN       ! Length of string for returned fits item
      INTEGER      ADDRESS      ! Address of dynamic memory element
      CHARACTER    COMMENT*80   ! 'Name of object'
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      INTEGER      DPTR		! Dynamic-memory pointer to data array
      INTEGER      DSLOT        ! Map slot number of input data array
      INTEGER      DUMMY
      INTEGER      EPTR		! Dynamic-memory pointer to data errors
      CHARACTER    ERROR*64     ! An error message
      INTEGER      ESLOT        ! Map slot number of input data errors
      LOGICAL      FAULT        ! TRUE if an erro occurs
      CHARACTER    FILE*80      ! The output file name
      LOGICAL      FOPEN        ! TRUE if the output was opened OK
      LOGICAL      GOTLU        ! TRUE if output logical unit opened OK
      INTEGER      IGNORE       ! Used to pass ignorable status
      INTEGER      LU           ! The output's logical unit number
      CHARACTER    NAME*80      ! The actual name of the input file
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      NX           ! Size of 1st dimension
      INTEGER      NXX          ! Size of x-axis data
      CHARACTER    OBJECT_NAME*64    ! e.g. 'Jupiter'
      LOGICAL      QEXIST       ! True if found data quality array
      INTEGER      QPTR		! Dynamic-memory pointer to data quality
      INTEGER      QSLOT        ! Map slot number of input data quality
      INTEGER      STATUS       ! Running status for DSA_ routines
      CHARACTER*32 STRINGS(2)  ! #1 = units, #2 = label
      LOGICAL      XEXIST       ! TRUE if the input has an x-axis data array
      INTEGER      XPTR 	! Dynamic-memory pointer to x-axis data
      INTEGER      XSLOT        ! Map slot number of x-axis data
      CHARACTER    ZLABEL*10    ! Data array label (e.g. 'flux')
      CHARACTER    ZUNITS*32    ! Data array units (e.g. 'mJy')
C
C     Dynamic memory support - defines DYNAMIC_MEM
C
      INCLUDE 'DYNAMIC_MEMORY'
      DATA LU/1/
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
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      CALL DSA_GET_ACTUAL_NAME('SPECT',NAME,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Check on dimensions of data
C
      CALL DSA_DATA_SIZE('SPECT',1,NDIM,DIMS,NX,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map data array
C
      CALL DSA_MAP_DATA('SPECT','READ','FLOAT',ADDRESS,DSLOT,STATUS)
      DPTR=DYN_ELEMENT(ADDRESS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get flux array units and label
C
      CALL DSA_GET_DATA_INFO ('SPECT', 2, STRINGS,0, DUMMY, STATUS )
      IF (STATUS.NE.0) GOTO 500
      ZUNITS(1:32) = STRINGS(1)(1:32)
      ZLABEL(1:10) = STRINGS(2)(1:10)
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
     :     QSLOT,STATUS)
        QPTR=DYN_ELEMENT(ADDRESS)
        IF (STATUS.NE.0) GOTO 500
      ELSE
        CALL PAR_WRUSER (
     :   'Data quality array missing. Will set to 0 (=OK).',
     :         IGNORE )
      ENDIF

C
C     Get the object name
C
      EXISTS = .FALSE.
      CALL DSA_SEEK_FITS('SPECT', 'OBJECT', EXISTS, ACCESS,
     :   ELEMENTS, STRLEN, STATUS )
      IF ( EXISTS ) THEN
         CALL DSA_GET_FITS_C( 'SPECT', 'OBJECT', 1, OBJECT_NAME,
     :      COMMENT, STATUS )
      ELSE
         OBJECT_NAME = 'Object not specified'
      ENDIF
C
C     Check that there is an X array.
C
      CALL DSA_SEEK_AXIS('SPECT',1,XEXIST,STATUS)
      CALL DSA_MAP_AXIS_DATA('SPECT',1,'READ','FLOAT',ADDRESS,XSLOT,
     :                                                       STATUS)
      XPTR=DYN_ELEMENT(ADDRESS)
      IF (STATUS.NE.0) GOTO 500
      IF (XEXIST) THEN
C
C        Warn if it only contains pixel numbers.
C
         CALL DSA_AXIS_SIZE('SPECT',1,1,NDIM,DIMS,NXX,STATUS)
         IF (DIMS(1).NE.NX) THEN
            CALL PAR_WRUSER(
     :          'Wavelength array and data array have different sizes',
     :                                                          IGNORE)
            FAULT=.TRUE.
            GO TO 500
         END IF
         IF (GEN_CHKNSF(DYNAMIC_MEM(XPTR),NX)) THEN
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
C
C     Get the name of the file to be created.
C
      CALL PAR_RDCHAR('FILE',' ',FILE)
C
C     Create file
C
      OPEN (UNIT=LU,ACCESS='SEQUENTIAL',STATUS='NEW',FILE=FILE,
     :      IOSTAT=STATUS)
      IF (STATUS.NE.0) THEN
         CALL GEN_FORTERR(STATUS,.FALSE.,ERROR)
         CALL PAR_WRUSER('Unable to open output file',IGNORE)
         CALL PAR_WRUSER(ERROR,IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
      GOTLU=.TRUE.
      FOPEN=.TRUE.
C
C     Now write out the data arrays.  This has be a subroutine call
C     because of the use of mapped arrays.
C
C   OK - how to get flux_quality array if it's not in file?
      CALL FIG_ASCOUT(LU,NX,DYNAMIC_MEM(XPTR),
     :    DYNAMIC_MEM(DPTR),ZLABEL, ZUNITS, OBJECT_NAME,
     :    DYNAMIC_MEM(EPTR),QEXIST,DYNAMIC_MEM(QPTR),STATUS)
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
         CLOSE (LU,IOSTAT=STATUS)
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
      SUBROUTINE FIG_ASCOUT(LU,NX,XDATA,ZDATA,ZLABEL,ZUNITS,
     :    OBJECT_NAME,ZERR,QEXIST,ZQUAL,STATUS)
C
C     F I G _ A S C O U T
C
C     WASCII utility routine.  Writes out the data arrays to the
C     output file.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) LU     (Integer) The logical unit on which the output file
C                has been opened.  This routine assumes the header data
C                is already written.  The file is not closed.
C     (>) NX     (Integer) Number of elements in the data.
C     (>) XDATA  (Real array XDATA(NX)) The wavelength data array.
C     (>) ZDATA  (Real array ZDATA(NX)) The flux data array.
C     (>) ZLABEL (Character) e.g. 'flux'
C     (>) ZUNITS (Character) e.g. 'mJy'
C     (>) OBJECT_NAME (Character) e.g. 'Jupiter'
C     (>) ZERR   (Real array ZERR(NX)) The flux error array.
C     (>) QEXIST (Logical) flag = .true. if data quality array was found.
C     (>) ZQUAL  (BYTE array ZQUAL(NX)) The flux data quality array.
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
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL QEXIST
      INTEGER LU, NX, STATUS
      REAL    XDATA(NX), ZDATA(NX), ZERR(NX)
      BYTE ZQUAL(NX)
      CHARACTER ZLABEL*(*)
      CHARACTER ZUNITS*(*)
      CHARACTER OBJECT_NAME*(*)
C
C     Local variables
C
      INTEGER I
      BYTE DUMMY

      DUMMY = 0

      WRITE ( LU, 93, IOSTAT=STATUS ) OBJECT_NAME
93    FORMAT ( ' * ', A )

      WRITE ( LU, 103, IOSTAT=STATUS )  ZLABEL
103   FORMAT ( ' ', '* Wavelength        ',A,'  Errors       Quality' )

      WRITE ( LU, 113, IOSTAT=STATUS )  ZUNITS
113   FORMAT ( ' ', '* (microns)         ',A)
C
C     Write the data out
C
      IF ( QEXIST ) THEN
        DO I=1,NX
          WRITE (LU,*,IOSTAT=STATUS) XDATA(I),ZDATA(I),ZERR(I),ZQUAL(I)
          IF (STATUS.NE.0) GO TO 320
        END DO
      ELSE
        DO I=1,NX
          WRITE (LU,*,IOSTAT=STATUS) XDATA(I),ZDATA(I),ZERR(I),DUMMY
          IF (STATUS.NE.0) GO TO 320
        END DO
      ENDIF

320    CONTINUE
C
      END
