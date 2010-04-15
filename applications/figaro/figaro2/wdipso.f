C+
      SUBROUTINE WDIPSO
C
C     W D I P S O
C
C     Writes a copy of a Figaro file in DIPSO/IUEDR format.  The file
C     can be written in any of what IUEDR calls SPECTRUM type 0, type 1
C     or type 2 format.
C
C     Command parameters -
C
C     SPECTRUM  (Character) The name of the Figaro file to be converted.
C     CODE      (Numeric) The type code for the format used.
C     FILE      (Character) The name of the DIPSO format file to be
C               created.
C
C     Command keywords -  None
C
C                                               KS / AAO 10th Oct 1986
C     Modified:
C
C     24th Oct 1986  KS / AAO.  Format code 2 data now written one pair
C                    of values to a line.
C     27th Aug 1987  DJA/ AAO.  Revised DSA_ routines - some specs
C                    changed.  Now uses DYN_ routines for
C                    dynamic-memory handling.
C     21st Oct 1992  HME / UoE, Starlink.  INCLUDE changed, TABs
C                    removed.
C     27th Jun 1993  KS / AAO. Now uses DSA_OPEN_TEXT_FILE if possible
C                    and uses DSA_GET_LU instead of VMS routines. Unused
C                    variables removed.
C     18th Jul 1996  MJCL / Starlink, UCL.  Set variables for storage of
C                    file names to 132 chars.
C     17th Jul 1996  MJCL / Starlink, UCL.  PAR_ABORT checking.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL GEN_CHKNSF
      INTEGER ICH_LEN
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
C
C     Local variables
C
      CHARACTER    COMMENT*80    !
      CHARACTER    DATE*20       ! The current date
      CHARACTER    DAY*9         ! The current day
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      INTEGER      DPTR          ! Dynamic-memory pointer to data array
      INTEGER      DSLOT         ! Map slot number of input data array
      CHARACTER    ERROR*64      ! An error message
      LOGICAL      FAULT         ! TRUE if an error occurs
      CHARACTER    FILE*132      ! The output file name
      LOGICAL      FOPEN         ! TRUE if the output was opened OK
      CHARACTER    HOUR*12       ! The current hour
      INTEGER      ICODE         ! The output code to use - see above
      INTEGER      IGNORE        ! Used to pass ignorable status
      INTEGER      LDATE         ! Length of the date string
      INTEGER      LDAY          ! Length of the day string
      INTEGER      LHOUR         ! Length of the hour string
      INTEGER      LU            ! The output's logical unit number
      CHARACTER    NAME*132      ! The actual name of the input file
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NX            ! Size of 1st dimension
      INTEGER      NXX           ! Size of x-axis data
      INTEGER      STATUS        ! Running status for DSA_ routines
      CHARACTER    STRING*80     ! Holds the name of the object name
      INTEGER      STYLE         ! The output style in DIPSO format
      REAL         VALUE         ! Temporary real number
      LOGICAL      XEXIST        ! TRUE if the input has an x-axis data
                                 ! array
      INTEGER      XPTR          ! Dynamic-memory pointer to x-axis data
      INTEGER      XSLOT         ! Map slot number of x-axis data
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
      CALL DSA_MAP_DATA('SPECT','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Check that there is an X array.
C
      CALL DSA_SEEK_AXIS('SPECT',1,XEXIST,STATUS)
      CALL DSA_MAP_AXIS_DATA('SPECT',1,'READ','FLOAT',XPTR,XSLOT,STATUS)
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
         IF (GEN_CHKNSF(%VAL(CNF_PVAL(XPTR)),NX)) THEN
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
C     Get the type code for the output data
C
      CALL PAR_RDVAL('CODE',0.0,2.0,0.0,' ',VALUE)
      ICODE=NINT(VALUE)
C
C     Get the name of the file to be created.
C
      CALL PAR_RDCHAR('FILE',' ',FILE)
      IF ( PAR_ABORT() ) GO TO 500
C
C     Create file, format etc depending on type code. For the formatted
C     files, use DSA_OPEN_TEXT_FILE, which avoids the need for system
C     specific OPEN keywords (particulalry on VAXes).
C
      CALL DSA_GET_LU (LU,STATUS)
      IF (STATUS.NE.0) GO TO 500
      IF (ICODE.EQ.0) THEN
         OPEN (UNIT=LU, ACCESS='SEQUENTIAL',FORM='UNFORMATTED',
     :         STATUS='NEW',FILE=FILE,IOSTAT=STATUS)
         IF (STATUS.NE.0) THEN
            CALL GEN_FORTERR(STATUS,.FALSE.,ERROR)
            CALL PAR_WRUSER('Unable to open output file',IGNORE)
            CALL PAR_WRUSER(ERROR,IGNORE)
            FAULT=.TRUE.
            GO TO 500
         END IF
      ELSE
         CALL DSA_OPEN_TEXT_FILE (FILE,'.DAT','NEW',.TRUE.,LU,
     :                                            STRING,STATUS)
         IF (STATUS.NE.0) GO TO 500
      END IF
      FOPEN=.TRUE.
C
C     Get the name of the object, if there is one.
C
      CALL DSA_OBJECT_NAME('SPECT',STRING,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Create second comment from file name and date
C
      STYLE=6
      CALL GEN_TIME(STYLE,DAY,LDAY,DATE,LDATE,HOUR,LHOUR)
      COMMENT='File '//NAME(:ICH_LEN(NAME))//
     :                    ', '//DATE(:LDATE)//' at '//HOUR(:LHOUR)
C
C     We are now in a position to write out the header information
C     - the second comment line is set to blank.
C
      IF (ICODE.EQ.0) THEN
         WRITE(LU,IOSTAT=STATUS) STRING(:79)
         IF (STATUS.EQ.0) THEN
            WRITE(LU,IOSTAT=STATUS) COMMENT(:79)
            IF (STATUS.EQ.0) THEN
               WRITE (LU,IOSTAT=STATUS) NX
            END IF
         END IF
      ELSE
         WRITE (LU,'(A79)',IOSTAT=STATUS) STRING(:79)
         IF (STATUS.EQ.0) THEN
            WRITE (LU,'(A79)',IOSTAT=STATUS) COMMENT(:79)
            IF (STATUS.EQ.0) THEN
               WRITE (LU,'(20X,I6)',IOSTAT=STATUS) NX
            END IF
         END IF
      END IF
      IF (STATUS.NE.0) THEN
         CALL GEN_FORTERR(STATUS,.FALSE.,ERROR)
         CALL PAR_WRUSER('Error writing header information',IGNORE)
         CALL PAR_WRUSER(ERROR,IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     Now write out the data arrays.  This has be a subroutine call
C     because of the use of mapped arrays.
C
      CALL FIG_DIPOUT(LU,NX,ICODE,%VAL(CNF_PVAL(XPTR)),
     :                     %VAL(CNF_PVAL(DPTR)),STATUS)
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
      SUBROUTINE FIG_DIPOUT(LU,NX,ICODE,XDATA,ZDATA,STATUS)
C
C     F I G _ D I P O U T
C
C     WDIPSO utility routine.  Writes out the data arrays to the
C     output file in DIPSO format.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) LU     (Integer) The logical unit on which the output file
C                has been opened.  This routine assumes the header data
C                is already written.  The file is not closed.
C     (>) NX     (Integer) Number of elements in the data.
C     (>) ICODE  (Integer) The format code (0,1 or 2) for the output file.
C     (>) XDATA  (Real array XDATA(NX)) The wavelength data array.
C     (>) ZDATA  (Real array ZDATA(NX)) The flux data array.
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
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER LU, NX, ICODE, STATUS
      REAL    XDATA(NX), ZDATA(NX)
C
C     Local variables
C
      INTEGER I
C
C     Write the data out
C
      IF (ICODE.EQ.0) THEN
         WRITE (LU,IOSTAT=STATUS) (XDATA(I),ZDATA(I),I=1,NX)
      ELSE IF (ICODE.EQ.1) THEN
         WRITE (LU,'(4(F8.3,E10.3))',IOSTAT=STATUS)
     :                                    (XDATA(I),ZDATA(I),I=1,NX)
      ELSE
         DO I=1,NX
            WRITE (LU,*,IOSTAT=STATUS) XDATA(I),ZDATA(I)
            IF (STATUS.NE.0) GO TO 320
         END DO
  320    CONTINUE
      END IF
C
      END
