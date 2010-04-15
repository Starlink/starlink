C+
      SUBROUTINE OFFDIST
C
C     O F F D I S T
C
C     Modifies the output s-distortion file produced by SDIST
C     to add an offset in Y to the fitted positions.
C
C     Command parameters -
C
C     INFILE    (Character) The name of the distortion file to
C               be modified.
C     OFFSET    (Numeric) The offset in Y to be added to the fits.
C     OUTFILE   (Character) The name of the resulting modified file.
C               Note that many of the applications that use these
C               files assume explicitly that they are called SDIST.DAT.
C               If OUTFILE is the same as INFILE, a new version of the
C               file is produced.
C
C     Command keywords - None
C
C     Input and output files -
C
C     SDIST.DAT contains the results of the fit(s), as written by
C               SDIST, in a format treated as follows -
C
C               3 header lines, all beginning with '*'
C               One line giving the number of spectra traced, in the
C               format 20X,I5.
C               Then, for each spectrum traced, one record giving
C               the spectrum number, and the leftmost and rightmost
C               pixels covered by the trace, then 1 record including
C               the average Y value in the spectrum, in format 16X,F13.7,
C               which is followed by 4 records giving the 11
C               polynomial coefficients for the fit.  Note that this
C               program only assumes the number of records for each
C               spectrum, and the position of the average Y value.  It
C               carefully only modifies the Y value fields, leaving all
C               the rest unchanged.
C
C     History:
C
C     16th Aug 1988.  Original version.  KS/AAO.
C     29th Sep 1992.  HME / UoE, Starlink.  No DEFAULTFILE keyword in
C                     OPEN statement, lowercase file sdist.dat. Output
C                     file sdist.dat2.
C      2nd Jul 1993.  KS/AAO. To get a clean compilation on SUNs, removed
C                     remaining VMS specific OPEN specifiers.
C+
      IMPLICIT NONE
C
C     Local variables
C
      LOGICAL   FAULT              ! Non DSA- error detected.
      INTEGER   INLU               ! Logical unit of infile
      CHARACTER FILENAME*132       ! Full name of output file
      INTEGER   FSTATUS            ! Status from I/O operations
      INTEGER   I                  ! Loop index
      INTEGER   IGNORE             ! Used to receive unimportant status value
      CHARACTER INFILE*132         ! Name of input file
      INTEGER   ISPECT             ! Loop index through spectra
      INTEGER   NSPECT             ! Number of spectra
      REAL      OFFSET             ! Value fo OFFSET parameter
      CHARACTER OUTFILE*132        ! Name of output file
      INTEGER   OUTLU              ! Logical unit of outfile
      CHARACTER RECORD*132         ! File records
      INTEGER   STATUS             ! Running status for DSA_ routines
      REAL      YPOSN              ! Average Y position of spectrum
C
C     Functions
C
      INTEGER ICH_LEN
C
C     Initial values
C
      FAULT=.FALSE.
C
C     Initialise DSA routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
C
C     Get name and open distortion file
C
      CALL PAR_RDCHAR('INFILE','sdist.dat',INFILE)
      CALL DSA_GET_LU(INLU,STATUS)
      IF (STATUS.NE.0) GO TO 500
      OPEN (UNIT=INLU,FILE=INFILE,STATUS='OLD',IOSTAT=FSTATUS)
      IF (FSTATUS.NE.0) THEN
         CALL PAR_WRUSER(
     :     'Unable to open distortion analysis file '//
     :                             INFILE(:ICH_LEN(INFILE)),IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     Get offset value
C
      CALL PAR_RDVAL('OFFSET',-1000.0,1000.0,0.0,'Pixels',OFFSET)
C
C     Get name of output file and open it.
C
      CALL PAR_RDCHAR('OUTFILE','sdist.dat2',OUTFILE)
      CALL DSA_GET_LU(OUTLU,STATUS)
      CALL DSA_OPEN_TEXT_FILE (OUTFILE,' ','NEW',.TRUE.,OUTLU,
     :                                        FILENAME,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER(
     :     'Unable to create new distortion analysis file '//
     :                             OUTFILE(:ICH_LEN(OUTFILE)),IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     Copy header records and number of fitted spectra.
C
      DO I=1,4
         CALL FIG_DIST_READ (INLU,INFILE,RECORD,FAULT)
         IF (FAULT) GO TO 500
         CALL FIG_DIST_WRITE (OUTLU,OUTFILE,RECORD,FAULT)
         IF (FAULT) GO TO 500
      END DO
C
C     Read number of fitted spectra - this will be in the last record
C
      READ (RECORD,'(20X,I5)',IOSTAT=FSTATUS) NSPECT
      IF (FSTATUS.NE.0) THEN
         CALL PAR_WRUSER('Format error in number of spectra in file'//
     :                             INFILE(:ICH_LEN(INFILE)),IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     Work through the spectra one by one.
C
      DO ISPECT=1,NSPECT
C
C        Copy the left, right, record.
C
         CALL FIG_DIST_READ (INLU,INFILE,RECORD,FAULT)
         IF (FAULT) GO TO 500
         CALL FIG_DIST_WRITE (OUTLU,OUTFILE,RECORD,FAULT)
         IF (FAULT) GO TO 500
C
C        Now read the record giving the average Y value.
C
         CALL FIG_DIST_READ (INLU,INFILE,RECORD,FAULT)
         IF (FAULT) GO TO 500
         READ (RECORD,'(16X,F13.7)',IOSTAT=FSTATUS) YPOSN
         IF (FSTATUS.NE.0) THEN
            CALL PAR_WRUSER('Format error in average Y position in '//
     :                             INFILE(:ICH_LEN(INFILE)),IGNORE)
            FAULT=.TRUE.
            GO TO 500
         END IF
C
C        Modify the Y value and write the record out.
C
         YPOSN = YPOSN + OFFSET
         WRITE (RECORD(17:29),'(F13.7)',IOSTAT=FSTATUS) YPOSN
         IF (FSTATUS.NE.0) THEN
            CALL PAR_WRUSER('Error formatting average Y position for '//
     :                                OUTFILE(:ICH_LEN(OUTFILE)),IGNORE)
            FAULT=.TRUE.
            GO TO 500
         END IF
         CALL FIG_DIST_WRITE (OUTLU,OUTFILE,RECORD,FAULT)
         IF (FAULT) GO TO 500
C
C        Write out the coefficient records
C
         DO I=1,4
            CALL FIG_DIST_READ (INLU,INFILE,RECORD,FAULT)
            IF (FAULT) GO TO 500
            CALL FIG_DIST_WRITE (OUTLU,OUTFILE,RECORD,FAULT)
            IF (FAULT) GO TO 500
         END DO
C
      END DO
C
C     Record full name of output file.
C
      CALL PAR_WRUSER ('New distortion analysis file '//
     :          FILENAME(:ICH_LEN(FILENAME))//' written.',IGNORE)
C
C     Tidy up
C
  500 CONTINUE
C
C     Close everything down (will include both files)
C
      CALL DSA_CLOSE(STATUS)
      IF (FAULT) CALL FIG_SETERR
C
      END
C+
      SUBROUTINE FIG_DIST_READ (INLU,INFILE,RECORD,FAULT)
C
C     F I G _ D I S T _ R E A D
C
C     Utility routine for OFFDIST.  Reads a record from the input
C     distortion file.
C
C     Parameters - (">" input, "<" output)
C
C     (>) INLU     (Integer) Logical unit of input file.
C     (>) INFILE   (Character) Name of input file.
C     (<) RECORD   (Character) Record read from file.
C     (<) FAULT    (Logical) Indicates success or failure of read.
C
C                                             KS / AAO  16th Aug 1988
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL FAULT
      INTEGER INLU
      CHARACTER*(*) INFILE,RECORD
C
C     Functions
C
      INTEGER ICH_LEN
C
C     Local variables
C
      INTEGER FSTATUS                   ! Fortran I/O status
      INTEGER IGNORE                    ! Status from error output - ignored
      CHARACTER STRING*80
C
      READ (INLU,'(A)',IOSTAT=FSTATUS) RECORD
      IF (FSTATUS.NE.0) THEN
         STRING='I/O error reading from distortion file '//
     :                                INFILE(:ICH_LEN(INFILE))
         CALL PAR_WRUSER(STRING,IGNORE)
         FAULT=.TRUE.
      ELSE
         FAULT=.FALSE.
      END IF
C
      END
C+
      SUBROUTINE FIG_DIST_WRITE (OUTLU,OUTFILE,RECORD,FAULT)
C
C     F I G _ D I S T _ W R I T E
C
C     Utility routine for OFFDIST.  Writes a record to the output
C     distortion file.
C
C     Parameters - (">" input, "<" output)
C
C     (>) OUTLU    (Integer) Logical unit of output file.
C     (>) OUTFILE  (Character) Name of output file.
C     (>) RECORD   (Character) Record to be written to file.
C     (<) FAULT    (Logical) Indicates success or failure of write.
C
C                                             KS / AAO  16th Aug 1988
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL FAULT
      INTEGER OUTLU
      CHARACTER*(*) OUTFILE,RECORD
C
C     Functions
C
      INTEGER ICH_LEN
C
C     Local variables
C
      INTEGER FSTATUS                   ! Fortran I/O status
      INTEGER IGNORE                    ! Status from error output - ignored
      INTEGER RLEN                      ! Significant characters in record
      CHARACTER STRING*80
C
      RLEN=MAX(1,ICH_LEN(RECORD))
      WRITE (OUTLU,'(A)',IOSTAT=FSTATUS) RECORD(:RLEN)
      IF (FSTATUS.NE.0) THEN
         STRING='I/O error writing to distortion file '//
     :                             OUTFILE(:ICH_LEN(OUTFILE))
         CALL PAR_WRUSER(STRING,IGNORE)
         FAULT=.TRUE.
      ELSE
         FAULT=.FALSE.
      END IF
C
      END
