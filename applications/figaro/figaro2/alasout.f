C+
      SUBROUTINE ALASOUT
C
C     A L A S O U T
C
C     ALASOUT takes data from a FIGARO spectrum and writes it to a file
C     in ACSII format, suitable for input to ALAS (Absorption Line
C     Analysis Software) as an 'Observed Profile File'.
C     The user can select the X range to be transferred, ie to cover
C     the desired line without too much extra spectrum.  This is
C     important since ALAS has a limit of 300 channels for these input
C     files (at least in version as per Starlink LUN/41.1).
C     For each selected channel an output line is written to the
C     file, giving the X value (F9.3) and the data value (F9.5).
C     Note that ALAS expects these data values to have been
C     normalised by the continuum.
C     The file created will have file type .ALS.
C
C     Command parameters
C
C     SPECTRUM    The input FIGARO spectrum
C     XSTART      First X value to transfer
C     XEND        Last X value to transfer
C     ALASFILE    File name of ALAS format output file (no file type
C                 or version should be appended).
C
C                                J.G. Robertson   September 1985
C     Modified:
C
C     22nd Apr 1986  KS / AAO.  Names of routines EXTRACT and CHUNK
C                    changed to avoid conflict with other Figaro
C                    routines.
C     27th Aug 1987  KS / AAO.  Revised DSA_ routines - some specs
C                    changed. Now uses DYN routines for dynamic-memory
C                    handling. Will no longer fail if data aren't FLOAT
C                    or DOUBLE, as DSA_ routines force conversion.
C                    EXTRCT1 and CHUNK1 removed - their functions being
C                    performed by DSA_ routines much more tidily. Can
C                    no longer alter XSTART,XEND if range is too big,
C                    this would never have worked before anyway!
C     20th Dec 1990  JMS / AAO. Now prints from XSTART to XEND. Also
C                    fixed the array-dimensioning problem by using
C                    dynamic memory.
C     29th Sep 1992  HME / UoE, Starlink.  Lowercase extension .als for
C                    ALAS file. INCLUDE changed. TABs removed. No RECL
C                    keyword in open statement. Give a message if file
C                    open fails.
C     13th Mar 1996  HME / UoE, Starlink.  Adapted to FDA.
C                    No concurrent mapping. Moved getting axis range in
C                    front of mapping axis data.
C                    No error messages from DTA. Call PAR_WRUSER instead
C                    of FIG_DTAERR.
C     18th Jul 1996  MJCL / Starlink, UCL.  ALAS set to 132 chars.
C     16th Jul 1996  MJCL / Starlink, UCL.  PAR_ABORT checking.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER ICH_LEN
      REAL GEN_ELEMF
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
C
C     Local variables
C
      CHARACTER    ALAS*132      ! The filename for the output
      REAL         D             ! Data value from the main input array
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      INTEGER      DPTR          ! Dynamic-memory pointer to data array
      INTEGER      DSLOT         ! Map slot number of input data array
      LOGICAL      FAULT         ! TRUE if an error occurs
      LOGICAL      FOPEN         ! TRUE if output opened OK
      INTEGER      I             ! Counter for pixels being output
      INTEGER      IGNORE        ! Used to pass ignorable status
      INTEGER      IXEN          ! Second pixel number to be selected
      INTEGER      IXST          ! First pixel number to be selected
      INTEGER      LEALAS        ! Length of output filename
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NPIX          ! Number of array elements to be
                                 ! transferred
      INTEGER      NX            ! Size of the 1st dimension of the
                                 ! input array
      INTEGER      NXX           ! Size of the x-axis array
      INTEGER      PDPOS         ! Position of '.' in output filename
      INTEGER      STATUS        ! Running status for DSA_ routines
      CHARACTER    STRING*80     ! Output message text
      REAL         X             ! A value from the x-axis array
      LOGICAL      XEXIST        ! Valid x-axis structure exists?
      INTEGER      XPTR          ! Dynamic-memory pointer to x-axis data
                                 ! array
      INTEGER      XSLOT         ! Map slot number of input x-axis data
                                 ! array
      REAL         XVEN          ! Value of last pixel in specified
                                 ! range
      REAL         XVST          ! Value of first pixel in specified
                                 ! range
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Initial value
C
      FOPEN=.FALSE.
      FAULT=.FALSE.
C
C     Get input name
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE('SPECT',10,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      IF (STATUS.NE.0) GO TO 500
C
C     Get output file name
C
      CALL PAR_RDCHAR('ALASFILE',' ',ALAS)
      IF ( PAR_ABORT() ) GO TO 500
      PDPOS=INDEX(ALAS,'.')
      IF (PDPOS.NE.0) THEN
         CALL PAR_WRUSER
     :     ('File type (and version) cannot be specified',IGNORE)
         ALAS=ALAS(:PDPOS-1)
      END IF
      LEALAS=ICH_LEN(ALAS)
      ALAS=ALAS(:LEALAS)//'.als'
      WRITE(STRING,1) ALAS
    1 FORMAT('Output ALAS format file will be ',A47)
      CALL PAR_WRUSER(STRING,IGNORE)
      OPEN(FILE=ALAS,UNIT=2,STATUS='NEW',IOSTAT=STATUS)
      IF (STATUS.NE.0) THEN
         FAULT=.TRUE.
         CALL PAR_WRUSER('Failed to open output file.',IGNORE)
         GOTO 500
      END IF
      FOPEN=.TRUE.
C
C     Check that there is a valid X-axis array in the input file
C
      CALL DSA_SEEK_AXIS('SPECT',1,XEXIST,STATUS)
      IF ((STATUS.NE.0).OR.(.NOT.XEXIST)) THEN
         CALL PAR_WRUSER(
     :      'Input file does not have valid X-array',IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
      CALL DSA_AXIS_SIZE('SPECT',1,1,NDIM,DIMS,NXX,STATUS)
      IF ((STATUS.NE.0).OR.(NX.NE.NXX)) THEN
         CALL PAR_WRUSER
     :     ('X-axis array size different from data array size',IGNORE)
         GO TO 500
      END IF
C
C     Get XSTART and XEND.
C
      CALL DSA_AXIS_RANGE('SPECT',1,' ',.FALSE.,XVST,XVEN,IXST,IXEN,
     :                    STATUS)
C
      NPIX=IXEN-IXST+1
      IF (NPIX.GT.300) THEN
         CALL PAR_WRUSER('***  WARNING  ***   More than'
     :                   //' 300 channels specified.',IGNORE)
         CALL PAR_WRUSER('May be rejected by ALAS',IGNORE)
      END IF
C
C     Map data
C
      CALL DSA_MAP_AXIS_DATA('SPECT',1,'READ','FLOAT',XPTR,XSLOT,
     :                                                       STATUS)
      IF (STATUS.NE.0) GOTO 500
C
      CALL DSA_MAP_DATA('SPECT','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Write to output file
C
      DO I=IXST,IXEN
         X=GEN_ELEMF(%VAL(CNF_PVAL(XPTR)),I)
         D=GEN_ELEMF(%VAL(CNF_PVAL(DPTR)),I)
         IF(X.GE.99999. .OR. X.LT.-9999.)THEN
           CALL PAR_WRUSER('X-axis value out of range -9999 to +99999.'
     :                                     //'  Set to 99999 !',IGNORE)
           X=99999.
         END IF
         IF(D.GE.999. .OR. D.LT.-99.)THEN
            CALL PAR_WRUSER('Data value out of range -99 to +999. '
     :                                     //' Set to 999 !',IGNORE)
            D=999.
         END IF
         WRITE(2,5)X,D
    5    FORMAT(3X,F9.3,3X,F9.5)
      END DO
C
C     Terminate
C
  500 CONTINUE
C
      IF (FOPEN) CLOSE(UNIT=2,IOSTAT=STATUS)
C
      CALL DSA_CLOSE(STATUS)
C
      IF (FAULT) CALL FIG_SETERR
C
      END
