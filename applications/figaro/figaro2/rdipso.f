C+
      SUBROUTINE RDIPSO
C
C     R D I P S O
C
C     Reads a file in DIPSO/IUEDR format and creates a Figaro file from
C     the data in it.  The file can have been written in any of what
C     IUEDR calls SPECTRUM type 0, type 1 or type 2 format.
C
C     Command parameters -
C
C     FILE      (Character) The name of the DIPSO format file to be
C               read.
C     CODE      (Numeric) The SPECTRUM type code for the format (0,1 or
C                2).
C     SPECTRUM  (Character) The name of the Figaro file to be created.
C
C     Command keywords -  None
C
C                                               KS / AAO 13th Oct 1986
C
C     Modified:
C
C      2nd Sep 1987  DJA/ AAO. Revised DSA_ routines - some specs
C                    changed. Now uses DYN routines for dynamic-memory
C                    handling.
C     21st Oct 1992  HME / UoE, Starlink.  INCLUDE changed, TABs
C                    removed.
C     26th Jun 1993  KS/AAO. Removed READONLY from OPEN statements in
C                    interests of portability. Changed to use
C                    DSA_GET_LU instead of VMS routines. Removed unused
C                    variables. Made the name of the
C                    structure-definition file lower case.
C     18th Jul 1996  MJCL / Starlink, UCL.  Set variables for storage of
C                    file names to 132 chars.
C     29th Jul 1996  MJCL / Starlink UCL.  PAR_ABORT checking.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER   ICH_ENCODE, ICH_TIDY
      CHARACTER ICH_CI*5
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
C
C     Local variables
C
      CHARACTER    COMMENT*80    !
      CHARACTER    ERROR*64      ! Error message text
      LOGICAL      FAULT         ! TRUE if an error occurred
      CHARACTER    FILE*132      ! The input file name
      LOGICAL      FOPEN         ! TRUE if the input file was opened OK
      INTEGER      ICODE         ! The input mode
      INTEGER      IGNORE        ! Used to pass ignorable status
      INTEGER      INVOKE        ! Used to invoke functions
      INTEGER      LENGTH        ! Length of a string
      INTEGER      LU            ! The input's logical unit number
      INTEGER      NEXT          ! String index position
      INTEGER      NX            ! Size of 1st dimension
      CHARACTER    OBJECT*80     ! The name of the object
      INTEGER      OPTR          ! Dynamic-memory pointer to output
                                 ! data array
      INTEGER      OSLOT         ! Map slot number for output data array
      INTEGER      STATUS        ! Running status for DSA_ routines
      CHARACTER    STRING*80     ! Output message text
      REAL         VALUE         ! Temporary real number
      INTEGER      XPTR          ! Dynamic-memory pointer to output axis
                                 ! data
      INTEGER      XSLOT         ! Map slot number of output axis data
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
C     First get a fortran logical unit to work with
C
      CALL DSA_GET_LU (LU,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get the name of the file to be converted, and get its format code
C
      CALL PAR_RDCHAR('FILE',' ',FILE)
      CALL PAR_RDVAL('CODE',0.0,2.0,0.0,' ',VALUE)
      IF ( PAR_ABORT() ) GO TO 500
      ICODE=NINT(VALUE)
C
C     Open the file
C
      IF (ICODE.EQ.0) THEN
         OPEN (UNIT=LU, ACCESS='SEQUENTIAL',FORM='UNFORMATTED',
     :                      STATUS='OLD',FILE=FILE,IOSTAT=STATUS)
      ELSE
         OPEN (UNIT=LU,ACCESS='SEQUENTIAL',STATUS='OLD',FILE=FILE,
     :                                              IOSTAT=STATUS)
      END IF
      IF (STATUS.NE.0) THEN
         CALL GEN_FORTERR(STATUS,.FALSE.,ERROR)
         CALL PAR_WRUSER('Unable to open input file',IGNORE)
         CALL PAR_WRUSER(ERROR,IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
      FOPEN=.TRUE.
C
C     Read object name and comments from file, and number of elements.
C
      IF (ICODE.EQ.0) THEN
         READ(LU,IOSTAT=IGNORE) OBJECT(:79)
         READ(LU,IOSTAT=IGNORE) COMMENT(:79)
         READ (LU,IOSTAT=STATUS) NX
      ELSE
         READ (LU,'(A79)',IOSTAT=STATUS) OBJECT(:79)
         IF (STATUS.EQ.0) THEN
            READ (LU,'(A79)',IOSTAT=STATUS) COMMENT(:79)
            IF (STATUS.EQ.0) THEN
               READ (LU,*,IOSTAT=STATUS) NX
            END IF
         END IF
      END IF
      IF (STATUS.NE.0) THEN
         CALL GEN_FORTERR(STATUS,.FALSE.,ERROR)
         CALL PAR_WRUSER('Error reading header information',IGNORE)
         CALL PAR_WRUSER(ERROR,IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     Log header information for user
C
      CALL PAR_WRUSER(' ',IGNORE)
      LENGTH=ICH_TIDY(OBJECT)
      CALL PAR_WRUSER(OBJECT(:LENGTH),IGNORE)
      LENGTH=ICH_TIDY(COMMENT)
      IF (LENGTH.GT.0) CALL PAR_WRUSER(COMMENT(:LENGTH),IGNORE)
      STRING='Data has '
      INVOKE=ICH_ENCODE(STRING,FLOAT(NX),10,0,NEXT)
      STRING(NEXT:)=' elements'
      CALL PAR_WRUSER(STRING(:NEXT+8),IGNORE)
      CALL PAR_WRUSER(' ',IGNORE)
C
C     Create output file structure
C
      CALL DSA_READ_STRUCT_DEF('spectrum',STATUS)
      CALL DSA_SET_STRUCT_VAR('NX',ICH_CI(NX),STATUS)
      CALL DSA_CREATE_STRUCTURE('SPECT','SPECTRUM','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Write the object name to the output
C
      CALL DSA_SET_OBJECT('SPECT',OBJECT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map the data arrays
C
      CALL DSA_MAP_DATA('SPECT','WRITE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
      CALL DSA_MAP_AXIS_DATA('SPECT',1,'WRITE','FLOAT',XPTR,XSLOT,
     :                       STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Now read in the data arrays.  This has be a subroutine call
C     because of the use of mapped arrays.
C
      CALL FIG_DIPIN(LU,NX,ICODE,%VAL(CNF_PVAL(XPTR)),
     :               %VAL(CNF_PVAL(OPTR)),STATUS)
      IF (STATUS.NE.0) THEN
         CALL GEN_FORTERR(STATUS,.FALSE.,ERROR)
         CALL PAR_WRUSER('Error reading data from output file',IGNORE)
         CALL PAR_WRUSER(ERROR,IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     Tidy up
C
  500 CONTINUE
C
      CALL DSA_CLOSE(STATUS)
C
C     Close input file
C
      IF (FOPEN) THEN
         CLOSE (LU,IOSTAT=STATUS)
         IF (STATUS.NE.0) THEN
            CALL GEN_FORTERR(STATUS,.FALSE.,ERROR)
            CALL PAR_WRUSER('Error closing input file',IGNORE)
            CALL PAR_WRUSER(ERROR,IGNORE)
            FAULT=.TRUE.
         END IF
      END IF
C
      IF (FAULT) CALL FIG_SETERR
C
      END
C+
      SUBROUTINE FIG_DIPIN(LU,NX,ICODE,XDATA,ZDATA,STATUS)
C
C     F I G _ D I P I N
C
C     RDIPSO utility routine.  Reads in the data arrays from the
C     DIPSO format input file.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) LU     (Integer) The logical unit on which the input file
C                has been opened.  This routine assumes the header data
C                is already read.  The file is not closed.
C     (>) NX     (Integer) Number of elements in the data.
C     (>) ICODE  (Integer) The format code (0,1 or 2) for the input file.
C     (<) XDATA  (Real array XDATA(NX)) The wavelength data array.
C     (<) ZDATA  (Real array ZDATA(NX)) The flux data array.
C     (<) STATUS (Integer) Status code.  0 => OK, non-zero values are
C                Fortran I/O error codes.
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C                                        KS / AAO 13th Oct 1986
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
C     Read the data in.
C
      IF (ICODE.EQ.0) THEN
         READ (LU,IOSTAT=STATUS) (XDATA(I),ZDATA(I),I=1,NX)
      ELSE IF (ICODE.EQ.1) THEN
         READ (LU,'(4(F8.3,E10.3))',IOSTAT=STATUS)
     :                                    (XDATA(I),ZDATA(I),I=1,NX)
      ELSE
         READ (LU,*,IOSTAT=STATUS) (XDATA(I),ZDATA(I),I=1,NX)
      END IF
C
      END
