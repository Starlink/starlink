C+
C              W D F I T S
C
C  Name:
C     WDFITS
C
C  Function:
C     Writes a Figaro image out to disk in FITS format.
C
C  Description:
C     Writes a Figaro image out to disk in FITS format.  Since the
C     Figaro format is a superset of FITS, not all the information
C     held in the Figaro file can be written to tape, so this
C     routine concentrates on the main data array.  This means that
C     it is really only suitable for images with no associated
C     calibration data.  If the file has axis structures that
C     contain linear data arrays, these will be converted into the
C     appropriate CDELTn, CRPIXn and CRVALn keywords.  Non-linear axis
C     data will be ignored. The only other information written into
C     the FITS header will be taken from the FITS-specific data
C     structure, should the structure contain one.  Any entries in this
C     structure that can reasonably be output as header quantities
C     (arrays of data cannot) will be.
C
C     If the end of the tape is reached while the data is being written,
C     the tape is backspaced to the start of the image data and an
C     end of tape mark is written.  The user is then given the option
C     of mounting a new tape and repeating the output.  Note that the
C     'standard' FITS recipie for handling end of tape - which allows
C     images to span tapes - is not followed.  In batch mode the user
C     is not given this option.
C
C     This program serves for the command WDFITS, which writes
C     an image out in the AAO de facto 'Disk Fits' format - ie to a
C     disk file whose 2880 byte records are exactly the same as the
C     records on a FITS tape would be, except that the data is not
C     byte-swapped.
C
C  Parameters:
C     IMAGE     (Character) The file containing the Figaro data
C               structure to be written to disk.
C     FILE      (Character) The name of the
C               disk file to which the data is to be written.
C
C  Keywords:
C     BIT16     Forces real data to be written out as 16 bit rather
C               than as 32 bit data.
C
C  User variables used:    (">" input, "<" output)
C
C  Support: Keith Shortridge, AAO.
C
C  Version date: 5th MArch 1993
C-
C  History:
C     14th June 1984. KS / CIT. Original version.
C     21st May 1985.  KS / AAO. Use of FITS.COMMENTS structure added.
C     22nd May 1985.  KS / AAO. Linear .X, .Y etc structures converted
C                     into CRVALn etc. keywords.
C     29th May 1985.  KS / AAO. Integer values now output as integers,
C                     instead of in floating point format.
C     10th June 1985. KS / AAO. Comments now allowed for axis values.
C     20th Sept 1985  KS / AAO. 16BIT added.  'DBLE' changed to 'DOUBLE'
C                    in all type tests.  WIFITS can never have worked
C                    on double precision data!
C     17th June 1986 KS / AAO. WDFITS added.  LIB$GET_LUN used to get
C                    disk logical unit number.
C     9th Oct 1987   KS / AAO. Now allows .FITS.x to be a structure
C                    with .DATA and .DESCRIPTION elements. Length of
C                    filenames increased.
C     5th Nov 1987   KS / AAO. Will now retry with a new tape if end
C                    of tape reached while writing image.
C     30th Jan 1990  KS / AAO. Substantially reworked to use DSA
C                    routines.
C                    BSCALE, BZERO now calculated in double precision in
C                    all cases. Blocked output now supported, through
C                    the new `BLOCKED' parameter.  Support for 'USHORT'
C                    data added.  Actions when end-of-tape hit revised
C                    slightly, and termination after last image made
C                    optional. FITS 'COMMENT', 'HISTORY' and blank
C                    items  treated as comments instead of character
C                    strings (no quotation marks in header).  Axis
C                    keywords now written before keywords taken from
C                    the FITS substructure and so take priority in case
C                    they have different values in the FITS
C                    substructure.  Fault signalled if tape reaches
C                    end and 'continue on new tape' option not taken -
C                    so a procedure can abort cleanly.
C      5th Mar 1993  KS/AAO. Added NOTERM keyword.
C     26th Apr 1993  KS/AAO. Introduced use of FIT_DFOPEN to get around
C                    system-dependent OPEN keywords.
C     20th Jul 1993  HME/UoE, Starlink.  Reduce code to WDFITS (and
C                    WJT) to avoid tape-specific calls to FIT_ and thus
C                    to avoid TIO calls altogether.  The removed code
C                    is commented with asterisks and can be reinstated
C                    easily.
C                    Also fixed the bug when deciding if a "double" can
C                    be written as integer. There a test between the
C                    unrelated float VALUE and the provisional double
C                    INT(DVALUE) was made.
C     11th Aug 1993  HME/UoE, Starlink.  Had forgotten one IF clause
C                     for tapes that made link with TIO_CLOSE necessary.
C      6th Jul 1994  HME/UoE, Starlink.  Rename 16BIT parameter to
C                    BIT16.
C     20th Mar 1996  HME/UoE, Starlink.  Added initial values for BLOCK
C                    and NOTERM.
C     21st Jun 1996  MJC/Starlink, RAL.  Fixed bug that could
C                    generate a second END card in the FITS header.
C     18th Jul 1996  MJCL/Starlink, UCL.  Set variables for storage of
C                    file names to 132 chars.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      SUBROUTINE WIFITS
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions used
C
      LOGICAL   FIG_KEYCHK, FIG_SCRCHK
      LOGICAL   PAR_ABORT
      INTEGER   ICH_CLEAN
      REAL      GEN_ELEMF
      CHARACTER GEN_NTH*2, ICH_CI*1
C
C     Dynamic memory definitions - defines DYNAMIC_MEM
C
      INCLUDE 'DYNAMIC_MEMORY'
C
C     Local variables
C
      CHARACTER ACCESS*1         ! Indicates type to use for FITS item
      LOGICAL   AEXIST           ! Indicates axis data exists
      INTEGER   AXDIM            ! Number of axis array dimensions
      INTEGER   AXELM            ! Number of elements in axis array
      INTEGER   AXPTR            ! Dynamic mem element for axis data
      INTEGER   AXSIZE           ! Length of axis data array
      DOUBLE PRECISION AXVAL1    ! Value of first axis data element
      DOUBLE PRECISION AXVALN    ! Value of last axis data element
      LOGICAL   BIT16            ! Value of BIT16 keyword
      INTEGER   BITPIX           ! Bits per pixel for tape array
      INTEGER   BLOCK            ! Value of BLOCKED parameter
      DOUBLE PRECISION BSCALD    ! Calculated value for BSCALE keyword
      DOUBLE PRECISION BZEROD    ! Calculated value for BZERO keyword
      CHARACTER CHAR_ARRAY(2)*32 ! Used to get axis label and units
      REAL      CHECK(80)        ! Record of keyword values
      CHARACTER COMMAND*16       ! Command being serviced
      CHARACTER COMMENT*64       ! Comment associated with keyword
      DOUBLE PRECISION DELTA     ! Increment in linear axis values
      INTEGER   DIMS(10)         ! Dimensions of main data array
      INTEGER   DSA_STATUS       ! Inherited status used by DSA routines
      DOUBLE PRECISION DUMMY     ! Dummy numeric parameter for axis info
      DOUBLE PRECISION DVALUE    ! General double precision variable
      CHARACTER ERROR*64         ! Text for error status value
      REAL      ERRORS(3)        ! Maximum error for BITPIX values
      DOUBLE PRECISION ERRORD(3) ! Maximum error for BITPIX values
      LOGICAL   EXIST            ! True while there are more FITS items
      LOGICAL   FAULT            ! Indicates non-DSA error detected
      CHARACTER FILE*132         ! Name of output file
      LOGICAL   FINISHED         ! Indicates data written OK to tape
      INTEGER   I                ! General loop index
      INTEGER   IDIM             ! Axis being processed
      INTEGER   INVOKE           ! Don't care function value
      LOGICAL   IOERR            ! Indicates a non-EOT I/O error
      INTEGER   IPTR             ! Dynamic mem element for data array
      INTEGER   LU               ! Logical unit for output disk file
      INTEGER   N                ! General temporary integer
      CHARACTER NAME*32          ! Name of FITS structure item
      CHARACTER NAMECK(80)*8     ! List of names of keywords used
      CHARACTER NCHAR*1          ! Used as '1', '2', etc axis ids.
      INTEGER   NDIM             ! Number of data dimensions
      INTEGER   NELM             ! Number of elements in data array
      LOGICAL   NOTERM           ! Value of 'NOTERM' keyword
      INTEGER   NKEY             ! Counter through keyword check tables
      INTEGER   OBJPTR           ! Counter through FITS items in
                                 ! structure
      INTEGER   OBELM            ! Number of elements in a FITS item
      DOUBLE PRECISION SCALED(3) ! Calculated BSCALE values for BITPIXes
      INTEGER   SLOT             ! Marker for DSA mapping reference
      CHARACTER STRING*40        ! Used for axis label and units
      INTEGER   STRLEN           ! Length of FITS character item -
                                 ! ignored
      LOGICAL   STRUCT           ! Indicates data array is structured
      LOGICAL   SWAP             ! Indicates output is to be
                                 ! byte-swapped
      INTEGER   STATUS           ! General non-DSA status variable
      LOGICAL   TOPEN            ! Indicated tape was opened
      LOGICAL   TOTAPE           ! Indicates output is to tape
      CHARACTER TYPE*16          ! Type of main data array
      LOGICAL   USEINT           ! Can output keyword as an integer
      REAL      VALUE            ! General floating point temporary
      REAL      VMAX             ! Maximum value in data
      REAL      VMIN             ! Minimum value in data
      DOUBLE PRECISION VMAXD     ! Maximum value in 'DOUBLE' data
      DOUBLE PRECISION VMIND     ! Minimum value in 'DOUBLE' data
      DOUBLE PRECISION ZEROD(3)  ! Calculates BZERO values for BITPIXes
C
C     Initial values
C
      FAULT=.FALSE.
      TOPEN=.FALSE.
      BLOCK=1
      NOTERM=.FALSE.
C
C     Initialise DSA routines
C
      DSA_STATUS=0
      CALL DSA_OPEN (DSA_STATUS)
C
C     Is this to tape (command WIFITS) or disk (command WJT/WDFITS)?
C
      CALL PAR_COMMAND(COMMAND)
      TOTAPE=COMMAND.EQ.'WIFITS'
      IF (TOTAPE) THEN
C
C        To tape. Get name of tape drive to use.
C
*        CALL VAR_GETCHR('TAPEO',0,0,TAPE,STATUS)
*        IF (STATUS.NE.0) THEN
*           CALL PAR_WRUSER(
*    :       'The output tape to be used has not been defined',STATUS)
*           CALL PAR_WRUSER(
*    :        'Use the TAPEO command, eg "TAPEO MTA0" to correct this.',
*    :        STATUS)
*           FAULT=.TRUE.
*           GO TO 500
*        END IF
C
C        Now open it
C
*        CALL FIT_INIT(TAPE(:ICH_LEN(TAPE)),STATUS)
*        IF (STATUS.NE.0) THEN
*           CALL FIT_ERROR(STATUS,ERROR)
*           CALL PAR_WRUSER('Unable to open tape drive '//
*    :                      TAPE(:ICH_LEN(TAPE)),STATUS)
*           CALL PAR_WRUSER(ERROR,STATUS)
*           FAULT=.TRUE.
*           GO TO 500
*        END IF
*        TOPEN=.TRUE.
      END IF
C
C     Get the name of the image to be output and open the file
C
      CALL DSA_INPUT ('IMAGE','IMAGE',DSA_STATUS)
      IF (DSA_STATUS.NE.0) GO TO 500
C
C     Find out how the tape is to be positioned (if used)
C
      IF (TOTAPE) THEN
*        REWIND=.FALSE.
*        CURRENT=.FALSE.
*        IF (PAR_GIVEN('REWIND'))
*    :                       CALL PAR_RDKEY('REWIND',.FALSE.,REWIND)
*        IF (PAR_GIVEN('CURRENT'))
*    :                       CALL PAR_RDKEY('CURRENT',.FALSE.,CURRENT)
*        IF ((.NOT.REWIND).AND.(.NOT.CURRENT)) THEN
*           CALL PAR_RDKEY('POSITION',.TRUE.,POSIT)
*           IF (.NOT.POSIT) THEN
*              IF (.NOT.PAR_GIVEN('CURRENT')) THEN
*                 CALL PAR_RDKEY('CURRENT',.TRUE.,CURRENT)
*              END IF
*              IF (.NOT.CURRENT) THEN
*                 IF (.NOT.PAR_GIVEN('REWIND')) THEN
*                    CALL PAR_RDKEY('REWIND',.TRUE.,REWIND)
*                 END IF
*                 IF (.NOT.REWIND) THEN
*                    CALL PAR_WRUSER(
*    :                 'No acceptable tape position, it seems',STATUS)
*                    FAULT=.TRUE.
*                    GO TO 500
*                 END IF
*              END IF
*           END IF
*        END IF
C
C        Position the tape
C
*        POSN='End'
*        IF (REWIND) POSN='Start'
*        IF (CURRENT) POSN='Here'
*        CALL FIT_POSN(POSN,STATUS)
*        IF (STATUS.NE.0) THEN
*           CALL FIT_ERROR(STATUS,ERROR)
*           CALL PAR_WRUSER('Error positioning tape',STATUS)
*           CALL PAR_WRUSER(ERROR,STATUS)
*           FAULT=.TRUE.
*           GO TO 500
*        END IF
C
C        Get blocking factor.
C
*        CALL PAR_RDVAL ('BLOCKED',1.0,10.0,1.0,' ',VALUE)
*        BLOCK=VALUE
*        CALL FIT_BSET(BLOCK)
C
C        Get 'suppress proper termination' flag.
C
*        CALL PAR_RDKEY ('NOTERM',.FALSE.,NOTERM)
      ELSE
C
C        To Disk.  Get name of file and create it.  The open
C        statements for WJT and WDFITS are slightly different,
C        as is the byte swapping.
C
         CALL PAR_RDCHAR('FILE',' ',FILE)
         IF (PAR_ABORT()) GO TO 500
         CALL DSA_GET_LU (LU,DSA_STATUS)
         IF (DSA_STATUS.NE.0) GO TO 500
         IF (COMMAND.EQ.'WJT') THEN
            CALL FIT_DFOPEN (LU,FILE,.FALSE.,STATUS)
            SWAP=.FALSE.
         ELSE
            CALL FIT_DFOPEN (LU,FILE,.TRUE.,STATUS)
            SWAP=.TRUE.
         END IF
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('Unable to open output file',STATUS)
            FAULT=.TRUE.
            GO TO 500
         END IF
         CALL FIT_DINIT(LU,SWAP,STATUS)
      END IF
C
C     Look at the main data array and get its dimensions and type
C
      CALL DSA_DATA_TYPE ('IMAGE',TYPE,STRUCT,DSA_STATUS)
      IF (STRUCT) TYPE='FLOAT'
      CALL DSA_DATA_SIZE ('IMAGE',10,NDIM,DIMS,NELM,DSA_STATUS)
      IF (DSA_STATUS.NE.0) GO TO 500
C
C     Map the data, using the appropriate data type.
C
      IF ((TYPE.EQ.'BIT').OR.(TYPE.EQ.'BYTE')) THEN
         TYPE='BYTE'
         BITPIX=8
      ELSE IF ((TYPE.EQ.'SHORT').OR.(TYPE.EQ.'USHORT')) THEN
         BITPIX=16
      ELSE IF (TYPE.EQ.'INT') THEN
         BITPIX=32
      ELSE IF ((TYPE.NE.'FLOAT').AND.(TYPE.NE.'DOUBLE')) THEN
         CALL PAR_WRUSER('Cannot handle data of type '//TYPE,STATUS)
         FAULT=.TRUE.
         GO TO 500
      END IF
      CALL DSA_MAP_DATA ('IMAGE','READ',TYPE,IPTR,SLOT,DSA_STATUS)
      IF (DSA_STATUS.NE.0) GO TO 500
C
C     If data was one of the real types, calculate the scaling and
C     zero factors required.  Note, real data written to JT's format
C     should be forced to be 16 bit, and the 16BIT keyword may also
C     apply.  Unsigned 16 bit data also requires a scale and zero.
C
      BSCALD=0.
      BZEROD=0.
      IF (TYPE.EQ.'FLOAT') THEN
         CALL FIT_SCALCD (%VAL(CNF_PVAL(IPTR)),NELM,.FALSE.,VMIN,VMAX,
     :                    SCALED,ZEROD,ERRORS)
         IF (COMMAND.NE.'WJT') THEN
            CALL PAR_RDKEY('BIT16',.FALSE.,BIT16)
            IF (PAR_ABORT()) GO TO 500
            IF (BIT16) THEN
               BITPIX=16
               BSCALD=SCALED(2)
               BZEROD=ZEROD(2)
            ELSE
               BITPIX=32
               BSCALD=SCALED(3)
               BZEROD=ZEROD(3)
            END IF
         ELSE
            BITPIX=16
            BSCALD=1.
            BZEROD=0.
         END IF
      ELSE IF (TYPE.EQ.'DOUBLE') THEN
         CALL FIT_SCALD (%VAL(CNF_PVAL(IPTR)),NELM,.FALSE.,VMIND,VMAXD,
     :                   SCALED,ZEROD,ERRORD)
         IF (COMMAND.NE.'WJT') THEN
            CALL PAR_RDKEY('BIT16',.FALSE.,BIT16)
            IF (PAR_ABORT()) GO TO 500
            IF (BIT16) THEN
               BITPIX=16
               BSCALD=SCALED(2)
               BZEROD=ZEROD(2)
            ELSE
               BITPIX=32
               BSCALD=SCALED(3)
               BZEROD=ZEROD(3)
            END IF
         ELSE
            BITPIX=16
            BSCALD=1.
            BZEROD=0.
         END IF
      ELSE IF (TYPE.EQ.'USHORT') THEN
         CALL FIT_SCALU (BSCALD,BZEROD)
      END IF
C
C     This is where we start to write the data out to the tape.  If
C     we hit the end of tape, and are not in batch mode, the user has
C     the option of mounting a new tape and we restart.  Hence the
C     loop on FINISHED.
C
      FINISHED=.FALSE.
      DO WHILE (.NOT.FINISHED)
C
C        Start to write the FITS header with the standard quantities.
C        Also put them into the check arrays so that we can trap any
C        disparate values that might be in the FITS substructure.
C
         NKEY=0
         CALL FIT_HSTAND (BITPIX,NDIM,DIMS,BSCALD,BZEROD,STATUS)
         IF (STATUS.NE.0) GO TO 450
         NKEY=NKEY+1
         NAMECK(NKEY)='BITPIX'
         CHECK(NKEY)=BITPIX
         NKEY=NKEY+1
         NAMECK(NKEY)='NAXIS'
         CHECK(NKEY)=NDIM
         DO I=1,NDIM
            NCHAR=ICH_CI(I)
            NKEY=NKEY+1
            NAMECK(NKEY)='NAXIS'//NCHAR
            CHECK(NKEY)=DIMS(I)
         END DO
         IF (BSCALD.NE.0.0) THEN
            NKEY=NKEY+1
            NAMECK(NKEY)='BSCALE'
            CHECK(NKEY)=BSCALD
            NKEY=NKEY+1
            NAMECK(NKEY)='BZERO'
            CHECK(NKEY)=BZEROD
         END IF
         IF (BLOCK.GT.1) THEN
            CALL FIT_WLOG('BLOCKED',.TRUE.,'May use blocked records',
     :                    STATUS)
            IF (STATUS.NE.0) GO TO 450
         END IF
C
C        Now look at the axis data, checking for linear data.  For any
C        axis that has linear data, generate the CDELTn, CRPIXn, and CRVALn
C        keywords.  Use the label for that axis as a comment for CRVALn,
C        and use any units to generate a CTYPEn keyword.  Note that the
C        restriction to 9 axes is an artificial one that happens to make
C        the formatting easier (only needs one digit).
C
         IF (NDIM.GT.9) THEN
            CALL PAR_WRUSER('Cannot handle calibration data properly '//
     :                      'for more than 9 dimensions',STATUS)
         END IF
         DO IDIM=1,MIN(NDIM,9)
            CALL DSA_SEEK_AXIS ('IMAGE',IDIM,AEXIST,DSA_STATUS)
            IF (AEXIST) THEN
C
C              Axis data exists.  We can't handle multi-dimensional axis
C              data, so we let DSA_AXIS_SIZE format an error message
C              if the dimensions are more than 1.  We then reset the bad
C              status it will return and treat the axis array as non-existent.
C
               CALL DSA_AXIS_SIZE ('IMAGE',IDIM,1,AXDIM,AXSIZE,AXELM,
     :                             DSA_STATUS)
               IF (DSA_STATUS.NE.0) THEN
                  DSA_STATUS=0
                  AEXIST=.FALSE.
               END IF
            END IF
            IF (AEXIST) THEN
               CALL DSA_MAP_AXIS_DATA ('IMAGE',IDIM,'READ','FLOAT',
     :                                 AXPTR,SLOT,DSA_STATUS)
               IF (DSA_STATUS.NE.0) GO TO 500
               IF (FIG_SCRCHK(AXSIZE,%VAL(CNF_PVAL(AXPTR)))) THEN
                  AXVAL1=GEN_ELEMF(%VAL(CNF_PVAL(AXPTR)),1)
                  AXVALN=GEN_ELEMF(%VAL(CNF_PVAL(AXPTR)),AXSIZE)
                  IF (AXSIZE.GT.1) THEN
                     DELTA=(AXVALN-AXVAL1)/DBLE(AXSIZE-1)
                  ELSE
                     DELTA=0.
                  END IF
                  CALL DSA_GET_AXIS_INFO ('IMAGE',IDIM,2,CHAR_ARRAY,
     :                                    0,DUMMY,DSA_STATUS)
                  STRING=CHAR_ARRAY(2)     ! Label
                  INVOKE=ICH_CLEAN(STRING)
                  NCHAR=ICH_CI(IDIM)
                  CALL FIT_WDBLE('CRVAL'//NCHAR,AXVAL1,STRING,STATUS)
                  IF (STATUS.NE.0) GO TO 450

                  NKEY=MIN(NKEY+1,80)
                  NAMECK(NKEY)='CRVAL'//NCHAR
                  CHECK(NKEY)=AXVAL1
                  CALL FIT_WDBLE('CDELT'//NCHAR,DELTA,' ',STATUS)
                  IF (STATUS.NE.0) GO TO 450

                  NKEY=MIN(NKEY+1,80)
                  NAMECK(NKEY)='CDELT'//NCHAR
                  CHECK(NKEY)=DELTA
                  CALL FIT_WREAL('CRPIX'//NCHAR,1.0,' ',STATUS)
                  IF (STATUS.NE.0) GO TO 450

                  NKEY=MIN(NKEY+1,80)
                  NAMECK(NKEY)='CRPIX'//NCHAR
                  CHECK(NKEY)=1.0
                  STRING=CHAR_ARRAY(1)   ! Units
                  IF (STRING.NE.' ') THEN
                     DO I=1,NKEY
                        IF (NAMECK(I).EQ.'CTYPE'//NCHAR) THEN
                           STATUS=1
                           GO TO 420     ! Matches, break loop
                        END IF
                     END DO
  420                CONTINUE
                     IF (STATUS.EQ.0) THEN
                        CALL FIT_WSTR('CTYPE'//NCHAR,
     :                         STRING(:ICH_CLEAN(STRING)),' ',STATUS)
                        IF (STATUS.NE.0) GO TO 450
                     END IF
                  END IF
               ELSE
                  CALL DSA_WRUSER('Warning - '//ICH_CI(IDIM)//
     :                     GEN_NTH(IDIM)//' axis data is not linear,')
                  CALL DSA_WRUSER(' and so cannot be output in terms '
     :                            //'of FITS keywords.')
               END IF
               CALL DSA_UNMAP (SLOT,DSA_STATUS)
            END IF
         END DO
C
C        Now, work through all the elements in any FITS-specific
C        substructure that the data might have, and write any
C        objects it contains into the FITS header.
C
         OBJPTR=0
         EXIST=.TRUE.
         DO WHILE (EXIST)
            OBJPTR=OBJPTR+1
            CALL DSA_NTH_FITS_ITEM ('IMAGE',OBJPTR,EXIST,NAME,ACCESS,
     :                              OBELM,STRLEN,DSA_STATUS)
            IF (STATUS.NE.0) GO TO 500
            IF (EXIST.AND.(ACCESS.NE.' ').AND.
     :          (NAME(1:8).NE.'END     ')) THEN
C
C              Given a new object name that we can handle, what happens
C              next depends on the type (and hence on ACCESS).
C
C              An integer
C
               IF ((ACCESS.EQ.'I').OR.(ACCESS.EQ.'S')) THEN
                  IF (OBELM.EQ.1) THEN
                     CALL DSA_GET_FITS_I ('IMAGE',NAME,1,N,COMMENT,
     :                                    DSA_STATUS)
                     IF (DSA_STATUS.NE.0) GO TO 500
                     IF (FIG_KEYCHK(NKEY,NAMECK,NAME,CHECK,DBLE(N)))
     :                                                            THEN
                        CALL FIT_WINT(NAME,N,COMMENT,STATUS)
                        IF (STATUS.NE.0)  GO TO 450
                        NKEY=MIN(NKEY+1,80)
                        NAMECK(NKEY)=NAME
                        CHECK(NKEY)=N
                     END IF
                  END IF
               END IF
C
C              A real number
C
               IF (OBELM.EQ.1) THEN
                  IF (ACCESS.EQ.'F') THEN
                     CALL DSA_GET_FITS_F ('IMAGE',NAME,1,VALUE,COMMENT,
     :                                    DSA_STATUS)
                     IF (DSA_STATUS.NE.0) GO TO 500
                     USEINT=.FALSE.
                     IF (ABS(VALUE).LT.1.0E8) THEN
                        USEINT=(VALUE.EQ.FLOAT(INT(VALUE)))
                     END IF
                     IF (FIG_KEYCHK(NKEY,NAMECK,NAME,CHECK,DBLE(VALUE)))
     :                                                              THEN
                        IF (USEINT) THEN
                           CALL FIT_WINT(NAME,INT(VALUE),COMMENT,STATUS)
                        ELSE
                           CALL FIT_WREAL(NAME,VALUE,COMMENT,STATUS)
                        END IF
                        IF (STATUS.NE.0)  GO TO 450
                        NKEY=MIN(NKEY+1,80)
                        NAMECK(NKEY)=NAME
                        CHECK(NKEY)=VALUE
                     END IF
                  END IF
                  IF (ACCESS.EQ.'D') THEN
                     CALL DSA_GET_FITS_D ('IMAGE',NAME,1,DVALUE,COMMENT,
     :                                    DSA_STATUS)
                     IF (DSA_STATUS.NE.0) GO TO 500
                     USEINT=.FALSE.
                     IF (ABS(DVALUE).LT.1.0E8) THEN
                        USEINT=(DVALUE.EQ.DBLE(INT(DVALUE)))
                     END IF
                     IF (FIG_KEYCHK(NKEY,NAMECK,NAME,CHECK,DVALUE)) THEN
                        IF (USEINT) THEN
                           CALL FIT_WINT(NAME,INT(DVALUE),COMMENT,
     :                                   STATUS)
                        ELSE
                           CALL FIT_WDBLE(NAME,DVALUE,COMMENT,STATUS)
                        END IF
                        IF (STATUS.NE.0)  GO TO 450
                        NKEY=MIN(NKEY+1,80)
                        NAMECK(NKEY)=NAME
                        CHECK(NKEY)=DVALUE
                     END IF
                  END IF
               END IF
C
C              A character string - which may have multiple values
C
               IF (ACCESS.EQ.'C') THEN
                  DO I=1,OBELM
                     STRING=' '
                     CALL DSA_GET_FITS_C ('IMAGE',NAME,I,STRING,COMMENT,
     :                                    DSA_STATUS)
                     IF (DSA_STATUS.NE.0) GO TO 500
                     IF ((NAME.EQ.' ').OR.(NAME.EQ.'HISTORY').OR.
     :                   (NAME.EQ.'COMMENT')) THEN
                        CALL FIT_WCMT(NAME,STRING(:ICH_CLEAN(STRING)),
     :                                COMMENT,STATUS)
                     ELSE
                        CALL FIT_WSTR(NAME,STRING(:ICH_CLEAN(STRING)),
     :                                COMMENT,STATUS)
                     END IF
                     IF (STATUS.NE.0)  GO TO 450
                  END DO
                  NKEY=MIN(NKEY+1,80)
                  NAMECK(NKEY)=NAME
                  CHECK(NKEY)=0.
               END IF
            END IF
         END DO
C
C        Terminate the header
C
         CALL FIT_WEND(STATUS)
         IF (STATUS.NE.0)  GO TO 450
C
C        Write out the image data in the appropriate type.  Note that
C        BITPIX=16 normally means that the input was 2 byte integer, but
C        in WJT it is also used for FLOAT data.
C
         IF (BITPIX.EQ.8) THEN
            CALL FIT_WRAYB(%VAL(CNF_PVAL(IPTR)),NELM,STATUS)
         ELSE IF (BITPIX.EQ.16) THEN
            IF (TYPE.EQ.'FLOAT') THEN
               CALL FIT_WRAYFD(%VAL(CNF_PVAL(IPTR)),NELM,BITPIX,
     :                         BSCALD,BZEROD,STATUS)
            ELSE IF (TYPE.EQ.'USHORT') THEN
               CALL FIT_WRAYU(%VAL(CNF_PVAL(IPTR)),NELM,STATUS)
            ELSE
               CALL FIT_WRAYS(%VAL(CNF_PVAL(IPTR)),NELM,STATUS)
            END IF
         ELSE IF (BITPIX.EQ.32) THEN
            IF (TYPE.EQ.'INT') THEN
               CALL FIT_WRAYI(%VAL(CNF_PVAL(IPTR)),NELM,STATUS)
            ELSE IF (TYPE.EQ.'FLOAT') THEN
               CALL FIT_WRAYFD(%VAL(CNF_PVAL(IPTR)),NELM,BITPIX,
     :                         BSCALD,BZEROD,STATUS)
            ELSE IF (TYPE.EQ.'DOUBLE') THEN
               CALL FIT_WRAYD(%VAL(CNF_PVAL(IPTR)),NELM,BITPIX,
     :                        BSCALD,BZEROD,STATUS)
            END IF
         END IF
C
C        Flush the data out
C
         IF (NOTERM) CALL FIT_NOTERM
         CALL FIT_CLOSE(STATUS)
         IF (STATUS.NE.0) GO TO 450
C
C        If we get here, the I/O was all OK, so we've finished.
C
         IF (STATUS.EQ.0)  FINISHED=.TRUE.
C
C        Program breaks to here if there was an I/O error from
C        the FIT_ routines.  So if FINISHED is not set, it must be
C        because of an I/O error, indicated by a bad STATUS value.
C
  450    CONTINUE
         IF (.NOT.FINISHED) THEN
C
C           The only error we can handle is if we ran out of tape.
C           If so, give the user (if not in batch mode) the option
C           of mounting a new tape and repeating.  We also make
C           attempting to terminate the tape an option, since this
C           has given problems on some drives (mainly those at the
C           AAT), although it is believed that the operation is
C           perfectly proper.
C
            IOERR=.TRUE.
            IF (TOTAPE) THEN
*              IF (FIT_QEOT(STATUS)) THEN
*                 IOERR=.FALSE.
*                 CALL PAR_WRUSER(
*    :            'End of tape reached.  Image will not fit on tape.',
*    :                                                         STATUS)
*                 IF (PAR_BATCH()) THEN
*                    TIDYUP=.TRUE.
*                 ELSE
*                    TIDYUP=PAR_QUEST('Try to terminate tape properly'
*    :                                  //' after last image?',.TRUE.)
*                 END IF
*                 IF (TIDYUP) THEN
*                    CALL FIT_ABORT(STATUS)
*                    IF (STATUS.NE.0) THEN
*                       CALL FIT_ERROR(STATUS,ERROR)
*                       CALL PAR_WRUSER(
*    :                    'Failed to terminate tape properly.',STATUS)
*                       CALL PAR_WRUSER(ERROR,STATUS)
*                    END IF
*                 END IF
*                 IF (PAR_BATCH()) THEN
*                    FINISHED=.TRUE.
*                    FAULT=.TRUE.
*                 ELSE
*                    IF (PAR_QUEST(
*    :                     'Re-write image on new tape?',.TRUE.)) THEN
*                       CALL FIT_END(STATUS)
*                       CALL FIT_DISMT(STATUS)
*                       TOPEN=.FALSE.
*                       CALL PAR_WRUSER(
*    :                    'Unload current tape, and mount a new one.',
*    :                                                         STATUS)
*                       CALL PAR_WRUSER('NOTE: Any data already on '//
*    :                           'this new tape will be LOST.',STATUS)
*                       DO WHILE (.NOT.PAR_QUEST('Ready?',.FALSE.))
*                          IF (PAR_ABORT()) GO TO 500
*                       END DO
*                       CALL FIT_INIT(TAPE(:ICH_LEN(TAPE)),STATUS)
*                       IF (STATUS.NE.0) THEN
*                          CALL FIT_ERROR(STATUS,ERROR)
*                          CALL PAR_WRUSER(
*    :                            'Unable to reopen tape drive '//
*    :                                     TAPE(:ICH_LEN(TAPE)),STATUS)
*                          CALL PAR_WRUSER(ERROR,STATUS)
*                          FAULT=.TRUE.
*                          GO TO 500
*                       END IF
*                       TOPEN=.TRUE.
*                       CALL FIT_BSET(BLOCK)
*                       CALL FIT_POSN('Start',STATUS)
*                    ELSE
*                       FINISHED=.TRUE.
*                       FAULT=.TRUE.
*                    END IF
*                 END IF
*              END IF
            END IF
C
C           If we weren't finished, and the I/O error wasn't the
C           one we've already handled, log the error and set the
C           finished flag.
C
            IF (IOERR) THEN
               CALL FIT_ERROR(STATUS,ERROR)
               CALL PAR_WRUSER('Error writing data out',STATUS)
               CALL PAR_WRUSER(ERROR,STATUS)
               FAULT=.TRUE.
               FINISHED=.TRUE.
            END IF
         END IF
      END DO
C
C     Close everything down
C
  500 CONTINUE
*     IF (TOPEN) THEN
*        CALL FIT_END(STATUS)
*        IF (STATUS.NE.0) THEN
*           CALL FIT_ERROR(STATUS,ERROR)
*           CALL PAR_WRUSER('Error closing down tape',STATUS)
*           CALL PAR_WRUSER(ERROR,STATUS)
*           FAULT=.TRUE.
*        END IF
*     END IF
      CALL DSA_CLOSE(DSA_STATUS)
C
      IF (FAULT) CALL FIG_SETERR
C
      END
C+
      LOGICAL FUNCTION FIG_KEYCHK (NKEY,NAMECK,NAME,CHECK,VALUE)
C
C     F I G _ K E Y C H K
C
C     WIFITS utility.  Checks that a named numeric keyword has not
C     been previously used.  If it has, it checks that the new value
C     is the same - within reasonable precision limits, and issues
C     a warning if it is not.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NKEY    (Integer) Number of keywords used so far.
C     (>) NAMECK  (Character array, NAMECK(NKEY)) Keywords used.
C     (>) NAME    (Character) Keyword to be checked.
C     (>) CHECK   (Real array CHECK(NKEY)) The values of the keywords.
C     (>) VALUE   (Double precision) The value of the keyword to
C                 be checked.
C
C     Returns -
C
C     (<) FIG_KEYCHK (Logical) True if the keyword has been used
C                 already, false if it has not.  This is irrespective
C                 of whether the value of the keyword is different
C                 or not.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     ICH_CF      (ICH_ package) Encode real number into a string
C     ICH_LEN     (  "    "    ) Posn of last non-blank char in string
C     DSA_WRUSER  (DSA_   "    ) Write string to user.
C
C                                             KS / AAO 22nd May 1985
C     Modified:
C
C     1st Feb 1990.   KS/AAO. Now uses DSA_WRUSER and ICH_CF to improve
C                     output format.  Message completely rephrased.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NKEY
      REAL    CHECK(*)
      DOUBLE PRECISION VALUE
      CHARACTER*(*) NAMECK(*),NAME
C
C     Functions
C
      INTEGER ICH_LEN
      CHARACTER ICH_CF*16
C
C     Local variables
C
      INTEGER KEY
      CHARACTER KNAME*8, STRING*72
C
C     See if name matches and if so check value
C
      FIG_KEYCHK=.TRUE.
      IF (LEN(NAME).GT.8) THEN
         KNAME=NAME(:8)
      ELSE
         KNAME=NAME
      END IF
      DO KEY=1,NKEY
         IF (NAMECK(KEY).EQ.KNAME) THEN
            FIG_KEYCHK=.FALSE.
            IF (ABS(CHECK(KEY)-VALUE)/VALUE.GT.0.000001) THEN
               CALL DSA_WRUSER(
     :           'Warning: the FITS-specific structure of the file ')
               CALL DSA_WRUSER('contains a value of ')
               STRING=ICH_CF(REAL(VALUE))
               CALL DSA_WRUSER(STRING(:ICH_LEN(STRING)))
               CALL DSA_WRUSER(' for the keyword "'//
     :                         KNAME(:ICH_LEN(KNAME)))
               CALL DSA_WRUSER(
     :           '", which conflicts with a previous value of ')
               STRING=ICH_CF(CHECK(KEY))
               CALL DSA_WRUSER(STRING(:ICH_LEN(STRING)))
               CALL DSA_WRUSER(
     :           ' obtained from the file''s actual data.')
               CALL DSA_WRUSER(' The value from the FITS-specific '//
     :                         'structure will be ignored.')
            END IF
            GO TO 500          ! Break out of loop on match
         END IF
      END DO
  500 CONTINUE
C
      END
