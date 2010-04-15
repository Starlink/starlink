C+
      SUBROUTINE GSPIKE
C
C     G S P I K E
C
C     Generates a 'spiketrum' from a table of X and Z values, given a
C     spectrum to use as a template for the X range to be used.  The
C     resulting spiketrum will be a spectrum with the same .X structure
C     as the template spectrum, and a .Z structure that has zeros
C     everywhere except at the points given in the table.  The table
C     file can include SET commands that set individual item values in
C     the resulting file, but the item names need to have been defined
C     in the file SPIKETRUM.DEF.
C
C     Command parameters -
C
C     SPECTRUM    (Character) The name of the template spectrum.
C     TABLE       (Character) The name of the file containing the
C                 table of X and Z values.  If TABLE contains no
C                 extension then '.TAB' will be assumed.
C     SPIKETRUM   (Character) The name of the spiketrum to be creeated.
C                 Note that this will always be a new file.
C
C     Command keywords -  None
C
C     User variables used - None
C
C                                           KS / CIT 7th May 1984
C     Modified:
C
C     29th May 1986  KS / AAO.  Use of DTA_MRVAR to map Z data for
C                    write access corrected.
C      3rd Sep 1987  DJA/ AAO.  Revised DSA_ routines - some specs
C                    changed. Now uses DYN routines for dynamic-memory
C                    handling.
C     29th Aug 1988  JM/ RAL.  Conversion to DSA completed.
C     13th Oct 1988  KS / AAO. Processing order for object specs changed
C                    to bypass bug if .TABLE specified.  Now sets X axis
C                    units and label properly.
C     25th Mar 1991  KS / AAO. Modify to use FIGX_SETOBJ so that now SET
C                    commands in the file have to be supported by
C                    EQUATES in the SPIKETRUM.DEF file (but can now be
C                    used for any file format that the .DEF file
C                    supports).  More testing for '!!' abort requests
C                    added.
C     24th Sep 1992  HME / UoE, Starlink.  Lowercase file extension TAB.
C                    INCLUDE changed. TABs removed. FIGX_SETOBJ is now
C                    DSA_SETOBJ. Lowercase file name spiketrum (.def).
C     18th Jul 1996  MJCL / Starlink, UCL.  Set variables for storage of
C                    file names to 132 chars.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL    PAR_ABORT
      INTEGER    DSA_TYPESIZE
      CHARACTER  ICH_CI*5
C
C     Local variables
C
      CHARACTER    AXINFO(2)*64  ! X-axis units and label.
      INTEGER      BYTES         !
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      DOUBLE PRECISION DUMMY     ! Dummy argument for axis information
      INTEGER      DXPTR         ! Dynamic-memory pointer to input axis
                                 ! data
      INTEGER      DXSLOT        ! Map slot number of input axis data
      LOGICAL      FAULT         !
      INTEGER      IGNORE        ! Used to pass ignorable status
      INTEGER      LUTAB         ! Logical unit number for table data
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NX            ! Size of 1st dimension
      INTEGER      OPTR          ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER      OSLOT         ! Map slot number for output data array
      INTEGER      OXPTR         ! Dynamic-memory pointer to output axis
                                 ! data
      INTEGER      OXSLOT        ! Map slot number of output axis data
      INTEGER      STATUS        ! Running status for DSA_ routines
      CHARACTER    TABLE*132     !
      LOGICAL      TABOPN        !
      INTEGER      WPTR          ! Dynamic-memory pointer to workspace
      INTEGER      WSLOT         ! Map slot number of workspace
C
C     Logical unit for table file
C
      DATA LUTAB/1/
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
      TABOPN=.FALSE.
C
C     Get the name of SPECTRUM and open the file
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get the dimensions of the X-axis data array
C
      CALL DSA_AXIS_SIZE('SPECT',1,1,NDIM,DIMS,NX,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get the name for TABLE
C
      CALL PAR_RDCHAR('TABLE',' ',TABLE)
      IF (PAR_ABORT()) GO TO 500     ! User requested abort
      CALL FIG_OPFILE(TABLE,'tab',LUTAB,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to open table file',IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
      TABOPN=.TRUE.
C
C     Get the name for SPIKETRUM, then create the basic structures in the file.
C
      CALL DSA_READ_STRUCT_DEF('spiketrum',STATUS)
      CALL DSA_SET_STRUCT_VAR('NX',ICH_CI(NX),STATUS)
      CALL DSA_SET_STRUCT_VAR('LVAL','TRUE',STATUS)
      CALL DSA_SET_STRUCT_VAR('RVAL','TRUE',STATUS)
      CALL DSA_CREATE_STRUCTURE('SPIKE','SPIKETRUM','SPIKETRUM',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Create the basic data structure for the spiketrum.
C
      CALL DSA_SET_OBJECT('SPIKE','Spiketrum from table '//TABLE,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map the X-axis data and the main data array
C
      CALL DSA_MAP_AXIS_DATA('SPECT',1,'READ','FLOAT',DXPTR,DXSLOT,
     :                       STATUS)
      IF (STATUS.NE.0) GOTO 500
      CALL DSA_MAP_AXIS_DATA('SPIKE',1,'WRITE','FLOAT',OXPTR,OXSLOT,
     :                       STATUS)
      IF (STATUS.NE.0) GOTO 500
      BYTES=NX*DSA_TYPESIZE('FLOAT',STATUS)
      CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(DXPTR)),%VAL(CNF_PVAL(OXPTR)))
C
      CALL DSA_MAP_DATA('SPIKE','WRITE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Grab space for the work array used
C
      CALL DSA_GET_WORKSPACE(BYTES,WPTR,WSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Set the X-axis label and units
C
      CALL DSA_GET_AXIS_INFO('SPECT',1,2,AXINFO,0,DUMMY,STATUS)
      CALL DSA_SET_AXIS_INFO('SPIKE',1,2,AXINFO,0,DUMMY,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Fill in the data for the spiketrum.
C
      CALL FIG_FSPIKE(NX,%VAL(CNF_PVAL(OXPTR)),LUTAB,
     :                %VAL(CNF_PVAL(WPTR)),%VAL(CNF_PVAL(OPTR)),STATUS)
      IF (STATUS.NE.0) FAULT=.TRUE.
C
C     Tidy up
C
  500 CONTINUE
C
      CALL DSA_CLOSE(STATUS)
C
      IF (TABOPN) THEN
         CLOSE (UNIT=LUTAB,IOSTAT=STATUS)
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('Error closing table file',IGNORE)
            FAULT=.TRUE.
         END IF
      END IF
C
      IF (FAULT) CALL FIG_SETERR
C
      END
C+
      SUBROUTINE FIG_FSPIKE(NX,XDATA,LUTAB,WORK,DATA,STATUS)
C
C     F I G _ F S P I K E
C
C     Reads a table file and fills up a spike array (a 'spiketrum')
C     using the data in the table, which should be basically a list
C     of x values and corresponding data values.  All other elements
C     of the array are set to zero.  The table file may also contain
C     control lines that set data objects in the file.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) NX      (Integer) Number of elements in the arrays.
C     (>) XDATA   (Real array XDATA(NX)) The X data values.
C     (>) LUTAB   (Integer) The logical unit number for the table file.
C                 The file should already be open.  This routine does
C                 not close the file.
C     (W) WORK    (Real arrya WORK(NX)) Workspace.
C     (<) DATA    (Real array DATA(NX)) The spiketrum array.
C     (<) STATUS  (Integer) Status code.  0 => OK, non-zero implies
C                 that an error occured.  An error message will have
C                 been output by this routine.
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     DTA_CRVAR     (DTA_ package) Create a data object
C     DAT_CRNAM     ( "      "   ) Create a data object name
C     DTA_WRVARF    ( "      "   ) Write a real value to a data object
C     DSA_SETOBJ   (DSA_   "   ) Set a data object as described in a record
C     GEN_BSEARCH   (GEN_    "   ) Search an array for a value
C     GEN_FILL      ( "      "   ) Fast fill of an array with a byte value
C     ICH_CFOLD     (ICH_    "   ) Conditional fold to upper case
C     ICH_LEN       ( "      "   ) Position of last non-blank char
C     ICH_NUMGT     ( "      "   ) Decode free format numbers from string
C     PAR_WRUSER    (PAR_    "   ) Write character string to user
C     DSA_TYPESIZE  (DSA_    "   ) Find the size of a data type
C
C     File format -
C
C     A table file has essentially one record per pair of values,
C     giving the X value (usually wavelength) and then the associated
C     data value, in free format, separated by a comma or spaces.
C     Blank records, and records whose first non-blank character is
C     a '*' are ignored.  Records need not be in ascending order
C     of X-value, although that is usually convenient.  Comments may
C     follow the data value, separated from it by spaces, a comma, or
C     a '*'.  The file may also contain 'SET' records, which allow
C     data objects in the file being generated to be set.  These
C     have the form
C
C            SET  data object = value
C
C     where 'data object' should be the name of the data object,
C     using a symbolic name defined as an EQUATEd variable in the
C     SPIKETRUM.DEF file (note that the name can have a structured form
C     and can even be ".Z.UNITS", for example AND that .Z.UNITS can
C     even be EQUATEd to, say, either of ".Z.UNITS" or just ".UNITS"
C     depending on the file format. 'Value' should either be a single
C     numeric quantity or a character string enclosed in double
C     quotes (").  For example
C
C            SET UNITS = "AB magnitudes"
C
C                                         KS / CIT 7th May 1984
C     Modified:
C
C     29th Aug 1988.  JM/RAL. Converted to use DSA routines.
C     13th Oct 1988.  KS/AAO. Problem with object specifications
C                     bypassed - now deferred until after table structure
C                     is created.
C     25th Mar 1991.  KS/AAO. Now uses FIGX_SETOBJ instead of FIG_SETOBJ;
C                     this means values to be set must be supported in
C                     SPIKETRUM.DEF, but also means other file formats can
C                     be supported.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, LUTAB, STATUS
      REAL    WORK(NX), XDATA(NX), DATA(NX)
C
C     Functions
C
      INTEGER GEN_BSEARCH, ICH_LEN, ICH_VERIF, DSA_TYPESIZE
      CHARACTER  ICH_CI*5
C
C     Maximum and minimum real data values - close to VAX limit.
C
      REAL FMAX, FMIN
      PARAMETER (FMAX=1.7E38, FMIN=-1.7E38)
C
C     Maximum number of object specifications allowed
C
      INTEGER MAXOBJ
      PARAMETER (MAXOBJ=10)
C
C     Local variables
C
      LOGICAL MORE, OBJWARN, REV
      INTEGER I, ICODES(2), IEN, IGNORE, IST, IWPT, NELM, NEXT, NOBJ
      REAL    VALUES(2), XLEFT, XRIGHT, XVALUE, ZLEFT, ZRIGHT, ZVALUE
      CHARACTER NAME*32, RECORD*80, NAMOUT*40, OBJSAV(MAXOBJ)*80
C
C     Set the data array to zero
C
      CALL GEN_FILL(NX*DSA_TYPESIZE('FLOAT',STATUS),0,DATA)
C
C     Start reading the table file
C
      REV=XDATA(NX).LT.XDATA(1)
      IF (REV) THEN
         XLEFT=FMAX
         XRIGHT=FMIN
      ELSE
         XLEFT=FMIN
         XRIGHT=FMAX
      END IF
      IWPT=0
      NOBJ=0
      OBJWARN=.FALSE.
      MORE=.TRUE.
      DO WHILE (MORE)
         READ (LUTAB,'(A)',IOSTAT=STATUS) RECORD
         IF (STATUS.EQ.-1) THEN
            MORE=.FALSE.
            STATUS=0
         ELSE IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('I/O error reading from table file',STATUS)
            MORE=.FALSE.
         ELSE
C
C           Record read OK.  Check for a comment record.
C
            IST=MAX(1,ICH_VERIF(RECORD,1,' '))
            IF ((RECORD.NE.' ').AND.(RECORD(IST:IST).NE.'*')) THEN
C
C              Not a comment.  Check for an object specification.
C
               CALL ICH_CFOLD(RECORD,'""')
               IEN=MIN(LEN(RECORD),IST+2)
               IF (RECORD(IST:IEN).NE.'SET') THEN
C
C                 Not an object spec.  Must be an ordinary X,Z pair.
C
                  CALL ICH_NUMGT(RECORD,IST,', ;*',';*',2,VALUES,ICODES,
     :                                                             NEXT)
                  IF ((ICODES(1).NE.0).OR.(ICODES(2).NE.0)) THEN
                     CALL PAR_WRUSER('Error in format of table file',
     :                                                        STATUS)
                     CALL PAR_WRUSER(RECORD(:ICH_LEN(RECORD)),STATUS)
                     MORE=.FALSE.
                     STATUS=1
                  ELSE
                     XVALUE=VALUES(1)
                     ZVALUE=VALUES(2)
                     NELM=GEN_BSEARCH(XDATA,NX,XVALUE)
                     IF (NELM.EQ.0) THEN
C
C                       X value is out of range we want.  See if it
C                       might be one of the bracketing values.
C
                        IF (REV) THEN
                           IF ((XVALUE.GT.XDATA(1)).AND.
     :                                        (XVALUE.LT.XLEFT)) THEN
                              XLEFT=XVALUE
                              ZLEFT=ZVALUE
                           ELSE IF ((XVALUE.LT.XDATA(NX)).AND.
     :                                        (XVALUE.GT.XRIGHT)) THEN
                              XRIGHT=XVALUE
                              ZRIGHT=ZVALUE
                           END IF
                        ELSE
                           IF ((XVALUE.LT.XDATA(1)).AND.
     :                                        (XVALUE.GT.XLEFT)) THEN
                              XLEFT=XVALUE
                              ZLEFT=ZVALUE
                           ELSE IF ((XVALUE.GT.XDATA(NX)).AND.
     :                                        (XVALUE.LT.XRIGHT)) THEN
                              XRIGHT=XVALUE
                              ZRIGHT=ZVALUE
                           END IF
                        END IF
                     ELSE
C
C                       Normal pair of values, in required range. This
C                       is where an element of the spiketrum gets set.
C
                        IWPT=MIN(NX,IWPT+1)
                        WORK(IWPT)=XVALUE
                        DATA(NELM)=ZVALUE
                     END IF
                  END IF
               ELSE
C
C                 Record is an object spec.  Remember it and service
C                 it later.  This needs to be looked at again.  The problem
C                 is that some of these may explicitly specify a structure
C                 (.TABLE for example) that isn't created until the call
C                 to DSA_ADD_STRUCTURE.  In any case, the whole thing needs
C                 abstracting in some way, ideally.
C
                  NOBJ=NOBJ+1
                  IF (NOBJ.GT.MAXOBJ) THEN
                     IF (.NOT.OBJWARN) THEN
                        CALL PAR_WRUSER(
     :                       'Too many object specifications',STATUS)
                        OBJWARN=.TRUE.
                     END IF
                     CALL PAR_WRUSER('Will ignore: '//
     :                  RECORD(:ICH_LEN(RECORD)),STATUS)
                     NOBJ=MAXOBJ
                  ELSE
                     OBJSAV(NOBJ)=RECORD(IST+3:)
                  END IF
               END IF
            END IF
         END IF
      END DO
C
C     Make sure we have some elements in the range
C
      IF (IWPT.LE.0) THEN
         CALL PAR_WRUSER('No table elements in wavelength range',STATUS)
         STATUS=-1
         GO TO 500    ! Error exit
      END IF
C
C     Define the number of elements in the DATA array in the TABLE structure.
C
      CALL DSA_SET_STRUCT_VAR('NTAB',ICH_CI(IWPT),STATUS)
C
C     Add the table structure to the output file.
C
      CALL DSA_ADD_STRUCTURE('SPIKE','TABLE_STRUCTURE',
     :                       'TABLE_STRUCT',STATUS)
C
C     If we have not errored yet, & have bracketing values, set them.
C     We ignore errors here - they shouldn't happen, and don't matter
C     much anyway.  Also set the .DATA array within the .TABLE structure
C     to the exact X values used.
C
      IF (STATUS.EQ.0) THEN
         CALL DSA_ELEMENT_NAME('SPIKE','TABLE_STRUCTURE',NAME,STATUS)
         IF (IWPT.GT.0) THEN
            CALL DTA_CRNAM(NAME,'DATA',1,IWPT,NAMOUT,IGNORE)
            CALL DTA_CRVAR(NAMOUT,'FLOAT',IGNORE)
            CALL DTA_CRNAM(NAME,'DATA',0,0,NAMOUT,IGNORE)
            CALL DTA_WRVARF(NAMOUT,IWPT,WORK,IGNORE)
         END IF
         IF ((XLEFT.NE.FMIN).AND.(XLEFT.NE.FMAX)) THEN
            CALL DTA_CRNAM(NAME,'XLEFT',0,0,NAMOUT,IGNORE)
            CALL DTA_CRVAR(NAMOUT,'FLOAT',IGNORE)
            CALL DTA_WRVARF(NAMOUT,1,XLEFT,IGNORE)
            CALL DTA_CRNAM(NAME,'ZLEFT',0,0,NAMOUT,IGNORE)
            CALL DTA_CRVAR(NAMOUT,'FLOAT',IGNORE)
            CALL DTA_WRVARF(NAMOUT,1,ZLEFT,IGNORE)
         END IF
         IF ((XRIGHT.NE.FMIN).AND.(XRIGHT.NE.FMAX)) THEN
            CALL DTA_CRNAM(NAME,'XRIGHT',0,0,NAMOUT,IGNORE)
            CALL DTA_CRVAR(NAMOUT,'FLOAT',IGNORE)
            CALL DTA_WRVARF(NAMOUT,1,XRIGHT,IGNORE)
            CALL DTA_CRNAM(NAME,'ZRIGHT',0,0,NAMOUT,IGNORE)
            CALL DTA_CRVAR(NAMOUT,'FLOAT',IGNORE)
            CALL DTA_WRVARF(NAMOUT,1,ZRIGHT,IGNORE)
         END IF
C
C        Now process the object specifications.
C
         DO I=1,NOBJ
            CALL DSA_SETOBJ(OBJSAV(I),1,'SPIKE',STATUS)
            IF (STATUS.NE.0) GO TO 320   ! Break I loop
         END DO
  320    CONTINUE
C
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
