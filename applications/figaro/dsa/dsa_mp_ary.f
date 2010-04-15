C+
C                      D S A _ M A P _ A R R A Y
C
C  Routine name:
C     DSA_MAP_ARRAY
C
C  Function:
C     Maps a named DTA_ data object.
C
C  Description:
C     Given the DTA_ system name of a data object that is either an
C     array or a structure that can provide array data, this routine
C     maps that data array (the whole or part of the array) and returns
C     the address of the dynamic memory array that may be used to
C     access the data.  Optionally, this routine can replace flagged
C     values in the data and remember their positions for replacement
C     later.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_MAP_ARRAY (NAME, MODE, TYPE, START, NMAP, NELM, COPY,
C                                UNFLAG, PROP, ADDRESS, SLOT, STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NAME       (Fixed string,descr) The DTA_ system name of the
C                    array to be mapped.
C     (>) MODE       (Fixed string,descr) A string that should be one of
C                    'READ', 'WRITE', or 'UPDATE', depending on how the
C                    data is to be accessed.  Only the first character is
C                    significant.
C     (>) TYPE       (Fixed string,descr) A string specifying the type of
C                    the array that the data is to be mapped onto.  This
C                    can be one of 'BYTE', 'CHAR', 'FLOAT', 'SHORT',
C                    'INT', 'USHORT' or 'DOUBLE' - and must be in upper case.
C     (>) START      (Integer,ref) The number of the first element in the
C                    array to be mapped - treating the array as linear, with
C                    elements numbered from 1 to NELM.
C     (>) NMAP       (Integer,ref) The number of elements in the array to be
C                    mapped. If the whole of the array is to be mapped,
C                    set START=1, NMAP=NELM.
C     (>) NELM       (Integer,ref) The number of elements in the data
C                    array.  Note that the calling program is expected
C                    to have determined this.
C     (>) COPY       (Logical,ref) If set, indicates that the mapped data
C                    is to be a copy of the data in the file, rather than
C                    mapped directly onto the file.  This usually indicates
C                    that some background processing is going to occur
C                    (like removal of flagged values) that should not be
C                    allowed to affect the file data itself.
C     (>) UNFLAG     (Logical,ref) If set, indicates that `flagged' values
C                    in the array are to be replaced but their positions
C                    remembered.
C     (>) PROP       (Logical,ref) If set, `flagged' values are to be
C                    propagated if any data conversion is performed.
C                    Normally, if the array contains such values they should
C                    be propagated, so this usually simply indicates that the
C                    data does in fact contain `flagged' values, and if false
C                    indicates that any values that may match the `flag' values
C                    do so only by coincidence and should be treated as actual
C                    values.
C     (<) ADDRESS    (Integer,ref) The address of the mapped array.
C     (<) SLOT       (Integer,ref) Slot number used as a handle for the
C                    mapping.
C     (!) STATUS     (Integer,ref) Status code.  If a non-zero value is
C                    passed, this routine will return immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ system.
C
C  External subroutines / functions used:
C     CNF_PVAL, DSA_WRUSER, DSA_GET_WORKSPACE, DSA_FMTCON, DSA_CSTRUCT
C     DTA_TYVAR, DTA_STRUC, DTA_ERROR, DTA_FRVAR, DTA_MRVARx, DTA_MUVARx
C     ICH_FOLD, ICH_CI, ICH_LEN, DSA_WRNAME, DSA_GET_WORK_ARRAY,
C     DSA_UNFLAG_DATA, CNV_PROPAGATE, CNV_QPROP, DSA__STRUCT_ARRAY,
C     DSA__ARRAY_ORIGIN, DSA_WRFLUSH
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Version date: 25th October 1994.
C
C  Authors: Keith Shortridge, AAO
C           Horst Meyerdierks, UoE, Starlink
C           Malcolm J. Currie, Starlink
C-
C  Common variable details:
C     (<) DTA_CODE      (Integer) Last DTA_ system error code.
C     (>) MAX_MAPS      (Integer parameter) Maximum number of mapped arrays.
C     (>) MAX_AXES      (Integer parameter) Maximum number of axes for data.
C     (!) MAP_USED      (Logical array) Indicates map slot in use.
C     (!) MAP_NAMES     (String array) Names of mapped arrays.
C     (!) MAP_ACTNAM    (String array) Names of actual arrays mapped.
C     (!) MAP_COUNT     (Integer array) Reference count for this array.
C     (!) MAP_POINTER   (Integer array) Memory address for the array.
C     (!) MAP_WORK      (Integer array) Workspace slot associated with array.
C     (!) MAP_MODE      (Character array) Access type for the data.
C     (!) MAP_SIZE      (Integer array) Number of elements in mapped array.
C     (!) MAP_CODE      (Integer array) Type code for the mapped array.
C     (!) MAP_TYPE      (String array) Type of actual data object.
C     (!) MAP_SCALED    (Logical array) Indicates object is a scaled array.
C     (!) MAP_START     (Integer array) First element mapped in array.
C     (!) WORK_LINK     (Integer array) Link to other work slots.
C     (<) WORK_PROP     (Logical array) Indicates flagged pixels propagated.
C     (>) MAX_MAP_CALLS (Integer parameter) Maximum number of map calls.
C     (!) MAP_CALL_USED (Logical array) Indicates table entry in use.
C     (<) MAP_CALL_MODE (Character array) Mode of mapping.
C     (<) MAP_CALL_SLOT (Integer array) Map table entry corresponding to call.
C     (<) MAP_CALL_WORK (Integer array) Work entry corresponding to call.
C     (<) MAP_CALL_NFLAG(Integer array) Number of values unflagged in array.
C     (<) MAP_CALL_FSLOT(Integer array) Work entry used for flag data.
C     (<) MAP_CALL_VSLOT(Integer array) Work entry used for err<->variance
C                       array conversions.
C
C  Subroutine / function details:
C     CNF_PVAL         Full pointer to dynamically allocated memory
C     DTA_TYVAR        Get type of data object
C     DTA_STRUC        Determine if a data object is a structure
C     DTA_ERROR        Get error message from a DTA_ code
C     DTA_FRVAR        Unmap a data object
C     DTA_MRVARx       Map data object (x = F,I,B,C,D or S) readonly
C     DTA_MUVARx       Map data object (x = F,I,B,C,D or S) for update
C     ICH_LEN          Position of last non-blank char in string
C     ICH_CI           Format an integer into a string
C     ICH_FOLD         Convert a string to upper case
C     DSA_FMTCON       Data array type conversion
C     DSA_CSTRUCT      Process a contracted structure
C     DSA_GET_WORKSPACE  Get a specified amount of workspace.
C     DSA_GET_WORKARRAY  Get a specified amount of workspace of given type.
C     DSA_TYPESIZE     Get size of an element of given type.
C     DSA_UNFLAG_DATA  Remove flagged data from an array.
C     DSA_WRNAME       Output the name of a data object to the user.
C     DSA_WRUSER       Output a string to the user.
C     DSA_WRFLUSH      Flush output message buffer to user.
C     DSA__STRUCT_ARRAY  Classify the stype of a structured array.
C     DSA__ARRAY_ORIGIN  Get origin values (if any) for a structured array.
C
C  History:
C     25th June 1987    Original version.  KS / AAO.
C     3rd  Feb  1988    Double<->float conversion error messages
C                       suppressed.  KS / AAO.
C     19th July 1988    COPY and UNFLAG parameters added.  Also now
C                       uses CNV_FMTCON and not DSA_FMTCON.  KS / AAO.
C     30th Aug  1988    Data conversion now omitted for writonly
C                       mapping.  KS / AAO.
C     25th Apr  1989    Support for USHORT type added.  KS / AAO.
C     8th  Sep  1989    PROP parameter added.  KS / AAO.
C     27th Apr  1990    Support for some SGP38 structured types added.
C                       MAP_STRUCT changed to MAP_SCALED. KS/AAO.
C     3rd  May  1990    Now sets the number of flagged values in common
C                       if DSA_UNFLAG_DATA is called.  KS/AAO.
C     21st Aug  1992    Replace CNV_ with DSA_FMTCON, which calls VEC_
C                       routines.  HME / UoE, Starlink.
C     31st Aug  1992    Use of DSA_WRFLUSH introduced. KS/AAO
C      1st Sep  1992    Usused variable declarations removed. KS/AAO.
C      4th Dec  1992    HME / UoE, Starlink.  The decision on whether to
C                       use OBJ_TYPE or DATA_TYPE for MAP_TYPE() was
C                       the wrong way round. Essentially MAP_TYPE() is
C                       always OBJ_TYPE.
C     16th Dec  1992    KS / AAO. To avoid UNIX implementation overheads,
C                       now map for update only if necessary.
C     24th Feb  1993    KS / AAO. Call to DSA_FMTCON now passes arrays instead
C                       of pointers to arrays.
C     27th May  1993    KS / AAO. Add test for a mapping for update or write
C                       of an array previously mapped readonly.
C      1st June 1993    KS / AAO. Modify previous fix to allow concurrent read
C                       and update mappings.
C     25th Oct  1994.   KS / AAO. Calling sequence now includes START and NMAP
C                       to support mapping of array subsets.
C      6th Feb  1995.   KS / AAO. Now uses new calling sequence for
C                       DSA_UNFLAG_DATA.
C     2005 May 31       Use CNF_PVAL for pointers to mapped data. MJC
C     2005 June 3       Replace DYNAMIC_MEMORY with
C                       %VAL(CNF_PVAL(ADDRESS)) contruct for 64-bit
C                       addressing.  MJC / Starlink
C
C  Note:
C     This version can handle the following types of SGP38 structured
C     arrays: SIMPLE, COMPLEX (when it operates only on the .REAL array)
C     SCALED.  It cannot handle arrays with origins other than 1.0.  It
C     can also handle the original Figaro scaled type 'CSTRUCT' (which
C     DSA__STRUCT_ARRAY treats as a 'SCALED' array)
C+
      SUBROUTINE DSA_MAP_ARRAY (NAME,MODE,TYPE,START,NMAP,NELM,
     :                         COPY,UNFLAG,PROP,ADDRESS,SLOT,STATUS)
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters
C
      LOGICAL COPY, UNFLAG, PROP
      INTEGER START, NMAP, NELM, ADDRESS, SLOT, STATUS
      CHARACTER*(*) NAME, MODE, TYPE
C
C     Functions used
C
      CHARACTER*10 ICH_CI
      INTEGER DSA_TYPESIZE, ICH_FOLD, ICH_LEN
C
C     DSA_ system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      CHARACTER ACCESS*1                 ! Access type U/W/R for main array
      CHARACTER ARRAY_NAME*64            ! DTA_ name of actual data array
      INTEGER   BYTES                    ! Number of workspace bytes needed
      CHARACTER CHR*1                    ! Temporary, used for start of MODE
      INTEGER   DTA_STATUS               ! Status returned by DTA_ routines
      CHARACTER DATA_TYPE*16             ! Type of named data object
      CHARACTER ERROR*64                 ! DTA_ system error text
      LOGICAL   FOUND                    ! Indicates item found in common
      INTEGER   FREE_SLOT                ! Map slot available for use
      INTEGER   I                        ! Loop index
      INTEGER   IEND                     ! Last element to be mapped
      INTEGER   IGNORE                   ! Used for status we don't care about
      INTEGER   IMEND                    ! Last element actually mapped
      INTEGER   IMST                     ! First element actually mapped
      INTEGER   INVOKE                   ! Dummy function argument
      INTEGER   IPT                      ! Start of element spec in name
      LOGICAL   KNOWN                    ! true if we can handle this structure
      INTEGER   LENAME                   ! Number of characters in ARRAY_NAME
      INTEGER   LENGTH                   ! Number of characters in NAME
      INTEGER   MAP_SLOT                 ! Map slot for array mapping
      INTEGER   MSTART                   ! Actual first mapped element
      INTEGER   NBAD                     ! Number of conversion errors
      INTEGER   NDIM                     ! Number of ORIGIN values
      INTEGER   NMAPPED                  ! Number of elements actually mapped
      CHARACTER NUMBER*10                ! Work string to format numbers
      CHARACTER OBJ_TYPE*16              ! Type of data array in structure
      INTEGER   OBJ_TYPE_CODE            ! Integer code for data array type
      INTEGER   ORIGIN(MAX_AXES)         ! Origin values for structured array
      INTEGER   POINTER                  ! Memory address of mapped array
      LOGICAL   SCALED                   ! True if data is a scaled array
      LOGICAL   STRUCT                   ! True if data object is a structure
      INTEGER   TYPE_CODE                ! Integer code for requested type
      LOGICAL   TYPE_OK                  ! Indicates requested type is valid
      CHARACTER VARIANT*16               ! Structured array variant
      INTEGER   WORK_LAST                ! Last work slot in chain
      INTEGER   WORK_PTR                 ! Memory address of work array
      INTEGER   WORK_SLOT                ! Slot number of work area
C
C     DSA_ type definitions.  Defines MAX_TYPES, xxxx_TYPE,
C                             TYPE_NAMES, TYPE_SIZE, FMTCON_CODE
C
      INCLUDE 'DSA_TYPES'
C
C     If bad status passed, return now
C
      IF (STATUS.NE.0) RETURN
C
C     Make sure the limits specified are valid - they ought to be, since
C     this is an internal routine of DSA, so this is really an internal
C     consistency check.
C
      IF ((START.LT.1).OR.(NMAP+START-1.GT.NELM)) THEN
         CALL DSA_WRUSER('Map call for ')
         CALL DSA_WRNAME(NAME)
         CALL DSA_WRUSER(' specifies start of ')
         NUMBER=ICH_CI(START)
         CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER(' and length of ')
         NUMBER=ICH_CI(NMAP)
         CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER(' elements for an array of ')
         NUMBER=ICH_CI(NELM)
         CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER(' elements.')
         CALL DSA_WRFLUSH
         STATUS=DSA__INVRANG
         GO TO 500
      END IF
C
C     See if we are mapping a subset of the array. If this is the case, we
C     see if there are any other unused subset mappings of the same array.
C     If we're seeing a lot of mapping of the same array by subsets, then
C     we don't want to keep unused mapped subsets hanging around.
C
      IF (NMAP.LT.NELM) THEN
         DO I=1,MAX_MAPS
            IF (MAP_USED(I)) THEN
               IF (MAP_NAMES(I).EQ.NAME) THEN
                  IF ((MAP_SIZE(I).NE.NELM).AND.
     :                             (MAP_COUNT(MAP_SLOT).GT.0)) THEN
                     CALL DTA_FRVAR(MAP_ACTNAM(MAP_SLOT),DTA_STATUS)
                     IF (DTA_STATUS.NE.0) THEN
                        CALL DSA_WRUSER('Error unmapping the array ')
                        CALL DSA_WRNAME(MAP_ACTNAM(MAP_SLOT))
                        CALL DSA_WRUSER('. ')
                        CALL DTA_ERROR(DTA_STATUS,ERROR)
                        CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
                        CALL DSA_WRUSER('.')
                        CALL DSA_WRFLUSH
                        STATUS=DSA__DTAERR
                        DTA_CODE=DTA_STATUS
                        GO TO 500
                     END IF
                  END IF
               END IF
            END IF
         END DO
      END IF
C
C     Get a spare slot to record the mapping.
C
      SLOT=0
      DO I=1,MAX_MAP_CALLS
         IF (.NOT.MAP_CALL_USED(I)) THEN
            SLOT=I
            GO TO 310       ! Break out of I loop
         END IF
      END DO
  310 CONTINUE
      IF (SLOT.EQ.0) THEN
         CALL DSA_WRUSER('Unable to find a free map call slot for ')
         CALL DSA_WRNAME(NAME)
         CALL DSA_WRUSER('. Too many items mapped at once.')
         CALL DSA_WRFLUSH
         STATUS=DSA__NOMPSL
         GO TO 500
      END IF
C
C     Initialise the call slot values
C
      MAP_CALL_WORK(SLOT)=0
      MAP_CALL_SLOT(SLOT)=0
      MAP_CALL_VSLOT(SLOT)=0
      MAP_CALL_NFLAG(SLOT)=0
      CHR=MODE(1:1)
      INVOKE=ICH_FOLD(CHR)
      MAP_CALL_MODE(SLOT)=CHR
C
C     See if this array has been mapped already.  Since we allow subset
C     mapping, we have to be careful here. We treat an already mapped
C     array that doesn't overlap as a completely separate mapping. We
C     do have to check that we're not seeing overlapping mappings, however.
C     We treat overlapping subset mappings as an error - although we could
C     probably survive overlapping readonly mappings, it sounds as
C     if they should be avoided. At the same time, find a spare map slot
C     in case we turn out to need one. We regard ourselves as having found
C     a mapping we can use if we find a mapping of the same array that is
C     a superset of the required mapping (or exactly the same mapping).
C
      FOUND=.FALSE.
      FREE_SLOT=0
      DO I=1,MAX_MAPS
         IF (MAP_USED(I)) THEN
            IF (MAP_NAMES(I).EQ.NAME) THEN
               IMST=MAP_START(I)
               IMEND=MAP_SIZE(I)+IMST-1
               IEND=START+NMAP-1
               IF (((START.LT.IMST).AND.(IEND.GT.IMST)).OR.
     :               ((START.LT.IMEND).AND.(IEND.GT.IMEND))) THEN
                  CALL DSA_WRUSER('Cannot support concurrent mappings ')
                  CALL DSA_WRUSER(
     :                           'of overlapping subsets of the array ')
                  CALL DSA_WRNAME(NAME)
                  CALL DSA_WRUSER('. ')
                  CALL DSA_WRFLUSH
                  STATUS=DSA__CONMAP
                  GO TO 500
               ELSE IF ((START.GE.IMST).AND.(IEND.LE.IMEND)) THEN
                  FOUND=.TRUE.
                  MAP_SLOT=I
                  IF (FREE_SLOT.NE.0) GO TO 320    ! Break out of loop
               END IF
            END IF
         ELSE
            FREE_SLOT=I
            IF (FOUND) GO TO 320        ! Break out of loop
         END IF
      END DO
  320 CONTINUE
C
C     If it has been mapped before, check that the mapping was compatible.
C
      IF (FOUND) THEN
C
C        Array is already mapped. If it was mapped readonly and we are now
C        trying to map it for write, this will be a problem. If the array
C        is still mapped (ie no call to DSA_UNMAP has been made) then we
C        have to use a new mapping. (This won't be a problem, since we can
C        reasonably have an existing mapping for read and a new mapping
C        for write. If the array has actually been freed but not physically
C        unmapped - DSA_UNMAP usually leaves arrays actually
C        mapped - then we can free it.
C
         IF ((CHR.NE.'R').AND.(MAP_MODE(MAP_SLOT).EQ.'R')) THEN
            IF (MAP_COUNT(MAP_SLOT).GT.0) THEN
               FOUND=.FALSE.
            ELSE
C
C              The array is no longer actively mapped by the DSA routines.
C              Free it and treat it as if we never found it.
C
               CALL DTA_FRVAR(MAP_ACTNAM(MAP_SLOT),DTA_STATUS)
               IF (DTA_STATUS.NE.0) THEN
                  CALL DSA_WRUSER('Error unmapping the array ')
                  CALL DSA_WRNAME(MAP_ACTNAM(MAP_SLOT))
                  CALL DSA_WRUSER('. ')
                  CALL DTA_ERROR(DTA_STATUS,ERROR)
                  CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
                  CALL DSA_WRUSER('.')
                  CALL DSA_WRFLUSH
                  STATUS=DSA__DTAERR
                  DTA_CODE=DTA_STATUS
                  GO TO 500
               END IF
               FOUND=.FALSE.
               FREE_SLOT=MAP_SLOT
            END IF
         END IF
      END IF
C
C     If the array isn't currently mapped, we need to map it. Normally, we
C     will map the array just as requested, from element START to element
C     START+NMAP-1. One exception is a scaled array that is being mapped for
C     write or update - we can't scale on the basis of a subset, so in that
C     case we have to map the whole array. We set MSTART and NMAPPED to the
C     start element and number of elements we actually map.
C
      SCALED=.FALSE.
      IF (.NOT.FOUND) THEN
         MSTART=START
         NMAPPED=NMAP
         IF (FREE_SLOT.EQ.0) THEN
            CALL DSA_WRUSER('Unable to find a free map slot for ')
            CALL DSA_WRNAME(NAME)
            CALL DSA_WRUSER('. Too many items mapped at once.')
            CALL DSA_WRFLUSH
            STATUS=DSA__NOMPSL
            GO TO 500
         END IF
         MAP_SLOT=FREE_SLOT
         CALL DTA_TYVAR(NAME,OBJ_TYPE,DTA_STATUS)
         CALL DTA_STRUC(NAME,STRUCT,DTA_STATUS)
         DATA_TYPE=OBJ_TYPE
         IF (DTA_STATUS.EQ.0) THEN
            IF (STRUCT) THEN
C
C              If its a structured array, DSA__STRUCT_ARRAY can classify
C              it (mainly by returning its variant as 'SCALED', 'SIMPLE'
C              - the only ones we can handle so far).  The later code
C              in this routine assumes that scaled arrays are based on
C              SHORT arrays, so even if this isn't the case we force it
C              (the map then may not strictly be to its primitive type,
C              but that's too bad - it's crazy to have any other sort of
C              scaled array - isn't it?)  We also check for origin values
C              that we can't handle.
C
               LENGTH=ICH_LEN(NAME)
               CALL DSA__STRUCT_ARRAY (NAME,LENGTH,OBJ_TYPE,
     :                         VARIANT,ARRAY_NAME,LENAME,KNOWN,STATUS)
               IF (KNOWN) THEN
                  CALL DSA__ARRAY_ORIGIN (NAME,LENGTH,OBJ_TYPE,MAX_AXES,
     :                                               ORIGIN,NDIM,STATUS)
                  DO I=1,NDIM
                     IF (ORIGIN(I).NE.1.0) THEN
                        CALL DSA_WRUSER('Cannot map ')
                        CALL DSA_WRNAME(NAME)
                        CALL DSA_WRUSER(
     :                    ', because this implementation of the data '
     :                    //'access routines cannot handle an array ')
                        CALL DSA_WRUSER(
     :                    'that has pixel origins other than 1.')
                        CALL DSA_WRFLUSH
                        STATUS=DSA__INVORI
                        GO TO 500
                     END IF
                  END DO
                  CALL DTA_TYVAR(ARRAY_NAME,OBJ_TYPE,DTA_STATUS)
                  IF (VARIANT.EQ.'SCALED') THEN
                     IF (CHR.NE.'R') THEN
                        MSTART=1
                        NMAPPED=NELM
                     END IF
                     OBJ_TYPE='SHORT'
                     SCALED=.TRUE.
                  END IF
               ELSE
                  CALL DSA_WRUSER('Cannot map ')
                  CALL DSA_WRNAME(NAME)
                  CALL DSA_WRUSER(', because it is of a type (')
                  CALL DSA_WRUSER(OBJ_TYPE(:ICH_LEN(OBJ_TYPE)))
                  CALL DSA_WRUSER(
     :                        ') which this system cannot handle.')
                  CALL DSA_WRFLUSH
                  STATUS=DSA__INVTYP
                  GO TO 500
               END IF
            ELSE
               ARRAY_NAME=NAME
            END IF
         END IF
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_WRUSER('Unable to access ')
            CALL DSA_WRNAME(NAME)
            CALL DSA_WRUSER(' for mapping. ')
            CALL DTA_ERROR(DTA_STATUS,ERROR)
            CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
            CALL DSA_WRUSER('.')
            CALL DSA_WRFLUSH
            STATUS=DSA__DTAERR
            DTA_CODE=DTA_STATUS
            GO TO 500
         END IF
C
C        At this point we should have the name of the actual array to
C        be mapped in ARRAY_NAME, and its type in OBJ_TYPE.  Note that
C        we map the array in its natural type, and try to map it for
C        readonly or update/write as requested, attempting a readonly map
C        if the update mode fails.  SCALED indicates that this is a scaled
C        array, and DATA_TYPE is the type of the named data object, which
C        will be the same as OBJ_TYPE if it is not a structured array.
C
C        If we are mapping a subset, then we need to append the start element
C        number to ARRAY_NAME.
C
         IF (MSTART.NE.1) THEN
            IPT=ICH_LEN(ARRAY_NAME)+1
            CALL DSA_ENCDIM(ARRAY_NAME,1,MSTART,IPT)
         END IF
         ACCESS='U'
         DTA_STATUS=0
         IF (OBJ_TYPE.EQ.'FLOAT') THEN
            OBJ_TYPE_CODE=FLOAT_TYPE
            IF (CHR.NE.'R') THEN
               CALL DTA_MUVARF(ARRAY_NAME,NMAPPED,POINTER,DTA_STATUS)
            END IF
            IF ((CHR.EQ.'R').OR.(DTA_STATUS.NE.0)) THEN
               ACCESS='R'
               CALL DTA_MRVARF(ARRAY_NAME,NMAPPED,POINTER,DTA_STATUS)
            END IF
         ELSE IF (OBJ_TYPE.EQ.'INT') THEN
            OBJ_TYPE_CODE=INT_TYPE
            IF (CHR.NE.'R') THEN
               CALL DTA_MUVARI(ARRAY_NAME,NMAPPED,POINTER,DTA_STATUS)
            END IF
            IF ((CHR.EQ.'R').OR.(DTA_STATUS.NE.0)) THEN
               ACCESS='R'
               CALL DTA_MRVARI(ARRAY_NAME,NMAPPED,POINTER,DTA_STATUS)
            END IF
         ELSE IF (OBJ_TYPE.EQ.'DOUBLE') THEN
            OBJ_TYPE_CODE=DOUBLE_TYPE
            IF (CHR.NE.'R') THEN
               CALL DTA_MUVARD(ARRAY_NAME,NMAPPED,POINTER,DTA_STATUS)
            END IF
            IF ((CHR.EQ.'R').OR.(DTA_STATUS.NE.0)) THEN
               ACCESS='R'
               CALL DTA_MRVARD(ARRAY_NAME,NMAPPED,POINTER,DTA_STATUS)
            END IF
         ELSE IF (OBJ_TYPE.EQ.'SHORT') THEN
            OBJ_TYPE_CODE=SHORT_TYPE
            IF (CHR.NE.'R') THEN
               CALL DTA_MUVARS(ARRAY_NAME,NMAPPED,POINTER,DTA_STATUS)
            END IF
            IF ((CHR.EQ.'R').OR.(DTA_STATUS.NE.0)) THEN
               ACCESS='R'
               CALL DTA_MRVARS(ARRAY_NAME,NMAPPED,POINTER,DTA_STATUS)
            END IF
         ELSE IF (OBJ_TYPE.EQ.'USHORT') THEN
            OBJ_TYPE_CODE=USHORT_TYPE
            IF (CHR.NE.'R') THEN
               CALL DTA_MUVARU(ARRAY_NAME,NMAPPED,POINTER,DTA_STATUS)
            END IF
            IF ((CHR.EQ.'R').OR.(DTA_STATUS.NE.0)) THEN
               ACCESS='R'
               CALL DTA_MRVARU(ARRAY_NAME,NMAPPED,POINTER,DTA_STATUS)
            END IF
         ELSE IF (OBJ_TYPE.EQ.'CHAR') THEN
            OBJ_TYPE_CODE=CHAR_TYPE
            IF (CHR.NE.'R') THEN
               CALL DTA_MUVARC(ARRAY_NAME,NMAPPED,POINTER,DTA_STATUS)
            END IF
            IF ((CHR.EQ.'R').OR.(DTA_STATUS.NE.0)) THEN
               ACCESS='R'
               CALL DTA_MRVARC(ARRAY_NAME,NMAPPED,POINTER,DTA_STATUS)
            END IF
         ELSE IF (OBJ_TYPE.EQ.'BYTE') THEN
            OBJ_TYPE_CODE=BYTE_TYPE
            IF (CHR.NE.'R') THEN
               CALL DTA_MUVARB(ARRAY_NAME,NMAPPED,POINTER,DTA_STATUS)
            END IF
            IF ((CHR.EQ.'R').OR.(DTA_STATUS.NE.0)) THEN
               ACCESS='R'
               CALL DTA_MRVARB(ARRAY_NAME,NMAPPED,POINTER,DTA_STATUS)
            END IF
         END IF
C
C        Error message if unable to map the array.
C
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_WRUSER('Unable to map the array ')
            CALL DSA_WRUSER(ARRAY_NAME(:ICH_LEN(ARRAY_NAME)))
            CALL DSA_WRUSER('. ')
            CALL DTA_ERROR(DTA_STATUS,ERROR)
            CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
            CALL DSA_WRUSER('.')
            CALL DSA_WRFLUSH
            STATUS=DSA__DTAERR
            DTA_CODE=DTA_STATUS
            GO TO 500
         END IF
C
C        Update the map table common entries
C
         WORK_SLOT=0
         MAP_USED(MAP_SLOT)=.TRUE.
         MAP_NAMES(MAP_SLOT)=NAME
         MAP_ACTNAM(MAP_SLOT)=ARRAY_NAME
         MAP_COUNT(MAP_SLOT)=1
         MAP_POINTER(MAP_SLOT)=POINTER
         MAP_WORK(MAP_SLOT)=WORK_SLOT
         MAP_MODE(MAP_SLOT)=ACCESS
         MAP_CODE(MAP_SLOT)=OBJ_TYPE_CODE
         MAP_SCALED(MAP_SLOT)=SCALED
         MAP_SIZE(MAP_SLOT)=NMAPPED
         MAP_START(MAP_SLOT)=MSTART
C
C        The common block stores what the type of the primitive array
C        is. If the names object was a stucture, we must use OBJ_TYPE.
C
         IF (.NOT.STRUCT) THEN
            MAP_TYPE(MAP_SLOT)=DATA_TYPE
         ELSE
            MAP_TYPE(MAP_SLOT)=OBJ_TYPE
         END IF
      ELSE
C
C        If the object is already mapped, pick up its access mode
C        and type and other things up from the common tables.
C
         ACCESS=MAP_MODE(MAP_SLOT)
         POINTER=MAP_POINTER(MAP_SLOT)
         OBJ_TYPE_CODE=MAP_CODE(MAP_SLOT)
         OBJ_TYPE=TYPE_NAMES(OBJ_TYPE_CODE)
         SCALED=MAP_SCALED(MAP_SLOT)
         DATA_TYPE=MAP_TYPE(MAP_SLOT)
         WORK_SLOT=MAP_WORK(MAP_SLOT)
         MAP_COUNT(MAP_SLOT)=MAP_COUNT(MAP_SLOT)+1
         MSTART=MAP_START(MAP_SLOT)
         NMAPPED=MAP_SIZE(MAP_SLOT)
      END IF
      MAP_CALL_SLOT(SLOT)=MAP_SLOT
C
C     We now have a slot with the data object mapped, either just now
C     or from a previous call.   See if we can give the caller the
C     access requested.  The only one we can't manage is to give write
C     or update access if the file only allowed read access.  However,
C     we do make sure the access requested was a valid choice.  Note
C     that this version doesn't in fact write-protect data mapped as
C     read-only.
C
      IF (INDEX('RWU',CHR).EQ.0) THEN
         CALL DSA_WRUSER('Invalid access mode (')
         CALL DSA_WRUSER(MODE(:ICH_LEN(MODE)))
         CALL DSA_WRUSER(') specified for the array ')
         CALL DSA_WRUSER(ARRAY_NAME(:ICH_LEN(ARRAY_NAME)))
         CALL DSA_WRUSER('.')
         CALL DSA_WRFLUSH
         STATUS=DSA__ACCINV
         GO TO 500
      END IF
      INVOKE=ICH_FOLD(CHR)
      IF ((ACCESS.EQ.'R').AND.(CHR.NE.'R')) THEN
         CALL DSA_WRUSER('Unable to provide write/update access to ')
         CALL DSA_WRUSER(ARRAY_NAME(:ICH_LEN(ARRAY_NAME)))
         CALL DSA_WRUSER('. This array, or its file, is protected.')
         CALL DSA_WRFLUSH
         STATUS=DSA__PROVIO
         GO TO 500
      END IF
C
C     See if the mapping was of the correct type, or if we are going
C     to have to do a type conversion into a workspace array.  If the
C     data is a scaled array, then we will always do type conversion.
C     There are some cases where we want to force a copy of the data.  If
C     we are going to unflag the data, we use a work array for that - we
C     could do it into the array directly mapped to the file, but if
C     we do and the program crashes it will leave the file unflagged.
C
      IF (UNFLAG.OR.SCALED.OR.(OBJ_TYPE.NE.TYPE).OR.COPY) THEN
         TYPE_OK=.FALSE.
         DO I=1,MAX_TYPES
            IF (TYPE.EQ.TYPE_NAMES(I)) THEN
               TYPE_CODE=I
               TYPE_OK=.TRUE.
               GO TO 360            ! Break out of I loop
            END IF
         END DO
  360    CONTINUE
         IF (.NOT.TYPE_OK) THEN
            CALL DSA_WRUSER('Unable to map the array ')
            CALL DSA_WRUSER(ARRAY_NAME(:ICH_LEN(ARRAY_NAME)))
            CALL DSA_WRUSER(' as an array of type ')
            CALL DSA_WRUSER(TYPE(:ICH_LEN(TYPE)))
            CALL DSA_WRUSER('.  Type is not valid.')
            CALL DSA_WRFLUSH
            STATUS=DSA__INVTYP
            GO TO 500
         END IF
C
C        OK, so the natural mapping of the object was not the one
C        we wanted.  Look to see if it has been mapped with this
C        type in any of the workspace slots.  Note that any workspace
C        arrays will be writeable.
C
         FOUND=.FALSE.
         WORK_LAST=WORK_SLOT
         DO WHILE ((WORK_SLOT.NE.0).AND.(.NOT.FOUND))
            IF (WORK_TYPE(WORK_SLOT).EQ.TYPE) THEN
               FOUND=.TRUE.
               WORK_PTR=WORK_POINTER(WORK_SLOT)
            ELSE
               WORK_LAST=WORK_SLOT
               WORK_SLOT=WORK_LINK(WORK_LAST)
            END IF
         END DO
         IF (.NOT.FOUND) THEN
C
C           No, we don't have it in workspace either, so we will have to
C           grab workspace and then convert the data.  First get the
C           workspace and link it into the chain for the array.
C
            BYTES=NMAPPED*TYPE_SIZE(TYPE_CODE)
            CALL DSA_GET_WORKSPACE (BYTES,WORK_PTR,WORK_SLOT,STATUS)
            IF (STATUS.NE.0) GO TO 500
            IF (WORK_LAST.EQ.0) THEN
               MAP_WORK(MAP_SLOT)=WORK_SLOT
            ELSE
               WORK_LINK(WORK_LAST)=WORK_SLOT
            END IF
            WORK_TYPE(WORK_SLOT)=TYPE
C
C           Now we can do the conversion.  The number of elements to be
C           converted is NMAPPED, the type codes for the input and output
C           arrays are OBJ_TYPE_CODE and TYPE_CODE, the input and output
C           arrays start at addresses POINTER and WORK_PTR.  If the
C           mapping was writeonly, we can omit the conversion.
C
            IF (CHR.NE.'W') THEN
               IF ((OBJ_TYPE_CODE.EQ.CHAR_TYPE).OR.
     :                               (TYPE_CODE.EQ.CHAR_TYPE)) THEN
                  CALL DSA_WRUSER('The character<->numeric conversion')
                  CALL DSA_WRUSER(' required to convert ')
                  CALL DSA_WRNAME(ARRAY_NAME)
                  CALL DSA_WRUSER(' from ')
                  CALL DSA_WRUSER(OBJ_TYPE(:ICH_LEN(OBJ_TYPE)))
                  CALL DSA_WRUSER(' to ')
                  CALL DSA_WRUSER(TYPE(:ICH_LEN(TYPE)))
                  CALL DSA_WRUSER(' is not supported.')
                  CALL DSA_WRFLUSH
                  STATUS=DSA__CHRCON
                  GO TO 500               ! Error exit
               END IF
C
C              For a primitive array, all that is required is a format
C              conversion.  We suppress error messages for real<->double
C              conversions, since they can only be for -0.0 values.  This
C              is a little unsatisfactory, but if, say, a new 'FLOAT' array
C              is created and mapped 'DOUBLE', this can happen and it looks
C              better if no error message occurs.  What we really need is
C              a DTA_ routine that knows if an array has been initialised.
C              (Actually, not converting for writeonly access may have
C              bypassed this problem.) This error supression has been removed
C              for the new version using VEC, so this comment may be purely
C              historical now.
C
               IF (.NOT.SCALED) THEN
                  WORK_PROP(WORK_SLOT)=PROP
                  CALL DSA_FMTCON(PROP,
     :               FMTCON_CODE(OBJ_TYPE_CODE),FMTCON_CODE(TYPE_CODE),
     :               %VAL(CNF_PVAL(POINTER)),
     :               %VAL(CNF_PVAL(WORK_PTR)),NMAPPED,NBAD)
                  IF (NBAD.NE.0) THEN
                     CALL DSA_WRUSER('Note: ')
                     NUMBER=ICH_CI(NBAD)
                     CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
                     CALL DSA_WRUSER(
     :                        ' numeric error(s) occurred converting ')
                     CALL DSA_WRNAME(ARRAY_NAME)
                     CALL DSA_WRUSER(' from ')
                     CALL DSA_WRUSER(OBJ_TYPE(:ICH_LEN(OBJ_TYPE)))
                     CALL DSA_WRUSER(' to ')
                     CALL DSA_WRUSER(TYPE(:ICH_LEN(TYPE)))
                     CALL DSA_WRUSER('.')
                     CALL DSA_WRFLUSH
                  END IF
               ELSE
C
C                 Now we deal with the possibility of a structure array.  Note
C                 that the only one we need to handle, 'SCALED', is always based
C                 on a short array.  Other structured types that we know about
C                 (basically, SIMPLE and COMPLEX) don't need any additional
C                 processing.  We can't handle sparse or spaced or polynomial
C                 arrays yet.
C
                  IF (SCALED) THEN
                     CALL DSA_CSTRUCT (NAME,NMAPPED,OBJ_TYPE_CODE,
     :                               POINTER,TYPE_CODE,WORK_PTR,STATUS)
                     IF (STATUS.NE.0) GO TO 500
                  END IF
               END IF
            END IF
C
         END IF
C
C        At this point, we know that we have the data, mapped as the
C        correct type, in a workspace array starting at address WORK_PTR,
C        either because it was there all along, or because we have just
C        created it.
C
         MAP_CALL_WORK(SLOT)=WORK_SLOT
         POINTER=WORK_PTR
      END IF
C
C     So, finally, we have POINTER giving the actual memory address of an
C     invocation of the data array of the requested type.  If requested,
C     we now do the unflag processing.
C
      IF (UNFLAG) THEN
         CALL DSA_GET_WORK_ARRAY(NMAPPED,'BYTE',WORK_PTR,WORK_SLOT,
     :                                                          STATUS)
         MAP_CALL_FSLOT(SLOT)=WORK_SLOT
         CALL DSA_ZFILL_ARRAY (NMAPPED,WORK_PTR,'BYTE',STATUS)
         CALL DSA_UNFLAG_DATA(NMAPPED,.FALSE.,TYPE,
     :                        %VAL( CNF_PVAL(POINTER) ),
     :                        %VAL( CNF_PVAL(WORK_PTR) ),
     :                        MAP_CALL_NFLAG(SLOT),STATUS)
         IF (STATUS.NE.0) GO TO 500     ! Error exit
      ELSE
         MAP_CALL_FSLOT(SLOT)=0
      END IF
C
C     Now that everything is OK, we flag the map call table slot as in use.
C     The address we actually return is not necessarily POINTER, which is the
C     address of the start of the mapped array - we have to allow for
C     the possibility that START and MSTART (the start of what we actually
C     mapped) are not the same.
C
      IF (START.EQ.MSTART) THEN
         ADDRESS=POINTER
      ELSE
         IGNORE=0
         ADDRESS=POINTER+(START-MSTART)*DSA_TYPESIZE(TYPE,IGNORE)
      END IF
      MAP_CALL_USED(SLOT)=.TRUE.
C
C     Exit
C
  500 CONTINUE
C
      END
