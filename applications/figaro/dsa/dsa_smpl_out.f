C+
C                    D S A _ S I M P L E _ O U T P U T
C
C  Routine name:
C     DSA_SIMPLE_OUTPUT
C
C  Function:
C     Creates a minimal output structure.
C
C  Description:
C     This routine builds a minimal output structure.  It is passed the
C     reference name of what should be an empty structure - that is one
C     created by DSA_OUTPUT (or equivalent), with no basis structure
C     specified, and creates in that a specified set of the basic items
C     one finds in a data structure such as a main data array, an error
C     array, axis arrays, etc.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SIMPLE_OUTPUT (REF_NAME,ITEM_LIST,TYPE,NDIM,DIMS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME       (Fixed string,descr) The reference name
C                        associated with the outut structure.
C     (>) ITEM_LIST      (Fixed string,descr) A list of the items to
C                        be created.  The format is described in detail
C                        below.
C     (>) TYPE           (Fixed string,descr) The default type for the
C                        data arrays.  This can be overidden in the
C                        item list.
C     (>) NDIM           (Integer,ref) Number of dimensions for the data
C                        arrays. This can be overidden in the item list.
C     (>) DIMS           (Integer array,ref) Dimensions for the data
C                        arrays. These can be overidden in the item list.
C     (!) STATUS         (Integer,ref) Status value.  If bad status is
C                        passed to it, this routine returns immediately.
C
C  Item list format:
C     The item list is a series of names of items to be created,
C     separated by commas.  The names accepted at present are `DATA' -
C     the main data array; `AXIS' - one axis data array for each
C     dimension of the main data array; `AXISn' - one specific axis
C     data array, n being a number from 1 up; `QUALITY' - a data
C     quality array; `ERRORS' - an errors array; `VARIANCE' - a
C     variance array.  The names may be abbreviated, and strings such
C     as `A1', `AX3' are accepted as abbreviations for `AXIS1',
C     `AXIS3'.  So, for example, the string `D,A,Q,E' indicates that
C     the main data array, all axis arrays, a data quality array and an
C     error array are to be created.  By default, all arrays are
C     created with the type specified in TYPE, except `QUALITY' which
C     is always created as `BYTE' unless explicitly overidden.  By
C     default, the data, quality, and error arrays are created with the
C     dimensions specified in NDIM and DIMS, while axis arrays are one
C     dimensional, with the length of the corresponding dimension of
C     the data array.  These can be overidden for individual arrays by
C     following the name with explicit dimensions, eg `D,A1[1024,2]',
C     which specifies the default dimensions for the data array, but a
C     first axis array of 1024 by 2.  The type can be overidden by
C     following the name (and dimensions, if specified) by a type in
C     parentheses, eg `D,A1[1024,2](DOUBLE)' which indicates that the
C     first axis array is to be double precision.  Note that you cannot
C     specify both `ERRORS' and `VARIANCE'.
C
C  External variables used:
C     Only common variables internal to the DSA package
C
C  External subroutines / functions used:
C
C     DTA_CRVAR, ICH_DELIM, ICH_FOLD, ICH_KEY, ICH_LEN, ICH_VERIF,
C     DSA_CREATE_ARRAY, DSA_DECODE_DIMS, DSA_REF_SLOT, DSA_WRUSER,
C     DSA__CREATE_DATA_ENV, DSA__CREATE_AXIS, DSA__AXIS_DATA_NAME,
C     DSA__DATA_NAME, DSA__ERROR_NAME, DSA__VARIANCE_NAME,
C     DSA__QUAL_NAME
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.  The
C     structure to be built must have been opened by a routine such
C     as DSA_OUTPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 28th November 1995
C-
C  Subroutine / function details:
C     DTA_CRVAR          Create a data structure object
C     ICH_DELIM          Find next occurrence of one of a set of delimiters
C     ICH_FOLD           Convert string to upper case
C     ICH_KEY            See if a word is one of a set of keywords
C     ICH_VERIF          Next character not in a specified set of characters
C     DSA_CREATE_ARRAY   Create a data array
C     DSA_DECODE_DIMS    Decode a character string dimension specification
C     DSA_REF_SLOT       Look up a reference name in common tables
C     DSA_WRUSER         Output message to user
C     DSA__CREATE_DATA_ENV  Ensure upper structures for data arrays exist
C     DSA__CREATE_AXIS      Create a specified axis structure
C     DSA__AXIS_DATA_NAME   Get name of data object used for axis data array
C     DSA__DATA_NAME     Get name of data object used for data array
C     DSA__ERROR_NAME    Get name of data object used for error array
C     DSA__VARIANCE_NAME Get name of data object used for variance array
C     DSA__QUAL_NAME     Get name of data object used for quality array
C
C  Common variable details:
C     (>) MAX_AXES       (Integer parameter) Maximum number of axes allowed
C
C  History:
C     5th July 1988  Original version.  KS / AAO.
C     25th July 1988 Default type for QUALITY made BYTE.  KS / AAO.
C     1st Sept 1988  Default axis dimensions now 1D, as intended.  KS/AAO.
C     16th Jan 1990  Modified to use DSA__ routines to handle structure
C                    details, rather than just assuming the original
C                    Figaro data format.  `VARIANCE' also allowed. KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992  Remove unused variable declarations. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     28th Nov 1995  Now sets SHAPE_CHECK and DATA_RESHAPE flags. KS/AAO.
C     25th Jul 1996  Catenation for Linux.  MJCL/Starlink, UCL.
C+
      SUBROUTINE DSA_SIMPLE_OUTPUT (REF_NAME,ITEM_LIST,TYPE,NDIM,
     :                                                    DIMS,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NDIM, DIMS(*), STATUS
      CHARACTER*(*) REF_NAME, ITEM_LIST, TYPE
C
C     Functions used
C
      INTEGER ICH_DELIM, ICH_FOLD, ICH_KEY, ICH_LEN, ICH_VERIF
C
C     DSA error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     DSA common definition - supplies MAX_AXES
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   AXIS                ! Axis number
      INTEGER   DTA_STATUS          ! Status from DTA_ calls
      CHARACTER END_CH*1            ! Last character in item list name
      INTEGER   ERR_CODE            ! Type of error information used
      INTEGER   ERRCNT              ! Number of error or variance arrays
      INTEGER   I                   ! Loop index
      INTEGER   IAX                 ! Index through axes
      INTEGER   INVOKE              ! Dummy function value
      INTEGER   IST                 ! Pointer to chars in ITEM_LIST
      INTEGER   KEY                 ! Key value for item list name
      INTEGER   LENGTH              ! Significant characters in OBJ_NAME
      CHARACTER LIM_CH*1            ! Maximum axis character
      INTEGER   LWORD               ! Length of item list name
      INTEGER   NEXT                ! Next char in word - ignored
      INTEGER   OBJDIM              ! Number of object dimensions
      INTEGER   OBJDIMS(MAX_AXES)   ! Object dimensions
      CHARACTER OBJ_NAME*80         ! DTA_ name of data items
      CHARACTER OBJTYPE*16          ! Individual data array type
      INTEGER   POSN                ! Position of delimiter in ITEM_LIST
      INTEGER   REF_SLOT            ! Common table entry for REF_NAME
      CHARACTER WORD*16             ! Item name from item list
      CHARACTER STRING*80           ! Local string storage
C
C     Symbolic names for item codes.  Note these must match the
C     order in the call to ICH_KEY
C
      INTEGER DATA_KEY, AXIS_KEY, ERR_KEY, QUAL_KEY, VAR_KEY
      PARAMETER (DATA_KEY=1, AXIS_KEY=2, ERR_KEY=3, QUAL_KEY=4,
     :           VAR_KEY=5)
C
C     If bad status passed, return immediately
C
      IF (STATUS.NE.0) RETURN
C
C     Look up reference name in common tables.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500       ! Error exit
C
C     We are always going to need an environment for the data arrays,
C     so create this now.
C
      CALL DSA__CREATE_DATA_ENV (REF_SLOT,DTA_STATUS)
C
C     Start to parse the item list
C
      ERRCNT=0
      LIM_CH=CHAR(MAX_AXES+ICHAR('0'))
      IST=1
      DO WHILE (IST.LE.LEN(ITEM_LIST))
         POSN=ICH_DELIM(ITEM_LIST,IST,', [(')
         IF (POSN.EQ.0) POSN=LEN(ITEM_LIST)+1
         LWORD=POSN-IST
         IF (LWORD.LE.0) THEN
            CALL DSA_WRUSER('The item specification "')
            CALL DSA_WRUSER(ITEM_LIST(:ICH_LEN(ITEM_LIST)))
            CALL DSA_WRUSER('" contains a null specification, '//
     :                                    'which has been ignored.')
            CALL DSA_WRFLUSH
            IST=POSN+1
         ELSE
C
C           At this point, we expect to have found an item name.
C
            WORD=ITEM_LIST(IST:POSN-1)
            INVOKE=ICH_FOLD(WORD)
            AXIS=0
            IF (LWORD.GT.1) THEN
               END_CH=WORD(LWORD:LWORD)
               IF ((END_CH.GE.'1').AND.(END_CH.LE.LIM_CH)) THEN
                  LWORD=LWORD-1
                  AXIS=ICHAR(END_CH)-ICHAR('0')
               END IF
            END IF
            KEY=ICH_KEY(WORD(:LWORD),1,' ',
     :              'DATA;AXIS;ERRORS;QUALITY;VARIANCE;','Abbr.',NEXT)
            IF (KEY.EQ.0) THEN
               CALL DSA_WRUSER('The item specification "'//WORD(:LWORD)
     :                                           //'" in the string "')
               CALL DSA_WRUSER(ITEM_LIST(:ICH_LEN(ITEM_LIST)))
               CALL DSA_WRUSER(
     :                '" does not match any of the recognised items:')
               CALL DSA_WRUSER(
     :                   ' "DATA, AXIS, ERRORS, QUALITY, VARIANCE"')
               CALL DSA_WRFLUSH
               STATUS=DSA__INVITM
               GO TO 500     ! Error exit
            END IF
            IF ((KEY.EQ.VAR_KEY).OR.(KEY.EQ.ERR_KEY)) THEN
               ERRCNT=ERRCNT+1
               IF (ERRCNT.GT.1) THEN
                  STRING='The string "'//ITEM_LIST(:ICH_LEN(ITEM_LIST))
                  CALL DSA_WRUSER(STRING)
                  CALL DSA_WRUSER(
     :                    '" specifies both error and variance data.')
                  CALL DSA_WRUSER(
     :               ' You cannot have both in the same structure.')
                  CALL DSA_WRFLUSH
                  STATUS=DSA__INVITM
                  GO TO 500     ! Error exit
               END IF
            END IF
C
C           Set default dimensions and type for the item
C
            DO I=1,NDIM
               OBJDIMS(I)=DIMS(I)
            END DO
            OBJDIM=NDIM
            IF (KEY.EQ.QUAL_KEY) THEN
               OBJTYPE='BYTE'
            ELSE
               OBJTYPE=TYPE
            END IF
            IF ((KEY.EQ.AXIS_KEY).AND.(AXIS.NE.0)) THEN
               OBJDIM=1
               OBJDIMS(1)=DIMS(AXIS)
            END IF
C
C           Now look to see if either a type or dimensions are specified
C
            IST=POSN
            IF (IST.LT.LEN(ITEM_LIST)) THEN
               IF (ITEM_LIST(IST:IST).EQ.'[') THEN
C
C                 The name seems to be followed by a dimension spec
C
                  IST=POSN
                  CALL DSA_DECODE_DIMS(ITEM_LIST,IST,MAX_AXES,POSN,
     :                                           OBJDIM,OBJDIMS,STATUS)
                  IF (STATUS.NE.0) THEN
                     CALL DSA_WRUSER('The item specification "')
                     CALL DSA_WRUSER(ITEM_LIST(:ICH_LEN(ITEM_LIST)))
                     CALL DSA_WRUSER('" contains an invalid '//
     :                                  'dimension specification for "')
                     CALL DSA_WRUSER(WORD(:LWORD))
                     CALL DSA_WRUSER('".')
                     CALL DSA_WRFLUSH
                     GO TO 500   ! Error exit
                  END IF
                  IST=POSN+1
               END IF
               IF (IST.LT.LEN(ITEM_LIST)) THEN
                  IF (ITEM_LIST(IST:IST).EQ.'(') THEN
                     POSN=ICH_DELIM(ITEM_LIST,IST,')')
                     IF (POSN.LE.0) THEN
                        CALL DSA_WRUSER('The item specification "')
                        CALL DSA_WRUSER(ITEM_LIST(:ICH_LEN(ITEM_LIST)))
                        CALL DSA_WRUSER('" contains an invalid '//
     :                                  'type specification for "')
                        CALL DSA_WRUSER(WORD(:LWORD))
                        CALL DSA_WRUSER('".')
                        CALL DSA_WRFLUSH
                        GO TO 500   ! Error exit
                     END IF
                     OBJTYPE=ITEM_LIST(IST+1:POSN-1)
                     IST=POSN+1
                  END IF
               END IF
               INVOKE=ICH_FOLD(OBJTYPE)
C
C              Make sure we're pointing to the start of the next item
C
               IF (IST.LE.LEN(ITEM_LIST)) THEN
                  IF (ITEM_LIST(IST:IST).EQ.',') THEN
                     IF (IST.LT.LEN(ITEM_LIST)) IST=IST+1
                  END IF
                  IST=ICH_VERIF(ITEM_LIST,IST,' ')
                  IF (IST.EQ.0) IST=LEN(ITEM_LIST)+1
               END IF
            END IF
C
C           Now we have a name for the item, the dimensions for it,
C           and the type for it.  So we create it.
C
            IF ((KEY.EQ.DATA_KEY).OR.(KEY.EQ.ERR_KEY).OR.
     :                 (KEY.EQ.QUAL_KEY).OR.(KEY.EQ.VAR_KEY)) THEN
C
C              One of the non-axis arrays.
C
               IF (KEY.EQ.DATA_KEY) THEN
                  CALL DSA__DATA_NAME (REF_SLOT,OBJ_NAME,LENGTH)
               ELSE IF (KEY.EQ.QUAL_KEY) THEN
                  CALL DSA__QUAL_NAME (REF_SLOT,OBJ_NAME,LENGTH)
               ELSE IF (KEY.EQ.ERR_KEY) THEN
                  CALL DSA__ERROR_NAME (REF_SLOT,OBJ_NAME,LENGTH,
     :                                                     ERR_CODE)
               ELSE IF (KEY.EQ.VAR_KEY) THEN
                  CALL DSA__VARIANCE_NAME (REF_SLOT,OBJ_NAME,LENGTH,
     :                                                     ERR_CODE)
               END IF
               CALL DSA_CREATE_ARRAY(' ',OBJ_NAME(:LENGTH),
     :                            OBJTYPE,OBJDIM,OBJDIMS,STATUS)
               IF (STATUS.NE.0) GO TO 500    ! Error exit
               IF (KEY.EQ.DATA_KEY) THEN
                  DATA_RESHAPE(REF_SLOT)=.TRUE.
                  SHAPE_CHECK(REF_SLOT)=.TRUE.
               END IF
C
            ELSE IF (KEY.EQ.AXIS_KEY) THEN
C
C              One (or all) of the axis arrays
C
               IF (AXIS.EQ.0) THEN
                  DO IAX=NDIM,1,-1
                     CALL DSA__CREATE_AXIS (REF_SLOT,IAX,DTA_STATUS)
                     CALL DSA__AXIS_DATA_NAME (REF_SLOT,IAX,OBJ_NAME,
     :                                                          LENGTH)
                     CALL DSA_CREATE_ARRAY(' ',OBJ_NAME(:LENGTH),
     :                                      OBJTYPE,1,DIMS(IAX),STATUS)
                  END DO
               ELSE
                  CALL DSA__CREATE_AXIS (REF_SLOT,AXIS,DTA_STATUS)
                  CALL DSA__AXIS_DATA_NAME (REF_SLOT,AXIS,OBJ_NAME,
     :                                                          LENGTH)
                  CALL DSA_CREATE_ARRAY(' ',OBJ_NAME(:LENGTH),
     :                                   OBJTYPE,OBJDIM,OBJDIMS,STATUS)
               END IF
            END IF
         END IF
      END DO
C
  500 CONTINUE
C
      END
