C+
C                       D S A _ _ F I T S _ D E T A I L S
C
C  Routine name:
C     DSA__FITS_DETAILS
C
C  Function:
C     Returns size and access details for a named FITS object.
C
C  Description:
C     This routine is a service routine for DSA_SEEK_FITS and similar
C     routines such as DSA_NTH_FITS_ITEM.  It takes the name or number
C     of an object in the FITS sub-structure and returns the type
C     (given as a single character indicating which DSA routine should
C     be used to access it) and size of the item.  If the item is not
C     present, this routine returns indicating that but does not treat
C     as an error and will not generate an error message.  If the number
C     for the item is non-zero, then this will be used, and information
C     about the nth item in the FITS substructure will be returned.  If
C     the number is zero, then the name will be used and this routine
C     will return information about the FITS item with that name.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__FITS_DETAILS (REF_SLOT,NTH,KEYWORD,EXIST,ACCESS,ELEMENTS,
C                                                           STRLEN,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT         (Integer,ref) The reference slot number for the
C                          structure in question.
C     (>) NTH              (Integer,ref) The number of the FITS item.  If
C                          passed as zero, then NAME is used instead.
C     (!) KEYWORD          (Fixed string,descr) The name - A FITS keyword -
C                          for the object in question.  If NTH is zero,
C                          then this is an input-only argument (>), and
C                          should be in upper case.  If NTH is non-zero,
C                          then this is an output-only argument (<).
C     (<) EXIST            (Logical,ref) Returned true if such an item
C                          exists, false otherwise.
C     (<) ACCESS           (Fixed string,descr) A single character that
C                          indicates the routine to be used to access
C                          the item in its `natural' form.  This will be
C                          one of `L',`I',`S',`C',`F',`D', corresponding
C                          to DSA_GET_FITS_L, DSA_GET_FITS_I, etc.  If
C                          the item exists but is of some non-standard
C                          type that these routines cannot access, then
C                          ACCESS will be set to blank.
C     (<) ELEMENTS         (Integer,ref) The number of elements in the
C                          FITS structure for this item.  This will be
C                          one for all bar the comment items that may
C                          occur multiply.
C     (<) STRLEN           (Integer,ref) If the item is a character string,
C                          this returns the number of characters in it.
C     (<) STATUS           (Integer,ref) Status code.  If bad status is
C                          passed to this routine, it returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:
C     DSA_WRNAME, DSA_WRUSER, DTA_TYVAR, DTA_STRUC, DTA_SZVAR,
C     DTA_ERROR, DTA_NMVAR, ICH_LEN, ICH_NUMBD, DSA__PARSE_FITS_ITEM,
C     DSA__FIND_NDF_FITS, DSA__NTH_NDF_FITS
C
C  Prior requirements:
C     The structure in question must have been opened by a routine such
C     as DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 31st August 1992
C
C  Note: This is an internal DSA routine and should not be called
C    from outside the DSA system.
C-
C  Subroutine / function details:
C     DSA_WRNAME     Output full name of DTA object to user
C     DSA_WRUSER     Output message to user
C     DSA_WRFLUSH    Flush message buffer to user
C     DTA_ERROR      Get description of DTA error code
C     DTA_NMVAR      Get name of nth data object in structure
C     DTA_STRUC      Determine if object is a structure or not
C     DTA_SZVAR      Get size of data object
C     DTA_TYVAR      Get type of data object
C     ICH_LEN        Position of last non-blank char in string
C     ICH_NUMBD      Decode a double precision number from a string
C     DSA__PARSE_FITS_ITEM  Get value string and comment from FITS string
C     DSA__FIND_NDF_FITS    Find a named FITS keyword in NDF FITS common
C     DSA__NTH_NDF_FITS     Find the nth FITS keyword in NDF FITS common
C
C  Common variable details:
C     (<) DTA_CODE    (Integer) Last DTA system error code
C     (>) OBJ_LEN     (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES   (String array) Name (as recognised by DTA_) of data
C                     object corresponding to reference name.
C     (>) NDF_FORMAT  (Logical array) Indicates structure format is Starlink's
C                     NDF format (described in SGP38).  If false, format is
C                     original Figaro format (DST files).
C
C  History:
C     1st Dec 1988.   Original version.  KS / AAO.
C     12th Feb 1990.  Name changed to DSA__, and NTH and REF_SLOT added
C                     to calling sequence.  NAME becomes KEYWORD and has
C                     changed meaning. Support for NDF format structures
C                     added.  'USHORT' data now returns 'I' instead of 'S'.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C     31st Aug 1992.  Change upper limit for SHORT to 2^15-1; was 2^16-1
C                     HME / UoE, Starlink.
C     25th Jul 1996.  MJCL / Starlink, UCL.  Moved DATA statement.
C
C  Note:
C     This version will support either the original Figaro data structures
C     or Starlink's NDF data structures.
C+
      SUBROUTINE DSA__FITS_DETAILS (REF_SLOT,NTH,KEYWORD,EXIST,ACCESS,
     :                                        ELEMENTS,STRLEN,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL EXIST
      INTEGER REF_SLOT, NTH, ELEMENTS, STRLEN, STATUS
      CHARACTER*(*) KEYWORD, ACCESS
C
C     Functions used
C
      INTEGER ICH_LEN, ICH_NUMBD
C
C     Data types and corresponding access characters (for original
C     Figaro data structures)
C
      INTEGER NTYPES
      PARAMETER (NTYPES=7)
      CHARACTER TYPES(NTYPES)*8
      CHARACTER CHARS(NTYPES)*1
C
C     DSA common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      CHARACTER CHAR1*1           ! First character of keyword name
      LOGICAL   CHRSTR            ! True if value is a quoted string
      LOGICAL   CMMT              ! True if keyword is comment-type
      CHARACTER COMMENT*32        ! Comment parsed from string - ignored
      INTEGER   DIMS(10)          ! Object dimensions
      CHARACTER DNAME*64          ! Name of actual data array
      INTEGER   DTA_STATUS        ! Status from DTA routines
      DOUBLE    PRECISION DVALUE  ! Value decoded from string
      CHARACTER ERROR*64          ! DTA error description
      LOGICAL   FLOAT             ! True if number is floating point
      INTEGER   I                 ! General loop index
      INTEGER   ISTR              ! String index in NDF string array
      INTEGER   IVAL              ! Integer version of DVALUE
      CHARACTER NAME*32           ! DTA system name of FITS item
      INTEGER   NDIM              ! Number of object dimensions
      INTEGER   NEXT              ! Next character after number - ignored
      INTEGER   NSFIG             ! Significant figures in numeric value
      INTEGER   NUM_STATUS        ! Status from attempted numeric decode
      CHARACTER STRING*80         ! Value string from NDF format item entry
      LOGICAL   STRUCT            ! True if object is a structure
      CHARACTER TYPE*16           ! Type of data object
C
      DATA TYPES/
     :  'DOUBLE', 'FLOAT', 'INT', 'CHAR', 'SHORT', 'BYTE', 'USHORT'/
      DATA CHARS/
     :  'D',      'F',     'I',   'C',    'S',     'L',    'I'/
C
C     Return immediately if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     The operations performed are quite different for NDF and for
C     original Figaro format structures.  So first we see what type
C     this is.
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        NDF format.  We look up the item either by name or by number,
C        having first made sure that the FITS data (if any) for this
C        structure has been read in.  This gives us the string number
C        (if non-zero) of the first string for this keyword.
C
         CALL DSA__READ_NDF_FITS (REF_SLOT,STATUS)
         IF (NTH.EQ.0) THEN
            CALL DSA__FIND_NDF_FITS (REF_SLOT,KEYWORD,1,.FALSE.,ISTR,
     :                                                          STATUS)
         ELSE
            CALL DSA__NTH_NDF_FITS (REF_SLOT,NTH,KEYWORD,ELEMENTS,
     :                                                     ISTR,STATUS)
         END IF
         IF (STATUS.NE.0) GO TO 500    ! Error exit
         EXIST=ISTR.NE.0
         IF (EXIST) THEN
C
C           We treat the potentially multi-valued comments specially,
C           so first we see if that's what we've got.
C
            CMMT=.FALSE.
            CHAR1=KEYWORD(1:1)
            IF (CHAR1.EQ.'C') THEN
               CMMT=KEYWORD.EQ.'COMMENT'
            ELSE IF (CHAR1.EQ.'H') THEN
               CMMT=KEYWORD.EQ.'HISTORY'
            ELSE IF (CHAR1.EQ.' ') THEN
               CMMT=KEYWORD.EQ.' '
            END IF
            IF (CMMT) THEN
C
C              It is one of the comment-type keywords.  In this case,
C              all we need find out about it is how many more of them
C              there are.  We do this rather crudely, by looking for
C              more and more until we run out.  Note that if NTH is
C              non-zero, we can omit this, since DSA__NTH_NDF_FITS will
C              have already got the answer for us.
C
               IF (NTH.EQ.0) THEN
                  ELEMENTS=1
                  DO WHILE (ISTR.NE.0)
                     CALL DSA__FIND_NDF_FITS (REF_SLOT,KEYWORD,
     :                                  ELEMENTS+1,.FALSE.,ISTR,STATUS)
                     IF (ISTR.NE.0) ELEMENTS=ELEMENTS+1
                  END DO
               END IF
               ACCESS='C'
               STRLEN=80
            ELSE
C
C              If it exists, and is not a multi-valued comment, get the
C              value string for it.  The on the basis of what we find there,
C              we can decide what type of access is appropriate.
C
               ELEMENTS=1
               CALL DSA__PARSE_FITS_ITEM (ISTR,CHRSTR,STRING,COMMENT,
     :                                                         STATUS)
               IF (STATUS.NE.0) GO TO 500      ! Error exit
               IF (CHRSTR) THEN
                  ACCESS='C'
                  STRLEN=80
               ELSE
C
C                 It might be a bit heavy handed, but the best way to see
C                 if a string is a valid number is to attempt to decode it.
C
                  NUM_STATUS=ICH_NUMBD (STRING,1,' ',DVALUE,NSFIG,NEXT)
                  IF (NUM_STATUS.EQ.0) THEN
                     FLOAT=.TRUE.
                     IF (ABS(DVALUE).LT.1.0D7) THEN
                        IF (INT(DVALUE).EQ.DVALUE) FLOAT=.FALSE.
                     END IF
                     IF (FLOAT) THEN
                        IF (NSFIG.GE.6) THEN
                           ACCESS='D'
                        ELSE
                           ACCESS='F'
                        END IF
                     ELSE
                        IVAL=DVALUE
                        IF ((IVAL.LE.32767).AND.(IVAL.GE.-32768)) THEN
                           ACCESS='S'
                        ELSE
                           ACCESS='I'
                        END IF
                     END IF
                  ELSE
C
C                    At this point, it wasn't a valid number, it wasn't
C                    a quoted string, and it wasn't a comment-type item.
C                    It probably isn't valid, but it could be logical.
C                    If it isn't logical, we'll suggest treating it as
C                    a character string, which will at least work.
C
                     IF ((STRING(1:1).EQ.'T').OR.(STRING(1:1).EQ.'F'))
     :                                                              THEN
                        ACCESS='L'
                     ELSE
                        ACCESS='C'
                     END IF
                  END IF
               END IF
            END IF
         END IF
C
      ELSE
C
C        Original Figaro format.  We have to generate the DTA name of the
C        structure object holding the item in question.
C
         IF (NTH.NE.0) THEN
C
C           It has been specified by number, so we have to get the name
C           of the nth item and trying to do this tells us if it exists
C           or not.  We trap the special name used for blank items.  Then
C           we get the type of the keyword.
C
            CALL DTA_NMVAR (OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                                '.FITS',NTH,KEYWORD,DTA_STATUS)
            IF (DTA_STATUS.NE.0) THEN
               EXIST=.FALSE.
               KEYWORD=' '
            END IF
            NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.FITS.'//
     :                                                          KEYWORD
            IF (KEYWORD.EQ.'BLANK_STRINGS') KEYWORD=' '
            CALL DTA_TYVAR (NAME,TYPE,DTA_STATUS)
         ELSE
C
C           It was specified by name, so we generate the name from that,
C           allowing for the case where blank keywords are given a special
C           non-blank item name.  Getting its type then also acts as an
C           existence check.
C
            IF (KEYWORD.EQ.' ') THEN
               NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                                           '.FITS.BLANK_STRINGS'
            ELSE
               NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.FITS.'//
     :                                                          KEYWORD
            END IF
            CALL DTA_TYVAR (NAME,TYPE,DTA_STATUS)
            EXIST=DTA_STATUS.EQ.0
         END IF
C
         IF (EXIST) THEN
C
C           It exists, but it just might be a structure with the data in
C           .DATA and the comment in .DESCRIPTION.  If it is a structure,
C           see if it has a .DATA component, and see if this it itself
C           a structure (which we can't handle).
C
            DNAME=NAME
            CALL DTA_STRUC (NAME,STRUCT,DTA_STATUS)
            IF (STRUCT) THEN
               DNAME=NAME(:ICH_LEN(NAME))//'.DATA'
               CALL DTA_TYVAR (DNAME,TYPE,DTA_STATUS)
               IF (DTA_STATUS.NE.0) THEN
                  CALL DSA_WRUSER ('Warning: the FITS item ')
                  CALL DSA_WRNAME (NAME)
                  CALL DSA_WRUSER (
     :                     ' is of unexpected type and cannot be '
     :                     //'accessed using the standard routines.')
                  CALL DSA_WRFLUSH
                  ACCESS=' '
                  GO TO 500      ! Immediate exit
               END IF
               CALL DTA_STRUC (DNAME,STRUCT,DTA_STATUS)
               IF (STRUCT) THEN
                  CALL DSA_WRUSER ('Warning: the FITS item ')
                  CALL DSA_WRNAME (DNAME)
                  CALL DSA_WRUSER (' is a structure and cannot be '//
     :                     'accessed using the standard routines.')
                  CALL DSA_WRFLUSH
                  ACCESS=' '
                  GO TO 500      ! Immediate exit
               END IF
            END IF
C
C           Work out the appropriate access routine.
C
            ACCESS=' '
            DO I=1,NTYPES
               IF (TYPES(I).EQ.TYPE) THEN
                  ACCESS=CHARS(I)
                  GO TO 320         ! Break out of I loop
               END IF
            END DO
  320       CONTINUE
C
C           If we didn't recognise the type, we fall back on suggesting
C           `double' as an access type.
C
            IF (ACCESS.EQ.' ') ACCESS='D'
C
C           Get the size of the item
C
            CALL DTA_SZVAR (DNAME,10,NDIM,DIMS,DTA_STATUS)
            IF (DTA_STATUS.NE.0) THEN
               CALL DSA_WRUSER ('Error getting size of the FITS item ')
               CALL DSA_WRNAME (DNAME)
               CALL DSA_WRUSER ('. ')
               CALL DTA_ERROR (DTA_STATUS,ERROR)
               CALL DSA_WRUSER (ERROR(:ICH_LEN(ERROR)))
               CALL DSA_WRUSER ('.')
               CALL DSA_WRFLUSH
               STATUS=DSA__DTAERR
               DTA_CODE=DTA_STATUS
               GO TO 500      ! Error exit
            END IF
            IF (ACCESS.EQ.'C') THEN
               IF (NDIM.GT.2) THEN
                  CALL DSA_WRUSER ('Warning: the FITS item ')
                  CALL DSA_WRNAME (DNAME)
                  CALL DSA_WRUSER (' is an string array of more than 2 '
     :               //'dimensions.  This is not a standard item.')
                  CALL DSA_WRFLUSH
               END IF
               STRLEN=DIMS(1)
               ELEMENTS=1
               DO I=2,NDIM
                  ELEMENTS=ELEMENTS*DIMS(I)
               END DO
            ELSE
               IF (NDIM.GT.1) THEN
                  CALL DSA_WRUSER ('Warning: the FITS item ')
                  CALL DSA_WRNAME (DNAME)
                  CALL DSA_WRUSER (' is a multi-dimensional numeric '//
     :                        'array.  This is not a standard item.')
                  CALL DSA_WRFLUSH
               END IF
               ELEMENTS=1
               DO I=1,NDIM
                  ELEMENTS=ELEMENTS*DIMS(I)
               END DO
            END IF
         END IF
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
