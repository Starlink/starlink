C+
C                   D S A _ _ P R E _ P U T _ F I T S
C
C  Routine name:
C     DSA__PRE_PUT_FITS
C
C  Function:
C     Performs pre-processing for DSA_PUT_FITS_{x} routines.
C
C  Description:
C     This routine is a utility for use by the DSA_PUT_FITS_{x}
C     routines, doing any necessary pre-processing ready for a
C     call to a DSA__WRITE_FITS_{x} routine.  It can handle both
C     original Figaro format data and Starlink's NDF format data,
C     which differ significantly in the way they handle FITS data.
C     For data in the original Figaro format, this routine creates
C     the basic structure needed for a FITS item, writes the comment
C     string for it, and returns the name of the DTA_ item to be used
C     to hold the value.  For NDF format data, all the FITS strings
C     for a file are held in common until the file is closed, and this
C     routine either finds the string corresponding to the keyword
C     in question, or allocates a new string to be used, and then
C     returns that string number.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__PRE_PUT_FITS (REF_NAME,KEYWORD,TYPE,LENSTR,COMMENT,
C                                           CODE,NAME,STRING,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME        (Fixed string,descr) The reference name for
C                         the structure.
C     (!) KEYWORD         (Fixed string,descr) The name of the item
C                         in question.  This should be a FITS keyword.
C                         It need not be passed in upper case, but it
C                         will be returned as an upper case version -
C                         this routine MUST be able to write to it.
C     (>) TYPE            (Fixed string,descr) The type of the object
C                         to be created.  This should be a DTA_ type
C                         name and should be in upper case.
C     (>) LENSTR          (Integer,ref) If item is a string, this should
C                         be the number of characters to use for it.  If
C                         item is numeric, this must be zero.
C     (>) COMMENT         (Fixed string,descr) The comment to be
C                         associated with the item.
C     (<) CODE            (Integer, ref) A code indicating the type of
C                         data structure holding the FITS items.
C     (<) NAME            (Fixed string,descr) Returned as the DTA_ name
C                         of the object to be used for the FITS value,
C                         for original Figaro format data.  Unused for
C                         NDF format data.
C     (<) STRING          (Integer) For original Figaro format data, this
C                         is the element of the common array FITS_STRINGS
C                         to use.  Set to zero for numeric data.  For a
C                         string, this is normally set to zero and NAME
C                         should be used, unless it is one of the comment
C                         strings that have to be buffered, in which case
C                         it must be put into the common array instead.
C                         For NDF format data, this is the element of the
C                         common array FITS_ARRAY to be used for the keyword.
C     (!) STATUS          (Integer,ref) Status code.  If bad status is
C                         passed to it, this routine returns immedaitely.
C
C  External variables used:
C     Only common variables internal to the DSA system
C
C  External subroutines / functions used:
C     DSA_WRUSER, DSA_FIND_REF, DSA_FITS_BUFF, DSA_GET_ACTUAL_NAME,
C     DTA_CRVAR, DTA_TYVAR, DTA_SZVAR, DTA_DLVAR, DTA_CRNAM, DTA_WRVARC,
C     DTA_ERROR, DSA__READ_NDF_FITS, DSA__FIND_NDF_FITS, DSA__FITS_SPACE
C
C  Prior requirements:
C     This routine is intended to be called by the set of DSA_PUT_FITS_{x}
C     routines.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_WRUSER      Output string to user
C     DSA_FIND_REF    Look up reference name in common tables
C     DSA_FITS_BUFF   Reserve space in common for buffered string
C     DSA_GET_ACTUAL_NAME  Get full name of structure
C     DSA__READ_NDF_FITS   Read NDF FITS data from file into common array
C     DSA__FIND_NDF_FITS   Look for a FITS keyword in the NDF common array
C     DSA__FITS_SPACE Get space for a number of strings in NDF common
C     DTA_CRVAR       Create data object
C     DTA_TYVAR       Get type of data object
C     DTA_SZVAR       Get dimensions of data object
C     DTA_DLVAR       Delete data object
C     DTA_CRNAM       Create name of data object, including dimensions
C     DTA_WRVARC      Write character string to a data object
C     DTA_ERROR       Get string describing a DTA error code
C
C  Common variable details:
C     (<) DTA_CODE    (Integer) Last DTA error code
C     (!) FITS_OPEN   (Logical array) Indicates FITS structure opened
C     (>) DST_CODE    (Integer parameter) Code for DST format structures.
C     (>) NDF_CODE    (Integer parameter) Code for NDF format structures.
C     (>) NDF_FORMAT  (Logical array) Indicates structure format is Starlink's
C                     NDF format (described in SGP38).  If false, format is
C                     original Figaro format (DST files).
C     (!) FITS_ITEMS  (Integer array) FITS item numbers for each array entry.
C                     This matches the string number in the original FITS
C                     header, but is -ve if the string has been modified.
C
C  History:
C     28th Nov 1988.   Original version.  KS / AAO.
C     14th Feb 1990.   Name changed to DSA__, calling sequence also
C                      modified.  Now supports NDF format files.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     30th Jul 1996    Workspace for string catenations.  MJCL/Starlink, UCL.
C+
      SUBROUTINE DSA__PRE_PUT_FITS (REF_NAME,KEYWORD,TYPE,LENSTR,
     :                              COMMENT,CODE,NAME,STRING,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER LENSTR, CODE, STRING, STATUS
      CHARACTER*(*) REF_NAME, KEYWORD, TYPE, NAME, COMMENT
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      LOGICAL   BUFFER         ! Indicates item needs to be buffered
      CHARACTER CHAR1*1        ! First character in KEYWORD
      LOGICAL   CTYPE          ! Indicates keyword is a comment-type keyword
      INTEGER   DTA_STATUS     ! Status return from DTA call
      CHARACTER ERROR*64       ! DTA error description
      LOGICAL   EXIST          ! True if object already exists
      INTEGER   IGNORE         ! Dummy status argument
      INTEGER   INVOKE         ! Dummy function value
      INTEGER   LENGTH         ! Characters in OBJ_NAME
      INTEGER   NCHAR          ! Number of significant characters in string
      INTEGER   NDIM           ! Number of dimensions of object
      INTEGER   NELM           ! Elements in existing object
      CHARACTER OBJ_NAME*80    ! DTA_ name for structure
      CHARACTER OBJECT*80      ! Name of data object to be created
      CHARACTER OLDTYPE*8      ! Type of existing object
      CHARACTER REF_NAME_UC*32 ! Upper case version of REF_NAME
      INTEGER   REF_SLOT       ! Reference table slot #
      CHARACTER STRUCTURE*80   ! Full name of structure
      CHARACTER WSTRING*80     ! Workspace for catenations
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Check on length of KEYWORD and convert it to upper case
C
      NCHAR=ICH_FOLD(KEYWORD)
      IF (NCHAR.GT.8) THEN
         WSTRING = 'Warning: "'//KEYWORD(:NCHAR)//
     :             '" is more than eight characters long, which '
         CALL DSA_WRUSER ( WSTRING )
         CALL DSA_WRUSER ('makes it unsuitable for a FITS keyword. ')
         CALL DSA_WRUSER ('It will be used as it stands, but may cause'
     :       //' problems if a FITS tape has to be written from this'
     :       //' structure.')
         CALL DSA_WRFLUSH
      END IF
C
C     Look up the reference name
C
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
      IF (STATUS.NE.0) GO TO 500      ! Error exit
C
C     What happens next is very different for the two formats.
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        NDF format.  All keywords are held in an array of FITS header
C        strings in common and output only when the file is closed.  If
C        the keyword is a multiple-valued type, then we always need a new
C        string in the array.  If it is a normal, single-valued, keyword,
C        then we look to see if we have it in the array, and only if it isn't
C        there already do we get a new slot for it.  We don't do anything
C        with COMMENT, since that has to incorporated in the string at the
C        same time as the value.  The string is flagged as modified (a -ve
C        item number).
C
         CODE=NDF_CODE
         IF (.NOT.FITS_OPEN(REF_SLOT)) THEN
            CALL DSA__READ_NDF_FITS (REF_SLOT,STATUS)
            IF (STATUS.NE.0) GO TO 500    ! Error exit
         END IF
         CHAR1=KEYWORD(1:1)
         IF (CHAR1.EQ.'C') THEN
            CTYPE=KEYWORD.EQ.'COMMENT'
         ELSE IF (CHAR1.EQ.'H') THEN
            CTYPE=KEYWORD.EQ.'HISTORY'
         ELSE IF (CHAR1.EQ.' ') THEN
            CTYPE=KEYWORD.EQ.' '
         ELSE
            CTYPE=.FALSE.
         END IF
         IF (CTYPE) THEN
            CALL DSA__FITS_SPACE (REF_SLOT,1,STRING,STATUS)
         ELSE
            CALL DSA__FIND_NDF_FITS (REF_SLOT,KEYWORD,1,.FALSE.,STRING,
     :                                                           STATUS)
            IF (STRING.EQ.0) THEN
               CALL DSA__FITS_SPACE (REF_SLOT,1,STRING,STATUS)
            END IF
         END IF
         IF (STATUS.EQ.0) FITS_ITEMS(STRING)=-ABS(FITS_ITEMS(STRING))
      ELSE
C
C        Original Figaro format.  FITS keywords are held in separate
C        DTA objects and are normally written immediately to them.  The
C        only exceptions are the comment-type keywords which are
C        potentially multi-valued and so are buffered.
C
         CODE=DST_CODE
C
C        See if the FITS structure has been opened yet.  If not, try to
C        create the two sub-structures used (if this fails, they probably
C        already exist)
C
         IF (.NOT.FITS_OPEN(REF_SLOT)) THEN
            CALL DTA_CRVAR (OBJ_NAME(:LENGTH)//'.FITS','STRUCT',
     :                                                       DTA_STATUS)
            CALL DTA_CRVAR (OBJ_NAME(:LENGTH)//'.COMMENTS','STRUCT',
     :                                                       DTA_STATUS)
            FITS_OPEN(REF_SLOT)=.TRUE.
         END IF
C
C        What happens next depends on whether or not the object is a
C        string, and if so, whether or not it is a `comment' string that
C        will need to be buffered.  DSA_FITS_BUFF tests to see if a string
C        is to be buffered, and if it is, returns a buffer slot number that
C        should be used for it.  And that's all we need.
C
         IF (LENSTR.EQ.0) THEN
            BUFFER=.FALSE.
         ELSE
            CALL DSA_FITS_BUFF (REF_SLOT,KEYWORD,STRING,STATUS)
            BUFFER=STRING.NE.0
         END IF
C
         IF (.NOT.BUFFER) THEN
C
C           Items that do not require buffering are written out directly.
C           First, we see if the object exists.  If it does, but is the
C           wrong type, or in the case of a string, too short, we have
C           to delete it first - a string could be reshaped, but that
C           optimisation can be added later.
C
            NAME=OBJ_NAME(:LENGTH)//'.FITS.'//KEYWORD
            CALL DTA_TYVAR (NAME,OLDTYPE,DTA_STATUS)
            EXIST=DTA_STATUS.EQ.0
            IF (EXIST) THEN
               IF (TYPE.NE.OLDTYPE) THEN
                  CALL DTA_DLVAR (NAME,DTA_STATUS)
                  EXIST=.FALSE.
               ELSE
                  CALL DTA_SZVAR (NAME,1,NDIM,NELM,DTA_STATUS)
                  IF (LENSTR.EQ.0) THEN
                     IF (NDIM.NE.0) THEN
                        CALL DTA_DLVAR (NAME,DTA_STATUS)
                        EXIST=.FALSE.
                     END IF
                  ELSE
                     IF (NELM.LT.LENSTR) THEN
                        CALL DTA_DLVAR (NAME,DTA_STATUS)
                        EXIST=.FALSE.
                     END IF
                  END IF
               END IF
            END IF
            IF (.NOT.EXIST) THEN
C
C              If it didn't exist, or if it did but was deleted, create it.
C              We don't test the status here.  If it fails, it will be
C              picked up when the caller tries to write to it.
C
               IF (LENSTR.EQ.0) THEN
                  NDIM=0
               ELSE
                  NDIM=1
               END IF
               CALL DTA_CRNAM (OBJ_NAME(:LENGTH)//'.FITS',KEYWORD,NDIM,
     :                                         LENSTR,OBJECT,DTA_STATUS)
               CALL DTA_CRVAR (OBJECT,TYPE,DTA_STATUS)
            END IF
C
C           Now we write the comment record as well.  We try to create this
C           directly and if it existed already we ignore the error.  Then
C           we write it out.
C
            NCHAR=ICH_LEN(COMMENT)
            IF (NCHAR.GT.0) THEN
               NCHAR=MAX(NCHAR,64)
               CALL DTA_CRNAM (OBJ_NAME(:LENGTH)//'.COMMENTS',KEYWORD,1,
     :                                          NCHAR,OBJECT,DTA_STATUS)
               CALL DTA_CRVAR (OBJECT,'CHAR',DTA_STATUS)
               CALL DTA_CRNAM (OBJ_NAME(:LENGTH)//'.COMMENTS',KEYWORD,
     :                                            0,0,OBJECT,DTA_STATUS)
               CALL DTA_WRVARC (OBJECT,NCHAR,COMMENT,DTA_STATUS)
               IF (DTA_STATUS.NE.0) THEN
                  CALL DSA_WRUSER (
     :                    'Unable to write the FITS comment for "')
                  CALL DSA_WRUSER (KEYWORD(:ICH_LEN(KEYWORD)))
                  CALL DSA_WRUSER ('" in ')
                  IGNORE=0
                  CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,IGNORE)
                  CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
                  CALL DSA_WRUSER ('. ')
                  CALL DTA_ERROR (DTA_STATUS,ERROR)
                  CALL DSA_WRUSER (ERROR(:ICH_LEN(ERROR)))
                  CALL DSA_WRUSER ('.')
                  CALL DSA_WRFLUSH
                  STATUS=DSA__DTAERR
                  DTA_CODE=DTA_STATUS
               END IF
            END IF
         END IF
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
