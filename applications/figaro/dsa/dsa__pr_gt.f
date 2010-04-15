C+
C                   D S A _ _ P R E _ G E T _ F I T S
C
C  Routine name:
C     DSA__PRE_GET_FITS
C
C  Function:
C     Performs pre-processing for DSA_GET_FITS_{x} routines.
C
C  Description:
C     This routine searches the basic structure used for the FITS
C     item specified, and if it exists, reads any comment string for it,
C     and returns a code specifying the type of data structure used
C     for the FITS items, and a string that will allow the specified
C     item's value to be read.  For original Figaro format data, this
C     string will be the name of the DTA item that holds the data.
C     For NDF data this will be an string containing the actual data
C     as a formatted string. Both CODE and STRING will normally be
C     passed to one of the DSA__READ_FITS_{x} routines, which know
C     how to handle them.  This routine expects the item to exist,
C     and will output error messages if it does not.  To see if an item
C     exists, the routine DSA_SEEK_FITS should be used.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__PRE_GET_FITS (REF_NAME,ITEM,ELEMENT,CODE,COMMENT,
C                                                      STRING,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME        (Fixed string,descr) The reference name for
C                         the structure.
C     (>) ITEM            (Fixed string,descr) The name of the item
C                         in question.  This should be a FITS keyword.
C     (>) ELEMENT         (Integer,ref) The element of the FITS item
C                         to be accessed.  Normally, a FITS item has
C                         only one element, in which case ELEMENT can be
C                         passed as either 0 or 1.  Items that can be
C                         multiple, such as `HISTORY' will need to
C                         specify the required element.
C     (<) CODE            (Integer, ref) A code indicating the type of
C                         data structure holding the FITS items.
C     (<) COMMENT         (Fixed string,descr) The comment that is
C                         associated with the item.  If there was no
C                         such comment, this is set to blanks.
C     (<) STRING          (Fixed string,descr) Returned as the DTA_ name
C                         of the object used for the FITS value, for
C                         original Figaro format data, as a formatted
C                         version of the data for NDF formatted data.
C     (!) STATUS          (Integer,ref) Status code.  If bad status is
C                         passed to it, this routine returns immedaitely.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:
C     DSA_WRUSER, DSA_FIND_REF, DSA_GET_ACTUAL_NAME, DTA_TYVAR, DTA_CRNAM,
C     DTA_RDVARC, DTA_STRUC, ICH_FOLD, ICH_LEN, DSA__READ_NDF_FITS,
C     DSA__FIND_NDF_FITS, DSA__PARSE_FITS_ITEM
C
C  Prior requirements:
C     This routine is intended to be called by the set of DSA_GET_FITS_{x}
C     routines.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (>) DST_CODE     (Integer parameter) Code for DST format structures.
C     (>) NDF_CODE     (Integer parameter) Code for NDF format structures.
C     (>) NDF_FORMAT   (Logical array) Indicates structure format is Starlink's
C                      NDF format (described in SGP38).  If false, format is
C                      original Figaro format (DST files).
C
C  Subroutine / function details:
C     ICH_LEN         Position of last non-blank character in string
C     ICH_FOLD        Convert string to upper case
C     DTA_TYVAR       Get type of data object
C     DTA_CRNAM       Create name of data object, including dimensions
C     DTA_RDVARC      Read character string from a data object
C     DTA_STRUC       See if data object is a structure
C     DSA_WRUSER      Output string to user
C     DSA_FIND_REF    Look up reference name in common tables
C     DSA_GET_ACTUAL_NAME  Get full name of structure
C     DSA__READ_NDF_FITS   Make sure FITS data for NDF files has been read
C     DSA__FIND_NDF_FITS   Find a FITS item in the NDF FITS common arrays
C     DSA__PARSE_FITS_ITEM Split an NDF FITS common string into value & comment
C
C  History:
C     2nd Dec 1988.   Original version.  KS / AAO.
C     7th Feb 1990.   Name changed to DSA__, modified to handle NDF data
C                     formats.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA__PRE_GET_FITS (REF_NAME,ITEM,ELEMENT,CODE,COMMENT,
     :                                                   STRING,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER ELEMENT, CODE, STATUS
      CHARACTER*(*) REF_NAME, ITEM, STRING, COMMENT
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN
C
C     DSA_ common variables
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      LOGICAL   CHRSTR         ! True if NDF item is a character string
      INTEGER   DIMS(2)        ! Element number to be accessed
      INTEGER   DTA_STATUS     ! Status return from DTA call
      LOGICAL   EXIST          ! True if object already exists
      CHARACTER FITS_NAME*80   ! DTA_ name for element of FITS structure
      INTEGER   IGNORE         ! Dummy status argument
      INTEGER   INVOKE         ! Dummy function value
      CHARACTER ITEM_NAME*16   ! Name of structure element to use
      INTEGER   LENAME         ! Characters in FITS_NAME
      INTEGER   LENGTH         ! Characters in OBJ_NAME
      INTEGER   NDIM           ! Number of dimensions of object
      INTEGER   NSTR           ! Number of string in NDF FITS common array
      CHARACTER OBJECT*80      ! Name of data object (comment) to be read
      CHARACTER OBJ_NAME*80    ! DTA_ name for structure
      CHARACTER REF_NAME_UC*32 ! Upper case version of REF_NAME
      INTEGER   REF_SLOT       ! Reference table slot #
      LOGICAL   STRUCT         ! Indicates object is a structure
      CHARACTER STRUCTURE*80   ! Full name of structure
      CHARACTER TYPE*16        ! Type of object
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name
C
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
      IF (STATUS.NE.0) GO TO 500      ! Error exit
C
C     What happens next is very different for the two types of data
C     structures.
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        The data structure is in NDF format.  We make sure the FITS
C        data (if any) has been read in, we look for the requested
C        item, and we parse it into its value and comment parts.
C
         CODE=NDF_CODE
         CALL DSA__READ_NDF_FITS (REF_SLOT,STATUS)
         CALL DSA__FIND_NDF_FITS (REF_SLOT,ITEM,ELEMENT,.TRUE.,NSTR,
     :                                                         STATUS)
         CALL DSA__PARSE_FITS_ITEM (NSTR,CHRSTR,STRING,COMMENT,STATUS)
C
      ELSE
C
C        The data structure is in original Figaro format.  We have to hunt
C        around the file for the various places the FITS information might
C        have been stored.
C
         CODE=DST_CODE
C
C        We trap the case where ITEM is blank, and substitute the name
C        of the special structure element used for the blank keyword.
C
         IF (ITEM.EQ.' ') THEN
            ITEM_NAME='BLANK_STRINGS'
         ELSE
            ITEM_NAME=ITEM
         END IF
C
C        Generate the name of the FITS item.  This can be either a
C        primitive item, or a structure.  If it is a structure, it
C        contains the comment as well as the data.  If it is not, the
C        comment should be in a separate .COMMENTS structure.
C
         FITS_NAME=OBJ_NAME(:LENGTH)//'.FITS.'//ITEM_NAME
         LENAME=ICH_LEN(FITS_NAME)
         CALL DTA_STRUC (FITS_NAME,STRUCT,DTA_STATUS)
         EXIST=DTA_STATUS.EQ.0
         IF (EXIST) THEN
            IF (STRUCT) THEN
               STRING=FITS_NAME(:LENAME)//'.DATA'
            ELSE
               STRING=FITS_NAME
            END IF
            CALL DTA_TYVAR (STRING,TYPE,DTA_STATUS)
            EXIST=DTA_STATUS.EQ.0
         END IF
         IF (.NOT.EXIST) THEN
            CALL DSA_WRUSER ('Unable to access the FITS item "')
            IF (ITEM.EQ.' ') THEN
               CALL DSA_WRUSER ('blank')
            ELSE
               CALL DSA_WRUSER (ITEM(:ICH_LEN(ITEM)))
            END IF
            CALL DSA_WRUSER ('" in ')
            IGNORE=0
            CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,IGNORE)
            CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
            CALL DSA_WRUSER ('.  No such item exists.')
            CALL DSA_WRFLUSH
            STATUS=DSA__NOOBJ
            GO TO 500      ! Error exit
         END IF
C
C        Read the comment associated with the object, if any.
C
         IF (STRUCT) THEN
            OBJECT=FITS_NAME(:LENAME)//'.DESCRIPTION'
         ELSE
            OBJECT=OBJ_NAME(:LENGTH)//'.COMMENTS.'//ITEM_NAME
         END IF
         CALL DTA_RDVARC (OBJECT,LEN(COMMENT),COMMENT,DTA_STATUS)
         IF (DTA_STATUS.NE.0) COMMENT=' '
C
C        Now for the object itself.  If an element of the object is
C        required, then we need to put dimension information in the
C        name string.  This has to be handled differently for
C        character strings.  Note that if the object is structured, we
C        got its type a while back, as a test to see if it existed.
C
         IF (ELEMENT.GT.1) THEN
            IF (.NOT.STRUCT) CALL DTA_TYVAR(STRING,TYPE,DTA_STATUS)
            IF (TYPE.EQ.'CHAR') THEN
               NDIM=2
               DIMS(1)=1
               DIMS(2)=ELEMENT
            ELSE
               NDIM=1
               DIMS(1)=ELEMENT
            END IF
            IF (STRUCT) THEN
               CALL DTA_CRNAM (FITS_NAME,'DATA',NDIM,DIMS,STRING,
     :                                                      DTA_STATUS)
            ELSE
               CALL DTA_CRNAM (OBJ_NAME(:LENGTH)//'.FITS',ITEM_NAME,
     :                                     NDIM,DIMS,STRING,DTA_STATUS)
            END IF
         END IF
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
