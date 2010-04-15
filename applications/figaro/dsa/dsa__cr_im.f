C+
C                   D S A _ _ C R E A T E _ I M A G I N A R Y
C
C  Routine name:
C     DSA__CREATE_IMAGINARY
C
C  Function:
C     Structure-specific conversion of a real main array into a complex one.
C
C  Description:
C     This routine assumes that the structure whose reference slot number
C     it is passed contains a real data array but no imaginary array.
C     It creates the necessary imaginary array, allowing for the requirements
C     of the format used by the structure.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__CREATE_IMAGINARY (REF_SLOT,TYPE,NDIM,DIMS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT        (Integer,ref) The reference slot number for the
C                         structure.
C     (>) TYPE            (Fixed string,descr) The type to be used for
C                         the imaginary array.  This should be a DTA
C                         primitive type.
C     (>) NDIM            (Integer,ref) The number of dimensions for the
C                         imaginary array.
C     (>) DIMS            (Integer array,ref) The dimensions for the
C                         imaginary array.
C     (!) STATUS          (Integer,ref) Status return code.  If bad status
C                         is passed to this routine it returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA package.
C
C  External subroutines / functions used:
C     DSA_CHECK_MAPPING, DTA_CRNAM, DTA_CRVAR, DTA_RNVAR, ICH_LEN,
C     DSA_WRUSER
C
C  Prior requirements:
C     REF_SLOT must refer to an open structure.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C
C  Note:
C     This version supports both the original Figaro data format and
C     Starlink's NDF format.
C-
C  Subroutine / function details:
C     DSA_CHECK_MAPPING  Checks for active mappings and closes inactive ones.
C     DSA_WRUSER    Outputs text to the user.
C     DTA_CRNAM     Generates the name of a data object, including dimensions.
C     DTA_CRVAR     Creates a named data object.
C     DTA_RNVAR     Renames a data object.
C     DTA_ERROR     Converts a DTA error code into text.
C     ICH_LEN       Position of last non-blank char in string.
C
C  Common variable details:
C     (>) OBJ_LEN       (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES     (String array) Name (as recognised by DTA_) of data
C                       object corresponding to reference name.
C     (>) NDF_FORMAT    (Logical array) Indicates structure format is Starlink's
C                       NDF format (described in SGP38).  If false, format is
C                       original Figaro format (DST files).
C     (<) DTA_CODE      (Integer) Last DTA_ system failure status code.
C     (>) ACTUAL_NAMES  (String array) Fully extended name for the structure.
C
C  History:
C     2nd  May 1990.   Original version.  KS / AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992    Trivial change to clip all lines to 80 chars. KS/AAO.
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     25th Jul 1995    With simple NDFs (structured arrays) the
C                      preferred form, this routine must take more care
C                      when moving DATA to REAL.
C+
      SUBROUTINE DSA__CREATE_IMAGINARY (REF_SLOT,TYPE,NDIM,DIMS,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, NDIM, DIMS(*), STATUS
      CHARACTER*(*) TYPE
C
C     Functions
C
      INTEGER ICH_LEN
C
C     DSA common definition and error codes
C
      INCLUDE 'DSA_COMMON'
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      INTEGER   DTA_STATUS       ! Status returned by DTA routines
      CHARACTER ERROR*64         ! Error text corresponding to DTA_STATUS
      INTEGER   IGNORE           ! A status we don't care about
      INTEGER   INLEN            ! Number of characters in INPUT
      CHARACTER INPUT*80         ! Name of main structured array
      CHARACTER NAME*80          ! Name of array structure components
      INTEGER   OUTLEN           ! Number of characters in OUTPUT
      CHARACTER OUTPUT*80        ! Name of temporary output array
      LOGICAL   STRUCT           ! True if old DATA_ARRAY is structure
C
C     Return immediately if bad status passed.
C
      IF (STATUS.NE.0) RETURN
C
C     See what format the structure is in.
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        For NDF format, things are a little messy, since there will
C        be an existing real array which may be either primitive or
C        structured, and we need to put it into a COMPLEX_ARRAY
C        structure along with our new imaginary array.  So we create
C        such a structure (using a temporary name), copy the existing array
C        into it, add the imaginary array, and then rename the structure
C        so that it is now the new data array.  If we do this, we have to
C        be sure that none of the DATA_ARRAY components are mapped (which
C        means that for these structures you cannot create an imaginary
C        array if there is an existing mapped real array, which is a pity).
C
C        In the case of a structured DATA_ARRAY, only its DATA component
C        is retained, we don't even look at what else might be in there.
C
         INPUT=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.DATA_ARRAY'
         INLEN=OBJ_LEN(REF_SLOT)+11
         CALL DSA_CHECK_MAPPING (INPUT,INLEN,
     :      ' create imaginary array by rearranging data structure ',
     :                                                        STATUS)
         IF (STATUS.NE.0) GO TO 500    ! Error exit
         OUTPUT=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.TEMP_ARRAY'
         OUTLEN=OBJ_LEN(REF_SLOT)+11
         CALL DTA_CRVAR(OUTPUT,'COMPLEX_ARRAY',DTA_STATUS)
         IF (DTA_STATUS.NE.0) GO TO 500     ! Error exit
         CALL DTA_STRUC(INPUT,STRUCT,DTA_STATUS)
         IF (DTA_STATUS.NE.0) GO TO 500     ! Error exit
         IF (STRUCT) THEN
            CALL DTA_RNVAR(INPUT(:INLEN)//'.DATA',
     :         OUTPUT(:OUTLEN)//'.REAL',DTA_STATUS)
            IF (DTA_STATUS.NE.0) GO TO 500     ! Error exit
            CALL DTA_DLVAR(INPUT,DTA_STATUS)
            IF (DTA_STATUS.NE.0) GO TO 500     ! Error exit
         ELSE
            CALL DTA_RNVAR(INPUT,OUTPUT(:OUTLEN)//'.REAL',DTA_STATUS)
            IF (DTA_STATUS.NE.0) GO TO 500     ! Error exit
         END IF
         CALL DTA_CRNAM(OUTPUT,'IMAGINARY',NDIM,DIMS,NAME,DTA_STATUS)
         CALL DTA_CRVAR(NAME,TYPE,DTA_STATUS)
         IF (DTA_STATUS.NE.0) GO TO 500     ! Error exit
         CALL DTA_RNVAR(OUTPUT,INPUT,DTA_STATUS)
         IF (DTA_STATUS.NE.0) GO TO 500     ! Error exit
C
      ELSE
C
C        For the original Figaro format, it's easy.  We just generate
C        the array required in the .Z structure and that's it.
C
         CALL DTA_CRNAM(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.Z',
     :                          'IMAGINARY',NDIM,DIMS,NAME,DTA_STATUS)
         CALL DTA_CRVAR(NAME,TYPE,DTA_STATUS)
      END IF
C
C     Exit
C
  500 CONTINUE
      IF (STATUS.EQ.0) THEN
         IF (DTA_STATUS.NE.0) THEN
            CALL DTA_ERROR (DTA_STATUS,ERROR)
            CALL DSA_WRUSER(
     :             'Error trying to create imaginary array in ')
            IGNORE=0
            CALL DSA_WRUSER(ACTUAL_NAMES(REF_SLOT)
     :                             (:ICH_LEN(ACTUAL_NAMES(REF_SLOT))))
            CALL DSA_WRUSER('. ')
            CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
            CALL DSA_WRUSER('.')
            CALL DSA_WRFLUSH
            DTA_CODE=DTA_STATUS
            STATUS=DSA__DTAERR
         END IF
      END IF
C
      END
