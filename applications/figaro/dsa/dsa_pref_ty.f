
C+
C                   D S A _ P R E F E R R E D _ T Y P E
C
C  Routine name:
C     DSA_PREFERRED_TYPE
C
C  Function:
C     Provides the primitive type best suited to a structured array.
C
C  Description:
C     The various DSA routines that return a type for an array, such
C     as DSA_DATA_TYPE, return a straightforward primitive type (such
C     as 'FLOAT') when the array is a simple primitive array.  However,
C     if the array is structured, then the type returned will not be
C     so straightforward, and the calling routine will probably not
C     be able to decide easily on the primitive type it should use to
C     handle the data in the array.  This routine takes a type
C     specification as returned by one of the type enquiry routines,
C     and returns the primitive type best suited to handling the data.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_PREFERRED_TYPE (STRUCT_TYPE,PRIM_TYPE,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) STRUCT_TYPE        (Fixed string,descr) A type as returned by
C                            one of the Figaro type enquiry routines.
C                            This can be a primitive or structured type.
C     (<) PRIM_TYPE          (Fixed string,descr) The primitive data type
C                            best suited to handling the data whose type
C                            is given by STRUCT_TYPE.
C     (!) STATUS             (Integer,ref) Status code.  If bad status is
C                            passed to it, this routine returns immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DSA__TYPE_DETAILS, ICH_LEN, DSA_WRUSER
C
C  Prior requirements: None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_LEN      Position of last non-blank char in string.
C     DSA_WRUSER   Output message to user.
C     DSA__TYPE_DETAILS  Looks at syntax of a type description.
C
C  History:
C     2nd  Feb 1990.   Original version.  KS / AAO.
C     27th Apr 1990.   Now uses DSA__TYPE_DETAILS and so supports some
C                      SGP38 structured types.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     25th Jul 1996    Catneation for Linux.   MJCL/Starlink, UCL.
C+
      SUBROUTINE DSA_PREFERRED_TYPE (STRUCT_TYPE,PRIM_TYPE,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) STRUCT_TYPE, PRIM_TYPE
C
C     Functions
C
      INTEGER   ICH_LEN
C
C     DSA system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      CHARACTER BASIC_TYPE*16    ! General type classification
      LOGICAL   KNOWN            ! True if type is known to DSA__TYPE_DETAILS
      INTEGER   LENGTH           ! Number of significant characters in type
      CHARACTER STRUCTURE*16     ! Type for structure - ignored
      CHARACTER VARIANT*16       ! Type variant - ignored
      CHARACTER STRING*80        ! Local string storage
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
      CALL DSA__TYPE_DETAILS (0,STRUCT_TYPE,BASIC_TYPE,VARIANT,
     :                                   PRIM_TYPE,STRUCTURE,KNOWN)
      IF (KNOWN) THEN
C
C        We need to know something about the various structure types.
C        In particular, you don't want to suggest the basic data type
C        (usually SHORT) for dealing with scaled arrays.
C
         IF ((BASIC_TYPE.EQ.'SCALED').OR.(BASIC_TYPE.EQ.'CSTRUCT')) THEN
            PRIM_TYPE='FLOAT'
         END IF
      ELSE
C
C        Type wasn't known.
C
         LENGTH=ICH_LEN(STRUCT_TYPE)
         IF (LENGTH.EQ.0) THEN
            CALL DSA_WRUSER('A blank string')
         ELSE
            STRING='"'//STRUCT_TYPE(:LENGTH)//'"'
            CALL DSA_WRUSER(STRING)
         END IF
         CALL DSA_WRUSER(' is not a type recognised by this program.')
         CALL DSA_WRUSER(' Either there has been a programming error,')
         CALL DSA_WRUSER(' or a file being processed has data'//
     :                                     ' of an unexpected type.')
         CALL DSA_WRFLUSH
         PRIM_TYPE='FLOAT'
         STATUS=DSA__INVTYP
      END IF
C
      END
