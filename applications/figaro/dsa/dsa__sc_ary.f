C+
C                        D S A _ _ S T R U C T _ A R R A Y
C
C  Routine name:
C     DSA__STRUCT_ARRAY
C
C  Function:
C     Checks for an array that might be a structured array.
C
C  Description:
C     This routine looks at a structured array to see if it appears
C     to be one of the structured array types supported by DSA. (These
C     are the original Figaro CSTRUCT type and the ARRAY and COMPLEX_ARRAY
C     types defined by the Starlink SGP38 standard).  If this is the case,
C     it looks to see if there is a variant defined (which SGP38 uses
C     to give more detail about the structure), and also determines the
C     name of the data array it contains.  (If this is a complex array
C     type, it returns the name of the real data array.)  If no variant
C     is specified, this routine returns the default value 'SIMPLE'. The
C     CSTRUCT array type, for convenience, is treated as having the
C     'SCALED' variant value, although it in fact has no such thing.
C     This routine does not output any error messages - the only way it
C     can fail is if it fails to read the VARIANT, and if it does then it
C     simply assumes SIMPLE.  If the array is not one of the structured
C     types we know about, this routine returns the structure name as
C     the name of the array, and sets the KNOWN flag to indicate that
C     this was not something we can handle.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__STRUCT_ARRAY (NAME,LENGTH,TYPE,VARIANT,ARRAY_NAME,
C                                               LENAME,KNOWN,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NAME        (Fixed string,descr) The name of the structure
C                     that is supposed to be an SGP38 array type.
C     (>) LENGTH      (Integer,ref) The number of significant characters
C                     in NAME.
C     (>) TYPE        (Fixed string,descr) The type of the structure.
C                     Must be in upper case.
C     (<) VARIANT     (Fixed string,descr) The variant describing the
C                     type of structured array.  If no variant is specified
C                     in the structure, 'SIMPLE' is returned.
C     (<) ARRAY_NAME  (Fixed string,descr) The name of the actual data array
C                     held in the structure.  If the structure holds complex
C                     data, this is the name of the real component.
C     (<) LENAME      (Integer,ref) The number of characters in ARRAY_NAME.
C     (<) KNOWN       (Logical,ref) True if the structure type is one
C                     that we know about.
C     (!) STATUS      (Integer,ref) Status code.  If non-zero status is
C                     passed to it, this routine returns immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     DTA_RDVARC
C
C  Prior requirements:
C     The structure in question should have been opened, and the structure
C     specified in NAME is assumed to exist.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DTA_RDVARC    Read the value of a character data object.
C
C  History:
C     15th Apr 1990.   Original version.  KS / AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     25th Jul 1996    Catenations for Linux.  MJCL/Starlink, UCL.
C+
      SUBROUTINE DSA__STRUCT_ARRAY (NAME,LENGTH,TYPE,VARIANT,ARRAY_NAME,
     :                                              LENAME,KNOWN,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL KNOWN
      INTEGER LENGTH, LENAME, STATUS
      CHARACTER*(*) NAME, TYPE, VARIANT, ARRAY_NAME
C
C     Local variables
C
      INTEGER DTA_STATUS            ! Status returned by call to DTA routine
      CHARACTER STRING*80           ! Local string storage
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Treat the CSTRUCT type as if it were an SGP38 SCALED array (upper
C     levels can still distinguish them by looking at TYPE).
C
      KNOWN=.TRUE.
      IF (TYPE.EQ.'CSTRUCT') THEN
         ARRAY_NAME=NAME(:LENGTH)//'.DATA'
         LENAME=LENGTH+5
         VARIANT='SCALED'
      ELSE
C
C        Check for the two SGP38 types we know about.  These do have
C        variants, so we can try to read the variant object.
C
         IF ((TYPE.EQ.'COMPLEX_ARRAY').OR.(TYPE.EQ.'ARRAY')) THEN
            IF (TYPE(1:1).EQ.'C') THEN
               ARRAY_NAME=NAME(:LENGTH)//'.REAL'
            ELSE
               ARRAY_NAME=NAME(:LENGTH)//'.DATA'
            END IF
            LENAME=LENGTH+5
C
C           See if there is a variant specified
C
            STRING=NAME(:LENGTH)//'.VARIANT'
            CALL DTA_RDVARC (STRING,LEN(VARIANT),VARIANT,DTA_STATUS)
            IF (DTA_STATUS.NE.0) VARIANT='SIMPLE'
         ELSE
C
C           We don't know this type, so treat it as an actual array
C           and set the variant to SIMPLE.
C
            ARRAY_NAME=NAME
            LENAME=LENGTH
            VARIANT='SIMPLE'
            KNOWN=.FALSE.
         END IF
      END IF
C
      END
