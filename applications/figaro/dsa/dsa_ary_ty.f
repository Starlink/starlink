C+
C                        D S A _ A R R A Y _ T Y P E
C
C  Routine name:
C     DSA_ARRAY_TYPE
C
C  Function:
C     Returns the type of a named primitive or structured array.
C
C  Description:
C     The type of a primitive array is a simple concept. The type of a
C     structured array is trickier.  This routine is called with the name
C     (the DTA name) of a data object and it returns a string that will
C     summarise its type and a flag that indicates if the object is
C     a structure.  If it is primitive, the type will simply be 'FLOAT'
C     or 'INT' or whatever.  If it is structured, the type string returned
C     will contain the necessary information, but unfortunately in a form
C     that may be hard to interpret.  In fact, the format is best regarded
C     as an internal format that should only be handled by routines such
C     as DSA_PREFERRED_TYPE.  (It is actually an `extended type' that is
C     either a primitive type or a structure type followed by an optional
C     slash and a primitive type, as in `COMPLEX/DOUBLE').
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_ARRAY_TYPE (NAME,DESCRIP,TYPE,STRUCT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NAME       (Fixed string,descr) The DTA system name of the object
C                    in question.  The object should exist.
C     (>) DESCRIP    (Fixed string,descr) A string describing the object.
C                    Typically something like 'quality array'.  This will
C                    be embedded in an error message should there be a
C                    problem with getting the type.
C     (<) TYPE       (Fixed string,descr) The type of the data array.  If
C                    STRUCT is returned false, this will be a primitive data
C                    type.
C     (<) STRUCT     (Logical,ref) Returned true if the array is a structured
C                    type, false if it is primitive.
C     (!) STATUS     (Integer,ref) Status code.  If bad status is passed to it
C                    this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:
C     ICH_LEN, DTA_STRUC, DTA_TYVAR
C
C  Prior requirements:
C     The object should exist, and its file should have been opened.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_LEN       Position of last non-blank char in string.
C     DSA_WRUSER    Output a message string to the user.
C     DTA_STRUC     Determine if a data object is a structure.
C     DTA_TYVAR     Determine the type of a data object.
C
C  Common variable details:
C     (<) DTA_CODE   (Integer) Last DTA system error code.
C
C  History:
C     22nd Feb 1990.   Original version.  KS / AAO.
C     3rd  May 1990.   Now returns a proper extended type specification
C                      instead of just the actual type of a structure.
C                      KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_ARRAY_TYPE (NAME,DESCRIP,TYPE,STRUCT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL STRUCT
      INTEGER STATUS
      CHARACTER*(*) NAME, DESCRIP, TYPE
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      CHARACTER ARRAY_NAME*80     ! Name of actual data array
      CHARACTER ARRAY_TYPE*16     ! Type of primitive array
      INTEGER   DTA_STATUS        ! DTA status code
      CHARACTER ERROR*64          ! DTA error description
      LOGICAL   KNOWN             ! True if structure is of a known type
      INTEGER   LENAME            ! Number of characters in ARRAY_NAME
      CHARACTER VARIANT*16        ! Structure variant
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Get the type of the object and see if it is a structure.  Note
C     that this implementation just returns the object type and doesn't
C     attempt to process it even for a structure.
C
      CALL DTA_STRUC (NAME,STRUCT,DTA_STATUS)
      CALL DTA_TYVAR (NAME,TYPE,DTA_STATUS)
      IF (DTA_STATUS.NE.0) GO TO 500    ! Error exit
C
      IF (STRUCT) THEN
         CALL DSA__STRUCT_ARRAY (NAME,ICH_LEN(NAME),TYPE,VARIANT,
     :                                 ARRAY_NAME,LENAME,KNOWN,STATUS)
         IF (KNOWN) THEN
            CALL DTA_TYVAR (ARRAY_NAME,ARRAY_TYPE,DTA_STATUS)
            IF (DTA_STATUS.NE.0) GO TO 500     ! Error exit
            IF (TYPE.EQ.'COMPLEX_ARRAY') THEN
               TYPE='COMPLEX'//'/'//ARRAY_TYPE
            ELSE
               TYPE=VARIANT(:ICH_LEN(VARIANT))//'/'//ARRAY_TYPE
            END IF
         ELSE
            STATUS=DSA__INVTYP
         END IF
      END IF
C
C     Exit.  On way out, if STATUS or DTA_STATUS is bad, put out an
C     error message.  (STATUS is only bad for an unknown structured type.
C
  500 CONTINUE
      IF ((STATUS.NE.0).OR.(DTA_STATUS.NE.0)) THEN
         CALL DSA_WRUSER(
     :          'Unable to get an array type for ')
         CALL DSA_WRNAME (NAME)
         CALL DSA_WRUSER ('. This should be a ')
         CALL DSA_WRUSER (DESCRIP(:ICH_LEN(DESCRIP)))
         IF (STATUS.NE.0) THEN
            CALL DSA_WRUSER (
     :         '. The data is a structured array of type "')
            CALL DSA_WRUSER (TYPE(:ICH_LEN(TYPE)))
            CALL DSA_WRUSER ('" which this system does not support.')
            CALL DSA_WRFLUSH
         ELSE
            CALL DSA_WRUSER ('. ')
            CALL DTA_ERROR (DTA_STATUS,ERROR)
            CALL DSA_WRUSER (ERROR(:ICH_LEN(ERROR))//'.')
            CALL DSA_WRFLUSH
            DTA_CODE=DTA_STATUS
            STATUS=DSA__DTAERR
         END IF
      END IF
C
      END
