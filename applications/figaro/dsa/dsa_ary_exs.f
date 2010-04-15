C+
C                         D S A _ A R R A Y _ E X I S T
C
C  Routine name:
C     DSA_ARRAY_EXIST
C
C  Function:
C     Determines whether or not a named data array exists.
C
C  Description:
C     Given the DTA_ system name of a data array, this routine
C     determines whether or not such a named object exists in a
C     form that can supply a data array.  That is, that the object
C     is either itself a data array or a structure defining such
C     an array.  At present, it can only handle a primitive data
C     array and a limited variety of structured arrays.  This routine
C     outputs no error messages if the array does not exist, but if
C     it exists but not in a form we can handle, then it complains.
C     Most routines that access arrays call this pretty early in the
C     processing, so this is a good place to locate the structures
C     that are going to cause trouble.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_ARRAY_EXIST (NAME,EXIST,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NAME       (Fixed string,descr) The DTA_ system name of the
C                    array in question.
C     (<) EXIST      (Logical,ref) True if a suitable data array does
C                    in fact exist.
C     (!) STATUS     (Integer,ref) Returned status value.  If a non-zero
C                    status value is passed, this routine returns immediately.
C
C  External variables used -  None.
C
C  External subroutines / functions used:
C     DTA_TYVAR, DTA_STRUC, DSA__STRUCT_ARRAY, DSA_WRUSER, DSA_WRNAME
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C
C  Note:
C     This is an DSA_ system internal routine, and should not be
C     called directly from outside the DSA_ package.
C-
C  Subroutine / function details:
C     DTA_STRUC    Determines if a data object is a structure.
C     DTA_TYVAR    Returns the type of a data object.
C     DSA__STRUCT_ARRAY Checks on what might be a structured array.
C     DSA_WRUSER   Outputs a message to the user.
C     DSA_WRNAME   Outputs the name of a data object to the user.
C
C  History:
C     22nd June 1987    Original version.  KS / AAO.
C     15th Apr  1990    Now uses DSA__STRUCT_ARRAY, and checks that we can
C                       handle the structure. KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C     20th Jul 1995     Make ARRAY_NAME longer. These days arrays may
C                       lie deeper in an NDF than you thought five
C                       years ago.
C+
      SUBROUTINE DSA_ARRAY_EXIST (NAME,EXIST,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL EXIST
      INTEGER STATUS
      CHARACTER*(*) NAME
C
C     Functions
C
      INTEGER ICH_LEN
C
C     DSA error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      CHARACTER ARRAY_NAME*80              ! Actual array name
      INTEGER   DTA_STATUS                 ! Status returned by DTA_ routines
      LOGICAL   KNOWN                      ! True if this is a known structure
      INTEGER   LENAME                     ! Length of ARRAY_NAME
      LOGICAL   STRUCT                     ! True if named object a structure
      CHARACTER TYPE*16                    ! Type of named object
      CHARACTER VARIANT*16                 ! Structure variant
C
C     If bad status passed, return now.
C
      IF (STATUS.NE.0) RETURN
C
C     See if the object exists, and if it is primitive or not
C
      CALL DTA_STRUC (NAME,STRUCT,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
         EXIST=.FALSE.
      ELSE
         IF (.NOT.STRUCT) THEN
            EXIST=.TRUE.
         ELSE
            TYPE=' '
            CALL DTA_TYVAR (NAME,TYPE,DTA_STATUS)
            CALL DSA__STRUCT_ARRAY (NAME,ICH_LEN(NAME),TYPE,VARIANT,
     :                                ARRAY_NAME,LENAME,KNOWN,STATUS)
            IF (.NOT.KNOWN) THEN
               CALL DSA_WRUSER ('The structure ')
               CALL DSA_WRNAME (NAME)
               CALL DSA_WRUSER (' is of a type ("')
               CALL DSA_WRUSER (TYPE(:ICH_LEN(TYPE)))
               CALL DSA_WRUSER ('") that this routine cannot handle.')
               CALL DSA_WRFLUSH
               STATUS=DSA__INVTYP
               EXIST=.FALSE.
            ELSE
               IF ((VARIANT.EQ.'SCALED').OR.(VARIANT.EQ.'SIMPLE')) THEN
                  CALL DTA_TYVAR (ARRAY_NAME(:LENAME),TYPE,DTA_STATUS)
                  IF (DTA_STATUS.NE.0) THEN
                     CALL DSA_WRNAME (NAME)
                     CALL DSA_WRUSER (' is a structure that should '//
     :                       'contain a data array, but does not.')
                     CALL DSA_WRFLUSH
                     STATUS=DSA__INVTYP
                     EXIST=.FALSE.
                  ELSE
                     EXIST=.TRUE.
                  END IF
               ELSE
                  CALL DSA_WRUSER ('The structure ')
                  CALL DSA_WRNAME (NAME)
                  CALL DSA_WRUSER (' is of a known type ("')
                  CALL DSA_WRUSER (TYPE(:ICH_LEN(TYPE)))
                  CALL DSA_WRUSER (
     :                         '"), but is a variant of that type ("')
                  CALL DSA_WRUSER (VARIANT(:ICH_LEN(VARIANT)))
                  CALL DSA_WRUSER (
     :                       '") that this routine cannot handle.')
                  CALL DSA_WRFLUSH
                  STATUS=DSA__INVTYP
                  EXIST=.FALSE.
               END IF
            END IF
         END IF
      END IF
C
      END
