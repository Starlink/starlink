C+
C                            D S A _ _ A R R A Y
C
C  Routine name:
C     DSA__ARRAY
C
C  Function:
C     Determines if a named object can be treated as an array.
C
C  Description:
C     Given the name of a data object in a structure, this routine
C     determines whether or not DSA_MAP_ARRAY will be able to map it.
C     This routine checks whether the object is primitive (in which
C     case it assumes it can be mapped, or if it is a structure that
C     can be treated as an array (a contracted or scaled structure,
C     for example).
C
C  Language:
C     FORTRAN
C
C  Call:
C     ARRAY = DSA__ARRAY (OBJECT)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) OBJECT      (Fixed string,descr) The DTA system name of the
C                     object that might be an array.
C
C  Returns:
C
C     (<) ARRAY       (Logical, function value) True if the object can
C                     be treated as an array.
C
C  External variables used:  None.
C
C  External subroutines / functions used:  DTA_STRUC, DTA_TYVAR
C
C  Prior requirements:  None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DTA_STRUC      Determine if an object is a structure
C     DTA_TYVAR      Get type of an object
C
C  History:
C     13th Dec 1989.   Original version.  KS / AAO.
C     27th Apr 1990.   Added support for some SGP38 structured types. KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version recognises the recognises the structured type 'CSTRUCT',
C     and the SGP38 array types 'ARRAY' and 'COMPLEX_ARRAY'.
C+
      LOGICAL FUNCTION DSA__ARRAY (OBJECT)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) OBJECT
C
C     Local variables
C
      INTEGER   DTA_STATUS      ! Status returned by DTA routines
      LOGICAL   STRUCT          ! True if object is a structure
      CHARACTER TYPE*16         ! Type of object, if a structure
C
      DSA__ARRAY = .FALSE.
      CALL DTA_STRUC (OBJECT,STRUCT,DTA_STATUS)
      IF (DTA_STATUS.EQ.0) THEN
         IF (.NOT.STRUCT) THEN
            DSA__ARRAY = .TRUE.
         ELSE
            CALL DTA_TYVAR (OBJECT,TYPE,DTA_STATUS)
            IF (DTA_STATUS.EQ.0) THEN
               IF ((TYPE.EQ.'CSTRUCT').OR.(TYPE.EQ.'ARRAY').OR.
     :                 (TYPE.EQ.'COMPLEX_ARRAY')) DSA__ARRAY = .TRUE.
            END IF
         END IF
      END IF
C
      END
