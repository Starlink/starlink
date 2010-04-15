C+
C                    D S A _ A D D _ S T R U C T U R E
C
C  Routine name:
C     DSA_ADD_STRUCTURE
C
C  Function:
C     Adds a defined sub-structure to a structure.
C
C  Description:
C     A structure definition file can equate element identifiers with
C     the name of a specific sub-structure.  Given the name of an
C     already opened structure, this routine will create in that
C     structure the sub-structure associated with such an element
C     identifier.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_ADD_STRUCTURE (REF_NAME,ELEMENT_ID,TYPE_ID,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name
C                       associated with the structure.
C     (>) ELEMENT_ID    (Fixed string,descr) The element identifier
C                       for the sub-structure to be created.
C     (>) TYPE_ID       (Fixed string,descr) The type of structure to
C                       be created.  This is a type identifier as
C                       defined in the structure definition file,
C                       not the specific data system type (although
C                       they may well be the same).
C     (!) STATUS        (Integer,ref) Status value.  If bad status is
C                       passed to this routine, it returns immediately.
C
C  External subroutines / functions used:
C     ICH_LEN, DSA_ELEMENT_NAME, DSA_DEF_STRUCT_TYPE, DTA_CRVAR,
C     DSA_WRUSER, DSA_WRNAME, DTA_ERROR, DSA_BUILD_STRUCTURE,
C     DSA_REF_SLOT
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the DSA_ system.
C     The structure definition file that defines the structure must
C     have been processed by DSA_READ_STRUCT_DEF, and the structure
C     must have been opened.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  External variable details:
C     (<) DTA_CODE   (Integer) Last DTA_ system error code.
C
C  Subroutine / function details:
C     DTA_CRVAR            Create a structure element
C     DTA_ERROR            Get error text for a DTA error code
C     DSA_BUILD_STRUCTURE  Create a structure of a defined type
C     DSA_ELEMENT_NAME     Get DTA name of a defined structure element
C     DSA_DEF_STRUCT_TYPE  Get type of a defined structure
C     DSA_WRUSER           Output message to user
C     DSA_WRNAME           Output file or structure name to user
C     DSA_REF_SLOT         Look up reference name in common tables
C     ICH_LEN              Position of last non-blank char in string
C
C  History:
C     4th Sept 1987   Original version.  KS / AAO.
C     18th March 1988 Comments expanded. KS / AAO.
C     11th Jan   1990 Now passes ref slot to DSA_BUILD_STRUCTURE.  KS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_ADD_STRUCTURE (REF_NAME,ELEMENT_ID,TYPE_ID,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) REF_NAME, ELEMENT_ID, TYPE_ID
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
      INTEGER   DTA_STATUS       ! Status return from DTA_ routines
      CHARACTER ERROR*64         ! Error description from DTA_ code
      CHARACTER OBJECT_NAME*80   ! DTA_ system name of structure to create
      INTEGER   REF_SLOT         ! Common table entry for reference name
      INTEGER   STRUCT_NO        ! Common table entry for structure def
      CHARACTER TYPE*16          ! Structure type
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Get the DSA internal slot number for the reference name
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
C
C     Get DTA_ system name of structure to be created.
C
      CALL DSA_ELEMENT_NAME (REF_NAME,ELEMENT_ID,OBJECT_NAME,STATUS)
C
C     Look up the type identifier in the structure definition tables
C
      CALL DSA_DEF_STRUCT_TYPE (REF_SLOT,TYPE_ID,.TRUE.,TYPE,
     :                                             STRUCT_NO,STATUS)
      IF (STATUS.NE.0) GO TO 500     ! Error exit
C
C     Create the structure
C
      CALL DTA_CRVAR (OBJECT_NAME,TYPE,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
         CALL DSA_WRUSER('Unable to create the structure ')
         CALL DSA_WRNAME(OBJECT_NAME)
         CALL DSA_WRUSER('. ')
         CALL DTA_ERROR(DTA_STATUS,ERROR)
         CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
         CALL DSA_WRUSER('.')
         CALL DSA_WRFLUSH
         DTA_CODE=DTA_STATUS
         STATUS=DSA__DTAERR
         GO TO 500       ! Error exit
      END IF
C
C     Now fill it up
C
      CALL DSA_BUILD_STRUCTURE (OBJECT_NAME,REF_SLOT,STRUCT_NO,STATUS)
C
C     Exit
C
  500 CONTINUE
C
      END
