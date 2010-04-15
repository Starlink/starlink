C+
C
C                      D S A _ I M A G I N A R Y _ T Y P E
C
C  Routine name:
C     DSA_IMAGINARY_TYPE
C
C  Function:
C     Returns the type of the imaginary data array.
C
C  Description:
C     This routine returns the DTA_ system type of the imaginary data array
C     of a structure.  If the array does not exist, an error message is
C     output and bad status is returned.  If the data array is a
C     structured type, this is also indicated.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_IMAGINARY_TYPE (REF_NAME,TYPE,STRUCT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (<) TYPE         (Fixed string,descr) The DTA_ system type of the
C                      imaginary data array.
C     (<) STRUCT       (Logical,ref) Set to indicate whether or not the
C                      array is a structured type.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used:
C     Only common variables internal to the DSA system.
C
C  External subroutines / functions used:
C     ICH_CI, ICH_LEN, DSA_FIND_REF, DSA_WRUSER, DSA_GET_ACTUAL_NAME,
C     DSA__IMAGINARY_NAME, DTA_STRUC, DTA_TYVAR
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     data structure must have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (<) DTA_CODE   (Integer) Last DTA system error code.
C
C  Subroutine / function details:
C     ICH_LEN       Position of last non-blank char in string.
C     DSA_FIND_REF  Look up reference name in common tables.
C     DSA_WRUSER    Output a message string to the user.
C     DSA_GET_ACTUAL_NAME  Get the full structure name from a ref_name.
C     DSA__IMAGINARY_NAME  Get the name of the imaginary array in a structure.
C     DTA_STRUC     Determine if a data object is a structure.
C     DTA_TYVAR     Determine the type of a data object.
C
C  History:
C     7th  Dec  1989.   Original version.  KS / AAO.
C     1st  May  1990.   Modified to support different data formats. KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_IMAGINARY_TYPE (REF_NAME,TYPE,STRUCT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL STRUCT
      CHARACTER*(*) REF_NAME,TYPE
      INTEGER STATUS
C
C     Functions used
C
      INTEGER   ICH_LEN
C
C     Local variables
C
      INTEGER   DTA_STATUS                  ! DTA error code.
      CHARACTER ERROR*64                    ! DTA_ error description
      INTEGER   IGNORE                      ! Dummy status argument
      INTEGER   LENGTH                      ! Object name length
      CHARACTER OBJ_NAME*128                ! DTA_ name of data object
      INTEGER   REF_SLOT                    ! Reference table slot # - ignored
      CHARACTER STRUCTURE_NAME*128          ! Full structure name from ref_name
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables and generate the name
C     of the imaginary array.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500   ! Error exit
      CALL DSA__IMAGINARY_NAME (REF_SLOT,OBJ_NAME,LENGTH)
C
C     Get its structure and type information.
C
      CALL DTA_STRUC (OBJ_NAME(:LENGTH),STRUCT,DTA_STATUS)
      CALL DTA_TYVAR (OBJ_NAME(:LENGTH),TYPE,DTA_STATUS)
C
      IF (DTA_STATUS.NE.0) THEN
         CALL DSA_WRUSER(
     :          'Unable to get type of imaginary data array in ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE_NAME,IGNORE)
         CALL DSA_WRUSER(STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
         CALL DSA_WRUSER('. ')
         CALL DTA_ERROR(DTA_STATUS,ERROR)
         CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR))//'.')
         CALL DSA_WRFLUSH
         DTA_CODE=DTA_STATUS
         STATUS=DSA__DTAERR
         GO TO 500
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
