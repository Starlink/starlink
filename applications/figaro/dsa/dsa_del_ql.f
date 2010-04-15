C+
C                     D S A _ D E L E T E _ Q U A L I T Y
C
C  Routine name:
C     DSA_DELETE_QUALITY
C
C  Function:
C     Deletes a quality information structure.
C
C  Description:
C     This routine completely removes the quality information structure
C     (which may be a simple array or a structure, depending on the data
C     data format) from a specified data structure. If the structure
C     does not contain a quality information structure, then this
C     routine does nothing. The quality array should not be mapped
C     when this routine is called, and nor should the main data array.
C     Note that this routine does no processing of the data - it merely
C     deletes the structure.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_DELETE_QUALITY (REF_NAME,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used:
C     Only common variables internal to the DSA system.
C
C  External subroutines / functions used:
C     ICH_FOLD, DSA_FIND_REF, DSA_CHECK_MAPPING,
C     DSA__DELETE_QUALITY, DSA_WRUSER,
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the data
C     structure in question should have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 12th February 1995
C-
C  Subroutine / function details:
C     ICH_FOLD             Fold a string into upper case
C     DSA_FIND_REF         Look up reference name in common tables
C     DSA_CHECK_MAPPING    Make sure nothing in a structure is mapped
C     DSA__DELETE_QUALITY  Delete an axis structure
C     DSA_WRUSER           Output a message to the user
C
C  Common variable details:
C     (<) QUAL_EXIST   (Integer array) State of knowledge about quality.
C                      Indicates unknown (0), known not to exist (-1),
C                      known to exist (1)
C
C  History:
C     12th Feb 1995.   Original version.  KS / AAO.
C+
      SUBROUTINE DSA_DELETE_QUALITY (REF_NAME,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) REF_NAME
C
C     Functions used
C
      INTEGER   ICH_FOLD
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   INVOKE                      ! Dummy function return value
      INTEGER   LENGTH                      ! Object name length
      CHARACTER OBJ_NAME*64                 ! DTA_ name of data object
      CHARACTER REF_NAME_UC*32              ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                    ! Reference table slot #
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     We need an upper case version of REF_NAME
C
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
C
C     Look up the reference name in the tables.
C
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
C
C     Make sure the various arrays are not still mapped.
C
      CALL DSA_CHECK_MAPPING (OBJ_NAME,LENGTH,
     :              'delete the quality information for ',STATUS)
C
C     Let DSA__DELETE_QUALITY do the real work, which is data format
C     dependent.
C
      CALL DSA__DELETE_QUALITY (REF_SLOT,STATUS)
C
C     Clear out any common information releating to the quality structure.
C
      IF (STATUS.EQ.0) THEN
         QUAL_EXIST(REF_SLOT) = -1
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
