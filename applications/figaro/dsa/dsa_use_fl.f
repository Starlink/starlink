C+
C              D S A _ U S E _ F L A G G E D _ V A L U E S
C
C  Routine name:
C     DSA_USE_FLAGGED_VALUES
C
C  Function:
C     Indicates that the calling routine will accept flagged values.
C
C  Description:
C     An application uses this routine to indicate that it expects flagged
C     (`bad' values, or `magic numbers') in the main data array from a
C     specified structure.  `Expects' here merely means that it is
C     prepared to process such values should there be any in the data,
C     and it is not necessary to protect it by removing any such values.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_USE_FLAGGED_VALUES (REF_NAME,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME       (Fixed string,descr) The reference name associated
C                        with the structure in question.
C     (!) STATUS         (Integer,ref) Status value.  If bad status is
C                        passed to it, this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA routines
C
C  External subroutines / functions used:
C     ICH_FOLD, DSA_FIND_REF, DSA_WRUSER, DSA_WRNAME
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     structure in question must have been opened.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 17th February 1995
C-
C  Subroutine / function details:
C     ICH_FOLD        Convert string to upper case
C     DSA_FIND_REF    Look up reference name in common tables
C     DSA_QF_CHECK    Checks program is handling data quality safely
C     DSA_WRUSER      Output string to user
C     DSA_WRNAME      Output DTA_ object name to user
C
C  Common variable details:
C     (<) USE_FLAGS   (Logical array) Indicates application accepts flagged
C                     data values.
C     (>) USE_QUALITY (Logical array) Indicates application will use a data
C                     quality array.
C     (!) QF_HANDLING (Integer array) Flags that record the use the program
C                     is making of the quality and flag information.
C     (>) QF_USE_FLAGS(Integer parameter) Indicates program has
C                     made a call to DSA_USE_FLAGGED_VALUES.
C  History:
C     14th July 1988   Original version.  KS / AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     6th  Feb 1995    Now allows both this and DSA_USE_QUALITY to be
C                      called for the same file. KS/AAO.
C     17th Feb 1995    Now sets and checks QF_HANDLING. KS/AAO.
C+
      SUBROUTINE DSA_USE_FLAGGED_VALUES (REF_NAME,STATUS)
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
      INTEGER ICH_FOLD
C
C     Local variables
C
      INTEGER   LENGTH                      ! Object name length
      CHARACTER OBJ_NAME*128                ! DTA_ name of data object
      INTEGER   REFLEN                      ! Length of REF_NAME_UC
      CHARACTER REF_NAME_UC*32              ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                    ! Reference table slot #
C
C     DSA system common
C
      INCLUDE 'DSA_COMMON'
C
C     DSA system errors
C
      INCLUDE 'DSA_ERRORS'
C
C     If bad status passed, return immediately
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables.
C
      REF_NAME_UC=REF_NAME
      REFLEN=ICH_FOLD(REF_NAME_UC)
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
C
C     If we found it, set the appropriate common flags
C
      IF (STATUS.EQ.0) THEN
         USE_FLAGS(REF_SLOT)=.TRUE.
         QF_HANDLING(REF_SLOT)=QF_HANDLING(REF_SLOT)+QF_USE_FLAGS
         CALL DSA_QF_CHECK(REF_SLOT,STATUS)
      END IF
C
      END
