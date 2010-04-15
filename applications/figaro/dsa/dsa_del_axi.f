C+
C                        D S A _ D E L E T E _ A X I S
C
C  Routine name:
C     DSA_DELETE_AXIS
C
C  Function:
C     Deletes a specified axis structure.
C
C  Description:
C     This routine completely removes the information for a specified
C     axis from a data structure.  If the structure already contains
C     no information for this axis, then this routine does nothing.
C     None of the arrays for this axis should be mapped when this
C     routine is called.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_DELETE_AXIS (REF_NAME,AXIS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (>) AXIS         (Integer,ref) The number of the axis in question.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used:
C     Only common variables internal to the DSA system.
C
C  External subroutines / functions used:
C     ICH_LEN, ICH_FOLD, ICH_CI, GEN_NTH, DSA_VALIDATE_AXIS, DSA_FIND_REF,
C     DSA__AXIS_NAME, DSA_CHECK_MAPPING, DSA__SAVE_AXIS, DSA_WRUSER,
C     DSA_WRNAME, DTA_ERROR, DTA_TYVAR
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the data
C     structure in question should have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_LEN            Position of last non-blank character in string
C     ICH_FOLD           Fold a string into upper case
C     ICH_CI             Returns an integer formatted into a string
C     GEN_NTH            Returns 'st', 'nd', 'rd', 'th' etc
C     DTA_ERROR          Convert DTA code into a character string
C     DTA_TYVAR          Get type of data object
C     DSA_VALIDATE_AXIS  Check that axis number is valid
C     DSA_FIND_REF       Look up reference name in common tables
C     DSA__AXIS_NAME     Get DTA name for axis structure
C     DSA__DELETE_AXIS   Delete an axis structure
C     DSA_WRUSER         Output a message to the user
C     DSA_WRNAME         Output a DTA object name to the user
C
C  Common variable details:
C     (<) AXIS_EXIST   (Integer array) State of knowledge about axis arrays.
C                      Indicates unknown (0), known not to exist (-1),
C                      exists and is 1d (1), exists and is multi-
C                      dimensional (2).  (2D array)
C     (<) AXIS_RESHAPE (Logical array) Indicates that the axis data structures
C                      were reshaped.
C     (<) AXIS_UPDATE  (Logical array) Indicates that an axis data array
C                      has been updated (or at least, mapped for update).
C     (<) WIDTH_UPDATE (Logical array) Indicates that an axis width array
C                      has been updated (or at least, mapped for update).
C     (<) DTA_CODE     (Integer) Last DTA_ system failure status code.
C
C  History:
C     14th Dec  1989.   Original version.  KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992     Remove unused variable declarations. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_DELETE_AXIS (REF_NAME,AXIS,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER AXIS, STATUS
      CHARACTER*(*) REF_NAME
C
C     Functions used
C
      INTEGER   ICH_FOLD, ICH_LEN
      CHARACTER ICH_CI*1, GEN_NTH*2
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      INTEGER   DTA_STATUS                  ! Status code from DTA_ routines
      CHARACTER ERROR*64                    ! DTA_ error description
      INTEGER   INVOKE                      ! Dummy function return value
      INTEGER   LENGTH                      ! Object name length
      CHARACTER OBJ_NAME*64                 ! DTA_ name of data object
      CHARACTER REF_NAME_UC*32              ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                    ! Reference table slot #
      CHARACTER TYPE*8                      ! Type of axis structure - ignored
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
      IF (STATUS.NE.0) GO TO 500             ! Error exit
C
C     Make sure AXIS is valid.
C
      CALL DSA_VALIDATE_AXIS (AXIS,REF_NAME_UC,STATUS)
C
C     Get the name of the axis structure itself in OBJ_NAME(:LENGTH)
C     and see if it exists - by trying to get its type.
C
      CALL DSA__AXIS_NAME (REF_SLOT,AXIS,OBJ_NAME,LENGTH)
      CALL DTA_TYVAR (OBJ_NAME(:LENGTH),TYPE,DTA_STATUS)
      IF (DTA_STATUS.EQ.0) THEN
C
C        It does exist, so we get to delete it.  First, however,
C        some of the axis arrays may be mapped, in which case we have to
C        see if they're still actively mapped and close them down if
C        not.
C
         CALL DSA_CHECK_MAPPING (OBJ_NAME,LENGTH,
     :                           'delete the axis structure ',STATUS)
         IF (STATUS.NE.0) GO TO 500    ! Error exit
C
C        Now delete the axis data
C
         CALL DSA__DELETE_AXIS (REF_SLOT,AXIS,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_WRUSER ('Error attempting to delete the '//
     :             ICH_CI(AXIS)//GEN_NTH(AXIS)//' axis information, ')
            CALL DSA_WRNAME (OBJ_NAME)
            CALL DSA_WRUSER ('. ')
            CALL DTA_ERROR (DTA_STATUS,ERROR)
            CALL DSA_WRUSER (ERROR(:ICH_LEN(ERROR)))
            CALL DSA_WRUSER ('.')
            CALL DSA_WRFLUSH
            DTA_CODE=DTA_STATUS
            STATUS=DSA__DTAERR
            GO TO 500     ! Error exit
         END IF
C
C        Clear out any common information releating to the axis.
C
         AXIS_EXIST(AXIS,REF_SLOT) = 0
         AXIS_RESHAPE(AXIS,REF_SLOT) = .FALSE.
         AXIS_UPDATE(AXIS,REF_SLOT) = .FALSE.
         WIDTH_UPDATE(AXIS,REF_SLOT) = .FALSE.
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
