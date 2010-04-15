C+
      SUBROUTINE DSA_MATCH_AXES (REF_NAME,REF_NAME2,STATUS)
C
C                     D S A _ M A T C H _ A X E S
C
C  Routine name:
C     DSA_MATCH_AXES
C
C  Function:
C     Checks that the axis information for two structures matches.
C
C  Description:
C     This routine checks that the axis information for two structures
C     matches.  It checks that a) the two structures have the same
C     number of axes, b) that the units, if any are given, are the same
C     for each axis, c) that the axis data, if it exists, is the same
C     for each axis.  If there are any discrepancies, an error message
C     is output, and bad status is returned.  This routine is intended
C     for use where no further processing is to be performed on the
C     axis data, but where the user should be warned of any possible
C     incompatibility.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_MATCH_AXES (REF_NAME,REF_NAME2,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name used to
C                       refer to the first data structure.
C     (>) REF_NAME2     (Fixed string,descr) The reference name used to
C                       refer to the second data structure.
C     (!) STATUS        (Integer,ref) Status value.  If bad status is
C                       passed, this routine returns immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     ICH_LEN, ICH_CI, DSA_DATA_SIZE, DSA_WRUSER, DSA_GET_ACTUAL_NAME,
C     DSA_MATCH_AXIS
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.  Both
C     structures should have been opened by, for example, DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variables used:
C     (>) MAX_AXES   (Integer parameter) Maximum number of axes
C
C  Subroutine / function details:
C     ICH_LEN        Position of last non-blank char in string
C     ICH_CI         Formats an integer into a string
C     DSA_DATA_SIZE  Get dimensions of main data array
C     DSA_WRUSER     Output message to user
C     DSA_MATCH_AXIS Compare data for a single axis
C     DSA_GET_ACTUAL_NAME  Get full structure name
C
C  History:
C     15th July 1987   Original version.  KS / AAO.
C     15th Feb  1991   Missing \N in DSA_WRUSER call fixed. KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) REF_NAME, REF_NAME2
C
C     Functions used
C
      INTEGER ICH_LEN
      CHARACTER*8 ICH_CI
C
C     DSA_ system common.  Defines MAX_AXES
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ system error codes.
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      INTEGER   AXIS_STATUS                ! Status from DSA_MATCH_AXIS
      INTEGER   DIMS(MAX_AXES)             ! Dimensions of first structure
      INTEGER   DIMS2(MAX_AXES)            ! Dimensions of second structure
      INTEGER   I                          ! Loop index
      INTEGER   NDIM                       ! First structure, # dimensions
      INTEGER   NDIM2                      ! Second structure, # dimensions
      INTEGER   NELM                       ! First structure, # elements
      INTEGER   NELM2                      ! Second structure, # elements
      CHARACTER NUMBER*8                   ! Used to format numbers
      LOGICAL   OK                         ! Used to flag mismatches
      CHARACTER STRUCTURE*128              ! Full structure name
C
C     Return immediately if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     Get dimensions of both arrays.
C
      CALL DSA_DATA_SIZE (REF_NAME,MAX_AXES,NDIM,DIMS,NELM,STATUS)
      CALL DSA_DATA_SIZE (REF_NAME2,MAX_AXES,NDIM2,DIMS2,NELM2,STATUS)
      IF (STATUS.NE.0) GO TO 500       ! Error exit
C
C     Compare them.  First, the number of dimensions.  We make the
C     same check for them bieng the same as does DSA_MATCH_SIZES,
C     allowing the case where extra axes of dimension 1 appear, but
C     we keep quiet in this case (DSA_MATCH_SIZES is usually called
C     before this routine, and it will have complained once already
C     in this case).
C
      IF (NDIM2.NE.NDIM) THEN
         OK=.TRUE.
         IF (NDIM.GT.NDIM2) THEN
            DO I=NDIM2+1,NDIM
               IF (DIMS(I).NE.1) OK=.FALSE.
            END DO
         ELSE
            DO I=NDIM+1,NDIM2
               IF (DIMS2(I).NE.1) OK=.FALSE.
            END DO
         END IF
         IF (.NOT.OK) THEN
            CALL DSA_WRUSER ('The main data array in ')
            CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,STATUS)
            CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
            CALL DSA_WRUSER (' is ')
            NUMBER=ICH_CI(NDIM)
            CALL DSA_WRUSER (NUMBER(:ICH_LEN(NUMBER)))
            CALL DSA_WRUSER ('-dimensional, whereas that in ')
            CALL DSA_GET_ACTUAL_NAME (REF_NAME2,STRUCTURE,STATUS)
            CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
            CALL DSA_WRUSER (' is ')
            NUMBER=ICH_CI(NDIM2)
            CALL DSA_WRUSER (NUMBER(:ICH_LEN(NUMBER)))
            CALL DSA_WRUSER ('-dimensional. ')
            CALL DSA_WRFLUSH
            STATUS=DSA__BADDIM
            GO TO 500                   ! Error exit.
         END IF
      END IF
C
C     Now check each axis in turn.
C
      DO I=1,NDIM
         AXIS_STATUS=0
         CALL DSA_MATCH_AXIS (REF_NAME,I,REF_NAME2,I,AXIS_STATUS)
         IF (AXIS_STATUS.NE.0) STATUS=AXIS_STATUS
      END DO
C
C     Exit
C
  500 CONTINUE
C
      END
