C+
      SUBROUTINE DSA_MATCH_SIZES (REF_NAME,REF_NAME2,STATUS)
C
C                     D S A _ M A T C H _ S I Z E S
C
C  Routine name:
C     DSA_MATCH_SIZES
C
C  Function:
C     Checks that the main data arrays of two structures match in size.
C
C  Description:
C     This routine is usually called prior to an operation performed
C     pixel-to-pixel on the main data arrays of two structures.  It
C     checks that they have the same dimensions, issuing an error
C     message and returning a bad status value if they do not match.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_MATCH_SIZES (REF_NAME,REF_NAME2,STATUS)
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
C  External subroutines / functions used:
C
C     ICH_LEN, ICH_CI, DSA_DATA_SIZE, DSA_WRUSER, DSA_GET_ACTUAL_NAME
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.  Both
C     structures should have been opened by, for example, DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_LEN          Position of last non blank char in string
C     ICH_CI           Format integer into string
C     DSA_DATA_SIZE    Get size of data array
C     DSA_GET_ACTUAL_NAME  Get full name from a reference name
C     DSA_WRUSER       Output to user
C
C  History:
C     15th July 1987   Original version.  KS / AAO.
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
C     Compare them.  First, the number of dimensions.  Note that we
C     allow additional dimensions, so long as they are 1.  (Some
C     data, particularly radio data in FITS format, is produced
C     with dimensions (N,1,1,1), for example!)
C
      IF (NDIM2.NE.NDIM) THEN
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
         IF (OK) THEN
            CALL DSA_WRUSER('However, all the extra dimensions are 1,')
            CALL DSA_WRUSER(' so this discrepancy can be ignored.')
            CALL DSA_WRFLUSH
         ELSE
            CALL DSA_WRUSER('They are incompatible.')
            CALL DSA_WRFLUSH
            STATUS=DSA__BADDIM
            GO TO 500               ! Error exit.
         END IF
      END IF
C
C     Now the actual dimensions
C
      OK=.TRUE.
      DO I=1,NDIM
         IF (DIMS(I).NE.DIMS2(I)) OK=.FALSE.
      END DO
      IF (.NOT.OK) THEN
         CALL DSA_WRUSER ('The main data array in ')
         CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,STATUS)
         CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER (' has dimensions (')
         DO I=1,NDIM
            NUMBER=ICH_CI(DIMS(I))
            IF (I.GT.1) CALL DSA_WRUSER(',')
            CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
         END DO
         CALL DSA_WRUSER('), whereas that in ')
         CALL DSA_GET_ACTUAL_NAME (REF_NAME2,STRUCTURE,STATUS)
         CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER (' has dimensions (')
         DO I=1,NDIM2
            NUMBER=ICH_CI(DIMS2(I))
            IF (I.GT.1) CALL DSA_WRUSER(',')
            CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
         END DO
         CALL DSA_WRUSER(').  These are incompatible.')
         CALL DSA_WRFLUSH
         STATUS=DSA__BADDIM
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
