C+
C                D S A _ M A T C H _ D I M E N S I O N
C
C  Routine name:
C     DSA_MATCH_DIMENSION
C
C  Function:
C     Checks the main data arrays of two structures match in one dimension.
C
C  Description:
C     This routine checks that one specified dimension of the main
C     array in one structure is the same as a specified dimension of
C     the main data array in another structure.  This is usually called
C     prior to an operation such as a copying of a 1D array into a
C     single cross-section of another array, where it is important that
C     the dimensions match.  If the specified dimensions do not match,
C     an error message is issued and a bad status value is returned.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_MATCH_DIMENSION (REF_NAME,DIM,REF_NAME2,DIM2,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name used to
C                       refer to the first data structure.
C     (>) DIM           (Integer, ref) The dimension of the first data
C                       structure to be tested.
C     (>) REF_NAME2     (Fixed string,descr) The reference name used to
C                       refer to the second data structure.
C     (>) DIM2          (Integer, ref) The dimension of the second data
C                       structure to be tested.
C     (!) STATUS        (Integer,ref) Status value.  If bad status is
C                       passed, this routine returns immediately.
C
C  External subroutines / functions used:
C
C     ICH_LEN, ICH_CI, DSA_DATA_SIZE, DSA_WRUSER, DSA_GET_ACTUAL_NAME
C     GEN_NTH
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
C     GEN_NTH          Returns 'st','nd','rd' as appropriate
C
C  History:
C     25th Jan 1989.  Original version.  KS / AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992   Remove unused variable declarations. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_MATCH_DIMENSION (REF_NAME,DIM,REF_NAME2,DIM2,
     :                                                          STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER DIM, DIM2, STATUS
      CHARACTER*(*) REF_NAME, REF_NAME2
C
C     Functions used
C
      INTEGER ICH_LEN
      CHARACTER*8 ICH_CI
      CHARACTER*2 GEN_NTH
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
      INTEGER   NDIM                       ! First structure, # dimensions
      INTEGER   NDIM2                      ! Second structure, # dimensions
      INTEGER   NELM                       ! First structure, # elements
      INTEGER   NELM2                      ! Second structure, # elements
      CHARACTER NUMBER*8                   ! Used to format numbers
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
C     Compare the two dimensions. First, check that each is valid.
C
      IF (DIM.GT.NDIM) THEN
         CALL DSA_WRUSER ('The main data array in ')
         CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,STATUS)
         CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER (' does not have a ')
         NUMBER=ICH_CI(DIM)
         CALL DSA_WRUSER (NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER (GEN_NTH(DIM)//' dimension.')
         STATUS=DSA__BADDIM
         GO TO 500     ! Error exit
      END IF
      IF (DIM2.GT.NDIM2) THEN
         CALL DSA_WRUSER ('The main data array in ')
         CALL DSA_GET_ACTUAL_NAME (REF_NAME2,STRUCTURE,STATUS)
         CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER (' does not have a ')
         NUMBER=ICH_CI(DIM2)
         CALL DSA_WRUSER (NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER (GEN_NTH(DIM2)//' dimension.')
         STATUS=DSA__BADDIM
         GO TO 500     ! Error exit
      END IF
C
C     Now the actual dimensions
C
      IF (DIMS(DIM).NE.DIMS2(DIM2)) THEN
         CALL DSA_WRUSER ('The main data array in ')
         CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,STATUS)
         CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER (' has a ')
         NUMBER=ICH_CI(DIM)
         CALL DSA_WRUSER (NUMBER(:ICH_LEN(NUMBER))//GEN_NTH(DIM))
         CALL DSA_WRUSER (' dimension of ')
         NUMBER=ICH_CI(DIMS(DIM))
         CALL DSA_WRUSER (NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER(', whereas that in ')
         CALL DSA_GET_ACTUAL_NAME (REF_NAME2,STRUCTURE,STATUS)
         CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER (' has a ')
         NUMBER=ICH_CI(DIM2)
         CALL DSA_WRUSER (NUMBER(:ICH_LEN(NUMBER))//GEN_NTH(DIM2))
         CALL DSA_WRUSER (' dimension of ')
         NUMBER=ICH_CI(DIMS2(DIM2))
         CALL DSA_WRUSER (NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER('.  These are incompatible.')
         CALL DSA_WRFLUSH
         STATUS=DSA__BADDIM
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
