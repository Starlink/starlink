C+
C                   D S A _ E R R O R _ I N F O R M A T I O N
C
C  Routine name:
C     DSA_ERROR_INFORMATION
C
C  Function:
C     Determines just what form of error information is available.
C
C  Description:
C     This routine returns three logical variables that indicate if
C     error information for a structure is held as a) an error array,
C     giving the uncertainties in the data for each point, b) a variance
C     array, giving the variance in the data for each point, or c) not
C     held at all.  This gives more precise information than do the
C     routines DSA_SEEK_ERRORS and DSA_SEEK_VARIANCE, which really only
C     let the caller know if error information is available at all.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_ERROR_INFORMATION (REF_NAME,ERRORS,VARIANCE,NONE,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME          (Fixed string,descr) The reference name used
C                           to identify the structure.
C     (<) ERRORS            (Logical,ref) True if the error information
C                           is held in an uncertainty array.
C     (<) VARIANCE          (Logical,ref) True if the error information
C                           is held in a variance array.
C     (<) NONE              (Logical,ref) True if no error information
C                           is held at all.
C     (!) STATUS            (Integer,ref) Status code. If bad status is
C                           passed, this routine returns immediately.
C
C  External variables used:
C     Only common parameters internal to the DSA routines.
C
C  External subroutines / functions used:
C     DSA_SEEK_ERRORS
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     structure in question must have been already opened.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (>) VARIANCE_CODE (Integer parameter) Error type is `variance'
C
C  Subroutine / function details:
C     DSA_SEEK_ERRORS   Indicates if error information is present.
C
C  History:
C     7th Dec 1989.   Original version.  KS / AAO.
C     12th Mar 1990.  Modified to allow for data formats that have error
C                     information in variance arrays.  KS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_ERROR_INFORMATION (REF_NAME,ERRORS,VARIANCE,
     :                                                      NONE,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL ERRORS, VARIANCE, NONE
      INTEGER STATUS
      CHARACTER*(*) REF_NAME
C
C     Local variables
C
      INTEGER   ERR_CODE         ! Error type code
      LOGICAL   EXIST            ! true if any error information is present.
      INTEGER   LENGTH           ! Number of chars in NAME - ignored
      CHARACTER NAME*32          ! Name of error or variance array - ignored
      INTEGER   REF_SLOT         ! Entry number in common arrays for REF_NAME
C
C     DSA common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Start by assuming no error info at all.
C
      VARIANCE=.FALSE.
      ERRORS=.FALSE.
      NONE=.TRUE.
C
C     First, see if there is any error inforamtion of any sort.  Either of
C     DSA_SEEK_ERRORS or DSA_SEEK_VARIANCE will tell us that, but they
C     try to hide the actual type from us.
C
      CALL DSA_SEEK_ERRORS(REF_NAME,EXIST,STATUS)
      IF (EXIST) THEN
C
C        There is error info, so we have to use lower level routines to
C        distinguish the type.   Look up REF_NAME in the tables to get
C        the reference slot number, and then DSA__ERROR_NAME will give
C        us what we need to know.
C
         NONE=.FALSE.
         CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
         CALL DSA__ERROR_NAME (REF_SLOT,NAME,LENGTH,ERR_CODE)
         IF (ERR_CODE.EQ.VARIANCE_CODE) THEN
            VARIANCE=.TRUE.
         ELSE
            ERRORS=.TRUE.
         END IF
      END IF
C
      END
