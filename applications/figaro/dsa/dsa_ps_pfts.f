C+
C                    D S A _ P O S T _ P U T _ F I T S
C
C  Routine name:
C     DSA_POST_PUT_FITS
C
C  Function:
C     Performs post processing for the DSA_PUT_FITS_{x} routines.
C
C  Description:
C     This routine is called by the DSA_PUT_FITS_{x} routines,
C     and does little more than test the status of the DTA call
C     used to write out the value.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_POST_PUT_FITS (DTA_STATUS,NAME,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) DTA_STATUS       (Integer,ref) The DTA_ status returned by
C                          the call to write out the FITS value.
C     (>) NAME             (Fixed string,descr) The DTA name of the FITS
C                          value being written.
C     (!) STATUS           (Integer,ref) Status code.  If bad status is
C                          passed to it, this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA package.
C
C  External subroutines / functions used:
C     DTA_ERROR, DSA_WRNAME, DSA_WRUSER
C
C  Prior requirements:
C     This is designed to be called from one of the DSA_PUT_FITS_{x}
C     routines.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_WRUSER      Output string to user
C     DSA_WRNAME      Output data object name to user
C     DTA_ERROR       Get string describing a DTA error code
C
C  Common variable details:
C     (<) DTA_CODE    (Integer) Last DTA error code
C
C  History:
C     28th Nov 1988   Original version.  KS / AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_POST_PUT_FITS (DTA_STATUS,NAME,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER DTA_STATUS, STATUS
      CHARACTER*(*) NAME
C
C     Functions used
C
      INTEGER ICH_LEN
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
      CHARACTER ERROR*64            ! DTA error description
C
C     Return immediately if bad status passed.
C
      IF (STATUS.NE.0) RETURN
C
C     Test status of DTA call
C
      IF (DTA_STATUS.NE.0) THEN
         CALL DSA_WRUSER ('Error writing FITS data to ')
         CALL DSA_WRNAME (NAME)
         CALL DSA_WRUSER ('. ')
         CALL DTA_ERROR (DTA_STATUS,ERROR)
         CALL DSA_WRUSER (ERROR(:ICH_LEN(ERROR)))
         CALL DSA_WRUSER ('.')
         CALL DSA_WRFLUSH
         STATUS=DSA__DTAERR
         DTA_CODE=DTA_STATUS
      END IF
C
      END
