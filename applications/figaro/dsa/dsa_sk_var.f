C+
C                      D S A _ S E E K _ V A R I A N C E
C
C  Routine name:
C     DSA_SEEK_VARIANCE
C
C  Function:
C     Determines whether or not a variance data array exists.
C
C  Description:
C     This routine looks to see if a data structure contains a data
C     array - or other information that could generate a data array -
C     giving the variance information for that structure.  Although it
C     is quite indifferent as to the existence or non-existence of
C     such an array, it will output a warning message if it finds
C     aa variance array whose size does not match that of the main data
C     array.  It will treat a non-matching variance array as not existing.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SEEK_VARIANCE (REF_NAME,EXIST,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (<) EXIST        (Logical,ref) True if there is variance data available.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DSA_SEEK_ERRORS
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     data structure should have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_SEEK_ERRORS    Determine whether or not a structure has error data.
C
C  History:
C     12th Dec 1989.   Original version.  KS / AAO.
C     12th March 1990. Modified to work with different data formats.  (Only
C                      changes required, in fact, were to the comments) KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_SEEK_VARIANCE (REF_NAME,EXIST,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL EXIST
      CHARACTER*(*) REF_NAME
      INTEGER STATUS
C
C     If there is any error information available, either as a variance
C     array or as an uncertainty array, DSA_SEEK_ERRORS will indicate that
C     it exists, without caring what type it actually is.  So it already
C     does all we need, and all we have to do is convert this call into
C     one to DSA_SEEK_ERRORS.
C
      CALL DSA_SEEK_ERRORS (REF_NAME,EXIST,STATUS)
C
C     Exit
C
      END
