C+
C                  D S A _ D E L E T E _ I M A G I N A R Y
C
C  Routine name:
C     DSA_DELETE_IMAGINARY
C
C  Function:
C     Converts a complex main data array into a real one.
C
C  Description:
C     This routine converts a complex main data array into a real
C     one.  Depending on the data structure details, this may simply
C     involve deleting the imaginary part, or it may involve some
C     more subtle conversions.  This routine does not return error
C     status if the main data array were real in the first place, but
C     it will return bad status and output an error message if the
C     conversion to real data cannot be performed (a protected file,
C     for example).
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_DELETE_IMAGINARY (REF_NAME,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DSA_REF_SLOT, DSA_WRUSER, DSA__DELETE_IMAGINARY, DSA_GET_ACTUAL_NAME,
C     DTA_ERROR, ICH_LEN
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
C     DSA_REF_SLOT     Look up reference name in common tables.
C     DSA_WRUSER       Output message string to user.
C     DSA_GET_ACTUAL_NAME    Get full name of structure from reference name.
C     DSA__DELETE_IMAGINARY  Delete imaginary array knowing structure details.
C     DTA_ERROR        Convert DTA status into error message.
C     ICH_LEN          Position of last non-blank char in string.
C
C  History:
C     2nd Feb 1989.   Original version.  KS / AAO.
C     1st May 1990.   Now uses DSA__DELETE_IMAGINARY to do most of the work,
C                     and so can handle different data formats.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_DELETE_IMAGINARY (REF_NAME,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) REF_NAME
      INTEGER STATUS
C
C     Functions
C
      INTEGER ICH_LEN
C
C     Local variables
C
      INTEGER   DTA_STATUS                  ! DTA routine status
      CHARACTER ERROR*64                    ! Error string
      INTEGER   IGNORE                      ! Status we don't care about
      INTEGER   REF_SLOT                    ! Reference table slot #
      CHARACTER STRUCTURE*128               ! Full name of structure
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
C
C     Use DSA__DELETE_IMAGINARY to do the hard work.
C
      CALL DSA__DELETE_IMAGINARY (REF_SLOT,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
         CALL DSA_WRUSER ('Unable to delete the imaginary part of '
     :                                     //'the main data array in ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,IGNORE)
         CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DTA_ERROR (DTA_STATUS,ERROR)
         CALL DSA_WRUSER('. ')
         CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
         CALL DSA_WRUSER('.')
         CALL DSA_WRFLUSH
      END IF
C
C     Exit
C
      END
