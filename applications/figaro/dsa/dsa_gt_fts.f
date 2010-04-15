C+
C                    D S A _ G E T _ F I T S _ E R R
C
C  Routine name:
C     DSA_GET_FITS_ERR
C
C  Function:
C     Outputs an error message when a read from a FITS item fails.
C
C  Description:
C     This routine is called by the DSA__READ_FITS_{x} routines,
C     as a utility ot output an error message in the event of their
C     being unable to read a value.  This routine is only used when
C     the NDF format is being used and the item in the FITS card
C     image cannot be properly converted.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_GET_FITS_ERR (STRING,ERROR,REF_NAME,ITEM,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) STRING          (Fixed string,descr) The formatted version
C                         of the data which has failed to be decoded
C                         properly.
C     (>) ERROR           (Fixed string,descr) A string describing
C                         the problem.
C     (>) REF_NAME        (Fixed string,descr) The reference name
C                         used to identify the structure.
C     (>) ITEM            (Fixed string,descr) The name of the item
C                         in question - (this is a FITS keyword).
C     (!) STATUS          (Integer,ref) Status code.  If bad status is
C                         passed to it, this routine returns immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     DSA_GET_ACTUAL_NAME, DSA_WRUSER, ICH_LEN
C
C  Prior requirements:
C     This is designed to be called from one of the DSA_GET_FITS_{x}
C     routines.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_GET_ACTUAL_NAME  Get full name of data structure
C     DSA_WRUSER           Output string to user
C     ICH_LEN              Position of last non-blank character in string
C
C  History:
C     8th Feb  1990.  Original version.  KS / AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C     25th Jul 1996   Catenation for Linux.  MJCL/Starlink, UCL.
C+
      SUBROUTINE DSA_GET_FITS_ERR (STRING,ERROR,REF_NAME,ITEM,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) STRING, ERROR, REF_NAME, ITEM
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      INTEGER   IGNORE                    ! Status value we don't care about
      CHARACTER STRUCTURE_NAME*128        ! Full name of data structure
      CHARACTER LSTRING*80                 ! Local string storage
C
C     Return immediately if bad status passed.
C
      IF (STATUS.NE.0) RETURN
C
      CALL DSA_WRUSER ('Error decoding FITS data for "')
      CALL DSA_WRUSER (ITEM(:ICH_LEN(ITEM)))
      CALL DSA_WRUSER ('" in the structure ')
      IGNORE=0
      CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE_NAME,STATUS)
      CALL DSA_WRUSER (STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
      IF (STRING.EQ.' ') THEN
         CALL DSA_WRUSER ('. A blank string ')
      ELSE
         LSTRING='. "'//STRING(:ICH_LEN(STRING))//'"'
         CALL DSA_WRUSER(LSTRING)
      END IF
      CALL DSA_WRUSER (ERROR(:ICH_LEN(ERROR)))
      CALL DSA_WRUSER ('.')
      CALL DSA_WRFLUSH
      STATUS=DSA__FITINV
C
      END
