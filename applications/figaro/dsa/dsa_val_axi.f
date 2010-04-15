C+
C                      D S A _ V A L I D A T E _ A X I S
C
C  Routine name:
C     DSA_VALIDATE_AXIS
C
C  Function:
C     Checks whether or not a given axis number is valid.
C
C  Description:
C     This is a small service routine that simply checks if a specified
C     axis number is one that is acceptable to the DSA system.  If
C     not (if it is too large, or zero, or negative) then an error
C     message is output and bad status is returned.  Note that this
C     has nothing to do with whether or not any given structure has
C     such an axis - it is just to make sure that no internal arrays
C     are accessed outside their bounds.  Although a reference name is
C     passed to this routine, this is only used when generating an
C     error message.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_VALIDATE_AXIS (AXIS,REF_NAME,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) AXIS         (Integer,ref) The number of the axis in question.
C     (>) REF_NAME     (Fixed string,descr) The DSA reference name for the
C                      structure in connection with which this axis value
C                      is being used.  Only used to generate error text.
C                      Note: this must be in upper case.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ system.
C     The structure specified by REF_NAME should already be open.
C
C  External subroutines / functions used:
C     ICH_CI, ICH_LEN, DSA_WRUSER, DSA_GET_ACTUAL_NAME, GEN_NTH
C
C  Prior requirements:  None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (>) MAX_AXES     (Integer parameter) Maximum number of axes supported.
C
C  Subroutine / function details:
C     GEN_NTH       Returns 'st','th','rd' etc appropriate to a number.
C     ICH_CI        Return an integer as a character string.
C     ICH_LEN       Position of last non-blank char in string.
C     DSA_WRUSER    Output a message string to the user.
C     DSA_GET_ACTUAL_NAME  Get the full structure name from a ref_name.
C
C  History:
C     14th Dec  1989.   Original version.  KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_VALIDATE_AXIS (AXIS,REF_NAME,STATUS)
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
      CHARACTER GEN_NTH*2, ICH_CI*12
      INTEGER   ICH_LEN
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      CHARACTER NUMBER*12                   ! Used to format the axis number
      CHARACTER STRUCTURE_NAME*128          ! Full structure name from ref_name
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Check the value of AXIS.
C
      IF ((AXIS.LT.1).OR.(AXIS.GT.MAX_AXES)) THEN
         NUMBER=ICH_CI(AXIS)
         CALL DSA_WRUSER('Unable to access the ')
         CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER))//GEN_NTH(AXIS))
         CALL DSA_WRUSER(' axis of the structure ')
         CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE_NAME,STATUS)
         CALL DSA_WRUSER(STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
         CALL DSA_WRUSER('.  Probable programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__AXINV
      END IF
C
      END
