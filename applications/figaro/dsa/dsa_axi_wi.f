C+
C
C                      D S A _ A X I S _ W I D T H _ T Y P E
C
C  Routine name:
C     DSA_AXIS_WIDTH_TYPE
C
C  Function:
C     Returns the type of an axis width array.
C
C  Description:
C     This routine returns the DTA_ system type of an axis width array
C     for a structure.  If the array does not exist, an error message is
C     output and bad status is returned.  If the array is a
C     structured type, this is also indicated.  If the width information
C     is held as a single value, the type of this value is returned.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_AXIS_WIDTH_TYPE (REF_NAME,AXIS,TYPE,STRUCT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (>) AXIS         (Integer,ref) The number of the axis in question.
C                      Should be between 1 and 6.
C     (<) TYPE         (Fixed string,descr) The DTA_ system type of the
C                      axis width array.
C     (<) STRUCT       (Logical,ref) Set to indicate whether or not the
C                      array is a structured type.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     ICH_CI, DSA_REF_SLOT, DSA_ARRAY_TYPE, DSA__AXIS_WIDTH_NAME
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     data structure must have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_FOLD              Convert a string to upper case.
C     DSA_REF_SLOT          Look up reference name in common tables.
C     DSA_ARRAY_TYPE        Get type of array.
C     DSA__AXIS_WIDTH_NAME  Get DTA name of axis width object.
C
C  History:
C     7th   Dec 1989.   Original version.  JM / AAO.
C     8th March 1990.   Modified to use DSA__ routines rather than assuming
C                       the original Figaro format.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_AXIS_WIDTH_TYPE (REF_NAME,AXIS,TYPE,STRUCT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL STRUCT
      CHARACTER*(*) REF_NAME,TYPE
      INTEGER AXIS,STATUS
C
C     Functions used
C
      CHARACTER GEN_NTH*2, ICH_CI*12
C
C     Local variables
C
      INTEGER   LENGTH                      ! Object name length
      CHARACTER NAME*64                     ! DTA_ name of axis width object
      INTEGER   REF_SLOT                    ! Reference table slot # - ignored
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Validate the axis number
C
      CALL DSA_VALIDATE_AXIS (AXIS,REF_NAME,STATUS)
C
C     Look up the reference name in the tables and get name of width
C     array.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500   ! Error exit
      CALL DSA__AXIS_WIDTH_NAME (REF_SLOT,AXIS,NAME,LENGTH)
C
C     Get its type
C
      CALL DSA_ARRAY_TYPE (NAME(:LENGTH),'width array for '
     :       //ICH_CI(AXIS)//GEN_NTH(AXIS)//' axis',TYPE,STRUCT,STATUS)
C
C     Exit
C
  500 CONTINUE
C
      END
