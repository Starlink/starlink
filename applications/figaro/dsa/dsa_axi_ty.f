C+
C
C                      D S A _ A X I S _ T Y P E
C
C  Routine name:
C     DSA_AXIS_TYPE
C
C  Function:
C     Returns the type of an axis array.
C
C  Description:
C     This routine returns the DTA_ system type of an axis array
C     of a structure.  If the array does not exist, an error message is
C     output and bad status is returned.  If the array is a
C     structured type, this is also indicated.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_AXIS_TYPE (REF_NAME,AXIS,TYPE,STRUCT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (>) AXIS         (Integer,ref) The number of the axis in question.
C                      Should be between 1 and 6.
C     (<) TYPE         (Fixed string,descr) The DTA_ system type of the
C                      axis array.
C     (<) STRUCT       (Logical,ref) Set to indicate whether or not the
C                      array is a structured type.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     ICH_CI, ICH_LEN, DSA_REF_SLOT, DSA_VALIDATE_AXIS, DSA__AXIS_NAME,
C     DSA_ARRAY_TYPE
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
C     ICH_LEN             Position of last non-blank char in string.
C     ICH_CI              Return integer number formatted into a string.
C     DSA_REF_SLOT        Look up reference name in common tables.
C     DSA_VALIDATE_AXIS   Check that axis number is valid.
C     DSA_ARRAY_TYPE      Get type of named array.
C     DSA__AXIS_DATA_NAME Get name of axis data array.
C
C  History:
C     9th Sep 1988.   Original version.  JM / AAO.
C     7th Dec 1989.   Now fails if the structure was originally specified
C                     as a primitive array - that's OK for the main data
C                     array, but if it isn't a structure it can't have an
C                     axis width.  KS / AAO.
C     22nd Feb 1990.  Now uses DSA__ routines to handle details of the
C                     data structure, so can handle different file formats.
C                     KS/AAO.
C     12th Mar 1990.  Was using DSA__AXIS_NAME instead of DSA__AXIS_DATA_NAME.
C                     Fixed.  KS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C
C+
      SUBROUTINE DSA_AXIS_TYPE (REF_NAME,AXIS,TYPE,STRUCT,STATUS)
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
      INTEGER   ICH_LEN
      CHARACTER GEN_NTH*2, ICH_CI*12
C
C     Local variables
C
      INTEGER   LENGTH                      ! Object name length
      CHARACTER NAME*128                    ! DTA_ name of axis data object
      CHARACTER NUMBER*12                   ! Formatted number
      INTEGER   REF_SLOT                    ! Reference table slot # - ignored
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Validate the number of the axis
C
      CALL DSA_VALIDATE_AXIS (AXIS,REF_NAME,STATUS)
      IF (STATUS.NE.0) GO TO 500     ! Error exit
C
C     Look up the reference name in the tables and generate the
C     axis array name.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500   ! Error exit
      CALL DSA__AXIS_DATA_NAME (REF_SLOT,AXIS,NAME,LENGTH)
C
C     Now get the type of the array
C
      NUMBER=ICH_CI(AXIS)
      CALL DSA_ARRAY_TYPE (NAME,NUMBER(:ICH_LEN(NUMBER))//
     :            GEN_NTH(AXIS)//' axis array',TYPE,STRUCT,STATUS)
C
C     Exit
C
  500 CONTINUE
C
      END
