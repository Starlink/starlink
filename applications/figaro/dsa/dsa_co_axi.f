C+
C                D S A _ C O E R C E _ A X I S _ D A T A
C
C  Routine name:
C     DSA_COERCE_AXIS_DATA
C
C  Function:
C     Forces the existence of an axis data array of a specified type.
C
C  Description:
C     This routine creates a new axis data array of specified type and
C     size.  If such an array already exists, it will replace it if
C     necessary - if the type is not that specified, or if the
C     dimensions differ.  Note that this can be used to increase the
C     number of dimensions of an array, (for example, if 1D axis data
C     has to be changed to 2D), but cannot be used to change the basic
C     dimensions of the array in the way that DSA_RESHAPE_AXIS can.  If
C     the array does exist, it should not be mapped at the time this
C     call is made.  The data, if any, in an existing array is
C     maintained unless the type is changed, in which case it is lost
C     completely.  If the data array merely changes size, then existing
C     data will not be lost, but it will not be re-ordered in the array
C     either, so the relation between array index values and data will
C     change unless the only change is to the last dimension of the
C     array.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_COERCE_AXIS_DATA (REF_NAME,AXIS,TYPE,NDIM,DIMS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name for the
C                       structure containing the axis data.
C     (>) AXIS          (Integer,ref) The number of the axis in question.
C     (>) TYPE          (Fixed string,descr) The type that the data array
C                       is to have.  This must be one of the primitive
C                       types recognised by the DSA_ routines.  (Usually,
C                       this will be 'FLOAT' or 'DOUBLE').
C     (>) NDIM          (Integer,ref) The number of dimensions the array
C                       is to have.
C     (>) DIMS          (Integer array,ref) The dimensions the array is
C                       to have.
C     (!) STATUS        (Integer,ref) Status code.  If bad status is
C                       passed, this routine returns immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     DSA_REF_SLOT, DSA_COERCE_ARRAY, DSA_VALIDATE_AXIS,
C     DSA__CREATE_AXIS, DSA__AXIS_DATA_NAME
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the DSA_ routines.
C     The structure in question must have been opened, eg by DSA_OUTPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 13th February 1995
C-
C  Subroutine / function details:
C     DSA_REF_SLOT        Look up reference name in common tables
C     DSA_COERCE_ARRAY    Force named array to specified size and type
C     DSA__CREATE_AXIS    Make sure a specified axis structure exists
C     DSA_VALIDATE_AXIS   Check that a given axis number is valid
C     DSA__AXIS_DATA_NAME Get DTA name of the axis data array for a structure
C
C  History:
C     4th  Aug 1987  Original version.  KS / AAO.
C     4th  Oct 1989  Reworked to become little more than a call to
C                    DSA_COERCE_ARRAY.  Comments modified to indicate
C                    that data is only lost if type changes.  KS/AAO.
C     8th  Mar 1990  Now uses DSA__ routines rather than assuming the
C                    original Figaro data format.  KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     15th Oct 1992  HME / UoE, Starlink.  Refuse creation of an N-D
C                    axis in an NDF.
C     13th Feb 1995  KS/AAO. N-D NDF axis arrays allowed again. These are
C                    now handled at file close time by DSA_CHECK_NDF_AXIS,
C                    which will move them to the axis extension.
C+
      SUBROUTINE DSA_COERCE_AXIS_DATA (REF_NAME,AXIS,TYPE,NDIM,
     :                                                   DIMS,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER AXIS, NDIM, DIMS(NDIM), STATUS
      CHARACTER*(*) REF_NAME, TYPE
C
C     Local variables
C
      CHARACTER DATA_NAME*80     ! DTA_ name for axis data array
      INTEGER   IGNORE           ! Dummy status value
      INTEGER   LENGTH           ! Length of OBJ_NAME
      INTEGER   REF_SLOT         ! Reference name common slot
C
C     DSA system common
C
      INCLUDE 'DSA_COMMON'
C
C     DSA system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Return if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     Check that AXIS has an acceptable value.
C
      CALL DSA_VALIDATE_AXIS (AXIS,REF_NAME,STATUS)
C
C     Look up the reference name in the tables and get the name
C     of the axis data array.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500    ! Error exit
      CALL DSA__AXIS_DATA_NAME (REF_SLOT,AXIS,DATA_NAME,LENGTH)
C
C     Make sure that the axis structure exists.
C
      CALL DSA__CREATE_AXIS (REF_SLOT,AXIS,IGNORE)
C
C     Let DSA_COERCE_ARRAY do the work.
C
      CALL DSA_COERCE_ARRAY (DATA_NAME(:LENGTH),TYPE,NDIM,DIMS,STATUS)
C
C     Exit
C
  500 CONTINUE
C
      END
