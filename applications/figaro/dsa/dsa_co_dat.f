C+
C               D S A _ C O E R C E _ D A T A _ A R R A Y
C
C  Routine name:
C     DSA_COERCE_DATA_ARRAY
C
C  Function:
C     Forces the existence of a data array of a specified type and size.
C
C  Description:
C     This routine creates a new data array of specified type and
C     size.  If such an array already exists, it will replace it if
C     necessary - if the type is not that specified, or if the dimensions
C     differ.  Note that this can be used to create a data array in a
C     structure that does not at present have such an array, but cannot
C     be used to change the basic dimensions of an existing array in the
C     way that DSA_RESHAPE_DATA can.  If the array does exist, it should
C     not be mapped at the time this call is made.  The data, if any, in
C     an existing array is maintained unless the type is changed, in which
C     case it is lost completely.  If the data array merely changes size,
C     then existing data will not be lost, but it will not be re-ordered
C     in the array either, so the relation between array index values and
C     data will change unless the only change is to the last dimension of
C     the array.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_COERCE_DATA_ARRAY (REF_NAME,TYPE,NDIM,DIMS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name for the
C                       structure containing or to contain the data.
C     (>) TYPE          (Fixed string,descr) The type that the data array
C                       is to have.  This must be one of the types
C                       recognised by the DSA_ routines.  (Usually,
C                       this will be 'FLOAT' or 'DOUBLE', but can be a
C                       structured type that the routines recognise).
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
C     DSA_COERCE_DATA_ARRAY, DSA__CREATE_DATA_ENV, DSA_MAIN_SIZE,
C     DSA_REF_SLOT, DSA__DATA_NAME
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the DSA_ routines.
C     The structure in question must have been opened, eg by DSA_OUTPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_MAIN_SIZE     Set data array dimensions in common
C     DSA_REF_SLOT      Look up reference name in common tables
C     DSA_COERCE_ARRAY  Coerce a named array to a given size and type
C     DSA__CREATE_DATA_ENV  Make sure there is a structure for the main array
C     DSA__DATA_NAME    Get DTA system name of main data array
C
C  History:
C     24th Aug 1987  Original version.  KS / AAO.
C     4th  Oct 1989  Now reworked as little more than a call to
C                    DSA_COERCE_ARRAY. Comments revised to indicate
C                    that data not lost except on type change.  KS/AAO.
C     18th Jan 1990  Now uses DSA__ routines to get data structure details
C                    instead of assuming the original Figaro format.  KS/AAO.
C     23rd Apr 1990  Dimensioning of DIMS changed to prevent adjustable
C                    array exception if called with NDIM=0.  KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992  Remove unused variable declarations. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C
C+
      SUBROUTINE DSA_COERCE_DATA_ARRAY (REF_NAME,TYPE,NDIM,
     :                                                 DIMS,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NDIM, DIMS(*), STATUS
      CHARACTER*(*) REF_NAME, TYPE
C
C     Local variables
C
      CHARACTER DATA_NAME*80          ! DTA_ name for axis data array
      CHARACTER ERROR*32              ! DTA error string - ignored
      INTEGER   IGNORE                ! Dummy status value
      INTEGER   LENGTH                ! Length of DATA_NAME
      INTEGER   REF_SLOT              ! Reference name common slot
C
C     Return if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the table.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
C
C     Get the DTA_ name for the data array.
C
      CALL DSA__DATA_NAME (REF_SLOT,DATA_NAME,LENGTH)
C
C     Make sure there's an environment for the data array.
C
      CALL DSA__CREATE_DATA_ENV (REF_SLOT,IGNORE)
C
C     Let DSA_COERCE_ARRAY do most of the work.
C
      CALL DSA_COERCE_ARRAY (DATA_NAME,TYPE,NDIM,DIMS,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Make sure we update the common data array size values
C
      IGNORE=0
      CALL DSA_MAIN_SIZE (REF_SLOT,.TRUE.,NDIM,NDIM,DIMS,ERROR,IGNORE)
C
C     Exit
C
  500 CONTINUE
C
      END
