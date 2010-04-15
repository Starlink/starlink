C+
C                       D S A _ M A I N _ S I Z E
C
C  Routine name:
C     DSA_MAIN_SIZE
C
C  Function:
C     Provides quick access to the dimensions of the main data array
C
C  Description:
C     Many DSA_ routines need to access the dimensions of the main data
C     array of a structure.  Since this can involve a number of DTA_
C     routine calls, especially if some complicated structure is in
C     use, the main array dimensions are held in common and can be
C     accessed either directly or by this routine.  The advantage of this
C     routine is that if the dimensions have not yet been recorded in
C     common, it will determine them and set the common variables
C     accordingly, ready for next time.  This routine may also be used
C     to set the common variables to a passed set of values.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_MAIN_SIZE (REF_SLOT,SET,MAXDIM,NDIM,DIMS,ERROR,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT        (Integer,ref) The reference slot number for the
C                         data structure.
C     (>) SET             (Logical,ref) If true, this routine sets the
C                         common dimension variables to the values passed
C                         in NDIM and DIMS.
C     (>) MAXDIM          (Integer,ref) The number of elements in DIMS.
C     (!) NDIM            (Integer,ref) The number of dimensions in the
C                         main data array.  NDIM=0 indicates that no such
C                         array exists.
C     (!) DIMS            (Integer array,ref) The dimensions of the main
C                         data array.
C     (<) ERROR           (Fixed string,descr) An error string describing
C                         any error that may have occurred.
C     (!) STATUS          (Integer,ref) Status code value.  If bad status
C                         is passed, this routine returns immediately.
C                         Note that if no main array exists, this is
C                         regarded as an error.  Also note that this
C                         routine outputs no error or other messages.
C
C  External variables used:
C     Only common variables used internally by the DSA_ package.
C
C  External subroutines / functions used:
C     DSA_ARRAY_SIZE, DSA__ARRAY, DSA__DATA_NAME
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (>) MAX_REFS    (Integer parameter) Maximum number of reference names.
C     (>) OBJ_LEN     (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES   (String array) Name (as recognised by DTA_) of data
C                     object corresponding to reference name.
C     (!) DATA_NDIM   (Integer array) Number of dimensions of main data array.
C     (!) DATA_DIMS   (Integer array) Dimensions of main data array. (2D array)
C
C  Subroutine / function details:
C     DSA_ARRAY_SIZE  Get the dimensions of a named array.
C     DSA__DATA_NAME  Get the name of the main data array.
C     DSA__ARRAY      Indicates if an object can be treated as an array.
C
C  History:
C     23rd June 1987    Original version.  KS / AAO.
C     16th Jan  1990    Modified to use DSA__ routines to handle the
C                       structure details, rather than assuming the original
C                       Figaro format.  KS/AAO.
C     26th Apr  1990    Declaration of DIMS modified to avoid crash if
C                       NDIM=0.  KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992     Remove unused variable declarations. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C+
      SUBROUTINE DSA_MAIN_SIZE (REF_SLOT,SET,MAXDIM,NDIM,DIMS,
     :                                                    ERROR,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL SET
      INTEGER REF_SLOT, MAXDIM, NDIM, DIMS(*), STATUS
      CHARACTER*(*) ERROR
C
C     Functions
C
      LOGICAL DSA__ARRAY
C
C     DSA_ system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   I                        ! Loop index through dimensions
      INTEGER   LENGTH                   ! Number of chars in object name
      CHARACTER NAME*80                  ! DTA system name of main data array
C
C     Return immediately if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     See if dimensions are to be set or got.
C
      IF (SET) THEN
C
C        Set is easy.  Just copy from NDIM and DIMS into common.
C
         DATA_NDIM(REF_SLOT)=NDIM
         DO I=1,NDIM
            DATA_DIMS(I,REF_SLOT)=DIMS(I)
         END DO
C
      ELSE
C
C        If not set, then first see if the dimensions are in common
C        already.  If so, just copy them over.
C
         IF (DATA_NDIM(REF_SLOT).GT.0) THEN
            NDIM=DATA_NDIM(REF_SLOT)
            DO I=1,NDIM
               DIMS(I)=DATA_DIMS(I,REF_SLOT)
            END DO
         ELSE
C
C           If not in common yet, we have to get them from the data,
C           and then set them in common.
C
C           Allow for the fact that the object may itself be the array. If
C           it is not itself an array, get the main data array name from
C           DSA__DATA_NAME.
C
            NAME=OBJ_NAMES(REF_SLOT)
            LENGTH=OBJ_LEN(REF_SLOT)
            IF (.NOT.DSA__ARRAY(NAME)) THEN
               CALL DSA__DATA_NAME(REF_SLOT,NAME,LENGTH)
            END IF
C
C           Now get the array dimensions.
C
            CALL DSA_ARRAY_SIZE (NAME(:LENGTH),
     :                               MAXDIM,NDIM,DIMS,ERROR,STATUS)
C
C           Check we got them OK, and set the values.
C
            IF (STATUS.EQ.0) THEN
               DATA_NDIM(REF_SLOT)=NDIM
               DO I=1,NDIM
                  DATA_DIMS(I,REF_SLOT)=DIMS(I)
               END DO
            ELSE
               NDIM=0
            END IF
         END IF
      END IF
C
      END
