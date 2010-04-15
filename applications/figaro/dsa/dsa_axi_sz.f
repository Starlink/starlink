C+
C
C                      D S A _ A X I S _ S I Z E
C
C  Routine name:
C     DSA_AXIS_SIZE
C
C  Function:
C     Returns the dimensions of an axis data array.
C
C  Description:
C     This routine returns the dimensions and total number of elements
C     in a specified axis data array.   If the axis data does not in
C     fact exist, this routine returns as if it were a 1D array of the
C     same size as the corresponding dimension of the data array.
C     This behaviour is in line with that of DSA_MAP_AXIS_DATA.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_AXIS_SIZE (REF_NAME,AXIS,MAX_DIM,NDIM,DIMS,
C                                                     ELEMENTS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (>) AXIS         (Integer,ref) The number of the axis in question.
C     (>) MAX_DIM      (Integer,ref) The maximum number of dimensions
C                      for the data.
C     (<) NDIM         (Integer,ref) The actual number of dimensions in
C                      the data.
C     (<) DIMS         (Integer array,ref) The number of elements in
C                      each axis of the data.  Elements DIMS(NDIM+1)
C                      to DIMS(MAX_DIM), if any, are set to 1.
C     (<) ELEMENTS     (Integer,ref) The total number of elements in the
C                      data.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     ICH_CI, ICH_LEN, DSA_FIND_REF, DSA_WRUSER, DSA_GET_ACTUAL_NAME,
C     DSA_ARRAY_SIZE, GEN_NTH, ICH_FOLD, DSA_SEEK_AXIS, DSA_VALIDATE_AXIS,
C     DSA__AXIS_DATA_NAME
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
C     GEN_NTH       Returns 'st','th','rd' etc appropriate to a number.
C     ICH_CI        Return an integer as a character string.
C     ICH_FOLD      Convert a string to upper case.
C     ICH_LEN       Position of last non-blank char in string.
C     DSA_FIND_REF  Look up reference name in common tables.
C     DSA_WRUSER    Output a message string to the user.
C     DSA_GET_ACTUAL_NAME  Get the full structure name from a ref_name.
C     DSA_SEEK_AXIS See if axis data exists or not.
C     DSA_ARRAY_SIZE  Get dimensions of a named data array.
C     DSA_VALIDATE_AXIS Check that a specified axis number is valid.
C     DSA__AXIS_DATA_NAME  Get DTA_ system name of axis data array.
C
C  History:
C     15th June 1987.   Original version.  KS / AAO.
C     8th  July 1988.   Add code to test for case AXIS>data dimensions. KS/AAO.
C     8th  Dec  1989.   Set unused elements of DIMS to 1.  KS/AAO.
C     19th Jan  1990.   Use DSA__ routines to get structure details rather
C                       than assume original Figaro structure.  KS/AAO.
C     21st Dec  1990.   Wasn't setting high elements of DIMS to 1 if axis
C                       array doesn't exist.  Fixed.  KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_AXIS_SIZE (REF_NAME,AXIS,MAX_DIM,NDIM,DIMS,
     :                                               ELEMENTS,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) REF_NAME
      INTEGER AXIS, MAX_DIM, NDIM, DIMS(MAX_DIM), ELEMENTS, STATUS
C
C     Functions used
C
      CHARACTER GEN_NTH*2, ICH_CI*12
      INTEGER   ICH_LEN, ICH_FOLD
C
C     Local variables
C
      INTEGER   DARRAY(10)                  ! Dimensions of data array
      CHARACTER ERROR*64                    ! DTA_ error description
      LOGICAL   EXIST                       ! True if axis data exists
      INTEGER   I                           ! Loop variable
      INTEGER   INVOKE                      ! Dummy function value
      INTEGER   LENGTH                      ! Object name length
      CHARACTER NUMBER*12                   ! Formatted number
      CHARACTER OBJ_NAME*128                ! DTA_ name of data object
      CHARACTER REF_NAME_UC*32              ! Upper case version of ref_name
      INTEGER   REF_SLOT                    ! Reference table slot # - ignored
      CHARACTER STRUCTURE_NAME*128          ! Full structure name from ref_name
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables.
C
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
C
C     Check that AXIS is a valid number.
C
      CALL DSA_VALIDATE_AXIS (AXIS,REF_NAME,STATUS)
      IF (STATUS.NE.0) GO TO 500     ! Error exit
C
C     See if the axis data does in fact exist.
C
      CALL DSA_SEEK_AXIS (REF_NAME,AXIS,EXIST,STATUS)
      IF (EXIST) THEN
C
C        Generate the name of the data array and get its size.  Note we
C        use our own array, which will be big enough (DTA_ arrays are
C        limited to 10 dimensions), just so we can generate a better
C        error message than just have DTA_SZVAR fail because MAX_DIM
C        is too small.
C
         CALL DSA__AXIS_DATA_NAME (REF_SLOT,AXIS,OBJ_NAME,LENGTH)
         CALL DSA_ARRAY_SIZE(OBJ_NAME(:LENGTH),
     :                                    10,NDIM,DARRAY,ERROR,STATUS)
         IF (STATUS.NE.0) THEN
            NUMBER=ICH_CI(AXIS)
            CALL DSA_WRUSER('Unable to get dimensions of '//
     :                       NUMBER(:ICH_LEN(NUMBER))//GEN_NTH(AXIS)//
     :                                         ' axis data array in ')
            CALL DSA_GET_ACTUAL_NAME(REF_NAME_UC,STRUCTURE_NAME,STATUS)
            CALL DSA_WRUSER(STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
            CALL DSA_WRUSER('. ')
            CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR))//'.')
            CALL DSA_WRFLUSH
            STATUS=DSA__DTAERR
            GO TO 500
         END IF
C
C        See if the array's dimensions are not too large
C
         IF (NDIM.GT.MAX_DIM) THEN
            NUMBER=ICH_CI(AXIS)
            CALL DSA_WRUSER('The '//NUMBER(:ICH_LEN(NUMBER))
     :                         //GEN_NTH(AXIS)//' axis data array in ')
            CALL DSA_GET_ACTUAL_NAME(REF_NAME_UC,STRUCTURE_NAME,STATUS)
            CALL DSA_WRUSER(STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
            NUMBER=ICH_CI(NDIM)
            CALL DSA_WRUSER(' is '//NUMBER(:ICH_LEN(NUMBER)))
            CALL DSA_WRUSER(
     :          '-dimensional, which is too many dimensions ')
            CALL DSA_WRUSER('for this application to handle.')
            CALL DSA_WRFLUSH
            STATUS=DSA__OVRDIM
            GO TO 500
         END IF
C
C        Calculate number of elements, and return dimensions in user's array.
C
         ELEMENTS=1
         DO I=1,NDIM
            DIMS(I)=DARRAY(I)
            ELEMENTS=ELEMENTS*DIMS(I)
         END DO
         DO I=NDIM+1,MAX_DIM
            DIMS(I)=1
         END DO
      ELSE
C
C        If the axis does not exist, use the main data dimensions.
C        (We ignore any error here, assuming it will be OK; if this
C        were not the case, DSA_SEEK_AXIS would have failed.)  If the
C        main array does not have this many dimensions, then we treat
C        the axis as having size 1.
C
         CALL DSA_MAIN_SIZE (REF_SLOT,.FALSE.,10,NDIM,DARRAY,
     :                                               ERROR,STATUS)
         IF (AXIS.LE.NDIM) THEN
            DIMS(1)=DARRAY(AXIS)
            ELEMENTS=DARRAY(AXIS)
         ELSE
            DIMS(1)=1
            ELEMENTS=1
         END IF
         NDIM=1
         DO I=NDIM+1,MAX_DIM
            DIMS(I)=1
         END DO
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
