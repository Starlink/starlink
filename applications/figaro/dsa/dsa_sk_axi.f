C+
C                      D S A _ S E E K _ A X I S
C
C  Routine name:
C     DSA_SEEK_AXIS
C
C  Function:
C     Determines whether or not an axis data array exists.
C
C  Description:
C     This routine looks to see if a data structure contains a data
C     array - or other axis information that could generate a data
C     array - for a specified axis.  If an axis array exists but has
C     dimensions incompatible with the main data array - the axis array
C     may have fewer dimensions than the main array, but those it has
C     must match the dimensions in the main array - then a warning
C     message is output and the array is treated as non-existent.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SEEK_AXIS (REF_NAME,AXIS,EXIST,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (>) AXIS         (Integer,ref) The number of the axis in question.
C     (<) EXIST        (Logical,ref) True if there is axis data available.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ system.
C
C  External subroutines / functions used:
C     ICH_CI, ICH_LEN, DSA_FIND_REF, DSA_WRUSER, DSA_GET_ACTUAL_NAME,
C     DSA_ARRAY_EXIST, DSA_ARRAY_SIZE, DSA_MAIN_SIZE, GEN_NTH, ICH_FOLD
C     DSA_VALIDATE_AXIS, DSA__AXIS_DATA_NAME
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     data structure must have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (>) MAX_AXES     (Integer parameter) Maximum number of axes supported.
C     (!) AXIS_EXIST   (Integer array) Indicates knowledge about axis data.
C
C  Subroutine / function details:
C     GEN_NTH       Returns 'st','th','rd' etc appropriate to a number.
C     ICH_CI        Return an integer as a character string.
C     ICH_FOLD      Convert string to upper case.
C     ICH_LEN       Position of last non-blank char in string.
C     DSA_FIND_REF  Look up reference name in common tables.
C     DSA_WRUSER    Output a message string to the user.
C     DSA_GET_ACTUAL_NAME  Get the full structure name from a ref_name.
C     DSA_ARRAY_EXIST   Determine if a named array exists.
C     DSA_ARRAY_SIZE    Determine dimensions of a named array.
C     DSA_MAIN_SIZE     Determine dimensions of main data array.
C
C  History:
C     22nd June 1987.   Original version.  KS / AAO.
C     8th  Dec  1989.   Minor bug in format of error messages fixed.  KS / AAO.
C     19th Jan  1990.   Modified to use DSA__ routines to get data structure
C                       details instead of assuming original Figaro format.
C                       KS/AAO
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_SEEK_AXIS (REF_NAME,AXIS,EXIST,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL EXIST
      CHARACTER*(*) REF_NAME
      INTEGER AXIS, STATUS
C
C     Functions used
C
      CHARACTER GEN_NTH*2, ICH_CI*12
      INTEGER   ICH_FOLD, ICH_LEN
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
      INTEGER   DIMS(MAX_AXES)              ! Dimensions of axis array
      INTEGER   DIMS2(MAX_AXES)             ! Dimensions of main data array
      CHARACTER ERROR*64                    ! Error string
      LOGICAL   FLAGS(MAX_AXES)             ! Flags dimensions in data
      INTEGER   I                           ! Loop index through axis dimensions
      INTEGER   IGNORE                      ! Dummy status argument
      INTEGER   INVOKE                      ! Dummy function value
      INTEGER   J                           ! Loop index through data dimensions
      INTEGER   LENGTH                      ! Object name length
      LOGICAL   MATCH                       ! Flags whether dimensions match
      INTEGER   NDIM                        ! # of dimensions in axis array
      INTEGER   NDIM2                       ! # of dimensions in main array
      CHARACTER NUMBER*12                   ! Formatted number
      CHARACTER OBJ_NAME*128                ! DTA_ name of data object
      CHARACTER REF_NAME_UC*32              ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                    ! Reference table slot #
      INTEGER   STATE                       ! Existence state code for array
      CHARACTER STRUCTURE_NAME*128          ! Full structure name from ref_name
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
C     Check the validity of the value of AXIS, and get the name of the
C     axis structure.
C
      CALL DSA_VALIDATE_AXIS (AXIS,REF_NAME,STATUS)
      IF (STATUS.NE.0) GO TO 500     ! Error exit
C
C     See if we already know about the axis in question.  The state
C     flag indicates either known not to exist (-ve), known to exist
C     (1 if 1d, 2 if nd), or unknown (0).
C
      STATE=AXIS_EXIST(AXIS,REF_SLOT)
      IF (STATE.GT.0) THEN
         EXIST=.TRUE.
      ELSE IF (STATE.LT.0) THEN
         EXIST=.FALSE.
      ELSE
C
C        Generate the name of the data array and see if it exists.
C
         CALL DSA__AXIS_DATA_NAME (REF_SLOT,AXIS,OBJ_NAME,LENGTH)
         CALL DSA_ARRAY_EXIST(OBJ_NAME(:LENGTH),EXIST,STATUS)
C
C        If it exists, get its dimensions and check them against those of
C        the main data array.  This can be a little tricky.  Consider a
C        3-d main array.  The first dimension of the y-axis data must match
C        the y-dimension of the data, and its other dimensions must match
C        other dimensions of the data, but not necessarily in any given order.
C        Hence the messy bit of code using FLAGS.
C
         IF ((STATUS.EQ.0).AND.EXIST) THEN
            CALL DSA_ARRAY_SIZE(OBJ_NAME(:LENGTH),
     :                                 MAX_AXES,NDIM,DIMS,ERROR,STATUS)
            CALL DSA_MAIN_SIZE(REF_SLOT,.FALSE.,MAX_AXES,NDIM2,DIMS2,
     :                                                  ERROR,STATUS)
            IF (STATUS.NE.0) THEN
               IGNORE=0
               CALL DSA_GET_ACTUAL_NAME (REF_NAME_UC,STRUCTURE_NAME,
     :                                                         IGNORE)
               CALL DSA_WRUSER(
     :          'Unable to check axis array dimensions against those ')
               CALL DSA_WRUSER(
     :                      'of the main data array in the structure ')
               CALL DSA_WRUSER(STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
               CALL DSA_WRUSER(' '//ERROR(:ICH_LEN(ERROR)))
               CALL DSA_WRUSER('.')
               CALL DSA_WRFLUSH
            ELSE
               MATCH=((NDIM.LE.NDIM2).AND.(DIMS(1).EQ.DIMS2(AXIS)))
               IF (MATCH) THEN
                  DO J=1,NDIM2
                     FLAGS(J)=.FALSE.
                  END DO
                  FLAGS(AXIS)=.TRUE.
                  DO I=2,NDIM
                     MATCH=.FALSE.
                     DO J=1,NDIM2
                        IF (DIMS(I).EQ.DIMS2(J)) THEN
                           FLAGS(J)=.TRUE.
                           MATCH=.TRUE.
                           GO TO 340                   ! Break out of J loop
                        END IF
                     END DO
  340                CONTINUE
                     IF (.NOT.MATCH) GO TO 360   ! Break out of I loop
                  END DO
  360             CONTINUE
               END IF
               IF (.NOT.MATCH) THEN
                  IGNORE=0
                  CALL DSA_GET_ACTUAL_NAME (REF_NAME_UC,STRUCTURE_NAME,
     :                                                           IGNORE)
                  NUMBER=ICH_CI(AXIS)
                  CALL DSA_WRUSER('The '//
     :                     NUMBER(:ICH_LEN(NUMBER))//GEN_NTH(AXIS)//
     :                             ' axis data array of the structure ')
                  CALL DSA_WRUSER(
     :                         STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
                  CALL DSA_WRUSER(
     :                         ' has dimensions which are incompatible')
                  CALL DSA_WRUSER(
     :                          ' with those of the main data array.')
                  CALL DSA_WRFLUSH
                  EXIST=.FALSE.
               END IF
            END IF
         END IF
C
C        Now we know, set the common flags so we don't have to look
C        it up again.
C
         IF (EXIST) THEN
            IF (NDIM.EQ.1) THEN
               STATE=1
            ELSE
               STATE=2
            END IF
         ELSE
            STATE=-1
         END IF
         AXIS_EXIST(AXIS,REF_SLOT)=STATE
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
