C+
C                      D S A _ S E E K _ W I D T H
C
C  Routine name:
C     DSA_SEEK_WIDTH
C
C  Function:
C     Determines whether or not axis width information exists.
C
C  Description:
C     This routine looks to see if a data structure contains a data
C     array - or other axis information that could generate a data
C     array - giving width information for the axis.  A width array,
C     unless it is a single value, must have the same dimensions as the
C     corresponding axis array.  If a width array exists but it
C     dimensions do not match the axis array then a warning message is
C     output and the array is treated as non-existent.  Note that the
C     width array is unusual, in that it will often be a single value,
C     so this routine, unlike most DSA_SEEK_{xxx} routines has an
C     additional SINGLE parameter that it uses to indicate this, and
C     another parameter used to return such a single width.  If this is
C     the case, it will be much simpler to use this value than to
C     mapping the array with DSA_MAP_WIDTH (although this will still
C     work).
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SEEK_WIDTH (REF_NAME,AXIS,EXIST,SINGLE,WIDTH,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (>) AXIS         (Integer,ref) The number of the axis in question.
C     (<) EXIST        (Logical,ref) True if there is axis width data
C                      available.
C     (<) SINGLE       (Logical,ref) True if the width data is a single,
C                      constant, value.
C     (<) WIDTH        (Double,ref) If SINGLE is returned as true, this
C                      is that single width value.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ system.
C
C  External subroutines / functions used:
C     ICH_CI, ICH_LEN, DSA_FIND_REF, DSA_WRUSER, DSA_GET_ACTUAL_NAME,
C     DSA_ARRAY_EXIST, DSA_ARRAY_SIZE, DSA_AXIS_SIZE, GEN_NTH, ICH_FOLD
C     DTA_SZVAR, DTA_STRUC, DTA_RDVARD, DTA_ERROR, DSA__AXIS_WIDTH_NAME
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
C     (<) DTA_CODE     (Integer) Last DTA system error code.
C
C  Subroutine / function details:
C     DTA_STRUC     Determine if a data object is a structure
C     DTA_SZVAR     Get the dimensions of a data object
C     DTA_RDVARD    Read a double precision data object
C     DTA_ERROR     Get text for a DTA error code
C     GEN_NTH       Returns 'st','th','rd' etc appropriate to a number.
C     ICH_CI        Return an integer as a character string.
C     ICH_FOLD      Convert string to upper case.
C     ICH_LEN       Position of last non-blank char in string.
C     DSA_FIND_REF  Look up reference name in common tables.
C     DSA_WRUSER    Output a message string to the user.
C     DSA_GET_ACTUAL_NAME  Get the full structure name from a ref_name.
C     DSA_ARRAY_EXIST  Determine if a named array exists.
C     DSA_ARRAY_SIZE   Determine dimensions of a named array.
C     DSA_AXIS_SIZE    Determine dimensions of axis data array.
C
C  History:
C     26th Aug 1988.   Original version.  KS / AAO.
C     14th Feb 1989.   Comments reformatted.  KS/AAO.
C     8th  Dec 1989.   Size requirements tightened - now must match
C                      corresponding axis array.  KS/AAO.
C     11th Dec 1989.   Now uses DSA__ routines for structure details.  KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992    Remove unused variable declarations. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_SEEK_WIDTH (REF_NAME,AXIS,EXIST,SINGLE,
     :                                                  WIDTH,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL EXIST, SINGLE
      CHARACTER*(*) REF_NAME
      INTEGER AXIS, STATUS
      DOUBLE PRECISION WIDTH
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
      INTEGER   DTA_STATUS                  ! Status returned by DTA_ routines.
      INTEGER   DIMS(MAX_AXES)              ! Dimensions of axis array
      INTEGER   DIMS2(MAX_AXES)             ! Dimensions of main data array
      CHARACTER ERROR*64                    ! Error string
      INTEGER   I                           ! Loop index through axis dimensions
      INTEGER   IGNORE                      ! Dummy status argument
      INTEGER   INVOKE                      ! Dummy function value
      INTEGER   LENGTH                      ! Object name length
      LOGICAL   MATCH                       ! Flags whether dimensions match
      CHARACTER NAME*80                     ! DTA_ name of data object
      INTEGER   NDIM                        ! # of dimensions in axis array
      INTEGER   NDIM2                       ! # of dimensions in main array
      INTEGER   NELM                        ! Elements in axis array - ignored
      CHARACTER NUMBER*12                   ! Formatted number
      CHARACTER OBJ_NAME*80                 ! DTA_ name of top level data object
      CHARACTER REF_NAME_UC*32              ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                    ! Reference table slot #
      LOGICAL   STRUCT                      ! Indicates object is a structure
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
      IF (STATUS.NE.0) GO TO 500
C
C     Get the name of the axis structure.
C
      IF ((AXIS.LT.1).OR.(AXIS.GT.MAX_AXES)) THEN
         NUMBER=ICH_CI(AXIS)
         CALL DSA_WRUSER('Unable to access the ')
         CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER))//GEN_NTH(AXIS))
         CALL DSA_WRUSER(' axis width data for the structure ')
         CALL DSA_GET_ACTUAL_NAME(REF_NAME_UC,STRUCTURE_NAME,STATUS)
         CALL DSA_WRUSER(STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
         CALL DSA_WRUSER('.  Probable programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__AXINV
         GO TO 500
      END IF
C
C     Generate the name of the data array and see if it exists.
C     First check explicitly for the possibility that it is a single
C     value.
C
      SINGLE=.FALSE.
      CALL DSA__AXIS_WIDTH_NAME (REF_SLOT,AXIS,NAME,LENGTH)
      CALL DTA_STRUC (NAME(:LENGTH),STRUCT,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
C
C        Doesn't exist at all
C
         EXIST=.FALSE.
      ELSE
C
C        Exists, one way or another.
C
         IF (.NOT.STRUCT) THEN
C
C           Exists as a primitive type.  See if it is scalar, and if
C           so get its value.
C
            CALL DTA_SZVAR (NAME(:LENGTH),1,NDIM,DIMS,DTA_STATUS)
            IF ((DTA_STATUS.EQ.0).AND.(NDIM.EQ.0)) THEN
               SINGLE=.TRUE.
               EXIST=.TRUE.
               CALL DTA_RDVARD (NAME(:LENGTH),1,WIDTH,DTA_STATUS)
               IF (DTA_STATUS.NE.0) THEN
                  CALL DSA_WRUSER('Unable to obtain axis width '//
     :                                               'value for the ')
                  NUMBER=ICH_CI(AXIS)
                  CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER))//
     :                                                  GEN_NTH(AXIS))
                  CALL DSA_WRUSER(' axis in the structure ')
                  IGNORE=0
                  CALL DSA_GET_ACTUAL_NAME (REF_NAME_UC,
     :                                        STRUCTURE_NAME,IGNORE)
                  CALL DSA_WRUSER(
     :                        STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
                  CALL DSA_WRUSER('. ')
                  CALL DTA_ERROR(DTA_STATUS,ERROR)
                  CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
                  CALL DSA_WRUSER('.')
                  CALL DSA_WRFLUSH
                  EXIST=.FALSE.
                  DTA_CODE=DTA_STATUS
                  STATUS=DSA__DTAERR
                  GO TO 500     ! Error exit
               END IF
            END IF
         END IF
C
         IF (.NOT.SINGLE) THEN
C
C           At this point, we know it exists and is not a scalar.
C
            CALL DSA_ARRAY_EXIST(NAME(:LENGTH),EXIST,STATUS)
C
C           If it exists, get its dimensions and check them against those of
C           the axis data array.
C
            IF ((STATUS.EQ.0).AND.EXIST) THEN
               CALL DSA_ARRAY_SIZE(NAME(:LENGTH),
     :                                 MAX_AXES,NDIM,DIMS,ERROR,STATUS)
               IF (NDIM.EQ.0) THEN
                  EXIST=.TRUE.
                  SINGLE=.TRUE.
               ELSE
                  SINGLE=.FALSE.
                  CALL DSA_AXIS_SIZE(REF_NAME,AXIS,MAX_AXES,NDIM2,
     :                                            DIMS2,NELM,STATUS)
                  IF (STATUS.NE.0) GO TO 500   ! Error exit
                  MATCH=NDIM.EQ.NDIM2
                  IF (MATCH) THEN
                     DO I=1,NDIM
                        IF (DIMS2(I).NE.DIMS(I)) MATCH=.FALSE.
                     END DO
                  END IF
                  IF (.NOT.MATCH) THEN
                     CALL DSA_GET_ACTUAL_NAME (REF_NAME_UC,
     :                                           STRUCTURE_NAME,IGNORE)
                     CALL DSA_WRUSER('The width array for the ')
                     NUMBER=ICH_CI(AXIS)
                     CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER))//
     :                                                  GEN_NTH(AXIS))
                     CALL DSA_WRUSER(' axis in the structure ')
                     CALL DSA_WRUSER(
     :                         STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
                     CALL DSA_WRUSER(
     :                         ' has dimensions which do not match')
                     CALL DSA_WRUSER(
     :                 ' those of the corresponding axis data array.')
                     CALL DSA_WRFLUSH
                     EXIST=.FALSE.
                  END IF
               END IF
            END IF
         END IF
C
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
