C+
C                      D S A _ S E T _ W I D T H
C
C  Routine name:
C     DSA_SET_WIDTH
C
C  Function:
C     Sets a single axis width value.
C
C  Description:
C     If a data structure contains, or is to contain, a single width
C     value, as opposed to a width array, then this routine can be used
C     to set such a value.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SET_WIDTH (REF_NAME,AXIS,WIDTH,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (>) AXIS         (Integer,ref) The number of the axis in question.
C     (>) WIDTH        (Double,ref) The single width value to be set.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ system.
C
C  External subroutines / functions used:
C     ICH_CI, ICH_LEN, DSA_FIND_REF, DSA_WRUSER, DSA_GET_ACTUAL_NAME,
C     GEN_NTH, ICH_FOLD, DTA_SZVAR, DTA_STRUC, DTA_WRVARD, DTA_ERROR,
C     DSA__AXIS_WIDTH_NAME
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
C     DTA_WRVARD    Write a double precision data object
C     DTA_ERROR     Get text for a DTA error code
C     GEN_NTH       Returns 'st','th','rd' etc appropriate to a number.
C     ICH_CI        Return an integer as a character string.
C     ICH_FOLD      Convert string to upper case.
C     ICH_LEN       Position of last non-blank char in string.
C     DSA_FIND_REF  Look up reference name in common tables.
C     DSA_WRUSER    Output a message string to the user.
C     DSA_GET_ACTUAL_NAME  Get the full structure name from a ref_name.
C
C  History:
C     24th Mar 1991.   Original version.  KS / AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_SET_WIDTH (REF_NAME,AXIS,WIDTH,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
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
      CHARACTER ERROR*64                    ! Error string
      LOGICAL   EXIST                       ! True if width info exists already
      INTEGER   IGNORE                      ! Dummy status argument
      INTEGER   INVOKE                      ! Dummy function value
      INTEGER   LENGTH                      ! Object name length
      CHARACTER NAME*80                     ! DTA_ name of data object
      INTEGER   NDIM                        ! # of dimensions in axis array
      CHARACTER NUMBER*12                   ! Formatted number
      CHARACTER OBJ_NAME*80                 ! DTA_ name of top level data object
      CHARACTER REF_NAME_UC*32              ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                    ! Reference table slot #
      LOGICAL   SINGLE                      ! True if width is single valued.
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
C
C     Make sure AXIS is valid.
C
      CALL DSA_VALIDATE_AXIS (AXIS,REF_NAME_UC,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Generate the name of the width array and see if it exists. If
C     it does exist, is it a scalar quantity or an array?
C
      SINGLE=.FALSE.
      CALL DSA__AXIS_WIDTH_NAME (REF_SLOT,AXIS,NAME,LENGTH)
      CALL DTA_STRUC (NAME(:LENGTH),STRUCT,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
         EXIST=.FALSE.
      ELSE
         EXIST=.TRUE.
         IF (.NOT.STRUCT) THEN
            CALL DTA_SZVAR (NAME(:LENGTH),1,NDIM,DIMS,DTA_STATUS)
            IF ((DTA_STATUS.EQ.0).AND.(NDIM.EQ.0)) THEN
               SINGLE=.TRUE.
               EXIST=.TRUE.
            END IF
         END IF
      END IF
C
C     If it exists and is not a simple scalar value, we need to
C     delete it first.
C
      IF (EXIST.AND.(.NOT.SINGLE)) THEN
         CALL DTA_DLVAR(NAME(:LENGTH),DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_WRUSER('Unable to delete existing axis width '//
     :                                    'array for the ')
            NUMBER=ICH_CI(AXIS)
            CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER))//GEN_NTH(AXIS))
            CALL DSA_WRUSER(' axis in the structure ')
            IGNORE=0
            CALL DSA_GET_ACTUAL_NAME (REF_NAME_UC,
     :                                        STRUCTURE_NAME,IGNORE)
            CALL DSA_WRUSER(STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
            CALL DSA_WRUSER('. ')
            CALL DTA_ERROR(DTA_STATUS,ERROR)
            CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
            CALL DSA_WRUSER('.')
            CALL DSA_WRFLUSH
            DTA_CODE=DTA_STATUS
            STATUS=DSA__DTAERR
            GO TO 500     ! Error exit
         ELSE
            EXIST=.FALSE.
         END IF
      END IF
C
C     If it didn't exist, (or doesn't now) we need to create it.  (We
C     don't bother to check status here, since if the create fails, so will
C     the subsequent write.)
C
      IF (.NOT.EXIST) CALL DTA_CRVAR(NAME(:LENGTH),'DOUBLE',DTA_STATUS)
C
C     Now write to it.
C
      CALL DTA_WRVARD(NAME(:LENGTH),1,WIDTH,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
         CALL DSA_WRUSER('Unable to single axis width value for the ')
         NUMBER=ICH_CI(AXIS)
         CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER))//GEN_NTH(AXIS))
         CALL DSA_WRUSER(' axis in the structure ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME (REF_NAME_UC,STRUCTURE_NAME,IGNORE)
         CALL DSA_WRUSER(STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
         CALL DSA_WRUSER('. ')
         CALL DTA_ERROR(DTA_STATUS,ERROR)
         CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
         CALL DSA_WRUSER('.')
         CALL DSA_WRFLUSH
         DTA_CODE=DTA_STATUS
         STATUS=DSA__DTAERR
         GO TO 500     ! Error exit
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
