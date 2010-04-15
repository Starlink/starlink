C+
C                      D S A _ C H E C K _ S T R U C T U R E
C
C  Routine name:
C     DSA_CHECK_STRUCTURE
C
C  Function:
C     Tidies up a structure just prior to its closedown.
C
C  Description:
C     This routine should be called just before a structure is shut
C     down.  It checks it for internal consistiency, testing a) the data
C     range structure, invalidating it if necessary, b) the error array,
C     deleting it if it has not been modified but the data has, c) the
C     axis structure, checking it against the data shape if either have
C     changed, deleting any axis structures that are no longer required
C     and checking those that are left for mismatches against the data
C     array, d) any reshaped arrays, deleting any that have been
C     reshaped but not updated. Finally - for an NDF - a check for the
C     adherence of the axis structure to SUN/33 is made. (If .AXIS
C     exists at all, it must have one element per data dimension and
C     each .AXIS(i) must have a DATA_ARRAY. Also each DATA_ARRAY must be
C     one-dimensional with the same dimension as the corresponding data
C     dimension, but this is not checked.)
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_CHECK_STRUCTURE (REF_SLOT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The common table slot used by
C                        the structure to be closed.
C     (!) STATUS         (Integer,ref) Status code.  If bad status is
C                        passed to this routine it returns immediately.
C                        This routine always returns good status if
C                        it runs.
C
C  External subroutines / functions used:
C     DTA_SZVAR, DTA_DLVAR, DSA_ARRAY_SIZE, DSA_ARRAY_EXIST,
C     DSA_MAIN_SIZE, DSA_WRUSER, ICH_LEN, GEN_NTH, DSA__INVALIDATE_RANGE,
C     DSA__ERROR_NAME, DSA__DELETE_QUALITY, DSA__AXIS_DATA_NAME,
C     DSA__AXIS_WIDTH_NAME, DSA__DELETE_AXIS, DSA_CHECK_NDF_AXIS
C
C  Prior requirements:
C     This routine is a service routine called as part of structure
C     shutdown.  It uses items in common that may be modified as part
C     of that shutdown, so the point at which it is called is quite
C     important.  It is intended to be called after the structure
C     mapping has been checked, but before any other changes are made.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 11th December 1995
C
C  Note:
C     This is an DSA internal routine and should not be called directly
C     from outside the DSA package.
C-
C  Subroutine / function details:
C     DTA_DLVAR             Delete a data object
C     DTA_SZVAR             Get size of a data object
C     DSA_ARRAY_EXIST       Test for existence of an array
C     DSA_ARRAY_SIZE        Get size of an array
C     DSA_CHECK_NDF_AXIS    Check the AXIS structure in an NDF
C     DSA_MAIN_SIZE         Get size of main data array
C     DSA_WRUSER            Output message to user
C     DSA__INVALIDATE_RANGE Set range structure invalid
C     DSA__ERROR_NAME       Get name of error array in structure
C     DSA__DELETE_QUALITY   Delete quality information from structure
C     DSA__AXIS_DATA_NAME   Get name of axis data array in structure
C     DSA__AXIS_WIDTH_NAME  Get name of axis width array in structure
C     DSA__DELETE_AXIS      Delete an entire axis from a structure
C     ICH_LEN               Position of last non-blank char in string
C     GEN_NTH               Get 'st', 'nd', 'rd' etc given a number
C
C  Common variable details:
C     (>) ACTUAL_NAMES  (String array) The fully extended name for the
C                       structure.
C     (>) DATA_UPDATE   (Logical array) Indicates that the data array has
C                       been updated (or at least, mapped for update).
C     (>) ERROR_UPDATE  (Logical array) Indicates that the error array has
C                       been updated (or at least, mapped for update).
C     (>) MAX_AXES      (Integer parameter) Maximum number of axes.
C     (>) OBJ_LEN       (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES     (String array) Name (as recognised by DTA_) of data
C                       object corresponding to reference name.
C     (>) RANGE_UPDATE  (Logical array) Indicates that the data range values
C                       have been updated.
C     (>) REF_USED      (Logical array) Inidcates reference slot in use.
C     (>) SHAPE_CHECK   (Logical array) Indicates that the data or axis
C                       arrays have been changed and should be checked for
C                       consistiency on closedown.
C  History:
C     4th  Jul 1988  Original version.  KS / AAO.
C     11th Dec 1989  Now deletes reshaped arrays that were not updated.
C                    All knowledge of structure contents moved into
C                    DSA__ routines.  KS/AAO.
C     17th Jan 1990  Calling sequence for DSA__ERROR_NAME changed. KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992  Remove unused variable declarations. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     16th Oct 1992  HME / UoE, Starlink.  Call DSA_ENCDIM instead of
C                    FIG_ENCDIM.
C                    Check the AXIS structure array for the NDF case.
C      7th Feb 1995  KS/AAO. Warns about but no longer deletes quality
C                    arrays when they might have become invalid.
C     28th Nov 1995  Now deletes the quality array in in the case where the
C                    data has been reshaped and the quality array has not
C                    been modified. KS/AAO.
C     11th Dec 1995  Now uses DSA__DELETE_QUALITY to delete the quality array,
C                    to make sure the whole structure is deleted. KS/AAO.
C+
      SUBROUTINE DSA_CHECK_STRUCTURE (REF_SLOT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, STATUS
C
C     Functions used
C
      INTEGER ICH_LEN
      CHARACTER*2 GEN_NTH
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   AXIS                ! Loop variable through axes
      INTEGER   DIMS(MAX_AXES)      ! Dimensions of axis data array
      INTEGER   DTA_STATUS          ! Status from DTA calls
      INTEGER   ERR_TYPE            ! Code giving type of error info - ignored
      LOGICAL   EXIST               ! Flags that axis array exists
      LOGICAL   FLAGS(MAX_AXES)     ! Flags dimensions in data
      INTEGER   I                   ! Loop index through axis dimensions
      INTEGER   IPT                 ! Pointer to dimension string
      INTEGER   J                   ! Loop index through data dimensions
      INTEGER   LENGTH              ! Number of characters in NAME
      INTEGER   MAIN_DIM            ! # dimensions of main data array
      INTEGER   MAIN_DIMS(MAX_AXES) ! Dimensions of main data array
      LOGICAL   MATCH               ! True if axis dimensions match data
      CHARACTER NAME*80             ! Name of DTA object in structure
      INTEGER   NDIM                ! # dimensions of axis data array
      INTEGER   NDWID               ! # dimensions of axis width array
      CHARACTER NTH*3               ! '1st', '2nd', etc.
      CHARACTER STRING*64           ! Errors and dimension formatting
      INTEGER   WDIMS(MAX_AXES)     ! Dimensions of axis data array
C
C     Return immediately if bad status passed.
C
      IF (STATUS.NE.0) RETURN
C
      IF (REF_USED(REF_SLOT)) THEN
C
C        Check for data reshaped but not updated - this surely is a
C        programming error, so say so.  However, deleting the main
C        data array under these circumstances seems a bit heavy-handed.
C        Setting the update flag here makes subsequent tests easier,
C        even if it isn't strictly true.
C
         IF (DATA_RESHAPE(REF_SLOT).AND..NOT.DATA_UPDATE(REF_SLOT)) THEN
            CALL DSA_WRUSER(
     :                 'Warning: the main data array of the structure ')
            CALL DSA_WRUSER(ACTUAL_NAMES(REF_SLOT)
     :                 (:ICH_LEN(ACTUAL_NAMES(REF_SLOT))))
            CALL DSA_WRUSER(' has been reshaped but the data has not '//
     :                 'been modified.  Probable programming error.')
            CALL DSA_WRFLUSH
            DATA_UPDATE(REF_SLOT)=.TRUE.
         END IF
C
C        Check the range structure
C
         IF (DATA_UPDATE(REF_SLOT).AND..NOT.RANGE_UPDATE(REF_SLOT)) THEN
            CALL DSA__INVALIDATE_RANGE(REF_SLOT,DTA_STATUS)
         END IF
C
C        Check for errors unchanged while data modified
C
         IF (DATA_UPDATE(REF_SLOT).AND..NOT.ERROR_UPDATE(REF_SLOT)) THEN
            CALL DSA__ERROR_NAME(REF_SLOT,NAME,LENGTH,ERR_TYPE)
            CALL DTA_DLVAR(NAME(:LENGTH),DTA_STATUS)
            IF ((DTA_STATUS.EQ.0).AND.LOG_DELETES) THEN
               CALL DSA_WRUSER(
     :                        'Note: The error array in the structure ')
               CALL DSA_WRUSER(ACTUAL_NAMES(REF_SLOT)
     :                 (:ICH_LEN(ACTUAL_NAMES(REF_SLOT))))
               CALL DSA_WRUSER(
     :                        ' is now invalid and has been deleted.')
               CALL DSA_WRFLUSH
            END IF
         END IF
C
C        Check for quality array unchanged while data modified. In this case,
C        we delete the array if there has been a shape change, but leave it
C        while issuing a warning if the shape has been left the same. The
C        check on SHAPE_CHECK isn't quite as rigorous as we might like - it
C        might refer just to an axis change - but is easier than coding a
C        full check on quality and main array dimensions.
C
         IF (DATA_UPDATE(REF_SLOT).AND..NOT.QUAL_UPDATE(REF_SLOT)
     :                      .AND.(QUAL_EXIST(REF_SLOT).GT.0)) THEN
            IF (SHAPE_CHECK(REF_SLOT)) THEN
               CALL DSA__DELETE_QUALITY(REF_SLOT,DTA_STATUS)
               IF ((DTA_STATUS.EQ.0).AND.LOG_DELETES) THEN
                  CALL DSA_WRUSER(
     :                    'Note: The quality array in the structure ')
                  CALL DSA_WRUSER(ACTUAL_NAMES(REF_SLOT)
     :                 (:ICH_LEN(ACTUAL_NAMES(REF_SLOT))))
                  CALL DSA_WRUSER(
     :                        ' is now invalid and has been deleted.')
                  CALL DSA_WRFLUSH
               END IF
            ELSE
               CALL DSA_WRUSER(
     :                  'Warning: The quality array in the structure ')
               CALL DSA_WRUSER(ACTUAL_NAMES(REF_SLOT)
     :              (:ICH_LEN(ACTUAL_NAMES(REF_SLOT))))
               CALL DSA_WRUSER(' is probably now invalid. The data ')
               CALL DSA_WRUSER ('array has been modified by a program '
     :            //'that ignored all data quality information.')
               CALL DSA_WRFLUSH
            END IF
         END IF
C
C        See if the shape of the data and or axes has changed.
C
         CALL DSA_MAIN_SIZE(REF_SLOT,.FALSE.,MAX_AXES,MAIN_DIM,
     :                                      MAIN_DIMS,STRING,STATUS)
         IF (SHAPE_CHECK(REF_SLOT)) THEN
C
C           Quietly delete any axes that are now surplus to requirements
C
            DO AXIS=MAX_AXES,MAIN_DIM+1,-1
               CALL DSA__DELETE_AXIS(REF_SLOT,AXIS,DTA_STATUS)
            END DO
C
C           Check that those axes that are still relevant match the
C           main data array dimensions.  The check is quite messy - see
C           DSA_SEEK_AXIS (from which it is lifted) for more details.
C           If a mismatch is found, flag it - it is surely the fault
C           of the program, and delete the axis structure.
C
            DO AXIS=1,MAIN_DIM
               STATUS=0
               NTH=CHAR(ICHAR('0')+AXIS)//GEN_NTH(AXIS)
               CALL DSA__AXIS_DATA_NAME(REF_SLOT,AXIS,NAME,LENGTH)
               CALL DSA_ARRAY_EXIST(NAME(:LENGTH),EXIST,STATUS)
               IF ((STATUS.EQ.0).AND.EXIST) THEN
                  CALL DSA_ARRAY_SIZE(NAME(:LENGTH),MAX_AXES,NDIM,
     :                                               DIMS,STRING,STATUS)
                  MATCH=((NDIM.LE.MAIN_DIM).AND.
     :                                    (DIMS(1).EQ.MAIN_DIMS(AXIS)))
                  IF (MATCH) THEN
                     DO J=1,MAIN_DIM
                        FLAGS(J)=.FALSE.
                     END DO
                     FLAGS(AXIS)=.TRUE.
                     DO I=2,NDIM
                        MATCH=.FALSE.
                        DO J=1,MAIN_DIM
                           IF (DIMS(I).EQ.MAIN_DIMS(J)) THEN
                              FLAGS(J)=.TRUE.
                              MATCH=.TRUE.
                              GO TO 340             ! Break out of J loop
                           END IF
                        END DO
  340                   CONTINUE
                        IF (.NOT.MATCH) GO TO 360   ! Break out of I loop
                     END DO
  360                CONTINUE
                  END IF
                  IF (.NOT.MATCH) THEN
                     CALL DSA_WRUSER('The data array for the '//NTH//
     :                                         'axis of the structure ')
                     CALL DSA_WRUSER(ACTUAL_NAMES(REF_SLOT)
     :                               (:ICH_LEN(ACTUAL_NAMES(REF_SLOT))))
                     CALL DSA_WRUSER(' has dimensions ')
                     IPT=1
                     CALL DSA_ENCDIM(STRING,NDIM,DIMS,IPT)
                     CALL DSA_WRUSER(STRING(:IPT))
                     CALL DSA_WRUSER(
     :                   ' which are incompatible with the dimensions ')
                     IPT=1
                     CALL DSA_ENCDIM(STRING,MAIN_DIM,MAIN_DIMS,IPT)
                     CALL DSA_WRUSER(STRING(:IPT))
                     CALL DSA_WRUSER(' of the main data array.')
                     CALL DSA_WRUSER(' Probable programming error.')
                     CALL DSA_WRUSER(
     :                         ' The axis data has now been deleted.')
                     CALL DSA_WRFLUSH
                     CALL DSA__DELETE_AXIS(REF_SLOT,AXIS,DTA_STATUS)
                  ELSE
C
C                    At this point, there has been a reshape performed
C                    and the axis that has just been checked does match
C                    the dimensions of the main data array.  Now, if this
C                    axis was reshaped, check that it's data array and
C                    width arrays (if they exist) have been updated and
C                    delete them if not.
C
                     IF (AXIS_RESHAPE(AXIS,REF_SLOT)) THEN
                        IF (.NOT.AXIS_UPDATE(AXIS,REF_SLOT)) THEN
                           CALL DSA__AXIS_DATA_NAME(REF_SLOT,AXIS,
     :                                                      NAME,LENGTH)
                           CALL DTA_DLVAR(NAME(:LENGTH),DTA_STATUS)
                           IF ((DTA_STATUS.EQ.0).AND.LOG_DELETES) THEN
                              CALL DSA_WRUSER(
     :                           'Note: The data array for the '//NTH//
     :                                       ' axis in the structure ')
                              CALL DSA_WRUSER(ACTUAL_NAMES(REF_SLOT)
     :                               (:ICH_LEN(ACTUAL_NAMES(REF_SLOT))))
                              CALL DSA_WRUSER(' is now invalid and has '
     :                                             //'been deleted.')
                              CALL DSA_WRFLUSH
                           END IF
                        END IF
C
C                       Note that the width information may be just
C                       a single value, in which case it won't have been
C                       updated.  We have to allow for that.
C
                        IF (.NOT.WIDTH_UPDATE(AXIS,REF_SLOT)) THEN
                           CALL DSA__AXIS_WIDTH_NAME(REF_SLOT,AXIS,
     :                                                     NAME,LENGTH)
                           CALL DTA_SZVAR(NAME(:LENGTH),MAX_AXES,NDWID,
     :                                                WDIMS,DTA_STATUS)
                           IF ((DTA_STATUS.EQ.0).AND.(NDWID.GE.1)) THEN
                              CALL DTA_DLVAR(NAME(:LENGTH),DTA_STATUS)
                              IF ((DTA_STATUS.EQ.0).AND.LOG_DELETES)
     :                                                             THEN
                                 CALL DSA_WRUSER(
     :                           'Note: The width array for the '//NTH//
     :                                        ' axis in the structure ')
                                 CALL DSA_WRUSER(ACTUAL_NAMES(REF_SLOT)
     :                               (:ICH_LEN(ACTUAL_NAMES(REF_SLOT))))
                                 CALL DSA_WRUSER(' is now invalid and '
     :                                        //'has been deleted.')
                                 CALL DSA_WRFLUSH
                              END IF
                           END IF
                        END IF
                     END IF
                  END IF
               END IF
            END DO
C
C           Just when we think the axes are OK, with all unecessary ones
C           deleted, we finally check that the axis information adheres
C           to the rules imposed by the NDF library.
C           We try to do this check even when above no need to check
C           axes was seen. This is to repair existing illegal NDFs.
C           There may be problems with file protection or HDS access
C           modes. Such problems would not occur if we move the call to
C           within this IF-block, because access would be write or
C           update.
C           If and only if the NDF axis check is outside this IF block,
C           then the above DSA_MAIN_SIZE should be outside as well.
C
         END IF
         CALL DSA_CHECK_NDF_AXIS(REF_SLOT,MAIN_DIM,MAIN_DIMS,STATUS)
      END IF
      STATUS=0
C
  500 CONTINUE
C
      END

