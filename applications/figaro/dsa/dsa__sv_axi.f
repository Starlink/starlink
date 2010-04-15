C+
C                   D S A _ _ S A V E _ A X I S
C
C  Routine name:
C     DSA__SAVE_AXIS
C
C  Function:
C     Saves or restores the axis information for a given axis.
C
C  Description:
C     This routine performs the structure dependent part of the
C     routines DSA_SAVE_AXIS and DSA_RESTORE_AXIS.   See those
C     routines for more information.  If the save or restore
C     fails, no error messages are output by this routine, but a
C     DTA error code is returned.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__SAVE_AXIS (REF_SLOT,AXIS,MODE,FROM,TO,DTA_STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (>) AXIS           (Integer,ref) The number of the axis in question.
C     (>) MODE           (Fixed string,descr) A string that starts either
C                        with 'S' for 'SAVE' or 'R' for 'RESTORE'.  Should
C                        be upper case, and is not tested for validity.
C     (<) FROM           (Fixed string,descr) The DTA name of the axis
C                        structure being saved (or restored from).  This is
C                        only provided so that the calling routine can use
C                        it to format a more useful error message.
C     (<) TO             (Fixed string,descr) The DTA name of the axis
C                        structure being restored (or being used to save
C                        the structure).
C     (<) DTA_STATUS     (Integer,ref) The DTA status code returned.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:
C     DTA_RNVAR, DTA_CVYAR, DTA_DLVAR, DSA__CREATE_EXTRA, DSA__CREATE_AXIS,
C     DSA__DELETE_AXIS
C
C  Prior requirements:
C     The structure must have been opened already and REF_SLOT must
C     be valid.  This is not tested by this routine.  You cannot SAVE
C     an already SAVED axis, and if you restore one, there must be no
C     existing axis.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DTA_RNVAR         Renames a data object.
C     DTA_CYVAR         Copy a data object.
C     DTA_DLVAR         Delete a data object.
C     DSA__CREATE_EXTRA Make sure the structure for additional info exists.
C     DSA__CREATE_AXIS  Make sure an axis structure exists.
C     DSA__DELETE_AXIS  Delete everything in a given axis structure.
C
C  Common variable details:
C     (>) OBJ_LEN       (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES     (String array) Name (as recognised by DTA_) of data
C                       object corresponding to reference name.
C     (>) NDF_FORMAT    (Logical array) Indicates structure format is Starlink's
C                       NDF format (described in SGP38).  If false, format is
C                       original Figaro format (DST files).
C
C  History:
C     14th Dec  1989.   Original version.  KS / AAO.
C     5th March 1990.   Support for NDF format added.  KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version supports both the original Figaro data structures
C     and Starlink's NDF structure.
C+
      SUBROUTINE DSA__SAVE_AXIS (REF_SLOT,AXIS,MODE,FROM,TO,DTA_STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, AXIS, DTA_STATUS
      CHARACTER*(*) MODE, FROM, TO
C
C     DSA system common
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      CHARACTER CHAR1*1        ! Character form of AXIS
C
C     Axis structure names.
C
      CHARACTER*6 AXIS_CHARS
      DATA AXIS_CHARS/'XYTUVW'/
C
C     Operation is different for the two formats.
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        NDF format.  Axis information is held in an element of a
C        structure array, so this can't be handled by a rename operation,
C        is has to be by an actual copy and delete.  We use structures
C        called `file.MORE.FIGARO.SAVE_AXISn' to save the data - these are
C        easier to delete than elements of a SAVE_AXIS array.
C
         CHAR1=CHAR(ICHAR('0')+AXIS)
         IF (MODE(1:1).EQ.'S') THEN
            CALL DSA__CREATE_EXTRA (REF_SLOT,DTA_STATUS)
            IF (DTA_STATUS.NE.0) GO TO 500     ! Error exit
            FROM=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.AXIS['//
     :                                                    CHAR1//']'
            TO=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                               '.MORE.FIGARO.SAVE_AXIS'//CHAR1
            CALL DTA_CYVAR (FROM,TO,DTA_STATUS)
            IF (DTA_STATUS.NE.0) GO TO 500     ! Error exit
            CALL DSA__DELETE_AXIS (REF_SLOT,AXIS,DTA_STATUS)
         ELSE
            FROM=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                               '.MORE.FIGARO.SAVE_AXIS'//CHAR1
            TO=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.AXIS['//
     :                                                    CHAR1//']'
            CALL DSA__CREATE_AXIS (REF_SLOT,AXIS,DTA_STATUS)
            IF (DTA_STATUS.NE.0) GO TO 500     ! Error exit
            CALL DTA_CYVAR (FROM,TO,DTA_STATUS)
            IF (DTA_STATUS.NE.0) GO TO 500     ! Error exit
            CALL DTA_DLVAR (FROM,DTA_STATUS)
         END IF
      ELSE
C
C        Original Figaro format.  Axis structures are individual sub-
C        structures, and the whole save or restore operation is just a
C        rename.  First generate 'FROM' and 'TO', using MODE.
C
         IF (MODE(1:1).EQ.'S') THEN
            FROM=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.'//
     :                                   AXIS_CHARS(AXIS:AXIS)
            TO=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.SAVE_'//
     :                                   AXIS_CHARS(AXIS:AXIS)
         ELSE
            TO=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.'//
     :                                   AXIS_CHARS(AXIS:AXIS)
            FROM=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.SAVE_'//
     :                                   AXIS_CHARS(AXIS:AXIS)
         END IF
C
C        Now perform the save (or restore) by renaming the structure
C
         CALL DTA_RNVAR (FROM,TO,DTA_STATUS)
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
