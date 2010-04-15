C+
C                  D S A _ _ C L E A R _ D A T A _ E N V
C
C  Routine name:
C     DSA__CLEAR_DATA_ENV
C
C  Function:
C     Clears out the environment for the main data array in a structure.
C
C  Description:
C     This routine, given the reference slot number for a structure,
C     clears out anything in that structure that is associated directly
C     with the main data array.  This is not a useful general thing to
C     do, but is needed for routines like DSA_RESHAPE_DATA which are
C     about to totally reorganise the data array and all its associated
C     paraphenalia, and so need a clean slate to work on.  This routine
C     outputs warning messages if it finds items in the structure that it
C     knows it doesn't expect (it can do this for NDF structures, for
C     example, because what they can contain is well-defined).
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__CLEAR_DATA_ENV (REF_SLOT,DTA_STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (<) DTA_STATUS     (Integer,ref) The DTA system status code from the
C                        operation.  0 => OK.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:
C     DTA_NMVAR, DTA_DLVAR, DSA__NDF-TOP_NAME
C
C  Prior requirements:
C     The structure must have been opened already and REF_SLOT must
C     be valid.  This is not tested by this routine.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DTA_DLVAR         Delete  named data object.
C     DTA_NMVAR         Get name of nth data object in structure.
C     DSA__NDF_TOP_NAME Check on object at top level of NDF structure.
C
C  Common variable details:
C     (>) NDF_FORMAT    (Logical array) Indicates structure format is Starlink's
C                       NDF format (described in SGP38).  If false, format is
C                       original Figaro format (DST files).
C     (>) OBJ_LEN       (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES     (String array) Name (as recognised by DTA_) of data
C                       object corresponding to reference name.
C
C  History:
C     26th Feb  1990.   Original version.  KS / AAO.
C     12th Mar  1990.   WARN parameter added to DSA__NDF_TOP_ITEM_CALL. KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version is for the both the original Figaro data structures
C     and the Starlink defined NDF structures.
C+
      SUBROUTINE DSA__CLEAR_DATA_ENV (REF_SLOT,DTA_STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, DTA_STATUS
C
C     DSA system common
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      LOGICAL   AXIS_ITEM          ! Set if item is part of axis data - ignored
      INTEGER   IPOS               ! Index into NDF structure items
      LOGICAL   MORE               ! Controls loop through NDF items
      CHARACTER NAME*64            ! Name of structure item for NDF files
      LOGICAL   USE                ! Indicates item is associated with data
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        For NDF format this operation is complicated by the fact
C        that the things in question aren't collected in a single
C        structure that can be deleted wholesale, but are at top level
C        together with a load of other stuff that shouldn't be
C        deleted.  DSA__NDF_TOP_NAME knows the difference.
C
         MORE=.TRUE.
         IPOS=1
         DO WHILE (MORE)
            CALL DTA_NMVAR (OBJ_NAMES(REF_SLOT),IPOS,NAME,DTA_STATUS)
            IF (DTA_STATUS.NE.0) THEN
               DTA_STATUS=0
               MORE=.FALSE.
            ELSE
C
C              If the item is associated with the data, we delete it.
C              Otherwise, we increment IPOS to look at the next item.
C              Note that deleting a item will pull the next item down
C              in the contents, so we don't increment IPOS in that case.
C
               CALL DSA__NDF_TOP_NAME (OBJ_NAMES(REF_SLOT),NAME,.FALSE.,
     :                                                    USE,AXIS_ITEM)
               IF (USE) THEN
                  CALL DTA_DLVAR (
     :                    OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                                          '.'//NAME,DTA_STATUS)
               ELSE
                  IPOS=IPOS+1
               END IF
           END IF
         END DO
      ELSE
C
C        For the original Figaro format, it's easy.  (But then, the
C        need for this routine came from stuff originally designed
C        for that format)
C
         CALL DTA_DLVAR(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.Z',
     :                                                      DTA_STATUS)
      END IF
C
      END
