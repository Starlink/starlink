C+
C                   D S A _ _ C R E A T E _ O B J E C T
C
C  Routine name:
C     DSA__CREATE_OBJECT
C
C  Function:
C     Creates an item to hold the object name for a data structure.
C
C  Description:
C    Given the reference slot number in the DSA common tables for an
C    already open structure, this routine creates an item in it to hold
C    the object name.  It will return an error status if the object
C    already exists.  If the upper levels of the structure required
C    for the object do not exist, they will be created as well.
C    No error messages are output if this routine fails but the
C    DTA error code from the create call is returned.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__CREATE_OBJECT (REF_SLOT,NCHAR,DTA_STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (>) NCHAR          (Integer,ref) The number of characters to be
C                        used for the object name.
C     (<) DTA_STATUS     (Integer,ref) The DTA status code from the delete
C                        operation.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:  DTA_CRVAR.
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
C     DTA_CRVAR         Creates a data object.
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
C     16th Jan  1990.   Original version.  KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version can handle either the original Figaro data structure,
C     or Starlink's NDF data structure.
C+
      SUBROUTINE DSA__CREATE_OBJECT (REF_SLOT,NCHAR,DTA_STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, NCHAR, DTA_STATUS
C
C     DSA system common
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      CHARACTER NAME*80       ! Name of object name data item.
C
C     Generate the name of the data object to be created.
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
         CALL DTA_CRNAM(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT)),'TITLE',
     :                                          1,NCHAR,NAME,DTA_STATUS)
      ELSE
         CALL DTA_CRNAM(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.OBS',
     :                                 'OBJECT',1,NCHAR,NAME,DTA_STATUS)
      END IF
C
C     If name generated OK, try to create it.
C
      IF (DTA_STATUS.EQ.0) THEN
         CALL DTA_CRVAR(NAME,'CHAR',DTA_STATUS)
C
C        If it failed, it may be that the environment didn't exist.  This
C        isn't possible in NDF format, where TITLE is a top-level object,
C        but it is possible in original Figaro format.  In this format,
C        try to create the .OBS structure first, then try again.
C
         IF ((DTA_STATUS.NE.0).AND..NOT.NDF_FORMAT(REF_SLOT)) THEN
            CALL DTA_CRVAR(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                                     '.OBS','STRUCT',DTA_STATUS)
            IF (DTA_STATUS.EQ.0) CALL DTA_CRVAR(NAME,'CHAR',DTA_STATUS)
         END IF
      END IF
C
      END
