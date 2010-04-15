C+
C                   D S A _ _ S E E K _ F L A G G E D
C
C  Routine name:
C     DSA__SEEK_FLAGGED
C
C  Function:
C     Determined if the main data array in a structure has `flagged' data.
C
C  Description:
C    Given the reference slot number in the DSA common tables for an
C    already open structure, this routine determines whether or not the
C    main data array holds flagged pixel values.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__SEEK_FLAGGED (REF_SLOT,FLAGGED,DTA_STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (<) FLAGGED        (Logical,ref) True if the data array contains
C                        (or may contain) flagged data.
C     (<) DTA_STATUS     (Integer,ref) The DTA status code returned.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:
C     DTA_RDVARI, DTA_STRUC.
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
C     DTA_RDVARI        Read an integer from a data object.
C     DTA_STRUC         Tests if a data object is a structure.
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
C     2nd  March 1990.   Original version.  KS / AAO.
C     21st Aug 1992      Automatic portability modifications
C                        ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992      "INCLUDE" filenames now upper case. KS/AAO
C     4th  Sep 1992      Changed .BAD_PIXEL to .DATA_ARRAY.BAD_PIXEL.
C                        HME/UoE.
C     15th Mar 1994.     Now allows both for structured and simple arrays
C                        in NDF format files. KS/AAO.
C
C  Note:
C     This version supports both the original Figaro data structures
C     and Starlink's NDF format.  For NDF format, it assumes the flag
C     is .BAD_PIXEL at top level if .DATA_ARRAY is a simple array. If
C     .DATA_ARRAY is itself a structure, it looks first for a .BAD_PIXEL
C     in that array structure, then falls back on a global .BAD_PIXEL
C     at top level.
C+
      SUBROUTINE DSA__SEEK_FLAGGED (REF_SLOT,FLAGGED,DTA_STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL FLAGGED
      INTEGER REF_SLOT, DTA_STATUS
C
C     DSA system common
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   FLAG             ! Used for the flag value
      LOGICAL   FOUND            ! True if we find an NDF data flag.
      CHARACTER OBJECT*32        ! DTA system name for flag value
      LOGICAL   STRUCT           ! True if data array is a structure.
C
C     We treat the two file formats separately, since the NDF case is
C     quite complex.
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        This is the NDF format case. We need first of all to find out
C        if the data array is a structure. If so, we look for a .BAD_PIXEL
C        item in that structure. If we find one, this has priority.  If we
C        don't find one (this, incidentally, will also be the case where the
C        top level object we're dealing with is the data array itself), then
C        we look for a .BAD_PIXEL flag at top-level.
C
         FOUND=.FALSE.
         CALL DTA_STRUC (OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                                 '.DATA_ARRAY',STRUCT,DTA_STATUS)
C
C        If we get bad status we assume that top_level.DATA_ARRAY doesn't
C        exist. If that's the case, we either don't have a valid file - but
C        no doubt we'll find that out later! - or we have the main array as
C        the top-level object. If we have good status AND a structure, then
C        we need to look first for .DATA_ARRAY.BAD_PIXEL.
C
         IF ((DTA_STATUS.EQ.0).AND.STRUCT) THEN
            OBJECT=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                                        '.DATA_ARRAY.BAD_PIXEL'
            CALL DTA_RDVARI (OBJECT,1,FLAG,DTA_STATUS)
            IF (DTA_STATUS.EQ.0) FOUND=.TRUE.
         END IF
C
C        If FOUND is set, we did find a .DATA_ARRAY.BAD_PIXEL and FLAG
C        contains its value. If not, we need to look for .BAD_PIXEL at
C        top level.
C
         IF (.NOT.FOUND) THEN
            OBJECT=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.BAD_PIXEL'
            CALL DTA_RDVARI (OBJECT,1,FLAG,DTA_STATUS)
            IF (DTA_STATUS.EQ.0) FOUND=.TRUE.
         END IF
C
C        Now, we've tried all the possibilities. FOUND is true if we
C        found a bad pixel flag, in which case the data is flagged if this
C        has a non-zero value (which we have in FLAG). Otherwise, if we
C        didn't find a flag at all, the decreed NDF default is that the
C        data is to be assumed flagged.
C
         IF (FOUND) THEN
            FLAGGED=FLAG.NE.0
         ELSE
            FLAGGED=.TRUE.
         END IF
C
      ELSE
C
C        This is the DST format case. Here the bad pixel flag is always
C        called .Z.FLAGGED. If this is set to 1, there are flagged values
C        in the data, and if it is zero or doesn't exist at all, then
C        the data is unflagged.
C
         OBJECT=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.Z.FLAGGED'
         CALL DTA_RDVARI (OBJECT,1,FLAG,DTA_STATUS)
         IF (DTA_STATUS.EQ.0) THEN
            FLAGGED=FLAG.NE.0
         ELSE
            DTA_STATUS=0
            FLAGGED=.FALSE.
         END IF
      END IF
C
C     Note that we're a bit slack with DTA_STATUS - if it's bad, we've
C     assumed that's because the flag doesn't exist, which is OK, so we
C     just clear the status.
C
      DTA_STATUS=0
C
      END
