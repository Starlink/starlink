C+
C                   D S A _ _ S E T _ F L A G G E D
C
C  Routine name:
C     DSA__SET_FLAGGED
C
C  Function:
C     Sets (or clears) the `data flagged' flag for a structure.
C
C  Description:
C    Given the reference slot number in the DSA common tables for an
C    already open structure, this routine sets (or clears) the flag that
C    indicates whether or not the main data array holds flagged pixel
C    values. No error messages are output if this fails, but a DTA
C    error code is returned.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__SET_FLAGGED (REF_SLOT,FLAGGED,DTA_STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common
C                        tables.
C     (>) FLAGGED        (Logical,ref) True if the flag is to be set (ie
C                        there is flagged data), false if it is to be
C                        cleared.
C     (<) DTA_STATUS     (Integer,ref) The DTA status code returned.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:
C     DTA_CRVAR, DTA_WRVARI.
C
C  Prior requirements:
C     The structure must have been opened already and REF_SLOT must
C     be valid.  This is not tested by this routine.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 1st February 1995.
C-
C  Subroutine / function details:
C     DTA_CRVAR         Create a named data object.
C     DTA_WRVARI        Write an integer to a data object.
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
C     13th Dec  1989.   Original version.  KS / AAO.
C     1st  Mar  1990.   Support for NDF files added.  KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C     4th  Sep 1992     DSA wrongly assumed BAD_PIXEL to be at the top
C                       level of the NDF. In fact it is in
C                       .DATA_ARRAY.BAD_PIXEL and can exist only in
C                       simple NDFs. In primitive NDFs it is always
C                       assumed TRUE. So I had to re-write the NDF part
C                       of this routine. HME/UoE, Starlink.
C     8th  Sep 1992     Reset DTA_STATUS before IF(.NOT.FLAGGED) block
C                       rather than inside. Otherwise the attempt to set
C                       the flag TRUE falsely returns an error status.
C     1st  Feb 1995.    I disagree with the 'wrongly' in the above comnment
C                       for 4th Sep 92. My reading of the SDF format is that
C                       you can have a top level BAD_PIXEL flag. The problem
C                       was that the original code didn't allow for the case
C                       where there was a structured array that could have
C                       its own BAD_PIXEL flag. The new version allows for
C                       both cases. KS/AAO.
C
C  Note:
C     This version supports both the original Figaro data structures
C     and Starlink's NDF format.
C
C+
      SUBROUTINE DSA__SET_FLAGGED (REF_SLOT,FLAGGED,DTA_STATUS)
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
      LOGICAL   STRUCT           ! True if NDF has a structured array
      INTEGER   FLAG             ! Flag value to write to data structure
      CHARACTER OBJECT*32        ! DTA system name for flag value
C
C     Set numeric value to be used for flag
C
      IF (FLAGGED) THEN
         FLAG=1
      ELSE
         FLAG=0
      END IF
C
C     Process is different for NDF and original Figaro format files.
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        NDF format. First, we look to see if the main data array is
C        a structure. This colours what happens next.
C
         CALL DTA_STRUC(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))
     :                           //'.DATA_ARRAY',STRUCT,DTA_STATUS)
         IF (DTA_STATUS.NE.0) STRUCT=.FALSE.
C
C        Whatever happens next, we construct the name of a top level
C        bad pixel flag - we're going to either set it or delete it..
C
         OBJECT=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.BAD_PIXEL'
C
         IF (STRUCT) THEN
C
C           If DATA_ARRAY is a structured array, then we ought to be using
C           .DATA_ARRAY.BAD_PIXEL as our flag, and that's the one
C           we set. Note that we set it explicitly, not relying on
C           default conventions (which confuse people looking at the
C           file with EXAM type routines). To avoid ambiguity, we
C           delete any top level bad pixel flag, then create the name
C           of the bad pixel flag in the array structure.
C
            CALL DTA_DLVAR (OBJECT,DTA_STATUS)
            OBJECT=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                                      '.DATA_ARRAY.BAD_PIXEL'
         END IF
C
C        We now have the name of the bad pixel flag to use in OBJECT.
C        We try to write to it, and if that fails, we try to create it
C        and then write to it.
C
         CALL DTA_WRVARI(OBJECT,1,FLAG,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DTA_CRVAR(OBJECT,'LOGICAL',DTA_STATUS)
            IF (DTA_STATUS.EQ.0) THEN
               CALL DTA_WRVARI(OBJECT,1,FLAG,DTA_STATUS)
            END IF
         END IF
C
      ELSE
C
C        Original Figaro format.
C
C        Generate flag name and try to write to it.  If this fails, it
C        just may be because it didn't exist.  If so, attempt to create it
C        and retry.  If creating the object fails, try to create the
C        environment (the .Z structure) and then try again.
C
         OBJECT=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.Z.FLAGGED'
         CALL DTA_WRVARI(OBJECT,1,FLAG,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DTA_CRVAR(OBJECT,'INT',DTA_STATUS)
            IF (DTA_STATUS.NE.0) THEN
               CALL DTA_CRVAR(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))
     :                                      //'.Z','STRUCT',DTA_STATUS)
               CALL DTA_CRVAR(OBJECT,'INT',DTA_STATUS)
            END IF
            CALL DTA_WRVARI(OBJECT,1,FLAG,DTA_STATUS)
         END IF
      END IF
C
      END
