C+
C                D S A _ _ C R E A T E _ Q U A L _ E N V
C
C  Routine name:
C     DSA__CREATE_QUAL_ENV
C
C  Function:
C     Creates the environment for a quality array if it does not yet exist.
C
C  Description:
C     This routine, given the reference slot number for a structure,
C     makes sure that the proper environment for a quality array exists
C     in the structure. This routine must be called as part of the sequence
C     that creates a quality array.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__CREATE_QUAL_ENV (REF_SLOT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (!) STATUS         (Integer,ref) Status code. If passed as non-zero,
C                        this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:
C     ICH_LEN, DTA_CRVAR, DTA_ERROR, DTA_WRVARC, DTA_WRVARB
C
C  Prior requirements:
C     The structure must have been opened already and REF_SLOT must
C     be valid.  This is not tested by this routine.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (>) NDF_FORMAT    (Logical array) Indicates structure format is Starlink's
C                       NDF format (described in SGP38).  If false, format is
C                       original Figaro format (DST files).
C     (>) OBJ_LEN       (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES     (String array) Name (as recognised by DTA_) of data
C                       object corresponding to reference name.
C
C  Subroutine / function details:
C     ICH_LEN      Position of last non-blank char in string
C     DTA_CRVAR    Create a DTA data object
C     DTA_ERROR    Get text string describing a DTA error code.
C     DTA_WRVARB   Write a byte (assumed unsigned) to a DTA object
C     DTA_WRVARC   Write a character string to a DTA object
C
C  History:
C     22nd Apr  1991.   Original version.  KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version supports both the original Figaro data structures
C     and Starlink's NDF format.
C+
      SUBROUTINE DSA__CREATE_QUAL_ENV (REF_SLOT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, STATUS
C
C     Functions
C
      INTEGER ICH_LEN
C
C     DSA system common and error codes
C
      INCLUDE 'DSA_COMMON'
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      BYTE      BADBITS          ! Used for the BADBITS value.
      INTEGER   DTA_STATUS       ! Status returned by DTA routines
      CHARACTER ERROR*64         ! Error description
      INTEGER   LENGTH           ! Number of characters in NAME
      CHARACTER NAME*64          ! Used to generate DTA_ object names
      LOGICAL   STRUCT           ! True if .QUALITY is a structure.
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        In NDF format, .QUALITY can be a primitive array, or it
C        can be a structure where the actual array is .QUALITY.QUALITY.
C        The latter is the normal default, so if .QUALITY doesn't exist
C        yet, we create here a .QUALITY as a structure.
C
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.QUALITY'
         LENGTH=OBJ_LEN(REF_SLOT)+8
         CALL DTA_STRUC(NAME(:LENGTH),STRUCT,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
C
C           Since the DTA_STRUC call failed, we assume this is because
C           the array doesn't exist, and create the structure.  We
C           create all the structure components except the array itself,
C           and set the BADBITS value explicitly to all ones.
C
            CALL DTA_CRVAR(NAME(:LENGTH),'QUALITY',DTA_STATUS)
            IF (DTA_STATUS.EQ.0) THEN
               NAME(LENGTH+1:)='.VARIANT[6]'
               CALL DTA_CRVAR(NAME(:LENGTH+14),'CHAR',DTA_STATUS)
               CALL DTA_WRVARC(NAME(:LENGTH+8),6,'SIMPLE',DTA_STATUS)
            END IF
            IF (DTA_STATUS.EQ.0) THEN
               NAME(LENGTH+1:)='.BADBITS'
               CALL DTA_CRVAR(NAME(:LENGTH+8),'BYTE',DTA_STATUS)
               BADBITS=-1
               CALL DTA_WRVARB(NAME(:LENGTH+8),1,BADBITS,DTA_STATUS)
            END IF
            IF (DTA_STATUS.NE.0) THEN
               CALL DSA_WRUSER('Error creating the quality structure ')
               CALL DSA_WRNAME(NAME(:LENGTH))
               CALL DSA_WRUSER('. ')
               CALL DTA_ERROR(DTA_STATUS,ERROR)
               CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
               CALL DSA_WRUSER('.')
               CALL DSA_WRFLUSH
               STATUS=DSA__DTAERR
               DTA_CODE=DTA_STATUS
            END IF
        END IF
C
      ELSE
C
C        In DST format, .Z.QUALITY is a primitive quality array,
C        and we don't need to create this since any calling routine
C        will have failed before now if there was no .Z structure
C        for it to find a main data array in.
C
      END IF
C
      END
