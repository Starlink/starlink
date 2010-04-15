C+
C                         D S A _ _ G E T _ B A D B I T S
C
C  Routine name:
C     DSA__GET_BADBITS
C
C  Function:
C     Returns the value of the BADBITS flag in a structure.
C
C  Description:
C     This routine, given the reference slot number for a structure,
C     returns the value of the BADBITS flag (if any) associated with
C     the quality array (if any).
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__GET_BADBITS (REF_SLOT,BADBITS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (<) BADBITS        (Byte,ref) The value of the BADBITS flag associated
C                        with the quality array in the structure.
C     (<) DTA_STATUS     (Integer,ref) Status code. This is a status code
C                        returned by the DTA system.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:
C     DTA_STRUC
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
C  History:
C     1st  May  1991.   Original version.  KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C     15th Aug 2005     Now use PRM for setting BADBITS
C
C  Note:
C     This version supports both the original Figaro data structures
C     and Starlink's NDF format.
C+
      SUBROUTINE DSA__GET_BADBITS (REF_SLOT,BADBITS,DTA_STATUS)
C
      IMPLICIT NONE
C
C     Global variables
C
      INCLUDE 'PRM_PAR'
C
C     Parameters
C
      INTEGER REF_SLOT, DTA_STATUS
      BYTE BADBITS
C
C     DSA system common
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   LENGTH           ! Number of characters in NAME
      CHARACTER NAME*64          ! Used to generate DTA system names
      LOGICAL   STRUCT           ! True if .QUALITY is a structure.
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        In NDF format, .QUALITY can be a primitive array, or it
C        can be a structure where the actual array is .QUALITY.QUALITY.
C        If it is primitive, there is no BADBITS value as such, but
C        interpretation is up to the application, so we can feel free
C        to assume a BADBITS value with all bits set.  If it is a
C        structure, then SGP 38 says that there should be a BADBITS
C        element of the structure, and if there isn't then zero must be
C        assumed.  If quality doesn't exist at all, we assume an all
C        ones value.
C
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.QUALITY'
         LENGTH=OBJ_LEN(REF_SLOT)+8
         CALL DTA_STRUC(NAME(:LENGTH),STRUCT,DTA_STATUS)
         IF (DTA_STATUS.EQ.0) THEN
            IF (STRUCT) THEN
C
C              .QUALITY exists, and is a structure
C
               NAME(LENGTH+1:)='.BADBITS'
               LENGTH=LENGTH+8
               CALL DTA_STRUC(NAME(:LENGTH),STRUCT,DTA_STATUS)
               IF (DTA_STATUS.NE.0) THEN
C
C                 .QUALITY exists, but .QUALITY.BADBITS doesn't.
C
                  BADBITS=0
               ELSE
C
C                 .QUALITY exists, and so does .QUALITY.BADBITS.
C
                  CALL DTA_RDVARB(NAME(:LENGTH),1,BADBITS,DTA_STATUS)
               END IF
            ELSE
C
C              .QUALITY exists, and is primitive.
C
               BADBITS=NUM__MAXUB
            END IF
         ELSE
C
C           .QUALITY doesn't exist at all.
C
            BADBITS=NUM__MAXUB
         END IF
      ELSE
C
C        In DST format, there is normally no BADBITS value, but
C        there seems no reason not to have one in .Z.BADBITS, so
C        we can at least look for that.  However, normally, we
C        assume an all ones value.
C
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.Z.BADBITS'
         LENGTH=OBJ_LEN(REF_SLOT)+10
         CALL DTA_STRUC(NAME(:LENGTH),STRUCT,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            BADBITS=NUM__MAXUB
         ELSE
            CALL DTA_RDVARB(NAME(:LENGTH),1,BADBITS,DTA_STATUS)
         END IF
      END IF
C
      END
