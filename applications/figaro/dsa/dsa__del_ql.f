C+
C                   D S A _ _ D E L E T E _ Q U A L I T Y
C
C  Routine name:
C     DSA__DELETE_QUALITY
C
C  Function:
C     Deletes a quality information substructure from a data structure.
C
C  Description:
C    Given the reference slot number in the DSA common tables for an
C    already open structure, this routine completely deletes any quality
C    information substructure from the open structure.  No error messages are
C    output if this fails (there may not even be such an axis substructure)
C    and the DTA error code from the delete call is returned.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__DELETE_QUALITY (REF_SLOT,DTA_STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (<) DTA_STATUS     (Integer,ref) The DTA status code from the delete
C                        operation.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:  DTA_DLVAR.
C
C  Prior requirements:
C     The structure must have been opened already and REF_SLOT must
C     be valid.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 12th February 1995
C-
C  Subroutine / function details:
C     DTA_DLVAR         Deletes a data object.
C
C  Common variable details:
C     (>) OBJ_LEN       (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES     (String array) Name (as recognised by DTA_) of data
C                       object corresponding to reference name.
C     (>) NDF_FORMAT    (Logical array) Indicates structure format is Starlink's
C                       NDF format (described in SGP38).  If false, format is
C                       original Figaro format (DST files).
C  History:
C     12th Feb 1995.    Original version.  KS / AAO.
C
C  Note:
C     This version supports both the original Figaro data structures
C     and Starlink's NDF format.
C+
      SUBROUTINE DSA__DELETE_QUALITY (REF_SLOT,DTA_STATUS)
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
      CHARACTER QUAL_INFO*32          ! DTA name of quality information
      INTEGER   LENGTH                ! Number of chars in QUAL_INFO
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        NDF format has the quality information in .QUALITY which may be
C        a simple array or a structure.
C
         QUAL_INFO=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.QUALITY'
         LENGTH=OBJ_LEN(REF_SLOT)+8
      ELSE
C
C        Original Figaro format has quality information in .Z.QUALITY,
C        which is normally a simple array.
C
         QUAL_INFO=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.Z.QUALITY'
         LENGTH=OBJ_LEN(REF_SLOT)+10
      END IF
C
C     Now that we have the name, delete it.
C
      CALL DTA_DLVAR(QUAL_INFO(:LENGTH),DTA_STATUS)
C
C     Exit
C
      END
