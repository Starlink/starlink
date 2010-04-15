C+
C               D S A _ _ C R E A T E _ A X I S _ E X T R A
C
C  Routine name:
C     DSA__CREATE_AXIS_EXTRA
C
C  Function:
C     Creates the extension substructure for an axis in a data structure.
C
C  Description:
C    Given the reference slot number in the DSA common tables for an
C    already open structure, and assuming that a specified axis substructure
C    exists in that open structure, this routine ensures that there is a
C    suitable place for extensions to the standard format for that axis,
C    creating it if necessary.  No error messages are output if this fails
C    but the DTA error code from the create call is returned.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__CREATE_AXIS_EXTRA (REF_SLOT,AXIS,DTA_STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (>) AXIS           (Integer,ref) The number of the axis in question.
C     (<) DTA_STATUS     (Integer,ref) The DTA status code from the creation
C                        operation.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:
C     DTA_CRVAR, DTA_TYVAR
C
C  Prior requirements:
C     The structure must have been opened already and REF_SLOT must
C     be valid.  AXIS should not be more than the maximum allowed by
C     the DSA routines.  None of this is tested by this routine.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DTA_CRVAR       Creates a data object.
C     DTA_TYVAR       Gets the type of a data object - used as existence check
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
C     27th Feb  1990.   Original version.  KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version handles both the original Figaro data structures
C     and Starlink's NDF format.
C+
      SUBROUTINE DSA__CREATE_AXIS_EXTRA (REF_SLOT,AXIS,DTA_STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, AXIS, DTA_STATUS
C
C     DSA system common
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      CHARACTER CHAR1*1     ! AXIS as a character
      INTEGER   LENGTH      ! Number of characters in NAME
      CHARACTER NAME*80     ! Name of the extra info sub-structure
      CHARACTER TYPE*16     ! Type of sub-structure - ignored
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        In NDF format, the axis structure is an element of a structure
C        array and extension information goes into a .MORE.FIGARO sub-
C        structure of it.  See if this substructure exists.  If it doesn't
C        then create the .MORE structure and then the .MORE.FIGARO structure.
C        Note that we require that the axis structure does exist.
C
         CHAR1=CHAR(ICHAR('0')+AXIS)
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.AXIS['//
     :                                          CHAR1//'].MORE.FIGARO'
         LENGTH=OBJ_LEN(REF_SLOT)+20
         CALL DTA_TYVAR (NAME,TYPE,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DTA_CRVAR (NAME(:LENGTH-7),'EXT',DTA_STATUS)
            CALL DTA_CRVAR (NAME(:LENGTH),'FIGARO_EXT',DTA_STATUS)
         END IF
      ELSE
C
C        In the original Figaro format, extra data can appear in the
C        axis substructure itself, and we've already required that that
C        exist, so we don't have to do anything.
C
      END IF
C
      END
