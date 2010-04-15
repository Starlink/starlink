C+
C               D S A _ _ C R E A T E _ D A T A _ E X T R A
C
C  Routine name:
C     DSA__CREATE_DATA_EXTRA
C
C  Function:
C     Creates the data extension substructure for a data structure.
C
C  Description:
C    Given the reference slot number in the DSA common tables for an
C    already open structure, this routine ensures that there is a suitable
C    place for extensions to the standard format for that structure connected
C    with the main data array (the range sub-structure, for example),
C    creating it if necessary.  No error messages are output if this fails
C    but the DTA error code from the create call is returned.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__CREATE_DATA_EXTRA (REF_SLOT,DTA_STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
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
C     be valid.  This is not tested by this routine.
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
C     12th March 1990.   Original version.  KS / AAO.
C     21st Aug 1992      Automatic portability modifications
C                        ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992      "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version handles both the original Figaro data structures
C     and Starlink's NDF format.
C+
      SUBROUTINE DSA__CREATE_DATA_EXTRA (REF_SLOT,DTA_STATUS)
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
      INTEGER   LENGTH      ! Number of characters in NAME
      CHARACTER NAME*80     ! Name of the extra info sub-structure
      CHARACTER TYPE*16     ! Type of sub-structure - ignored
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        In NDF format, extension information goes into a .MORE.FIGARO sub-
C        structure of the top level structure.  See if this substructure
C        exists.  If it doesn't then create the .MORE structure and then the
C        .MORE.FIGARO structure.
C
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.MORE.FIGARO'
         LENGTH=OBJ_LEN(REF_SLOT)+12
         CALL DTA_TYVAR (NAME,TYPE,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DTA_CRVAR (NAME(:LENGTH-7),'EXT',DTA_STATUS)
            CALL DTA_CRVAR (NAME(:LENGTH),'FIGARO_EXT',DTA_STATUS)
         END IF
      ELSE
C
C        In the original Figaro format, extra data can appear in the
C        .Z substructure itself.  We see if that exists, and attempt to
C        create it if it doesn't.
C
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.Z'
         LENGTH=OBJ_LEN(REF_SLOT)+2
         CALL DTA_TYVAR (NAME,TYPE,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DTA_CRVAR (NAME(:LENGTH),'STRUCT',DTA_STATUS)
         END IF
      END IF
C
      END
