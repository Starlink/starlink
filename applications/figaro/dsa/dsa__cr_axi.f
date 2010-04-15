C+
C                   D S A _ _ C R E A T E _ A X I S
C
C  Routine name:
C     DSA__CREATE_AXIS
C
C  Function:
C     Creates an axis substructure in a data structure.
C
C  Description:
C    Given the reference slot number in the DSA common tables for an
C    already open structure, this routine ensures that a specified
C    axis substructure exists in that open structure, creating it if
C    necessary.  No error messages are output if this fails but the
C    DTA error code from the create call is returned.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__CREATE_AXIS (REF_SLOT,AXIS,DTA_STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (>) AXIS           (Integer,ref) The number of the axis in question.
C     (<) DTA_STATUS     (Integer,ref) The DTA status code from the delete
C                        operation.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:
C     DTA_CRVAR, DTA_SZVAR, DTA_RNVAR
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
C     DTA_CRVAR         Creates a data object.
C     DTA_SZVAR         Gets the dimensions of a data object.
C     DTA_RNVAR         Renames a data object - used here to resize it.
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
C     15th Dec  1989.   Original version.  KS / AAO.
C     17th Jan  1990.   Now handles NDF format.  KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version handles both the original Figaro data structures
C     and Starlink's NDF format.
C+
      SUBROUTINE DSA__CREATE_AXIS (REF_SLOT,AXIS,DTA_STATUS)
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
      INTEGER   LENAME      ! Characters in NAME up to the dimension information
      CHARACTER NAME*80     ! Name of the axis structure, including dimensions
      INTEGER   NAXES       ! Number of axes already defined.
      INTEGER   NDIM        ! Dimensions of axis structure - assumed to be 1.
C
C     Names of axis data structures in the original Figaro format.
C
      CHARACTER AXIS_NAMES*6
      DATA AXIS_NAMES/'XYTUVW'/
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        In NDF format, the axis structure is an element of a structure
C        array.  See if this exists, and if so, see how big it it.  If
C        it doesn't exist, create it.  If it does exist but is too small,
C        resize it.  Note that NAME includes the dimension information,
C        while NAME(:LENAME) does not.
C
         CALL DTA_CRNAM(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT)),'AXIS',
     :                                          1,AXIS,NAME,DTA_STATUS)
         LENAME=OBJ_LEN(REF_SLOT)+5
         CALL DTA_SZVAR(NAME(:LENAME),1,NDIM,NAXES,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DTA_CRVAR(NAME,'AXIS',DTA_STATUS)
         ELSE
            IF (NAXES.LT.AXIS) THEN
               CALL DTA_RNVAR(NAME(:LENAME),NAME,DTA_STATUS)
            END IF
         END IF
      ELSE
C
C        For the original Figaro format, just attempt to create the
C        appropriate axis structure.
C
         CALL DTA_CRVAR(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.'//
     :                      AXIS_NAMES(AXIS:AXIS),'STRUCT',DTA_STATUS)
      END IF
C
      END
