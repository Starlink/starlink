C+
C                D S A _ _ C R E A T E _ D A T A _ E N V
C
C  Routine name:
C     DSA__CREATE_DATA_ENV
C
C  Function:
C     Ensures that a data structure has the environment for the data array.
C
C  Description:
C    Given the reference slot number in the DSA common tables for an
C    already open structure, this routine ensures that, should the data
C    format being used require the presence of a substructure to contain
C    the main data array (eg the .Z structure in the original Figaro
C    format), such a substructure does exist. No error messages are output
C    if this fails but any DTA error code generated is returned.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__CREATE_DATA_ENV (REF_SLOT,DTA_STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (<) DTA_STATUS     (Integer,ref) Any applicable DTA status code
C                        produced by this operation.  Zero if all OK.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:
C     DTA_CRVAR, DTA_STRUC, DTA_WRVARI
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
C     DTA_CRVAR    Creates a data object.
C     DTA_STRUC    Sees if object is a structure - used as an existence test
C     DTA_WRVARI   Writes an integer value to a data structure object
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
C     16th Jan 1990.   Original version.  KS / AAO.
C     22nd Apr 1991.   Now sets NDF flagged value flag false by default. KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     3rd  Sep 1992    No longer create the BAD_PIXEL flag for NDFs. It used
C                      to be created in the wrong place anyway. HME/UoE
C
C  Note:
C     This version can handle both the original Figaro data structures
C     and Starlink's NDF format.
C+
      SUBROUTINE DSA__CREATE_DATA_ENV (REF_SLOT,DTA_STATUS)
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
      INTEGER STRUCT        ! Indicates if data object is a structure - ignored
C
C     In NDF format, the data arrays are at top level, so little
C     needs doing. However, we do set the flagged value flag to false
C     unless it already exists, in which case we leave it alone.  In
C     THE original Figaro format, a .Z structure must exist, so we create that.
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C        CALL DTA_CRVAR(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
C    :          '.BAD_PIXEL','LOGICAL',DTA_STATUS)
C        IF (DTA_STATUS.EQ.0) THEN
C           CALL DTA_WRVARI(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
C    :          '.BAD_PIXEL',1,0,DTA_STATUS)
C        ELSE
C           DTA_STATUS=0
C        END IF
      ELSE
         CALL DTA_STRUC(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.Z',
     :                                               STRUCT,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DTA_CRVAR(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                                        '.Z','STRUCT',DTA_STATUS)
         END IF
      END IF
C
      END
