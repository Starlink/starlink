C+
C                   D S A _ _ A X I S _ D A T A _ N A M E
C
C  Routine name:
C     DSA__AXIS_DATA_NAME
C
C  Function:
C     Returns the name of the data array for an axis in a structure.
C
C  Description:
C     This routine, given the reference slot number for a structure,
C     returns the DTA system name of the axis data array for the
C     specified axis, should it contain one.  Whether or not the array
C     actually exists is not the province of this routine, which is just
C     a repository of naming information, although if there are alternative
C     locations for arrays it may have to look at the structure to see
C     which should be used.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__AXIS_DATA_NAME (REF_SLOT,AXIS,NAME,LENGTH)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (>) AXIS           (Integer,ref) The number of the axis in question.
C     (<) NAME           (Fixed string,descr) The DTA system name of the
C                        axis data array in the structure.
C     (<) LENGTH         (Integer,ref) The number of significant characters
C                        in NAME.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:  DTA_TYVAR.
C
C  Prior requirements:
C     The structure must have been opened already and REF_SLOT must
C     be valid.  AXIS should not be more than the maximum allowed by
C     the DSA routines.  None of this is tested by this routine.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 13th February 1995
C-
C  Common variable details:
C     (>) OBJ_LEN       (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES     (String array) Name (as recognised by DTA_) of data
C                       object corresponding to reference name.
C
C  History:
C     11th Dec  1989.   Original version.  KS / AAO.
C     17th Jan  1990.   Modified to support NDF format.  KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C     13th Feb 1995     Now looks for a multiple dimensional array in
C                       an NDF extension as well as in the standard location
C                       KS/AAO.
C
C  Note:
C     This version supports both the original Figaro data structures
C     and Starlink's NDF data structure.
C+
      SUBROUTINE DSA__AXIS_DATA_NAME (REF_SLOT,AXIS,NAME,LENGTH)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, AXIS, LENGTH
      CHARACTER*(*) NAME
C
C     DSA system common
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   DTA_STATUS        ! Status from call to DTA_TYVAR
      CHARACTER TYPE*16           ! Axis data type - ignored
C
C     Names of axis data structures in the original Figaro format.
C
      CHARACTER AXIS_NAMES*6
      DATA AXIS_NAMES/'XYTUVW'/
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        The NDF format has a standard location for the axis data, namely
C        in file.AXIS[n].DATA_ARRAY. However, these can only be one
C        dimensional. If Figaro creates a multi-dimensional axis array, as
C        it needs to at time, it has to go in file.AXIS[n].EXT.FIGARO.
C        DATA_ARRAY. However, in this case there still has to be a 1D array
C        in the standard location (don't ask me why, it's the NDF standard!).
C        So there may be two axis arrays in the structure, and in that
C        case Figaro wants to use the one in the extension. So we look for
C        that first.
C
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.AXIS['//
     :               CHAR(AXIS+ICHAR('0'))//'].MORE.FIGARO.DATA_ARRAY'
         LENGTH=OBJ_LEN(REF_SLOT)+31
         CALL DTA_TYVAR (NAME(:LENGTH),TYPE,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
C
C           There wasn't an array in the extension, so we use the standard
C           location. It doesn't matter if one exists there or not. (If
C           Figaro creates a multi-dimensional array in this location,
C           thereby violating the NDF standard, this will be picked up
C           by DSA_CHECK_NDF_AXIS and copied to the extension location
C           when the structure is closed down.)
C
            NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.AXIS['//
     :               CHAR(AXIS+ICHAR('0'))//'].DATA_ARRAY'
            LENGTH=OBJ_LEN(REF_SLOT)+19
         END IF
C
      ELSE
C
C        In the original Figaro format, we use file.X.DATA, file.Y.DATA
C        etc.
C
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.'//
     :                               AXIS_NAMES(AXIS:AXIS)//'.DATA'
         LENGTH=OBJ_LEN(REF_SLOT)+7
      END IF
C
      END
