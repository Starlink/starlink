C+
C               D S A _ _ E X T R A _ N A M E
C
C  Routine name:
C     DSA__EXTRA_NAME
C
C  Function:
C     Returns the name of the main sub-structure used for extra information.
C
C  Description:
C     This routine, given the reference slot number for a structure,
C     returns the DTA system name of the sub-structure used to hold extra
C     information not associated directly with either the main data array
C     or the axis arrays (for which specific name routines such as
C     DSA__MAG_FLAG_NAME are available). Whether or not the structure actually
C     exists is not the province of this routine, which is just a repository
C     of naming information.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__EXTRA_NAME (REF_SLOT,NAME,LENGTH)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (<) NAME           (Fixed string,descr) The DTA system name of the
C                        extra information sub-structure for the structure.
C     (<) LENGTH         (Integer,ref) The number of significant characters
C                        in NAME.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:  None.
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
C     (>) OBJ_LEN       (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES     (String array) Name (as recognised by DTA_) of data
C                       object corresponding to reference name.
C     (>) NDF_FORMAT    (Logical array) Indicates structure format is Starlink's
C                       NDF format (described in SGP38).  If false, format is
C                       original Figaro format (DST files).
C
C  History:
C     12th March 1990.   Original version.  KS / AAO.
C     21st Aug 1992      Automatic portability modifications
C                        ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992      "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version supports both the original Figaro data structures
C     and Starlink's NDF format.
C+
      SUBROUTINE DSA__EXTRA_NAME (REF_SLOT,NAME,LENGTH)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, LENGTH
      CHARACTER*(*) NAME
C
C     DSA system common
C
      INCLUDE 'DSA_COMMON'
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                                     '.MORE.FIGARO'
         LENGTH=OBJ_LEN(REF_SLOT)+12
      ELSE
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))
         LENGTH=OBJ_LEN(REF_SLOT)
      END IF
C
      END
