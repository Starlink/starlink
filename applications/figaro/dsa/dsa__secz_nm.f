C+
C               D S A _ _ S E C Z _ N A M E
C
C  Routine name:
C     DSA__SECZ_NAME
C
C  Function:
C     Returns the name of the data item holding a sec(z) value.
C
C  Description:
C     This routine, given the reference slot number for a structure,
C     returns the DTA system name of the sub-structure used to hold a
C     sec(z) value for the observation from which the main data array
C     was obtained, should such an object exist. Whether or not the object
C     actually exists is not the province of this routine, which is just
C     a repository of naming information.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__SECZ_NAME (REF_SLOT,NAME,LENGTH)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (<) NAME           (Fixed string,descr) The DTA system name of the
C                        sec(z) value for the structure.
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
      SUBROUTINE DSA__SECZ_NAME (REF_SLOT,NAME,LENGTH)
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
     :                                     '.MORE.FIGARO.SECZ'
         LENGTH=OBJ_LEN(REF_SLOT)+17
      ELSE
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.OBS.SECZ'
         LENGTH=OBJ_LEN(REF_SLOT)+9
      END IF
C
      END
