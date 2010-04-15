C+
C                         D S A _ _ O B J E C T _ N A M E
C
C  Routine name:
C     DSA__OBJECT_NAME
C
C  Function:
C     Returns the name of the item holding the object name in a structure.
C
C  Description:
C     This routine, given the refernece slot number for a structure,
C     returns the DTA system name of the item holding its object name,
C     should it contain one.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__OBJECT_NAME (REF_SLOT,NAME,LENGTH)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (<) NAME           (Fixed string,descr) The DTA system name of the
C                        object string in the structure.
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
C     (>) NDF_FORMAT    (Logical array) Indicates structure format is Starlink's
C                       NDF format (described in SGP38).  If false, format is
C                       original Figaro format (DST files).
C     (>) OBJ_LEN       (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES     (String array) Name (as recognised by DTA_) of data
C                       object corresponding to reference name.
C
C  History:
C     16th Jan  1990.   Original version.  KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version will handle either the original Figaro data structures
C     or Starlink's NDF format.
C+
      SUBROUTINE DSA__OBJECT_NAME (REF_SLOT,NAME,LENGTH)
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
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.TITLE'
         LENGTH=OBJ_LEN(REF_SLOT)+6
      ELSE
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.OBS.OBJECT'
         LENGTH=OBJ_LEN(REF_SLOT)+11
      END IF
C
      END
