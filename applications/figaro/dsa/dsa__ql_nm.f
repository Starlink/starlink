C+
C                         D S A _ _ Q U A L _ N A M E
C
C  Routine name:
C     DSA__QUAL_NAME
C
C  Function:
C     Returns the name of the quality array in a structure.
C
C  Description:
C     This routine, given the refernece slot number for a structure,
C     returns the DTA system name of its quality array, should it
C     contain one.  Whether or not the array actually exists is not
C     the province of this routine, which is just a repository of
C     naming information.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__QUAL_NAME (REF_SLOT,NAME,LENGTH)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (<) NAME           (Fixed string,descr) The DTA system name of the
C                        quality array in the structure.
C     (<) LENGTH         (Integer,ref) The number of significant characters
C                        in NAME.
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
C     11th Dec  1989.   Original version.  KS / AAO.
C     17th Jan  1990.   Modified to handle NDF format.  KS/AAO.
C     22nd Apr  1991.   Now allows for a structure in NDF format. KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version supports both the original Figaro data structures
C     and Starlink's NDF format.
C+
      SUBROUTINE DSA__QUAL_NAME (REF_SLOT,NAME,LENGTH)
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
C     Local variables
C
      INTEGER   DTA_STATUS       ! Status returned by DTA routines
      LOGICAL   STRUCT           ! True if .QUALITY is a structure.
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        In NDF format, .QUALITY can be a primitive array, or it
C        can be a structure where the actual array is .QUALITY.QUALITY.
C        The latter is the normal default, so if .QUALITY doesn't exist
C        yet that's what will be created by DSA__CREATE_QUAL_ENV.
C
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.QUALITY'
         LENGTH=OBJ_LEN(REF_SLOT)+8
         CALL DTA_STRUC(NAME(:LENGTH),STRUCT,DTA_STATUS)
         IF (DTA_STATUS.EQ.0) THEN
            IF (STRUCT) THEN
               NAME(LENGTH+1:)='.QUALITY'
               LENGTH=LENGTH+8
            END IF
         ELSE
            NAME(LENGTH+1:)='.QUALITY'
            LENGTH=LENGTH+8
         END IF
      ELSE
C
C        In DST format, .Z.QUALITY is a primitive quality array.
C
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.Z.QUALITY'
         LENGTH=OBJ_LEN(REF_SLOT)+10
      END IF
C
      END
