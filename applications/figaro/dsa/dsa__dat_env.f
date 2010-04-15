C+
C                  D S A _ _ D A T A _ E N V _ N A M E
C
C  Routine name:
C     DSA__DATA_ENV_NAME
C
C  Function:
C     Returns the environment name for the main data array in a structure.
C
C  Description:
C     This routine, given the refernece slot number for a structure,
C     returns the DTA system name of the environment for its main data
C     array, should it contain one.  Note that the environment name is
C     no more than the name of the structure that contains the main data
C     array.  Whether or not the array actually exists is not the province
C     of this routine, which is just a repository of naming information.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__DATA_ENV_NAME (REF_SLOT,NAME,LENGTH)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (<) NAME           (Fixed string,descr) The DTA system name of the
C                        environment for the main data array in the structure.
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
C     26th Feb  1989.   Original version.  KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version is for the both the original Figaro data structures
C     and the Starlink defined NDF structures.
C+
      SUBROUTINE DSA__DATA_ENV_NAME (REF_SLOT,NAME,LENGTH)
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
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))
         LENGTH=OBJ_LEN(REF_SLOT)
      ELSE
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.Z'
         LENGTH=OBJ_LEN(REF_SLOT)+2
      END IF
C
      END
