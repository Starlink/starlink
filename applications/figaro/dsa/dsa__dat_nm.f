C+
C                         D S A _ _ D A T A _ N A M E
C
C  Routine name:
C     DSA__DATA_NAME
C
C  Function:
C     Returns the name of the main data array in a structure.
C
C  Description:
C     This routine, given the reference slot number for a structure,
C     returns the DTA system name of its main data array, should it
C     contain one.  Whether or not the array actually exists is not
C     the province of this routine, which is just a repository of
C     naming information.  (However, this routine does add a check
C     for an existing primitive top-level array - that is, if the
C     reference name specifies an existing object that is primitive,
C     then this routine will return its name and not a structured
C     component of it.)
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__DATA_NAME (REF_SLOT,NAME,LENGTH)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (<) NAME           (Fixed string,descr) The DTA system name of the
C                        main data array in the structure.
C     (<) LENGTH         (Integer,ref) The number of significant characters
C                        in NAME.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:
C     DTA_STRUC, DSA__ARRAY
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
C  Subroutine / function details:
C     DTA_STRUC         See if a named object is structured.
C     DSA__ARRAY        See if a structure is a structured array.
C
C  History:
C     13th Dec  1989.   Original version.  KS / AAO.
C     15th Jan  1990.   NDF case added.  KS/AAO.
C     23rd Apr  1990.   Now actually looks at the structure to see if it is
C                       a primitive array or not.
C     27th Apr  1990.   Now uses DSA__ARRAY rather than just DTA_STRUC to see
C                       is this is a structure or an array.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version is for the both the original Figaro data structures
C     and the Starlink defined NDF structures.
C
C     This routine is not efficient.  There is a good case for using a
C     common string for the data name that this can look at and only
C     probe the structure if that isn't set.
C+
      SUBROUTINE DSA__DATA_NAME (REF_SLOT,NAME,LENGTH)
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
C     Functions
C
      LOGICAL DSA__ARRAY
C
C     Logical variables
C
      INTEGER DTA_STATUS          ! Status returned from DTA routines
      LOGICAL STRUCT              ! True if ref-name refers to a structure
C
      CALL DTA_STRUC (OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT)),STRUCT,
     :                                                    DTA_STATUS)
      IF ((DTA_STATUS.EQ.0).AND..NOT.STRUCT) THEN
C
C        Top-level object is a primitive array
C
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))
         LENGTH=OBJ_LEN(REF_SLOT)
      ELSE
         IF (DSA__ARRAY(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT)))) THEN
C
C           Top level object is a structure, but it is a structured
C           array so can be treated as the array.
C
            NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))
            LENGTH=OBJ_LEN(REF_SLOT)
         ELSE
C
C           Top level object is a proper Figaro structure.  The name of
C           the data array will depend on the file format.
C
            IF (NDF_FORMAT(REF_SLOT)) THEN
               NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                                                   '.DATA_ARRAY'
               LENGTH=OBJ_LEN(REF_SLOT)+11
            ELSE
               NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.Z.DATA'
               LENGTH=OBJ_LEN(REF_SLOT)+7
            END IF
         END IF
      END IF
C
      END
