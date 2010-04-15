C+
C                   D S A _ _ A X I S _ N A M E
C
C  Routine name:
C     DSA__AXIS_NAME
C
C  Function:
C     Returns the name of the axis substructure for a given axis.
C
C  Description:
C     This routine, given the reference slot number for a structure,
C     returns the DTA system name of the axis substructure for the
C     specified axis, should it contain one.  Whether or not the structure
C     actually exists is not the province of this routine, which is just
C     a repository of naming information.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__AXIS_NAME (REF_SLOT,AXIS,NAME,LENGTH)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (>) AXIS           (Integer,ref) The number of the axis in question.
C     (<) NAME           (Fixed string,descr) The DTA system name of the
C                        axis substructure in the structure.
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
C     be valid.  AXIS should not be more than the maximum allowed by
C     the DSA routines.  None of this is tested by this routine.
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
C  History:
C     14th Dec  1989.   Original version.  KS / AAO.
C     2nd March 1990.   Support for NDF format added.  KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version supports both the original Figaro data structures
C     and Starlink's NDF format.
C+
      SUBROUTINE DSA__AXIS_NAME (REF_SLOT,AXIS,NAME,LENGTH)
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
C     Names of axis data structures in the original Figaro format.
C
      CHARACTER AXIS_NAMES*6
      DATA AXIS_NAMES/'XYTUVW'/
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.AXIS['//
     :                                  CHAR(ICHAR('0')+AXIS)//']'
         LENGTH=OBJ_LEN(REF_SLOT)+8
      ELSE
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.'//
     :                               AXIS_NAMES(AXIS:AXIS)
         LENGTH=OBJ_LEN(REF_SLOT)+2
      END IF
C
      END
