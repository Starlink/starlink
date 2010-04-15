C+
C                   D S A _ _ A X I S _ L F L A G _ N A M E
C
C  Routine name:
C     DSA__AXIS_LFLAG_NAME
C
C  Function:
C     Returns the name of the `log binning' flag for an axis in a structure.
C
C  Description:
C     This routine, given the refernece slot number for a structure,
C     returns the DTA system name of the `log binning' flag for the
C     specified axis, should it contain one.  Whether or not the flag
C     actually exists is not the province of this routine, which is just
C     a repository of naming information.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__AXIS_LFLAG_NAME (REF_SLOT,AXIS,NAME,LENGTH)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (>) AXIS           (Integer,ref) The number of the axis in question.
C     (<) NAME           (Fixed string,descr) The DTA system name of the
C                        axis `log binning' flag in the structure.
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
C
C  History:
C     15th Dec  1989.   Original version.  KS / AAO.
C     27th Feb  1990.   Support for NDF format added.  KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version supports both the original Figaro data structures
C     and Starlink's NDF format.
C+
      SUBROUTINE DSA__AXIS_LFLAG_NAME (REF_SLOT,AXIS,NAME,LENGTH)
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
      CHARACTER CHAR1*1     ! Used to generate axis number as a character
C
C     Names of axis data structures in the original Figaro format.
C
      CHARACTER AXIS_NAMES*6
      DATA AXIS_NAMES/'XYTUVW'/
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
         CHAR1=CHAR(ICHAR('0')+AXIS)
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.AXIS['
     :                                //CHAR1//'].MORE.FIGARO.LOG'
         LENGTH=OBJ_LEN(REF_SLOT)+24
      ELSE
         NAME=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.'//
     :                            AXIS_NAMES(AXIS:AXIS)//'.LOG'
         LENGTH=OBJ_LEN(REF_SLOT)+6
      END IF
C
      END
