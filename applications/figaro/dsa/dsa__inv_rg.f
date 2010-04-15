C+
C                   D S A _ _ I N V A L I D A T E _ R A N G E
C
C  Routine name:
C     DSA__INVALIDATE_RANGE
C
C  Function:
C     Invalidates the range information for a data structure.
C
C  Description:
C    Given the reference slot number in the DSA common tables for an
C    already open structure, this routine invalidates any data range
C    information it might hold.  No error messages are output if this
C    fails, but a DTA error code is returned.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__INVALIDATE_RANGE (REF_SLOT,DTA_STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (<) DTA_STATUS     (Integer,ref) The DTA status code returned.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:  DTA_WRVARI.
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
C     DTA_WRVARI        Write an integer to a data object.
C
C  Common variable details:
C     (>) OBJ_LEN       (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES     (String array) Name (as recognised by DTA_) of data
C                       object corresponding to reference name.
C     (>) NDF_FORMAT    (Logical array) Indicates structure format is Starlink's
C                       NDF format (described in SGP38).  If false, format is
C                       original Figaro format (DST files).
C
C  History:
C     11th Dec  1989.   Original version.  KS / AAO.
C     5th March 1990.   Support for NDF format added. KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version supports both the original Figaro data structures
C     and Starlink's NDF format.
C+
      SUBROUTINE DSA__INVALIDATE_RANGE (REF_SLOT,DTA_STATUS)
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
C     Actually, this routine is a bit odd, because it was in fact the
C     first DSA__ routine written.  Done now, it probably wouldn't be
C     used at all - the calling routine would call DSA__RANGE_STRUCT_NAME
C     and make up the name from there.
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
         CALL DTA_WRVARI (OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                    '.MORE.FIGARO.RANGE.VALID',1,0,DTA_STATUS)
      ELSE
         CALL DTA_WRVARI (OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                              '.Z.RANGE.VALID',1,0,DTA_STATUS)
      END IF
C
      END
