C+
C                D S A _ _ S A M E _ F I L E _ F O R M A T
C
C  Routine name:
C     DSA__SAME_FILE_FORMAT
C
C  Function:
C     Sets DSA file format variables for one file to match those of another.
C
C  Description:
C     If a new data file is being opened and is to have the same file format
C     as another file, this routine can be used to set the common file format
C     variables for the new file to match that of the other file.  This can
C     be used as an alternative to calling DSA__SET_FILE_TYPE.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__SAME_FILE_FORMAT (REF_SLOT,REF_SLOT2,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The reference slot number in
C                        common used for the structure that has just been
C                        opened.
C     (>) REF_SLOT2      (Integer,ref) The reference slot number in
C                        common used for the structure whose type the
C                        new structure is to match.
C     (!) STATUS         (Integer,ref) Status variable.  If bad status is
C                        passed to it, this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA system.
C
C  External subroutines / functions used: None.
C
C  Prior requirements:
C     REF_SLOT and REF_SLOT2 must refer to already opened structures.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (!) NDF_FORMAT    (Logical array) Indicates structure format is Starlink's
C                       NDF format (described in SGP38).  If false, format is
C                       original Figaro format (DST files).
C
C  History:
C     19th Jan 1990.   Original version.  KS / AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA__SAME_FILE_FORMAT (REF_SLOT,REF_SLOT2,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, REF_SLOT2, STATUS
C
C     DSA common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Copy format setting
C
      NDF_FORMAT(REF_SLOT)=NDF_FORMAT(REF_SLOT2)
C
      END
