C+
C                         D S A _ D E F E X T S
C
C  Routine name:
C     DSA_DEFEXTS
C
C  Function:
C     Returns default file extensions for the DSA_ routines
C
C  Description:
C     This routine returns a set of possible default file extensions
C     for the DSA_ routines.  It should not necessarily be assumed that
C     these are three character strings.  In fact, this routine returns
C     the extensions preceeded by a '.' in any case, so normally the
C     argument should be at least four characters. This is an extended
C     version of DSA_DEFEXT which allows for the possibility that there
C     may be more than one possible file extension.  It returns the
C     possible file extensions in order of preference, the first always
C     being the single extension that DSA_DEFEXT will return.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_DEFEXTS (MAX_EXTS,DEFEXTS,NUMBER)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) MAX_EXTS    (Integer,ref) The number of elements in the array
C                     DEFEXTS.  If this is less than the actual number of
C                     extensions in use, then only the first MAX_EXTS will
C                     be returned, but NUMBER will still be returned as
C                     the actual number in use.
C     (<) DEFEXTS     (Fixed string array,descr) The default extensions,
C                     preceeded by a '.' - eg '.dat', in order of
C                     preference.
C     (<) NUMBER      (Integer,ref) The actual number of extensions in
C                     use.  Note that this may be larger than MAX_EXTS.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:  None.
C
C  Prior requirements:
C     DSA_OPEN must have been called.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variables details:
C     (>) DISK_FORMAT  (Integer) Code controlling which formats are supported.
C     (>) DST_ONLY     (Integer parameter) Only DST format is supported.
C     (>) DST_THEN_NDF (Integer parameter) DST and NDF supported, DST default.
C     (>) NDF_ONLY     (Integer parameter) Only NDF format is supported.
C     (>) NDF_THEN_DST (Integer parameter) NDF and DST supported, NDF default.
C
C  History:
C     14th Feb  1991.   Original version.  KS / AAO.
C     21st Aug  1992.   Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     28th Aug  1992.   Now returns extensions in lower case. KS/AAO.
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version allows the default extension to be either the '.dst'
C     of the original Figaro format, or the '.sdf' of Starlink's NDF
C     format.  Arguably, this information should be handled by a DSA__
C     routine, since format knowledge is supposed to be their province.
C+
      SUBROUTINE DSA_DEFEXTS (MAX_EXTS,DEFEXTS,NUMBER)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER MAX_EXTS, NUMBER
      CHARACTER*(*) DEFEXTS(*)
C
C     DSA common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Return the default extensions, depending on the preferred format.
C
      IF (DISK_FORMAT.EQ.DST_ONLY) THEN
         NUMBER=1
         IF (MAX_EXTS.GE.1) DEFEXTS(1)='.dst'
      ELSE IF (DISK_FORMAT.EQ.NDF_ONLY) THEN
         NUMBER=1
         IF (MAX_EXTS.GE.1) DEFEXTS(1)='.sdf'
      ELSE IF (DISK_FORMAT.EQ.DST_THEN_NDF) THEN
         NUMBER=2
         IF (MAX_EXTS.GE.1) DEFEXTS(1)='.dst'
         IF (MAX_EXTS.GE.2) DEFEXTS(2)='.sdf'
      ELSE IF (DISK_FORMAT.EQ.NDF_THEN_DST) THEN
         NUMBER=2
         IF (MAX_EXTS.GE.1) DEFEXTS(1)='.sdf'
         IF (MAX_EXTS.GE.2) DEFEXTS(2)='.dst'
      END IF
C
      END
