C+
C                         D S A _ D E F E X T
C
C  Routine name:
C     DSA_DEFEXT
C
C  Function:
C     Returns the default file extension for the DSA_ routines
C
C  Description:
C     This routine returns the default file extension for the DSA_
C     routines.  It should not necessarily be assumed that this is
C     a three character string.  In fact, this routine returns
C     the extension preceeded by a '.' in any case, so normally the
C     argument should be at least four characters.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_DEFEXT (DEFEXT)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (<) DEFEXT      (Fixed string,descr) The default extension,
C                     preceeded by a '.' - eg '.dat'
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
C
C  History:
C     16th June 1987.   Original version.  KS / AAO.
C     20th Feb  1990.   Modified to allow for .SDF files. KS/AAO.
C     21st Aug  1992.   Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     28th Aug  1992.   Now returns extensions in lower case. KS/AAO.
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version allows the default extension to be either the '.dst'
C     of the original Figaro format, or the '.sdf' of Starlink's NDF
C     format.  Strictly, this information should be handled by a DSA__
C     routine, since format knowledge is supposed to be their province.
C+
      SUBROUTINE DSA_DEFEXT (DEFEXT)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) DEFEXT
C
C     DSA common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Return the default extension, depending on the preferred format.
C
      IF ((DISK_FORMAT.EQ.DST_ONLY).OR.
     :                          (DISK_FORMAT.EQ.DST_THEN_NDF)) THEN
         DEFEXT='.dst'
      ELSE
         DEFEXT='.sdf'
      END IF
C
      END
