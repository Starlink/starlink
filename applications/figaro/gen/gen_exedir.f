C+
C                            G E N _ E X E D I R
C
C  Routine name:
C     GEN_EXEDIR
C
C  Function:
C     Dummy function.
C
C  Description:
C     This routine is discontinued. It returns a blank string and string
C     length of one.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL GEN_EXEDIR (DIRECTORY,NCH)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (<) DIRECTORY     (Fixed string,descr) Always ' '.
C     (<) NCH           (Integer,ref) Always 1.
C
C  Author: Horst Meyerdierks, UoE, Starlink
C-
C  History:
C     6th Sept 1988.   Original version.  KS / AAO.
C     26th Aug 1992.   Discontinue.  HME / UoE, Starlink.
C+
      SUBROUTINE GEN_EXEDIR (DIRECTORY,NCH)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NCH
      CHARACTER*(*) DIRECTORY
C
      DIRECTORY = ' '
      NCH = 1
C
      END
